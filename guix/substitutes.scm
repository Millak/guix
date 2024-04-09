;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2021, 2023-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2018 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2020 Christopher Baines <mail@cbaines.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix substitutes)
  #:use-module (guix narinfo)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix combinators)
  #:use-module (guix config)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix cache)
  #:use-module ((guix build utils)
                #:select (mkdir-p dump-port delete-file-recursively))
  #:use-module ((guix build download)
                #:select ((open-connection-for-uri
                           . guix:open-connection-for-uri)
                          resolve-uri-reference
                          nar-uri-abbreviation))
  #:use-module ((guix serialization) #:select (restore-file dump-file))
  #:autoload   (gnutls) (error->string error/premature-termination
                                       error/invalid-session error/again
                                       error/interrupted
                                       error/push-error error/pull-error)
  #:autoload   (guix store deduplication) (dump-file/deduplicate)
  #:use-module (guix progress)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 vlist)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-71)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (guix http-client)
  #:export (%narinfo-cache-directory

            call-with-connection-error-handling

            lookup-narinfos
            lookup-narinfos/diverse

            http-response-error?
            download-nar))

(define %narinfo-ttl
  ;; Number of seconds during which cached narinfo lookups are considered
  ;; valid for substitute servers that do not advertise a TTL via the
  ;; 'Cache-Control' response header.
  (* 36 3600))

(define %narinfo-negative-ttl
  ;; Likewise, but for negative lookups---i.e., cached lookup failures (404).
  (* 2 60))

(define %narinfo-transient-error-ttl
  ;; Likewise, but for transient errors such as 504 ("Gateway timeout").
  (* 1 60))

(define %narinfo-cache-directory
  ;; A local cache of narinfos, to avoid going to the network.  Most of the
  ;; time, 'guix substitute' is called by guix-daemon as root and stores its
  ;; cached data in /var/guix/….  However, when invoked from 'guix challenge'
  ;; as a user, it stores its cache in ~/.cache.
  (if (getenv "_NIX_OPTIONS")                     ;invoked by guix-daemon
      (or (and=> (getenv "XDG_CACHE_HOME")
                 (cut string-append <> "/guix/substitute"))
          (string-append %state-directory "/substitute/cache"))
      (string-append (cache-directory #:ensure? #f) "/substitute")))

(define %debug?
  ;; Enable debug mode by setting the GUIX_SUBSTITUTE_DEBUG environmnent
  ;; variable.
  (make-parameter
   (getenv "GUIX_SUBSTITUTE_DEBUG")))

(define-syntax-rule (debug fmt args ...)
  (when (%debug?)
    (format #t fmt args ...)))

(define (narinfo-cache-file cache-url path)
  "Return the name of the local file that contains an entry for PATH.  The
entry is stored in a sub-directory specific to CACHE-URL."
  ;; The daemon does not sanitize its input, so PATH could be something like
  ;; "/gnu/store/foo".  Gracefully handle that.
  (match (store-path-hash-part path)
    (#f
     (leave (G_ "'~a' does not name a store item~%") path))
    ((? string? hash-part)
     (string-append %narinfo-cache-directory "/"
                    (bytevector->base32-string (sha256 (string->utf8 cache-url)))
                    "/" hash-part))))

(define (cache-narinfo! cache-url path narinfo ttl)
  "Cache locally NARNIFO for PATH, which originates from CACHE-URL, with the
given TTL (a number of seconds or #f).  NARINFO may be #f, in which case it
indicates that PATH is unavailable at CACHE-URL."
  (define now
    (current-time time-monotonic))

  (define (cache-entry cache-uri narinfo)
    `(narinfo (version 2)
              (cache-uri ,cache-uri)
              (date ,(time-second now))
              (ttl ,(or ttl
                        (if narinfo %narinfo-ttl %narinfo-negative-ttl)))
              (value ,(and=> narinfo narinfo->string))))

  (let ((file (narinfo-cache-file cache-url path)))
    (mkdir-p (dirname file))
    (with-atomic-file-output file
      (lambda (out)
        (write (cache-entry cache-url narinfo) out))
      #:sync? #f))

  narinfo)

(define %unreachable-hosts
  ;; Set of names of unreachable hosts.
  (make-hash-table))

(define* (call-with-connection-error-handling uri proc)
  "Call PROC, and catch if a connection fails, print a warning and return #f."
  (define host
    (uri-host uri))

  (catch #t
    proc
    (match-lambda*
      (('getaddrinfo-error error)
       (unless (hash-ref %unreachable-hosts host)
         (hash-set! %unreachable-hosts host #t)   ;warn only once
         (warning (G_ "~a: host not found: ~a~%")
                  host (gai-strerror error)))
       #f)
      (('system-error . args)
       (unless (hash-ref %unreachable-hosts host)
         (hash-set! %unreachable-hosts host #t)
         (warning (G_ "~a: connection failed: ~a~%") host
                  (strerror
                   (system-error-errno `(system-error ,@args)))))
       #f)
      (('gnutls-error error proc . rest)
       (if (memq error (list error/premature-termination
                             error/pull-error
                             error/push-error))
           (begin
             (warning (G_ "~a: TLS connection failed: in ~a: ~a~%") host
                      proc (error->string error))
             #f)
           (apply throw 'gnutls-error error proc rest)))
      (args
       (apply throw args)))))

(define (narinfo-request cache-url path)
  "Return an HTTP request for the narinfo of PATH at CACHE-URL."
  ;; Ensure BASE has a trailing slash so that REF is correct regardless of
  ;; whether the user-provided CACHE-URL has a trailing slash.
  (let* ((base (string->uri (if (string-suffix? "/" cache-url)
                                cache-url
                                (string-append cache-url "/"))))
         (ref (build-relative-ref
               #:path (string-append (store-path-hash-part path) ".narinfo")))
         (url (resolve-uri-reference ref base))
         (headers '((User-Agent . "GNU Guile"))))
    (build-request url #:method 'GET #:headers headers)))

(define (narinfo-from-file file url)
  "Attempt to read a narinfo from FILE, using URL as the cache URL.  Return #f
if file doesn't exist, and the narinfo otherwise."
  (catch 'system-error
    (lambda ()
      (call-with-input-file file
        (cut read-narinfo <> url)))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          #f
          (apply throw args)))))

(define* (fetch-narinfos url paths
                         #:key
                         (open-connection guix:open-connection-for-uri)
                         (make-progress-reporter
                          (const progress-reporter/silent)))
  "Retrieve all the narinfos for PATHS from the cache at URL and return them."
  (define progress-reporter
    (make-progress-reporter (length paths)
                            #:url url))

  (define hash-part->path
    (let ((mapping (fold (lambda (path result)
                           (vhash-cons (store-path-hash-part path) path
                                       result))
                         vlist-null
                         paths)))
      (lambda (hash)
        (match (vhash-assoc hash mapping)
          (#f #f)
          ((_ . path) path)))))

  (define (read-to-eof port)
    "Read from PORT until EOF is reached.  The data are discarded."
    (dump-port port (%make-void-port "w")))

  (define (handle-narinfo-response request response port result)
    (let* ((code   (response-code response))
           (len    (response-content-length response))
           (cache  (response-cache-control response))
           (ttl    (and cache (assoc-ref cache 'max-age))))
      (progress-reporter-report! progress-reporter)

      ;; Make sure to read no more than LEN bytes since subsequent bytes may
      ;; belong to the next response.
      (if (= code 200)                            ; hit
          (let ((narinfo (read-narinfo port url #:size len)))
            (if (string=? (dirname (narinfo-path narinfo))
                          (%store-prefix))
                (begin
                  (cache-narinfo! url (narinfo-path narinfo) narinfo ttl)
                  (cons narinfo result))
                result))
          (let* ((path      (uri-path (request-uri request)))
                 (hash-part (basename
                             (string-drop-right path 8)))) ;drop ".narinfo"
            ;; Log the failing queries and indicate if it failed because the
            ;; narinfo is being baked.
            (let ((baking?
                   (assoc-ref (response-headers response) 'x-baking)))
              (debug "could not fetch ~a~a ~a~a~%"
                     url path code
                     (if baking? " (baking)" "")))
            (if len
                (get-bytevector-n port len)
                (read-to-eof port))
            (cache-narinfo! url (hash-part->path hash-part) #f
                            (if (or (= 404 code) (= 202 code))
                                ttl
                                %narinfo-transient-error-ttl))
            result))))

  (define (do-fetch uri)
    (case (and=> uri uri-scheme)
      ((http https)
       ;; Note: Do not check HTTPS server certificates to avoid depending
       ;; on the X.509 PKI.  We can do it because we authenticate
       ;; narinfos, which provides a much stronger guarantee.
       (let* ((requests (map (cut narinfo-request url <>) paths))
              (result   (begin
                          (start-progress-reporter! progress-reporter)
                          (call-with-connection-error-handling
                           uri
                           (lambda ()
                             (http-multiple-get uri
                                                handle-narinfo-response '()
                                                requests
                                                #:open-connection open-connection
                                                #:verify-certificate? #f))))))
         (stop-progress-reporter! progress-reporter)
         result))
      ((file #f)
       (let* ((base  (string-append (uri-path uri) "/"))
              (files (map (compose (cut string-append base <> ".narinfo")
                                   store-path-hash-part)
                          paths)))
         (filter-map (cut narinfo-from-file <> url) files)))
      (else
       (leave (G_ "~s: unsupported server URI scheme~%")
              (if uri (uri-scheme uri) url)))))

  (do-fetch (string->uri url)))

(define (cached-narinfo cache-url path)
  "Check locally if we have valid info about PATH coming from CACHE-URL.
Return two values: a Boolean indicating whether we have valid cached info, and
that info, which may be either #f (when PATH is unavailable) or the narinfo
for PATH."
  (define now
    (current-time time-monotonic))

  (define cache-file
    (narinfo-cache-file cache-url path))

  (catch 'system-error
    (lambda ()
      (call-with-input-file cache-file
        (lambda (p)
          (match (read p)
            (('narinfo ('version 2)
                       ('cache-uri cache-uri)
                       ('date date) ('ttl ttl) ('value #f))
             ;; A cached negative lookup.
             (if (obsolete? date now ttl)
                 (values #f #f)
                 (values #t #f)))
            (('narinfo ('version 2)
                       ('cache-uri cache-uri)
                       ('date date) ('ttl ttl) ('value value))
             ;; A cached positive lookup
             (if (obsolete? date now ttl)
                 (values #f #f)
                 (values #t (string->narinfo value cache-uri))))
            (('narinfo ('version v) _ ...)
             (values #f #f))
            ((? eof-object?)                      ;corrupt file
             (values #f #f))))))
    (lambda _
      (values #f #f))))

(define* (lookup-narinfos cache paths
                          #:key (open-connection guix:open-connection-for-uri)
                          (make-progress-reporter
                           (const progress-reporter/silent)))
  "Return the narinfos for PATHS, invoking the server at CACHE when no
information is available locally."
  (let-values (((cached missing)
                (fold2 (lambda (path cached missing)
                         (let-values (((valid? value)
                                       (cached-narinfo cache path)))
                           (if valid?
                               (if value
                                   (values (cons value cached) missing)
                                   (values cached missing))
                               (values cached (cons path missing)))))
                       '()
                       '()
                       paths)))
    (values (if (null? missing)
                cached
                (let ((missing (fetch-narinfos cache missing
                                               #:open-connection open-connection
                                               #:make-progress-reporter
                                               make-progress-reporter)))
                  (append cached (or missing '()))))
            (length missing))))

(define* (lookup-narinfos/diverse caches paths authorized?
                                  #:key (open-connection
                                         guix:open-connection-for-uri)
                                  (make-progress-reporter
                                   (const progress-reporter/silent)))
  "Look up narinfos for PATHS on all of CACHES, a list of URLS, in that order.
That is, when a cache lacks an AUTHORIZED? narinfo, look it up in the next
cache, and so on.

Return a list of narinfos for PATHS or a subset thereof.  The returned
narinfos are either AUTHORIZED?, or they claim a hash that matches an
AUTHORIZED? narinfo."
  (define (select-hit result)
    (lambda (path)
      (match (vhash-fold* cons '() path result)
        ((one)
         one)
        ((several ..1)
         (let ((authorized (find authorized? (reverse several))))
           (and authorized
                (find (cut equivalent-narinfo? <> authorized)
                      several)))))))

  (let loop ((caches caches)
             (paths  paths)
             (result vlist-null)                  ;path->narinfo vhash
             (hits   '()))                        ;paths
    (match paths
      (()                                         ;we're done
       ;; Now iterate on all the HITS, and return exactly one match for each
       ;; hit: the first narinfo that is authorized, or that has the same hash
       ;; as an authorized narinfo, in the order of CACHES.
       (filter-map (select-hit result) hits))
      (_
       (match caches
         ((cache rest ...)
          (let* ((narinfos (lookup-narinfos cache paths
                                            #:open-connection open-connection
                                            #:make-progress-reporter
                                            make-progress-reporter))
                 (definite (map narinfo-path (filter authorized? narinfos)))
                 (missing  (lset-difference string=? paths definite))) ;XXX: perf
            (loop rest missing
                  (fold vhash-cons result
                        (map narinfo-path narinfos) narinfos)
                  (append definite hits))))
         (()                                      ;that's it
          (filter-map (select-hit result) hits)))))))

(define %random-state
  (seed->random-state (+ (ash (cdr (gettimeofday)) 32) (getpid))))

(define-syntax-rule (with-timeout duration handler body ...)
  "Run BODY; when DURATION seconds have expired, call HANDLER, and run BODY
again.  If DURATION is #f, run BODY with no timeout."
  (let ((thunk (lambda () body ...)))
    (if duration
        (begin
          (sigaction SIGALRM
            (lambda (signum)
              (sigaction SIGALRM SIG_DFL)
              handler))
          (alarm duration)
          (call-with-values
              (lambda ()
                (let try ()
                  (catch 'system-error
                    thunk
                    (lambda args
                      ;; Before Guile v2.0.9-39-gfe51c7b, the SIGALRM triggers EINTR
                      ;; because of the bug at
                      ;; <http://lists.gnu.org/archive/html/guile-devel/2013-06/msg00050.html>.
                      ;; When that happens, try again.  Note: SA_RESTART cannot be
                      ;; used because of <http://bugs.gnu.org/14640>.
                      (if (= EINTR (system-error-errno args))
                          (begin
                            ;; Wait a little to avoid bursts.
                            (usleep (random 3000000 %random-state))
                            (try))
                          (apply throw args))))))
            (lambda result
              (alarm 0)
              (sigaction SIGALRM SIG_DFL)
              (apply values result))))
        (thunk))))

(define-syntax-rule (catch-system-error exp)
  (catch 'system-error
    (lambda () exp)
    (const #f)))

(define http-response-error?
  (let ((kind-and-args-exception?
         (exception-predicate &exception-with-kind-and-args)))
    (lambda (exception)
      "Return true if EXCEPTION denotes an error with the http response"
      (->bool
       (memq (exception-kind exception)
             '(bad-response bad-header bad-header-component))))))

(define %fetch-timeout
  ;; Number of seconds after which networking is considered "slow".
  5)

(define* (download-nar narinfo destination
                       #:key deduplicate? print-build-trace?
                       (fetch-timeout %fetch-timeout)
                       fast-decompression?
                       (open-connection-for-uri guix:open-connection-for-uri))
  "Download the nar prescribed in NARINFO, which is assumed to be authentic
and authorized, and write it to DESTINATION.  When DEDUPLICATE? is true, and
if DESTINATION is in the store, deduplicate its files.  Use
OPEN-CONNECTION-FOR-URI to open connections."
  (define destination-in-store?
    (string-prefix? (string-append (%store-prefix) "/")
                    destination))

  (define (dump-file/deduplicate* . args)
    ;; Make sure deduplication looks at the right store (necessary in test
    ;; environments).
    (apply dump-file/deduplicate
           (append args (list #:store (%store-prefix)))))

  (define (fetch uri)
    (case (uri-scheme uri)
      ((file)
       (let ((port (open-file (uri-path uri) "r0b")))
         (values port (stat:size (stat port)))))
      ((http https)
       ;; Test this with:
       ;;   sudo tc qdisc add dev eth0 root netem delay 1500ms
       ;; and then cancel with:
       ;;   sudo tc qdisc del dev eth0 root
       (with-timeout fetch-timeout
         (begin
           (warning (G_ "while fetching ~a: server is somewhat slow~%")
                    (uri->string uri))
           (warning (G_ "try `--no-substitutes' if the problem persists~%")))
         (let loop ((port  (open-connection-for-uri uri))
                    (attempt 0))
           (guard (c ((or (network-error? c)
                          (http-response-error? c))
                      (close-port port)

                      ;; Perform a single retry in the case of an error,
                      ;; mostly to mimic the behaviour of
                      ;; with-cached-connection
                      (if (= attempt 0)
                          (loop (open-connection-for-uri uri) 1)
                          (raise c))))
             (let ((port
                    response
                    (http-fetch uri #:text? #f
                                #:port port
                                #:keep-alive? #t
                                #:buffered? #f)))
               (values port
                       (response-content-length response)))))))
      (else
       (raise
        (formatted-message
         (G_ "unsupported substitute URI scheme: ~a~%")
         (uri->string uri))))))

  (define (try-fetch choices)
    (match choices
      (((uri compression file-size) rest ...)
       (guard (c ((and (pair? rest)
                       (or (http-get-error? c)
                           (network-error? c)))
                  (warning (G_ "download from '~a' failed, trying next URL~%")
                           (uri->string uri))
                  (try-fetch rest)))
         (let ((port download-size (fetch uri)))
           (unless print-build-trace?
             (format (current-error-port)
                     (G_ "Downloading ~a...~%") (uri->string uri)))
           (values port uri compression download-size))))
      (()
       (raise
        (formatted-message
         (G_ "no valid nar URLs for ~a at ~a~%")
         (narinfo-path narinfo)
         (narinfo-uri-base narinfo))))))

  ;; Delete DESTINATION first--necessary when starting over after a failed
  ;; download.
  (catch-system-error (delete-file-recursively destination))

  (let ((choices (narinfo-preferred-uris narinfo
                                         #:fast-decompression?
                                         fast-decompression?)))
    ;; 'guix publish' without '--cache' doesn't specify a Content-Length, so
    ;; DOWNLOAD-SIZE is #f in this case.
    (let* ((raw uri compression download-size (try-fetch choices))
           (progress
            (let* ((dl-size  (or download-size
                                 (and (equal? compression "none")
                                      (narinfo-size narinfo))))
                   (reporter (if print-build-trace?
                                 (progress-reporter/trace
                                  destination
                                  (uri->string uri) dl-size
                                  (current-error-port))
                                 (progress-reporter/file
                                  (uri->string uri) dl-size
                                  (current-error-port)
                                  #:abbreviation nar-uri-abbreviation))))
              ;; Keep RAW open upon completion so we can later reuse
              ;; the underlying connection.  Pass the download size so
              ;; that this procedure won't block reading from RAW.
              (progress-report-port reporter raw
                                    #:close? #f
                                    #:download-size dl-size)))
           (input pids
                  ;; NOTE: This 'progress' port of current process will be
                  ;; closed here, while the child process doing the
                  ;; reporting will close it upon exit.
                  (decompressed-port (string->symbol compression)
                                     progress))

           ;; Compute the actual nar hash as we read it.
           (algorithm expected (narinfo-hash-algorithm+value narinfo))
           (hashed get-hash (open-hash-input-port algorithm input)))

      ;; Unpack the Nar at INPUT into DESTINATION.
      (restore-file hashed destination
                    #:dump-file (if (and destination-in-store?
                                         deduplicate?)
                                    dump-file/deduplicate*
                                    dump-file))
      (close-port hashed)
      (close-port input)

      ;; Wait for the reporter to finish.
      (every (compose zero? cdr waitpid) pids)

      (values expected
              (get-hash)))))

;;; Local Variables:
;;; eval: (put 'with-timeout 'scheme-indent-function 1)
;;; End:

;;; substitutes.scm ends here
