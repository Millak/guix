;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2024 Ludovic Courtès <ludo@gnu.org>
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

;;; Commentary:
;;;
;;; This is the "binary substituter".  It is invoked by the daemon do check
;;; for the existence of available "substitutes" (pre-built binaries), and to
;;; actually use them as a substitute to building things locally.
;;;
;;; If possible, substitute a binary for the requested store path, using a Nix
;;; "binary cache".  This program implements the Nix "substituter" protocol.
;;;
;;; Code:

(define-module (guix scripts substitute)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix narinfo)
  #:use-module (guix store)
  #:use-module (guix substitutes)
  #:use-module (guix utils)
  #:use-module (guix config)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module ((guix serialization) #:select (restore-file dump-file))
  #:autoload   (guix scripts discover) (read-substitute-urls)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix cache)
  #:use-module (gcrypt pk-crypto)
  #:use-module (guix pki)
  #:autoload   (guix build utils) (mkdir-p delete-file-recursively)
  #:use-module ((guix build download)
                #:select (uri-abbreviation
                          (open-connection-for-uri
                           . guix:open-connection-for-uri)))
  #:use-module (guix progress)
  #:use-module ((guix build syscalls)
                #:select (set-thread-name mkdtemp!))
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-71)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (guix http-client)
  #:export (%allow-unauthenticated-substitutes?
            %allow-unsafe-substitute-uris?
            %reply-file-descriptor

            substitute-urls
            guix-substitute))

(define %narinfo-expired-cache-entry-removal-delay
  ;; How often we want to remove files corresponding to expired cache entries.
  (* 5 24 3600))

(define (warn-about-missing-authentication)
  (warning (G_ "authentication and authorization of substitutes \
disabled!~%"))
  #t)

(define %allow-unauthenticated-substitutes?
  ;; Whether to allow unchecked substitutes.  This is useful for testing
  ;; purposes, and should be avoided otherwise.
  (make-parameter
   (and=> (getenv "GUIX_ALLOW_UNAUTHENTICATED_SUBSTITUTES")
          (cut string-ci=? <> "yes"))))

(define %allow-unsafe-substitute-uris?
  ;; Whether to allow substitutes with a "file://" URI.  These are useful for
  ;; testing purposes but should never be allowed otherwise, as the URIs
  ;; contained in narinfos are not signed and could point to any
  ;; attacker-controlled location.  Use GUIX_ALLOW_UNAUTHENTICATED_SUBSTITUTES
  ;; as a way to tell when we're testing.
  (make-parameter (%allow-unauthenticated-substitutes?)))

(define %allow-unsafe-narinfo-uris?
  ;; Like %allow-unsafe-substitute-uris?, but affecting the urls used to fetch
  ;; narinfos.
  (make-parameter (%allow-unauthenticated-substitutes?)))

(define (at-most max-length lst)
  "If LST is shorter than MAX-LENGTH, return it and the empty list; otherwise
return its MAX-LENGTH first elements and its tail."
  (let loop ((len 0)
             (lst lst)
             (result '()))
    (match lst
      (()
       (values (reverse result) '()))
      ((head . tail)
       (if (>= len max-length)
           (values (reverse result) lst)
           (loop (+ 1 len) tail (cons head result)))))))

(define (lookup-narinfo caches path authorized?)
  "Return the narinfo for PATH in CACHES, or #f when no substitute for PATH
was found."
  (match (lookup-narinfos/diverse
          caches (list path) authorized?
          #:open-connection open-connection-for-uri/cached)
    ((answer) answer)
    (_        #f)))

(define (cached-narinfo-expiration-time file)
  "Return the expiration time for FILE, which is a cached narinfo."
  (define max-ttl
    ;; Upper bound on the TTL used to avoid keeping around cached narinfos for
    ;; too long, which makes the cache bigger and more expensive to traverse
    ;; when deleting old entries.
    (* 2 24 60 60))

  (catch 'system-error
    (lambda ()
      (call-with-input-file file
        (lambda (port)
          (match (read port)
            (('narinfo ('version 2) ('cache-uri uri)
                       ('date date) ('ttl ttl) ('value #f))
             (+ date (min ttl max-ttl)))
            (('narinfo ('version 2) ('cache-uri uri)
                       ('date date) ('ttl ttl) ('value value))
             (+ date (min ttl max-ttl)))
            (x
             0)))))
    (lambda args
      ;; FILE may have been deleted.
      0)))

(define (narinfo-cache-directories directory)
  "Return the list of narinfo cache directories (one per cache URL.)"
  (map (cut string-append directory "/" <>)
       (scandir %narinfo-cache-directory
                (lambda (item)
                  (and (not (member item '("." "..")))
                       (file-is-directory?
                        (string-append %narinfo-cache-directory
                                       "/" item)))))))

(define* (cached-narinfo-files #:optional
                               (directory %narinfo-cache-directory))
  "Return the list of cached narinfo files under DIRECTORY."
  (append-map (lambda (directory)
                (map (cut string-append directory "/" <>)
                     (scandir directory
                              (lambda (file)
                                (= (string-length file) 32)))))
              (narinfo-cache-directories directory)))

(define-syntax with-networking
  (syntax-rules ()
    "Catch DNS lookup errors and TLS errors and gracefully exit."
    ;; Note: no attempt is made to catch other networking errors, because DNS
    ;; lookup errors are typically the first one, and because other errors are
    ;; a subset of `system-error', which is harder to filter.
    ((_ exp ...)
     ;; Use a pre-unwind handler so that re-throwing preserves useful
     ;; backtraces.  'with-throw-handler' works for Guile 2.2 and 3.0.
     (with-throw-handler #t
       (lambda () exp ...)
       (match-lambda*
         (('getaddrinfo-error error)
          (leave (G_ "host name lookup error: ~a~%")
                 (gai-strerror error)))
         (('gnutls-error error proc . rest)
          (let ((error->string (module-ref (resolve-interface '(gnutls))
                                           'error->string)))
            (leave (G_ "TLS error in procedure '~a': ~a~%")
                   proc (error->string error))))
         (args
          (apply throw args)))))))


;;;
;;; Help.
;;;

(define (show-help)
  (display (G_ "Usage: guix substitute OPTION [ARGUMENT]...
Internal tool to substitute a pre-built binary to a local build.\n"))
  (display (G_ "
      --query            report on the availability of substitutes for the
                         store file names passed on the standard input"))
  (display (G_ "
      --substitute STORE-FILE DESTINATION
                         download STORE-FILE and store it as a Nar in file
                         DESTINATION"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))



;;;
;;; Daemon/substituter protocol.
;;;

;; Whether to initially prefer fast decompression or not
(define %default-fast-decompression? #t)

(define (call-with-cpu-usage-monitoring thunk)
  (let ((before (times)))
    (call-with-values thunk
      (lambda vals
        (let* ((after (times))
               (usage (if (= (tms:clock after) (tms:clock before))
                          0
                          (/ (- (tms:utime after) (tms:utime before))
                             (- (tms:clock after) (tms:clock before))
                             1.))))
          (apply values (append vals (list usage))))))))

(define-syntax-rule (with-cpu-usage-monitoring exp ...)
  "Evaluate EXP...  Return its CPU usage as a fraction between 0 and 1."
  (call-with-cpu-usage-monitoring (lambda () exp ...)))

(define (display-narinfo-data port narinfo)
  "Write to PORT the contents of NARINFO in the format expected by the
daemon."
  (format port "~a\n~a\n~a\n"
          (narinfo-path narinfo)
          (or (and=> (narinfo-deriver narinfo)
                     (cute string-append (%store-prefix) "/" <>))
              "")
          (length (narinfo-references narinfo)))
  (for-each (cute format port "~a/~a~%" (%store-prefix) <>)
            (narinfo-references narinfo))

  (let ((uri compression file-size
             (narinfo-best-uri narinfo
                               #:fast-decompression?
                               %default-fast-decompression?)))
    (format port "~a\n~a\n"
            (or file-size 0)
            (or (narinfo-size narinfo) 0))))

(define* (process-query port command
                        #:key cache-urls acl)
  "Reply on PORT to COMMAND, a query as written by the daemon to this process's
standard input.  Use ACL as the access-control list against which to check
authorized substitutes."
  (define valid?
    (if (%allow-unauthenticated-substitutes?)
        (begin
          (warn-about-missing-authentication)

          (const #t))
        (lambda (obj)
          (valid-narinfo? obj acl))))

  (define* (make-progress-reporter total #:key url)
    (define done 0)

    (define (report-progress)
      (erase-current-line (current-error-port)) ;erase current line
      (force-output (current-error-port))
      (format (current-error-port)
              (G_ "looking for substitutes on '~a'... ~5,1f%")
              url (* 100. (/ done total)))
      (set! done (+ 1 done)))

    (progress-reporter
     (start report-progress)
     (report report-progress)
     (stop (lambda ()
             (newline (current-error-port))))))

  (match (string-tokenize command)
    (("have" paths ..1)
     ;; Return the subset of PATHS available in CACHE-URLS.
     (let ((substitutable (lookup-narinfos/diverse
                           cache-urls paths valid?
                           #:open-connection open-connection-for-uri/cached
                           #:make-progress-reporter make-progress-reporter)))
       (for-each (lambda (narinfo)
                   (format port "~a~%" (narinfo-path narinfo)))
                 substitutable)
       (newline port)))
    (("info" paths ..1)
     ;; Reply info about PATHS if it's in CACHE-URLS.
     (let ((substitutable (lookup-narinfos/diverse
                           cache-urls paths valid?
                           #:open-connection open-connection-for-uri/cached
                           #:make-progress-reporter make-progress-reporter)))
       (for-each (cut display-narinfo-data port <>) substitutable)
       (newline port)))
    (wtf
     (error "unknown `--query' command" wtf))))

(define %max-cached-connections
  ;; Maximum number of connections kept in cache by
  ;; 'open-connection-for-uri/cached'.
  16)

(define %fetch-timeout
  ;; Number of seconds after which networking is considered "slow".
  5)

(define open-connection-for-uri/cached
  (let ((cache '()))
    (lambda* (uri #:key fresh? (timeout %fetch-timeout) verify-certificate?)
      "Return a connection for URI, possibly reusing a cached connection.
When FRESH? is true, delete any cached connections for URI and open a new one.
Return #f if URI's scheme is 'file' or #f.

When true, TIMEOUT is the maximum number of seconds to wait for
connection establishment.  When VERIFY-CERTIFICATE? is true, verify HTTPS
server certificates."
      (define host (uri-host uri))
      (define scheme (uri-scheme uri))
      (define key (list host scheme (uri-port uri)))

      (and (not (memq scheme '(file #f)))
           (match (assoc-ref cache key)
             (#f
              ;; Open a new connection to URI and evict old entries from
              ;; CACHE, if any.
              (let ((socket
                     (guix:open-connection-for-uri
                      uri
                      #:verify-certificate? verify-certificate?
                      #:timeout timeout))
                    (new-cache evicted
                               (at-most (- %max-cached-connections 1) cache)))
                (for-each (match-lambda
                            ((_ . port)
                             (false-if-exception (close-port port))))
                          evicted)
                (set! cache (alist-cons key socket new-cache))
                socket))
             (socket
              (if (or fresh? (port-closed? socket))
                  (begin
                    (false-if-exception (close-port socket))
                    (set! cache (alist-delete key cache))
                    (open-connection-for-uri/cached uri #:timeout timeout
                                                    #:verify-certificate?
                                                    verify-certificate?))
                  (begin
                    ;; Drain input left from the previous use.
                    (drain-input socket)
                    socket))))))))

(define (uri-safe? uri)
  "Return a boolean indicating whether URI, which is either a uri or its
string representation, is generally safe to use without requiring any trust."
  (let ((uri (if (uri? uri)
                 uri
                 (string->uri uri))))
    (and uri
         (case (uri-scheme uri)
           ((#f file) #f)
           (else #t)))))

(define (narinfo-uris-safe? narinfo)
  (or (%allow-unsafe-substitute-uris?)
      (every uri-safe? (narinfo-uris narinfo))))

(define* (process-substitution/fallback narinfo destination
                                        #:key cache-urls acl
                                        deduplicate? print-build-trace?
                                        fast-decompression?)
  "Attempt to substitute NARINFO, which is assumed to be authorized or
equivalent, by trying to download its nar from each entry in CACHE-URLS.

This can be less efficient than 'lookup-narinfo', which stops at the first
entry that provides a valid narinfo, but it makes sure we eventually find a
way to download the nar."
  ;; Note: Keep NARINFO's uri-base in CACHE-URLS: that lets us retry in case
  ;; this was a transient issue.
  (let loop ((cache-urls cache-urls))
    (match cache-urls
      (()
       ;; Failure, so return two values like download-nar
       (values #f #f))
      ((cache-url rest ...)
       (match (lookup-narinfos cache-url
                               (list (narinfo-path narinfo))
                               #:open-connection
                               open-connection-for-uri/cached)
         ((alternate)
          (if (and (narinfo-uris-safe? alternate)
                   (or (equivalent-narinfo? narinfo alternate)
                       (valid-narinfo? alternate acl)
                       (%allow-unauthenticated-substitutes?)))
              (guard (c ((or (http-get-error? c)
                             (network-error? c))
                         (when (http-get-error? c)
                           (warning (G_ "download from '~a' failed: ~a, ~s~%")
                                    (uri->string (http-get-error-uri c))
                                    (http-get-error-code c)
                                    (http-get-error-reason c)))
                         (loop rest)))
                (download-nar alternate destination
                              #:deduplicate? deduplicate?
                              #:print-build-trace? print-build-trace?
                              #:fast-decompression?
                              fast-decompression?
                              #:open-connection-for-uri
                              open-connection-for-uri/cached
                              #:keep-alive? #t))
              (loop rest)))
         (()
          (loop rest)))))))

(define* (process-substitution store-item destination
                               #:key cache-urls acl
                               deduplicate? print-build-trace?
                               fast-decompression?)
  "Substitute STORE-ITEM (a store file name) from CACHE-URLS, and write it to
DESTINATION as a nar file.  Verify the substitute against ACL, and verify its
hash against what appears in the narinfo.  When DEDUPLICATE? is true, and if
DESTINATION is in the store, deduplicate its files.  Print a status line to
PORT."
  (define narinfo
    (lookup-narinfo cache-urls store-item
                    (if (%allow-unauthenticated-substitutes?)
                        (const #t)
                        (cut valid-narinfo? <> acl))))

  (define (fallback)
    (process-substitution/fallback narinfo destination
                                   #:cache-urls cache-urls
                                   #:acl acl
                                   #:deduplicate? deduplicate?
                                   #:print-build-trace?
                                   print-build-trace?
                                   #:fast-decompression?
                                   fast-decompression?))

  (unless narinfo
    (raise
     (formatted-message
      (G_ "no valid substitute for '~a'~%")
      store-item)))

  (let ((expected-hash
         actual-hash
         (cond
          ((narinfo-uris-safe? narinfo)
           (guard
               (c ((or (http-get-error? c)
                       (network-error? c))
                   (when (http-get-error? c)
                     (warning (G_ "download from '~a' failed: ~a, ~s~%")
                              (uri->string (http-get-error-uri c))
                              (http-get-error-code c)
                              (http-get-error-reason c)))
                   (format
                    (current-error-port)
                    (G_ "retrying download of '~a' with other substitute URLs...~%")
                    store-item)
                   (fallback)))
             (download-nar narinfo destination
                           #:deduplicate? deduplicate?
                           #:print-build-trace? print-build-trace?
                           #:fast-decompression? fast-decompression?
                           #:open-connection-for-uri
                           open-connection-for-uri/cached
                           #:keep-alive? #t)))
          (else
           (fallback)))))
    (values narinfo
            expected-hash
            actual-hash)))


;;;
;;; Entry point.
;;;

(define (check-acl-initialized)
  "Warn if the ACL is uninitialized."
  (define (singleton? acl)
    ;; True if ACL contains just the user's public key.
    (and (file-exists? %public-key-file)
         (let ((key (call-with-input-file %public-key-file
                      (compose string->canonical-sexp
                               read-string))))
           (match acl
             ((thing)
              (equal? (canonical-sexp->string thing)
                      (canonical-sexp->string key)))
             (_
              #f)))))

  (let ((acl (acl->public-keys (current-acl))))
    (when (or (null? acl) (singleton? acl))
      (warning (G_ "ACL for archive imports seems to be uninitialized, \
substitutes may be unavailable\n")))))

(define (daemon-options)
  "Return a list of name/value pairs denoting build daemon options."
  (define %not-newline
    (char-set-complement (char-set #\newline)))

  (match (getenv "_NIX_OPTIONS")
    (#f                           ;should not happen when called by the daemon
     '())
    (newline-separated
     ;; Here we get something of the form "OPTION1=VALUE1\nOPTION2=VALUE2\n".
     (filter-map (lambda (option=value)
                   (match (string-index option=value #\=)
                     (#f                          ;invalid option setting
                      #f)
                     (equal-sign
                      (cons (string-take option=value equal-sign)
                            (string-drop option=value (+ 1 equal-sign))))))
                 (string-tokenize newline-separated %not-newline)))))

(define find-daemon-option
  (let ((options (delay (daemon-options))))
    (lambda (option)
      "Return the value of build daemon option OPTION, or #f if it could not be
found."
      (assoc-ref (force options) option))))

(define (assert-safe-uris uris)
  (unless (or (%allow-unsafe-narinfo-uris?)
              (every uri-safe? uris))
    (leave (G_ "unsafe or invalid URI in ~S~%") uris))
  uris)

(define %default-substitute-urls
  (match (or (and=> (find-daemon-option "untrusted-substitute-urls") ;client
                    (compose assert-safe-uris string-tokenize))
             (and=> (find-daemon-option "substitute-urls") ;admin
                    string-tokenize))
    ((urls ...)
     urls)
    (#f
     ;; This can only happen when this script is not invoked by the
     ;; daemon.
     '("http://bordeaux.guix.gnu.org"
       "http://ci.guix.gnu.org"))))

;; In order to prevent using large number of discovered local substitute
;; servers, limit the local substitute urls list size.
(define %max-substitute-urls 50)

(define* (randomize-substitute-urls urls
                                    #:key
                                    (max %max-substitute-urls))
  "Return a list containing MAX urls from URLS, picked randomly. If URLS list
is shorter than MAX elements, then it is directly returned."
  (define (random-item list)
    (list-ref list (random (length list))))

  (if (<= (length urls) max)
      urls
      (let loop ((res '())
                 (urls urls))
        (if (eq? (length res) max)
            res
            (let ((url (random-item urls)))
              (loop (cons url res) (delete url urls)))))))

(define %local-substitute-urls
  ;; If the following option is passed to the daemon, use the substitutes list
  ;; provided by "guix discover" process.
  (let* ((option (find-daemon-option "discover"))
         (discover? (and option (string=? option "true"))))
    (if discover?
        (randomize-substitute-urls (filter uri-safe?
                                           (read-substitute-urls)))
        '())))

(define substitute-urls
  ;; List of substitute URLs.
  (make-parameter (append %local-substitute-urls
                          %default-substitute-urls)))

(define (client-terminal-columns)
  "Return the number of columns in the client's terminal, if it is known, or a
default value."
  (or (and=> (or (find-daemon-option "untrusted-terminal-columns")
                 (find-daemon-option "terminal-columns"))
             (lambda (str)
               (let ((number (string->number str)))
                 (and number (max 20 (- number 1))))))
      80))

(define (validate-uri uri)
  (unless (string->uri uri)
    (leave (G_ "~a: invalid URI~%") uri)))

(define %reply-file-descriptor
  ;; The file descriptor where replies to the daemon must be sent, or #f to
  ;; use the current output port instead.
  (make-parameter 4))

;; XXX: copied from (guix utils)
(define (call-with-temporary-directory-in directory proc)
  "Call PROC with a name of a temporary directory; close the directory and
delete it when leaving the dynamic extent of this call."
  (let* ((template  (string-append directory "/guix-directory-"
                                   ;; In case the temporary directory may be
                                   ;; garbage collected, including our pid
                                   ;; ensures that no live process can mistake
                                   ;; our freshly-created instance for their
                                   ;; old deleted one.
                                   (number->string (getpid))
                                   ".XXXXXX"))
         (tmp-dir   (mkdtemp! template)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc tmp-dir))
      (lambda ()
        (false-if-exception (delete-file-recursively tmp-dir))))))

(define (rename-file* from to)
  "Like `rename-file', but temporarily make FROM user-writable if it is an
unwritable directory, restoring its original permissions afterward.

This requires that, in the case that FROM is a directory, it is readable."
  (let ((st (lstat from)))
    (match (stat:type st)
      ('directory
       (catch 'system-error
         (lambda ()
           (rename-file from to))
         (lambda args
           (cond
            ((= (system-error-errno args) EACCES)
             ;; Assume it's because FROM is not writable.  Open FROM
             ;; so we can use fchmod instead of risking following a symbolic
             ;; link.
             (call-with-port (open from (logior
                                         ;; XXX: requires that FROM is
                                         ;; readable.
                                         O_RDONLY
                                         O_NOFOLLOW
                                         O_DIRECTORY))
               (lambda (port)
                 ;; stat again using fstat this time just in case what we
                 ;; opened isn't the directory we originally lstat'ed.
                 (let ((perms (stat:perms (stat port))))
                   (chmod port (logior perms #o200))
                   (rename-file from to)
                   (chmod port perms)))))
            (else (apply throw args))))))
      (_ (rename-file from to)))))

(define-command (guix-substitute . args)
  (category internal)
  (synopsis "implement the build daemon's substituter protocol")

  (match args
    ((or ("-V") ("--version"))
     (show-version-and-exit "guix substitute"))
    ((or ("-h") ("--help") ())
     (leave-on-EPIPE (show-help))
     (exit 0))
    (_ #t))

  (define print-build-trace?
    (match (or (find-daemon-option "untrusted-print-extended-build-trace")
               (find-daemon-option "print-extended-build-trace"))
      (#f #f)
      ((= string->number number) (> number 0))
      (_ #f)))

  (define deduplicate?
    (find-daemon-option "deduplicate"))

  (define reply-port
    ;; Port used to reply to the daemon.
    (if (%reply-file-descriptor)
        (fdopen (%reply-file-descriptor) "wl")
        (current-output-port)))

  (mkdir-p %narinfo-cache-directory)
  (maybe-remove-expired-cache-entries %narinfo-cache-directory
                                      cached-narinfo-files
                                      #:entry-expiration
                                      cached-narinfo-expiration-time
                                      #:cleanup-period
                                      %narinfo-expired-cache-entry-removal-delay)
  (check-acl-initialized)

  ;; Sanity-check SUBSTITUTE-URLS so we can provide a meaningful error
  ;; message.
  (for-each validate-uri (substitute-urls))

  ;; Attempt to install the client's locale so that messages are suitably
  ;; translated.  LC_CTYPE must be a UTF-8 locale; it's the case by default
  ;; so don't change it.
  (match (or (find-daemon-option "untrusted-locale")
             (find-daemon-option "locale"))
    (#f     #f)
    (locale (false-if-exception (setlocale LC_MESSAGES locale))))

  (catch 'system-error
    (lambda ()
      (set-thread-name "guix substitute"))
    (const #t))                                   ;GNU/Hurd lacks 'prctl'

  (with-networking
   (with-error-handling                           ; for signature errors
     (match args
       (("--query")
        (let ((acl (current-acl)))
          (let loop ((command (read-line)))
            (or (eof-object? command)
                (begin
                  (process-query reply-port command
                                 #:cache-urls (substitute-urls)
                                 #:acl acl)
                  (loop (read-line)))))))
       (("--substitute")
        ;; Download STORE-PATH and store it as a Nar in file DESTINATION.
        ;; Specify the number of columns of the terminal so the progress
        ;; report displays nicely.
        (parameterize ((current-terminal-columns (client-terminal-columns)))
          (let loop ((fast-decompression?
                      %default-fast-decompression?))
            (match (read-line)
              ((? eof-object?)
               #t)
              ((= string-tokenize ("substitute" store-path destination))
               (let ((narinfo
                      expected-hash
                      actual-hash
                      cpu-usage
                      (with-cpu-usage-monitoring
                       ;; Restore inside a temporary directory until the hash
                       ;; can be verified so that no dangling references a
                       ;; user may have laying around will point to
                       ;; attacker-controlled content in the meantime.
                       (call-with-temporary-directory-in (dirname destination)
                         (lambda (temp-directory)
                           (let* ((temp-destination
                                   (string-append temp-directory "/restored"))
                                  (narinfo
                                   expected-hash
                                   actual-hash
                                   (process-substitution
                                    store-path temp-destination
                                    #:cache-urls (substitute-urls)
                                    #:acl (current-acl)
                                    #:deduplicate? deduplicate?
                                    #:print-build-trace?
                                    print-build-trace?
                                    #:fast-decompression?
                                    fast-decompression?)))
                             (when (and expected-hash actual-hash
                                        (bytevector=? actual-hash
                                                      expected-hash))
                               (catch 'system-error
                                 (lambda ()
                                   (delete-file-recursively destination))
                                 (const #f))
                               (rename-file* temp-destination destination))
                             (values narinfo expected-hash actual-hash)))))))

                 (if expected-hash
                     (begin
                       ;; Skip a line after what 'progress-reporter/file'
                       ;; printed, and another one to visually separate
                       ;; substitutions.  When PRINT-BUILD-TRACE? is true,
                       ;; leave it up to (guix status) to prettify things.
                       (newline (current-error-port))
                       (unless print-build-trace?
                         (newline (current-error-port)))

                       ;; Check whether we got the data announced in NARINFO.
                       (if (bytevector=? actual-hash expected-hash)
                           ;; Tell the daemon that we're done.
                           (format reply-port "success ~a ~a~%"
                                   (narinfo-hash narinfo) (narinfo-size narinfo))
                           ;; The actual data has a different hash than that in NARINFO.
                           (format reply-port "hash-mismatch ~a ~a ~a~%"
                                   (hash-algorithm-name
                                    (narinfo-hash-algorithm+value narinfo))
                                   (bytevector->nix-base32-string expected-hash)
                                   (bytevector->nix-base32-string actual-hash)))

                       ;; Create a hysteresis: depending on CPU usage, favor
                       ;; compression methods with faster decompression (like
                       ;; ztsd) or methods with better compression ratios
                       ;; (like lzip).  This stems from the observation that
                       ;; substitution can be CPU-bound when high-speed
                       ;; networks are used:
                       ;; <https://lists.gnu.org/archive/html/guix-devel/2020-12/msg00177.html>.
                       ;; To simulate "slow" networking or changing
                       ;; conditions, run:
                       ;;   sudo tc qdisc add dev eno1 root tbf rate 512kbit latency 50ms burst 1540
                       ;; and then cancel with:
                       ;;   sudo tc qdisc del dev eno1 root
                       (loop (cond
                              ;; Whether to prefer fast decompression over
                              ;; good compression ratios.  This serves in
                              ;; particular to choose between lzip (high
                              ;; compression ratio but low decompression
                              ;; throughput) and zstd (lower compression ratio
                              ;; but high decompression throughput).
                              ((> cpu-usage .8) #t)
                              ((< cpu-usage .2) #f)
                              (else fast-decompression?))))
                     (begin
                       (report-error (G_ "failed to find alternative substitute for '~a'~%")
                                     (narinfo-path narinfo))
                       (display "not-found\n" reply-port)

                       (loop fast-decompression?)))))))))
       (opts
        (leave (G_ "~a: unrecognized options~%") opts))))))

;;; Local Variables:
;;; eval: (put 'with-redirected-error-port 'scheme-indent-function 0)
;;; eval: (put 'call-with-temporary-directory-in 'scheme-indent-function 1)
;;; End:

;;; substitute.scm ends here
