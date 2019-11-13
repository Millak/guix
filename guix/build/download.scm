;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (guix build download)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module ((web client) #:hide (open-socket-for-uri))
  #:use-module (web response)
  #:use-module (guix base64)
  #:use-module (guix ftp-client)
  #:use-module (guix build utils)
  #:use-module (guix progress)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:autoload   (ice-9 ftw) (scandir)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (open-socket-for-uri
            open-connection-for-uri
            http-fetch
            %x509-certificate-directory
            close-connection
            resolve-uri-reference
            maybe-expand-mirrors
            url-fetch
            byte-count->string
            uri-abbreviation
            nar-uri-abbreviation
            store-path-abbreviation))

;;; Commentary:
;;;
;;; Fetch data such as tarballs over HTTP or FTP (builder-side code).
;;;
;;; Code:

(define %http-receive-buffer-size
  ;; Size of the HTTP receive buffer.
  65536)

(define* (ellipsis #:optional (port (current-output-port)))
  "Make a rough guess at whether Unicode's HORIZONTAL ELLIPSIS can be written
in PORT's encoding, and return either that or ASCII dots."
  (if (equal? (port-encoding port) "UTF-8")
      "…"
      "..."))

(define* (store-path-abbreviation store-path #:optional (prefix-length 6))
  "If STORE-PATH is the file name of a store entry, return an abbreviation of
STORE-PATH for display, showing PREFIX-LENGTH characters of the hash.
Otherwise return STORE-PATH."
  (if (string-prefix? (%store-directory) store-path)
      (let ((base (basename store-path)))
        (string-append (string-take base prefix-length)
                       (ellipsis)
                       (string-drop base 32)))
      store-path))

(define* (uri-abbreviation uri #:optional (max-length 42))
  "If URI's string representation is larger than MAX-LENGTH, return an
abbreviation of URI showing the scheme, host, and basename of the file."
  (define uri-as-string
    (uri->string uri))

  (define (elide-path)
    (let* ((path   (uri-path uri))
           (base   (basename path))
           (prefix (string-append (symbol->string (uri-scheme uri)) "://"

                                  ;; `file' URIs have no host part.
                                  (or (uri-host uri) "")

                                  (string-append "/" (ellipsis) "/"))))
      (if (> (+ (string-length prefix) (string-length base)) max-length)
          (string-append prefix (ellipsis)
                         (string-drop base (quotient (string-length base) 2)))
          (string-append prefix base))))

  (if (> (string-length uri-as-string) max-length)
      (let ((short (elide-path)))
        (if (< (string-length short) (string-length uri-as-string))
            short
            uri-as-string))
      uri-as-string))

(define (nar-uri-abbreviation uri)
  "Abbreviate URI, which is assumed to be the URI of a nar as served by Hydra
and 'guix publish', something like
\"http://example.org/nar/1ldrllwbna0aw5z8kpci4fsvbd2w8cw4-texlive-bin-2015\"."
  (let* ((uri  (if (string? uri) (string->uri uri) uri))
         (path (basename (uri-path uri))))
    (if (and (> (string-length path) 33)
             (char=? (string-ref path 32) #\-))
        (string-drop path 33)
        path)))

(define* (ftp-fetch uri file #:key timeout print-build-trace?)
  "Fetch data from URI and write it to FILE.  Return FILE on success.  Bail
out if the connection could not be established in less than TIMEOUT seconds."
  (let* ((conn (match (and=> (uri-userinfo uri)
                             (cut string-split <> #\:))
                 (((? string? user))
                  (ftp-open (uri-host uri) #:timeout timeout
                                           #:username user))
                 (((? string? user) (? string? pass))
                  (ftp-open (uri-host uri) #:timeout timeout
                                           #:username user
                                           #:password pass))
                 (_ (ftp-open (uri-host uri) #:timeout timeout))))
         (size (false-if-exception (ftp-size conn (uri-path uri))))
         (in   (ftp-retr conn (basename (uri-path uri))
                         (dirname (uri-path uri))
                         #:timeout timeout)))
    (call-with-output-file file
      (lambda (out)
        (dump-port* in out
                    #:buffer-size %http-receive-buffer-size
                    #:reporter
                    (if print-build-trace?
                        (progress-reporter/trace
                         file (uri->string uri) size)
                        (progress-reporter/file
                         (uri-abbreviation uri) size)))))

    (ftp-close conn)
    (unless print-build-trace?
      (newline))
    file))

;; Autoload GnuTLS so that this module can be used even when GnuTLS is
;; not available.  At compile time, this yields "possibly unbound
;; variable" warnings, but these are OK: we know that the variables will
;; be bound if we need them, because (guix download) adds GnuTLS as an
;; input in that case.

;; XXX: Use this hack instead of #:autoload to avoid compilation errors.
;; See <http://bugs.gnu.org/12202>.
(module-autoload! (current-module)
                  '(gnutls)
                  '(gnutls-version make-session connection-end/client))

(define %tls-ports
  ;; Mapping of session record ports to the underlying file port.
  (make-weak-key-hash-table))

(define (register-tls-record-port record-port port)
  "Hold a weak reference from RECORD-PORT to PORT, where RECORD-PORT is a TLS
session record port using PORT as its underlying communication port."
  (hashq-set! %tls-ports record-port port))

(define %x509-certificate-directory
  ;; The directory where X.509 authority PEM certificates are stored.
  (make-parameter (or (getenv "GUIX_TLS_CERTIFICATE_DIRECTORY")
                      (getenv "SSL_CERT_DIR")     ;like OpenSSL
                      "/etc/ssl/certs")))

(define (set-certificate-credentials-x509-trust-file!* cred file format)
  "Like 'set-certificate-credentials-x509-trust-file!', but without the file
name decoding bug described at
<https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26948#17>."
  (let ((data (call-with-input-file file get-bytevector-all)))
    (set-certificate-credentials-x509-trust-data! cred data format)))

(define (make-credendials-with-ca-trust-files directory)
  "Return certificate credentials with X.509 authority certificates read from
DIRECTORY.  Those authority certificates are checked when
'peer-certificate-status' is later called."
  (let ((cred  (make-certificate-credentials))
        (files (or (scandir directory
                            (lambda (file)
                              (string-suffix? ".pem" file)))
                   '())))
    (for-each (lambda (file)
                (let ((file (string-append directory "/" file)))
                  ;; Protect against dangling symlinks.
                  (when (file-exists? file)
                    (set-certificate-credentials-x509-trust-file!*
                     cred file
                     x509-certificate-format/pem))))
              (or files '()))
    cred))

(define (peer-certificate session)
  "Return the certificate of the remote peer in SESSION."
  (match (session-peer-certificate-chain session)
    ((first _ ...)
     (import-x509-certificate first x509-certificate-format/der))))

(define (assert-valid-server-certificate session server)
  "Return #t if the certificate of the remote peer for SESSION is a valid
certificate for SERVER, where SERVER is the expected host name of peer."
  (define cert
    (peer-certificate session))

  ;; First check whether the server's certificate matches SERVER.
  (unless (x509-certificate-matches-hostname? cert server)
    (throw 'tls-certificate-error 'host-mismatch cert server))

  ;; Second check its validity and reachability from the set of authority
  ;; certificates loaded via 'set-certificate-credentials-x509-trust-file!'.
  (match (peer-certificate-status session)
    (()                                           ;certificate is valid
     #t)
    ((statuses ...)
     (throw 'tls-certificate-error 'invalid-certificate cert server
            statuses))))

(define (print-tls-certificate-error port key args default-printer)
  "Print the TLS certificate error represented by ARGS in an intelligible
way."
  (match args
    (('host-mismatch cert server)
     (format port
             "X.509 server certificate for '~a' does not match: ~a~%"
             server (x509-certificate-dn cert)))
    (('invalid-certificate cert server statuses)
     (format port
             "X.509 certificate of '~a' could not be verified:~%~{  ~a~%~}"
             server
             (map certificate-status->string statuses)))))

(set-exception-printer! 'tls-certificate-error
                        print-tls-certificate-error)

(define* (tls-wrap port server #:key (verify-certificate? #t))
  "Return PORT wrapped in a TLS connection to SERVER.  SERVER must be a DNS
host name without trailing dot."
  (define (log level str)
    (format (current-error-port)
            "gnutls: [~a|~a] ~a" (getpid) level str))

  (let ((session  (make-session connection-end/client))
        (ca-certs (%x509-certificate-directory)))

    ;; Some servers such as 'cloud.github.com' require the client to support
    ;; the 'SERVER NAME' extension.  However, 'set-session-server-name!' is
    ;; not available in older GnuTLS releases.  See
    ;; <http://bugs.gnu.org/18526> for details.
    (if (module-defined? (resolve-interface '(gnutls))
                         'set-session-server-name!)
        (set-session-server-name! session server-name-type/dns server)
        (format (current-error-port)
                "warning: TLS 'SERVER NAME' extension not supported~%"))

    (set-session-transport-fd! session (fileno port))
    (set-session-default-priority! session)

    ;; The "%COMPAT" bit allows us to work around firewall issues (info
    ;; "(gnutls) Priority Strings"); see <http://bugs.gnu.org/23311>.
    ;; Explicitly disable SSLv3, which is insecure:
    ;; <https://tools.ietf.org/html/rfc7568>.
    ;;
    ;; FIXME: Since we currently fail to handle TLS 1.3 (with GnuTLS 3.6.5),
    ;; remove it; see <https://bugs.gnu.org/34102>.
    (set-session-priorities! session
                             (string-append
                              "NORMAL:%COMPAT:-VERS-SSL3.0"

                              ;; The "VERS-TLS1.3" priority string is not
                              ;; supported by GnuTLS 3.5.
                              (if (string-prefix? "3.5." (gnutls-version))
                                  ""
                                  ":-VERS-TLS1.3")))

    (set-session-credentials! session
                              (if (and verify-certificate? ca-certs)
                                  (make-credendials-with-ca-trust-files
                                   ca-certs)
                                  (make-certificate-credentials)))

    ;; Uncomment the following lines in case of debugging emergency.
    ;;(set-log-level! 10)
    ;;(set-log-procedure! log)

    (catch 'gnutls-error
      (lambda ()
        (handshake session))
      (lambda (key err proc . rest)
        (cond ((eq? err error/warning-alert-received)
               ;; Like Wget, do no stop upon non-fatal alerts such as
               ;; 'alert-description/unrecognized-name'.
               (format (current-error-port)
                       "warning: TLS warning alert received: ~a~%"
                       (alert-description->string (alert-get session)))
               (handshake session))
              (else
               ;; XXX: We'd use 'gnutls_error_is_fatal' but (gnutls) doesn't
               ;; provide a binding for this.
               (apply throw key err proc rest)))))

    ;; Verify the server's certificate if needed.
    (when verify-certificate?
      (catch 'tls-certificate-error
        (lambda ()
          (assert-valid-server-certificate session server))
        (lambda args
          (close-port port)
          (apply throw args))))

    (let ((record (session-record-port session)))
      ;; Since we use `fileno' above, the file descriptor behind PORT would be
      ;; closed when PORT is GC'd.  If we used `port->fdes', it would instead
      ;; never be closed.  So we use `fileno', but keep a weak reference to
      ;; PORT, so the file descriptor gets closed when RECORD is GC'd.
      (register-tls-record-port record port)

      ;; Write HTTP requests line by line rather than byte by byte:
      ;; <https://bugs.gnu.org/22966>.  This is possible with Guile >= 2.2.
      (setvbuf record 'line)

      record)))

(define (ensure-uri uri-or-string)                ;XXX: copied from (web http)
  (cond
   ((string? uri-or-string) (string->uri uri-or-string))
   ((uri? uri-or-string) uri-or-string)
   (else (error "Invalid URI" uri-or-string))))

(define* (open-socket-for-uri uri-or-string #:key timeout)
  "Return an open input/output port for a connection to URI.  When TIMEOUT is
not #f, it must be a (possibly inexact) number denoting the maximum duration
in seconds to wait for the connection to complete; passed TIMEOUT, an
ETIMEDOUT error is raised."
  ;; Includes a fix for <http://bugs.gnu.org/15368> which affects Guile's
  ;; 'open-socket-for-uri' up to 2.0.11 included, uses 'connect*' instead
  ;; of 'connect', and uses AI_ADDRCONFIG.

  (define http-proxy (current-http-proxy))
  (define uri (ensure-uri (or http-proxy uri-or-string)))
  (define addresses
    (let ((port (uri-port uri)))
      (delete-duplicates
       (getaddrinfo (uri-host uri)
                    (cond (port => number->string)
                          (else (symbol->string (uri-scheme uri))))
                    (if (number? port)
                        (logior AI_ADDRCONFIG AI_NUMERICSERV)
                        AI_ADDRCONFIG))
       (lambda (ai1 ai2)
         (equal? (addrinfo:addr ai1) (addrinfo:addr ai2))))))

  (let loop ((addresses addresses))
    (let* ((ai (car addresses))
           (s  (with-fluids ((%default-port-encoding #f))
                 ;; Restrict ourselves to TCP.
                 (socket (addrinfo:fam ai) SOCK_STREAM IPPROTO_IP))))
      (catch 'system-error
        (lambda ()
          (connect* s (addrinfo:addr ai) timeout)

          ;; Buffer input and output on this port.
          (setvbuf s 'block)
          ;; If we're using a proxy, make a note of that.
          (when http-proxy (set-http-proxy-port?! s #t))
          s)
        (lambda args
          ;; Connection failed, so try one of the other addresses.
          (close s)
          (if (null? (cdr addresses))
              (apply throw args)
              (loop (cdr addresses))))))))

(define (setup-http-tunnel port uri)
  "Establish over PORT an HTTP tunnel to the destination server of URI."
  (define target
    (string-append (uri-host uri) ":"
                   (number->string
                    (or (uri-port uri)
                        (match (uri-scheme uri)
                          ('http 80)
                          ('https 443))))))
  (format port "CONNECT ~a HTTP/1.1\r\n" target)
  (format port "Host: ~a\r\n\r\n" target)
  (force-output port)
  (read-response port))

(define* (open-connection-for-uri uri
                                  #:key
                                  timeout
                                  (verify-certificate? #t))
  "Like 'open-socket-for-uri', but also handle HTTPS connections.  The
resulting port must be closed with 'close-connection'.  When
VERIFY-CERTIFICATE? is true, verify HTTPS server certificates."
  ;; Note: Guile 2.2.0's (web client) has a same-named export that's actually
  ;; undefined.  See Guile commit 011669af3b428e5626f7bbf66b11d57d9768c047.

  (define https?
    (eq? 'https (uri-scheme uri)))

  (define https-proxy (let ((proxy (getenv "https_proxy")))
                        (and (not (equal? proxy ""))
                             proxy)))

  (let-syntax ((with-https-proxy
                (syntax-rules ()
                  ((_ exp)
                   ;; For HTTPS URIs, honor 'https_proxy', not 'http_proxy'.
                   (let ((thunk (lambda () exp)))
                     (if (and https?
                              (module-variable
                               (resolve-interface '(web client))
                               'current-http-proxy))
                         (parameterize ((current-http-proxy https-proxy))
                           (thunk))
                         (thunk)))))))
    (with-https-proxy
     (let ((s (open-socket-for-uri uri #:timeout timeout)))
       ;; Buffer input and output on this port.
       (setvbuf s 'block %http-receive-buffer-size)

       (when (and https? https-proxy)
         (setup-http-tunnel s uri))

       (if https?
           (tls-wrap s (uri-host uri)
                     #:verify-certificate? verify-certificate?)
           s)))))

(define (close-connection port)
  "Like 'close-port', but (1) idempotent, and (2) also closes the underlying
port if PORT is a TLS session record port."
  ;; FIXME: This is a partial workaround for <http://bugs.gnu.org/20145>,
  ;; because 'http-fetch' & co. may return a chunked input port whose 'close'
  ;; method calls 'close-port', not 'close-connection'.
  (unless (port-closed? port)
    (close-port port))
  (and=> (hashq-ref %tls-ports port)
         close-connection))

;; XXX: This is an awful hack to make sure the (set-port-encoding! p
;; "ISO-8859-1") call in `read-response' passes, even during bootstrap
;; where iconv is not available.
(module-define! (resolve-module '(web response))
                'set-port-encoding!
                (lambda (p e) #f))

;; XXX: Work around <http://bugs.gnu.org/23421>, fixed in Guile commit
;; 16050431f29d56f80c4a8253506fc851b8441840.  Guile's date validation
;; procedure rejects dates in which the hour is not padded with a zero but
;; with whitespace.
(begin
  (define-syntax string-match?
    (lambda (x)
      (syntax-case x ()
        ((_ str pat) (string? (syntax->datum #'pat))
         (let ((p (syntax->datum #'pat)))
           #`(let ((s str))
               (and
                (= (string-length s) #,(string-length p))
                #,@(let lp ((i 0) (tests '()))
                     (if (< i (string-length p))
                         (let ((c (string-ref p i)))
                           (lp (1+ i)
                               (case c
                                 ((#\.)  ; Whatever.
                                  tests)
                                 ((#\d)  ; Digit.
                                  (cons #`(char-numeric? (string-ref s #,i))
                                        tests))
                                 ((#\a)  ; Alphabetic.
                                  (cons #`(char-alphabetic? (string-ref s #,i))
                                        tests))
                                 (else   ; Literal.
                                  (cons #`(eqv? (string-ref s #,i) #,c)
                                        tests)))))
                         tests)))))))))

  (define (parse-rfc-822-date str space zone-offset)
    (let ((parse-non-negative-integer (@@ (web http) parse-non-negative-integer))
          (parse-month (@@ (web http) parse-month))
          (bad-header (@@ (web http) bad-header)))
      ;; We could verify the day of the week but we don't.
      (cond ((string-match? (substring str 0 space) "aaa, dd aaa dddd dd:dd:dd")
             (let ((date (parse-non-negative-integer str 5 7))
                   (month (parse-month str 8 11))
                   (year (parse-non-negative-integer str 12 16))
                   (hour (parse-non-negative-integer str 17 19))
                   (minute (parse-non-negative-integer str 20 22))
                   (second (parse-non-negative-integer str 23 25)))
               (make-date 0 second minute hour date month year zone-offset)))
            ((string-match? (substring str 0 space) "aaa, d aaa dddd dd:dd:dd")
             (let ((date (parse-non-negative-integer str 5 6))
                   (month (parse-month str 7 10))
                   (year (parse-non-negative-integer str 11 15))
                   (hour (parse-non-negative-integer str 16 18))
                   (minute (parse-non-negative-integer str 19 21))
                   (second (parse-non-negative-integer str 22 24)))
               (make-date 0 second minute hour date month year zone-offset)))

            ;; The next two clauses match dates that have a space instead of
            ;; a leading zero for hours, like " 8:49:37".
            ((string-match? (substring str 0 space) "aaa, dd aaa dddd  d:dd:dd")
             (let ((date (parse-non-negative-integer str 5 7))
                   (month (parse-month str 8 11))
                   (year (parse-non-negative-integer str 12 16))
                   (hour (parse-non-negative-integer str 18 19))
                   (minute (parse-non-negative-integer str 20 22))
                   (second (parse-non-negative-integer str 23 25)))
               (make-date 0 second minute hour date month year zone-offset)))
            ((string-match? (substring str 0 space) "aaa, d aaa dddd  d:dd:dd")
             (let ((date (parse-non-negative-integer str 5 6))
                   (month (parse-month str 7 10))
                   (year (parse-non-negative-integer str 11 15))
                   (hour (parse-non-negative-integer str 17 18))
                   (minute (parse-non-negative-integer str 19 21))
                   (second (parse-non-negative-integer str 22 24)))
               (make-date 0 second minute hour date month year zone-offset)))

            (else
             (bad-header 'date str)        ; prevent tail call
             #f))))
  (module-set! (resolve-module '(web http))
               'parse-rfc-822-date parse-rfc-822-date))

;; XXX: Work around broken proxy handling on Guile 2.2 <= 2.2.2, fixed in
;; Guile commits 7d0d9e2c25c1e872cfc7d14ab5139915f1813d56 and
;; 6ad28ae3bc6a6d9e95ab7d70510d12c97673a143.  See bug report at
;; <https://lists.gnu.org/archive/html/guix-devel/2017-11/msg00070.html>.
(cond-expand
  (guile-2.2
   (when (<= (string->number (micro-version)) 2)
     (let ()
       (define put-symbol (@@ (web http) put-symbol))
       (define put-non-negative-integer
         (@@ (web http) put-non-negative-integer))
       (define write-http-version
         (@@ (web http) write-http-version))

       (define (write-request-line method uri version port)
         "Write the first line of an HTTP request to PORT."
         (put-symbol port method)
         (put-char port #\space)
         (when (http-proxy-port? port)
           (let ((scheme (uri-scheme uri))
                 (host (uri-host uri))
                 (host-port (uri-port uri)))
             (when (and scheme host)
               (put-symbol port scheme)
               (put-string port "://")
               (cond
                ((string-index host #\:)          ;<---- The fix is here!
                 (put-char port #\[)              ;<---- And here!
                 (put-string port host)
                 (put-char port #\]))
                (else
                 (put-string port host)))
               (unless ((@@ (web uri) default-port?) scheme host-port)
                 (put-char port #\:)
                 (put-non-negative-integer port host-port)))))
         (let ((path (uri-path uri))
               (query (uri-query uri)))
           (if (string-null? path)
               (put-string port "/")
               (put-string port path))
           (when query
             (put-string port "?")
             (put-string port query)))
         (put-char port #\space)
         (write-http-version version port)
         (put-string port "\r\n"))

       (module-set! (resolve-module '(web http)) 'write-request-line
                    write-request-line))))
  (else #t))

(define (resolve-uri-reference ref base)
  "Resolve the URI reference REF, interpreted relative to the BASE URI, into a
target URI, according to the algorithm specified in RFC 3986 section 5.2.2.
Return the resulting target URI."

  (define (merge-paths base-path rel-path)
    (let* ((base-components (string-split base-path #\/))
           (base-directory-components (match base-components
                                        ((components ... last) components)
                                        (() '())))
           (base-directory (string-join base-directory-components "/")))
      (string-append base-directory "/" rel-path)))

  (define (remove-dot-segments path)
    (let loop ((in
                ;; Drop leading "." and ".." components from a relative path.
                ;; (absolute paths will start with a "" component)
                (drop-while (match-lambda
                              ((or "." "..") #t)
                              (_ #f))
                            (string-split path #\/)))
               (out '()))
      (match in
        (("." . rest)
         (loop rest out))
        ((".." . rest)
         (match out
           ((or () (""))
            (error "remove-dot-segments: too many '..' components" path))
           (_
            (loop rest (cdr out)))))
        ((component . rest)
         (loop rest (cons component out)))
        (()
         (string-join (reverse out) "/")))))

  (cond ((or (uri-scheme ref)
             (uri-host   ref))
         (build-uri (or (uri-scheme ref)
                        (uri-scheme base))
                    #:userinfo (uri-userinfo ref)
                    #:host     (uri-host     ref)
                    #:port     (uri-port     ref)
                    #:path     (remove-dot-segments (uri-path ref))
                    #:query    (uri-query    ref)
                    #:fragment (uri-fragment ref)))
        ((string-null? (uri-path ref))
         (build-uri (uri-scheme base)
                    #:userinfo (uri-userinfo base)
                    #:host     (uri-host     base)
                    #:port     (uri-port     base)
                    #:path     (remove-dot-segments (uri-path base))
                    #:query    (or (uri-query ref)
                                   (uri-query base))
                    #:fragment (uri-fragment ref)))
        (else
         (build-uri (uri-scheme base)
                    #:userinfo (uri-userinfo base)
                    #:host     (uri-host     base)
                    #:port     (uri-port     base)
                    #:path     (remove-dot-segments
                                (if (string-prefix? "/" (uri-path ref))
                                    (uri-path ref)
                                    (merge-paths (uri-path base)
                                                 (uri-path ref))))
                    #:query    (uri-query    ref)
                    #:fragment (uri-fragment ref)))))

(define* (http-fetch uri #:key timeout (verify-certificate? #t))
  "Return an input port containing the data at URI, and the expected number of
bytes available or #f.  When TIMEOUT is true, bail out if the connection could
not be established in less than TIMEOUT seconds.  When VERIFY-CERTIFICATE? is
true, verify HTTPS certificates; otherwise simply ignore them."

  (define headers
    `(;; Some web sites, such as http://dist.schmorp.de, would block you if
      ;; there's no 'User-Agent' header, presumably on the assumption that
      ;; you're a spammer.  So work around that.
      (User-Agent . "GNU Guile")

      ;; Some servers, such as https://alioth.debian.org, return "406 Not
      ;; Acceptable" when not explicitly told that everything is accepted.
      (Accept . "*/*")

      ;; Basic authentication, if needed.
      ,@(match (uri-userinfo uri)
          ((? string? str)
           `((Authorization . ,(string-append "Basic "
                                              (base64-encode
                                               (string->utf8 str))))))
          (_ '()))))

  (let*-values (((connection)
                 (open-connection-for-uri uri
                                          #:timeout timeout
                                          #:verify-certificate?
                                          verify-certificate?))
                ((resp port)
                 (http-get uri #:port connection #:decode-body? #f
                           #:streaming? #t
                           #:headers headers))
                ((code)
                 (response-code resp)))
    (case code
      ((200)                                      ; OK
       (values port (response-content-length resp)))
      ((301                                       ; moved permanently
        302                                       ; found (redirection)
        303                                       ; see other
        307                                       ; temporary redirection
        308)                                      ; permanent redirection
       (let ((uri (resolve-uri-reference (response-location resp) uri)))
         (format #t "following redirection to `~a'...~%"
                 (uri->string uri))
         (close connection)
         (http-fetch uri
                     #:timeout timeout
                     #:verify-certificate? verify-certificate?)))
      (else
       (error "download failed" (uri->string uri)
              code (response-reason-phrase resp))))))


(define-syntax-rule (false-if-exception* body ...)
  "Like `false-if-exception', but print the exception on the error port."
  (catch #t
    (lambda ()
      body ...)
    (lambda (key . args)
      #f)
    (lambda (key . args)
      (print-exception (current-error-port) #f key args))))

(define (uri-vicinity dir file)
  "Concatenate DIR, slash, and FILE, keeping only one slash in between.
This is required by some HTTP servers."
  (string-append (string-trim-right dir #\/) "/"
                 (string-trim file #\/)))

(define (maybe-expand-mirrors uri mirrors)
  "If URI uses the 'mirror' scheme, expand it according to the MIRRORS alist.
Return a list of URIs."
  (case (uri-scheme uri)
    ((mirror)
     (let ((kind (string->symbol (uri-host uri)))
           (path (uri-path uri)))
       (match (assoc-ref mirrors kind)
         ((mirrors ..1)
          (map (compose string->uri (cut uri-vicinity <> path))
               mirrors))
         (_
          (error "unsupported URL mirror kind" kind uri)))))
    (else
     (list uri))))

(define* (url-fetch url file
                    #:key
                    (timeout 10) (verify-certificate? #t)
                    (mirrors '()) (content-addressed-mirrors '())
                    (hashes '())
                    print-build-trace?)
  "Fetch FILE from URL; URL may be either a single string, or a list of
string denoting alternate URLs for FILE.  Return #f on failure, and FILE
on success.

When MIRRORS is defined, it must be an alist of mirrors; it is used to resolve
'mirror://' URIs.

HASHES must be a list of algorithm/hash pairs, where each algorithm is a
symbol such as 'sha256 and each hash is a bytevector.
CONTENT-ADDRESSED-MIRRORS must be a list of procedures that, given a hash
algorithm and a hash, return a URL where the specified data can be retrieved
or #f.

When VERIFY-CERTIFICATE? is true, validate HTTPS server certificates;
otherwise simply ignore them."
  (define uri
    (append-map (cut maybe-expand-mirrors <> mirrors)
                (match url
                  ((_ ...) (map string->uri url))
                  (_       (list (string->uri url))))))

  (define (fetch uri file)
    (format #t "~%Starting download of ~a~%From ~a...~%"
            file (uri->string uri))
    (case (uri-scheme uri)
      ((http https)
       (false-if-exception*
        (let-values (((port size)
                      (http-fetch uri
                                  #:verify-certificate? verify-certificate?
                                  #:timeout timeout)))
          (call-with-output-file file
            (lambda (output)
              (dump-port* port output
                          #:buffer-size %http-receive-buffer-size
                          #:reporter (if print-build-trace?
                                         (progress-reporter/trace
                                          file (uri->string uri) size)
                                         (progress-reporter/file
                                          (uri-abbreviation uri) size)))
              (newline)))
          file)))
      ((ftp)
       (false-if-exception* (ftp-fetch uri file
                                       #:timeout timeout
                                       #:print-build-trace?
                                       print-build-trace?)))
      (else
       (format #t "skipping URI with unsupported scheme: ~s~%"
               uri)
       #f)))

  (define content-addressed-uris
    (append-map (lambda (make-url)
                  (filter-map (match-lambda
                                ((hash-algo . hash)
                                 (let ((file (strip-store-file-name file)))
                                   (string->uri (make-url file hash-algo hash)))))
                              hashes))
                content-addressed-mirrors))

  ;; Make this unbuffered so 'progress-report/file' works as expected.  'line
  ;; means '\n', not '\r', so it's not appropriate here.
  (setvbuf (current-output-port) 'none)

  (setvbuf (current-error-port) 'line)

  (let try ((uri (append uri content-addressed-uris)))
    (match uri
      ((uri tail ...)
       (or (fetch uri file)
           (try tail)))
      (()
       (format (current-error-port) "failed to download ~s from ~s~%"
               file url)
       #f))))

;;; download.scm ends here
