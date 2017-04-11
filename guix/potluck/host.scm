;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Wingo <wingo@pobox.com>
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

(define-module (guix potluck host)
  #:use-module (guix config)
  #:use-module (guix base32)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix potluck packages)
  #:use-module (guix potluck build-systems)
  #:use-module (guix potluck licenses)
  #:use-module (guix scripts)
  #:use-module (guix scripts hash)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 q)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:export (host-potluck))


;;;
;;; git utilities
;;;

(define-condition-type &git-condition &condition git-condition?
  (argv git-condition-argv)
  (output git-condition-output)
  (status git-condition-status))

(define-syntax false-if-git-error
  (syntax-rules ()
    ((_ body0 body ...)
     (guard (c ((git-condition? c) #f))
        body0 body ...))))

(define (shell:quote str)
  (with-output-to-string
    (lambda ()
      (display #\')
      (string-for-each (lambda (ch)
                         (if (eqv? ch #\')
                             (begin (display #\\) (display #\'))
                             (display ch)))
                       str)
      (display #\'))))

(define (run env input-file args)
  (define (prepend-env args)
    (if (null? env)
        args
        (cons "env" (append env args))))
  (define (redirect-input args)
    (if input-file
        (list "sh" "-c"
              (string-append (string-join (map shell:quote args) " ")
                             "<" input-file))
        args))
  (let* ((real-args (redirect-input (prepend-env args)))
         (pipe (apply open-pipe* OPEN_READ real-args))
         (output (get-string-all pipe))
         (ret (close-pipe pipe)))
    (case (status:exit-val ret)
      ((0) output)
      (else (raise (condition (&git-condition
                               (argv real-args)
                               (output output)
                               (status ret))))))))

(define* (git* args #:key (input #f) (env '()))
  (if input
      (call-with-temporary-output-file
       (lambda (file-name file-port)
         (display input file-port)
         (close-port file-port)
         (run env file-name (cons* "git" args))))
      (run env #f (cons* "git" args))))

(define (git . args)
  (git* args))

(define (git-rev-parse rev)
  (string-trim-both (git "rev-parse" rev)))

(define (git-config key)
  (string-trim-both (git "config" key)))

(define* (git-describe #:optional (ref "HEAD"))
  (string-trim-both (git "describe")))


;;;
;;; async queues
;;;

(define-record-type <async-queue>
  (make-aq mutex condvar q)
  async-queue?
  (mutex aq-mutex)
  (condvar aq-condvar)
  (q aq-q))

(set-record-type-printer!
 <async-queue>
 (lambda (aq port)
   (format port "<async-queue ~a ~a>" (object-address aq)
           (q-length (aq-q aq)))))

(define* (make-async-queue)
  (make-aq (make-mutex)
           (make-condition-variable)
           (make-q)))

(define* (async-queue-push! aq item)
  (with-mutex (aq-mutex aq)
    (enq! (aq-q aq) item)
    (signal-condition-variable (aq-condvar aq))))

(define* (async-queue-pop! aq)
  (with-mutex (aq-mutex aq)
    (let lp ()
      (cond
       ((q-empty? (aq-q aq))
        (wait-condition-variable (aq-condvar aq) (aq-mutex aq))
        (lp))
       (else
        (q-pop! (aq-q aq)))))))


;;;
;;; backend
;;;

(define (process-update git-checkout remote-git-url branch)
  (pk 'hey git-checkout remote-git-url branch))

(define (service-queue git-checkout queue)
  (let lp ()
    (match (async-queue-pop! queue)
      ((remote-git-url . branch)
       (format (current-error-port) "log: handling ~a / ~a\n"
               remote-git-url branch)
       (catch #t
         (lambda ()
           (process-update git-checkout remote-git-url branch)
           (format (current-error-port) "log: success ~a / ~a\n"
                   remote-git-url branch))
         (lambda (k . args)
           (format (current-error-port) "log: failure ~a / ~a\n"
                   remote-git-url branch)
           (print-exception (current-error-port) #f k args)))
       (lp)))))


;;;
;;; frontend
;;;

(define* (validate-public-uri str #:key (schemes '(http https)))
  (define (public-host? host)
    ;; There are other ways to spell "localhost" using raw IPv4 or IPv6
    ;; addresses; this is just a sanity check.
    (not (member host '("localhost" "127.0.0.1" "[::1]"))))
  (let ((uri (and (string? str) (string->uri str))))
    (unless (and uri
                 (memq (uri-scheme uri) schemes)
                 (not (uri-fragment uri))
                 (public-host? (uri-host uri)))
      (error "expected a public URI" str))))

(define (validate-non-empty-string str)
  (unless (and (string? str)
               (not (string-null? str)))
    (error "expected a non-empty string" str)))

(define (enqueue-update params queue)
  (let ((remote-git-url (hash-ref params "git-url"))
        (branch-name (hash-ref params "branch")))
    (validate-public-uri remote-git-url)
    (validate-non-empty-string branch-name)
    (async-queue-push! queue (cons remote-git-url branch-name))))

(define (handler request body queue)
  (match (cons (request-method request)
               (split-and-decode-uri-path (uri-path (request-uri request))))
    (('GET)
     (values (build-response #:code 200)
             "todo: show work queue"))
    (('POST "api" "enqueue-update")
     ;; An exception will cause error 500.
     (enqueue-update (json->scm body) queue)
     (values (build-response #:code 200)
             ""))
    (_
     (values (build-response #:code 404)
             ""))))

(define (host-potluck host local-port local-git-checkout-dir)
  (let ((worker-thread #f)
        (queue (make-async-queue)))
    (dynamic-wind (lambda ()
                    (set! worker-thread
                      (make-thread
                       (service-queue local-git-checkout-dir queue))))
                  (lambda () (run-server
                              (lambda (request body)
                                (handler request body queue))
                              ;; Always listen on localhost.
                              'http `(#:port ,local-port)))
                  (lambda ()
                    (cancel-thread worker-thread)))))
