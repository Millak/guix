;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016-2021, 2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (build-self)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix ui)
  #:use-module (guix config)
  #:use-module (guix modules)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (build))

;;; Commentary:
;;;
;;; When loaded, this module returns a monadic procedure of at least one
;;; argument: the source tree to build.  It returns a derivation that
;;; builds it.
;;;
;;; This file uses modules provided by the already-installed Guix.  Those
;;; modules may be arbitrarily old compared to the version we want to
;;; build.  Because of that, it must rely on the smallest set of features
;;; that are likely to be provided by the (guix) and (gnu) modules, and by
;;; Guile itself, forever and ever.
;;;
;;; Code:

(define (date-version-string)
  "Return the current date and hour in UTC timezone, for use as a poor
person's version identifier."
  ;; XXX: Replace with a Git commit id.
  (date->string (current-date 0) "~Y~m~d.~H"))

(define guile-gcrypt
  ;; The 'guile-gcrypt' package from the host Guix.
  (match (find-best-packages-by-name "guile-gcrypt" #f)
    ((package . _)
     package)))

(define* (build-program source version
                        #:optional (guile-version (effective-version))
                        #:key (pull-version 0) (channel-metadata #f)
                        built-in-builders)
  "Return a program that computes the derivation to build Guix from SOURCE.
If BUILT-IN-BUILDERS is provided, it should be a list of
strings and this will be used instead of the builtin builders provided by the
build daemon, from within the generated build program."
  (define select?
    ;; Select every module but (guix config) and non-Guix modules.
    ;; Also exclude (guix channels): it is autoloaded by (guix describe), but
    ;; only for peripheral functionality.
    (match-lambda
      (('guix 'config) #f)
      (('guix 'channels) #f)
      (('guix 'build 'download) #f)             ;autoloaded by (guix download)
      (('guix _ ...)   #t)
      (('gnu _ ...)    #t)
      (_               #f)))

  (define fake-gcrypt-hash
    ;; Fake (gcrypt hash) module; see below.
    (scheme-file "hash.scm"
                 #~(define-module (gcrypt hash)
                     #:export (sha1 sha256))))

  (define fake-git
    (scheme-file "git.scm" #~(define-module (git))))

  (with-imported-modules `(((guix config)
                            => ,(make-config.scm))

                           ;; To avoid relying on 'with-extensions', which was
                           ;; introduced in 0.15.0, provide a fake (gcrypt
                           ;; hash) just so that we can build modules, and
                           ;; adjust %LOAD-PATH later on.
                           ((gcrypt hash) => ,fake-gcrypt-hash)

                           ;; (guix git-download) depends on (git) but only
                           ;; for peripheral functionality.  Provide a dummy
                           ;; (git) to placate it.
                           ((git) => ,fake-git)

                           ,@(source-module-closure `((guix store)
                                                      (guix self)
                                                      (guix derivations)
                                                      (gnu packages bootstrap))
                                                    (list source)
                                                    #:select? select?))
    (gexp->script "compute-guix-derivation"
                  #~(begin
                      (use-modules (ice-9 match))

                      (eval-when (expand load eval)
                        ;; (gnu packages …) modules are going to be looked up
                        ;; under SOURCE.  (guix config) is looked up in FRONT.
                        (match (command-line)
                          ((_ source _ ...)
                           (match %load-path
                             ((front _ ...)
                              (unless (string=? front source) ;already done?
                                (set! %load-path
                                  (list source
                                        (string-append #$guile-gcrypt
                                                       "/share/guile/site/"
                                                       (effective-version))
                                        front)))))))

                        ;; Only load Guile-Gcrypt, our own modules, or those
                        ;; of Guile.
                        (set! %load-compiled-path
                          (cons (string-append #$guile-gcrypt "/lib/guile/"
                                               (effective-version)
                                               "/site-ccache")
                                %load-compiled-path))

                        ;; Disable position recording to save time and space
                        ;; when loading the package modules.
                        (read-disable 'positions))

                      (use-modules (guix store)
                                   (guix self)
                                   (guix derivations)
                                   (srfi srfi-1))

                      (match (command-line)
                        ((_ source system version protocol-version
                            build-output)
                         ;; The current input port normally wraps a file
                         ;; descriptor connected to the daemon, or it is
                         ;; connected to /dev/null.  In the former case, reuse
                         ;; the connection such that we inherit build options
                         ;; such as substitute URLs and so on; in the latter
                         ;; case, attempt to open a new connection.
                         (let* ((proto (string->number protocol-version))
                                (store (if (integer? proto)
                                           (port->connection
                                            (duplicate-port
                                             (current-input-port)
                                             "w+0")
                                            #:version proto
                                            #:built-in-builders
                                            '#$built-in-builders)
                                           (open-connection
                                            #:built-in-builders
                                            '#$built-in-builders)))
                                (sock  (socket AF_UNIX SOCK_STREAM 0)))
                           ;; Connect to BUILD-OUTPUT and send it the raw
                           ;; build output.
                           (connect sock AF_UNIX build-output)

                           (display
                            (and=>
                             ;; Silence autoload warnings and the likes.
                             (parameterize ((current-warning-port
                                             (%make-void-port "w"))
                                            (current-build-output-port sock))
                               (run-with-store store
                                 (guix-derivation source version
                                                  #$guile-version
                                                  #:channel-metadata
                                                  '#$channel-metadata
                                                  #:pull-version
                                                  #$pull-version)
                                 #:system system))
                             derivation-file-name))))))
                  #:module-path (list source))))

(define (proxy input output)
  "Dump the contents of INPUT to OUTPUT until EOF is reached on INPUT.
Display a spinner when nothing happens."
  (define spin
    (circular-list "-" "\\" "|" "/" "-" "\\" "|" "/"))

  (setvbuf input 'block 16384)
  (let loop ((spin spin))
    (match (select (list input) '() '() 1)
      ((() () ())
       (when (isatty? (current-error-port))
         (display (string-append "\b" (car spin))
                  (current-error-port))
         (force-output (current-error-port)))
       (loop (cdr spin)))
      (((_) () ())
       ;; Read from INPUT as much as can be read without blocking.
       (let ((bv (get-bytevector-some input)))
         (unless (eof-object? bv)
           (put-bytevector output bv)
           (loop spin)))))))

(define (call-with-clean-environment thunk)
  (let ((env (environ)))
    (dynamic-wind
      (lambda ()
        (environ '()))
      thunk
      (lambda ()
        (environ env)))))

(define-syntax-rule (with-clean-environment exp ...)
  "Evaluate EXP in a context where zero environment variables are defined."
  (call-with-clean-environment (lambda () exp ...)))

;; The procedure below is our return value.
(define* (build source
                #:key verbose?
                (version (date-version-string)) channel-metadata
                system
                (pull-version 0)

                ;; For the standalone Guix, default to Guile 3.0.  For old
                ;; versions of 'guix pull' (pre-0.15.0), we have to use the
                ;; same Guile as the current one.
                (guile-version (if (> pull-version 0)
                                   "3.0"
                                   (effective-version)))
                built-in-builders
                #:allow-other-keys
                #:rest rest)
  "Return a derivation that unpacks SOURCE into STORE and compiles Scheme
files."
  ;; Build the build program and then use it as a trampoline to build from
  ;; SOURCE.
  (mlet %store-monad ((build  (build-program source version guile-version
                                             #:channel-metadata channel-metadata
                                             #:pull-version pull-version
                                             #:built-in-builders
                                             built-in-builders))
                      (system (if system (return system) (current-system)))
                      (home -> (getenv "HOME"))

                      ;; Note: Use the deprecated names here because the
                      ;; caller might be Guix <= 0.16.0.
                      (port   ((store-lift nix-server-socket)))
                      (major  ((store-lift nix-server-major-version)))
                      (minor  ((store-lift nix-server-minor-version))))
    (mbegin %store-monad
      ;; Before 'with-build-handler' was implemented and used, we had to
      ;; explicitly call 'show-what-to-build*'.
      (munless (module-defined? (resolve-module '(guix store))
                                'with-build-handler)
        (show-what-to-build* (list build)))
      (built-derivations (list build))

      ;; Use the port beneath the current store as the stdin of BUILD.  This
      ;; way, we know 'open-pipe*' will not close it on 'exec'.  If PORT is
      ;; not a file port (e.g., it's an SSH channel), then the subprocess's
      ;; stdin will actually be /dev/null.
      (let* ((sock   (socket AF_UNIX SOCK_STREAM 0))
             (node   (let ((file (string-append (or (getenv "TMPDIR") "/tmp")
                                                "/guix-build-output-"
                                                (number->string (getpid)))))
                       (bind sock AF_UNIX file)
                       (listen sock 1)
                       file))
             (pipe   (with-input-from-port port
                       (lambda ()
                         ;; Make sure BUILD is not influenced by
                         ;; $GUILE_LOAD_PATH & co.
                         (with-clean-environment
                          (setenv "GUILE_WARN_DEPRECATED" "no") ;be quiet and drive
                          (setenv "COLUMNS" "120") ;show wider backtraces
                          (when home
                            ;; Inherit HOME so that 'xdg-directory' works.
                            (setenv "HOME" home))
                          (open-pipe* OPEN_READ
                                      (derivation->output-path build)
                                      source system version
                                      (if (file-port? port)
                                          (number->string
                                           (logior major minor))
                                          "none")
                                      node))))))
        (format (current-error-port) "Computing Guix derivation for '~a'...  "
                system)

        ;; Wait for a connection on SOCK and proxy build output so it can be
        ;; processed according to the settings currently in effect (build
        ;; traces, verbosity level, and so on).
        (match (accept sock)
          ((port . _)
           (close-port sock)
           (delete-file node)
           (proxy port (current-build-output-port))))

        ;; Now that the build output connection was closed, read the result, a
        ;; derivation file name, from PIPE.
        (let ((str    (get-string-all pipe))
              (status (close-pipe pipe)))
          (match str
            ((? eof-object?)
             (error "build program failed" (list build status)))
            ((? derivation-path? drv)
             (mbegin %store-monad
               (return (newline (current-error-port)))
               ((store-lift add-temp-root) drv)
               (return (read-derivation-from-file drv))))
            ("#f"
             ;; Unsupported PULL-VERSION.
             (return #f))
            ((? string? str)
             (raise (condition
                     (&message
                      (message (format #f "You found a bug: the program '~a'
failed to compute the derivation for Guix (version: ~s; system: ~s;
host version: ~s; pull-version: ~s).
Please report the COMPLETE output above by email to <~a>.~%"
                                       (derivation->output-path build)
                                       version system %guix-version pull-version
                                       %guix-bug-report-address))))))))))))

;; This file is loaded by 'guix pull'; return it the build procedure.
build

;; Local Variables:
;; eval: (put 'with-load-path 'scheme-indent-function 1)
;; End:

;;; build-self.scm ends here
