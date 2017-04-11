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

(define-module (guix scripts potluck)
  #:use-module (guix config)
  #:use-module (guix base32)
  #:use-module ((guix build-system) #:select (build-system-description))
  #:use-module ((guix licenses) #:select (license-uri))
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix potluck build-systems)
  #:use-module (guix potluck host)
  #:use-module (guix potluck licenses)
  #:use-module (guix potluck packages)
  #:use-module (guix scripts)
  #:use-module (guix scripts hash)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (guix-potluck))


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
;;; guix potluck init
;;;

(define* (init-potluck host remote-git-url #:key
                       (build-system 'gnu) (autoreconf? #f)
                       (license 'gplv3+))
  (let* ((cwd (getcwd))
         (dot-git (in-vicinity cwd ".git"))
         (potluck-dir (in-vicinity cwd "potluck"))
         (package-name (basename cwd)))
    (unless (and (file-exists? dot-git)
                 (file-is-directory? dot-git))
      (leave (_ "init: must be run from the root of a git checkout~%")))
    (when (file-exists? potluck-dir)
      (leave (_ "init: ~a already exists~%") potluck-dir))
    (let* ((user-name (git-config "user.name"))
           (pkg-name (basename cwd))
           (pkg-commit (git-rev-parse "HEAD"))
           (pkg-version
            (catch #t
              (lambda () (git-describe pkg-commit))
              (lambda _
                (format (current-error-port)
                        "guix potluck init: git describe failed\n")
                (format (current-error-port)
                        "Add a tag so that git can compute a version.\n")
                (exit 1))))
           ;; FIXME: Race condition if HEAD changes between git-rev-parse and
           ;; here.
           (pkg-sha256 (guix-hash-git-checkout cwd)))
      (format #t (_ "Creating potluck/~%"))
      (mkdir potluck-dir)
      (format #t (_ "Creating potluck/README.md~%"))
      (call-with-output-file (in-vicinity potluck-dir "README.md")
        (lambda (port)
          (format port
                  "\
This directory defines potluck packages.  Each file in this directory should
define one package.  See https://potluck.guixsd.org/ for more information.
")))
      (format #t (_ "Creating potluck/~a.scm~%") package-name)
      (call-with-output-file (in-vicinity potluck-dir
                                          (string-append package-name ".scm"))
        (lambda (port)
          
          (define-syntax-rule (dsp exp) (display exp port))
          (dsp ";;; guix potluck package\n")
          (dsp ";;; Copyright (C) 2017 ")
          (dsp user-name)
          (dsp "\n")
          (dsp "
;;; This file is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.  No warranty.  See
;;; https://www.gnu.org/licenses/gpl.html for a copy of the GPLv3.

")
          (pretty-print-potluck-package
           port
           (potluck-package
            (name pkg-name)
            (version pkg-version)
            (source
             (potluck-source
              (git-uri remote-git-url)
              (git-commit pkg-commit)
              (sha256 (bytevector->nix-base32-string pkg-sha256))))
            (build-system build-system)
            (inputs '())
            (native-inputs
             (if autoreconf?
                 '("autoconf" "automake" "libtool" "pkg-config")
                 '()))
            (arguments
             (if autoreconf?
                 '(#:phases (modify-phases %standard-phases
                              (add-before 'configure 'autoconf
                                (lambda _
                                  (zero?
                                   (system* "autoreconf" "-vfi"))))))
                 '()))
            (home-page remote-git-url)
            (synopsis "Declarative synopsis here")
            (description
             (string-append (string-titlecase pkg-name)
                            " is a ..."))
            (license license)))))
      (format #t (_ "
Done.  Now open potluck/~a.scm in your editor, fill out its \"synopsis\" and
\"description\" fields, add dependencies to the 'inputs' field, and try to
build with

  guix build --file=potluck/~a.scm

When you get that working, commit your results to git via:

  git add potluck && git commit -m 'Add initial Guix potluck files.'
") pkg-name pkg-name))))

;;;
;;; guix potluck update
;;;

(define (request-potluck-update host git-url branch)
  (call-with-values (lambda ()
                      (http-post (build-uri 'https
                                            #:host host
                                            #:path "/api/enqueue-update")
                                 #:body (scm->json-string
                                         `((git-url . ,git-url)
                                           (branch . ,branch)))))
    (lambda (response body)
      (unless (eqv? (response-code response) 200)
        (error "request failed"
               (response-code response)
               (response-reason-phrase response)
               body)))))


;;;
;;; Options.
;;;

(define (show-help)
  (display (_ "Usage: guix potluck [OPTION ...] ACTION [ARG ...]
Create \"potluck\" packages, register them with a central service, and arrange
to serve those packages as a Guix channel. Some ACTIONS require additional
ARGS.\n"))
  (newline)
  (display (_ "The valid values for ACTION are:\n"))
  (newline)
  (display (_ "\
   init             create potluck recipe for current working directory\n"))
  (display (_ "\
   update           ask potluck host to add or update a potluck package\n"))
  (display (_ "\
   host-channel     run web service providing potluck packages as Guix channel\n"))

  (newline)
  (display (_ "The available OPTION flags are:\n"))
  (display (_ "
      --host=URL         for 'update' and 'host-channel', the name of the
                         channel host
                         (default: guix-potluck.org)"))
  (display (_ "
      --port=PORT        for 'host-channel', the local TCP port on which to
                         listen for HTTP connections
                         (default: 8080)"))
  (display (_ "
      --build-system=SYS for 'init', specify the build system.  Use
                         --build-system=help for all available options."))
  (display (_ "
      --autotools        for 'init', like --build-system=gnu but additionally
                         indicating that the package needs autoreconf before
                         running ./configure"))
  (display (_ "
      --license=LICENSE  for 'init', specify the license of the package.  Use
                         --license=help for all available options."))
  (display (_ "
      --verbosity=LEVEL  use the given verbosity LEVEL"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix potluck")))
        (option '("build-system") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'build-system arg result)))
        (option '("autotools") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'autoreconf? #t
                              (alist-cons 'build-system "gnu" result))))
        (option '("license") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'license arg result)))
        (option '("host") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'host arg result)))
        (option '("port") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'port arg result)))
        (option '("verbosity") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'verbosity (string->number arg) result)))))

(define %default-options
  ;; Alist of default option values.
  `((host . "guix-potluck.org")
    (port . "8080")
    (verbosity . 0)))

(define (parse-host host-str)
  ;; Will throw if the host is invalid somehow.
  (build-uri 'https #:host host-str)
  host-str)

(define (parse-url url-str)
  (unless (string->uri url-str)
    (leave (_ "invalid url: ~a~%") url-str))
  url-str)

(define (parse-port port-str)
  (let ((port (string->number port-str)))
    (cond
     ((and port (exact-integer? port) (<= 0 port #xffff))
      port)
     (else
      (leave (_ "invalid port: ~a~%") port-str)))))

(define (parse-build-system sys-str)
  (unless sys-str
    (leave (_ "\
init: missing --build-system; try --build-system=help for options~%")))
  (let ((sys (string->symbol (string-downcase sys-str))))
    (when (eq? sys 'help)
      (format #t "guix potluck: Available build systems:~%")
      (for-each
       (lambda (name)
         (let ((sys (build-system-by-name name)))
           (format #t "  ~a ~25t~a~%" name (build-system-description sys))))
       (all-potluck-build-system-names))
      (exit 0))
    (unless (build-system-by-name sys)
      (leave (_ "invalid build system: ~a; try --build-system=help~%") sys))
    sys))

(define (parse-license license-str)
  (unless license-str
    (leave (_ "init: missing --license; try --license=help for options~%")))
  (let ((license (string->symbol (string-downcase license-str))))
    (when (eq? license 'help)
      (format #t "guix potluck: Available licenses:~%")
      (for-each
       (lambda (name)
         (let ((license (license-by-name name)))
           (format #t "  ~a ~25t~a~%" name (license-uri license))))
       (all-potluck-license-names))
      (format #t "
If your package's license is not in this list, add it to Guix first.~%")
      (exit 0))
    (unless (license-by-name license)
      (leave (_ "invalid license: ~a; try --license=help~%") license))
    license))


;;;
;;; Entry point.
;;;

(define (guix-potluck . args)
  (define (parse-sub-command arg result)
    (if (assoc-ref result 'action)
        (alist-cons 'argument arg result)
        (alist-cons 'action (string->symbol arg) result)))

  (define (match-pair car)
    ;; Return a procedure that matches a pair with CAR.
    (match-lambda
      ((head . tail)
       (and (eq? car head) tail))
      (_ #f)))

  (with-error-handling
    (let* ((opts     (parse-command-line args %options
                                         (list %default-options)
                                         #:argument-handler
                                         parse-sub-command))
           (action   (assoc-ref opts 'action))
           (args     (reverse (filter-map (match-pair 'argument) opts))))
      (define (see-help)
        (format (current-error-port)
                (_ "Try 'guix potluck --help' for more information.~%")))
      (define (wrong-number-of-args usage)
        (format (current-error-port)
                (_ "guix potluck ~a: wrong number of arguments~%")
                action)
        (display usage (current-error-port))
        (newline (current-error-port))
        (see-help)
        (exit 1))
      (match action
        (#f
         (format (current-error-port)
                 (_ "guix potluck: missing command name~%"))
         (see-help)
         (exit 1))
        ('init
         (match args
           ((remote-git-url)
            (init-potluck (parse-host (assoc-ref opts 'host))
                          (parse-url remote-git-url)
                          #:build-system (parse-build-system
                                          (assoc-ref opts 'build-system))
                          #:autoreconf? (assoc-ref opts 'autoreconf?)
                          #:license (parse-license
                                     (assoc-ref opts 'license))))
           (args
            (wrong-number-of-args
             (_ "usage: guix potluck init [OPT...] REMOTE-GIT-URL")))))
        ('update
         (match args
           ((remote-git-url branch)
            (request-potluck-update (parse-host (assoc-ref opts 'host))
                                    (parse-url remote-git-url)
                                    branch))
           (args
            (wrong-number-of-args
             (_ "usage: guix potluck update REMOTE-GIT-URL BRANCH-NAME")))))
        ('host-channel
         (match args
           ((local-git-checkout)
            (host-potluck (parse-host (assoc-ref opts 'host))
                          (parse-port (assoc-ref opts 'port))
                          local-git-checkout))
           (args
            (wrong-number-of-args
             (_ "usage: guix potluck host-channel CHANNEL-DIRECTORY"))
            (exit 1))))
        (action
         (leave (_ "~a: unknown action~%") action))))))
