;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2019-2021, 2023-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix scripts time-machine)
  #:use-module (guix channels)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix inferior)
  #:use-module (guix store)
  #:use-module (guix status)
  #:use-module ((guix git)
                #:select (update-cached-checkout with-git-error-handling))
  #:use-module ((guix utils)
                #:select (%current-system))
  #:use-module ((guix scripts pull)
                #:select (channel-list))
  #:use-module ((guix scripts build)
                #:select (%standard-build-options
                          show-build-options-help
                          set-build-options-from-command-line))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:export (guix-time-machine))


;;;
;;; Command-line options.
;;;

(define (show-help)
  (display (G_ "Usage: guix time-machine [OPTION] -- COMMAND ARGS...
Execute COMMAND ARGS... in an older version of Guix.\n"))
  (display (G_ "
  -C, --channels=FILE    deploy the channels defined in FILE"))
  (display (G_ "
  -q, --no-channel-files
                         inhibit loading of user and system 'channels.scm'"))
  (display (G_ "
      --url=URL          use the Git repository at URL"))
  (display (G_ "
      --commit=COMMIT    use the specified COMMIT"))
  (display (G_ "
      --branch=BRANCH    use the tip of the specified BRANCH"))
  (display (G_ "
      --disable-authentication
                         disable channel authentication"))
  (newline)
  (show-build-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (cons* (option '(#\C "channels") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'channel-file arg result)))
         (option '(#\q "no-channel-files") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'ignore-channel-files? #t result)))
         (option '("url") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'repository-url arg
                               (alist-delete 'repository-url result))))
         (option '("commit") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'ref `(tag-or-commit . ,arg) result)))
         (option '("branch") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'ref `(branch . ,arg) result)))
         (option '("disable-authentication") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'authenticate-channels? #f result)))
         (option '(#\h "help") #f #f
                 (lambda args
                   (leave-on-EPIPE (show-help))
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix time-machine")))

         %standard-build-options))

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (offload? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (authenticate-channels? . #t)
    (graft? . #t)
    (debug . 0)
    (verbosity . 1)))

(define (parse-args args)
  "Parse the list of command line arguments ARGS."
  ;; The '--' token is used to separate the command to run from the rest of
  ;; the operands.
  (let-values (((args command) (break (cut string=? "--" <>) args)))
    (let ((opts (parse-command-line args %options
                                    (list %default-options))))
      (when (assoc-ref opts 'argument)
        (leave (G_ "~A: extraneous argument~%")
               (assoc-ref opts 'argument)))

      (match command
        (() opts)
        (("--") opts)
        (("--" command ...) (alist-cons 'exec command opts))))))


;;;
;;; Avoiding traveling too far back.
;;;

;;; The required inferiors mechanism relied on by 'guix time-machine' was
;;; firmed up in v1.0.0; it is the oldest, safest commit that can be travelled
;;; to.
(define %oldest-possible-commit
  "4a0b87f0ec5b6c2dcf82b372dd20ca7ea6acdd9c") ;v0.16.0

(define %reference-channels
  (list (channel (inherit %default-guix-channel)
                 (commit %oldest-possible-commit))))

(define (validate-guix-channel channel start commit relation)
  "Raise an error if CHANNEL is the 'guix' channel and the RELATION of COMMIT
to %OLDEST-POSSIBLE-COMMIT is not that of an ancestor."
  (unless (or (not (guix-channel? channel))
              (memq relation '(ancestor self)))
    (raise (formatted-message
            (G_ "cannot travel past commit `~a' from May 1st, 2019")
            (string-take %oldest-possible-commit 12)))))



;;;
;;; Entry point.
;;;

(define-command (guix-time-machine . args)
  (synopsis "run commands from a different revision")

  (with-error-handling
    (with-git-error-handling
     (let* ((opts         (parse-args args))
            (channels     (channel-list opts))
            (command-line (assoc-ref opts 'exec))
            (ref          (assoc-ref opts 'ref))
            (substitutes?  (assoc-ref opts 'substitutes?))
            (authenticate? (assoc-ref opts 'authenticate-channels?)))
       (if command-line
           (let* ((directory
                   (with-store store
                     (with-status-verbosity (assoc-ref opts 'verbosity)
                       (with-build-handler (build-notifier #:use-substitutes?
                                                           substitutes?
                                                           #:verbosity
                                                           (assoc-ref opts 'verbosity)
                                                           #:dry-run? #f)
                         (set-build-options-from-command-line store opts)
                         (cached-channel-instance store channels
                                                  #:authenticate? authenticate?
                                                  #:reference-channels
                                                  %reference-channels
                                                  #:validate-channels
                                                  validate-guix-channel)))))
                  (executable (string-append directory "/bin/guix")))
             (apply execl (cons* executable executable command-line)))
           (warning (G_ "no command specified; nothing to do~%")))))))
