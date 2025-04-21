;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2025 jgart <jgart@dismail.de>
;;; Copyright © 2025 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (guix scripts import elpa)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import elpa)
  #:use-module (guix import utils)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-elpa))


;;;
;;; Command-line options.
;;;

(define %default-options
  '((repo . gnu)))

(define (show-help)
  (display (G_ "Usage: guix import elpa PACKAGE-NAME
Import the latest package named PACKAGE-NAME from an ELPA repository.\n"))
  (display (G_ "
  -a, --archive=ARCHIVE          specify the archive repository"))
  (display (G_ "
  -l, --list-archives            list ELPA repositories supported by the importer"))
  (display (G_ "
  -h, --help                     display this help and exit"))
  (display (G_ "
  -r, --recursive                generate package expressions for all Emacs packages that are not yet in Guix"))
  (display (G_ "
  -V, --version                  display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix import elpa")))
         (option '(#\a "archive") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'repo (string->symbol arg)
                               (alist-delete 'repo result))))
         (option '(#\l "list-archives") #f #f
                 (lambda (opt name arg result)
                   (display (G_ "The following archives are supported:\n"))
                   (for-each (match-lambda
                               ((sym . repo)
                                (format #t "  ~a: ~a\n" sym repo)))
                             %elpa-archives)
                   (display
                    (G_ "The argument to --archive should be one of these \
symbols, e.g. gnu (the default).\n"))
                   (exit 0)))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-elpa . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts))))
    (match args
      ((spec)
       (define-values (package-name version)
         (package-name->name+version spec))
       (when version
         (warning (G_ "this importer does not consider the version~%")))
       (match (if (assoc-ref opts 'recursive)
                  (elpa-recursive-import package-name
                                         (or (assoc-ref opts 'repo) 'gnu))
                  (elpa->guix-package package-name
                                      #:repo (assoc-ref opts 'repo)))
         ((or #f '())
          (leave (G_ "failed to download meta-data for package '~a'~%") package-name))
         (('package etc ...) `(package ,@etc))
         ((? list? sexps) (map
                           (match-lambda
                             ((and ('package ('name name) . rest) pkg)
                              `(define-public ,(string->symbol name)
                                 ,pkg))
                             (_ #f))
                           sexps))))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))

;;; elpa.scm ends here
