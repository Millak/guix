;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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
  -h, --help                     display this help and exit"))
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
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-elpa . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (G_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (alist-cons 'argument arg result))
                %default-options))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts))))
    (match args
      ((package-name)
       (let ((sexp (elpa->guix-package package-name (assoc-ref opts 'repo))))
         (unless sexp
           (leave (G_ "failed to download package '~a'~%") package-name))
         sexp))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))

;;; elpa.scm ends here
