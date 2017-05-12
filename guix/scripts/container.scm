;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (guix scripts container)
  #:use-module (ice-9 match)
  #:use-module (guix ui)
  #:export (guix-container))

(define (show-help)
  (display (G_ "Usage: guix container ACTION ARGS...
Build and manipulate Linux containers.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   exec            execute a command inside of an existing container\n"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %actions '("exec"))

(define (resolve-action name)
  (let ((module (resolve-interface
                 `(guix scripts container ,(string->symbol name))))
        (proc (string->symbol (string-append "guix-container-" name))))
    (module-ref module proc)))

(define (guix-container . args)
  (with-error-handling
    (match args
      (()
       (format (current-error-port)
               (G_ "guix container: missing action~%")))
      ((or ("-h") ("--help"))
       (show-help)
       (exit 0))
      (("--version")
       (show-version-and-exit "guix container"))
      ((action args ...)
       (if (member action %actions)
           (apply (resolve-action action) args)
           (format (current-error-port)
                   (G_ "guix container: invalid action~%")))))))
