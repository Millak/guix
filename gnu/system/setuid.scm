;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu system setuid)
  #:use-module (gnu system privilege)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (setuid-program
            setuid-program?
            setuid-program-program
            setuid-program-setuid?
            setuid-program-setgid?
            setuid-program-user
            setuid-program-group

            file-like->setuid-program))

;;; Commentary:
;;;
;;; Do not use this module in new code.  It used to define data structures
;;; representing setuid/setgid programs, but is now a mere compatibility shim
;;; wrapping a subset of (gnu system privilege).
;;;
;;; Code:

(define-syntax setuid-program
  (lambda (fields)
    (syntax-case fields ()
      ((_ (field value) ...)
       #`(privileged-program
          (setuid? (match (assoc-ref '((field value) ...) 'setuid?)
                     ((#f) #f)
                     (_ #t)))
          #,@(remove (match-lambda ((f _) (eq? (syntax->datum f) 'setuid?)))
                     #'((field value) ...)))))))

(define setuid-program?        privileged-program?)
(define setuid-program-program privileged-program-program)
(define setuid-program-setuid? privileged-program-setuid?)
(define setuid-program-setgid? privileged-program-setgid?)
(define setuid-program-user    privileged-program-user)
(define setuid-program-group   privileged-program-group)

(define (file-like->setuid-program program)
  (setuid-program (program program)))
