;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts system edit)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix ui)
  #:autoload   (guix utils) (string-closest)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:autoload   (guix scripts edit) (spawn-editor)
  #:export (guix-system-edit))

(define (service-type-not-found type)
  "Report an error about @var{type} not being found and exit."
  (report-error (G_ "~a: no such service type~%") type)

  (let* ((type      (symbol->string type))
         (available (fold-service-types (lambda (type lst)
                                          (cons (symbol->string
                                                 (service-type-name type))
                                                lst))
                                        '()))
         (closest   (string-closest type available)))
    (unless (or (not closest) (string=? closest type))
      (display-hint (format #f (G_ "Did you mean @code{~a}?~%")
                            closest))))

  (exit 1))


(define (guix-system-edit . args)
  (when (null? args)
    (leave (G_ "no service types specified, nothing to edit~%")))

  (let* ((types (append-map (lambda (type)
                              (let ((type (string->symbol type)))
                                (match (lookup-service-types type)
                                  (() (service-type-not-found type))
                                  ((one) (list one))
                                  (lst
                                   (warning (N_ "~a: ~a matching service type~%"
                                                "~a: ~a matching service types~%"
                                                (length lst))
                                            type (length lst))
                                   lst))))
                            args)))
    (spawn-editor (filter-map service-type-location types))))
