;;; GNU Guix --- Functional package management for GNU
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

(use-modules (guix packages)
             (guix gexp)
             (gnu services)
             (gnu tests install)
             (ice-9 match)
             (guix utils)
             (srfi srfi-26)
             (srfi srfi-1))

;; TODO find a better way to check for thunked fields
(define* (thunked-field? field obj)
  (and
   (procedure? obj)
   (eq?
    (car (procedure-minimum-arity obj))
    1)
   ; Uhhh
   (string-suffix? "(x)>"
                   (format #f "~a" obj))))

;; Prints names of all packages, prepending the service they
;; come from.
(define* (print-all-packages obj #:optional (pred package?))
  (match obj
    ((? pred)
     (format #t "\"~a\"~%" (package-name obj))
     (list obj))
    ((? gexp?)
     (list))
    ((a . b)
     (append (print-all-packages a pred)
           (print-all-packages b pred)))
    ((_ ...)
     (apply append
            (map (cut print-all-packages <> pred)
                 obj)))
    (#(_ ...)
     (print-all-packages (vector->list obj) proc))
    ((or (? service-type?)
         (? origin?))
     (list))
    ((? record?)
      (let* ((record-type (record-type-descriptor obj))
             (record-fields (record-type-fields record-type)))
        (when (service? obj)
          (format #t ";; ~a-service~%" (service-type-name (service-kind obj))))
        (apply append
               (map (lambda (field)
                      (let* ((accessor (record-accessor record-type field))
                             (field-obj (accessor obj)))
                        (if (thunked-field? field field-obj)
                            (print-all-packages (field-obj obj) pred)
                            (print-all-packages field-obj pred))))
                    record-fields))))
    ((? parameter?)
     (list))
    (_
     (list))))

(define full-installer-os
  ((@@ (gnu tests install) installation-target-desktop-os-for-gui-tests)))

(define minimal-installer-os
  ((@@ (gnu tests install) installation-target-os-for-gui-tests)))

(display "Full os packages:")
(print-all-packages full-installer-os)
(display "Minimal os packages:")
(print-all-packages minimal-installer-os)

;; Note that the printed packages will contain duplicates.
;; I recommend to first put packages out of the profile service
;; and out of packages field (they will be above service packages)
;; at the bottom of the list and then filtering only unique lines, ie.
;; with `awk '!a[$0]++'`. And lastly removing services that no longer
;; have packages and also the packages that are already in minimal manifest
;; (in case of desktop manifest)
