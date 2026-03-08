;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu services configuration environment-variables)
  #:use-module (gnu services configuration)
  #:use-module (gnu services configuration utils)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (serialize-string-environment-variable
            serialize-boolean-environment-variable
            serialize-number-environment-variable
            serialize-maybe-string-environment-variable
            serialize-maybe-boolean-environment-variable
            serialize-maybe-number-environment-variable

            serialize-environment-variables))

(define* (field-name->environment-variable field-name
                                           #:key prefix
                                           (uglify uglify-snake-case))
  "Serializes FIELD-NAME, a field name from @code{(gnu services configuration)},
to an environment variable name through UGLIFY, by default a procedure that is
passed FIELD-NAME, and returns a snake case string representation of
the field name.  Trailing @code{?} in the name are dropped and @code{-} get
replaced by @code{_}.  When PREFIX is a string, it is prepended to the result.
The result of UGLIFY is then upcased and returned.

For example the procedure would convert @code{'a-field} to @code{\"A_FIELD\"}."
  (let ((variable (string-upcase
                   (uglify field-name))))
    (if (string? prefix)
        (string-append prefix variable)
        variable)))

(define* (serialize-string-environment-variable field-name value
                                                #:key prefix
                                                #:allow-other-keys)
  (cons (field-name->environment-variable field-name #:prefix prefix)
        value))

(define* (serialize-maybe-string-environment-variable field-name value
                                                      #:key prefix
                                                      #:allow-other-keys)
  (if (maybe-value-set? value)
      (serialize-string-environment-variable field-name value #:prefix prefix)
      #f))

(define* (serialize-boolean-environment-variable field-name value
                                                 #:key prefix
                                                 (true-value "true")
                                                 (false-value "false")
                                                 #:allow-other-keys)
  (serialize-string-environment-variable
   field-name (if value true-value false-value)
   #:prefix prefix))

(define* (serialize-maybe-boolean-environment-variable field-name value
                                                       #:key prefix
                                                       #:allow-other-keys)
  (if (maybe-value-set? value)
      (serialize-boolean-environment-variable field-name value #:prefix prefix)
      #f))

(define* (serialize-number-environment-variable field-name value
                                                #:key prefix
                                                #:allow-other-keys)
  (cons (field-name->environment-variable field-name #:prefix prefix)
        (number->string value)))

(define* (serialize-maybe-number-environment-variable field-name value
                                                      #:key prefix
                                                      #:allow-other-keys)
  (if (maybe-value-set? value)
      (serialize-number-environment-variable field-name value #:prefix prefix)
      #f))

(define (environment-variable-serializer field)
  (define type (configuration-field-type field))
  (match type
    ('string serialize-string-environment-variable)
    ('maybe-string serialize-maybe-string-environment-variable)
    ('number serialize-number-environment-variable)
    ('integer serialize-number-environment-variable)
    ('positive serialize-number-environment-variable)
    ('maybe-number serialize-maybe-number-environment-variable)
    ('maybe-integer serialize-maybe-number-environment-variable)
    ('maybe-positive serialize-maybe-number-environment-variable)
    ('boolean serialize-boolean-environment-variable)
    ('maybe-boolean serialize-boolean-environment-variable)
    (_
     (raise
      (formatted-message
       (G_ "Unknown environment-variable field type: ~a")
       type)))))

(define* (serialize-environment-variables config fields
                                          #:optional selection negate?
                                          #:key prefix
                                          (true-value "true")
                                          (false-value "false"))
  "Serializes the fields whose name is included in SELECTION from CONFIG, a
configuration from @code{(gnu services configuration)}, and FIELDS, the
list of its field records, to a list of pairs.  When NEGATE? is #t all services
not included in SELECTION will be serialized.  Each  pair represents an
environment variable.  The first element of each pair is the variable name, the
second is the value.  When PREFIX is a string it is prepended to the variable
name.  TRUE-VALUE and FALSE-VALUE will be used as a representation for
respectfully @code{#t} and @code{#f}."
  (define selected-names
    (or selection
        (map configuration-field-name fields)))
  (define filtered
    (filter-configuration-fields fields selected-names negate?))
  (define getters
    (map configuration-field-getter filtered))
  (define names
    (map configuration-field-name filtered))
  (define serializers
    (map environment-variable-serializer filtered))

  (filter-map (match-lambda ((serializer name getter)
                             (serializer name (getter config)
                                         #:prefix prefix
                                         #:true-value true-value
                                         #:false-value false-value)))
              (zip serializers names getters)))
