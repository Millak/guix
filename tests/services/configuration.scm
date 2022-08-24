;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (tests services configuration)
  #:use-module (gnu services configuration)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services configuration) module.

(test-begin "services-configuration")

(define (serialize-number field value)
  (format #f "~a=~a" field value))


;;;
;;; define-configuration macro.
;;;

(define-configuration port-configuration
  (port (number 80) "The port number.")
  (no-serialization))

(test-equal "default value, no serialization"
  80
  (port-configuration-port (port-configuration)))

(test-equal "wrong type for a field"
  '("configuration.scm" 57 11)                    ;error location
  (guard (c ((configuration-error? c)
             (let ((loc (error-location c)))
               (list (basename (location-file loc))
                     (location-line loc)
                     (location-column loc)))))
    (port-configuration
     ;; This is line 56; the test relies on line/column numbers!
     (port "This is not a number!"))))

(define-configuration port-configuration-cs
  (port (number 80) "The port number." empty-serializer))

(test-equal "default value, custom serializer"
  80
  (port-configuration-cs-port (port-configuration-cs)))

(define-configuration port-configuration-ndv
  (port (number) "The port number."))

(test-equal "no default value, provided"
  55
  (port-configuration-ndv-port (port-configuration-ndv
                                (port 55))))

(test-assert "no default value, not provided"
  (guard (c ((configuration-error? c)
             #t))
    (port-configuration-ndv-port (port-configuration-ndv))))

(define (custom-number-serializer name value)
  (format #f "~a = ~a;" name value))

(define-configuration serializable-configuration
  (port (number 80) "The port number." custom-number-serializer))

(test-assert "serialize-configuration"
  (gexp?
   (let ((config (serializable-configuration)))
     (serialize-configuration config serializable-configuration-fields))))

(define-configuration serializable-configuration
  (port (number 80) "The port number." custom-number-serializer)
  (no-serialization))

(test-assert "serialize-configuration with no-serialization"
  ;; When serialization is disabled, the serializer is set to #f, so
  ;; attempting to use it fails with a 'wrong-type-arg' error.
  (not (false-if-exception
        (let ((config (serializable-configuration)))
          (serialize-configuration config serializable-configuration-fields)))))

(define (custom-prefix-serialize-integer field-name name) name)

(define-configuration configuration-with-prefix
  (port (integer 10) "The port number.")
  (prefix custom-prefix-))

(test-assert "serialize-configuration with prefix"
  (gexp?
   (let ((config (configuration-with-prefix)))
     (serialize-configuration config configuration-with-prefix-fields))))


;;;
;;; define-maybe macro.
;;;
(define-maybe number)

(define-configuration config-with-maybe-number
  (port  (maybe-number 80) "")
  (count maybe-number ""))

(test-equal "maybe value serialization"
  "port=80"
  (serialize-maybe-number "port" 80))

(define (config-with-maybe-number->string x)
  (eval (gexp->approximate-sexp
         (serialize-configuration x config-with-maybe-number-fields))
        (current-module)))

(test-equal "maybe value serialization of the instance"
  "port=42count=43"
  (config-with-maybe-number->string
   (config-with-maybe-number
    (port 42)
    (count 43))))

(test-equal "maybe value serialization of the instance, unspecified"
  "port=42"
  (config-with-maybe-number->string
   (config-with-maybe-number
    (port 42))))

(define (serialize-symbol name value)
  (format #f "~a=~a~%" name value))

(define-maybe symbol)

(define-configuration config-with-maybe-symbol
  (protocol maybe-symbol ""))

;;; Maybe symbol values are currently seen as serializable, because the
;;; unspecified value is '%unset-marker%, which is a symbol itself.
;;; TODO: Remove expected fail marker after resolution.
(test-expect-fail 1)
(test-equal "symbol maybe value serialization, unspecified"
  ""
  (gexp->approximate-sexp
   (serialize-configuration (config-with-maybe-symbol)
                            config-with-maybe-symbol-fields)))

(define-maybe/no-serialization string)

(define-configuration config-with-maybe-string/no-serialization
  (name (maybe-string) "The name of the item.")
  (no-serialization))

(test-assert "maybe value without serialization no procedure bound"
  (not (defined? 'serialize-maybe-string)))

(test-assert "maybe type, no default"
  (eq? %unset-value
       (config-with-maybe-string/no-serialization-name
        (config-with-maybe-string/no-serialization))))

(test-assert "maybe type, with default"
  (equal?
   "foo"
   (config-with-maybe-string/no-serialization-name
    (config-with-maybe-string/no-serialization
     (name "foo")))))
