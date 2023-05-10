;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu tests pam)
  #:use-module (gnu tests)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system pam)
  #:use-module (gnu system vm)
  #:use-module (guix gexp)
  #:use-module (ice-9 format)
  #:export (%test-pam-limits
            %test-pam-limits-deprecated))


;;;
;;; pam-limits-service-type
;;;

(define pam-limit-entries
  (list
   (pam-limits-entry "@realtime" 'both 'rtprio 99)
   (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

(define (run-test-pam-limits config)
  "Run tests in a os with pam-limits-service-type configured."
  (define os
    (marionette-operating-system
     (simple-operating-system
      (service pam-limits-service-type config))))

  (define vm
    (virtual-machine os))

  (define name (format #f "pam-limit-service~:[~;-deprecated~]"
                       (file-like? config)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (let ((marionette (make-marionette (list #$vm))))

            (test-runner-current (system-test-runner #$output))

            (test-begin #$name)

            (test-assert "/etc/security/limits.conf ready"
              (wait-for-file "/etc/security/limits.conf" marionette))

            (test-equal "/etc/security/limits.conf content matches"
              #$(string-join (map pam-limits-entry->string pam-limit-entries)
                             "\n" 'suffix)
              (marionette-eval
               '(begin
                  (use-modules (rnrs io ports))
                  (call-with-input-file "/etc/security/limits.conf"
                    get-string-all))
               marionette))

            (test-end)))))

  (gexp->derivation (string-append name "-test") test))

(define %test-pam-limits
  (system-test
   (name "pam-limits-service")
   (description "Test that pam-limits-service can serialize its config
(as a list) to @file{limits.conf}.")
   (value (run-test-pam-limits pam-limit-entries))))

(define %test-pam-limits-deprecated
  (system-test
   (name "pam-limits-service-deprecated")
   (description "Test that pam-limits-service can serialize its config
(as a file-like object) to @file{limits.conf}.")
   (value (run-test-pam-limits
           (plain-file "limits.conf"
                       (string-join (map pam-limits-entry->string
                                         pam-limit-entries)
                                    "\n" 'suffix))))))
