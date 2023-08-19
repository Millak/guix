;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
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
  #:export (%test-pam-limits))


;;;
;;; pam-limits-service-type
;;;

(define pam-limit-entries
  (list
   ;; make sure the limits apply to root (uid 0)
   (pam-limits-entry ":0" 'both 'rtprio 99)               ;default is 0
   (pam-limits-entry ":0" 'both 'memlock 'unlimited)))    ;default is 8192 kbytes

(define (run-test-pam-limits config)
  "Run tests in a os with pam-limits-service-type configured."
  (define os
    (marionette-operating-system
     (simple-operating-system
      (service pam-limits-service-type config))
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine os))

  (define name "pam-limits-service")

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build syscalls))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build syscalls)
                       (srfi srfi-64))

          (let ((marionette (make-marionette (list #$vm))))

            (test-runner-current (system-test-runner #$output))

            (test-begin #$name)

            (test-equal "log in on tty1 and read limits"
              '(("99")                  ;real-time priority
                ("unlimited"))          ;max locked memory

              (begin
                ;; Wait for tty1.
                (marionette-eval '(begin
                                    (use-modules (gnu services herd))
                                    (start-service 'term-tty1))
                                 marionette)

                (marionette-control "sendkey ctrl-alt-f1" marionette)

                ;; Now we can type.
                (marionette-type "root\n" marionette)
                (marionette-type "ulimit -r > real-time-priority\n" marionette)
                (marionette-type "ulimit -l > max-locked-memory\n" marionette)

                ;; Read the two files.
                (marionette-eval '(use-modules (rnrs io ports)) marionette)
                (let ((guest-file (lambda (file)
                                    (string-tokenize
                                     (wait-for-file file marionette
                                                    #:read 'get-string-all)))))
                  (list (guest-file "/root/real-time-priority")
                        (guest-file "/root/max-locked-memory")))))

            (test-end)))))

  (gexp->derivation (string-append name "-test") test))

(define %test-pam-limits
  (system-test
   (name "pam-limits-service")
   (description "Test that pam-limits-service actually sets the limits as
configured.")
   (value (run-test-pam-limits pam-limit-entries))))
