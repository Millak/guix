;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2022 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu tests cachefilesd)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services linux)
  #:use-module (guix gexp)
  #:export (%test-cachefilesd))

(define %cachefilesd-os
  (simple-operating-system
   (service cachefilesd-service-type
            (cachefilesd-configuration
             (cache-directory "/var/cache/fscache")))))

(define (run-cachefilesd-test)
  "Run tests in %cachefilesd-os, which has cachefilesd running."
  (define os
    (marionette-operating-system
     %cachefilesd-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine os))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))
          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "cachefilesd")

          (test-assert "service is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'cachefilesd))
             marionette))

          (test-end))))
  (gexp->derivation "cachefilesd-test" test))

(define %test-cachefilesd
  (system-test
   (name "cachefilesd")
   (description "Test that the cachefilesd runs when started.")
   (value (run-cachefilesd-test))))
