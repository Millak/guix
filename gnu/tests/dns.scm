;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Sören Tempel <soeren@soeren-tempel.net>
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

(define-module (gnu tests dns)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services dns)
  #:use-module (gnu services networking)
  #:use-module (gnu packages dns)
  #:use-module (guix gexp)
  #:export (%test-unbound))

(define %unbound-os
  ;; TODO: Unbound config
  (let ((base-os
          (simple-operating-system
            (service dhcpcd-service-type)
            (service unbound-service-type
                     (unbound-configuration
                       (server
                         (unbound-server
                           (interface '("127.0.0.1" "::1"))
                           (extra-options
                             '((local-data . "example.local A 192.0.2.1"))))))))))
    (operating-system
      (inherit base-os)
      (packages
        (append (list
                  `(,isc-bind "utils")
                  unbound)
                (operating-system-packages base-os))))))

(define (run-unbound-test)
  "Run tests in %unbound-os with a running unbound daemon on localhost."
  (define os
    (marionette-operating-system
     %unbound-os
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
          (test-begin "unbound")

          (test-assert "service is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))

                ;; Make sure the 'unbound-control' and 'host' command is found.
                (setenv "PATH" "/run/current-system/profile/bin:/run/current-system/profile/sbin")

                (start-service 'unbound))
             marionette))

          (test-equal "unbound remote control works"
            0
            (marionette-eval
              '(status:exit-val
                 (system* "unbound-control" "-s" "/run/unbound.sock" "status"))
              marionette))

          ;; We use a custom local-data A record here to avoid depending
          ;; on network access and being able to contact the root servers.
          (test-equal "resolves local-data domain"
            "192.0.2.1"
            (marionette-eval
              '(begin
                 (use-modules (ice-9 popen) (rnrs io ports))

                 (let* ((port (open-input-pipe "dig @127.0.0.1 example.local +short"))
                        (out  (get-string-all port)))
                   (close-port port)
                   (string-drop-right out 1))) ;; drop newline
              marionette))

          (test-end))))
  (gexp->derivation "unbound-test" test))

(define %test-unbound
  (system-test
   (name "unbound")
   (description "Test that the unbound can respond to queries.")
   (value (run-unbound-test))))
