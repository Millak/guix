;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2025 Artur Wroblewski <wrobell@riseup.net>
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

(define-module (gnu tests high-availability)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services high-availability)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-rabbitmq))

(define %rabbitmq-config-file
  (plain-file "rabbitmq.conf" "
listeners.tcp.1 = 127.0.0.1:15672
listeners.tcp.2 = ::1:15672
"))

(define %rabbitmq-os
  (simple-operating-system
    (service rabbitmq-service-type
             (rabbitmq-configuration (config-file %rabbitmq-config-file)))))

(define* (run-rabbitmq-test #:key (rabbitmq-port 15672))
  "Run tests in %RABBITMQ-OS, forwarding PORT."
  (define os
    (marionette-operating-system
      %rabbitmq-os
      #:imported-modules '((gnu services herd)
                           (guix combinators))))

  (define forwarded-port 15672)

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 512)
     (port-forwardings `((,rabbitmq-port . ,forwarded-port)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette)
                       (ice-9 rdelim))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-runner-current (system-test-runner #$output))
          (test-begin "rabbitmq")

          ;; Wait for RabbitMQ broker to be up and running.
          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'rabbitmq)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((#t) #t)
                     ((pid) pid)))))
             marionette))

          (test-assert "RabbitMQ port ready"
            (wait-for-tcp-port #$forwarded-port marionette))

          (test-assert "RabbitMQ log file exists"
            (marionette-eval
             '(file-exists? "/var/log/rabbitmq/rabbit@komputilo.log")
             marionette))

          (test-end))))

  (gexp->derivation "rabbitmq-test" test))

(define %test-rabbitmq
  (system-test
   (name "rabbitmq")
   (description "Connect to a running RABBITMQ server.")
   (value (run-rabbitmq-test))))
