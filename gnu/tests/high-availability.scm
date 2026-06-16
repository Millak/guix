;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2025-2026 Artur Wroblewski <wrobell@riseup.net>
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
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages high-availability)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services high-availability)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-epmd
            %test-rabbitmq))

;;;
;;; Erlang Port Mapper Daemon (epmd) service.
;;;
(define %epmd-os
  (simple-operating-system
    (service epmd-service-type
             (epmd-configuration (port 14369)))))

(define* (run-epmd-test #:key (epmd-port 14369))
  "Run tests in %EPMD-OS, forwarding PORT."
  (define os
    (marionette-operating-system
      %epmd-os
      #:imported-modules '((gnu services herd)
                           (guix combinators))))

  (define forwarded-port 14369)

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 512)
     (port-forwardings `((,epmd-port . ,forwarded-port)))))

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
          (test-begin "epmd")

          ;; Wait for epmd to be up and running.
          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'epmd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((#t) #t)
                     ((pid) pid)))))
             marionette))

          (test-assert "epmd port ready"
            (wait-for-tcp-port #$forwarded-port marionette))

          (test-assert "epmd connection is successful"
            (marionette-eval
             '(begin
                (use-modules (guix build utils))

                (current-output-port (open-file "/dev/console" "w0"))
                (invoke #$(file-append erlang "/bin/epmd")
                        "-port"
                        "14369"
                        "-names"))
             marionette))
          (test-end))))

  (gexp->derivation "epmd-test" test))

(define %test-epmd
  (system-test
   (name "epmd")
   (description "Connect to a running epmd service.")
   (value (run-epmd-test))))

(define %rabbitmq-config-file
  (plain-file "rabbitmq.conf" "
listeners.tcp.1 = 127.0.0.1:15672
listeners.tcp.2 = ::1:15672

distribution.listener.interface = 127.0.0.1
"))

(define %rabbitmq-os
  (simple-operating-system
    (service rabbitmq-service-type
             (rabbitmq-configuration (node-name "rabbit@komputilo")
                                     (config-file %rabbitmq-config-file)))))

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

          (test-assert "RabbitMQ await startup command is successful"
            (marionette-eval
             '(begin
                (use-modules (guix build utils))

                (current-output-port (open-file "/dev/console" "w0"))
                (invoke #$(file-append rabbitmq "/sbin/rabbitmqctl")
                        "await_startup"
                        "-n"
                        "rabbit@komputilo"))
             marionette))

          (test-assert "RabbitMQ status command is successful"
            (marionette-eval
             '(begin
                (use-modules (guix build utils))

                (current-output-port (open-file "/dev/console" "w0"))
                (invoke #$(file-append rabbitmq "/sbin/rabbitmqctl")
                        "status"
                        "-n"
                        "rabbit@komputilo"))
             marionette))
          (test-end))))

  (gexp->derivation "rabbitmq-test" test))

(define %test-rabbitmq
  (system-test
   (name "rabbitmq")
   (description "Connect to a running RABBITMQ server.")
   (value (run-rabbitmq-test))))
