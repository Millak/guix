;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>
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

(define-module (gnu tests linux)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:export (%test-tuned))

(define %tuned-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service tuned-service-type
            (tuned-configuration
             (power-profiles-daemon-support? #t)))))

(define (run-tuned-service-test)
  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      %tuned-os
      (list))
     #:imported-modules '((gnu build dbus-service)
                          (gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
      (operating-system os)
      (volatile? #f)
      (memory-size 1024)
      (disk-image-size (* 5000 (expt 2 20)))
      (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build dbus-service)
                             (gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-1) (srfi srfi-11) (srfi srfi-64)
                       (gnu build dbus-service)
                       (gnu build marionette))

          (define marionette
            ;; Relax timeout to accommodate older systems and
            ;; allow for pulling the image.
            (make-marionette (list #$vm) #:timeout 60))

          (test-runner-current (system-test-runner #$output))
          (test-begin "tuned-service")

          (marionette-eval
           '(begin
              (use-modules (gnu services herd))
              (wait-for-service 'user-processes))
           marionette)

          (test-assert "tuned running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (wait-for-service 'tuned))
             marionette))

          (test-assert "/etc/tuned/tuned-main.conf exists"
            (wait-for-file "/etc/tuned/tuned-main.conf" marionette))

          (test-equal "/etc/tuned/tuned-main.conf is well formed"
            '("daemon=true"
              "dynamic_tuning=false"
              "default_instance_priority=0"
              "recommend_command=true"
              "sleep_interval=1"
              "update_interval=10")
            (marionette-eval
             '(begin
                (use-modules (srfi srfi-1)
                             (srfi srfi-26)
                             (ice-9 textual-ports))
                ;; The profile_dirs field may contain store paths,
                ;; so we check only the first 7 fields and exclude the last.
                (take
                 (filter
                  (compose not
                           (cut string=? "" <>))
                  (string-split
                   (call-with-input-file "/etc/tuned/tuned-main.conf"
                     get-string-all)
                   #\newline))
                 6))
             marionette))

          (test-assert "tuned-ppd running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (wait-for-service 'tuned-ppd))
             marionette))

          (test-assert "/etc/tuned/ppd.conf exists"
            (wait-for-file "/etc/tuned/ppd.conf" marionette))

          (test-equal "/etc/tuned/ppd.conf is well formed"
            '("[main]"
              "default=balanced"
              "battery_detection=true"
              "sysfs_acpi_monitor=true"

              "[profiles]"
              "power-saver=powersave"
              "balanced=balanced"
              "performance=throughput-performance"

              "[battery]"
              "balanced=balanced-battery")
            (marionette-eval
             '(begin
                (use-modules (srfi srfi-26)
                             (ice-9 textual-ports))
                ;; The profile_dirs field may contain store paths,
                ;; so we check only the first 7 fields and exclude the last.
                (filter
                 (compose not
                          (cut string=? "" <>))
                 (string-split
                  (call-with-input-file "/etc/tuned/ppd.conf"
                    get-string-all)
                  #\newline)))
             marionette))

          (test-assert "get tuned profile"
            (begin
              (define (run-test)
                (marionette-eval
                 `(begin
                    (use-modules (ice-9 popen)
                                 (ice-9 textual-ports))
                    (define slurp
                      (lambda args
                        (let* ((port
                                (apply open-pipe* OPEN_READ
                                       (list "sh" "-l" "-c"
                                             (string-join args " "))))
                               (output (get-string-all port))
                               (status (close-pipe port)))
                          output)))
                    (slurp "tuned-adm" "active"))
                 marionette))
              ;; Allow services to come up on slower machines.
              (with-retries
               80 1
               (string=? "Current active profile: balanced\n" (run-test)))))

          (test-assert "set tuned profile"
            (begin
              (define (run-test)
                (marionette-eval
                 `(begin
                    (use-modules (ice-9 popen)
                                 (ice-9 textual-ports))
                    (define slurp
                      (lambda args
                        (let* ((port
                                (apply open-pipe* OPEN_READ
                                       (list "sh" "-l" "-c"
                                             (string-join args " "))))
                               (output (get-string-all port))
                               (status (close-pipe port)))
                          output)))
                    (slurp "tuned-adm" "profile" "powersave")
                    (slurp "tuned-adm" "active"))
                 marionette))
              ;; Allow services to come up on slower machines.
              (with-retries
               80 1
               (string=? "Current active profile: powersave\n" (run-test)))))

          (test-end))))

  (gexp->derivation "tuned-service-test" test))

(define %test-tuned
  (system-test
   (name "tuned")
   (description "Test TuneD service.")
   (value (run-tuned-service-test))))
