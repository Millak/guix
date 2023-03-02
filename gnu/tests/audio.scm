;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2022 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu tests audio)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services audio)
  #:use-module (gnu services networking)
  #:use-module (gnu packages mpd)
  #:use-module (guix gexp)
  #:export (%test-mpd
            %test-mympd))

(define %mpd-os
  (simple-operating-system
   (service mpd-service-type)))

(define (run-mpd-test)
  "Run tests in %mpd-os, which has mpd running."
  (define os
    (marionette-operating-system
     %mpd-os
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
          (test-begin "mpd")

          (test-assert "service is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'mpd))
             marionette))

          (test-assert "mpd listening"
            ;; Wait until mpd is actually listening before spawning 'mpc'.
            (wait-for-tcp-port 6600 marionette))

          (test-equal "mpc connect"
            0
            (marionette-eval
             '(system* #$(file-append mpd-mpc "/bin/mpc"))
             marionette))

          (test-end))))
  (gexp->derivation "mpd-test" test))

(define %test-mpd
  (system-test
   (name "mpd")
   (description "Test that the mpd can run and be connected to.")
   (value (run-mpd-test))))

(define (run-mympd-test)
  (define os (marionette-operating-system
              (simple-operating-system (service dhcp-client-service-type)
                                       (service mympd-service-type))
              #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((8080 . 80)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (srfi srfi-8)
                       (web client)
                       (web response)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "mympd")
          (test-assert "service is running"
            (marionette-eval '(begin
                                (use-modules (gnu services herd))

                                (start-service 'mympd))
                             marionette))

          (test-assert "HTTP port ready"
            (wait-for-tcp-port 80 marionette))

          (test-equal "http-head"
            200
            (receive (x _) (http-head "http://localhost:8080") (response-code x)))

          (test-end))))
  (gexp->derivation "mympd-test" test))

(define %test-mympd
  (system-test
   (name "mympd")
   (description "Connect to a running myMPD service.")
   (value (run-mympd-test))))
