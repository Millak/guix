;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>.
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

(define-module (gnu tests vnstat)
  #:use-module (gnu tests)
  #:use-module ((gnu packages networking) #:select (socat vnstat))
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu services monitoring)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (guix gexp)
  #:use-module (ice-9 format)
  #:export (%test-vnstat))


(define (run-vnstat-test)
  "Run tests in a vm which has vnstat running."

  (define vnstat-config
    (vnstat-configuration
     (max-bandwidth 0)
     (time-sync-wait 0)
     (bandwidth-detection-interval 0)))

  (define inetd-service-entry-config
    (inetd-entry
     (name "discard")
     (socket-type 'stream)
     (protocol "tcp")
     (wait? #t)
     (user "nobody")))

  (define os
    (marionette-operating-system
     (simple-operating-system
      (service dhcp-client-service-type)
      (service vnstat-service-type
               vnstat-config)
      (service inetd-service-type
               (inetd-configuration
                (entries
                 (list inetd-service-entry-config)))))
     #:imported-modules '((gnu services herd))))

  (define forwarded-port 9999)

  (define vm
    ;; The 'discard' port is 9.  Avoid using 'getservbyname' as that might
    ;; fail depending on what /etc/services has (if it's available).
    (let ((guest-port 9))
      (virtual-machine
       (operating-system os)
       (port-forwardings `((,forwarded-port . ,guest-port))))))

  ;; The test duration is inconsistent, at times a test may complete under
  ;; 2 minutes and at times it may take up to 5 minutes.
  (define test-timeout (* 60 5))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (let ((marionette (make-marionette (list #$vm)))
                (pid-file #$(vnstat-configuration-pid-file vnstat-config)))

            (test-runner-current (system-test-runner #$output))
            (test-begin "vnstat")

            (test-assert "service is running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'vnstatd))
               marionette))

            (test-assert "vnstatd ready"
              (wait-for-file pid-file marionette))

            ;; Pump garbage into the 'discard' inetd service within the vm.
            (let* ((socat #$(file-append socat "/bin/socat"))
                   (dest-addr #$(format #f "TCP4:localhost:~d"
                                        forwarded-port))
                   (args `("socat" "-u" "/dev/zero" ,dest-addr))
                   ;; XXX: Guile bug (22/03/2023, Guile 3.0.9)
                   ;; Fixed in main: <https://issues.guix.gnu.org/61073>
                   ;; FIXME: re-add #:output (%make-void-port "w") below on
                   ;; next Guile release.
                   (garbage-pump-pid
                    (spawn socat args)))
              (test-group-with-cleanup "Logging"
                ;; To aid debugging, this test returns #t on success
                ;; and either #f or 'timed-out otherwise.
                (test-eq "vnstatd is logging"
                  #t
                  (marionette-eval
                   '(begin
                      (use-modules (ice-9 popen)
                                   (ice-9 match)
                                   (sxml simple)
                                   (sxml xpath))

                      (define selector
                        (let ((xpath '(vnstat interface traffic total)))
                          (compose (node-pos 1) (sxpath xpath))))

                      (let loop ((i 0))
                        (let* ((vnstat #$(file-append vnstat "/bin/vnstat"))
                               (query-cmd (format #f "~a --xml" vnstat))
                               (proc (compose selector xml->sxml))
                               (result
                                (call-with-port
                                    (open-input-pipe query-cmd) proc)))
                          (match result
                            ;; Counter still warming up.
                            ((('total ('rx "0") ('tx "0")))
                             (sleep 1)
                             (if (< i #$test-timeout)
                                 (loop (+ i 1))
                                 'timed-out))
                            ;; Count of bytes on iface was non-zero.
                            ((('total ('rx rx) ('tx tx)))
                             #t)
                            ;; Unknown data encountered, perhaps the
                            ;; data format changed?
                            (_ #f)))))
                   marionette))
                ;; Cleanup: shutdown garbage pump.
                (kill garbage-pump-pid SIGTERM)))

            (test-end)))))

  (gexp->derivation "vnstat-test" test))

(define %test-vnstat
  (system-test
   (name "vnstat")
   (description "Basic tests for vnstat service.")
   (value (run-vnstat-test))))
