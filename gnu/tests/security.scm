;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2025 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu tests security)
  #:use-module (guix gexp)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services opensnitch)
  #:use-module (gnu services security)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:export (%test-fail2ban-basic
            %test-fail2ban-extension
            %test-fail2ban-simple
            %test-opensnitch))


;;;
;;; fail2ban tests
;;;

(define-syntax-rule (fail2ban-test test-name test-os tests-more ...)
  (lambda ()
    (define os
      (marionette-operating-system
       test-os
       #:imported-modules '((gnu services herd))))

    (define vm
      (virtual-machine
       (operating-system os)
       (port-forwardings '())))

    (define test
      (with-imported-modules '((gnu build marionette)
                               (guix build utils))
        #~(begin
            (use-modules (srfi srfi-64)
                         (gnu build marionette))

            (define marionette (make-marionette (list #$vm)))

            (test-runner-current (system-test-runner #$output))
            (test-begin test-name)

            (test-assert "fail2ban running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'fail2ban))
               marionette))

            (test-assert "fail2ban socket ready"
              (wait-for-unix-socket
               "/var/run/fail2ban/fail2ban.sock" marionette))

            (test-assert "fail2ban running after restart"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (restart-service 'fail2ban))
               marionette))

            (test-assert "fail2ban socket ready after restart"
              (wait-for-unix-socket
               "/var/run/fail2ban/fail2ban.sock" marionette))

            (test-assert "fail2ban pid ready"
              (marionette-eval
               '(file-exists? "/var/run/fail2ban/fail2ban.pid")
               marionette))

            (test-assert "fail2ban log file"
              (marionette-eval
               '(file-exists? "/var/log/fail2ban.log")
               marionette))

            tests-more ...

            (test-end))))

    (gexp->derivation test-name test)))

;; The /var/log/secure file that is monitored by fail2ban has to be present
;; before the service starts. This limitation is discussed here:
;; https://github.com/fail2ban/fail2ban/issues/1593.
(define %auth-log-activation-service
  (simple-service 'create-authlog-file
                  activation-service-type
                  #~(begin
                      (call-with-output-file "/var/log/secure"
                        (lambda (port)
                          (display "" port))))))

(define run-fail2ban-basic-test
  (fail2ban-test
   "fail2ban-basic-test"

   (simple-operating-system
    (service fail2ban-service-type))))

(define %test-fail2ban-basic
  (system-test
   (name "fail2ban-basic")
   (description "Test basic fail2ban running capability.")
   (value (run-fail2ban-basic-test))))

(define %fail2ban-server-cmd
  (program-file
   "fail2ban-server-cmd"
   #~(begin
       (let ((cmd #$(file-append fail2ban "/bin/fail2ban-server")))
         (apply execl cmd cmd `("-p" "/var/run/fail2ban/fail2ban.pid"
                                "-s" "/var/run/fail2ban/fail2ban.sock"
                                ,@(cdr (program-arguments))))))))

(define run-fail2ban-simple-test
  (fail2ban-test
   "fail2ban-simple-test"

   (simple-operating-system
    %auth-log-activation-service
    (service fail2ban-service-type
             (fail2ban-configuration
               (jails (list (fail2ban-jail-configuration
                              (name "sshd")))))))

   (test-equal "fail2ban sshd jail running status output"
     '("Status for the jail: sshd"
       "|- Filter"
       "|  |- Currently failed:\t0"
       "|  |- Total failed:\t0"
       "|  `- File list:\t/var/log/secure"
       "`- Actions"
       "   |- Currently banned:\t0"
       "   |- Total banned:\t0"
       "   `- Banned IP list:\t"
       "")
     (marionette-eval
      '(begin
         (use-modules (ice-9 rdelim) (ice-9 popen) (rnrs io ports))
         (let ((call-command
                (lambda (cmd)
                  (let* ((err-cons (pipe))
                         (port (with-error-to-port (cdr err-cons)
                                 (lambda () (open-input-pipe cmd))))
                         (_ (setvbuf (car err-cons) 'block
                                     (* 1024 1024 16)))
                         (result (read-delimited "" port)))
                    (close-port (cdr err-cons))
                    (values result (read-delimited "" (car err-cons)))))))
           (string-split
            (call-command
             (string-join (list #$%fail2ban-server-cmd "status" "sshd") " "))
            #\newline)))
      marionette))

   (test-equal "fail2ban sshd jail running exit code"
     0
     (marionette-eval
      '(status:exit-val (system* #$%fail2ban-server-cmd "status" "sshd"))
      marionette))))

(define %test-fail2ban-simple
  (system-test
   (name "fail2ban-simple")
   (description "Test simple fail2ban running capability.")
   (value (run-fail2ban-simple-test))))

(define run-fail2ban-extension-test
  (fail2ban-test
   "fail2ban-extension-test"

   (simple-operating-system
    %auth-log-activation-service
    (service (fail2ban-jail-service openssh-service-type
                                    (fail2ban-jail-configuration
                                      (name "sshd")
                                      (enabled? #t)))
             (openssh-configuration))
    (service static-networking-service-type
             (list %qemu-static-networking)))

   (test-equal "fail2ban sshd jail running status output"
     '("Status for the jail: sshd"
       "|- Filter"
       "|  |- Currently failed:\t0"
       "|  |- Total failed:\t0"
       "|  `- File list:\t/var/log/secure"
       "`- Actions"
       "   |- Currently banned:\t0"
       "   |- Total banned:\t0"
       "   `- Banned IP list:\t"
       "")
     (marionette-eval
      '(begin
         (use-modules (ice-9 rdelim) (ice-9 popen) (rnrs io ports))
         (let ((call-command
                (lambda (cmd)
                  (let* ((err-cons (pipe))
                         (port (with-error-to-port (cdr err-cons)
                                 (lambda () (open-input-pipe cmd))))
                         (_ (setvbuf (car err-cons) 'block
                                     (* 1024 1024 16)))
                         (result (read-delimited "" port)))
                    (close-port (cdr err-cons))
                    (values result (read-delimited "" (car err-cons)))))))
           (string-split
            (call-command
             (string-join (list #$%fail2ban-server-cmd "status" "sshd") " "))
            #\newline)))
      marionette))

   (test-equal "fail2ban sshd jail running exit code"
     0
     (marionette-eval
      '(status:exit-val (system* #$%fail2ban-server-cmd "status" "sshd"))
      marionette))))

(define %test-fail2ban-extension
  (system-test
   (name "fail2ban-extension")
   (description "Test extension fail2ban running capability.")
   (value (run-fail2ban-extension-test))))


;;;
;;; OpenSnitch tests
;;;

(define (run-opensnitch-test)
  (define os
    (marionette-operating-system
     (simple-operating-system
      (service opensnitch-service-type)
      (service static-networking-service-type
               (list %qemu-static-networking)))
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build utils))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "opensnitch")

          (test-assert "opensnitch running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'opensnitch))
             marionette))

          (test-assert "opensnitch log file"
            (marionette-eval
             '(file-exists? "/var/log/opensnitchd.log")
             marionette))

          (test-assert "opensnitch rules directory"
            (marionette-eval
             '(file-exists? "/etc/opensnitchd/rules")
             marionette))

          (test-assert "opensnitch process running"
            (marionette-eval
             `(zero? (system* ,#$(file-append procps "/bin/pgrep")
                              "-x" "opensnitchd"))
             marionette))

          (test-assert "opensnitch running after restart"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (restart-service 'opensnitch))
             marionette))

          (test-assert "opensnitch process running after restart"
            (marionette-eval
             `(let loop ((tries 0))
                (if (zero? (system* ,#$(file-append procps "/bin/pgrep")
                                    "-x" "opensnitchd"))
                    #t
                    (if (< tries 30)
                        (begin (sleep 1) (loop (+ tries 1)))
                        #f)))
             marionette))

          (test-end))))

  (gexp->derivation "opensnitch-test" test))

(define %test-opensnitch
  (system-test
   (name "opensnitch")
   (description "Test OpenSnitch application firewall daemon.")
   (value (run-opensnitch-test))))
