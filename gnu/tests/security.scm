;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 muradm <mail@muradm.net>
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
  #:use-module (gnu services)
  #:use-module (gnu services security)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:export (%test-fail2ban-basic
            %test-fail2ban-extension
            %test-fail2ban-simple))


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
   "fail2ban-basic-test"

   (simple-operating-system
    (service fail2ban-service-type (fail2ban-configuration
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
    (service (fail2ban-jail-service openssh-service-type (fail2ban-jail-configuration
                                                          (name "sshd") (enabled? #t)))
             (openssh-configuration)))

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
