;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Simon Streit <simon@netpanic.org>
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

(define-module (gnu tests samba)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu services samba)
  #:use-module (gnu packages samba)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-samba
            %test-wsdd))


;;;
;;; The Samba service.
;;;

(define %samba-os
  (let ((base-os (simple-operating-system
                  (simple-service 'create-target-directory activation-service-type
                                  #~(begin
                                      (mkdir-p "/srv/samba/guest")
                                      (chown "/srv/samba/guest"
                                             (passwd:uid (getpw "nobody"))
                                             (passwd:gid (getpw "nobody")))))
                  (service dhcp-client-service-type)
                  (service samba-service-type
                           (samba-configuration
                            (config-file (plain-file "smb.conf" "
[global]
    workgroup = WORKGROUP
    server string = Samba Server
    server role = standalone server
    log file = /var/log/samba/log.%m
    logging = file

[guest]
    path = /srv/samba/guest
    read only = no
    guest ok = yes
    guest only = yes
")))))))
    (operating-system
      (inherit base-os)
      (packages (cons samba (operating-system-packages base-os))))))

(define* (run-samba-test)
  "Return a test of an OS running Samba service."

  (define vm
    (virtual-machine
     (operating-system (marionette-operating-system
                        %samba-os
                        #:imported-modules '((gnu services herd))))
     (port-forwardings '((8135 . 135)
                         (8137 . 137)
                         (8138 . 138)
                         (8445 . 445)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-26)
                       (srfi srfi-64))

          (define marionette
            (make-marionette '(#$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "samba")

          (test-assert "samba-smbd running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'samba-smbd))
             marionette))

          (test-assert "samba-nmbd running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'samba-nmbd))
             marionette))

          (test-assert "samba-winbindd running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'samba-winbindd))
             marionette))

          (test-assert "smbd service process id"
            (let ((pid
                   (number->string (wait-for-file "/var/run/samba/smbd.pid"
                                                  marionette))))
              (marionette-eval `(file-exists? (string-append "/proc/" ,pid))
                               marionette)))

          (test-assert "nmbd service process id"
            (let ((pid
                   (number->string (wait-for-file "/var/run/samba/nmbd.pid"
                                                  marionette))))
              (marionette-eval `(file-exists? (string-append "/proc/" ,pid))
                               marionette)))

          (test-assert "winbindd service process id"
            (let ((pid
                   (number->string (wait-for-file "/var/run/samba/winbindd.pid"
                                                  marionette))))
              (marionette-eval `(file-exists? (string-append "/proc/" ,pid))
                               marionette)))

          (test-assert "samba-smbd is listening for peers"
            (wait-for-tcp-port 445 marionette))

          (test-equal "smbclient connect"
            0
            (marionette-eval
             '(system* #$(file-append samba "/bin/smbclient")
                       "--list=localhost" "--no-pass")
             marionette))

          (test-equal "smbclient connect"
            0
            (marionette-eval
             '(system* #$(file-append samba "/bin/smbclient")
                       "--list=localhost" "--no-pass")
             marionette))

          (test-end))))

  (gexp->derivation "samba-test" test))

(define %test-samba
  (system-test
   (name "samba")
   (description "Connect to a running Samba daemon.")
   (value (run-samba-test))))


;;;
;;; The wsdd service.
;;;

(define %wsdd-os
  (let ((base-os (simple-operating-system
                  (service dhcp-client-service-type)
                  (service wsdd-service-type))))
    (operating-system
      (inherit base-os)
      (packages (cons wsdd (operating-system-packages base-os))))))

(define* (run-wsdd-test)
  "Return a test of an OS running wsdd service."

  (define vm
    (virtual-machine
     (operating-system (marionette-operating-system
                        %wsdd-os
                        #:imported-modules '((gnu services herd))))
     (port-forwardings '((3702 . 3702)
                         (5357 . 5357)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-26)
                       (srfi srfi-64))

          (define marionette
            (make-marionette '(#$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "wsdd")

          ;; Here shall be more tests to begin with.

          (test-assert "wsdd running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'wsdd))
             marionette))

          (test-end))))

  (gexp->derivation "wsdd-test" test))

(define %test-wsdd
  (system-test
   (name "wsdd")
   (description "Connect to a running wsdd daemon.")
   (value (run-wsdd-test))))
