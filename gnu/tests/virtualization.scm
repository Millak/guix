;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2020-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu tests virtualization)
  #:use-module (gnu tests)
  #:use-module (gnu image)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system images hurd)
  #:use-module ((gnu system shadow) #:select (%base-user-accounts))
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages virtualization)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:export (%test-libvirt
            %test-qemu-guest-agent
            %test-childhurd
            %test-build-vm))


;;;
;;; Libvirt.
;;;

(define %libvirt-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   (service libvirt-service-type)))

(define (run-libvirt-test)
  "Run tests in %LIBVIRT-OS."
  (define os
    (marionette-operating-system
     %libvirt-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '())
     (memory-size 512)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "libvirt")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'libvirtd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          ;; Give the libvirtd service time to start up.
          (sleep 1)

          (test-eq "fetch version"
            0
            (marionette-eval
             `(begin
                (chdir "/tmp")
                (system* ,(string-append #$libvirt "/bin/virsh")
                         "-c" "qemu:///system" "version"))
             marionette))

          (test-eq "connect"
            0
            (marionette-eval
             `(begin
                (chdir "/tmp")
                (system* ,(string-append #$libvirt "/bin/virsh")
                         "-c" "qemu:///system" "connect"))
             marionette))

          (test-eq "create default network"
            0
            (marionette-eval
             '(begin
                (chdir "/tmp")
                (system* #$(file-append libvirt "/bin/virsh")
                         "-c" "qemu:///system" "net-define"
                         #$(file-append libvirt
                                        "/etc/libvirt/qemu/networks/default.xml")))
             marionette))

          (test-eq "start default network"
            0
            (marionette-eval
             '(begin
                (chdir "/tmp")
                (system* #$(file-append libvirt "/bin/virsh")
                         "-c" "qemu:///system" "net-start" "default"))
             marionette))

          (test-assert "configured firmwares are available to libvirt"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen)
                             (ice-9 textual-ports)
                             (srfi srfi-1)
                             (srfi srfi-26))
                (let* ((conf-firmwares (list #$@(libvirt-configuration-firmwares
                                                 (libvirt-configuration))))
                       (virsh #$(file-append libvirt "/bin/virsh"))
                       (input-pipe (open-pipe*
                                    OPEN_READ
                                    virsh "-c" "qemu:///system"
                                    "domcapabilities" "--xpath"
                                    "/domainCapabilities/os/loader/value/text()"))
                       (output (get-string-all input-pipe))
                       (found-firmwares (string-split (string-trim-both output)
                                                      #\newline)))
                  (close-pipe input-pipe)
                  ;; Check that every configured firmware package is covered
                  ;; by at least by one firmware file available to libvirt.
                  (every (lambda (conf-firmware)
                           ;; The firmwares listed by virsh contains their
                           ;; full file names, not just their package output.
                           (any (cut string-prefix? conf-firmware <>)
                                found-firmwares))
                         conf-firmwares)))
             marionette))

          (test-end))))

  (gexp->derivation "libvirt-test" test))

(define %test-libvirt
  (system-test
   (name "libvirt")
   (description "Connect to the running LIBVIRT service.")
   (value (run-libvirt-test))))


;;;
;;; QEMU Guest Agent service.
;;;

(define %qemu-guest-agent-os
  (simple-operating-system
   (service qemu-guest-agent-service-type)))

(define (run-qemu-guest-agent-test)
  "Run tests in %QEMU-GUEST-AGENT-OS."
  (define os
    (marionette-operating-system
     %qemu-guest-agent-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 rdelim)
                       (srfi srfi-64))

          (define marionette
            ;; Ensure we look for the socket in the correct place below.
            (make-marionette (list #$vm) #:socket-directory "/tmp"))

          (define* (try-read port #:optional (attempts 10))
            ;; Try reading from a port several times before giving up.
            (cond ((char-ready? port)
                   (let ((response (read-line port)))
                     (close-port port)
                     response))
                  ((> attempts 1)
                   (sleep 1)
                   (try-read port (- attempts 1)))
                  (else "")))

          (define (run command)
            ;; Run a QEMU guest agent command and return the response.
            (let ((s (socket PF_UNIX SOCK_STREAM 0)))
              (connect s AF_UNIX "/tmp/qemu-ga")
              (display command s)
              (try-read s)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "qemu-guest-agent")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'qemu-guest-agent)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          (test-equal "ping guest"
            "{\"return\": {}}"
            (run "{\"execute\": \"guest-ping\"}"))

          (test-assert "get network interfaces"
            (string-contains
             (run "{\"execute\": \"guest-network-get-interfaces\"}")
             "127.0.0.1"))

          (test-end))))

  (gexp->derivation "qemu-guest-agent-test" test))

(define %test-qemu-guest-agent
  (system-test
   (name "qemu-guest-agent")
   (description "Run commands in a virtual machine using QEMU guest agent.")
   (value (run-qemu-guest-agent-test))))


;;;
;;; GNU/Hurd virtual machines, aka. childhurds.
;;;

(define %childhurd-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service hurd-vm-service-type
            (hurd-vm-configuration
             (os (operating-system
                   (inherit %hurd-vm-operating-system)
                   (users (cons (user-account
                                 (name "test")
                                 (group "users")
                                 (password ""))   ;empty password
                                %base-user-accounts))))))))

(define* (run-command-over-ssh command
                               #:key (port 10022) (user "test"))
  "Return a program that runs COMMAND over SSH and prints the result on standard
output."
  (define run
    (with-extensions (list guile-ssh)
      #~(begin
          (use-modules (ssh session)
                       (ssh auth)
                       (ssh popen)
                       (ice-9 match)
                       (ice-9 textual-ports))

          (let ((session (make-session #:user #$user
                                       #:port #$port
                                       #:host "localhost"
                                       #:timeout 120
                                       #:log-verbosity 'rare)))
            (match (connect! session)
              ('ok
               (userauth-password! session "")
               (display
                (get-string-all
                 (open-remote-input-pipe* session #$@command))))
              (status
               (error "could not connect to guest over SSH"
                      session status)))))))

  (program-file "run-command-over-ssh" run))

(define (run-childhurd-test)
  (define (import-module? module)
    ;; This module is optional and depends on Guile-Gcrypt, do skip it.
    (and (guix-module-name? module)
         (not (equal? module '(guix store deduplication)))))

  (define os
    (marionette-operating-system
     %childhurd-os
     #:imported-modules (source-module-closure
                         '((gnu services herd)
                           (guix combinators)
                           (gnu build install))
                         #:select? import-module?)))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size (* 1024 3))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            ;; Emulate as much as the host CPU supports so that, possibly, KVM
            ;; is available inside as well ("nested KVM"), provided
            ;; /sys/module/kvm_intel/parameters/nested (or similar) allows it.
            (make-marionette (list #$vm "-cpu" "max")))

          (test-runner-current (system-test-runner #$output))
          (test-begin "childhurd")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd)
                             (ice-9 match))

                (match (start-service 'childhurd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          (test-equal "childhurd SSH server replies"
            "SSH"
            ;; Check from within the guest whether its childhurd's SSH
            ;; server is reachable.  Do that from the guest: port forwarding
            ;; to the host won't work because QEMU listens on 127.0.0.1.
            (marionette-eval
             '(begin
                (use-modules (ice-9 match)
                             (ice-9 textual-ports))

                (let loop ((n 60))
                  (if (zero? n)
                      'all-attempts-failed
                      (let ((s (socket PF_INET SOCK_STREAM 0))
                            (a (make-socket-address AF_INET
                                                    INADDR_LOOPBACK
                                                    10022)))
                        (format #t "connecting to childhurd SSH server...~%")
                        (connect s a)
                        (match (get-string-n s 3)
                          ((? eof-object?)
                           (close-port s)
                           (sleep 1)
                           (loop (- n 1)))
                          (str
                           (close-port s)
                           str))))))
             marionette))

          (test-equal "SSH up and running"
            "childhurd GNU\n"

            ;; Connect from the guest to the chidhurd over SSH and run the
            ;; 'uname' command.
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen)
                             (ice-9 textual-ports))

                (get-string-all
                 (open-input-pipe #$(run-command-over-ssh '("uname" "-on")))))
             marionette))

          (test-assert "guix-daemon up and running"
            (let ((drv (marionette-eval
                        '(begin
                           (use-modules (ice-9 popen)
                                        (ice-9 textual-ports))

                           (get-string-all
                            (open-input-pipe
                             #$(run-command-over-ssh
                                '("guix" "build" "coreutils"
                                  "--no-grafts" "-d")))))
                        marionette)))
              ;; We cannot compare the .drv with (raw-derivation-file
              ;; coreutils) on the host: they may differ due to fixed-output
              ;; derivations and changes introduced compared to the 'guix'
              ;; package snapshot.
              (and (string-suffix? ".drv"
                                   (pk 'drv (string-trim-right drv)))
                   drv)))

          (test-assert "copy-on-write store"
            ;; Set up a writable store.  The root partition is already an
            ;; overlayfs, which is not suitable as the bottom part of this
            ;; additional overlayfs; thus, create a tmpfs for the backing
            ;; store.
            ;; TODO: Remove this when <virtual-machine> creates a writable
            ;; store.
            (marionette-eval
             '(begin
                (use-modules (gnu build install)
                             (guix build syscalls))

                (mkdir "/run/writable-store")
                (mount "none" "/run/writable-store" "tmpfs")
                (mount-cow-store "/run/writable-store" "/backing-store")
                (system* "df" "-hT"))
             marionette))

          (test-equal "offloading"
            0
            (marionette-eval
             '(and (file-exists? "/etc/guix/machines.scm")
                   (system* "guix" "offload" "test"))
             marionette))

          (test-end))))

  (gexp->derivation "childhurd-test" test))

(define %test-childhurd
  (system-test
   (name "childhurd")
   (description
    "Connect to the GNU/Hurd virtual machine service, aka. a childhurd, making
sure that the childhurd boots and runs its SSH server.")
   (value (run-childhurd-test))))


;;;
;;; Virtual build machine.
;;;

(define %build-vm-os
  (simple-operating-system
   (service virtual-build-machine-service-type
            (virtual-build-machine
             (cpu-count 1)
             (memory-size (* 1 1024))))))

(define (run-build-vm-test)
  (define (import-module? module)
    ;; This module is optional and depends on Guile-Gcrypt, do skip it.
    (and (guix-module-name? module)
         (not (equal? module '(guix store deduplication)))))

  (define os
    (marionette-operating-system
     %build-vm-os
     #:imported-modules (source-module-closure
                         '((gnu services herd)
                           (gnu build install))
                         #:select? import-module?)))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size (* 1024 3))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            ;; Emulate as much as the host CPU supports so that, possibly, KVM
            ;; is available inside as well ("nested KVM"), provided
            ;; /sys/module/kvm_intel/parameters/nested (or similar) allows it.
            (make-marionette (list #$vm "-cpu" "max")))

          (test-runner-current (system-test-runner #$output))
          (test-begin "build-vm")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd)
                             (ice-9 match))

                (start-service 'build-vm))
             marionette))

          (test-assert "guest SSH up and running"
            ;; Note: Pass #:peek? #t because due to the way QEMU port
            ;; forwarding works, connecting to 11022 always works even if the
            ;; 'sshd' service hasn't been started yet in the guest.
            (wait-for-tcp-port 11022 marionette
                               #:peek? #t))

          (test-assert "copy-on-write store"
            ;; Set up a writable store.  The root partition is already an
            ;; overlayfs, which is not suitable as the bottom part of this
            ;; additional overlayfs; thus, create a tmpfs for the backing
            ;; store.
            ;; TODO: Remove this when <virtual-machine> creates a writable
            ;; store.
            (marionette-eval
             '(begin
                (use-modules (gnu build install)
                             (guix build syscalls))

                (mkdir "/run/writable-store")
                (mount "none" "/run/writable-store" "tmpfs")
                (mount-cow-store "/run/writable-store" "/backing-store")
                (system* "df" "-hT"))
             marionette))

          (test-equal "offloading"
            0
            (marionette-eval
             '(and (file-exists? "/etc/guix/machines.scm")
                   (system* "guix" "offload" "test"))
             marionette))

          (test-end))))

  (gexp->derivation "build-vm-test" test))

(define %test-build-vm
  (system-test
   (name "build-vm")
   (description
    "Offload to a virtual build machine over SSH.")
   (value (run-build-vm-test))))
