;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>.
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

(define-module (gnu tests vnc)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages glib)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services vnc)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:export (%test-xvnc))

(define %xvnc-os
  (operating-system
    ;; Usual boilerplate.
    (host-name "komputilo")
    (timezone "Europe/Berlin")
    (locale "en_US.UTF-8")
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/sdX"))))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))

    (users (cons (user-account
                  (name "dummy")
                  (group "users")
                  (supplementary-groups '("wheel" "netdev"
                                          "audio" "video")))
                 %base-user-accounts))
    (packages (append (map specification->package
                           '("dbus"     ;for dbus-run-session
                             "dconf"
                             "gnome-settings-daemon" ;for schemas
                             "ratpoison"
                             "tigervnc-client"
                             "xterm"))
                      %base-packages
                      (list `(,glib "bin")
                            glib)))
    (services (cons*
               (service openssh-service-type (openssh-configuration
                                              (permit-root-login #t)
                                              (allow-empty-passwords? #t)))
               (service xvnc-service-type (xvnc-configuration
                                           (display-number 5)
                                           (security-types (list "None"))
                                           (log-level 100)
                                           (localhost? #f)
                                           (xdmcp? #t)
                                           (inetd? #t)))
               (modify-services %desktop-services
                 (gdm-service-type config => (gdm-configuration
                                              (inherit config)
                                              (auto-login? #t)
                                              (auto-suspend? #f)
                                              (default-user "root")
                                              (debug? #t)
                                              (xdmcp? #t))))))))

(define (run-xvnc-test)
  "Run tests in %XVNC-OS."

  (define os (marionette-operating-system
              %xvnc-os
              #:imported-modules (source-module-closure
                                  '((gnu services herd)))))

  (define vm (virtual-machine
              (operating-system os)
              (memory-size 1024)))

  (define test
    (with-imported-modules (source-module-closure
                            '((gnu build marionette)
                              (guix build utils)))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build utils)
                       (srfi srfi-26)
                       (srfi srfi-64))

          (let ((marionette (make-marionette (list #$vm))))

            (test-runner-current (system-test-runner #$output))
            (test-begin "xvnc")

            (test-assert "service running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'xvnc))
               marionette))

            (test-assert "wait for port 5905, IPv4"
              (wait-for-tcp-port 5905 marionette))

            (test-assert "wait for port 5905, IPv6"
              (wait-for-tcp-port 5905 marionette
                                 #:address
                                 '(make-socket-address
                                   AF_INET6 (inet-pton AF_INET6 "::1") 5905)))

            (test-assert "gdm auto-suspend is disabled"
              ;; More a GDM than a Xvnc test, but since it's a cross-cutting
              ;; concern and we have everything set up here, we might as well
              ;; check it here.
              (marionette-eval
               '(begin
                  ;; Check that DCONF_PROFILE is set...
                  (invoke "/bin/sh" "-lc" "\
pgrep gdm | head -n1 | xargs -I{} grep -Fq DCONF_PROFILE /proc/{}/environ")

                  ;; ... and that
                  (invoke "/bin/sh" "-lc" "\
sudo -E -u gdm env DCONF_PROFILE=/etc/dconf/profile/gdm dbus-run-session \
gsettings get org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type \
| grep -Fq nothing"))
               marionette))

            (test-assert "vnc lands on the gdm login screen"
              ;; This test runs vncviewer on the local VM and verifies that it
              ;; manages to access the GDM login screen (via XDMCP).
              (begin
                (define (ratpoison-abort)
                  (marionette-control "sendkey ctrl-g" marionette))

                (define (ratpoison-help)
                  (marionette-control "sendkey ctrl-t" marionette)
                  (marionette-type "?" marionette)
                  (sleep 1))            ;wait for help screen to appear

                (define (ratpoison-exec command)
                  (marionette-control "sendkey ctrl-t" marionette)
                  (marionette-type "!" marionette)
                  (marionette-type (string-append command "\n") marionette))

                ;; Wait until the ratpoison help screen can be displayed; this
                ;; means the window manager is ready.
                (wait-for-screen-text marionette
                                      (cut string-contains <> "key bindings")
                                      #:ocr #$(file-append tesseract-ocr
                                                           "/bin/tesseract")
                                      #:pre-action ratpoison-help
                                      #:post-action ratpoison-abort)

                ;; Run vncviewer and expect the GDM login screen (accessed via
                ;; XDMCP).  This can take a while to appear on slower machines.
                (ratpoison-exec "vncviewer localhost:5905")
                ;; XXX: tesseract narrowly recognizes "Guix" as "uix" from the
                ;; background image; ocrad fares worst.  Sadly, 'Username' is
                ;; not recognized at all.
                (wait-for-screen-text marionette
                                      (cut string-contains <> "uix")
                                      #:ocr #$(file-append tesseract-ocr
                                                           "/bin/tesseract")
                                      #:timeout 120)))

            (test-end)))))

  (gexp->derivation "xvnc-test" test))

(define %test-xvnc
  (system-test
   (name "xvnc")
   (description "Basic tests for the Xvnc service.  One of the tests validate
that XDMCP works with GDM, and is therefore heavy in terms of disk and memory
requirements.")
   (value (run-xvnc-test))))
