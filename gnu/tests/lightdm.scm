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

(define-module (gnu tests lightdm)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages ratpoison)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services lightdm)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:export (%test-lightdm))

(define minimal-desktop-services
  (list polkit-wheel-service
        (service upower-service-type)
        (accountsservice-service)
        (service polkit-service-type)
        (elogind-service)
        (dbus-service)
        x11-socket-directory-service))

(define %lightdm-os
  (operating-system
    (inherit %simple-os)
    (packages (cons* ocrad ratpoison xterm %base-packages))
    (services
     (cons* (service lightdm-service-type
                     (lightdm-configuration
                      (allow-empty-passwords? #t)
                      (debug? #t)
                      (xdmcp? #t)
                      (vnc-server? #t)
                      (vnc-server-command
                       (file-append tigervnc-server "/bin/Xvnc"
                                    "  -SecurityTypes None"))
                      (greeters (list (lightdm-gtk-greeter-configuration
                                       (allow-debugging? #t))))
                      (seats (list (lightdm-seat-configuration
                                    (name "*")
                                    (user-session "ratpoison"))))))

            ;; For debugging.
            (service dhcp-client-service-type)
            (service openssh-service-type
                     (openssh-configuration
                      (permit-root-login #t)
                      (allow-empty-passwords? #t)))
            (append minimal-desktop-services
                    (remove (lambda (service)
                              (eq? (service-kind service) guix-service-type))
                            %base-services))))))

(define (run-lightdm-test)
  "Run tests in %LIGHTDM-OS."

  (define os (marionette-operating-system
              %lightdm-os
              #:imported-modules (source-module-closure
                                  '((gnu services herd)))))

  (define vm (virtual-machine os))

  (define test
    (with-imported-modules (source-module-closure
                            '((gnu build marionette)))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-26)
                       (srfi srfi-64))

          (let ((marionette (make-marionette (list #$vm))))

            (test-runner-current (system-test-runner #$output))
            (test-begin "lightdm")

            (test-assert "service is running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'lightdm))
               marionette))

            (test-assert "service can be stopped"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (stop-service 'lightdm))
               marionette))

            (test-assert "service can be restarted"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (restart-service 'lightdm))
               marionette))

            (test-assert "login screen is displayed"
              ;; GNU Ocrad fails to recognize the "Log In" button text, so use
              ;; Tesseract.
              (wait-for-screen-text marionette
                                    (cut string-contains <> "Log In")
                                    #:ocr #$(file-append tesseract-ocr
                                                         "/bin/tesseract")))

            (test-assert "can connect to TCP port 5900 on IPv4"
              (wait-for-tcp-port 5900 marionette))

            ;; The VNC server fails to listen to IPv6 due to "Error binding to
            ;; address [::]:5900: Address already in use" (see:
            ;; https://github.com/canonical/lightdm/issues/266).
            (test-expect-fail 1)
            (test-assert "can connect to TCP port 5900 on IPv6"
              (wait-for-tcp-port 5900 marionette
                                 #:address
                                 `(make-socket-address
                                   AF_INET6
                                   (inet-pton AF_INET6 "::1")
                                   5900)))

            (test-end)))))

  (gexp->derivation "lightdm-test" test))

(define %test-lightdm
  (system-test
   (name "lightdm")
   (description "Basic tests for the LightDM service.")
   (value (run-lightdm-test))))
