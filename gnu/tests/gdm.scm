;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Bruno Victal <mirai@makinata.eu>.
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

(define-module (gnu tests gdm)
  #:use-module (gnu tests)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system vm)
  #:use-module (guix gexp)
  #:use-module (ice-9 format)
  #:export (%test-gdm-x11
            %test-gdm-wayland
            %test-gdm-wayland-tmpfs))

(define* (make-os #:key wayland? tmp-tmpfs?)
  (operating-system
    (inherit %simple-os)
    (services
     (modify-services %desktop-services
       (gdm-service-type config => (gdm-configuration
                                    (inherit config)
                                    (wayland? wayland?)))))
    (file-systems (if tmp-tmpfs? (cons (file-system
                                         (mount-point "/tmp")
                                         (device "none")
                                         (type "tmpfs")
                                         (flags '(no-dev no-suid))
                                         (check? #f))
                                       %base-file-systems)
                      %base-file-systems))))

(define* (run-gdm-test #:key wayland? tmp-tmpfs?)
  "Run tests in a vm which has gdm running."
  (define os
    (marionette-operating-system
     (make-os #:wayland? wayland? #:tmp-tmpfs? tmp-tmpfs?)
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 1024)))

  (define name (format #f "gdm-~:[x11~;wayland~]~:[~;-tmpfs~]" wayland? tmp-tmpfs?))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 format)
                       (srfi srfi-64))

          (let* ((marionette (make-marionette (list #$vm)))
                 (expected-session-type #$(if wayland? "wayland" "x11")))

            (test-runner-current (system-test-runner #$output))
            (test-begin #$name)

            ;; service for gdm is called xorg-server
            (test-assert "service is running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'xorg-server))
               marionette))

            (test-assert "gdm ready"
              (wait-for-file "/var/run/gdm/gdm.pid" marionette))

            (test-equal (string-append "session-type is " expected-session-type)
              expected-session-type
              (marionette-eval
               '(begin
                  (use-modules (ice-9 popen)
                               (ice-9 rdelim))

                  (let* ((loginctl #$(file-append elogind "/bin/loginctl"))
                         (get-session-cmd (string-join `(,loginctl "show-user" "gdm"
                                                                   "--property Display" "--value")))
                         (session (call-with-port (open-input-pipe get-session-cmd) read-line))
                         (get-type-cmd (string-join `(,loginctl "show-session" ,session
                                                                "--property Type" "--value")))
                         (type (call-with-port (open-input-pipe get-type-cmd) read-line)))
                    type))
               marionette))

            (test-end)))))

  (gexp->derivation (string-append name "-test") test))

(define %test-gdm-x11
  (system-test
   (name "gdm-x11")
   (description "Basic tests for the GDM service. (X11)")
   (value (run-gdm-test))))

(define %test-gdm-wayland
  (system-test
   (name "gdm-wayland")
   (description "Basic tests for the GDM service. (Wayland)")
   (value (run-gdm-test #:wayland? #t))))

(define %test-gdm-wayland-tmpfs
  (system-test
   ;; See <https://issues.guix.gnu.org/57589>.
   (name "gdm-wayland-tmpfs")
   (description "Basic tests for the GDM service. (Wayland, /tmp as tmpfs)")
   (value (run-gdm-test #:wayland? #t #:tmp-tmpfs? #t))))
