;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Ian Eure <ian@retrospec.tv>
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

(define-module (tests services xorg)
  #:use-module (guix diagnostics)
  #:use-module (guix packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module ((srfi srfi-1) #:select (find))
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services xorg) module.

(define %config-empty (xorg-configuration))

(define %default-server (xorg-configuration-server %config-empty))



(test-begin "merge-xorg-configurations")

(define merge-xorg-configurations
  (@@ (gnu services xorg) merge-xorg-configurations))

(define gdm-configuration-xorg
  (@@ (gnu services xorg) gdm-configuration-xorg))

;; keyboard-layout tests.

(define %config-xorg-keyboard-layout-1
  (xorg-configuration
    (keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))))

(define %config-xorg-keyboard-layout-2
  (xorg-configuration
    (keyboard-layout (keyboard-layout "us" #:options '("ctrl:esc")))))

;; Later keyboard layouts replace earlier defaults
(test-equal
    (keyboard-layout "us" #:options '("ctrl:nocaps"))
  (xorg-configuration-keyboard-layout
   (merge-xorg-configurations
    (list %config-empty %config-xorg-keyboard-layout-1))))

;; Later keyboard layouts replace earlier customizations.
(test-equal
    (keyboard-layout "us" #:options '("ctrl:esc"))
  (xorg-configuration-keyboard-layout
   (merge-xorg-configurations (list %config-empty
                                    %config-xorg-keyboard-layout-1
                                    %config-xorg-keyboard-layout-2))))

;; server, server-arguments tests.

(define %custom-server-1
  (package
    (inherit xorg-server)
    (name "fake-xorg-server")))

(define %custom-server-2
  (package
    (inherit xorg-server)
    (name "another-fake-xorg-server")))

(define %custom-server-1-arguments
  (cons "-nosilk" %default-xorg-server-arguments))

(define %custom-server-2-arguments
  (cons* "-logverbose" "9" %default-xorg-server-arguments))

(define %config-custom-server-1
  (xorg-configuration
    (server %custom-server-1)))

(define %config-custom-server-2
  (xorg-configuration
    (server %custom-server-2)))

(define %config-custom-server-1-and-arguments
  (xorg-configuration
    (inherit %config-custom-server-1)
    (server-arguments %custom-server-1-arguments)))

(define %config-custom-server-2-and-arguments
  (xorg-configuration
    (inherit %config-custom-server-2)
    (server-arguments %custom-server-2-arguments)))

;; Custom server is prioritized over earlier default.
(test-equal
    %custom-server-1
  (xorg-configuration-server
   (merge-xorg-configurations (list %config-empty
                                    %config-custom-server-1))))

;; Custom server preserves arguments.
(test-equal
    (list %custom-server-1 %custom-server-1-arguments)
  (let ((cfg (merge-xorg-configurations
              (list
               %config-empty
               %config-custom-server-1-and-arguments))))
    (list (xorg-configuration-server cfg)
          (xorg-configuration-server-arguments cfg))))

;; Later custom arguments replace earlier.
(test-equal
    (list %custom-server-2 %custom-server-2-arguments)
  (let ((cfg (merge-xorg-configurations
              (list
               %config-empty
               %config-custom-server-1-and-arguments
               %config-custom-server-2-and-arguments))))
    (list (xorg-configuration-server cfg)
          (xorg-configuration-server-arguments cfg))))

;; Custom server is prioritized over later default.
(test-equal
    %custom-server-1
  (xorg-configuration-server
   (merge-xorg-configurations (list %config-custom-server-1
                                    %config-empty))))

;; Custom arguments are prioritized over earlier custom server.
(test-equal
    %custom-server-2-arguments
  (xorg-configuration-server-arguments
   (merge-xorg-configurations
    (list
     (xorg-configuration (server %custom-server-1))
     (xorg-configuration (server-arguments %custom-server-2-arguments))))))

;; Later custom servers are prioritized over earlier.
(test-equal
    %custom-server-2
  (xorg-configuration-server
   (merge-xorg-configurations (list %config-custom-server-1
                                    %config-empty
                                    %config-custom-server-2))))

(test-equal
    %custom-server-2
  (xorg-configuration-server
   (merge-xorg-configurations (list %config-empty
                                    %config-custom-server-1
                                    %config-custom-server-2))))

(test-equal
    %custom-server-1
  (xorg-configuration-server
   (merge-xorg-configurations (list %config-empty
                                    %config-custom-server-1))))

;; Make sure it works in the context of an operating-system.
(test-equal
    %custom-server-2
  (let ((os (operating-system
              (host-name "test")
              (bootloader
                (bootloader-configuration
                  (bootloader grub-bootloader)
                  (targets '("/dev/sdX"))))
              (file-systems
               (cons
                (file-system
                  (device (file-system-label "my-root"))
                  (mount-point "/")
                  (type "ext4"))
                %base-file-systems))
              (services
               (cons*
                (simple-service 'server-2 gdm-service-type
                                %config-custom-server-2)
                (simple-service 'server-1 gdm-service-type
                                %config-custom-server-1)
                (service gdm-service-type)
                %base-services)))))
    (xorg-configuration-server
     (gdm-configuration-xorg
      (service-value
       (fold-services
        (operating-system-services os)
        #:target-type gdm-service-type))))))

;; extra-config tests.

;; Extra configurations append.
(let ((snippet-one "# First")
      (snippet-two "# Second"))
  (test-equal
      (list snippet-one snippet-two)
    (xorg-configuration-extra-config
     (merge-xorg-configurations
      (list (xorg-configuration (extra-config (list snippet-one)))
            (xorg-configuration (extra-config (list snippet-two))))))))

;; drivers tests.

(define %drivers-custom-1 '("done"))
(define %drivers-custom-2 '("dtwo"))

(test-equal
    (append %drivers-custom-1 %drivers-custom-2)
  (xorg-configuration-drivers
   (merge-xorg-configurations
    (list
     (xorg-configuration (drivers %drivers-custom-1))
     (xorg-configuration (drivers %drivers-custom-2))))))

(test-end "merge-xorg-configurations")
