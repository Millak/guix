;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2025 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
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

(define-module (gnu system images rock-4c-plus)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (guix platforms arm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (srfi srfi-26)
  #:export (rock-4c-plus-barebones-os
            rock-4c-plus-image-type
            rock-4c-plus-barebones-raw-image))

(define rock-4c-plus-barebones-os
  (operating-system
    (host-name "huesca")
    (timezone "Europe/Madrid")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-rock-4c-plus-rk3399-bootloader)
                 (targets '("/dev/sda"))))
    (initrd-modules '())
    (kernel linux-libre-arm64-generic)
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (services (append (list
                       (service
                        agetty-service-type
                        (agetty-configuration
                          (extra-options '("-L")) ;no carrier detect
                          ;; Upstream u-boot uses 1500000 as well, so the
                          ;; u-boot-rock-4c-plus-rk3399 inherit it too.
                          (baud-rate "1500000")
                          (term "vt100")
                          (tty "ttyS2")))
                       (service dhcpcd-service-type))
                      %base-services))))

(define rock-4c-plus-image-type
  (image-type
   (name 'rock-4c-plus-raw)
   (constructor (lambda (os)
                  (image
                   (inherit (raw-with-offset-disk-image (expt 2 24)))
                   (operating-system os)
                   (platform aarch64-linux))))))

(define rock-4c-plus-barebones-raw-image
  (image
   (inherit
    (os+platform->image rock-4c-plus-barebones-os aarch64-linux
                        #:type rock-4c-plus-image-type))
   (name 'rock-4c-plus-barebones-raw-image)))

rock-4c-plus-barebones-raw-image
