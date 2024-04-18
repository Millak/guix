;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Herman Rimm <herman@rimm.ee>
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

(define-module (gnu system images orangepi-r1-plus-lts-rk3328)
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
  #:export (orangepi-r1-plus-lts-rk3328-barebones-os
            orangepi-r1-plus-lts-rk3328-image-type
            orangepi-r1-plus-lts-rk3328-barebones-raw-image))

(define orangepi-r1-plus-lts-rk3328-barebones-os
  (operating-system
    (host-name "windmolen")
    (timezone "Europe/Amsterdam")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                  (bootloader u-boot-orangepi-r1-plus-lts-rk3328-bootloader)
                  (targets '("/dev/mmcblk0"))))
    (initrd-modules '())
    (kernel linux-libre-arm64-generic)
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4")) %base-file-systems))
    (services
     (cons* (service agetty-service-type
                     (agetty-configuration (extra-options '("-L")) ;no carrier detect
                                           (baud-rate "1500000")
                                           (term "vt100")
                                           (tty "ttyS2")))
            (service dhcp-client-service-type)
            (service ntp-service-type) %base-services))))

(define orangepi-r1-plus-lts-rk3328-image-type
  (image-type (name 'orangepi-r1-plus-lts-rk3328-raw)
              (constructor (lambda (os)
                             (image (inherit (raw-with-offset-disk-image (expt
                                                                          2 24)))
                                    (operating-system
                                      os)
                                    (platform aarch64-linux))))))

(define orangepi-r1-plus-lts-rk3328-barebones-raw-image
  (image (inherit (os+platform->image orangepi-r1-plus-lts-rk3328-barebones-os
                   aarch64-linux
                   #:type orangepi-r1-plus-lts-rk3328-image-type))
         (name 'orangepi-r1-plus-lts-rk3328-barebones-raw-image)))

orangepi-r1-plus-lts-rk3328-barebones-raw-image
