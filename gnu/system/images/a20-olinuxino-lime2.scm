;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Tanguy Le Carrour <tanguy@bioneland.org>
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

(define-module (gnu system images a20-olinuxino-lime2)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (guix platforms arm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (srfi srfi-26)
  #:export (a20-olinuxino-lime2-barebones-os
            a20-olinuxino-lime2-image-type
            a20-olinuxino-lime2-barebones-raw-image))

(define a20-olinuxino-lime2-barebones-os
  (operating-system
    (host-name "olimex")
    (timezone "Europe/Paris")
    (locale "en_GB.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-a20-olinuxino-lime2-bootloader)
                 (targets '("/dev/mmcblk0"))))
    (initrd-modules '())
    (kernel linux-libre-arm-generic)
    (kernel-arguments '("console=tty1"))
    (file-systems (cons (file-system
                          (device (file-system-label "root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))))

(define a20-olinuxino-lime2-image-type
  (image-type
   (name 'a20-olinuxino-lime2-raw)
   (constructor (lambda (os)
                  (image
                   (inherit (raw-with-offset-disk-image (* 8192 512)))
                   (operating-system os)
                   (platform armv7-linux))))))

(define a20-olinuxino-lime2-barebones-raw-image
  (image
   (inherit
    (os+platform->image a20-olinuxino-lime2-barebones-os armv7-linux
                        #:type a20-olinuxino-lime2-image-type))
   (name 'a20-olinuxino-lime2-barebones-raw-image)))

;; Return the default image.
a20-olinuxino-lime2-barebones-raw-image
