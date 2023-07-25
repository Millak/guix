;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu system images visionfive2)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (guix platforms riscv)
  #:use-module (srfi srfi-26)
  #:export (visionfive2-barebones-os
            visionfive2-image-type
            visionfive2-barebones-raw-image))

(define visionfive2-barebones-os
  (operating-system
    (host-name "visionfive2")
    (timezone "Asia/Jerusalem")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-starfive-visionfive2-bootloader)
                 (targets '("/dev/vda"))))
    (initrd-modules '())
    ;(kernel linux-libre-riscv64-generic)
    (kernel ((@@ (gnu packages linux) make-linux-libre*)
             linux-libre-6.4-version
             linux-libre-6.4-gnu-revision
             linux-libre-6.4-source
             '("riscv64-linux")
             #:extra-version "riscv64-generic"))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (services
      (append (list (service agetty-service-type
                             (agetty-configuration
                              (extra-options '("-L")) ; no carrier detect
                              (baud-rate "115200")
                              (term "vt100")
                              (tty "ttySIF0")))
                    (service dhcp-client-service-type))
              %base-services))))

(define visionfive2-disk-image
  (image-without-os
   (format 'disk-image)
   (partition-table-type 'gpt)
   ;; https://source.denx.de/u-boot/u-boot/-/blob/master/doc/board/starfive/visionfive2.rst
   ;; The boot process goes:
   ;; * zero-stage bootloader (ZSBL) in ROM and first-stage bootloader (FSBL)
   ;;   together perform the SoC initialization and hand off to the next stage.
   ;; * BBL (Berkeley bootloader) takes the device-tree and brings up the rest
   ;;   of the board, then boots the desired operating system.
   (partitions (list
                (partition
                 ;; Recommended UUID for the first-stage bootloader (FSBL).
                 (size (* 2 (expt 2 20)))
                 (label "spl")
                 (offset (* 4096 512))
                 (file-system "unformatted")
                 (uuid (uuid "2E54B353-1271-4842-806F-E436D6AF6985")))
                (partition
                 ;; Specific offset for compatibility with the defaults for
                 ;; u-boot SPL.  Recommended UUID for the BBL (Berkeley bootloader).
                 (size (* 4 (expt 2 20)))
                 (label "uboot")
                 (offset (* 8192 512))
                 (file-system "unformatted")
                 (uuid (uuid "BC13C2FF-59E6-4262-A352-B275FD6F7172")))
                root-partition))))

(define visionfive2-image-type
  (image-type
   (name 'visionfive2-raw)
   (constructor (cut image-with-os visionfive2-disk-image <>))))

(define visionfive2-barebones-raw-image
  (image
   (inherit
    (os+platform->image visionfive2-barebones-os riscv64-linux
                        #:type visionfive2-image-type))
   (name 'visionfive2-barebones-raw-image)))

;; Return the default image.
visionfive2-barebones-raw-image
