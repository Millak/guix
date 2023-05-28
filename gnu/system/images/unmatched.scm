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

(define-module (gnu system images unmatched)
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
  #:export (unmatched-barebones-os
            unmatched-image-type
            unmatched-barebones-raw-image))

(define unmatched-barebones-os
  (operating-system
    (host-name "unmatched")
    (timezone "Asia/Jerusalem")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-sifive-unmatched-bootloader)
                 (targets '("/dev/vda"))))
    (initrd-modules '())
    (kernel linux-libre-riscv64-generic)
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

(define unmatched-disk-image
  (image-without-os
   (format 'disk-image)
   (partition-table-type 'gpt)
   ;; https://source.denx.de/u-boot/u-boot/-/blob/master/doc/board/sifive/unmatched.rst
   ;; The boot process goes:
   ;; * zero-stage bootloader (ZSBL) in ROM and first-stage bootloader (FSBL)
   ;;   together perform the SoC initialization and hand off to the next stage.
   ;; * BBL (Berkeley bootloader) takes the device-tree and brings up the rest
   ;;   of the board, then boots the desired operating system.
   (partitions (list
                (partition
                 ;; Special UUID for HiFive first-stage bootloader (FSBL).
                 (size (* 1 (expt 2 20)))
                 (label "spl")
                 (offset (* 34 512))
                 (file-system "unformatted")
                 (uuid (uuid "5b193300-fc78-40cd-8002-e86c45580b47")))
                (partition
                 ;; Specific offset for compatibility with the defaults for
                 ;; u-boot SPL.  Special UUID for HiFive BBL (Berkeley bootloader).
                 (size (* 4 (expt 2 20)))
                 (label "uboot")
                 (offset (* 2082 512))
                 (file-system "unformatted")
                 (uuid (uuid "2e54b353-1271-4842-806f-e436d6af6985")))
                root-partition))))

(define unmatched-image-type
  (image-type
   (name 'unmatched-raw)
   (constructor (cut image-with-os unmatched-disk-image <>))))

(define unmatched-barebones-raw-image
  (image
   (inherit
    (os+platform->image unmatched-barebones-os riscv64-linux
                        #:type unmatched-image-type))
   (name 'unmatched-barebones-raw-image)))

;; Return the default image.
unmatched-barebones-raw-image
