;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024-2025 Zheng Junjie <z572@z572.online>
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
  #:use-module (gnu services dbus)
  #:use-module (gnu services dns)
  #:use-module (gnu services avahi)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)

  #:use-module (gnu packages ssh)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system linux-initrd)
  #:use-module (guix platforms riscv)
  #:use-module (srfi srfi-26)
  #:export (visionfive2-barebones-os
            visionfive2-image-type
            visionfive2-barebones-raw-image))

;;; Commentary:
;;;
;;; VisionFive2 can boot from MMC1 (SPI flash) or MMC2 (SD card) selected
;;; by DIP switches MSEL[1:0], you may want boot from MMC2 to use the
;;; U-Boot from Guix System instead of the vendor U-Boot in MMC1.  Before
;;; doing so, make sure you have a correct 'fdtfile' in the environment:
;;;
;;; uboot> setenv fdtfile starfive/jh7110-starfive-visionfive-2-v1.3b.dtb
;;; uboot> saveenv
;;;
;;; Code:

(define visionfive2-barebones-os
  (operating-system
    (host-name "visionfive2")
    (timezone "Etc/UTC")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-starfive-visionfive2-bootloader)
                 (targets '("/dev/mmcblk0"))))
    (file-systems (cons (file-system
                          (device (file-system-label "Guix_image"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (kernel-arguments (list "earlycon" "clk_ignore_unused"))
    (initrd-modules
     (append (list "mmc_block"
                   "clk-starfive-jh7110-aon"
                   "clk-starfive-jh7110-stg"
                   "phy-jh7110-dphy-tx"
                   "pcie_starfive"
                   "nvme")
             %base-initrd-modules))
    (firmware '())
    (packages (append (list cloud-utils neofetch) %base-packages))
    (services
     (append (list (service openssh-service-type
                            (openssh-configuration
                             (openssh openssh-sans-x)
                             (permit-root-login #t)
                             (allow-empty-passwords? #t)))
                   (service agetty-service-type
                            (agetty-configuration
                             (extra-options '("-L"))
                             (baud-rate "115200")
                             (term "vt100")
                             (tty "ttyS0")))
                   (service dhcp-client-service-type))
             %base-services))))

(define visionfive2-disk-image
  (image-without-os
   (format 'disk-image)
   (partition-table-type 'gpt)
   (partitions (list
                (partition
                 (size (* 1 (expt 2 20)))
                 (label "spl")
                 (offset (* 34 512))
                 (file-system "unformatted")
                 (uuid (uuid "2E54B353-1271-4842-806F-E436D6AF6985")))
                (partition
                 (size (* 4 (expt 2 20)))
                 (label "uboot")
                 (offset (* 2082 512))
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
