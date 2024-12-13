;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023-2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu bootloader u-boot)
  #:use-module (gnu bootloader extlinux)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages bootloaders)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (u-boot-bootloader
            u-boot-a20-olinuxino-lime-bootloader
            u-boot-a20-olinuxino-lime2-bootloader
            u-boot-a20-olinuxino-micro-bootloader
            u-boot-bananapi-m2-ultra-bootloader
            u-boot-beaglebone-black-bootloader
            u-boot-cubietruck-bootloader
            u-boot-firefly-rk3399-bootloader
            u-boot-mx6cuboxi-bootloader
            u-boot-nintendo-nes-classic-edition-bootloader
            u-boot-novena-bootloader
            u-boot-orangepi-r1-plus-lts-rk3328-bootloader
            u-boot-orangepi-zero2w-bootloader
            u-boot-pine64-plus-bootloader
            u-boot-pine64-lts-bootloader
            u-boot-pinebook-bootloader
            u-boot-pinebook-pro-rk3399-bootloader
            u-boot-puma-rk3399-bootloader
            u-boot-rock64-rk3328-bootloader
            u-boot-rockpro64-rk3399-bootloader
            u-boot-sifive-unmatched-bootloader
            u-boot-qemu-riscv64-bootloader
            u-boot-starfive-visionfive2-bootloader
            u-boot-ts7970-q-2g-1000mhz-c-bootloader
            u-boot-wandboard-bootloader))

(define (make-u-boot-installer file)
  (let ((file
          (match file
            ((? string?)
             (list #~(install-file (string-append bootloader #$file)
                                   install-dir)))
            ((? file-like?) (list #~(install-file #$file install-dir)))
            (#f '()))))
    #~(lambda (bootloader device mount-point)
        (let ((install-dir (string-append mount-point "/boot")))
          #$@file))))

(define (write-u-boot-image files block-size)
  "FILES is a list of (FILE COUNT OFFSET) tuples.  Each FILE is written
to the target image at BLOCK-SIZE * OFFSET.  The number of bytes written
is BLOCK-SIZE * COUNT, or FILE size if COUNT is not given."
  (define (write-file-to-image file)
    (match file
      ((file count ... offset)
       (let* ((file #~(string-append bootloader "/libexec/" #$file))
              (size (match count
                      (() #~(stat:size (stat #$file)))
                      ((count) (* count block-size)))))
         #~(write-file-on-device #$file #$size image
                                 #$(* offset block-size))))))
  #~(lambda (bootloader _ image)
      #$@(map write-file-to-image files)))

(define install-u-boot
  #~(lambda (bootloader root-index image)
      (if bootloader
        (error "Failed to install U-Boot"))))

(define install-beaglebone-black-u-boot
  ;; http://wiki.beyondlogic.org/index.php?title=BeagleBoneBlack_Upgrading_uBoot
  ;; This first stage bootloader called MLO (U-Boot SPL) is expected at
  ;; 0x20000 by BBB ROM code. The second stage bootloader will be loaded by
  ;; the MLO and is expected at 0x60000.  Write both first stage ("MLO") and
  ;; second stage ("u-boot.img") images, read in BOOTLOADER directory, to the
  ;; specified DEVICE.
  (write-u-boot-image '(("MLO" 256 256) ("u-boot.img" 768 1024)) 512))

(define install-allwinner-u-boot
  (write-u-boot-image '(("u-boot-sunxi-with-spl.bin" 8)) 1024))

(define install-allwinner64-u-boot
  (write-u-boot-image '(("u-boot-sunxi-with-spl.bin" 8)
                        ("u-boot-sunxi-with-spl.fit.itb" 40))
                      1024))

(define install-imx-u-boot
  (write-u-boot-image '(("SPL" 1) ("u-boot.img" 69)) 1024))

(define install-puma-rk3399-u-boot
  (write-u-boot-image '(("idbloader.img" 64) ("u-boot.itb" 512)) 512))

(define install-rockchip-u-boot
  (write-u-boot-image '(("idbloader.img" 64) ("u-boot.itb" 16384)) 512))

(define install-sifive-unmatched-u-boot
  (write-u-boot-image '(("spl/u-boot-spl.bin" 34) ("u-boot.itb" 2082))
                      512))

(define install-starfive-visionfive2-u-boot
  (write-u-boot-image '(("spl/u-boot-spl.bin.normal.out" 34)
                        ("u-boot.itb" 2082))
                      512))

(define install-starfive-visionfive2-uEnv.txt
  (make-u-boot-installer
    ;; If the board SPI uses the vendor's U-Boot, it will find starfive/
    ;; starfive_visionfive2.dtb.  We cannot guarantee that users will
    ;; update this U-Boot, so set the FDT explicitly.
    (plain-file "uEnv.txt"
      "fdtfile=starfive/jh7110-starfive-visionfive-2-v1.3b.dtb~%")))


;;;
;;; Bootloader definitions.
;;;

(define u-boot-bootloader
  (bootloader
   (inherit extlinux-bootloader)
   (name 'u-boot)
   (package #f)
   (installer #f)
   (disk-image-installer install-u-boot)))

(define u-boot-beaglebone-black-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-am335x-boneblack)
   (disk-image-installer install-beaglebone-black-u-boot)))

(define u-boot-allwinner-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (disk-image-installer install-allwinner-u-boot)))

(define u-boot-allwinner64-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (disk-image-installer install-allwinner64-u-boot)))

(define u-boot-imx-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (disk-image-installer install-imx-u-boot)))

(define u-boot-rockchip-bootloader
  ;; SD and eMMC use the same format
  (bootloader
   (inherit u-boot-bootloader)
   (disk-image-installer install-rockchip-u-boot)))

(define u-boot-nintendo-nes-classic-edition-bootloader
  (bootloader
    (inherit u-boot-allwinner-bootloader)
    (package u-boot-nintendo-nes-classic-edition)))

(define u-boot-a20-olinuxino-lime-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-a20-olinuxino-lime)))

(define u-boot-a20-olinuxino-lime2-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-a20-olinuxino-lime2)))

(define u-boot-a20-olinuxino-micro-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-a20-olinuxino-micro)))

(define u-boot-bananapi-m2-ultra-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-bananapi-m2-ultra)))

(define u-boot-cubietruck-bootloader
  (bootloader
    (inherit u-boot-allwinner-bootloader)
    (package u-boot-cubietruck)))

(define u-boot-firefly-rk3399-bootloader
  (bootloader
   (inherit u-boot-rockchip-bootloader)
   (package u-boot-firefly-rk3399)))

(define u-boot-mx6cuboxi-bootloader
  (bootloader
   (inherit u-boot-imx-bootloader)
   (package u-boot-mx6cuboxi)))

(define u-boot-wandboard-bootloader
  (bootloader
   (inherit u-boot-imx-bootloader)
   (package u-boot-wandboard)))

(define u-boot-novena-bootloader
  (bootloader
   (inherit u-boot-imx-bootloader)
   (package u-boot-novena)))

(define u-boot-orangepi-r1-plus-lts-rk3328-bootloader
  (bootloader
   (inherit u-boot-rockchip-bootloader)
   (package u-boot-orangepi-r1-plus-lts-rk3328)))

(define u-boot-orangepi-zero2w-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-orangepi-zero2w)))

(define u-boot-pine64-plus-bootloader
  (bootloader
   (inherit u-boot-allwinner64-bootloader)
   (package u-boot-pine64-plus)))

(define u-boot-pine64-lts-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-pine64-lts)))

(define u-boot-pinebook-bootloader
  (bootloader
   (inherit u-boot-allwinner64-bootloader)
   (package u-boot-pinebook)))

(define u-boot-puma-rk3399-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-puma-rk3399)
   (disk-image-installer install-puma-rk3399-u-boot)))

(define u-boot-rock64-rk3328-bootloader
  (bootloader
   (inherit u-boot-rockchip-bootloader)
   (package u-boot-rock64-rk3328)))

(define u-boot-rockpro64-rk3399-bootloader
  (bootloader
   (inherit u-boot-rockchip-bootloader)
   (package u-boot-rockpro64-rk3399)))

(define u-boot-pinebook-pro-rk3399-bootloader
  (bootloader
   (inherit u-boot-rockchip-bootloader)
   (package u-boot-pinebook-pro-rk3399)))

(define u-boot-ts7970-q-2g-1000mhz-c-bootloader
  ;; This bootloader doesn't really need to be installed, as it is read from
  ;; an SPI memory chip, not the SD card.  It is copied to /boot/u-boot.imx
  ;; for convenience and should be manually flashed at the U-Boot prompt.
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-ts7970-q-2g-1000mhz-c)
   (installer (make-u-boot-installer "libexec/u-boot.imx"))
   (disk-image-installer #f)))

(define u-boot-sifive-unmatched-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-sifive-unmatched)
   (disk-image-installer install-sifive-unmatched-u-boot)))

(define u-boot-starfive-visionfive2-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-starfive-visionfive2)
   (installer install-starfive-visionfive2-uEnv.txt)
   (disk-image-installer install-starfive-visionfive2-u-boot)))

(define u-boot-qemu-riscv64-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-qemu-riscv64)
   (installer (make-u-boot-installer "libexec/u-boot.bin"))
   (disk-image-installer #f)))
