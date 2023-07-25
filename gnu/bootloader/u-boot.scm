;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu bootloader u-boot)
  #:use-module (gnu bootloader extlinux)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages bootloaders)
  #:use-module (guix gexp)
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
            u-boot-pine64-plus-bootloader
            u-boot-pine64-lts-bootloader
            u-boot-pinebook-bootloader
            u-boot-pinebook-pro-rk3399-bootloader
            u-boot-puma-rk3399-bootloader
            u-boot-rock64-rk3328-bootloader
            u-boot-rockpro64-rk3399-bootloader
            u-boot-sifive-unmatched-bootloader
            u-boot-starfive-visionfive2-bootloader
            u-boot-ts7970-q-2g-1000mhz-c-bootloader
            u-boot-wandboard-bootloader))

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
  #~(lambda (bootloader root-index image)
      (let ((mlo (string-append bootloader "/libexec/MLO"))
            (u-boot (string-append bootloader "/libexec/u-boot.img")))
        (write-file-on-device mlo (* 256 512)
                              image (* 256 512))
        (write-file-on-device u-boot (* 1024 512)
                              image (* 768 512)))))

(define install-allwinner-u-boot
  #~(lambda (bootloader root-index image)
      (let ((u-boot (string-append bootloader
                                   "/libexec/u-boot-sunxi-with-spl.bin")))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 8 1024)))))

(define install-allwinner64-u-boot
  #~(lambda (bootloader root-index image)
      (let ((spl (string-append bootloader "/libexec/u-boot-sunxi-with-spl.bin"))
            (u-boot (string-append bootloader "/libexec/u-boot-sunxi-with-spl.fit.itb")))
        (write-file-on-device spl (stat:size (stat spl))
                              image (* 8 1024))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 40 1024)))))

(define install-imx-u-boot
  #~(lambda (bootloader root-index image)
      (let ((spl (string-append bootloader "/libexec/SPL"))
            (u-boot (string-append bootloader "/libexec/u-boot.img")))
        (write-file-on-device spl (stat:size (stat spl))
                              image (* 1 1024))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 69 1024)))))

(define install-puma-rk3399-u-boot
  #~(lambda (bootloader root-index image)
      (let ((spl (string-append bootloader "/libexec/idbloader.img"))
            (u-boot (string-append bootloader "/libexec/u-boot.itb")))
        (write-file-on-device spl (stat:size (stat spl))
                              image (* 64 512))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 512 512)))))

(define install-firefly-rk3399-u-boot
  #~(lambda (bootloader root-index image)
      (let ((idb (string-append bootloader "/libexec/idbloader.img"))
            (u-boot (string-append bootloader "/libexec/u-boot.itb")))
        (write-file-on-device idb (stat:size (stat idb))
                              image (* 64 512))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 16384 512)))))

(define install-rock64-rk3328-u-boot
  #~(lambda (bootloader root-index image)
      (let ((idb (string-append bootloader "/libexec/idbloader.img"))
            (u-boot (string-append bootloader "/libexec/u-boot.itb")))
        (write-file-on-device idb (stat:size (stat idb))
                              image (* 64 512))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 16384 512)))))

(define install-rockpro64-rk3399-u-boot
  #~(lambda (bootloader root-index image)
      (let ((idb (string-append bootloader "/libexec/idbloader.img"))
            (u-boot (string-append bootloader "/libexec/u-boot.itb")))
        (write-file-on-device idb (stat:size (stat idb))
                              image (* 64 512))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 16384 512)))))

(define install-pinebook-pro-rk3399-u-boot install-rockpro64-rk3399-u-boot)

(define install-u-boot-ts7970-q-2g-1000mhz-c-u-boot
  #~(lambda (bootloader device mount-point)
      (let ((u-boot.imx (string-append bootloader "/libexec/u-boot.imx"))
            (install-dir (string-append mount-point "/boot")))
        (install-file u-boot.imx install-dir))))

(define install-sifive-unmatched-u-boot
  #~(lambda (bootloader root-index image)
      (let ((spl (string-append bootloader "/libexec/spl/u-boot-spl.bin"))
            (u-boot (string-append bootloader "/libexec/u-boot.itb")))
        (write-file-on-device spl (stat:size (stat spl))
                              image (* 34 512))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 2082 512)))))

(define install-starfive-visionfive2-u-boot
  #~(lambda (bootloader root-index image)
      (let ((spl (string-append bootloader "/libexec/spl/u-boot-spl.bin.normal.out"))
            (u-boot (string-append bootloader "/libexec/u-boot.itb")))
        (write-file-on-device spl (stat:size (stat spl))
                              image (* 4096 512))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 8192 512)))))



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
  ;; SD and eMMC use the same format
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-firefly-rk3399)
   (disk-image-installer install-firefly-rk3399-u-boot)))

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
  ;; SD and eMMC use the same format
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-rock64-rk3328)
   (disk-image-installer install-rock64-rk3328-u-boot)))

(define u-boot-rockpro64-rk3399-bootloader
  ;; SD and eMMC use the same format
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-rockpro64-rk3399)
   (disk-image-installer install-rockpro64-rk3399-u-boot)))

(define u-boot-pinebook-pro-rk3399-bootloader
  ;; SD and eMMC use the same format
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-pinebook-pro-rk3399)
   (disk-image-installer install-pinebook-pro-rk3399-u-boot)))

(define u-boot-ts7970-q-2g-1000mhz-c-bootloader
  ;; This bootloader doesn't really need to be installed, as it is read from
  ;; an SPI memory chip, not the SD card.  It is copied to /boot/u-boot.imx
  ;; for convenience and should be manually flashed at the U-Boot prompt.
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-ts7970-q-2g-1000mhz-c)
   (installer install-u-boot-ts7970-q-2g-1000mhz-c-u-boot)
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
   (disk-image-installer install-starfive-visionfive2-u-boot)))
