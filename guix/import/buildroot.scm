;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
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

(define-module (guix import buildroot)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 receive)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (web uri)
  #:use-module (guix build-system)
  #:use-module (guix build-system ocaml)
  #:use-module (guix http-client)
  #:use-module (guix git)
  #:use-module (guix ui)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (guix import utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (buildroot->guix-package))

(define download
  (memoize
   (lambda* (url #:optional git)
     (with-store store
       (if git
           (latest-repository-commit store url)
           (download-to-store store url))))))

(define (decomment line)
  (match (string-split line #\#)
   ((a b ...)
    (string-trim a))))

(define (read-configuration-file port)
  (filter-map
       (lambda (line)
         (match (string-split (decomment line) #\=)
          (("") ; comment etc
           #f)
          ((key value-string)
           (cons key
           (cond
            ((string-prefix? "\"" value-string)
             (substring value-string 1 (- (string-length value-string) 1))) ; FIXME: Be more correct.
            ((string-prefix? "0" value-string)
             (string->number value-string))
            ((string-prefix? "1" value-string)
             (string->number value-string))
            ((string-prefix? "2" value-string)
             (string->number value-string))
            ((string-prefix? "3" value-string)
             (string->number value-string))
            ((string-prefix? "4" value-string)
             (string->number value-string))
            ((string-prefix? "5" value-string)
             (string->number value-string))
            ((string-prefix? "6" value-string)
             (string->number value-string))
            ((string-prefix? "7" value-string)
             (string->number value-string))
            ((string-prefix? "8" value-string)
             (string->number value-string))
            ((string-prefix? "9" value-string)
             (string->number value-string))
            (else
             (string->symbol value-string)))))))
       (read-lines port)))

(define (buildroot->guix-package name)
  (let* ((buildroot-root-directory
          (download "git://git.buildroot.net/buildroot" #t))
         (buildroot-configuration-directory
          (string-append buildroot-root-directory "/configs"))
         (buildroot-configuration-name
          (string-append buildroot-configuration-directory "/" name))
         (buildroot-configuration
          (if (file-exists? buildroot-configuration-name)
              (call-with-input-file buildroot-configuration-name read-configuration-file)
              (begin
                (display "Unknown buildroot configuration.  Possible values are: " (current-error-port))
                (display (scandir buildroot-configuration-directory) (current-error-port))
                (newline (current-error-port))
                #f)))
         (buildroot-board-directory
          (string-append buildroot-root-directory "/board")))
    (and buildroot-configuration (assoc-ref buildroot-configuration "BR2_TARGET_UBOOT_BOARD_DEFCONFIG")
         (begin
         ;(write buildroot-configuration)
         ;(newline)
          ; BR2_TARGET_UBOOT=y
          ; BR2_TARGET_UBOOT_BOARD_DEFCONFIG="orangepi_zero"
          ; BR2_TARGET_UBOOT_NEEDS_DTC=y
          ; BR2_TARGET_UBOOT_FORMAT_CUSTOM=y
          ; BR2_TARGET_UBOOT_FORMAT_CUSTOM_NAME="u-boot-sunxi-with-spl.bin"
          ; BR2_TARGET_UBOOT_BOOT_SCRIPT=y
          ; BR2_TARGET_UBOOT_BOOT_SCRIPT_SOURCE="board/orangepi/orangepi-zero/boot.cmd"
          ; [BR2_PACKAGE_HOST_DOSFSTOOLS=y => install-buildroot-u-boot requires "dosfstools" package in profile]
          ; BR2_PACKAGE_HOST_GENIMAGE=y => install-buildroot-u-boot requires "genimage" package in profile"
          ; [BR2_PACKAGE_HOST_MTOOLS=y => install-buildroot-u-boot requires "mtools" package in profile]
          ; [BR2_PACKAGE_HOST_UBOOT_TOOLS=y => install-buildroot-u-boot requires "u-boot-tools" package in profile]
          ; BR2_ROOTFS_POST_IMAGE_SCRIPT
          ; BR2_ROOTFS_POST_SCRIPT_ARGS
          ; BR2_TARGET_ROOTFS_EXT2
          ; BR2_TARGET_ROOTFS_EXT2_4
          ; BR2_TARGET_ROOTFS_EXT2_SIZE
          (values
           `((bootloader
             (inherit u-boot-bootloader)
             (package
              (make-u-boot-package
               ,(string-append (assoc-ref buildroot-configuration "BR2_TARGET_UBOOT_BOARD_DEFCONFIG")
                               "_defconfig")
               (if (eq? (assoc-ref "BR2_aarch64" 'y)
                        "aarch64-linux-gnu"
                        "arm-linux-gnueabihf"))))
             (installer install-buildroot-u-boot))))))))
