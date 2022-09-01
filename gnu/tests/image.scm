;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (gnu tests image)
  #:use-module (gnu)
  #:use-module (gnu image)
  #:use-module (gnu tests)
  #:autoload   (gnu system image) (system-image root-offset)
  #:use-module (gnu system uuid)
  #:use-module (gnu system vm)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (ice-9 format)
  #:export (%test-images))

;;; Commentary:
;;;
;;; This module provides tests for the image creation process that is
;;; performed by "genimage" under the hood.
;;;
;;; The image partitionment is checked using Guile-Parted.  The content of the
;;; images is out of the scope of this module.  Other test modules such as
;;; (gnu tests installation) make sure that the produced images are viable.
;;;
;;; Code:

;; A dummy initializer creating a simple file in the partition.
(define dummy-initializer
  #~(lambda* (root . rest)
      (mkdir root)
      (call-with-output-file
          (string-append root "/test")
        (lambda (port)
          (format port "content")))))

(define %simple-efi-os
  (operating-system
    (inherit %simple-os)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))))))

;; An MBR disk image with a single ext4 partition.
(define i1
  (image
   (format 'disk-image)
   (operating-system %simple-os)
   (partitions
    (list
     (partition
      (size (* 1024 1024)) ;1MiB
      (offset root-offset)
      (label "test")
      (file-system "ext4")
      (flags '(boot))
      (initializer dummy-initializer))))))

;; A GPT disk image with a single ext4 partition.
(define i2
  (image
   (format 'disk-image)
   (operating-system %simple-efi-os)
   (partition-table-type 'gpt)
   (partitions
    (list
     (partition
      (size (* 1024 1024)) ;1MiB
      (offset root-offset)
      (label "test")
      (file-system "ext4")
      (flags '(boot))
      (initializer dummy-initializer))))))

;; An MBR disk image with multiple ext4 partitions.
(define i3
  (image
   (format 'disk-image)
   (operating-system %simple-os)
   (partitions
    (list
     (partition
      (size (* 1024 1024)) ;1MiB
      (offset root-offset)
      (label "test")
      (file-system "ext4")
      (flags '(boot))
      (initializer dummy-initializer))
     (partition
      (size (* 1024 1024)) ;1MiB
      (label "test2")
      (file-system "ext4")
      (flags '())
      (initializer dummy-initializer))))))

;; A GPT disk image with multiple ext4 partitions.
(define i4
  (image
   (format 'disk-image)
   (operating-system %simple-efi-os)
   (partition-table-type 'gpt)
   (partitions
    (list
     (partition
      (size (* 1024 1024)) ;1MiB
      (offset root-offset)
      (label "test")
      (file-system "ext4")
      (flags '(boot))
      (initializer dummy-initializer))
     (partition
      (size (* 1024 1024)) ;1MiB
      (label "test2")
      (file-system "ext4")
      (flags '())
      (initializer dummy-initializer))))))

;; A GPT disk image with fat32 and ext4 partitions.
(define i5
  (image
   (format 'disk-image)
   (operating-system %simple-efi-os)
   (partition-table-type 'gpt)
   (partitions
    (list
     (partition
      (size (* 1024 1024 128)) ;128MiB
      (offset root-offset)
      (label "test")
      (file-system "fat32")
      (flags '(esp))
      (initializer dummy-initializer))
     (partition
      (size (* 1024 1024 256)) ;256MiB
      (label "test2")
      (file-system "ext4")
      (flags '(boot))
      (initializer dummy-initializer))))))

(define (run-images-test)
  (define test
    (with-imported-modules '((srfi srfi-64)
                             (gnu build marionette))
      (with-extensions (list guile-parted guile-bytestructures)
        #~(begin
            (use-modules (gnu build marionette)
                         (srfi srfi-1)
                         (srfi srfi-26)
                         (srfi srfi-64)
                         (parted))

            (define (image->disk img)
              (disk-new (get-device img)))

            (test-runner-current (system-test-runner #$output))
            (test-begin "images")

            ;; Image i1.
            (define i1-image
              #$(system-image i1))
            (define d1-device
              (get-device i1-image))

            (test-equal "msdos"
              (disk-type-name (disk-probe d1-device)))

            (test-equal 1
              (disk-get-primary-partition-count (disk-new d1-device)))

            (test-assert
                (let* ((disk (disk-new d1-device))
                       (partitions (disk-partitions disk))
                       (boot-partition (find normal-partition? partitions)))
                  (partition-get-flag boot-partition PARTITION-FLAG-BOOT)))

            ;; Image i2.
            (define i2-image
              #$(system-image i2))
            (define d2-device
              (get-device i2-image))

            (test-equal "gpt"
              (disk-type-name (disk-probe d2-device)))

            (test-equal 1
              (disk-get-primary-partition-count (disk-new d2-device)))

            (test-equal "test"
                (let* ((disk (disk-new d2-device))
                       (partitions (disk-partitions disk))
                       (boot-partition (find normal-partition? partitions)))
                  (partition-get-name boot-partition)))

            ;; Image i3.
            (define i3-image
              #$(system-image i3))
            (define d3-device
              (get-device i3-image))

            (test-equal "msdos"
              (disk-type-name (disk-probe d3-device)))

            (test-equal 2
              (disk-get-primary-partition-count (disk-new d3-device)))

            ;; Image i4.
            (define i4-image
              #$(system-image i4))
            (define d4-device
              (get-device i4-image))

            (test-equal "gpt"
              (disk-type-name (disk-probe d4-device)))

            (test-equal 2
              (disk-get-primary-partition-count (disk-new d4-device)))

            ;; Image i5.
            (define i5-image
              #$(system-image i5))
            (define d5-device
              (get-device i5-image))

            (define (sector->byte sector)
              (/ (* sector (device-sector-size d5-device))
                 MEBIBYTE-SIZE))

            (test-equal "gpt"
              (disk-type-name (disk-probe d5-device)))

            (test-equal 2
              (disk-get-primary-partition-count (disk-new d5-device)))

            (test-equal '("fat32" "ext4")
              (map (compose filesystem-type-name partition-fs-type)
                   (filter data-partition?
                           (disk-partitions (disk-new d5-device)))))

            ;; The first partition has a 1MiB offset has a 128MiB size. The
            ;; second partition should then start at 129MiB.
            (test-equal '(1 129)
              (map (compose sector->byte partition-start)
                   (filter data-partition?
                           (disk-partitions (disk-new d5-device)))))

            (test-end)))))

  (gexp->derivation "images-test" test))

(define %test-images
  (system-test
   (name "images")
   (description "Build and test basic system images.")
   (value (run-images-test))))
