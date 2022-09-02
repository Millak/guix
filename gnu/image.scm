;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2022 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (gnu image)
  #:use-module (guix platform)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (partition
            partition?
            partition-device
            partition-size
            partition-offset
            partition-file-system
            partition-file-system-options
            partition-label
            partition-uuid
            partition-flags
            partition-initializer

            image
            image?
            image-name
            image-format
            image-platform
            image-size
            image-operating-system
            image-partition-table-type
            image-partitions
            image-compression?
            image-volatile-root?
            image-shared-store?
            image-shared-network?
            image-substitutable?

            image-type
            image-type?
            image-type-name
            image-type-constructor

            os->image
            os+platform->image))


;;;
;;; Sanitizers.
;;;

;; Image and partition sizes can be either be a size in bytes or the 'guess
;; symbol denoting that the size should be estimated by Guix, according to the
;; image content.
(define-with-syntax-properties (validate-size (value properties))
  (unless (and value
               (or (eq? value 'guess) (integer? value)))
    (raise
       (make-compound-condition
        (condition
         (&error-location
          (location (source-properties->location properties))))
        (formatted-message
         (G_ "size (~a) can only be 'guess or a numeric expression ~%")
         value 'field))))
  value)


;;;
;;; Partition record.
;;;

;; The partition offset should be a bytes count as an integer.
(define-with-syntax-properties (validate-partition-offset (value properties))
  (unless (and value (integer? value))
    (raise
       (make-compound-condition
        (condition
         (&error-location
          (location (source-properties->location properties))))
        (formatted-message
         (G_ "the partition offset (~a) can only be a \
numeric expression ~%") value 'field))))
  value)

;; The supported partition flags.
(define-with-syntax-properties (validate-partition-flags (value properties))
  (let ((bad-flags (lset-difference eq? value '(boot esp))))
    (unless (and (list? value) (null? bad-flags))
      (raise
       (make-compound-condition
        (condition
         (&error-location
          (location (source-properties->location properties))))
        (formatted-message
         (G_ "unsupported partition flag(s): ~a ~%") bad-flags)))))
  value)

(define-record-type* <partition> partition make-partition
  partition?
  (size                 partition-size   ;size in bytes as integer or 'guess
                        (default 'guess)
                        (sanitize validate-size))
  (offset               partition-offset
                        (default 0)   ;offset in bytes as integer
                        (sanitize validate-partition-offset))
  (file-system          partition-file-system
                        (default "ext4"))  ;string
  (file-system-options  partition-file-system-options
                        (default '()))  ;list of strings
  (label                partition-label)  ;string
  (uuid                 partition-uuid
                        (default #false))  ;<uuid>
  (flags                partition-flags
                        (default '())  ;list of symbols
                        (sanitize validate-partition-flags))
  (initializer          partition-initializer
                        (default #false))) ;gexp | #false


;;;
;;; Image record.
;;;

(define-syntax-rule (define-set-sanitizer name field set)
  "Define NAME as a procedure or macro that raises an error if passed a value
that is not in SET, mentioning FIELD in the error message."
  (define-with-syntax-properties (name (value properties))
    (unless (memq value 'set)
      (raise
       (make-compound-condition
        (condition
         (&error-location
          (location (source-properties->location properties))))
        (formatted-message (G_ "~s: invalid '~a' value") value 'field))))
    value))

;; The supported image formats.
(define-set-sanitizer validate-image-format format
  (disk-image compressed-qcow2 docker iso9660))

;; The supported partition table types.
(define-set-sanitizer validate-partition-table-type partition-table-type
  (mbr gpt))

(define-record-type* <image>
  image make-image
  image?
  (name               image-name ;symbol
                      (default #false))
  (format             image-format                ;symbol
                      (sanitize validate-image-format))
  (platform           image-platform ;<platform>
                      (default #false))
  (size               image-size  ;size in bytes as integer
                      (default 'guess)
                      (sanitize validate-size))
  (operating-system   image-operating-system  ;<operating-system>
                      (default #f))
  (partition-table-type image-partition-table-type ; 'mbr or 'gpt
                      (default 'mbr)
                      (sanitize validate-partition-table-type))
  (partitions         image-partitions ;list of <partition>
                      (default '()))
  (compression?       image-compression? ;boolean
                      (default #true))
  (volatile-root?     image-volatile-root? ;boolean
                      (default #true))
  (shared-store?      image-shared-store? ;boolean
                      (default #false))
  (shared-network?    image-shared-network? ;boolean
                      (default #false))
  (substitutable?     image-substitutable? ;boolean
                      (default #true)))


;;;
;;; Image type.
;;;

;; The role of this record is to provide a constructor that is able to turn an
;; <operating-system> record into an <image> record.  Some basic <image-type>
;; records are defined in the (gnu system image) module.  They are able to
;; turn an <operating-system> record into an EFI or an ISO 9660 bootable
;; image, a Docker image or even a QCOW2 image.
;;
;; Other <image-type> records are defined in the (gnu system images ...)
;; modules.  They are dedicated to specific machines such as Novena and Pine64
;; SoC boards that require specific images.
;;
;; All the available <image-type> records are collected by the 'image-modules'
;; procedure.  This allows the "guix system image" command to turn a given
;; <operating-system> record into an image, thanks to the specified
;; <image-type>.  In that case, the <image-type> look up is done using the
;; name field of the <image-type> record.

(define-record-type* <image-type>
  image-type make-image-type
  image-type?
  (name           image-type-name) ;symbol
  (constructor    image-type-constructor)) ;<operating-system> -> <image>


;;;
;;; Image creation.
;;;

(define* (os->image os #:key type)
  "Use the image constructor from TYPE, an <image-type> record to turn the
given OS, an <operating-system> record into an image and return it."
  (let ((constructor (image-type-constructor type)))
    (constructor os)))

(define* (os+platform->image os platform #:key type)
  "Use the image constructor from TYPE, an <image-type> record to turn the
given OS, an <operating-system> record into an image targeting PLATFORM, a
<platform> record and return it."
  (image
   (inherit (os->image os #:type type))
   (platform platform)))
