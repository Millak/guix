;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundationdevices.com>
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

(define-module (guix platforms xtensa)
  #:use-module (guix platform)
  #:use-module (guix records)
  #:export (xtensa-ath9k-elf))

(define xtensa-ath9k-elf
  (platform
   (target "xtensa-ath9k-elf")
   (system #f)
   (glibc-dynamic-linker #f)))
