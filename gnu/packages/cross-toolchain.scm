;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
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

(define-module (gnu packages cross-toolchain)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages gcc)
  #:use-module (guix packages))

;;; Commentary:
;;;
;;; This module provides packages for cross compilation toolchains.  These
;;; packages must not be used at the top level to avoid cyclic module
;;; dependencies caused by the (gnu packages cross-base) module referring to
;;; to top level bindings from (gnu packages gcc).
;;;
;;; The real purpose of these packages is for installation on profiles by users
;;; and other packages should make use of the toolchain through the usual cross
;;; compilation methods. For example, by using the `#:target' argument on
;;; packages or `--target' on the command line.

(define-public gcc-cross-avr-toolchain
  (cross-gcc-toolchain "avr"))

(define-public gcc-cross-i686-w64-mingw32-toolchain
  (cross-gcc-toolchain "i686-w64-mingw32"))

(define-public gcc-cross-or1k-elf-toolchain
  (cross-gcc-toolchain "or1k-elf"))

(define-public gcc-cross-x86_64-w64-mingw32-toolchain-13
  (cross-gcc-toolchain "x86_64-w64-mingw32"
                       #:base-gcc gcc-13))

(define-public gcc-cross-x86_64-w64-mingw32-toolchain
  (cross-gcc-toolchain "x86_64-w64-mingw32"))
