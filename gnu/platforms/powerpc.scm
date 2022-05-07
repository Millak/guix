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

(define-module (gnu platforms powerpc)
  #:use-module (gnu platform)
  #:use-module (gnu packages linux)
  #:use-module (guix records)
  #:export (powerpc-linux
            powerpc64le-linux))

(define powerpc-linux
  (platform
   (target "powerpc-linux-gnu")
   (system "powerpc-linux")
   (linux-architecture "powerpc")
   (glibc-dynamic-linker "/lib/ld.so.1")))

(define powerpc64le-linux
  (platform
   (target "powerpc64le-linux-gnu")
   (system "powerpc64le-linux")
   (linux-architecture "powerpc")
   (glibc-dynamic-linker "/lib/ld64.so.2")))
