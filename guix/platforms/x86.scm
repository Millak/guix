;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix platforms x86)
  #:use-module (guix platform)
  #:use-module (guix records)
  #:export (i686-linux
            x86_64-linux
            x86_64-linux-x32
            i686-mingw
            x86_64-mingw
            i586-gnu))

(define i686-linux
  (platform
   (target "i686-linux-gnu")
   (system "i686-linux")
   (linux-architecture "i386")
   (rust-target "i686-unknown-linux-gnu")
   (glibc-dynamic-linker "/lib/ld-linux.so.2")))

(define x86_64-linux
  (platform
   (target "x86_64-linux-gnu")
   (system "x86_64-linux")
   (linux-architecture "x86_64")
   (rust-target "x86_64-unknown-linux-gnu")
   (glibc-dynamic-linker "/lib/ld-linux-x86-64.so.2")))

(define x86_64-linux-x32
  (platform
   (target "x86_64-linux-gnux32")
   (system #f)
   (linux-architecture "x86_64")
   (rust-target "x86_64-unknown-linux-gnux32")
   (glibc-dynamic-linker "/lib/ld-linux-x32.so.2")))

(define i686-mingw
  (platform
   (target "i686-w64-mingw32")
   (system #f)
   (rust-target "i686-pc-windows-gnu")
   (glibc-dynamic-linker #f)))

(define x86_64-mingw
  (platform
   (target "x86_64-w64-mingw32")
   (system #f)
   (rust-target "x86_64-pc-windows-gnu")
   (glibc-dynamic-linker #f)))

(define i586-gnu
  (platform
   (target "i586-pc-gnu")
   (system "i586-gnu")
   (rust-target "i686-unknown-hurd-gnu")
   (glibc-dynamic-linker "/lib/ld.so.1")))
