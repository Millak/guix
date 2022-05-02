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

(define-module (gnu platforms x86)
  #:use-module (gnu platform)
  #:use-module (gnu packages linux)
  #:use-module (guix records)
  #:export (i686-linux
            x86_64-linux
            i686-mingw
            x86_64-mingw))

(define i686-linux
  (platform
   (target "i686-linux-gnu")
   (system "i686-linux")
   (linux-architecture "i386")))

(define x86_64-linux
  (platform
   (target "x86_64-linux-gnu")
   (system "x86_64-linux")
   (linux-architecture "x86_64")))

(define i686-mingw
  (platform
   (target "i686-w64-mingw32")
   (system #f)))

(define x86_64-mingw
  (platform
   (target "x86_64-w64-mingw32")
   (system #f)))

(define hurd
  (platform
   (target "i586-pc-gnu")
   (system "i586-gnu")))
