;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2025 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages data-language)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages plotutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml))

(define-public gnudatalanguage
  (package
    (name "gnudatalanguage")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/gnudatalanguage/gdl/releases/download/v"
             version "/gdl-v" version ".tar.gz"))

       (sha256
        (base32
         "0qln7851dpfy78yyx5k01ch2yrik3k1a1a2727rvpqf5rnmx6kkl"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config python python-numpy))
    (inputs
     (list eccodes
           eigen
           expat
           fftw
           fftwf
           glpk
           graphicsmagick
           gsl
           hdf5
           libgeotiff
           libjpeg-turbo
           libpng
           libtiff
           libtirpc
           ncurses
           netcdf
           plplot
           proj
           qhull
           readline
           shapelib
           udunits
           wxwidgets
           zlib))
    (arguments
      (list
        #:configure-flags #~(list "-DHDF=OFF"))) ; keep default "-DHDF5=ON"
    (home-page "https://gnudatalanguage.github.io/")
    (synopsis "Compiler for GDL, an IDL-compatible programming language")
    (description
     "GDL (GNU Data Language) is an incremental compiler compatible with
IDL (Interactive Data Language) and to some extent with PV-WAVE.  Together
with its library routines it serves as a tool for data analysis and
visualization in such disciplines as astronomy, geosciences and medical
imaging.")
    (license license:gpl2+)))
