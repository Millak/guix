;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2025 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2022 Andy Tai <atai@atai.org>
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

(define-module (gnu packages lesstif)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages c)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))


(define-public motif
  ;; This commit is from February 2023 and v2.3.8 from 2017.
  (let ((commit "0f556b0873c72ba1152a12fd54c3198ee039e413")
        (revision "1"))
    (package
      (name "motif")
      (version (git-version "2.3.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.code.sf.net/p/motif/code")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1g28i4978p9dpcdxv2yx4m17bchdypm3yy6m6gzchdlrv2iklhl9"))))
      (build-system gnu-build-system)
      (inputs
       (list libx11 xorgproto))
      (propagated-inputs
       (list fontconfig freetype libxext libxft libxpm libxt xbitmaps))
      (native-inputs
       ;; This fails to build with GCC 14 due to missing header includes and
       ;; other C semantics issues.
       (list gcc-11
             autoconf
             automake
             byacc
             flex
             libtool
             pkg-config))
      (home-page "https://motif.ics.com/motif")
      (synopsis "Toolkit for the X window system")
      (description "Motif is a standard graphical user interface, (as defined
by the IEEE 1295 specification), used on more than 200 hardware and software
platforms.  It provides application developers, end users, and system vendors
with a widely used environment for standardizing application presentation on a
wide range of platforms.")
      (license license:lgpl2.1+))))
