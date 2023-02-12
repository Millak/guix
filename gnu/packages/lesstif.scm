;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))


(define-public lesstif
  (package
    (name "lesstif")
    (version "0.95.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://sourceforge/lesstif/lesstif/" version
               "/lesstif-" version ".tar.bz2"))
        (sha256
          (base32
            "1qzpxjjf7ri1jzv71mvq5m9g8hfaj5yzwp30rwxlm6n2b24a6jpb"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list printproto))
    (inputs
      (list libxext libxt))
    (home-page "https://lesstif.sourceforge.net/")
    (synopsis "Clone of the Motif toolkit for the X window system")
    (description "Clone of the Motif toolkit for the X window system.")
    (license license:gpl2+))) ; some files are lgpl2.1+ or x11

(define-public motif
  ;; This commit is from September 2021 and v2.3.8 from 2017.
  (let ((commit "59858b0811e8d9dfaeb142f2b5a96f55482be1ed")
        (revision "0"))
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
           "0yycq0vzga9qmgbfzn2d02ilpwxixwdv2j1d8sddl4dripcrg21m"))))
      (build-system gnu-build-system)
      (inputs
       (list libx11 xorgproto))
      (propagated-inputs
       (list fontconfig freetype libxext libxft libxpm libxt xbitmaps))
      (native-inputs
       (list autoconf automake byacc flex libtool pkg-config))
      (home-page "https://motif.ics.com/motif")
      (synopsis "Motif toolkit for the X window system")
      (description "Motif is a standard graphical user interface, (as defined
by the IEEE 1295 specification), used on more than 200 hardware and software
platforms.  It provides application developers, end users, and system vendors
with a widely used environment for standardizing application presentation on a
wide range of platforms.")
      (license license:lgpl2.1+))))
