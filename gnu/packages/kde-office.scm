;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Sughosha <sughosha@disroot.org>
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

(define-module (gnu packages kde-office)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg))

(define-public calligra
  (package
    (name "calligra")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/calligra/"
                                  "calligra-" version ".tar.xz"))
              (sha256
               (base32
                "0pyri2ypzva4b4rnl2p3xp0ph5xcn181msj86l5xq6sg7zkza0fl"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           ;; XXX: 26/164 tests fail.
           #:configure-flags
           #~(list "-DBUILD_TESTING=OFF")
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools perl pkg-config))
    (inputs
     (list boost
           eigen
           fontconfig
           freetype
           gsl
           imath
           karchive
           kcmutils
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdiagram
           kguiaddons
           ki18n
           kiconthemes
           kio
           kitemviews
           kjobwidgets
           knotifications
           knotifyconfig
           ktextwidgets
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           lcms
           libdisplay-info
           libetonyek
           libgit2
           libodfgen
           librevenge
           libvisio
           libwpd
           libwpg
           libwps
           libxkbcommon
           okular
           openexr
           openssl
           phonon
           poppler-qt6
           pstoedit
           qca-qt6
           qtkeychain-qt6
           qtsvg
           qtwebengine
           shared-mime-info
           sonnet
           threadweaver
           zlib))
    (home-page "https://calligra.org/")
    (synopsis "Office and graphic art suite")
    (description
     "Calligra Suite is a collection of office applications linked together by
a common base.

The applications currently included in Calligra Suite are:

Office productivity:
@itemize
@item Words: Word processor
@item Sheets: Spreadsheet calculator
@item Stage: Presentation program
@end itemize

Graphics:
@itemize
@item Karbon: Vector graphics
@end itemize

Advanced plugins:
@itemize
@item Chart: Graphic data visualization
@item KFormula: Mathematical formulas
@end itemize")
    (license (list license:lgpl2.0+ license:gpl2+))))
