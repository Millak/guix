;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Sughosha <sughosha@disroot.org>
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

(define-module (gnu packages kde-graphics)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg))

(define-public gwenview
  (package
    (name "gwenview")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/gwenview-" version ".tar.xz"))
       (sha256
        (base32 "0z6ngbb1pkgdy7z6nla788wh7wcz3ny64041yv7k0falzpwfx4jd"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:test-exclude
           (string-append "("
                          (string-join '("placetreemodeltest"
                                         "historymodeltest"
                                         "contextmanagertest"
                                         "urlutilstest")
                                       "|")
                          ")")))
    (native-inputs
     (list extra-cmake-modules
           kdoctools
           pkg-config))
    (inputs
     (list baloo
           cfitsio
           exiv2
           plasma-activities
           kcolorpicker
           kcrash
           kguiaddons
           ki18n
           kiconthemes
           kimageannotator
           kio
           kitemmodels
           knotifications
           kparts
           lcms
           libjpeg-turbo
           libkdcraw
           libpng
           libtiff
           libxkbcommon
           phonon
           purpose
           qtimageformats
           qtmultimedia
           qtsvg
           qtwayland
           wayland
           wayland-protocols
           zlib))
    (home-page "https://userbase.kde.org/Gwenview")
    (synopsis "Image viewer for KDE")
    (description
     "Gwenview is an image viewer for KDE.  It also provides image editing and
annotating features.")
    (license license:gpl2+)))

(define-public kdiagram
  (package
    (name "kdiagram")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kdiagram/" version
                           "/kdiagram-" version ".tar.xz"))
       (sha256
        (base32 "0vcw339v6nl1haznp58spimanfhw143cindbym1q3ccxrp1b0na6"))
       (patches (search-patches
                 "kdiagram-Fix-missing-link-libraries.patch"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list qtsvg))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/graphics/kdiagram")
    (synopsis "Libraries for creating business diagrams")
    (description "This package provides libraries for integrating business
diagrams in Qt-based applications.

@code{KCharts} provides an implementation of the ODF Chart specification.  It
supports stock charts, box charts, and whisker charts.  @code{KGantt} provides
a module for implementing ODF Gantt charts, which are bar charts that
illustrate project schedules.")
    (license license:gpl2+)))

(define-public kolourpaint
  (package
    (name "kolourpaint")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kolourpaint-" version ".tar.xz"))
       (sha256
        (base32 "05xxcbfhn28896jk4dv8gq892fwjq501gbab7q5lry2nvazsqgnh"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcrash
           kguiaddons
           ki18n
           kio
           kjobwidgets
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libksane
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "http://kolourpaint.org/")
    (synopsis "Paint program for KDE")
    (description "KolourPaint is a paint program for KDE.  It is useful for
painting, image manipulating and icon editing.")
    (license (list license:lgpl2.0+ license:bsd-2))))

(define-public kseexpr
  (package
    (name "kseexpr")
    (version "4.0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://invent.kde.org/graphics/kseexpr")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "888q3kkv2wq426w000iq14wy3a45rrnn0bmsdks6caz4vq04ccay"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DBUILD_TESTS=ON"))) ; disabled by default
    (native-inputs
     (list bison doxygen extra-cmake-modules flex googletest))
    (inputs
     (list ki18n libpng qtbase-5))
    (home-page "https://invent.kde.org/graphics/kseexpr")
    (synopsis "Embeddable expression evaluation engine")
    (description "This package contains the fork of Disney Animation's SeExpr
expression library, that is used in Krita.")
    (license license:gpl3+)))
