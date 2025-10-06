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

(define-module (gnu packages kde-education)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline))

(define-public analitza
  (package
    (name "analitza")
    (version "25.08.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/education/analitza")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q9jfkvs1xyxl7fmw0jzh7fh7rd278825i5w61cikqpwnjmp5jzf"))))
    (native-inputs (list extra-cmake-modules qttools))
    (inputs (list eigen qtbase qtdeclarative qtsvg))
    (build-system qt-build-system)
    (home-page "https://invent.kde.org/education/analitza")
    (synopsis "Library to add mathematical features to a program")
    (description "Analitza is a library to work with mathematical objects.
It adds mathematical features to your program, such as symbolic computations
and some numerical methods; for instance the library can parse mathematical
expressions and let you evaluate and draw them.")
    (license license:gpl2+)))

(define-public kalgebra
  (package
    (name "kalgebra")
    (version "25.08.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/education/kalgebra")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00cryk135104pdid6sai763yxh6ghlzgg9rfi5qxlb0nidia0mgl"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-qt-process-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin/kalgebra"))
                    (qt-process-path
                     (search-input-file
                      inputs "/lib/qt6/libexec/QtWebEngineProcess")))
                (wrap-program bin
                  `("QTWEBENGINEPROCESS_PATH" = (,qt-process-path)))))))))
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list analitza
           kconfigwidgets
           kcoreaddons
           kdoctools
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libplasma
           ncurses
           qtbase
           qtdeclarative
           qtsvg
           qtwayland
           qtwebengine
           qtwebchannel
           readline))
    (home-page "https://invent.kde.org/education/kalgebra")
    (synopsis "Calculator and plotting tool")
    (description "KAlgebra is a calculator that lets you plot different types
of 2D and 3D functions and to calculate easy (and not so easy) calculations,
such as addition, trigonometric functions or derivatives.")
    (license license:gpl2+)))

(define-public labplot
  (package
    (name "labplot")
    (version "2.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/labplot"
                           "/labplot-" version ".tar.xz"))
       (sha256
        (base32 "17b78s84hqq51chhzfx5in9b6ijkwa6xhq1y8sclscirvz46majk"))))
    (build-system qt-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DENABLE_CANTOR=OFF" ;not packaged
                   "-DENABLE_MQTT=OFF" ;not packaged (qtmqtt)
                   ;; FIXME: readstat (optional dependency) is available in the
                   ;; statistics module, but that module can't be used here.
                   "-DENABLE_READSTAT=OFF"
                   ;; This is a bundled library that is not packaged.
                   "-DENABLE_LIBORIGIN=ON")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     ;; This test fails, I don't know why.
                     (invoke "ctest" "-E" "(ParserTest|ReadStatFilterTest|\
WorksheetElementTest)")))))))
    (native-inputs (list bison
                         extra-cmake-modules
                         pkg-config
                         python-wrapper
                         qttools-5))
    (inputs
     (list breeze-qt5 ;for dark themes
           breeze-icons ;for icons
           gsl
           karchive-5
           kcompletion-5
           kconfig-5
           kconfigwidgets-5
           kcoreaddons-5
           kcrash-5
           kdoctools-5
           ki18n-5
           kiconthemes-5
           kio-5
           knewstuff-5
           kparts-5
           kservice-5
           ksyntaxhighlighting-5
           ktextwidgets-5
           kwidgetsaddons-5
           kxmlgui-5
           qtbase-5
           qtsvg-5
           shared-mime-info
           ;; Optional.
           cfitsio
           fftw
           hdf5
           libcerf
           lz4
           netcdf
           qtserialport
           zlib))
    (home-page "https://labplot.kde.org/")
    (synopsis "Interactive graphing and analysis of scientific data")
    (description "LabPlot is a tool for interactive graphing and analysis of
scientific data.  It provides an easy way to create, manage and edit plots and
to perform data analysis.")
    (license (list license:gpl2+     ;labplot
                   license:gpl3+)))) ;liborigin
