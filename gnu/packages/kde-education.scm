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
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages protobuf)
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
    (version "2.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/labplot"
                           "/labplot-" version ".tar.xz"))
       (sha256
        (base32 "0shhdinrynsi1lhny8ag0hw83r6iaqsk34a7gipmn3plvnzmb0g2"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list "-DENABLE_CANTOR=OFF" ;not packaged
                   "-DENABLE_MQTT=OFF" ;not packaged (qtmqtt)
                   ;; FIXME: readstat (optional dependency) is available in the
                   ;; statistics module, but that module can't be used here.
                   "-DENABLE_READSTAT=OFF"
                   ;; This is a bundled library that is not packaged.
                   "-DENABLE_LIBORIGIN=ON")
           #:test-exclude
           (string-append "("
                          (string-join '("ParserTest"
                                         "ReadStatFilterTest"
                                         "WorksheetElementTest")
                                       "|")
                          ")")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     ;; This test fails, I don't know why.
                     (invoke "ctest" "-E" test-exclude)))))))
    (native-inputs (list bison
                         extra-cmake-modules
                         kdoctools
                         pkg-config
                         python-wrapper
                         qttools))
    (inputs
     (list breeze ;for dark themes
           breeze-icons ;for icons
           discount
           eigen
           gsl
           karchive
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kiconthemes
           kio
           knewstuff
           kparts
           kservice
           ksyntaxhighlighting
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           matio
           orcus
           purpose
           poppler-qt6
           qtsvg
           shared-mime-info
           ;; Optional.
           cfitsio
           fftw
           hdf5
           libcerf
           lz4
           netcdf
           qt-advanced-docking-system
           qtserialport
           qtwayland
           qxlsx
           zlib
           zstd))
    (home-page "https://labplot.kde.org/")
    (synopsis "Interactive graphing and analysis of scientific data")
    (description "LabPlot is a tool for interactive graphing and analysis of
scientific data.  It provides an easy way to create, manage and edit plots and
to perform data analysis.")
    (license (list license:gpl2+     ;labplot
                   license:gpl3+)))) ;liborigin

(define-public marble-qt
  (package
    (name "marble-qt")
    (version "25.08.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/education/marble.git/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04x6i7k1c09xn74rcx3vr4m8wpqb6bb24pwiyw6n65z1vf3qm3y5"))))
    (build-system qt-build-system)
    (arguments
     ;; FIXME: libmarblewidget-qt5.so.28 not found.  Also enable the
     ;; corresponding configure flag to build tests.
     (list
      #:tests? #f
      #:qtbase qtbase
      #:configure-flags #~(list "-DBUILD_MARBLE_TOOLS=YES" ;file conversion tools
                                "-DBUILD_TOUCH=YES")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'alter-osmctools-lookup
                     (lambda _
                       (substitute* "tools/vectorosm-tilecreator/autotests/CMakeLists.txt"
                         (("\\$<TARGET_FILE:osmconvert>")
                          (which "osmconvert"))))))))
    (native-inputs (list abseil-cpp extra-cmake-modules kdoctools osmctools
                         qttools))
    ;; One optional dependency missing: libwlocate.
    (inputs (list gpsd
                  kcoreaddons
                  kcrash
                  ki18n
                  kio
                  knewstuff
                  kparts
                  krunner
                  kwallet
                  perl
                  phonon
                  protobuf
                  qt5compat
                  qtdeclarative
                  qtlocation
                  qtpositioning
                  qtserialport
                  qtsvg
                  qtwebchannel
                  qtwebengine
                  shapelib
                  shared-mime-info
                  zlib))
    (home-page "https://marble.kde.org/")
    (synopsis "Virtual globe and world atlas")
    (description
     "Marble is similar to a desktop globe.  At closer scale it
becomes a world atlas, while OpenStreetMap takes the user to street level.  It
supports searching for places of interest, viewing Wikipedia articles,
creating routes by drag and drop and more.")
    (license license:lgpl2.1+)))
