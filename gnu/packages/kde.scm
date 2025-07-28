;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019-2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Prafulla Giri <pratheblackdiamond@gmail.com>
;;; Copyright © 2020-2025 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2021 la snesne <lasnesne@lagunposprasihopre.org>
;;; Copyright © 2021, 2022, 2023, 2024, 2025 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Mehmet Tekman <mtekman89@gmail.com>
;;; Copyright © 2024 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2025 Sughosha <sughosha@disroot.org>
;;; Copyright © 2025 Junker <dk@junkeria.club>
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

(define-module (gnu packages kde)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages kde-plasma)
  ;; Including this module breaks the build.
  ;#:use-module ((gnu packages kde-systemtools) #:select (dolphin))
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public baloo-widgets
  (package
    (name "baloo-widgets")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/baloo-widgets-" version ".tar.xz"))
       (sha256
        (base32 "1wamfsl9nq7si4sys0y49yrf5gwvr16m0qgpd3xww8dddma7ckc6"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list baloo kconfig ki18n kio))
    (arguments
     (list #:configure-flags #~(list "-DBUILD_WITH_QT6=ON")
           #:qtbase qtbase))
    (home-page "https://community.kde.org/Baloo")
    (synopsis "Wigets for use with Baloo")
    (description "Baloo is a framework for searching and managing metadata.
This package contains GUI widgets for baloo.")
    (license license:lgpl2.0+)))

(define-public futuresql
  (package
    (name "futuresql")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/futuresql/futuresql-"
                           version ".tar.xz"))
       (sha256
        (base32 "0hxxpv672jw3d14gk6dilphfcwkmbyffv0r9pakkr2v1m7axhkp4"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs (list qcoro-qt6))
    (arguments
     (list #:qtbase qtbase
           #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (home-page "https://api.kde.org/futuresql/html/index.html")
    (synopsis "Non-blocking Qt database framework")
    (description "This package provides a non-blocking Qt database framework.")
    (license license:lgpl2.1+)))

(define-public grantleetheme
  (package
    (name "grantleetheme")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/grantleetheme-" version ".tar.xz"))
       (sha256
        (base32 "0imf47wf4v8vzzyk9sq6plh9pp2l2q0dycyhdb7hp4jxlj3n4jcg"))))
    (build-system qt-build-system)
    (arguments (list
                #:qtbase qtbase
                #:tests? #f))  ; unexpected error in the test suite.
    (native-inputs
     (list extra-cmake-modules libxml2)) ;; xmllint required for tests
    (inputs
     (list kguiaddons
           ki18n
           kiconthemes
           knewstuff
           kxmlgui))
    (propagated-inputs (list ktexttemplate))
    (home-page "https://invent.kde.org/pim/grantleetheme")
    (synopsis "Library providing Grantlee theme support")
    (description "This library provides Grantlee theme support.")
    (license ;; LGPL for libraries, FDL for documentation
     (list license:lgpl2.1+ license:fdl1.2+))))

(define-public akregator
  (package
    (name "akregator")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akregator-" version ".tar.xz"))
       (sha256
        (base32 "0gimz1k5yzkmpn4mq1dfvx3x5n6y7jmm74npy2mn3mdmi9lrg6hh"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list akonadi
           akonadi-contacts
           akonadi-mime
           bash-minimal
           boost
           breeze-icons
           gpgme
           grantleetheme
           kcmutils
           kcontacts
           kcrash
           kiconthemes
           kimap
           kitemmodels
           kmessagelib
           kmime
           knotifications
           knotifyconfig
           kontactinterface
           kpimcommon
           kpimtextedit
           kquickcharts
           kstatusnotifieritem
           ktextaddons
           ktexteditor
           ktextwidgets
           kuserfeedback
           libkdepim
           libkleo
           qgpgme-qt6
           qtdeclarative
           qtwebchannel
           qtwebengine
           syndication))
    (home-page "https://apps.kde.org/en/akregator")
    (synopsis "KDE Feed Reader")
    (description
     "Akregator is a news feed reader.  It enables you to follow news
sites, blogs and other RSS/Atom-enabled websites without the need to manually
check for updates using a web browser.  Akregator is designed to be both easy to
use and to be powerful enough to read hundreds of news sources conveniently.
It comes with a fast search, advanced archiving functionality and an internal
browser for easy news reading.")
    (license license:gpl2+)))

(define-public gwenview
  (package
    (name "gwenview")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/gwenview-" version ".tar.xz"))
       (sha256
        (base32 "0rh4249wqhm35ahpyhpnxdnaw8s0hklx2mdsmfj6m20f26w90ifb"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "ctest" "-E"
                             (string-append
                              "("
                              (string-join '("placetreemodeltest"
                                             "historymodeltest"
                                             "contextmanagertest"
                                             "urlutilstest")
                                           "|")
                              ")"))))))))
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

(define-public analitza
  (package
    (name "analitza")
    (version "24.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/education/analitza")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "029n48pshcnyidnzv2ikyhamxg6j0ndbjavwrxdc29hrm00dmw8m"))))
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
    (version "24.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/education/kalgebra")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g4rrq3csp0w6xhc5cbbilz7xhhq9zdngc8bc9d16p02xz61qd4i"))))
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
           qtwebengine
           qtwebchannel
           readline))
    (home-page "https://invent.kde.org/education/kalgebra")
    (synopsis "Calculator and plotting tool")
    (description "KAlgebra is a calculator that lets you plot different types
of 2D and 3D functions and to calculate easy (and not so easy) calculations,
such as addition, trigonometric functions or derivatives.")
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

(define-public kdsoap-ws-discovery-client
  (package
    (name "kdsoap-ws-discovery-client")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kdsoap-ws-discovery-client/"
                           "/kdsoap-ws-discovery-client-" version ".tar.xz"))
       (sha256
        (base32 "0yj2ngw4li5r6zhmkh2lb8fdf8ixz6pp5hxsb4342pz72g04glic"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs (list kdsoap))
    (arguments (list
                ;; test require network.
                #:tests? #f
                #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")
                #:qtbase qtbase))
    (home-page "https://caspermeijn.gitlab.io/kdsoap-ws-discovery-client/")
    (synopsis "WS-Discovery client library based on KDSoap")
    (description "This package provides a ws-Discovery client library based on
KDSoap.")
    (license license:gpl3+)))

(define-public kirigami-addons
  (package
    (name "kirigami-addons")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/libraries/kirigami-addons")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "099giz2a4pb68pyynwpzq8i49qxqs6mv9qjvi30sqcmkyvmqrpxh"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f)) ;failing tests
    (native-inputs (list extra-cmake-modules))
    (inputs (list kconfig
                  kcoreaddons
                  kglobalaccel
                  kguiaddons
                  ki18n
                  kirigami
                  qtdeclarative))
    (home-page "https://invent.kde.org/libraries/kirigami-addons")
    (synopsis "Add-ons for the Kirigami framework")
    (description
     "This package provides Kirigami components usable by both touch
and desktop experiences.")
    (license license:lgpl2.0+)))

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

(define-public kcolorchooser
  (package
    (name "kcolorchooser")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kcolorchooser-" version ".tar.xz"))
       (sha256
        (base32
         "0af20kdr09r8vdmxaqq5djdni37r7ik3mhfn5864q9jy07017816"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcoreaddons
           ki18n
           kxmlgui))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://apps.kde.org/de/kcolorchooser/")
    (synopsis "Color selector utility")
    (description "KColorChooser is a utility to select a color.")
    (license license:expat)))

(define-public kolourpaint
  (package
    (name "kolourpaint")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kolourpaint-" version ".tar.xz"))
       (sha256
        (base32 "0fg72dfk5jh2hqf2lplivc0a6gilzgz8l14wfk95s8fmmcsilxxs"))))
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
           kxmlgui))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "http://kolourpaint.org/")
    (synopsis "Paint program for KDE")
    (description "KolourPaint is a paint program for KDE.  It is useful for
painting, image manipulating and icon editing.")
    (license (list license:lgpl2.0+ license:bsd-2))))

(define-public krita
  (package
    (name "krita")
    (version "5.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/krita/" version "/krita-" version
             ".tar.gz"))
       (sha256
        (base32 "19nb98rh8j9jdd8hz8m56hrpljqv74p7j1k5plqnkwpbdmaszj88"))
       (patches (search-patches "krita-bump-sip-abi-version-to-12.8.patch"
                                "krita-xsimd-13-compat.patch"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DCMAKE_CXX_FLAGS=-fPIC")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-raqm
                    (lambda _
                      ;; Uncomment the substitute block underneath this once the
                      ;; libraqm variable is patched upstream. This will force it to
                      ;; use the Guix provided library.
                      ;; (substitute* "CMakeLists.txt"
                      ;; (("add_subdirectory\\(3rdparty_vendor\\)")
                      ;; "find_package(Raqm 0.10.1 REQUIRED)"))
                      ;; (delete-file-recursively "3rdparty_vendor"))
                      ;;
                      ;; Patch the supplied vendor Raqm library (v0.10.1) to use fPIC
                      (substitute* "3rdparty_vendor/raqm/CMakeLists.txt"
                        (("set\\(CMAKE_AUTOMOC OFF\\)")
                         "set(CMAKE_AUTOMOC OFF)
set(CMAKE_CXX_FLAGS \"${CMAKE_CXX_FLAGS} -fPIC\" )
set(CMAKE_C_FLAGS \"${CMAKE_C_FLAGS} -fPIC\" ) "))))
                  (add-after 'install 'wrap-bin
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (python-path (getenv "GUIX_PYTHONPATH")))
                        (wrap-program (string-append out "/bin/krita")
                          `("GUIX_PYTHONPATH" ":" prefix (,python-path)))))))))
    (native-inputs
     (list curl
           eigen
           extra-cmake-modules
           gettext-minimal
           kitemmodels
           pkg-config
           qwt
           vc))
    (inputs
     (list bash-minimal
           boost
           exiv2
           fontconfig
           fftw-cmake
           ;; fftw
           ;; We use fftw-cmake since fftwm doesn't provide the required
           ;; CMake files when build with gnu.
           ;; See: https://bugzilla.redhat.com/show_bug.cgi?id=1729652#c5
           freetype
           fribidi
           giflib
           gsl
           harfbuzz
           imath
           immer
           karchive-5
           kcompletion-5
           kconfig-5
           kcoreaddons-5
           kcrash-5
           kguiaddons-5
           ki18n-5
           kiconthemes-5
           kio-5
           kitemviews-5
           kseexpr
           kwidgetsaddons-5
           kwindowsystem-5
           kxmlgui-5
           lager
           lcms
           libheif
           libjpeg-turbo
           libjxl
           libkdcraw-qt5
           libmypaint
           libpng
           ;; libraqm
           ;; We use the provided 3rd_party_vendor library instead of
           ;; libraqm 0.10.1 with patches until libraqm is patched.
           ;; See: https://github.com/HOST-Oman/libraqm/issues/191
           libraw
           libtiff
           libunibreak
           libwebp
           libx11
           libxcb
           libxi
           mlt
           opencolorio
           openexr
           openjpeg
           perl
           poppler-qt5
           python-pyqt
           python-pyqt5-sip
           qtbase-5
           qtdeclarative-5
           qtmultimedia-5
           qtsvg-5
           qtx11extras
           quazip-5
           sdl2
           xsimd
           zlib
           zug))
    (home-page "https://krita.org")
    (synopsis "Digital painting application")
    (description
     "Krita is a professional painting tool designed for concept artists,
illustrators, matte and texture artists, and the VFX industry.  Notable
features include brush stabilizers, brush engines and wrap-around mode.")
    (license license:gpl2+)))

(define-public libqaccessibilityclient
  (package
    (name "libqaccessibilityclient")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/" name
                                  "/libqaccessibilityclient-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0csxbwy4479196l32j4xnk672kiyggcaf3fi3q2cbj9dc94c8l2c"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f ;TODO: Failing tests
           #:configure-flags
           #~(list (string-append
                    "-DQT_MAJOR_VERSION="
                    #$(version-major
                       (package-version (this-package-input "qtbase")))))
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              ;; make Qt render "offscreen", required for tests
                              (setenv "QT_QPA_PLATFORM" "offscreen")
                              ;; For missing '/etc/machine-id'
                              (setenv "DBUS_FATAL_WARNINGS" "0")
                              (setenv "HOME"
                                      (getcwd))
                              (invoke "dbus-launch" "ctest")))))))
    (native-inputs (list dbus extra-cmake-modules))
    (inputs (list qtbase))
    (home-page "https://invent.kde.org/libraries/libqaccessibilityclient")
    (synopsis "Helper library to make writing accessibility tools easier")
    (description "This package provides library that is used when writing
accessibility clients such as screen readers.")
    (license license:lgpl2.1+)))

(define-public libqaccessibilityclient-qt5
  (package
    (inherit libqaccessibilityclient)
    (name "libqaccessibilityclient-qt5")
    (inputs (modify-inputs (package-inputs libqaccessibilityclient)
              (replace "qtbase" qtbase-5)))))

(define-public qca
  (package
    (name "qca")
    (version "2.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/qca/" version
                            "/qca-" version ".tar.xz"))
        (sha256
         (base32 "0kkf8wyc7slii86danfl4cx59yhcyc363ydiwapnnyyxihlxamf5"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl qtbase-5))
    (home-page "https://userbase.kde.org/QCA")
    (synopsis "Libraries for the Qt Cryptographic Architecture")
    (description "The Qt Cryptographic Architecture (QCA) provides a
straightforward and cross-platform API for a range of cryptographic features,
including SSL/TLS, X.509 certificates, SASL, OpenPGP, S/MIME CMS, and smart
cards.")
    (license license:lgpl2.1+)))

(define-public qca-qt6
  (package
    (inherit qca)
    (name "qca-qt6")
    (arguments (list #:configure-flags #~(list "-DBUILD_WITH_QT6=ON")))
    (inputs
     (list openssl qtbase qt5compat))))

(define-public kopeninghours
  (package
    (name "kopeninghours")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0z8jhp47zdyl50b2jrda5fw86i8vkmswv4g26f2h77fz0pnq31k6"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DBUILD_WITH_QT6=ON")
      #:phases #~(modify-phases %standard-phases
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         (setenv "QT_QPA_PLATFORM" "offscreen")
                         (invoke "ctest" "-E"
                                 "(evaluatetest|iterationtest)")))))))
    (native-inputs (list bison extra-cmake-modules flex))
    (inputs (list boost
                  kholidays
                  ki18n
                  osmctools
                  qtbase
                  qtdeclarative))
    (home-page "https://invent.kde.org/libraries/kopeninghours")
    (synopsis "Get opening hours from OpenStreetMap")
    (description
     "This package provides a library for parsing and evaluating OpenStreetMap
opening hours expressions.")
    (license license:lgpl2.0+)))

(define-public kosmindoormap
  (package
    (name "kosmindoormap")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ik8mi2ljhlj0sv1pshk6a97flagrpj40mlv2l0j6lya5ngn7mw9"))))
    (build-system cmake-build-system)
    (native-inputs (list bison extra-cmake-modules flex python-minimal))
    (inputs (list ki18n
                  kopeninghours
                  kpublictransport
                  qtbase
                  qtdeclarative
                  libxkbcommon
                  zlib))
    (home-page "https://invent.kde.org/libraries/kosmindoormap")
    (synopsis "Indoor map renderer")
    (description
     "This package provides facilities for rendering OpenStreetMap
multi-floor indoor maps.")
    (license license:lgpl2.0+)))

(define-public kpublictransport
  (package
    (name "kpublictransport")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kpublictransport-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1hi189yx81gabpk7czmqx2xy1slnjhhq8m5gv07avfhsw0kab8ba"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'check-setup
                          (lambda* (#:key inputs #:allow-other-keys)
                            (setenv "QT_QPA_PLATFORM" "offscreen")
                            (setenv "HOME" ".")
                            (setenv "TZ" "Europe/Prague")
                            (setenv "TZDIR"
                                    (search-input-directory inputs
                                                            "share/zoneinfo")))))))
    (native-inputs (list extra-cmake-modules pkg-config tzdata-for-tests))
    ;; TODO: clipper and osmctools are not detected
    (inputs (list clipper
                  osmctools
                  protobuf
                  qtdeclarative
                  zlib
                  networkmanager-qt
                  ki18n))
    (home-page "https://api.kde.org/kdepim/kpublictransport/html/index.html")
    (synopsis "Library for accessing realtime public transport data")
    (description
     "This package provides a library for accessing realtime public
transport data and for performing public transport journey queries.")
    (license (list license:lgpl2.0+))))

(define-public ksanecore
  (package
    (name "ksanecore")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                             "/src/ksanecore-" version ".tar.xz"))
       (sha256
        (base32 "06g43b1l72aghkhcn59ss8kjc4sammn5ii5x5sql34kmvgiwamwk"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list ki18n
           sane))
    (home-page "https://invent.kde.org/libraries/ksanecore")
    (synopsis "Library providing logic to interface scanners")
    (description
     "KSaneCore is a library that provides a Qt interface for the SANE library
for scanner hardware.")
    (license license:lgpl3+)))

(define-public libksane
  (package
    (name "libksane")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                             "/src/libksane-" version ".tar.xz"))
       (sha256
        (base32 "1b0cbf4cq0ajl5xlpy75wj4p1zsri2igh23pswj8ysnrrk0pxg5w"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list ki18n
           ksanecore
           ktextwidgets
           kwallet
           kwidgetsaddons))
    (home-page "https://invent.kde.org/graphics/libksane")
    (synopsis "Library providing QWidget with logic to interface scanners")
    (description
     "Libksane is a Qt-based interface for SANE library to control flat
scanners.")
    (license license:lgpl3+)))

(define-public snorenotify
  (package
    (name "snorenotify")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/snorenotify/"
                            version "/src/snorenotify-" version ".tar.xz"))
        (sha256
         (base32
          "0jz6ivk90h7iwgyxar7xzzj8yvzn6s1my6cqs9bdnwqswfk1nhbd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; both tests fail, require display
    (inputs
     (list qtbase-5))
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (home-page "https://techbase.kde.org/Projects/Snorenotify")
    (synopsis "Qt notification framework")
    (description "Snorenotify is a multi platform Qt notification framework.
Using a plugin system it is possible to create notifications with many
different notification systems.")
    (license license:lgpl3)))

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

(define-public okular
  (package
    (name "okular")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1836wiiq6frvz4ddsi1iir4dkmd9p0lc4mwd5pn5swbb03f9824d"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          ;; use installed data to check.
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ctest"
                        "--output-on-failure"
                        "--rerun-failed"
                        "-E"
                        "(annotationtoolbartest|mainshelltest|parttest|\
chmgeneratortest)"))))
          (add-before 'check 'check-setup
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((share (string-append (assoc-ref outputs "out") "/share")))
                (setenv "QT_QPA_PLATFORM" "offscreen")
                (setenv "HOME" ".")
                (setenv "XDG_DATA_DIRS"
                        (string-append
                         share ":" (getenv "XDG_DATA_DIRS")))
                (invoke "update-desktop-database" "-v" share)))))))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config
           ;; for test
           desktop-file-utils
           python-wrapper))
    (inputs
     (list ebook-tools
           breeze-icons
           discount
           djvulibre
           plasma-activities
           chmlib
           kdegraphics-mobipocket
           karchive
           kbookmarks
           kcompletion
           kconfig
           libjpeg-turbo
           libtiff
           kirigami
           purpose
           freetype
           ki18n
           kiconthemes
           kio
           kparts
           kpty
           ktextwidgets
           qtspeech
           kwallet
           kwindowsystem
           libkexiv2
           libspectre
           libzip
           libxkbcommon
           phonon
           poppler-qt6
           qca
           qtdeclarative
           qtsvg
           qtwayland
           threadweaver
           kcrash))
    (home-page "https://apps.kde.org/okular/")
    (synopsis "Document viewer")
    (description
     "Okular is a document viewer developed for KDE.  It can display files in
a variety of formats, including PDF, PostScript, DejaVu, and EPub.")
    (license license:gpl2+)))

(define-public poxml
  (package
    (name "poxml")
    (version "24.12.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kde/stable/release-service/" version
                              "/src/poxml-" version ".tar.xz"))
              (sha256
               (base32
                "0nmclngg0mrd1j2app6fggpvp93sw5p4q1nddwq8is0dabm57yx1"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list gettext-minimal qtbase))
    (home-page "https://apps.kde.org/development/")
    (synopsis "Tools for translating DocBook XML files with Gettext")
    (description "This is a collection of tools that facilitate translating
DocBook XML files using Gettext message files (PO files).  Also included are
several command-line utilities for manipulating DocBook XML files, PO files and
PO template files.")
    (license license:gpl2+)))

(define-public kdegraphics-mobipocket
  (package
    (name "kdegraphics-mobipocket")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0rpam31s5cvky4w3bb2qp1pjv0gm9f63a2jv6bcim7qnz050bvvn"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kio qtbase qt5compat))
    (home-page "https://apps.kde.org/en/kdegraphics_mobipocket")
    (synopsis "KDE thumbnailer for Mobipocket files")
    (description "This package provides a KDE plugin that shows thumbnails of
Mobipocket e-books in Dolphin and other KDE apps.")
    (license license:gpl2+)))

(define-public kdegraphics-thumbnailers
  (package
    (name "kdegraphics-thumbnailers")
    (version "24.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/graphics/kdegraphics-thumbnailers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vlrn6wg9rpg2cnm6y243accbrgcpdmkg4y8qasw6ify2hjhgfmi"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:configure-flags
      #~'("-DQT_MAJOR_VERSION=6")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "ps/gscreator.cpp"
                (("\"gs\",")
                 (string-append "\""
                                (search-input-file inputs "bin/gs") "\","))
                (("\"dvips\",")
                 (string-append "\""
                                (search-input-file inputs "bin/dvips") "\","))))))))
    (native-inputs (list extra-cmake-modules))
    (inputs (list ghostscript
                  karchive
                  kdegraphics-mobipocket
                  kio
                  libkdcraw
                  libkexiv2
                  qtbase
                  texlive-dvips-bin))
    (home-page "https://apps.kde.org/kdegraphics_thumbnailers")
    (synopsis "KDE thumbnailer for media files")
    (description "These plugins allow KDE software to display thumbnails for
PostScript, PDF, RAW, Mobipocket, and Blender files.")
    (license license:gpl2)))

(define-public libkexiv2
  (package
    (name "libkexiv2")
    (version "25.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1a5mwywza7wxprygl06k89msmykyb6m7si4mdbqsr3yvx68xgf6p"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list exiv2 qtbase))
    (home-page "https://invent.kde.org/graphics/libkexiv2")
    (synopsis "Manipulate the metadata of images")
    (description "Libkexiv2 wraps the Exiv2 library, allowing to manipulate
picture metadata as EXIF/IPTC and XMP.")
    (license license:gpl2+)))

(define-public kuserfeedback
  ;; FIXME: Try to reduce data collection and ensure transmission i disabled by default.
  ;; FIXME: Check https://www.reddit.com/r/kde/comments/f7ojg9 for insights
  (package
    (name "kuserfeedback")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kuserfeedback/"
                           "/kuserfeedback-" version ".tar.xz"))
       (sha256
        (base32 "04zx5wfzqyargbvkbd66iabi4mfsn34qh5mbhpm90inx4aw0h8r5"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           qttools
           ;; For optional component "Survey target expression parser"
           bison
           flex
           ;; For syntax checking and unit tests of PHP server code
           ;;("php" ,php)
           ;;("phpunit" ,phpunit)
           ))
    (inputs
     (list qtdeclarative qtsvg))
    (arguments
     (list
      #:qtbase qtbase
      #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")
      #:tests? #f))  ;; 4/17 fail
    (home-page "https://api.kde.org/frameworks/kuserfeedback/html/")
    (synopsis "Collect application feedback via telemetry and targeted
surveys")
    (description "This framework consists of the following components:
@itemize
@item Libraries for use in applications.
@item QML bindings for the above.
@item A server application.
@item A management and analytics application.
@end itemize")
    (license license:expat)))
