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
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-graphics)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages kde-plasma)
  ;; Including this module breaks the build.
  ;#:use-module ((gnu packages kde-systemtools) #:select (dolphin))
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages tls)
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
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/libraries/kirigami-addons")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f93y893kvqysdrvcr1f6f5rmg38k0hbmf53r672xrxis5yar0vl"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f)) ;failing tests
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcolorscheme
                  kconfig
                  kcoreaddons
                  kcrash
                  kglobalaccel
                  kguiaddons
                  ki18n
                  kiconthemes
                  kirigami
                  qtdeclarative))
    (home-page "https://invent.kde.org/libraries/kirigami-addons")
    (synopsis "Add-ons for the Kirigami framework")
    (description
     "This package provides Kirigami components usable by both touch
and desktop experiences.")
    (license license:lgpl2.0+)))

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
    (version "25.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "02xdklp1d2d5dxmsykchw37fs1vi1f6b1mk33cywlq63qbsgpniw"))))
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
    (version "25.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ilia0ydm78s7nvrh8ph5865x52zc1pd9k1qgad767sr91z5r15h"))))
    (build-system cmake-build-system)
    (native-inputs (list bison extra-cmake-modules flex python-minimal))
    (inputs (list ki18n
                  kirigami-addons
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

(define-public ksanecore
  (package
    (name "ksanecore")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                             "/src/ksanecore-" version ".tar.xz"))
       (sha256
        (base32 "1ilp0rgb4pj9q9wx6mh8gmgn9vmly18a28lnglyf1blpwxpbvjzg"))))
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
