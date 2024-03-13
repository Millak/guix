;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2019, 2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages kde-frameworks)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public extra-cmake-modules
  (package
    (name "extra-cmake-modules")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0z4nqravsfzlsgvkg5rha2d0qxfr3pfncw7z2fxzzqvzj7mfk6im"))))
    (build-system cmake-build-system)
    (native-inputs
     ;; Add test dependency, except on armhf where building it is too
     ;; expensive.
     (if (and (not (%current-target-system))
              (string=? (%current-system) "armhf-linux"))
         '()
         (list qtbase-5)))               ;for tests (needs qmake)
    (arguments
     (list
      #:tests? (and (not (%current-target-system))
                    (not (null? (package-native-inputs this-package))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-lib-and-libexec-path
            (lambda _
              (substitute* "kde-modules/KDEInstallDirsCommon.cmake"
                ;; Always install into /lib and not into /lib64.
                (("\"lib64\"") "\"lib\"")
                ;; Install into /libexec and not into /lib/libexec.
                (("LIBDIR \"libexec\"") "EXECROOTDIR \"libexec\""))

              ;; Determine the install path by the major version of Qt.
              ;; TODO: Base the following on values taken from Qt
              ;; Install plugins into lib/qt5/plugins
              ;; TODO: Check if this is okay for Android, too
              ;; (see comment in KDEInstallDirs.cmake)
              (substitute* '("kde-modules/KDEInstallDirs5.cmake"
                             "kde-modules/KDEInstallDirs6.cmake")
                ;; Fix the installation path of Qt plugins.
                (("_define_relative\\(QTPLUGINDIR \"\\$\\{_pluginsDirParent}\" \"plugins\"")
                 "_define_relative(QTPLUGINDIR \"${_pluginsDirParent}\" \"qt${QT_MAJOR_VERSION}/plugins\"")
                ;; Fix the installation path of QML files.
                (("_define_relative\\(QMLDIR LIBDIR \"qml\"")
                 "_define_relative(QMLDIR LIBDIR \"qt${QT_MAJOR_VERSION}/qml\""))

              ;; Qt Quick Control 1 is no longer available in Qt 6.
              (substitute* '("kde-modules/KDEInstallDirs5.cmake")
                (("_define_relative\\(QTQUICKIMPORTSDIR QTPLUGINDIR \"imports\"")
                 "_define_relative(QTQUICKIMPORTSDIR LIBDIR \"qt5/imports\""))

              (substitute* "modules/ECMGeneratePriFile.cmake"
                ;; Install pri-files into lib/qt${QT_MAJOR_VERSION}/mkspecs
                (("set\\(ECM_MKSPECS_INSTALL_DIR mkspecs/modules")
                 "set(ECM_MKSPECS_INSTALL_DIR lib/qt${QT_MAJOR_VERSION}/mkspecs/modules"))))
          ;; Work around for the failed test KDEFetchTranslations.
          ;; It complains that the cmake project name is not
          ;; "frameworks/extra-cmake-modules".
          ;; TODO: Fix it upstream.
          (add-after 'unpack 'fix-test
            (lambda _
              (substitute* "tests/KDEFetchTranslations/CMakeLists.txt"
                (("frameworks/extra-cmake-modules") "extra-cmake-modules"))))
          ;; install and check phase are swapped to prevent install from failing
          ;; after testsuire has run
          (add-after 'install 'check-post-install
            (assoc-ref %standard-phases 'check))
          (delete 'check))))
    ;; optional dependencies - to save space, we do not add these inputs.
    ;; Sphinx > 1.2:
    ;;   Required to build Extra CMake Modules documentation in Qt Help format.
    ;; Qt5LinguistTools , Qt5 linguist tools. , <http://www.qt.io/>
    ;;   Required to run tests for the ECMPoQmTools module.
    ;; Qt5Core
    ;;   Required to run tests for the ECMQtDeclareLoggingCategory module,
    ;;   and for some tests of the KDEInstallDirs module.
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "CMake module files for common software used by KDE")
    (description "The Extra CMake Modules package, or ECM, adds to the
modules provided by CMake to find common software.  In addition, it provides
common build settings used in software produced by the KDE community.")
    (license license:bsd-3)))

(define-public kquickcharts
  (package
    (name "kquickcharts")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/frameworks/"
                                  (version-major+minor version)
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1f91x92qdzxp31z7ixx9jn41hq9f3w9hjia94pab9vsnaz8prbd1"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (system "Xvfb :1 -screen 0 640x480x24 &")
                              (setenv "DISPLAY" ":1")
                              (setenv "QT_QPA_PLATFORM" "offscreen")
                              (invoke "ctest")))))))
    (inputs (list qtbase-5 qtdeclarative-5 qtquickcontrols2-5
                  xorg-server-for-tests))
    (native-inputs (list extra-cmake-modules glslang pkg-config))
    (home-page "https://api.kde.org/frameworks/kquickcharts/html/index.html")
    (synopsis "QtQuick plugin providing high-performance charts")
    (description
     "The Quick Charts module provides a set of charts that can be
used from QtQuick applications for both simple display of data as well as
continuous display of high-volume data.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public phonon
  (package
    (name "phonon")
    (version "4.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/phonon"
                    "/" version "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "16pk8g5rx00x45gnxrqg160b1l02fds1b7iz6shllbfczghgz1rj"))))
    (build-system cmake-build-system)
    (native-inputs
     ;; TODO: Add building the super experimental QML support
     (list appstream extra-cmake-modules pkg-config qttools-5))
    (inputs (list glib qtbase-5 pulseaudio))
    (arguments
     (list #:configure-flags
           #~'("-DCMAKE_CXX_FLAGS=-fPIC"
               "-DPHONON_BUILD_QT6=OFF") ;KDE is still using Qt 5
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'install 'patch-installdir
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((regex (string-append "(INSTALL DESTINATION \")"
                                               #$(this-package-input "qtbase"))))
                     (substitute* "cmake_install.cmake"
                       ((regex all dest)
                        (string-append dest #$output)))))))))
    (home-page "https://community.kde.org/Phonon")
    (synopsis "KDE's multimedia library")
    (description "KDE's multimedia library.")
    (license license:lgpl2.1+)))

(define-public phonon-backend-gstreamer
  (package
    (name "phonon-backend-gstreamer")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/phonon/"
                    name "/" version "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1wk1ip2w7fkh65zk6rilj314dna0hgsv2xhjmpr5w08xa8sii1y5"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list phonon
           qtbase-5
           qtx11extras
           gstreamer
           gst-plugins-base
           libxml2))
    (arguments
     `(#:configure-flags
       '( "-DPHONON_BUILD_PHONON4QT5=ON")))
    (home-page "https://community.kde.org/Phonon")
    (synopsis "Phonon backend which uses GStreamer")
    (description "Phonon makes use of backend libraries to provide sound.
Phonon-GStreamer is a backend based on the GStreamer multimedia library.")
    ;; license: source files mention "either version 2.1 or 3"
    (license (list license:lgpl2.1 license:lgpl3))))


;; Tier 1
;;
;; Tier 1 frameworks depend only on Qt (and possibly a small number of other
;; third-party libraries), so can easily be used by an Qt-based project.

(define-public attica
  (package
    (name "attica")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0gkdsm1vyyyxxyl4rni9s2bdz5w6zphzjl58fddjl899da06hqfq"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-network-tests
            (lambda _
              ;; These tests require network access.
              (substitute* "autotests/CMakeLists.txt"
                ((".*providertest.cpp") "")))))))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Open Collaboration Service client library")
    (description "Attica is a Qt library that implements the Open
Collaboration Services API version 1.6.

It grants easy access to the services such as querying information about
persons and contents.  The library is used in KNewStuff3 as content provider.
In order to integrate with KDE's Plasma Desktop, a platform plugin exists in
kdebase.

The REST API is defined here:
http://freedesktop.org/wiki/Specifications/open-collaboration-services/")
    (license (list license:lgpl2.1+ license:lgpl3+))))

(define-public bluez-qt
  (package
    (name "bluez-qt")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ni50jwnb5ww8mkql0p3q8660c0srj8p0ik27lvxakwdq4wf6l9s"))))
    (build-system cmake-build-system)
    (native-inputs
     (list dbus extra-cmake-modules))
    (inputs
     (list qtdeclarative-5
           qtbase-5))
    (arguments
     (list #:configure-flags
           #~(list (string-append
                    "-DUDEV_RULES_INSTALL_DIR=" #$output "/lib/udev/rules.d"))
	#:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (setenv "DBUS_FATAL_WARNINGS" "0")
                  (invoke "dbus-launch" "ctest" "-E" "bluezqt-qmltests")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "QML wrapper for BlueZ")
    (description "bluez-qt is a Qt-style library for accessing the bluez
Bluetooth stack.  It is used by the KDE Bluetooth stack, BlueDevil.")
    (license (list license:lgpl2.1+ license:lgpl3+))))

(define-public breeze-icons
  (package
    (name "breeze-icons")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/frameworks/"
                                  (version-major+minor version)
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0z5cpv10jyjdwjfkm7nj6hyp4vj29apm476hvbpb4gan27jyb91y"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules
           fdupes
           `(,gtk+ "bin")
           python
           python-lxml))                ;for 24x24 icon generation
    (inputs (list qtbase-5))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'update-cache
                          (lambda* _
                            (invoke "gtk-update-icon-cache"
                                    (string-append #$output
                                                   "/share/icons/breeze"))
                            (invoke "gtk-update-icon-cache"
                                    (string-append #$output
                                                   "/share/icons/breeze-dark")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Default KDE Plasma 5 icon theme")
    (description "Breeze provides a freedesktop.org compatible icon theme.
It is the default icon theme for the KDE Plasma 5 desktop.")
    ;; The license file mentions lgpl3+. The license files in the source
    ;; directories are lgpl3, while the top directory contains the lgpl2.1.
    ;; text.
    (license license:lgpl3+)))

(define-public kapidox
  (package
    (name "kapidox")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0xxw3lvipyax8r1af3ypwjj6waarbp2z9n11fjb4kvyigsypglmb"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f ; test need network
           #:phases #~(modify-phases %standard-phases
                        (delete 'sanity-check)))) ;its insane.
    (propagated-inputs
     ;; kapidox is a python programm
     ;; TODO: check if doxygen has to be installed, the readme does not
     ;; mention it. The openSuse .rpm lists doxygen, graphviz, graphviz-gd,
     ;; and python-xml.
     (list python python-jinja2 python-pyyaml))
    (inputs
     (list qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Doxygen Tools")
    (description "This framework contains scripts and data for building API
documentation (dox) in a standard format and style for KDE.

For the actual documentation extraction and formatting the Doxygen tool is
used, but this framework provides a wrapper script to make generating the
documentation more convenient (including reading settings from the target
framework or other module) and a standard template for the generated
documentation.")
    ;; Most parts are bsd-2, but incuded jquery is expat
    ;; This list is taken from http://packaging.neon.kde.org/cgit/
    (license (list license:bsd-2 license:expat))))

(define-public karchive
  (package
    (name "karchive")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/frameworks/"
                                  (version-major+minor version)
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "015gc1zarny8r478p7g9m6r67l5dk3r0vcp28ilmfmznxy0k0hda"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E" "karchivetest")))))))
    (native-inputs
     (list extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list bzip2 qtbase-5 xz zlib `(,zstd "lib")))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt 5 addon providing access to numerous types of archives")
    (description
     "KArchive provides classes for easy reading, creation and
manipulation of @code{archive} formats like ZIP and TAR.

It also provides transparent compression and decompression of data, like the
GZip format, via a subclass of QIODevice.")
    ;; The included licenses is are gpl2 and lgpl2.1, but the sources are
    ;; under a variety of licenses.
    ;; This list is taken from http://packaging.neon.kde.org/cgit/
    (license (list license:lgpl2.1 license:lgpl2.1+
                   license:lgpl3+ license:bsd-2))))

(define-public kcalendarcore
  (package
    (name "kcalendarcore")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0aimda01zqw4fz5ldvz4vh767bi10r00kvm62n89nxhsq46wlk7p"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules perl tzdata-for-tests))
    (inputs
     (list libical qtbase-5))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-test
            (lambda _
              ;; Reported as https://bugs.kde.org/show_bug.cgi?id=484306
              (substitute* "autotests/CMakeLists.txt"
                (("testdateserialization")
                 ""))))
          (add-before 'check 'check-setup
            (lambda* (#:key inputs #:allow-other-keys) ;;; XXX: failing test
              (setenv "QT_QPA_PLATFORM" "offscreen")
              (setenv "TZ" "Europe/Prague")
              (setenv "TZDIR"
                      (search-input-directory inputs
                                              "share/zoneinfo")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Library for interfacing with calendars")
    (description "This library provides access to and handling of calendar
data.  It supports the standard formats iCalendar and vCalendar and the group
scheduling standard iTIP.

A calendar contains information like incidences (events, to-dos, journals),
alarms, time zones, and other useful information.  This API provides access to
that calendar information via well known calendar formats iCalendar (or iCal)
and the older vCalendar.")
    (license (list license:lgpl3+ license:bsd-2))))

(define-public kcodecs
  (package
    (name "kcodecs")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "080zvcqd8iq05p5x3qaf3rryx75lg2l2j1dr18sp50ir50zfwh2w"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules gperf qttools-5))
    (inputs (list qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "String encoding and manipulating library")
    (description "KCodecs provide a collection of methods to manipulate
strings using various encodings.

It can automatically determine the charset of a string, translate XML
entities, validate email addresses, and find encodings by name in a more
tolerant way than QTextCodec (useful e.g. for data coming from the
Internet).")
    ;; The included licenses is are gpl2 and lgpl2.1, but the sources are
    ;; under a variety of licenses.
    ;; This list is taken from http://packaging.neon.kde.org/cgit/
    (license (list license:gpl2 license:gpl2+ license:bsd-2
                   license:lgpl2.1 license:lgpl2.1+ license:expat
                   license:lgpl3+ license:mpl1.1))))

(define-public kcolorpicker
  (package
    (name "kcolorpicker")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ksnip/kColorPicker")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zg078mkdapsd6vq7qy2vxbfzn6i382ss1a1w0xzvpy148ifaj42"))))
    (build-system qt-build-system)
    (propagated-inputs (list qtbase-5))
    (arguments
     (list #:configure-flags #~'("-DBUILD_TESTS=ON")))
    (home-page "https://github.com/ksnip/kColorPicker")
    (synopsis "Color Picker with popup menu")
    (description
     "@code{KColorPicker} is a subclass of @code{QToolButton} with color popup
menu which lets you select a color.  The popup features a color dialog button
which can be used to add custom colors to the popup menu.")
    (license license:lgpl3+)))

(define-public kconfig
  (package
    (name "kconfig")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0hghdh4p6cq9ckp4g5jdgd8w47pdsxxvzimrdfjrs71lmy8ydiy2"))))
    (build-system cmake-build-system)
    (native-inputs
     (list dbus extra-cmake-modules inetutils qttools-5
           xorg-server-for-tests))
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests? ;; kconfigcore-kconfigtest fails inconsistently!!
               (setenv "HOME" (getcwd))
               (setenv "QT_QPA_PLATFORM" "offscreen")
               (invoke "ctest" "-E" "(kconfigcore-kconfigtest|\
kconfiggui-kstandardshortcutwatchertest)")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Kconfiguration settings framework for Qt")
    (description "KConfig provides an advanced configuration system.
It is made of two parts: KConfigCore and KConfigGui.

KConfigCore provides access to the configuration files themselves.
It features:

@enumerate
@item Code generation: describe your configuration in an XML file, and use
`kconfig_compiler to generate classes that read and write configuration
entries.

@item Cascading configuration files (global settings overridden by local
settings).

@item Optional shell expansion support (see docs/options.md).

@item The ability to lock down configuration options (see docs/options.md).
@end enumerate

KConfigGui provides a way to hook widgets to the configuration so that they
are automatically initialized from the configuration and automatically
propagate their changes to their respective configuration files.")
    ;; The included licenses is are gpl2 and lgpl2.1, but the sources are
    ;; under a variety of licenses.
    ;; This list is taken from http://packaging.neon.kde.org/cgit/
    (license (list license:lgpl2.1 license:lgpl2.1+ license:expat
                   license:lgpl3+ license:gpl1 ; licende:mit-olif
                   license:bsd-2 license:bsd-3))))

(define-public kcoreaddons
  (package
    (name "kcoreaddons")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1wv3s3xsiii96k17nzs2fb0ih2lyg52krf58v44nlk9wfi4wmnqx"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5 shared-mime-info))
    ;; TODO: FAM: File alteration notification http://oss.sgi.com/projects/fam
    (inputs
     (list qtbase-5))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'blacklist-failing-test
            (lambda _
              ;; Blacklist failing tests.
              (with-output-to-file "autotests/BLACKLIST"
                (lambda _
                  ;; FIXME: Make it pass.  Test failure caused by stout/stderr
                  ;; being interleaved.
                  (display "[test_channels]\n*\n")
                  ;; FIXME
                  (display "[test_inheritance]\n*\n")))))
          (add-before 'check 'check-setup
            (lambda _
              (setenv "HOME" (getcwd))
              (setenv "TMPDIR" (getcwd)))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt addon library with a collection of non-GUI utilities")
    (description "KCoreAddons provides classes built on top of QtCore to
perform various tasks such as manipulating mime types, autosaving files,
creating backup files, generating random sequences, performing text
manipulations such as macro replacement, accessing user information and
many more.")
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kdbusaddons
  (package
    (name "kdbusaddons")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0pzzznyxhi48z5hhdsdxz3vaaihrdshpx65ha2v2nn2gh3ww7ikm"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules dbus qttools-5))
    (inputs
     (list qtbase-5 qtx11extras kinit-bootstrap))
    ;; kinit-bootstrap: kinit package which does not depend on kdbusaddons.
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'patch-source
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; look for the kdeinit5 executable in kinit's store directory,
                   ;; instead of the current application's directory:
                   (substitute* "src/kdeinitinterface.cpp"
                     (("<< QCoreApplication::applicationDirPath..")
                      (string-append
                       "<< QString::fromUtf8(\"/"
                       (dirname (search-input-file inputs "bin/kdeinit5"))
                       "\")" )))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "DBUS_FATAL_WARNINGS" "0")
                     (invoke "dbus-launch" "ctest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Convenience classes for DBus")
    (description "KDBusAddons provides convenience classes on top of QtDBus,
as well as an API to create KDED modules.")
    ;; Some source files mention lgpl2.0+, but the included license is
    ;; the lgpl2.1. Some source files are under non-copyleft licenses.
    (license license:lgpl2.1+)))

(define-public kdnssd
  (package
    (name "kdnssd")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zw5rkprr54j05ic8zljk57zahp2v6333slr253r3n1679zqlv64"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list avahi ; alternativly dnssd could be used
           qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Network service discovery using Zeroconf")
    (description "KDNSSD is a library for handling the DNS-based Service
Discovery Protocol (DNS-SD), the layer of Zeroconf that allows network services,
such as printers, to be discovered without any user intervention or centralized
infrastructure.")
    (license license:lgpl2.1+)))

(define-public kgraphviewer
  (package
    (name "kgraphviewer")
    (version "2.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/kgraphviewer/"
                    version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1h6pgg89gvxl8gw7wmkabyqqrzad5pxyv5lsmn1fl4ir8lcc5q2l"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase-5
           boost
           graphviz
           kiconthemes
           kparts
           qtsvg-5))
    (native-inputs
     (list pkg-config extra-cmake-modules kdoctools))
    (home-page "https://apps.kde.org/kgraphviewer/")
    (synopsis "Graphviz dot graph viewer for KDE")
    (description "KGraphViewer is a Graphviz DOT graph file viewer, aimed to
replace the other outdated Graphviz tools.")
    (license license:gpl2+)))

(define-public kguiaddons
  (package
    (name "kguiaddons")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0riya9plcz9c1ndhdbsradssndshbm12705swn7vf7am17n7f947"))))
    (build-system qt-build-system)
    ;; TODO: Build packages for the Python bindings.  Ideally this will be
    ;; done for all versions of python guix supports.  Requires python,
    ;; python-sip, clang-python, libclang.  Requires python-2 in all cases for
    ;; clang-python.
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list qtbase-5 qtwayland-5 qtx11extras plasma-wayland-protocols wayland))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Utilities for graphical user interfaces")
    (description "The KDE GUI addons provide utilities for graphical user
interfaces in the areas of colors, fonts, text, images, keyboard input.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kholidays
  (package
    (name "kholidays")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
       (sha256
        (base32 "19r8dxglz5ll6iyvigsccil3ikvcsnyy5nwcpjvjr1c0brigcjmy"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (home-page "https://invent.kde.org/frameworks/kholidays")
    (synopsis "Library for regional holiday information")
    (description "This library provides a C++ API that determines holiday and
other special events for a geographical region.")
    (license license:lgpl2.0+)))

(define-public ki18n
  (package
    (name "ki18n")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1yg03awcx5ay6lgbgwv91i0ankrm94z9m0wky4v03gnwnvw8pa0v"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list gettext-minimal python))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list qtbase-5 qtdeclarative-5 qtscript iso-codes))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME"
                                      (getcwd))
                              (invoke "ctest" "-E"
                               "(kcountrytest|kcountrysubdivisiontest)")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Gettext-based UI text internationalization")
    (description "KI18n provides functionality for internationalizing user
interface text in applications, based on the GNU Gettext translation system.  It
wraps the standard Gettext functionality, so that the programmers and translators
can use the familiar Gettext tools and workflows.

KI18n provides additional functionality as well, for both programmers and
translators, which can help to achieve a higher overall quality of source and
translated text.  This includes argument capturing, customizable markup, and
translation scripting.")
    (license license:lgpl2.1+)))

(define-public kidletime
  (package
    (name "kidletime")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "06sc9w54g4n7s5gjkqz08rgcz6v3pr0bdgx3gbjgzass6l4m8w7p"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list libxscrnsaver ; X-Screensaver based poller, fallback mode
           qtbase-5 qtx11extras))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Reporting of idle time of user and system")
    (description "KIdleTime is a singleton reporting information on idle time.
It is useful not only for finding out about the current idle time of the PC,
but also for getting notified upon idle time events, such as custom timeouts,
or user activity.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kirigami
  ;; Kirigami is listed as tier 1 framework, but optionally includes
  ;; plasma-framework which is tier 3.
  (package
    (name "kirigami")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    "kirigami2-" version ".tar.xz"))
              (sha256
               (base32
                "1bd232gs4394fa3aq31mjqrn8f3vjsghx7817szi7ryvnn6fnqkw"))))
    (properties `((upstream-name . "kirigami2")))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list kwindowsystem
           ;; TODO: Find a way to activate this optional include without
           ;; introducing a recursive dependency.
           ;;("plasma-frameworks" ,plasma-framework) ;; Tier 3!
           qtbase-5
           qtdeclarative-5
           qtquickcontrols2-5
           qtsvg-5
           ;; Run-time dependency
           qtgraphicaleffects))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "QtQuick components for mobile user interfaces")
    (description "Kirigami is a set of high level QtQuick components looking
and feeling well on both mobile and desktop devices.  They ease the creation
of applications that follow the Kirigami Human Interface Guidelines.")
    (license license:lgpl2.1+)))

(define-public kitemmodels
  (package
    (name "kitemmodels")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1bfmcrbcbrvp2rcaf32vzvarqwp41gn6s4xpf56hnxbwf9kgk1fl"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Set of item models extending the Qt model-view framework")
    (description "KItemModels provides the following models:

@enumerate
@item KBreadcrumbSelectionModel - Selects the parents of selected items to
create breadcrumbs.

@item KCheckableProxyModel - Adds a checkable capability to a source model.

@item KConcatenateRowsProxyModel - Concatenates rows from multiple source models.

@item KDescendantsProxyModel - Proxy Model for restructuring a Tree into a list.

@item KExtraColumnsProxyModel - Adds columns after existing columns.

@item KLinkItemSelectionModel - Share a selection in multiple views which do
not have the same source model.

@item KModelIndexProxyMapper - Mapping of indexes and selections through proxy
models.

@item KRearrangeColumnsProxyModel - Can reorder and hide columns from the source
model.

@item KRecursiveFilterProxyModel - Recursive filtering of models.

@item KSelectionProxyModel - A Proxy Model which presents a subset of its source
model to observers
@end enumerate")
    (license license:lgpl2.1+)))

(define-public kitemviews
  (package
    (name "kitemviews")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "00vl2ck0pq0sqcxvhlr2pimgr27hd9v7y9dz6w4arb5smi5q1ixg"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Set of item views extending the Qt model-view framework")
    (description "KItemViews includes a set of views, which can be used with
item models.  It includes views for categorizing lists and to add search filters
to flat and hierarchical lists.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kplotting
  (package
    (name "kplotting")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "17x58pplln0plqiyhjpzdiqxngylxq5gkc5gk7b91xzm783x2k0n"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Data plotting library")
    (description "KPlotWidget is a QWidget-derived class that provides a virtual
base class for easy data-plotting.  The idea behind KPlotWidget is that you only
have to specify information in \"data units\", the natural units of the
data being plotted.  KPlotWidget automatically converts everything to screen
pixel units.")
    (license license:lgpl2.1+)))

(define-public ksyntaxhighlighting
  (package
    (name "ksyntaxhighlighting")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    "syntax-highlighting-" version ".tar.xz"))
              (sha256
               (base32
                "1skblg2m0sar63qrgkjsg0w9scixggm5qj7lp4gzjn4hwq6m3n63"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules perl qttools-5
           ;; Optional, for compile-time validation of syntax definition files:
           qtxmlpatterns))
    (inputs
     (list qtbase-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'unpatch-source-shebang
           (lambda _
             ;; revert the patch-shebang phase on scripts which are
             ;; in fact test data
             (substitute* '("autotests/input/highlight.sh"
                            "autotests/folding/highlight.sh.fold")
               (((which "sh")) " /bin/sh")) ;; space in front!
             (substitute* '("autotests/input/highlight.pl"
                            "autotests/folding/highlight.pl.fold")
               (((which "perl")) "/usr/bin/perl")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Syntax highlighting engine for Kate syntax definitions")
    (description "This is a stand-alone implementation of the Kate syntax
highlighting engine.  It's meant as a building block for text editors as well
as for simple highlighted text rendering (e.g. as HTML), supporting both
integration with a custom editor as well as a ready-to-use
@code{QSyntaxHighlighter} sub-class.")
    (properties `((upstream-name . "syntax-highlighting")))
    (license license:lgpl2.1+)))

(define-public plasma-wayland-protocols
  (package
    (name "plasma-wayland-protocols")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/" name "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "17jg1nvpq0718wv3nc54kd7pcn4riz2p2pcvay1kr4fbg79bz0ql"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (arguments '(#:tests? #f))          ;no tests
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Plasma Wayland Protocols")
    (description
     "This package contains XML files describing non-standard Wayland
protocols used in KDE Plasma.")
    ;; The XML files have varying licenses, open them for details.
    (license (list license:bsd-3
                   license:lgpl2.1+
                   license:expat))))

(define-public kwayland
  (package
    (name "kwayland")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1lzmlbv5vl656cigjj07hbc0gj6g1i2xqanvnhxj360109kzilf1"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list libxkbcommon
           plasma-wayland-protocols
           qtbase-5
           qtwayland-5
           wayland
           wayland-protocols))
    (arguments
     (list
      ;; Tests spawn Wayland sessions that cannot run in parallel.
      #:parallel-tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-XDG_RUNTIME_DIR
            (lambda _
              (setenv "XDG_RUNTIME_DIR" (getcwd))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ctest" "-E"
                        ;; XXX: maybe is upstream bug
                        "(kwayland-testWaylandRegistry|\
kwayland-testPlasmaShell|kwayland-testPlasmaWindowModel)")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt-style API to interact with the wayland client and server")
    (description "As the names suggest they implement a Client respectively a
Server API for the Wayland protocol.  The API is Qt-styled removing the needs to
interact with a for a Qt developer uncomfortable low-level C-API.  For example
the callback mechanism from the Wayland API is replaced by signals, data types
are adjusted to be what a Qt developer expects - two arguments of int are
represented by a QPoint or a QSize.")
    (license license:lgpl2.1+)))

(define-public kwidgetsaddons
  (package
    (name "kwidgetsaddons")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1cc8lsk9v0cp2wiy1q26mlkf8np0yj01sq8a7w13ga5s6hv4sh2n"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5 xorg-server-for-tests))
    (inputs
     (list qtbase-5))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "XDG_CACHE_HOME" "/tmp/xdg-cache")
                (invoke "ctest" "-E"
                        "(ksqueezedtextlabelautotest|\
kwidgetsaddons-kcolumnresizertest)")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Large set of desktop widgets")
    (description "Provided are action classes that can be added to toolbars or
menus, a wide range of widgets for selecting characters, fonts, colors, actions,
dates and times, or MIME types, as well as platform-aware dialogs for
configuration pages, message boxes, and password requests.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kwindowsystem
  (package
    (name "kwindowsystem")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "03xbsf1pmswd2kpn3pdszp4vndclsh7j02fp22npxaxllmfr4va9"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules
           pkg-config
           dbus ; for the tests
           openbox ; for the tests
           qttools-5
           xorg-server-for-tests)) ; for the tests
    (inputs
     (list libxrender
           qtbase-5
           qtx11extras
           xcb-util-keysyms
           xcb-util-wm))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              ;; The test suite requires a running window anager
              (when tests?
                (setenv "XDG_RUNTIME_DIR" "/tmp")
                (system "Xvfb :1 -ac -screen 0 640x480x24 &")
                (setenv "DISPLAY" ":1")
                (sleep 5) ;; Give Xvfb a few moments to get on it's feet
                (system "openbox &")
                (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
                (setenv "DBUS_FATAL_WARNINGS" "0")
                (invoke "dbus-launch" "ctest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE access to the windowing system")
    (description "KWindowSystem provides information about and allows
interaction with the windowing system.  It provides a high level API, which
is windowing system independent and has platform specific
implementations.  This API is inspired by X11 and thus not all functionality
is available on all windowing systems.

In addition to the high level API, this framework also provides several
lower level classes for interaction with the X Windowing System.")
    ;; Some source files mention lgpl2.0+, but the included license is
    ;; the lgpl2.1. Some source files are under non-copyleft licenses.
    (license license:lgpl2.1+)))

(define-public modemmanager-qt
  (package
    (name "modemmanager-qt")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "16jqhmcpsffl9a7c0bb4hwjy3bw5rakdsnc5n6y8djc6237jl9pi"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules dbus pkg-config))
    (propagated-inputs
     ;; Headers contain #include <ModemManager/ModemManager.h>
     (list modem-manager))
    (inputs
     (list qtbase-5))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "DBUS_FATAL_WARNINGS" "0")
                              (invoke "dbus-launch" "ctest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt wrapper for ModemManager DBus API")
    (description "ModemManagerQt provides access to all ModemManager features
exposed on DBus.  It allows you to manage modem devices and access to
information available for your modem devices, like signal, location and
messages.")
    (license license:lgpl2.1+)))

(define-public networkmanager-qt
  (package
    (name "networkmanager-qt")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "10anjsnrzawrfjlznjvvl2sbxrajl2ddnq2kgl314b5dk7z3yk4n"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules dbus pkg-config))
    (propagated-inputs
     ;; Headers contain #include <NetworkManager.h> and
     ;;                 #include <libnm/NetworkManager.h>
     (list network-manager))
    (inputs
     (list qtbase-5))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "DBUS_FATAL_WARNINGS" "0")
                              (invoke "dbus-launch" "ctest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt wrapper for NetworkManager DBus API")
    (description "NetworkManagerQt provides access to all NetworkManager
features exposed on DBus.  It allows you to manage your connections and control
your network devices and also provides a library for parsing connection settings
which are used in DBus communication.")
    (license license:lgpl2.1+)))

(define-public oxygen-icons
  (package
    (name "oxygen-icons")
    (version "5.112.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "5" "-" version ".tar.xz"))
              (sha256
               (base32
                "0yw2mixy5p8pw9866rfr0wcjhvilznakd0h6934svv0dk3lv054a"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules fdupes))
    (inputs
     (list qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen provides the standard icon theme for the KDE desktop")
    (description "Oxygen icon theme for the KDE desktop")
    (license license:lgpl3+)
    (properties '((upstream-name . "oxygen-icons5")))))

(define-public prison
  (package
    (name "prison")
    (version "5.114.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "1wbr1lryxmrx65ilq1bhqsdhhikrih977nhpb02fq0cqnvv7v9i7"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list libdmtx qrencode qtbase-5)) ;; TODO: rethink: nix propagates this
    (home-page "https://api.kde.org/frameworks/prison/html/index.html")
    (synopsis "Barcode generation abstraction layer")
    (description "Prison is a Qt-based barcode abstraction layer/library and
provides uniform access to generation of barcodes with data.")
    (license license:lgpl2.1+)))

(define-public pulseaudio-qt
  (package
    (name "pulseaudio-qt")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/pulseaudio-qt"
                                  "/pulseaudio-qt-" version ".tar.xz"))
              (sha256
               (base32
                "0rfh1qawf2bxh26p5xr03z2zizpnfw8k9ycwhmmqac11rx765jnq"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list glib pulseaudio qtdeclarative-5 qtbase-5))
    (home-page "https://invent.kde.org/libraries/pulseaudio-qt/")
    (synopsis "Qt bindings for PulseAudio")
    (description
     "pulseaudio-qt is a Qt-style wrapper for libpulse.  It allows querying
and manipulation of various PulseAudio objects such as @code{Sinks},
@code{Sources} and @code{Streams}.  It does not wrap the full feature set of
libpulse.")
    ;; User can choose between LGPL version 2.1 or 3.0; or
    ;; "any later version accepted by the membership of KDE e.V".
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public qqc2-desktop-style
  (package
    (name "qqc2-desktop-style")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1y5g91vybjvhwmzpfwrc70q5j7jxf5b972f9fh2vzb930jir6c8g"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kauth
           kconfigwidgets ; optional
           kcoreaddons
           kiconthemes ; optional
           kirigami
           qtbase-5
           qtdeclarative-5
           qtquickcontrols2-5
           qtx11extras ; optional
           sonnet)) ; optional
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "QtQuickControls2 style that integrates with the desktop")
    (description "This is a style for QtQuickControls2 which is using
QWidget's QStyle to paint the controls in order to give it a native look and
feel.")
    ;; Mostly LGPL 2+, but many files are dual-licensed
    (license (list license:lgpl2.1+ license:gpl3+))))

(define-public solid
  (package
    (name "solid")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1slxlj5jhp8g745l328932934633nl81sq3n8fd73h655hymsk4s"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "DBUS_FATAL_WARNINGS" "0")
                              (invoke "dbus-launch" "ctest")))))))
    (native-inputs
     (list bison dbus extra-cmake-modules flex qttools-5))
    (inputs
     (list qtbase-5 qtdeclarative-5 eudev))
    ;; TODO: Add runtime-only dependency MediaPlayerInfo
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Desktop hardware abstraction")
    (description "Solid is a device integration framework.  It provides a way of
querying and interacting with hardware independently of the underlying operating
system.")
    (license license:lgpl2.1+)))

(define-public sonnet
  (package
    (name "sonnet")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0zxi96i3gfpx759qc1nyz7jqlswg5ivgr1w9gbbsm1x5fi9ikadx"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list aspell
           hunspell
           ;; TODO: hspell (for Hebrew), Voikko (for Finish)
           qtdeclarative-5
           qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Multi-language spell checker")
    (description "Sonnet is a plugin-based spell checking library for Qt-based
applications.  It supports several different plugins, including HSpell, Enchant,
ASpell and HUNSPELL.")
    (license license:lgpl2.1+)))

(define-public threadweaver
  (package
    (name "threadweaver")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1y07g58w6z3i11y3djg3aaxanhp9hzaciq61l4dn1gqwghn09xgh"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Helper for multithreaded programming")
    (description "ThreadWeaver is a helper for multithreaded programming.  It
uses a job-based interface to queue tasks and execute them in an efficient way.")
    (license license:lgpl2.1+)))

(define-public libkdcraw
  (package
    (name "libkdcraw")
    (version "23.08.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1mm3gsp7lfqxb9irk59hrzaxdqjv28iwaa1xmpazw4q62nmlj7mi"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config extra-cmake-modules))
    (inputs
     (list libraw qtbase-5))
    (home-page "https://invent.kde.org/graphics/libkdcraw")
    (synopsis "C++ interface used to decode RAW picture files")
    (description "Libkdcraw is a C++ interface around LibRaw library used to
decode RAW picture files.")
    (license (list license:gpl2+ license:bsd-3))))

;; Tier 2
;;
;; Tier 2 frameworks additionally depend on tier 1 frameworks, but still have
;; easily manageable dependencies.

(define-public kactivities
  (package
    (name "kactivities")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "10pyynqz8c22la9aqms080iqlisj3irbi1kwnn3s0vg5dsjxr1p3"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list boost
           kauth
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kio
           kitemviews
           kjobwidgets
           kservice
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           qtbase-5
           qtdeclarative-5
           solid))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Core components for the KDE Activity concept")
    (description "KActivities provides the infrastructure needed to manage a
user's activities, allowing them to switch between tasks, and for applications
to update their state to match the user's current activity.  This includes a
daemon, a library for interacting with that daemon, and plugins for integration
with other frameworks.")
    ;; triple licensed
    (license (list license:gpl2+ license:lgpl2.0+ license:lgpl2.1+))))

(define-public kauth
  (package
    (name "kauth")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rkf9mc9718wn8pzd3d3wcg3lsn0vkr9a2cqnz86rbg3cf2qdbir"))))
    (build-system cmake-build-system)
    (native-inputs
     (list dbus extra-cmake-modules qttools-5))
    (inputs
     (list kcoreaddons polkit-qt qtbase-5))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cmake-install-directories
            (lambda _
              ;; Make packages using kauth put their policy files and helpers
              ;; into their own prefix.
              (substitute* "KF5AuthConfig.cmake.in"
                (("@KAUTH_POLICY_FILES_INSTALL_DIR@")
                 "${KDE_INSTALL_DATADIR}/polkit-1/actions")
                (("@KAUTH_HELPER_INSTALL_DIR@")
                 "${KDE_INSTALL_LIBEXECDIR}")
                (("@KAUTH_HELPER_INSTALL_ABSOLUTE_DIR@")
                 "${KDE_INSTALL_LIBEXECDIR}"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "DBUS_FATAL_WARNINGS" "0")
                (invoke "dbus-launch" "ctest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Execute actions as privileged user")
    (description "KAuth provides a convenient, system-integrated way to offload
actions that need to be performed as a privileged user to small set of helper
utilities.")
    (license license:lgpl2.1+)))

(define-public kcompletion
  (package
    (name "kcompletion")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0qvdxqlh1dklkbmqfjg5gc3dkdicgzn6q5lgvyf8cv46dinj6mwc"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list kconfig kwidgetsaddons qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Powerful autocompletion framework and widgets")
    (description "This framework helps implement autocompletion in Qt-based
applications.  It provides a set of completion-ready widgets, or can be
integrated it into your application's other widgets.")
    (license license:lgpl2.1+)))

(define-public kcontacts
  (package
    (name "kcontacts")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (patches
               (search-patches "kcontacts-incorrect-country-name.patch"))
              (sha256
               (base32
                "0lyqvbs216p5zpssaf4pyccph7nbwkbvhpmhbi32y2rm23cmxlwf"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules xorg-server-for-tests)) ; for the tests
    (inputs
     (list qtbase-5))
    (propagated-inputs
     (list ;; As required by KF5ContactsConfig.cmake.
      iso-codes kcodecs kconfig kcoreaddons qtdeclarative-5 ki18n))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda _
              (setenv "HOME" (getcwd))
              (system "Xvfb +extension GLX :1 -screen 0 640x480x24 &")
              (setenv "DISPLAY" ":1"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "API for contacts/address book data following the vCard standard")
    (description "This library provides a vCard data model, vCard
input/output, contact group management, locale-aware address formatting, and
localized country name to ISO 3166-1 alpha 2 code mapping and vice verca.
")
    (license license:lgpl2.1+)))

(define-public kcrash
  (package
    (name "kcrash")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1avi4yd3kpjqxrvci1nicxbh9mjafj1w2vgfmqanq66b76s4kxj1"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcoreaddons kwindowsystem qtbase-5 qtx11extras))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Graceful handling of application crashes")
    (description "KCrash provides support for intercepting and handling
application crashes.")
    (license license:lgpl2.1+)))

(define-public kdoctools
  (package
    (name "kdoctools")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "15s58r2zvdckw30x9q9ir8h1i8q2ncfgjn9h4jnmylwm79z3z27v"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list docbook-xml-4.5
           docbook-xsl
           karchive
           ki18n
           libxml2
           libxslt
           perl
           perl-uri
           qtbase-5))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'cmake-find-docbook
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (find-files "cmake" "\\.cmake$")
                (("CMAKE_SYSTEM_PREFIX_PATH")
                 "CMAKE_PREFIX_PATH"))
              (substitute* "cmake/FindDocBookXML4.cmake"
                (("^.*xml/docbook/schema/dtd.*$")
                 "xml/dtd/docbook\n"))
              (substitute* "cmake/FindDocBookXSL.cmake"
                (("^.*xml/docbook/stylesheet.*$")
                 (string-append "xml/xsl/docbook-xsl-"
                                #$(package-version docbook-xsl)
                                "\n")))))
          (add-after 'install 'add-symlinks
            ;; Some package(s) (e.g. kdelibs4support) refer to this locale by a
            ;; different spelling.
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((xsl (string-append (assoc-ref outputs "out")
                                        "/share/kf5/kdoctools/customization/xsl/")))
                (symlink (string-append xsl "pt_br.xml")
                         (string-append xsl "pt-BR.xml"))))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Create documentation from DocBook")
    (description "Provides tools to generate documentation in various format
from DocBook files.")
    (license license:lgpl2.1+)))

(define-public kfilemetadata
  (package
    (name "kfilemetadata")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "15va29chlsrxii02w1ax718hp1b14ym59lcfyzh7w30zlf681560"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; FIXME: Test can't find audio/x-speex mimeinfo
                ;; (but it can find audio/x-speex+ogg).
                (invoke "ctest" "-E"
                        "(usermetadatawritertest|embeddedimagedatatest|\
taglibextractortest)")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs
     (list attr
           ebook-tools
           karchive
           kconfig
           kcoreaddons
           kdegraphics-mobipocket
           ki18n
           qtmultimedia-5
           qtbase-5
           ;; Required run-time packages
           catdoc
           ;; Optional run-time packages
           exiv2
           ffmpeg
           poppler-qt5
           taglib))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Extract metadata from different fileformats")
    (description "KFileMetaData provides a simple library for extracting the
text and metadata from a number of different files.  This library is typically
used by file indexers to retrieve the metadata.  This library can also be used
by applications to write metadata.")
    (license (list license:lgpl2.0 license:lgpl2.1 license:lgpl3))))

(define-public kimageannotator
  (package
    (name "kimageannotator")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ksnip/kImageAnnotator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ac0cxbq88zzvkgs5d39xpzk0h7xhl57yswgd760x77py0rl5bqf"))))
    (build-system qt-build-system)
    (arguments
     (list #:configure-flags #~'("-DBUILD_TESTS=ON")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda _
                   ;; 1 test requires a running X server, it calls
                   ;; 'XCloseDisplay'.
                   (system "Xvfb :1 -screen 0 640x480x24 &")
                   (setenv "DISPLAY" ":1")
                   (invoke "ctest" "--test-dir" "tests"))))))
    (native-inputs
     (list qttools-5 xorg-server-for-tests))
    (inputs
     (list googletest qtsvg-5 kcolorpicker))
    (propagated-inputs
     (list qtbase-5))
    (home-page "https://github.com/ksnip/kImageAnnotator")
    (synopsis "Image annotating library")
    (description "This library provides tools to annotate images.")
    (license license:lgpl3+)))

(define-public kimageformats
  (package
    (name "kimageformats")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1nfzpgnrbwncx9zp9cwa169jlfv7i85p00a07d4jc5hrdyvvkn0w"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list karchive ; for Krita and OpenRaster images
           openexr-2 ; for OpenEXR high dynamic-range images
           qtbase-5))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda _
              ;; make Qt render "offscreen", required for tests
              (setenv "QT_QPA_PLATFORM" "offscreen")
              (setenv "QT_PLUGIN_PATH"
                      (string-append (getcwd) "/bin:"
                                     (getenv "QT_PLUGIN_PATH")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME"
                        (getcwd))
                ;; FIXME: I guess kde's qt no this fail.
                ;; see https://invent.kde.org/frameworks/kimageformats/-/jobs/1046283
                (invoke "ctest" "-E" "kimageformats-read-psd")))))
      ;; FIXME: The header files of ilmbase (propagated by openexr) are not
      ;; found when included by the header files of openexr, and an explicit
      ;; flag needs to be set.
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_FLAGS=-I"
                                               (assoc-ref %build-inputs
                                                          "ilmbase")
                                               "/include/OpenEXR"))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Plugins to allow QImage to support extra file formats")
    (description "This framework provides additional image format plugins for
QtGui.  As such it is not required for the compilation of any other software,
but may be a runtime requirement for Qt-based software to support certain image
formats.")
    (license license:lgpl2.1+)))

(define-public kjobwidgets
  (package
    (name "kjobwidgets")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ymlqi5cqcs79nj1vff8pqwgvy0dxj5vv7l529w3a3n315hkrny8"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list kcoreaddons kwidgetsaddons qtbase-5 qtx11extras))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Widgets for showing progress of asynchronous jobs")
    (description "KJobWIdgets provides widgets for showing progress of
asynchronous jobs.")
    (license license:lgpl2.1+)))

(define-public knotifications
  (package
    (name "knotifications")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0cjd5ml9hyzprjgmrc132cmp7g9hnl0h5swlxw2ifqnxxyfkg72b"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules dbus pkg-config qttools-5))
    (inputs
     (list kcodecs
           kconfig
           kcoreaddons
           kwindowsystem
           libcanberra
           libdbusmenu-qt
           phonon
           qtdeclarative-5
           qtbase-5
           qtspeech-5
           qtx11extras))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME"
                                      (getcwd))
                              (setenv "DBUS_FATAL_WARNINGS" "0")
                              (invoke "dbus-launch" "ctest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Desktop notifications")
    (description "KNotification is used to notify the user of an event.  It
covers feedback and persistent events.")
    (license license:lgpl2.1+)))

(define-public kpackage
  (package
    (name "kpackage")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0v165az3k5lfszxy0kl2464573y0dcq92fyfiklwnkkcjsvba69d"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list karchive
           kconfig
           kcoreaddons
           kdoctools
           ki18n
           qtbase-5))
    (arguments
     (list
      ;; The `plasma-querytest' test is known to fail when tests are run in parallel:
      ;; <https://sources.debian.org/src/kpackage/5.107.0-1/debian/changelog/#L92>
      #:parallel-tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "src/kpackage/package.cpp"
                (("externalPaths.false.")
                 "externalPaths(true)"))
              ;; Make QDirIterator follow symlinks
              (substitute* '("src/kpackage/packageloader.cpp")
                (("^\\s*(const QDirIterator::IteratorFlags flags = QDirIterator::Subdirectories)(;)"
                  _ a b)
                 (string-append a " | QDirIterator::FollowSymlinks" b))
                (("^\\s*(QDirIterator it\\(.*, QDirIterator::Subdirectories)(\\);)"
                  _ a b)
                 (string-append a " | QDirIterator::FollowSymlinks" b)))))
          (add-after 'unpack 'patch-tests
            (lambda _
              ;; /bin/ls doesn't exist in the build-container use /etc/passwd
              (substitute* "autotests/packagestructuretest.cpp"
                (("(addDirectoryDefinition\\(\")bin(\".*\")bin(\".*\")bin\""
                  _ a b c)
                 (string-append a "etc" b "etc" c "etc\""))
                (("filePath\\(\"bin\", QStringLiteral\\(\"ls\"))")
                 "filePath(\"etc\", QStringLiteral(\"passwd\"))")
                (("\"/bin/ls\"")
                 "\"/etc/passwd\""))))
          (add-before 'check 'check-setup
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Installation and loading of additional content as packages")
    (description "The Package framework lets the user install and load packages
of non binary content such as scripted extensions or graphic assets, as if they
were traditional plugins.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kpty
  (package
    (name "kpty")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0fm7bfp89kvg1a64q8piiyal71p6vjnqcm13zak6r9fbfwcm0gs9"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcoreaddons ki18n
           ;; TODO: utempter, for managing UTMP entries
           qtbase-5))
    (arguments
     (list #:tests? #f ; FIXME: 1/1 tests fail.
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-tests
                          (lambda _
                            (substitute* "autotests/kptyprocesstest.cpp"
                              (("/bin/bash")
                               (which "bash"))))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Interfacing with pseudo terminal devices")
    (description "This library provides primitives to interface with pseudo
terminal devices as well as a KProcess derived class for running child processes
and communicating with them using a pty.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kunitconversion
  (package
    (name "kunitconversion")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1qyqvl8fy105zwma5nrkz9zg5932w2f33daw0azhj322iffrm39n"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ;; Requires network.
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list ki18n qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Converting physical units")
    (description "KUnitConversion provides functions to convert values in
different physical units.  It supports converting different prefixes (e.g. kilo,
mega, giga) as well as converting between different unit systems (e.g. liters,
gallons).")
    (license license:lgpl2.1+)))

(define-public syndication
  (package
    (name "syndication")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "13rjb1zm9yd8vbm9h7avqih5v0rr2srqwglm29l7mcnankqlh4n7"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcodecs qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "RSS/Atom parser library")
    (description "@code{syndication} supports RSS (0.9/1.0, 0.91..2.0) and
Atom (0.3 and 1.0) feeds.  The library offers a unified, format-agnostic view
on the parsed feed, so that the using application does not need to distinguish
between feed formats.")
    (license license:lgpl2.1+)))


;; Tier 3
;;
;; Tier 3 frameworks are generally more powerful, comprehensive packages, and
;; consequently have more complex dependencies.

(define-public baloo
  (package
    (name "baloo")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "19sib1y0m5h2gnnpr9rfk810p6pdfm4zzxlm0a44r7910llp8i50"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kcoreaddons kfilemetadata))
    (native-inputs
     (list dbus extra-cmake-modules))
    (inputs
     (list kbookmarks
           kcompletion
           kconfig
           kcrash
           kdbusaddons
           kidletime
           kio
           kitemviews
           ki18n
           kjobwidgets
           kservice
           kwidgetsaddons
           kxmlgui
           lmdb
           qtbase-5
           qtdeclarative-5
           solid))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "DBUS_FATAL_WARNINGS" "0")
                              (setenv "HOME"
                                      (getcwd))
                              (invoke "dbus-launch" "ctest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "File searching and indexing")
    (description "Baloo provides file searching and indexing.  It does so by
maintaining an index of the contents of your files.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kactivities-stats
  (package
    (name "kactivities-stats")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zhrs2p3c831rwx7ww87i82k5i236vfywdxv7zhz93k3vffyqby7"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list boost kactivities kconfig qtbase-5 qtdeclarative-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Access usage statistics collected by the activity manager")
    (description "The KActivitiesStats library provides a querying mechanism for
the data that the activity manager collects---which documents have been opened
by which applications, and what documents have been linked to which activity.")
    ;; triple licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+ license:lgpl3+))))

(define-public kbookmarks
  (package
    (name "kbookmarks")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "06lnsyjhh80mdcqjww40glinmrjydbmkhv27a267vf34r7kam9rc"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kwidgetsaddons))
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list kauth
           kcodecs
           kconfig
           kconfigwidgets
           kcoreaddons
           kiconthemes
           kxmlgui
           qtbase-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Bookmarks management library")
    (description "KBookmarks lets you access and manipulate bookmarks stored
using the XBEL format.")
    (license license:lgpl2.1+)))

(define-public kcmutils
  (package
    (name "kcmutils")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1pblf3c60m0gn3vhdprw28f8y54kij02jwz91r2vnmng8d1xkrp9"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kconfigwidgets kservice))
    (native-inputs
     (list extra-cmake-modules))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "src/kpluginselector.cpp"
                ;; make QDirIterator follow symlinks
                (("^\\s*(QDirIterator it\\(.*, QDirIterator::Subdirectories)(\\);)"
                  _ a b)
                 (string-append a
                                " | QDirIterator::FollowSymlinks" b)))
              (substitute* "src/kcmoduleloader.cpp"
                ;; print plugin name when loading fails
                (("^\\s*(qWarning\\(\\) << \"Error loading) (plugin:\")( << loader\\.errorString\\(\\);)"
                  _ a b c)
                 (string-append a
                                " KCM plugin\" << mod.service()->library() << \":\""
                                c)))))
          (add-before 'check 'check-setup
            (lambda _
              (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (inputs
     (list kauth
           kcodecs
           kconfig
           kcoreaddons
           kdeclarative
           kguiaddons
           kiconthemes
           kitemviews
           ki18n
           kpackage
           kwidgetsaddons
           kxmlgui
           qtbase-5
           qtdeclarative-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Utilities for KDE System Settings modules")
    (description "KCMUtils provides various classes to work with KCModules.
KCModules can be created with the KConfigWidgets framework.")
    (license license:lgpl2.1+)))

(define-public kconfigwidgets
  (package
    (name "kconfigwidgets")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "16layydkcwfbvzxqjzprkq8bbxifn0z0wm7mc9bzwrfxy761rjnj"))))
    (build-system qt-build-system)
    (propagated-inputs
     (list kauth kcodecs kconfig kwidgetsaddons))
    (native-inputs
     (list extra-cmake-modules kdoctools qttools-5))
    (inputs
     (list kcoreaddons
           kguiaddons
           ki18n
           ;; todo: PythonModuleGeneration
           qtbase-5
           qttools-5))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "src/khelpclient.cpp"
                ;; make QDirIterator follow symlinks
                (("^\\s*(QDirIterator it\\(.*, QDirIterator::Subdirectories)(\\);)" _ a b)
                 (string-append a " | QDirIterator::FollowSymlinks" b)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME"
                        (getcwd))
                (invoke "ctest" "-E" "kstandardactiontest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Widgets for configuration dialogs")
    (description "KConfigWidgets provides easy-to-use classes to create
configuration dialogs, as well as a set of widgets which uses KConfig to store
their settings.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kdeclarative
  (package
    (name "kdeclarative")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0w98pj8acxb4m9645963rzq5vja1fbih5czz24mf9zdqlg2dkz8g"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kconfig kpackage qtdeclarative-5))
    (native-inputs
     (list dbus extra-cmake-modules pkg-config xorg-server-for-tests))
    (inputs
     (list kauth
           kcoreaddons
           kglobalaccel
           kguiaddons
           kiconthemes
           kio
           ki18n
           kjobwidgets
           knotifications
           kservice
           kwidgetsaddons
           kwindowsystem
           libepoxy
           qtbase-5
           qtdeclarative-5
           solid))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'start-xorg-server
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; The test suite requires a running X server, setting
                   ;; QT_QPA_PLATFORM=offscreen does not suffice.
                   (system "Xvfb :1 -screen 0 640x480x24 &")
                   (setenv "DISPLAY" ":1")))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME"
                             (getcwd))
                     (setenv "XDG_RUNTIME_DIR"
                             (getcwd))
                     (setenv "QT_QPA_PLATFORM" "offscreen")
                     (setenv "DBUS_FATAL_WARNINGS" "0")
                     (invoke "dbus-launch" "ctest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Integration of QML and KDE work spaces")
    (description "KDeclarative provides integration of QML and KDE work spaces.
It's comprises two parts: a library used by the C++ part of your application to
intergrate QML with KDE Frameworks specific features, and a series of QML imports
that offer bindings to some of the Frameworks.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kded
  (package
    (name "kded")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "00n4isc4ahii0ldrg761lkmnq27kmrfqs9zkmpvmgbg57259mvc3"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           kdoctools
           kservice
           qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Central daemon of KDE work spaces")
    (description "KDED stands for KDE Daemon.  KDED runs in the background and
performs a number of small tasks.  Some of these tasks are built in, others are
started on demand.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kdesignerplugin
  (package
    (name "kdesignerplugin")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/portingAids/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0zlvkayv6zl5rp1076bscmdzyw93y7sxqb5848w11vs0g9amcj9n"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools qttools-5))
    (inputs
     (list kconfig
           kcoreaddons
           kdoctools
           qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Integrating KDE frameworks widgets with Qt Designer")
    (description "This framework provides plugins for Qt Designer that allow it
to display the widgets provided by various KDE frameworks, as well as a utility
(kgendesignerplugin) that can be used to generate other such plugins from
ini-style description files.")
    (license license:lgpl2.1+)))

(define-public kdesu
  (package
    (name "kdesu")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "14dcf32izn4lxr8vx372rfznflc1rcxwanx06phkd8mx9zyg4jxr"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kpty))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kconfig kcoreaddons ki18n kservice qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "User interface for running shell commands with root privileges")
    (description "KDESU provides functionality for building GUI front ends for
(password asking) console mode programs.  kdesu and kdessh use it to interface
with su and ssh respectively.")
    (license license:lgpl2.1+)))

(define-public kemoticons
  (package
    (name "kemoticons")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0w87prkhdmba7y8ylbycdpwdzd2djmp7hvv5ljb9s4aqqhnn3vw4"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kservice))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list karchive kconfig kcoreaddons qtbase-5))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'check-setup
                          (lambda _
                            (setenv "HOME"
                                    (getcwd))
                            ;; make Qt render "offscreen", required for tests
                            (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Convert text emoticons to graphical emoticons")
    (description "KEmoticons converts emoticons from text to a graphical
representation with images in HTML.  It supports setting different themes for
emoticons coming from different providers.")
    ;; dual licensed, image files are licensed under cc-by-sa4.0
    (license (list license:gpl2+ license:lgpl2.1+ license:cc-by-sa4.0))))

(define-public kglobalaccel
  (package
    (name "kglobalaccel")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "19mmav055fnzyl760fyhf0pdvaidd5i1h04l2hcnpin4p1jnpfap"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list kconfig
           kcrash
           kcoreaddons
           kdbusaddons
           kwindowsystem
           qtbase-5
           qtx11extras
           qtdeclarative-5
           xcb-util-keysyms))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Global desktop keyboard shortcuts")
    (description "KGlobalAccel allows you to have global accelerators that are
independent of the focused window.  Unlike regular shortcuts, the application's
window does not need focus for them to be activated.")
    (license license:lgpl2.1+)))

(define-public kiconthemes
  (package
    (name "kiconthemes")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ndiqmcs1ybj4acc6k3p9jwq09slqc4nj12ifqvlxrfj3ak6sb28"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5 shared-mime-info))
    (inputs
     (list karchive
           kauth
           kcodecs
           kcoreaddons
           kconfig
           kconfigwidgets
           ki18n
           kitemviews
           kwidgetsaddons
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'check-setup
                 (lambda* (#:key inputs #:allow-other-keys)
                   (setenv "XDG_DATA_DIRS"
                           (string-append #$(this-package-native-input
                                             "shared-mime-info")
                                          "/share"))
                   (setenv "HOME" (getcwd))
                   ;; make Qt render "offscreen", required for tests
                   (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Icon GUI utilities")
    (description "This library contains classes to improve the handling of icons
in applications using the KDE Frameworks.")
    (license license:lgpl2.1+)))

(define-public kinit
  (package
    (name "kinit")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0b6z9gq05vz20hm5y9ai3sbqq3gxwm3a3z88dkvi7dywk7vbqcph"))
              ;; Use the store paths for other packages and dynamically loaded
              ;; libs
              (patches (search-patches "kinit-kdeinit-extra_libs.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Set patched-in values:
              (substitute* "src/kdeinit/kinit.cpp"
                (("GUIX_PKGS_KF5_KIO") #$(this-package-input "kio"))
                (("GUIX_PKGS_KF5_PARTS") #$(this-package-input "kparts"))
                (("GUIX_PKGS_KF5_PLASMA")
                 #$(this-package-input "plasma-framework"))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "KDEINIT5_LIBRARY_PATH")
            (files '("lib/")))))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kauth
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdoctools
           kio
           kitemviews
           ki18n
           kjobwidgets
           kparts
           kservice
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libcap ; to install start_kdeinit with CAP_SYS_RESOURCE
           plasma-framework
           qtbase-5
           solid))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Library to speed up start of applications on KDE workspaces")
    (description "Kdeinit is a process launcher similar to init used for booting
UNIX.  It launches processes by forking and then loading a dynamic library which
contains a @code{kdemain(@dots{})} function.  Using kdeinit to launch KDE
applications makes starting KDE applications faster and reduces memory
consumption.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kio
  (package
    (name "kio")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0nwmxbfhvfw69q07vxvflri7rkdczyc89xv4ll3nrzrhgf15kb2z"))
              (patches (search-patches "kio-search-smbd-on-PATH.patch"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list acl
           kbookmarks
           kconfig
           kcompletion
           kcoreaddons
           kitemviews
           kjobwidgets
           kservice
           kwindowsystem
           kxmlgui
           solid))
    (native-inputs
     (list extra-cmake-modules dbus kdoctools qttools-5))
    (inputs (list mit-krb5
                  karchive
                  kauth
                  kcodecs
                  kconfigwidgets
                  kcrash
                  kdbusaddons
                  kded
                  kguiaddons
                  kiconthemes
                  ki18n
                  knotifications
                  ktextwidgets
                  kwallet
                  kwidgetsaddons
                  libxml2
                  libxslt
                  qtbase-5
                  qtdeclarative-5
                  qtscript
                  qtx11extras
                  sonnet
                  `(,util-linux "lib")  ; libmount
                  zlib))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              ;; Better error message (taken from NixOS)
              (substitute* "src/kiod/kiod_main.cpp"
                (("(^\\s*qCWarning(KIOD_CATEGORY) << \
\"Error loading plugin:\")( << loader.errorString();)" _ a b)
                 (string-append a "<< name" b)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" (getcwd))
                (setenv "XDG_RUNTIME_DIR" (getcwd))
                (setenv "QT_QPA_PLATFORM" "offscreen")
                (setenv "DBUS_FATAL_WARNINGS" "0")
                (invoke "dbus-launch" "ctest"
                        "--rerun-failed" "--output-on-failure"
                        "-E"
                        ;; The following tests fail or are flaky (see:
                        ;; https://bugs.kde.org/show_bug.cgi?id=440721).
                        (string-append "(kiocore-jobtest"
                                       "|kiocore-kmountpointtest"
                                       "|kiowidgets-kdirlistertest"
                                       "|kiocore-kfileitemtest"
                                       "|kiocore-ktcpsockettest"
                                       "|kiocore-mimetypefinderjobtest"
                                       "|kiocore-krecentdocumenttest"
                                       "|kiocore-http_jobtest"
                                       "|kiogui-openurljobtest"
                                       "|kioslave-httpheaderdispositiontest"
                                       "|applicationlauncherjob_forkingtest"
                                       "|applicationlauncherjob_scopetest"
                                       "|applicationlauncherjob_servicetest"
                                       "|commandlauncherjob_forkingtest"
                                       "|commandlauncherjob_scopetest"
                                       "|commandlauncherjob_servicetest"
                                       "|kiowidgets-kdirmodeltest"
                                       "|kiowidgets-kurifiltertest-colon-separator"
                                       "|kiofilewidgets-kfilewidgettest"
                                       "|kiowidgets-kurifiltertest-space-separator"
                                       "|kioworker-httpheaderdispositiontest)")))))
          (add-after 'install 'add-symlinks
            ;; Some package(s) (e.g. bluedevil) refer to these service types by
            ;; the wrong name.  I would prefer to patch those packages, but I
            ;; cannot find the files!
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((kst5 (string-append #$output "/share/kservicetypes5/")))
                (symlink (string-append kst5 "kfileitemactionplugin.desktop")
                         (string-append kst5 "kfileitemaction-plugin.desktop"))))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Network transparent access to files and data")
    (description "This framework implements a lot of file management functions.
It supports accessing files locally as well as via HTTP and FTP out of the box
and can be extended by plugins to support other protocols as well.  There is a
variety of plugins available, e.g. to support access via SSH.  The framework can
also be used to bridge a native protocol to a file-based interface.  This makes
the data accessible in all applications using the KDE file dialog or any other
KIO enabled infrastructure.")
    (license license:lgpl2.1+)))

(define-public knewstuff
  (package
    (name "knewstuff")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "15xmx7rnnrsz2cj044aviyr4hi9h8r0nnva9qzcjcq2hkkgj7wjj"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list attica kservice kxmlgui))
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list karchive
           kauth
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kio
           kitemviews
           ki18n
           kiconthemes
           kjobwidgets
           kpackage
           ktextwidgets
           kwidgetsaddons
           qtbase-5
           qtdeclarative-5
           solid
           sonnet))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _ ; XDG_DATA_DIRS isn't set
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Framework for downloading and sharing additional application data")
    (description "The KNewStuff library implements collaborative data sharing
for applications.  It uses libattica to support the Open Collaboration Services
specification.")
    (license license:lgpl2.1+)))

(define-public knotifyconfig
  (package
    (name "knotifyconfig")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "049n64qlr69zv1dc1dhgbsca37179hp06xfsxnhg97lblz3p3gds"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kauth
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kio
           kitemviews
           ki18n
           kjobwidgets
           knotifications
           kservice
           kwidgetsaddons
           kxmlgui
           phonon
           qtbase-5
           solid))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Configuration dialog for desktop notifications")
    (description "KNotifyConfig provides a configuration dialog for desktop
notifications which can be embedded in your application.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kparts
  (package
    (name "kparts")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rrf765p554r7l8j23gx5zxdq6wimh0v91qdkwz7ilm2qr16vd5v"))))
    (build-system qt-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-partloader-test
                    (lambda _
                      (substitute* "autotests/CMakeLists.txt"
                        ;; XXX: PartLoaderTest wants to create a .desktop file
                        ;; in the common locations and test that MIME types work.
                        ;; The setup required for this is extensive, skip for now.
                        (("partloadertest\\.cpp") "")))))))
    (propagated-inputs
     (list kio ktextwidgets kxmlgui))
    (native-inputs
     (list extra-cmake-modules shared-mime-info))
    (inputs
     (list kauth
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kiconthemes
           kitemviews
           ki18n
           kjobwidgets
           kservice
           kwidgetsaddons
           qtbase-5
           solid
           sonnet))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Plugin framework for user interface components")
    (description "This library implements the framework for KDE parts, which are
widgets with a user-interface defined in terms of actions.")
    (license license:lgpl2.1+)))

(define-public kpeople
  (package
    (name "kpeople")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "04v0s3amn6lbb16qvp1r6figckva6xk8z7djk8jda8fbnx8dx2r1"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kconfig
           kcoreaddons
           kitemviews
           ki18n
           kservice
           kwidgetsaddons
           qtbase-5
           qtdeclarative-5))
    (arguments
     `(#:tests? #f)) ; FIXME: 1/3 tests fail.
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Provides access to all contacts and aggregates them by person")
    (description "KPeople offers unified access to our contacts from different
sources, grouping them by person while still exposing all the data.  KPeople
also provides facilities to integrate the data provided in user interfaces by
providing QML and Qt Widgets components.  The sources are plugin-based, allowing
to easily extend the contacts collection.")
    (license license:lgpl2.1+)))

(define-public krunner
  (package
    (name "krunner")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rjs9b87bi4f6pdm9fwnha2sj2mrq260l80iz2jq1zah83p546sw"))))
    (build-system qt-build-system)
    (propagated-inputs
     (list plasma-framework))
    (native-inputs
     (list extra-cmake-modules
           ;; For tests.
           dbus))
    (inputs
     (list kactivities
           kauth
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kio
           kitemviews
           ki18n
           kjobwidgets
           kpackage
           kservice
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           qtdeclarative-5
           solid
           threadweaver))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths-for-test
            ;; This test tries to access paths like /home, /usr/bin and /bin/ls
            ;; which don't exist in the build-container. Change to existing paths.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "autotests/runnercontexttest.cpp"
                (("/home\"") "/tmp\"") ;; single path-part
                (("//usr/bin\"") (string-append (getcwd) "\"")) ;; multiple path-parts
                (("/bin/ls")
                 (search-input-file inputs "/bin/ls")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" (getcwd))
                (setenv "QT_QPA_PLATFORM" "offscreen")
                (invoke "dbus-launch" "ctest")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Framework for Plasma runners")
    (description "The Plasma workspace provides an application called KRunner
which, among other things, allows one to type into a text area which causes
various actions and information that match the text appear as the text is being
typed.")
    (license license:lgpl2.1+)))

(define-public kservice
  (package
    (name "kservice")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0jdvlplnsb9w628wh3ip6awxvhgyc097zh7ls9614ymkbnpc9xca"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kconfig kcoreaddons kdoctools))
    (native-inputs
     (list bison extra-cmake-modules flex shared-mime-info))
    (inputs
     (list kcrash kdbusaddons kdoctools ki18n qtbase-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           ;; Adopted from NixOS' patches "qdiriterator-follow-symlinks" and
           ;; "no-canonicalize-path".
           (lambda _
             (substitute* "src/sycoca/kbuildsycoca.cpp"
               ;; make QDirIterator follow symlinks
               (("^\\s*(QDirIterator it\\(.*, QDirIterator::Subdirectories)(\\);)" _ a b)
                (string-append a " | QDirIterator::FollowSymlinks" b)))
             (substitute* "src/sycoca/vfolder_menu.cpp"
               ;; Normalize path, but don't resolve symlinks (taken from
               ;; NixOS)
               (("^\\s*QString resolved = QDir\\(dir\\)\\.canonicalPath\\(\\);")
                "QString resolved = QDir::cleanPath(dir);"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" (getcwd))
               (setenv "QT_QPA_PLATFORM" "offscreen")
               ;; Disable failing tests.
               (invoke "ctest" "-E" "(kautostarttest|ksycocatest|kapplicationtradertest)")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Plugin framework for desktop services")
    (description "KService provides a plugin framework for handling desktop
services.  Services can be applications or libraries.  They can be bound to MIME
types or handled by application specific code.")
    ;; triple licensed
    (license (list license:gpl2+ license:gpl3+ license:lgpl2.1+))))

(define-public ktexteditor
  (package
    (name "ktexteditor")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    "ktexteditor-" version ".tar.xz"))
              (sha256
               (base32
                "06amzk6290imi2gj3v1k3f56zdlad7zbz4wwlf34v4iibj9mfgw8"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kparts
           ksyntaxhighlighting))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list editorconfig-core-c
           karchive
           kauth
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kguiaddons
           kiconthemes
           kio
           kitemviews
           ki18n
           kjobwidgets
           kparts
           kservice
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libgit2
           perl
           qtbase-5
           qtdeclarative-5
           qtscript
           qtxmlpatterns
           solid
           sonnet))
    (arguments
     (list #:phases
       #~(modify-phases %standard-phases
         (add-after 'unpack 'setup
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "XDG_DATA_DIRS" ; FIXME build phase doesn't find parts.desktop
                     (string-append #$(this-package-input "kparts") "/share"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests? ;; Maybe locale issues with tests?
               (setenv "QT_QPA_PLATFORM" "offscreen")
               (invoke "ctest" "-E" "(kateview_test|movingrange_test)"))))
         (add-after 'install 'add-symlinks
           ;; Some package(s) (e.g. plasma-sdk) refer to these service types
           ;; by the wrong name.  I would prefer to patch those packages, but
           ;; I cannot find the files!
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((kst5 (string-append #$output
                                        "/share/kservicetypes5/")))
               (symlink (string-append kst5 "ktexteditorplugin.desktop")
                        (string-append kst5 "ktexteditor-plugin.desktop"))))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Full text editor component")
    (description "KTextEditor provides a powerful text editor component that you
can embed in your application, either as a KPart or using the KF5::TextEditor
library.")
    ;; triple licensed
    (license (list license:gpl2+ license:lgpl2.0+ license:lgpl2.1+))))

(define-public ktextwidgets
  (package
    (name "ktextwidgets")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0w1wwyd3fy351rmkhf3i55is5031j2zxvswm0b1sb3pd159v888v"))))
    (build-system qt-build-system)
    (propagated-inputs
     (list ki18n sonnet))
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list kauth
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kiconthemes
           kservice
           kwidgetsaddons
           kwindowsystem
           qtbase-5
           qtspeech-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Text editing widgets")
    (description "KTextWidgets provides widgets for displaying and editing text.
It supports rich text as well as plain text.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kwallet
  (package
    (name "kwallet")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1cji8bvy5m77zljyrrgipsw8pxcds1sgikxlq3sdfxymcsw2wr36"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases
       #~(modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests? ;; Seems to require network.
               (invoke "ctest" "-E"
                       "(fdo_secrets_test)")))))))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list gpgme
           kauth
           kcodecs
           kconfig
           kconfigwidgets
           kcoreaddons
           kdbusaddons
           kdoctools
           kiconthemes
           ki18n
           knotifications
           kservice
           kwidgetsaddons
           kwindowsystem
           libgcrypt
           phonon
           qgpgme
           qca
           qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Safe desktop-wide storage for passwords")
    (description "This framework contains an interface to KWallet, a safe
desktop-wide storage for passwords and the kwalletd daemon used to safely store
the passwords on KDE work spaces.")
    (license license:lgpl2.1+)))

(define-public kxmlgui
  (package
    (name "kxmlgui")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0gvjf32ssc0r0bdpb1912ldsr5rjls8vrscwy5gm9g5gw504hmmr"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kconfig kconfigwidgets))
    (native-inputs
     (list extra-cmake-modules qttools-5 xorg-server-for-tests))
    (inputs
     (list attica
           kauth
           kcodecs
           kcoreaddons
           kglobalaccel
           kguiaddons
           kiconthemes
           kitemviews
           ki18n
           ktextwidgets
           kwidgetsaddons
           kwindowsystem
           qtbase-5
           sonnet))
    (arguments
     (list #:phases
       #~(modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" (getcwd))
               (setenv "QT_QPA_PLATFORM" "offscreen") ;; These tests fail
               (invoke "ctest" "-E" "(ktoolbar_unittest|kxmlgui_unittest)")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Framework for managing menu and toolbar actions")
    (description "KXMLGUI provides a framework for managing menu and toolbar
actions in an abstract way.  The actions are configured through a XML description
and hooks in the application code.  The framework supports merging of multiple
descriptions for integrating actions from plugins.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kxmlrpcclient
  (package
    (name "kxmlrpcclient")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/portingAids/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1fgjai3vj3yk67ynhd7blilyrdhdn5nvma3v3j1sbdg98pr7qzar"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kio))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kauth
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kitemviews
           ki18n
           kjobwidgets
           kservice
           kwidgetsaddons
           kxmlgui
           qtbase-5
           solid))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "XML-RPC client")
    (description "This library contains simple XML-RPC Client support.  It is a
complete client and is easy to use.  Only one interface is exposed,
kxmlrpcclient/client.h and from that interface, you only need to use 3 methods:
setUrl, setUserAgent and call.")
    ;; dual licensed
    (license (list license:bsd-2 license:lgpl2.1+))))

(define-public plasma-framework
  (package
    (name "plasma-framework")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "058hl76q35bw3rzmv348azk1lmhkpgmfrxr3jd9s1hphijr8sgcx"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list kpackage kservice))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs (list kactivities
                  karchive
                  kauth
                  kbookmarks
                  kcodecs
                  kcompletion
                  kconfig
                  kconfigwidgets
                  kcoreaddons
                  kdbusaddons
                  kdeclarative
                  kglobalaccel
                  kguiaddons
                  kiconthemes
                  kirigami
                  kitemviews
                  kio
                  ki18n
                  kjobwidgets
                  knotifications
                  kwayland
                  kwidgetsaddons
                  kwindowsystem
                  kxmlgui
                  ;; XXX: "undefined reference to `glGetString'" errors occur without libglvnd,
                  libglvnd
                  phonon
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtsvg-5
                  qtx11extras
                  solid))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME"
                             (getcwd))
                     (setenv "QT_QPA_PLATFORM" "offscreen") ;; These tests fail
                     (invoke "ctest" "-E"
                             (string-append "(plasma-dialogstatetest"
                                            "|plasma-iconitemtest"
                                            "|plasma-themetest"
                                            "|iconitemhidpitest"
                                            "|dialognativetest)"))))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Libraries, components and tools of Plasma workspaces")
    (description "The plasma framework provides QML components, libplasma and
script engines.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public purpose
  (package
    (name "purpose")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1lj67f0x4gvbh9by3c3crbbwwnx7b9ifjna9ggziya4m6zj0m4z1"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list ;;TODO: ("kaccounts" ,kaccounts)
           kconfig
           kcoreaddons
           knotifications
           ki18n
           kio
           kirigami
           qtbase-5
           qtdeclarative-5))
    (arguments
     (list #:tests? #f ;; seem to require network; don't find QTQuick components
           #:configure-flags #~'("-DBUILD_TESTING=OFF"))) ; not run anyway
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Offers available actions for a specific purpose")
    (description "This framework offers the possibility to create integrate
services and actions on any application without having to implement them
specifically.  Purpose will offer them mechanisms to list the different
alternatives to execute given the requested action type and will facilitate
components so that all the plugins can receive all the information they
need.")
    (license license:lgpl2.1+)))

;; This version of kdbusaddons does not use kinit as an input, and is used to
;; build kinit-bootstrap, as well as bootstrap versions of all kinit
;; dependencies which also rely on kdbusaddons.
(define kdbusaddons-bootstrap
  (package
    (inherit kdbusaddons)
    (source (origin
              (inherit (package-source kdbusaddons))
              (patches '())))
    (inputs (modify-inputs (package-inputs kdbusaddons) (delete "kinit")))
    (arguments
     (substitute-keyword-arguments (package-arguments kdbusaddons)
       ((#:phases phases)
        #~(modify-phases #$phases
           (delete 'patch-source)))))))

(define kinit-bootstrap
  ((package-input-rewriting `((,kdbusaddons . ,kdbusaddons-bootstrap))) kinit))

(define-public ktextaddons
  (package
    (name "ktextaddons")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/" name "/" name "-" version ".tar.xz"))
       (sha256
        (base32
         "1p0p17dnh96zmzfb91wri7bryr90pvwb07r95n6xdad8py5dnlla"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda _
                   (setenv "HOME" (getcwd))
                   ;; XXX: 6 tests failed due to:
                   ;;   missing icons
                   ;;   translators plugins not available during tests
                   (invoke "ctest" "-E"
                           "(grammalecteresultwidgettest|grammalecteconfigwidgettest||grammalecteresultjobtest|languagetoolconfigwidgettest|translator-translatorwidgettest|translator-translatorengineloadertest)"))))))
    (native-inputs
     (list extra-cmake-modules
           qttools-5))
    (inputs
     (list karchive
           kconfigwidgets
           kcoreaddons
           ki18n
           kio
           ksyntaxhighlighting
           kxmlgui
           qtbase-5
           qtkeychain
           sonnet))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Various text handling addons")
    (description "This library provides text addons (autocorrection, text to
speak, grammar checking, text translator, emoticon support) for Qt
applications.")
    (license
     (list license:lgpl2.0+ license:bsd-3 license:gpl2+ license:cc0))))


;; Tier 4
;;
;; Tier 4 frameworks can be mostly ignored by application programmers; this
;; tier consists of plugins acting behind the scenes to provide additional
;; functionality or platform integration to existing frameworks (including
;; Qt).

(define-public kde-frameworkintegration
  (package
    (name "kde-frameworkintegration")
    (version "5.114.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    "frameworkintegration-" version ".tar.xz"))
              (sha256
               (base32
                "1dqgzhhh8gnvl8jsvh2i6pjn935d61avh63b4z9kpllhvp9a2lnd"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    ;; TODO: Optional packages not yet in Guix: packagekitqt5, AppStreamQt
    (inputs (list kconfig
                  kconfigwidgets
                  kcoreaddons
                  ki18n
                  kiconthemes
                  kitemviews
                  knewstuff
                  knotifications
                  kpackage
                  kwidgetsaddons
                  qtbase-5
                  qtx11extras))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; Make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Frameworks 5 workspace and cross-framework integration plugins")
    (description "Framework Integration is a set of plugins responsible for
better integration of Qt applications when running on a KDE Plasma
workspace.")
    ;; This package is distributed under either LGPL2 or LGPL3, but some
    ;; files are explicitly LGPL2+.
    (license (list license:lgpl2.0 license:lgpl3 license:lgpl2.0+))
    (properties `((upstream-name . "frameworkintegration")))))


;; Porting Aids
;;
;; Porting Aids frameworks provide code and utilities to ease the transition
;; from kdelibs 4 to KDE Frameworks 5. Code should aim to port away from this
;; framework, new projects should avoid using these libraries.

(define-public kdelibs4support
  (package
    (name "kdelibs4support")
    (version "5.114.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "17473him2fjfcw5f88diarqac815wsakfyb9fka82a4qqh9l41mc"))))
    (build-system cmake-build-system)
    (native-inputs
     (list dbus
           docbook-xml-4.4 ; optional
           extra-cmake-modules
           kdoctools
           perl
           perl-uri
           pkg-config
           qttools
           shared-mime-info
           kjobwidgets ;; required for running the tests
           strace
           tzdata-for-tests))
    (propagated-inputs
     ;; These are required to be installed along with this package, see
     ;; lib64/cmake/KF5KDELibs4Support/KF5KDELibs4SupportConfig.cmake
     (list karchive
           kauth
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdesignerplugin
           kdoctools
           kemoticons
           kguiaddons
           kiconthemes
           kinit
           kitemmodels
           knotifications
           kparts
           ktextwidgets
           kunitconversion
           kwindowsystem
           qtbase-5))
    (inputs
     (list kcompletion
           kconfig
           kded
           kglobalaccel
           ki18n
           kio
           kservice
           kwidgetsaddons
           kxmlgui
           libsm
           networkmanager-qt
           openssl
           qtsvg-5
           qttools-5
           qtx11extras))
    ;; FIXME: Use Guix ca-bundle.crt in etc/xdg/ksslcalist and
    ;; share/kf5/kssl/ca-bundle.crt
    ;; TODO: NixOS has nix-kde-include-dir.patch to change std-dir "include"
    ;; into "@dev@/include/". Think about whether this is needed for us, too.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-cmake-to-find-docbook
           (lambda _
             (substitute* "cmake/FindDocBookXML4.cmake"
               (("^.*xml/docbook/schema/dtd.*$")
                "xml/dtd/docbook\n"))))
         (delete 'check)
         (add-after 'install 'check-post-install
           (lambda* (#:key inputs tests? #:allow-other-keys)
             (setenv "HOME" (getcwd))
             (setenv "TZDIR"    ; KDateTimeTestsome needs TZDIR
                     (search-input-directory inputs
                                             "share/zoneinfo"))
             ;; Make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             ;; enable debug output
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; enable debug output
             (setenv "DBUS_FATAL_WARNINGS" "0")
             ;; Make kstandarddirstest pass (see https://bugs.kde.org/381098)
             (mkdir-p ".kde-unit-test/xdg/config")
             (with-output-to-file ".kde-unit-test/xdg/config/foorc"
               (lambda () #t))  ;; simply touch the file
             ;; Blacklist a test-function (failing at build.kde.org, too).
             (with-output-to-file "autotests/BLACKLIST"
               (lambda _
                 (display "[testSmb]\n*\n")))
             (invoke "dbus-launch" "ctest"
                     "-E" "kstandarddirstest"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Frameworks 5 porting aid from KDELibs4")
    (description "This framework provides code and utilities to ease the
transition from kdelibs 4 to KDE Frameworks 5.  This includes CMake macros and
C++ classes whose functionality has been replaced by code in CMake, Qt and
other frameworks.

Code should aim to port away from this framework eventually.  The API
documentation of the classes in this framework and the notes at
http://community.kde.org/Frameworks/Porting_Notes should help with this.")
    ;; Most files are distributed under LGPL2+, but the package includes code
    ;; under a variety of licenses.
    (license (list license:lgpl2.1+ license:lgpl2.0 license:lgpl2.0+
                   license:gpl2 license:gpl2+
                   license:expat license:bsd-2 license:bsd-3
                   license:public-domain))))

(define-public khtml
  (package
    (name "khtml")
    (version "5.114.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "1mf84zs9hjvmi74f8rgqzrfkqjq597f9k64dn1bqcj13v0w10vry"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules perl))
    (inputs
     (list giflib
           gperf
           karchive
           kcodecs
           kglobalaccel
           ki18n
           kiconthemes
           kio
           kjs
           knotifications
           kparts
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libjpeg-turbo
           libpng
           openssl
           phonon
           qtbase-5
           qtx11extras
           sonnet))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Frameworks 5 HTML widget and component")
    (description "KHTML is a web rendering engine, based on the KParts
technology and using KJS for JavaScript support.")
    ;; Most files are distributed under LGPL2+, but the package includes code
    ;; under a variety of licenses.
    (license (list license:lgpl2.0+ license:lgpl2.1+
                   license:gpl2  license:gpl3+
                   license:expat license:bsd-2 license:bsd-3))))

(define-public kjs
  (package
    (name "kjs")
    (version "5.114.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "08nh6yr6bqifpb5s9a4wbjwmwnm7zp5k8hcdmyb6mlcbam9qp6j7"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools perl pkg-config))
    (inputs
     (list pcre qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Frameworks 5 support for Javascript scripting in Qt
applications")
    (description "Add-on library to Qt which adds JavaScript scripting
support.")
    ;; Most files are distributed under LGPL2+, but the package also includes
    ;; code under a variety of licenses.
    (license (list license:lgpl2.1+
                   license:bsd-2 license:bsd-3
                   (license:non-copyleft "file://src/kjs/dtoa.cpp")))))

(define-public kjsembed
  (package
    (name "kjsembed")
    (version "5.114.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "1xglisxv7nfsbj9lgpvc4c5ql4f6m7n71vf7vih5ff3aqybrkgxa"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools qttools-5))
    (inputs
     (list ki18n kjs qtbase-5 qtsvg-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Frameworks 5 embedded Javascript engine for Qt")
    (description "KJSEmbed provides a method of binding Javascript objects to
QObjects, so you can script your applications.")
    (license license:lgpl2.1+)))

(define-public kmediaplayer
  (package
    (name "kmediaplayer")
    (version "5.114.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "092yvzvrkvr8xxncw7h5ghfd2bggzxsqfj67c2vhymhfw4i0c54x"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools qttools-5))
    (inputs
     (list kcompletion
           kcoreaddons
           ki18n
           kiconthemes
           kio
           kparts
           kwidgetsaddons
           kxmlgui
           qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Frameworks 5 plugin interface for media player features")
    (description "KMediaPlayer builds on the KParts framework to provide a
common interface for KParts that can play media files.

This framework is a porting aid.  It is not recommended for new projects, and
existing projects that use it are advised to port away from it, and use plain
KParts instead.")
    (license license:expat)))

(define-public kross
  (package
    (name "kross")
    (version "5.114.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "0bbpi63lxbb4ylx2jd172a2bqyxkd606n7w8zrvcjy466lkv3sz4"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools qttools-5))
    (inputs
     (list kcompletion
           kcoreaddons
           ki18n
           kiconthemes
           kparts
           kwidgetsaddons
           kxmlgui
           qtbase-5
           qtscript))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Frameworks 5 solution for application scripting")
    (description "Kross is a scripting bridge for the KDE Development Platform
used to embed scripting functionality into an application.  It supports
QtScript as a scripting interpreter backend.

Kross provides an abstract API to provide scripting functionality in a
interpreter-independent way.  The application that uses Kross should not need
to know anything about the scripting language being used.  The core of Kross
provides the framework to deal transparently with interpreter-backends and
offers abstract functionality to deal with scripts.")
    ;; Most files are distributed under LGPL2+, but the package includes code
    ;; under a variety of licenses.
    (license (list license:lgpl2.0+ license:lgpl2.1+
                   license:lgpl2.0 license:gpl3+))))

(define-public kdav
  (package
    (name "kdav")
    (version "5.114.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "11959fxz24snk2l31kw8w96wah0s2fjimimrxh6xhppiy5qp2fp2"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcoreaddons ki18n kio qtbase-5 qtxmlpatterns))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                               ;; Seems to require network.
                              (invoke "ctest" "-E"
                                      "(kdav-davcollectionsmultifetchjobtest|\
kdav-davitemfetchjob)")))))))
    (home-page "https://invent.kde.org/frameworks/kdav")
    (synopsis "DAV protocol implementation with KJobs")
    (description "This is a DAV protocol implementation with KJobs.  Calendars
and todos are supported, using either GroupDAV or CalDAV, and contacts are
supported using GroupDAV or CardDAV.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))
