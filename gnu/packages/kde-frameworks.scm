;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public extra-cmake-modules
  (package
    (name "extra-cmake-modules")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "14h7519bfkwzh58lxgxrj5jlfhfnrakhjcm6lmhafnkg5h56m4kr"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("qtbase" ,qtbase))) ; For tests (needs qmake)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-lib-path
           (lambda _
             ;; Always install into /lib and not into /lib64.
             (substitute* "kde-modules/KDEInstallDirs.cmake"
               (("\"lib64\"") "\"lib\"")
               ;; TODO: Base the following on values taken from Qt
               ;; Install plugins into lib/qt5/plugins
               ;; TODO: Check if this is okay for Android, too
               ;; (see comment in KDEInstallDirs.cmake)
               (("_define_relative\\(QTPLUGINDIR \"\\$\\{_pluginsDirParent}\" \"plugins\"")
                "_define_relative(QTPLUGINDIR \"${_pluginsDirParent}\" \"qt5/plugins\"")
               ;; Install imports into lib/qt5/imports
               (("_define_relative\\(QTQUICKIMPORTSDIR QTPLUGINDIR \"imports\"")
                "_define_relative(QTQUICKIMPORTSDIR LIBDIR \"qt5/imports\"")
               ;; Install qml-files into lib/qt5/qml
               (("_define_relative\\(QMLDIR LIBDIR \"qml\"")
                "_define_relative(QMLDIR LIBDIR \"qt5/qml\""))
             (substitute* "modules/ECMGeneratePriFile.cmake"
               ;; Install pri-files into lib/qt5/mkspecs
               (("set\\(ECM_MKSPECS_INSTALL_DIR mkspecs/modules")
                "set(ECM_MKSPECS_INSTALL_DIR lib/qt5/mkspecs/modules"))
             #t))
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

(define-public phonon
  (package
    (name "phonon")
    (version "4.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/phonon"
                    "/" version "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0bfy8iqmjhlg3ma3iqd3kxjc2zkzpjgashbpf5x17y0dc2i1whxl"))))
    (build-system cmake-build-system)
    (native-inputs
     ;; TODO: Think about adding pulseaudio. Is it required for sound?
     ;; TODO: Add building the super experimental QML support
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:configure-flags
       '("-DCMAKE_CXX_FLAGS=-fPIC"
         "-DPHONON_BUILD_PHONON4QT5=ON")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'patch-installdir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((regex (string-append "(INSTALL DESTINATION \")"
                                         (assoc-ref inputs "qtbase"))))
               (substitute* "cmake_install.cmake"
                 ((regex all dest)
                  (string-append dest (assoc-ref outputs "out")))))
           #t)))))
    (home-page "https://phonon.kde.org")
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
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("qtbase" ,qtbase)
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qttools" ,qttools)
       ("qtx11extras" ,qtx11extras)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libxml2" ,libxml2)))
    (arguments
     `(#:configure-flags
       '( "-DPHONON_BUILD_PHONON4QT5=ON")))
    (home-page "https://phonon.kde.org")
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0dvsjwl2c3liiicv2m8p1m7hlc3fcapsj5pfirmdm1l38wcayr60"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-network-tests
           (lambda _
             ;; These tests require network access.
             (substitute* "autotests/CMakeLists.txt"
               ((".*providertest.cpp") "")))))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "179am9czhb9mcirmja5bxjmqil3qg81v56n8rmkkp5q50qpln4dh"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     ;; TODO: qtdeclarative (yields one failing test)
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:configure-flags
       (list (string-append
              "-DUDEV_RULES_INSTALL_DIR=" %output "/lib/udev/rules.d"))
       ;; TODO: Make tests pass: DBUS_FATAL_WARNINGS=0 still yields 7/8 tests
       ;; failing.  When running after install, tests hang.
       #:tests? #f))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "QML wrapper for BlueZ")
    (description "bluez-qt is a Qt-style library for accessing the bluez
Bluetooth stack.  It is used by the KDE Bluetooth stack, BlueDevil.")
    (license (list license:lgpl2.1+ license:lgpl3+))))

(define-public breeze-icons
  (package
    (name "breeze-icons")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "17kil9dlh8w5r29arrjbv47b7jxz45kqzpg06zw3vz70bdgbb995"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("fdupes" ,fdupes)
       ("libxml2" ,libxml2)))
    (inputs
     `(("qtbase" ,qtbase)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0cwprhpz731bqqhr6vra6lc76k5i4pm3m9j0q21km9wm2djfmk19"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; has no test target
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (propagated-inputs
     ;; kapidox is a python programm
     ;; TODO: check if doxygen has to be installed, the readme does not
     ;; mention it. The openSuse .rpm lists doxygen, graphviz, graphviz-gd,
     ;; and python-xml.
     `(("python" ,python)
       ("python-jinja2" ,python-jinja2)
       ("python-pyyaml" ,python-pyyaml)))
    (inputs
     `(("qtbase" ,qtbase)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ypykybxl9dz57rgsfihx4rfhzhphidypb2f5nwrw1f6vlqkawzs"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("bzip2" ,bzip2)
       ("qtbase" ,qtbase)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt 5 addon providing access to numerous types of archives")
    (description "KArchive provides classes for easy reading, creation and
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1cqqwpd6faz7sd6jrda564xxvwr231b175h2w3ilxx6hvz9yrps3"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("perl" ,perl)
       ("tzdata" ,tzdata-for-tests)))
    (inputs
     `(("libical" ,libical)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-failing-libical3-tests
           (lambda _
             ;; testicaltimezones fails with some time-zone issue
             (substitute* "autotests/CMakeLists.txt"
               (("macro_unit_tests\\(testicaltimezones\\)" line)
                (string-append "## " line))
               (("target_link_libraries\\(testicaltimezones " line)
                (string-append "## " line)))
             (for-each
              delete-file
              (list
               ;; test cases are generated for each .ics file. These fail:
               "autotests/data/Compat-libical3/AppleICal_1.5.ics"
               "autotests/data/Compat-libical3/Evolution_2.8.2_timezone_test.ics"
               "autotests/data/Compat-libical3/KOrganizer_3.1a.ics"
               "autotests/data/Compat-libical3/MSExchange.ics"
               "autotests/data/Compat-libical3/Mozilla_1.0.ics"))
             #t))
         (add-before 'check 'set-timezone
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "Europe/Prague")
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0w5q37lqdzyl84723kg9sh9xxn2g2wv0xfxpy3218hmc9364s89h"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("gperf" ,gperf)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))
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

(define-public kconfig
  (package
    (name "kconfig")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ya0fvrrfiw4a5wkydcjpnkkyq3ampxy5h210dm04qim6a9x8rb1"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("inetutils" ,inetutils)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "TMPDIR" (getcwd))
             #t))
         (replace 'check
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             (invoke "dbus-launch" "ctest" "."))))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1w658wfksxglr0fjpa2p8gmjbvpd2knzhk3byk2s9dwil7i7cvjd"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)
       ("shared-mime-info" ,shared-mime-info)
       ;; TODO: FAM: File alteration notification http://oss.sgi.com/projects/fam
       ("xorg-server" ,xorg-server-for-tests))) ; for the tests
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'blacklist-failing-test
           (lambda _
             ;; Blacklist failing tests.
             (with-output-to-file "autotests/BLACKLIST"
               (lambda _
                 ;; FIXME: Make it pass.  Test failure caused by stout/stderr
                 ;; being interleaved.
                 (display "[test_channels]\n*\n")
                 ;; This fails with ENOSPC because of too many inotify watches.
                 (display "[benchNotifyWatcher]\n*\n")))
             #t))
         ;; See upstream commit ee424e9b62368485bba4193053cabb553a1d268e
         (add-after 'unpack 'fix-broken-test
           (lambda _
             (substitute* "autotests/kdirwatch_unittest.cpp"
               (("QVERIFY\\(waitForRecreationSignal\\(watch, existingFile\\)\\);" m)
                (string-append m "\nwaitUntilNewSecond();")))
             #t))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "TMPDIR" (getcwd))
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "11zwr89v0k4m1235rzr39z4cbfdwn5zg34abs62nfnr8vmba62qv"))
              (patches (search-patches "kdbusaddons-kinit-file-name.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("kinit" ,kinit-bootstrap))) ;; kinit-bootstrap: kinit package which does not depend on kdbusaddons.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'patch-source
          (lambda* (#:key inputs #:allow-other-keys)
            ;; look for the kdeinit5 executable in kinit's store directory,
            ;; instead of the current application's directory:
            (substitute* "src/kdeinitinterface.cpp"
              (("@SUBSTITUTEME@") (assoc-ref inputs "kinit")))))
         (replace 'check
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "ctest" "."))))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1bn9c6ii1n6ns478i3lp7yzga7dbcqvdb43rfzmz7gndnmh853gm"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("avahi" ,avahi) ; alternativly dnssd could be used
       ("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Network service discovery using Zeroconf")
    (description "KDNSSD is a library for handling the DNS-based Service
Discovery Protocol (DNS-SD), the layer of Zeroconf that allows network services,
such as printers, to be discovered without any user intervention or centralized
infrastructure.")
    (license license:lgpl2.1+)))

(define-public kguiaddons
  (package
    (name "kguiaddons")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0k22kargqxf0j09wzk1x90b526npj8a0210ilk0n1k6spc8xa6mr"))))
    (build-system cmake-build-system)
    ;; TODO: Build packages for the Python bindings.  Ideally this will be
    ;; done for all versions of python guix supports.  Requires python,
    ;; python-sip, clang-python, libclang.  Requires python-2 in all cases for
    ;; clang-python.
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Utilities for graphical user interfaces")
    (description "The KDE GUI addons provide utilities for graphical user
interfaces in the areas of colors, fonts, text, images, keyboard input.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kholidays
  (package
    (name "kholidays")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
       (sha256
        (base32 "1035kh0na4hwp4pnsi8p1nd7r114d4bgdnjr0g2hwmy868vx0a67"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; blacklist a failing test function TODO: make it pass
             (with-output-to-file "autotests/BLACKLIST"
               (lambda _
                 (display "[testDefaultRegions]\n*\n")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://cgit.kde.org/kholidays.git")
    (synopsis "Library for regional holiday information")
    (description "This library provides a C++ API that determines holiday and
other special events for a geographical region.")
    (license license:lgpl2.0+)))

(define-public ki18n
  (package
    (name "ki18n")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1fdp0bdvqbqzbxynbg3bcdridr8j3qnm9spyrrdg5q95sz99a4s9"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("gettext" ,gettext-minimal)
       ("python" ,python)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0zd51lsz23rqwk4lnzs5653sanprgr80p33yk8gmgsihgyfbjv0r"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxscrnsaver" ,libxscrnsaver) ; X-Screensaver based poller, fallback mode
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    "kirigami2-" version ".tar.xz"))
              (sha256
               (base32
                "1v76d1xgmi9kkxiwq4y6rzaxs1hwwm95pw490m09rgqhli7180yv"))))
    (properties `((upstream-name . "kirigami2")))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ;; TODO: Find a way to activate this optional include without
       ;; introducing a recursive dependency.
       ;;("plasma-frameworks" ,plasma-framework) ;; Tier 3!
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtsvg" ,qtsvg)
       ;; Run-time dependency
       ("qtgraphicaleffects" ,qtgraphicaleffects)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "QtQuick components for mobile user interfaces")
    (description "Kirigami is a set of high level QtQuick components looking
and feeling well on both mobile and desktop devices.  They ease the creation
of applications that follow the Kirigami Human Interface Guidelines.")
    (license license:lgpl2.1+)))

(define-public kitemmodels
  (package
    (name "kitemmodels")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "10bhg0db9gdg2hlc02ngg1i2q7a99862d5973hxqzf620d28p5rd"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "15h0w16wgj94kxz4vgjb34i3pyx5w1f2npj86j4d2sa0mxxpmqyz"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Set of item views extending the Qt model-view framework")
    (description "KItemViews includes a set of views, which can be used with
item models.  It includes views for categorizing lists and to add search filters
to flat and hierarchical lists.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kplotting
  (package
    (name "kplotting")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "11kpq34j37c1gsvj5nxhkc31bw8gw2n7nkqsfx87jw9f4v2vhmr9"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qttools" ,qttools)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _ ; kplotting
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    "syntax-highlighting-" version ".tar.xz"))
              (sha256
               (base32
                "08y0m4wcq1sbxvlga3w72waj5xckh5jra5x7rvnp87dl38qxhacs"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("perl" ,perl)
       ("qttools" ,qttools)
       ;; Optional, for compile-time validation of syntax definition files:
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'unpatch-source-shebang
           (lambda _
             ;; revert the patch-shebang phase on scripts which are
             ;; in fact test data
             (substitute* '("autotests/input/test.bash"
                            "autotests/folding/test.bash.fold")
               (((which "bash")) "/bin/bash"))
             (substitute* '("autotests/input/highlight.sh"
                            "autotests/folding/highlight.sh.fold")
               (((which "sh")) " /bin/sh")) ;; space in front!
             (substitute* '("autotests/input/highlight.pl"
                            "autotests/folding/highlight.pl.fold")
               (((which "perl")) "/usr/bin/perl"))
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Syntax highlighting engine for Kate syntax definitions")
    (description "This is a stand-alone implementation of the Kate syntax
highlighting engine.  It's meant as a building block for text editors as well
as for simple highlighted text rendering (e.g. as HTML), supporting both
integration with a custom editor as well as a ready-to-use
@code{QSyntaxHighlighter} sub-class.")
    (properties `((upstream-name . "syntax-highlighting")))
    (license license:lgpl2.1+)))

(define-public kwayland
  (package
    (name "kwayland")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "03z8hiw0mrhl3f0y7ypc46ic0w5vlp56prbqbk448jin5hakkkdg"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("qtbase" ,qtbase)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)))
    (arguments
     `(#:tests? #f ; FIXME tests require weston to run
                   ; weston requires wayland flags in mesa
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "XDG_RUNTIME_DIR" "/tmp")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1nzgv3v3kyq5jm2b9xri6qjawspr9ycxhskfvj8kkgr46dr35qyc"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             ;; Xvfb doesn't have proper glx support and needs a pixeldepth
             ;; of 24 bit to avoid "libGL error: failed to load driver: swrast"
             ;;                    "Could not initialize GLX"
             (system (string-append (assoc-ref inputs "xorg-server")
                                    "/bin/Xvfb :1 -screen 0 640x480x24 &"))
             (setenv "DISPLAY" ":1")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0nbkb8pkrbbp1s892ndzhakzri3qxpw48kibgb0rqz6i0azfxrz8"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("dbus" ,dbus) ; for the tests
       ("openbox" ,openbox) ; for the tests
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server-for-tests))) ; for the tests
    (inputs
     `(("libxrender" ,libxrender)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("xcb-utils-keysyms" ,xcb-util-keysyms)
       ("xcb-util-wm" ,xcb-util-wm)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'blacklist-failing-tests
           (lambda _
             ;; Blacklist a failing test-functions. FIXME: Make it pass.
             (with-output-to-file "autotests/BLACKLIST"
               (lambda _
                 (display "[testGroupLeader]\n*\n")
                 (display "[testClientMachine]\n*\n"))) ;; requires network
             #t))
         (replace 'check
           (lambda _
             ;; The test suite requires a running window anager
             (setenv "XDG_RUNTIME_DIR" "/tmp")
             (system "Xvfb :1 -ac -screen 0 640x480x24 &")
             (setenv "DISPLAY" ":1")
             (sleep 5) ;; Give Xvfb a few moments to get on it's feet
             (system "openbox &")
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "ctest" "."))))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0yv5q71cyyy04051ny5lr7n35pw391gxr6l324n6c0z7kgc560ad"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; Headers contain #include <ModemManager/ModemManager.h>
     `(("modem-manager" ,modem-manager)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "ctest" "."))))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ydfz6ikya1h8gg9c716y8lmy45iqnx0gaghkm0vz3c3ckx99l9g"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; Headers contain #include <NetworkManager.h> and
     ;;                 #include <libnm/NetworkManager.h>
     `(("network-manager" ,network-manager)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "ctest" "."))))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "5" "-" version ".tar.xz"))
              (sha256
               (base32
                "0rh9bysiisnpz3c92hyba98f58dlmwf9zhbbb4vmab0qvm5d3q9g"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("fdupes" ,fdupes)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen provides the standard icon theme for the KDE desktop")
    (description "Oxygen icon theme for the KDE desktop")
    (license license:lgpl3+)
    (properties '((upstream-name . "oxygen-icons5")))))

(define-public prison
  (package
    (name "prison")
    (version "5.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "0spkm5j2qmwl374xrbm054rjv4rcqwsz0wl5v2nyrfq2ma4bzzw5"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("libdmtx" ,libdmtx)
       ("qrencode" ,qrencode)
       ("qtbase" ,qtbase))) ;; TODO: rethink: nix propagates this
    (home-page "https://api.kde.org/frameworks/prison/html/index.html")
    (synopsis "Barcode generation abstraction layer")
    (description "Prison is a Qt-based barcode abstraction layer/library and
provides uniform access to generation of barcodes with data.")
    (license license:lgpl2.1+)))

(define-public qqc2-desktop-style
  (package
    (name "qqc2-desktop-style")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1sz2735fn5665pyb5v70y2fmw2mkziq83di4h06pwday9vf2r6l5"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kauth" ,kauth)
       ("kconfigwidgets" ,kconfigwidgets) ; optional
       ("kcoreaddons" ,kcoreaddons)
       ("kiconthemes" ,kiconthemes) ; optional
       ("kirigami" ,kirigami)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtquickcontrols2" ,qtquickcontrols2)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0i124kyhd1hfnajp0hnqma35ycr3bh60z39gc6byhxb75p59swnl"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "ctest" "."))))))
    (native-inputs
     `(("bison" ,bison)
       ("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("flex" ,flex)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("udev" ,eudev)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0gqxmyxmwn2rs9f8x2z8pfmbx0mvkyh7nalnsmfqkph8f0fja9ig"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("hunspell" ,hunspell)
       ;; TODO: hspell (for Hebrew), Voikko (for Finish)
       ("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Multi-language spell checker")
    (description "Sonnet is a plugin-based spell checking library for Qt-based
applications.  It supports several different plugins, including HSpell, Enchant,
ASpell and HUNSPELL.")
    (license license:lgpl2.1+)))

(define-public threadweaver
  (package
    (name "threadweaver")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0z2906nmamvcg85jip98wq834rjs3nc1qlv3q182gw1cnh4nbdag"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Helper for multithreaded programming")
    (description "ThreadWeaver is a helper for multithreaded programming.  It
uses a job-based interface to queue tasks and execute them in an efficient way.")
    (license license:lgpl2.1+)))


;; Tier 2
;;
;; Tier 2 frameworks additionally depend on tier 1 frameworks, but still have
;; easily manageable dependencies.

(define-public kactivities
  (package
    (name "kactivities")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "03h5r7z2a449hfvaj3zk1n7dhpj49liy5q60fb4nxz4yjkgca27s"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("boost" ,boost)
       ("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "08gkgmxaxqpavsqdmcgs4i8j63pg5qld7agd58irpw29pgq1fj9c"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("polkit-qt" ,polkit-qt)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "ctest" "."))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Execute actions as privileged user")
    (description "KAuth provides a convenient, system-integrated way to offload
actions that need to be performed as a privileged user to small set of helper
utilities.")
    (license license:lgpl2.1+)))

(define-public kcompletion
  (package
    (name "kcompletion")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "10ggypg09acc19gkvxsigfsaq8s5vqv64ada307blpzy8j74bisb"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Powerful autocompletion framework and widgets")
    (description "This framework helps implement autocompletion in Qt-based
applications.  It provides a set of completion-ready widgets, or can be
integrated it into your application's other widgets.")
    (license license:lgpl2.1+)))

(define-public kcontacts
  (package
    (name "kcontacts")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0xs5c5l4vck5i6879jax1nf93if02f9hyfkn60l36cxbphnbpw0h"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("xorg-server" ,xorg-server))) ; for the tests
    (inputs
     `(("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             ;; Xvfb doesn't have proper glx support and needs a pixeldepth
             ;; of 24 bit to avoid "libGL error: failed to load driver: swrast"
             ;;                    "Could not initialize GLX"
             (system (string-append (assoc-ref inputs "xorg-server")
                                    "/bin/Xvfb :1 -screen 0 640x480x24 &"))
             (setenv "DISPLAY" ":1")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xl3bzxfchfafcplil3g07gq1a3fnwx1i40bxp4jfsgb8d8slfwc"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Graceful handling of application crashes")
    (description "KCrash provides support for intercepting and handling
application crashes.")
    (license license:lgpl2.1+)))

(define-public kdoctools
  (package
    (name "kdoctools")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zna9szii2r9agwfqbglk904zzpr3fyrz6x5v2rxrm3c9m110sm4"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("karchive" ,karchive)
       ("ki18n" ,ki18n)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("perl" ,perl)
       ("perl-uri" ,perl-uri)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
                               ,(package-version docbook-xsl) "\n")))
             #t))
         (add-after 'install 'add-symlinks
           ;; Some package(s) (e.g. kdelibs4support) refer to this locale by a
           ;; different spelling.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((xsl (string-append (assoc-ref outputs "out")
                                       "/share/kf5/kdoctools/customization/xsl/")))
               (symlink (string-append xsl "pt_br.xml")
                        (string-append xsl "pt-BR.xml")))
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Create documentation from DocBook")
    (description "Provides tools to generate documentation in various format
from DocBook files.")
    (license license:lgpl2.1+)))

(define-public kfilemetadata
  (package
    (name "kfilemetadata")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "11wvddb7nqdyi5rin3xa0p32lmvc62yw47hwcdzc01lkfbxdn050"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-failing-test
           (lambda _
             ;; Blacklist a failing test-function. FIXME: Make it pass.
             ;; UserMetaDataWriterTest fails with getxattr("…/writertest.txt")
             ;; -> EOPNOTSUPP (Operation not supported)
             (with-output-to-file "autotests/BLACKLIST"
               (lambda _
                 (display "[testMimetype]\n*\n")
                 (display "[test]\n*\n")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (inputs
     `(("attr" ,attr)
       ;; TODO: EPub http://sourceforge.net/projects/ebook-tools
       ("karchive" ,karchive)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("qtmultimedia" ,qtmultimedia)
       ("qtbase" ,qtbase)
       ;; Required run-time packages
       ("catdoc" ,catdoc)
       ;; Optional run-time packages
       ("exiv2" ,exiv2)
       ("ffmpeg" ,ffmpeg)
       ("poppler-qt5" ,poppler-qt5)
       ("taglib" ,taglib)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Extract metadata from different fileformats")
    (description "KFileMetaData provides a simple library for extracting the
text and metadata from a number of different files.  This library is typically
used by file indexers to retrieve the metadata.  This library can also be used
by applications to write metadata.")
    (license (list license:lgpl2.0 license:lgpl2.1 license:lgpl3))))

(define-public kimageformats
  (package
    (name "kimageformats")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "15y339by7jvsbmfqb33s3ain7sfhx3789xy2xjpkh5f2f9v29r2b"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("karchive" ,karchive) ; for Krita and OpenRaster images
       ("openexr" ,openexr) ; for OpenEXR high dynamic-range images
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This test fails regularly (also at KDE CI, see
         ;; https://build.kde.org/job/Frameworks%20kimageformats%20kf5-qt5%20XenialQt5.7/6/testReport/)
         ;; delete offending portion
         (add-after 'unpack 'neuter-read-xcf-test
           (lambda _
             (delete-file "autotests/read/xcf/simple-rgba-gimp-2.8.10.png")
             (delete-file "autotests/read/xcf/simple-rgba-gimp-2.8.10.xcf")))
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             (setenv "QT_PLUGIN_PATH"
                     (string-append (getcwd) "/bin:"
                                    (getenv "QT_PLUGIN_PATH")))
             #t)))
       ;; FIXME: The header files of ilmbase (propagated by openexr) are not
       ;; found when included by the header files of openexr, and an explicit
       ;; flag needs to be set.
       #:configure-flags
       (list (string-append "-DCMAKE_CXX_FLAGS=-I"
                            (assoc-ref %build-inputs "ilmbase")
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0pg055ampm69yr1vn03s1ys3p18raj8f5pcg0hg9hb4sjkyrr3pn"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Widgets for showing progress of asynchronous jobs")
    (description "KJobWIdgets provides widgets for showing progress of
asynchronous jobs.")
    (license license:lgpl2.1+)))

(define-public knotifications
  (package
    (name "knotifications")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rknfn50kq6ng7gclgg0n4qyhv2grc7xcf3gcax3bbyy46gajsl8"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("qttools" ,qttools)))
    (inputs
     `(("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qtspeech" ,qtspeech)
       ;; TODO: Think about adding dbusmenu-qt5 from
       ;; https://launchpad.net/libdbusmenu-qt
       ("qtx11extras" ,qtx11extras)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             #t))
         (replace 'check
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "ctest" "."))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Desktop notifications")
    (description "KNotification is used to notify the user of an event.  It
covers feedback and persistent events.")
    (license license:lgpl2.1+)))

(define-public kpackage
  (package
    (name "kpackage")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1h57kicsjkisysd2mmd5q9q5m421xqcv3p8vkjlqxkph5sa7y2q5"))
              ;; Default to: external paths/symlinks can be followed by a
              ;; package
              (patches (search-patches "kpackage-allow-external-paths.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("karchive" ,karchive)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kdoctools" ,kdoctools)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             ;; Make QDirIterator follow symlinks
             (substitute* '("src/kpackage/packageloader.cpp"
                            "src/kpackage/private/packagejobthread.cpp")
               (("^\\s*(const QDirIterator::IteratorFlags flags = QDirIterator::Subdirectories)(;)" _ a b)
                (string-append a " | QDirIterator::FollowSymlinks" b))
               (("^\\s*(QDirIterator it\\(.*, QDirIterator::Subdirectories)(\\);)" _ a b)
                (string-append a " | QDirIterator::FollowSymlinks" b)))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda _
             ;; /bin/ls doesn't exist in the build-container use /etc/passwd
             (substitute* "autotests/packagestructuretest.cpp"
               (("(addDirectoryDefinition\\(\")bin(\".*\")bin(\".*\")bin\""
                 _ a b c)
                (string-append a "etc" b "etc" c "etc\""))
               (("filePath\\(\"bin\", QStringLiteral\\(\"ls\"))")
                "filePath(\"etc\", QStringLiteral(\"passwd\"))")
               (("\"/bin/ls\"") "\"/etc/passwd\""))
             #t))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Installation and loading of additional content as packages")
    (description "The Package framework lets the user install and load packages
of non binary content such as scripted extensions or graphic assets, as if they
were traditional plugins.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kpty
  (package
    (name "kpty")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "10v61016kd1q41n4dwac1nsiixji64i5y501nx0hphpjkjbaklkr"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ;; TODO: utempter, for managing UTMP entries
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; FIXME: 1/1 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "autotests/kptyprocesstest.cpp"
               (("/bin/bash") (which "bash")))
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Interfacing with pseudo terminal devices")
    (description "This library provides primitives to interface with pseudo
terminal devices as well as a KProcess derived class for running child processes
and communicating with them using a pty.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kunitconversion
  (package
    (name "kunitconversion")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0yzq2jxja9wnhz91cyli2mnw32xynnzbf1lr0hk6vi8a1w2mkrzz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-a-failing-test-case
           (lambda _
             ;; FIXME: Re-enable this test-case. It was committed with the
             ;; message: "tsan says it's clean, apart from issues in Qt
             ;; (reported upstream)"
             (substitute* "autotests/convertertest.cpp"
               (("const int numThreads = 2") "const int numThreads = 0")))))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1d0k6x11giylfkr183sm307n4v6rjpwkqp7y9wfhimjrcprwf2g6"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcodecs" ,kcodecs)
       ("qtbase" ,qtbase)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1qhqv19glg98146cyc7ykq02b5b9m936xcml1w1p8fx32cp2zbch"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kfilemetadata" ,kfilemetadata)))
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kbookmarks" ,kbookmarks)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kidletime" ,kidletime)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("lmdb" ,lmdb)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t))
         (add-after 'unpack 'remove-failing-test
           ;; This test fails on i686 and aarch64
           (lambda _
             (substitute* "autotests/unit/file/CMakeLists.txt"
               (("^\\s*ecm_add_test\\(.* TEST_NAME metadatamovertest .*" line)
                (string-append "# " line)))
             #t))
         (replace 'check
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "ctest" "."))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "File searching and indexing")
    (description "Baloo provides file searching and indexing.  It does so by
maintaining an index of the contents of your files.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kactivities-stats
  (package
    (name "kactivities-stats")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "187fyl92pnkn317pi1pkmf7bv5z6aqdhqa6b8531xgl06gyigy45"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("boost" ,boost)
       ("kactivities" ,kactivities)
       ("kconfig" ,kconfig)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Access usage statistics collected by the activity manager")
    (description "The KActivitiesStats library provides a querying mechanism for
the data that the activitiy manager collects - which documents have been opened
by which applications, and what documents have been linked to which activity.")
    ;; triple licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+ license:lgpl3+))))

(define-public kbookmarks
  (package
    (name "kbookmarks")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "011r2ms57ycz5kma0q35cji4isj0lskrxrambk11jibs1xjdis2v"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kwidgetsaddons" ,kwidgetsaddons)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kiconthemes" ,kiconthemes)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Bookmarks management library")
    (description "KBookmarks lets you access and manipulate bookmarks stored
using the XBEL format.")
    (license license:lgpl2.1+)))

(define-public kcmutils
  (package
    (name "kcmutils")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1q12n9sfgrm2f8bspsl5i9gbv4jwl25w5id9m9rnnr87hck35g68"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kconfigwidgets" ,kconfigwidgets)
       ("kservice" ,kservice)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "src/kpluginselector.cpp"
               ;; make QDirIterator follow symlinks
               (("^\\s*(QDirIterator it\\(.*, QDirIterator::Subdirectories)(\\);)" _ a b)
                (string-append a " | QDirIterator::FollowSymlinks" b)))
             (substitute* "src/kcmoduleloader.cpp"
               ;; print plugin name when loading fails
               (("^\\s*(qWarning\\(\\) << \"Error loading) (plugin:\")( << loader\\.errorString\\(\\);)" _ a b c)
                (string-append a " KCM plugin\" << mod.service()->library() << \":\"" c)))
             #t)))))
    (inputs
     `(("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kdeclarative" ,kdeclarative)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kpackage" ,kpackage)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Utilities for KDE System Settings modules")
    (description "KCMUtils provides various classes to work with KCModules.
KCModules can be created with the KConfigWidgets framework.")
    (license license:lgpl2.1+)))

(define-public kconfigwidgets
  (package
    (name "kconfigwidgets")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "098xdfvnyz9bdkc6iyq5r2s4vkfdhbrri4015yzbs73j4f2wcxz5"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kwidgetsaddons" ,kwidgetsaddons)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ;; todo: PythonModuleGeneration
       ("qtbase" ,qtbase)
       ("qttools" ,qttools)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "src/khelpclient.cpp"
               ;; make QDirIterator follow symlinks
               (("^\\s*(QDirIterator it\\(.*, QDirIterator::Subdirectories)(\\);)" _ a b)
                (string-append a " | QDirIterator::FollowSymlinks" b)))
             #t))
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "027wnrmr79fxkf4jpdpfhp7smdai2wn9y71l1h4kgqgwfq8anwy8"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kconfig" ,kconfig)
       ("kpackage" ,kpackage)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kglobalaccel" ,kglobalaccel)
       ("kguiaddons" ,kguiaddons)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libepoxy" ,libepoxy)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server, setting
             ;; QT_QPA_PLATFORM=offscreen does not suffice.
             (system (string-append (assoc-ref inputs "xorg-server")
                                    "/bin/Xvfb :1 -screen 0 640x480x24 &"))
             (setenv "DISPLAY" ":1")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0kjr6ff1g6xyas5si9b7zilcqcx4mgfkzfpq2c59fz1wd8q8fl05"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("kinit" ,kinit)
       ("kservice" ,kservice)
       ("qtbase" ,qtbase)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/portingAids/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1w7glszd82iyw6kxzmp0568rm3qfadi7vw7gfxg4c15w5ikkvxn9"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kdoctools" ,kdoctools)
       ("qtbase" ,qtbase)
       ;; optional:
       ("kcompletion" ,kcompletion)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("kio" ,kio)
       ("kplotting" ,kplotting)
       ("ktextwidgets" ,ktextwidgets)
       ("kdewebkit" ,kdewebkit)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtwebkit" ,qtwebkit)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "038m0sdpnsy50m3g78sfg7wk26933rh3578qsnapw2cyimx8ypxx"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kpty" ,kpty)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kservice" ,kservice)
       ("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "User interface for running shell commands with root privileges")
    (description "KDESU provides functionality for building GUI front ends for
(password asking) console mode programs.  kdesu and kdessh use it to interface
with su and ssh respectively.")
    (license license:lgpl2.1+)))

(define-public kdewebkit
  (package
    (name "kdewebkit")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/portingAids/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1s3bmn1ck7xvc7a6gv7ywv9161hdahiiw20aq88s4qkm2s5wyy6v"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kio" ,kio)
       ("kjobwidgets" ,kjobwidgets)
       ("kparts" ,kparts)
       ("kservice" ,kservice)
       ("kwallet" ,kwallet)
       ("qtbase" ,qtbase)
       ("qttools" ,qttools)
       ("qtwebkit" ,qtwebkit)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Integration for QtWebKit")
    (description "This library provides KDE integration of the HTML rendering
engine WebKit via QtWebKit.")
    (license license:lgpl2.1+)))

(define-public kemoticons
  (package
    (name "kemoticons")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0li1lz5kqlczvgq6b2x3nkr0hl96a337ab83fq9wxp5sx440d6sd"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kservice" ,kservice)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("karchive" ,karchive)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xpfvwnrj81mk3di02n37b469gxzmnk89icmcz6wwyk54m86fw76"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcrash" ,kcrash)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kservice" ,kservice)
       ("kwindowsystem" ,kwindowsystem)
       ("libxcb" ,libxcb)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("xcb-util-keysyms" ,xcb-util-keysyms)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Global desktop keyboard shortcuts")
    (description "KGlobalAccel allows you to have global accelerators that are
independent of the focused window.  Unlike regular shortcuts, the application's
window does not need focus for them to be activated.")
    (license license:lgpl2.1+)))

(define-public kiconthemes
  (package
    (name "kiconthemes")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "18p4np4chykpy2ck8c76999nh8gdj75hjfakg1pakdqhlash9q6w"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kcoreaddons" ,kcoreaddons)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("ki18n" ,ki18n)
       ("kitemviews" ,kitemviews)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qttools" ,qttools)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "XDG_DATA_DIRS"
                     (string-append (assoc-ref inputs "shared-mime-info")
                                    "/share"))
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Icon GUI utilities")
    (description "This library contains classes to improve the handling of icons
in applications using the KDE Frameworks.")
    (license license:lgpl2.1+)))

(define-public kinit
  (package
    (name "kinit")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "06p0i9674nzam0f4ac2jrl9pgkycz0lc0kdn24ds2rlh85cqvjwx"))
              ;; Use the store paths for other packages and dynamically loaded
              ;; libs
              (patches (search-patches "kinit-kdeinit-extra_libs.patch"
                                       "kinit-kdeinit-libpath.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Set patched-in values:
             (substitute* "src/kdeinit/kinit.cpp"
               (("GUIX_PKGS_KF5_KIO") (assoc-ref inputs "kio"))
               (("GUIX_PKGS_KF5_PARTS") (assoc-ref inputs "kparts"))
               (("GUIX_PKGS_KF5_PLASMA") (assoc-ref inputs "plasma-framework")))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "KDEINIT5_LIBRARY_PATH")
            (files '("lib/")))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdoctools" ,kdoctools)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kparts" ,kparts)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libcap" ,libcap) ; to install start_kdeinit with CAP_SYS_RESOURCE
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "048gq6j8di6nzsacpr8j40kw6y5607zgwqr21ahcnnb0gd2sly4l"))
              (patches (search-patches "kio-search-smbd-on-PATH.patch"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kbookmarks" ,kbookmarks)
       ("kconfig" ,kconfig)
       ("kcompletion" ,kcompletion)
       ("kcoreaddons" ,kcoreaddons)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kxmlgui" ,kxmlgui)
       ("solid" ,solid)))
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(;; TODO:  LibACL , <ftp://oss.sgi.com/projects/xfs/cmd_tars>
       ("krb5" ,mit-krb5)
       ("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("kiconthemes" ,kiconthemes)
       ("ki18n" ,ki18n)
       ("knotifications" ,knotifications)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("qtbase" ,qtbase)
       ("qtscript" ,qtscript)
       ("qttools" ,qttools)
       ("qtx11extras" ,qtx11extras)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f ; FIXME: 41/50 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             ;; Better error message (taken from NixOS)
             (substitute* "src/kiod/kiod_main.cpp"
               (("(^\\s*qCWarning(KIOD_CATEGORY) << \"Error loading plugin:\")( << loader.errorString();)" _ a b)
                (string-append a "<< name" b)))
             #t))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t))
         (add-after 'install 'add-symlinks
           ;; Some package(s) (e.g. bluedevil) refer to these service types by
           ;; the wrong name.  I would prefer to patch those packages, but I
           ;; cannot find the files!
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((kst5 (string-append (assoc-ref outputs "out")
                                        "/share/kservicetypes5/")))
               (symlink (string-append kst5 "kfileitemactionplugin.desktop")
                        (string-append kst5 "kfileitemaction-plugin.desktop"))))))))
    ;;(replace 'check
    ;;  (lambda _
    ;;    (setenv "DBUS_FATAL_WARNINGS" "0")
    ;;    (zero? (system* "dbus-launch" "ctest" ".")))))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0pkm7qapkd9xypmhsjfwlgq0l4nfgya5b23lv3zkci8lrbssaqsv"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("attica" ,attica)
       ("kservice" ,kservice)
       ("kxmlgui" ,kxmlgui)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kjobwidgets" ,kjobwidgets)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _ ; XDG_DATA_DIRS isn't set
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Framework for downloading and sharing additional application data")
    (description "The KNewStuff library implements collaborative data sharing
for applications.  It uses libattica to support the Open Collaboration Services
specification.")
    (license license:lgpl2.1+)))

(define-public knotifyconfig
  (package
    (name "knotifyconfig")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1d90gsnxy3kqd6f1bkhflrdwwx3w65sw4c7nfpkdxxhlqhzc9zr4"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("knotifications" ,knotifications)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Configuration dialog for desktop notifications")
    (description "KNotifyConfig provides a configuration dialog for desktop
notifications which can be embedded in your application.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kparts
  (package
    (name "kparts")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0md0349r4mdm2r04p5s3pgm17v4w40r3kz58lzp6qfcw25b969nw"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kio" ,kio)
       ("ktextwidgets" ,ktextwidgets)
       ("kxmlgui" ,kxmlgui)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("solid" ,solid)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Plugin framework for user interface components")
    (description "This library implements the framework for KDE parts, which are
widgets with a user-interface defined in terms of actions.")
    (license license:lgpl2.1+)))

(define-public kpeople
  (package
    (name "kpeople")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1q2hh2i8hd638p907g0srdxmxm9h2ay91dmhslqzcgwnlhln4gfl"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (arguments
     `(#:tests? #f ; FIXME: 1/3 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0gjqwzsg865gha8k7hda28hsrbfzhw9jz610lkqff5713lqbcn5a"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("plasma-framework" ,plasma-framework)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kpackage" ,kpackage)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)
       ("threadweaver" ,threadweaver)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths-for-test
           ;; This test tries to access paths like /home, /usr/bin and /bin/ls
           ;; which don't exist in the build-container. Change to existing paths.
           (lambda _
             (substitute* "autotests/runnercontexttest.cpp"
               (("/home\"") "/tmp\"") ;; single path-part
               (("//usr/bin\"") (string-append (getcwd) "\"")) ;; multiple path-parts
               (("/bin/ls" path)
                (string-append (assoc-ref %build-inputs "coreutils") path)))))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             ;; Blacklist some failing test-functions. FIXME: Make them pass.
             (with-output-to-file "bin/BLACKLIST"
               (lambda _
                 (display "[testMatch]\n*\n")
                 (display "[testMulti]\n*\n")))
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rdapw9wirfq6pyaza0k7sg6q21rv3cfyalpzrwfrk5gkgpqxzwk"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)))
    (native-inputs
     `(("bison" ,bison)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("flex" ,flex)))
    (inputs
     `(("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; FIXME: 6/10 tests fail.
       #:phases
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
                "QString resolved = QDir::cleanPath(dir);"))
             #t))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    "ktexteditor-" version ".tar.xz"))
              (sha256
               (base32
                "0vfl6rr3624n8v29z07z5c0nh5af5kfg28cnp5ksfvfnssxfzfqa"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kparts" ,kparts)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(;; TODO: editor-config
       ("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kguiaddons" ,kguiaddons)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("ksyntaxhighlighting" ,ksyntaxhighlighting)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("libgit2" ,libgit2)
       ("perl" ,perl)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ("solid" ,solid)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f ; FIXME: 2/54 tests fail: Cannot find fontdirectory qtbase/lib/font
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'setup
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "XDG_DATA_DIRS" ; FIXME build phase doesn't find parts.desktop
                     (string-append (assoc-ref inputs "kparts") "/share"))
             #t))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t))
         (add-after 'install 'add-symlinks
           ;; Some package(s) (e.g. plasma-sdk) refer to these service types
           ;; by the wrong name.  I would prefer to patch those packages, but
           ;; I cannot find the files!
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((kst5 (string-append (assoc-ref outputs "out")
                                        "/share/kservicetypes5/")))
               (symlink (string-append kst5 "ktexteditorplugin.desktop")
                        (string-append kst5 "ktexteditor-plugin.desktop"))
               #t))))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1qb1mad5bg19xwykzpwk2b3s505ka4jkg0fsi56ri57wq8gv4qha"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("ki18n" ,ki18n)
       ("sonnet" ,sonnet)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kiconthemes" ,kiconthemes)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)
       ("qtspeech" ,qtspeech)
       ("qttools" ,qttools)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Text editing widgets")
    (description "KTextWidgets provides widgets for displaying and editing text.
It supports rich text as well as plain text.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kwallet
  (package
    (name "kwallet")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1dp3mrgl0h6wqp4wcqpvnvkacwkrrn5lrv6bdl008kx1y594s28g"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("gpgme" ,gpgme)
       ("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("kiconthemes" ,kiconthemes)
       ("ki18n" ,ki18n)
       ("knotifications" ,knotifications)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("libgcrypt" ,libgcrypt)
       ("phonon" ,phonon)
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Safe desktop-wide storage for passwords")
    (description "This framework contains an interface to KWallet, a safe
desktop-wide storage for passwords and the kwalletd daemon used to safely store
the passwords on KDE work spaces.")
    (license license:lgpl2.1+)))

(define-public kxmlgui
  (package
    (name "kxmlgui")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0x0mfapgl8w4xal8lamh3v27a9kdb7qyaxdn48gja4x85pkvi87i"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("attica" ,attica)
       ("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kcoreaddons" ,kcoreaddons)
       ("kglobalaccel" ,kglobalaccel)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)
       ("qttools" ,qttools)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f ; FIXME: 1/5 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0k82cyxjipqpky7gs0zb5s1bc6jf22d5vwpwyiqjy32da63mc89c"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kio" ,kio)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
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
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0p0ziqw70lklx59d0nij3p9yi1vdacdfyrrp7zf74iwxd7598hci"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kpackage" ,kpackage)
       ("kservice" ,kservice)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kactivities" ,kactivities)
       ("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kdoctools" ,kdoctools)
       ("kglobalaccel" ,kglobalaccel)
       ("kguiaddons" ,kguiaddons)
       ("kiconthemes" ,kiconthemes)
       ("kirigami" ,kirigami)
       ("kitemviews" ,kitemviews)
       ("kio" ,kio)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("knotificantions" ,knotifications)
       ("kwayland" ,kwayland)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)))
    (arguments
     `(#:tests? #f ; FIXME: 9/15 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Libraries, components and tools of Plasma workspaces")
    (description "The plasma framework provides QML components, libplasma and
script engines.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

;; This version of kdbusaddons does not use kinit as an input, and is used to
;; build kinit-bootstrap, as well as bootstrap versions of all kinit
;; dependencies which also rely on kdbusaddons.
(define kdbusaddons-bootstrap
  (package
    (inherit kdbusaddons)
    (source (origin
              (inherit (package-source kdbusaddons))
              (patches '())))
    (inputs (alist-delete "kinit" (package-inputs kdbusaddons)))
    (arguments
     (substitute-keyword-arguments (package-arguments kdbusaddons)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'patch-source)))))))

(define kinit-bootstrap
  ((package-input-rewriting `((,kdbusaddons . ,kdbusaddons-bootstrap))) kinit))


;; Tier 4
;;
;; Tier 4 frameworks can be mostly ignored by application programmers; this
;; tier consists of plugins acting behind the scenes to provide additional
;; functionality or platform integration to existing frameworks (including
;; Qt).

(define-public kde-frameworkintegration
  (package
    (name "kde-frameworkintegration")
    (version "5.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/frameworks/"
                    (version-major+minor version) "/"
                    "frameworkintegration-" version ".tar.xz"))
              (sha256
               (base32
                "0n8n6lnzswi2q848qy2b4rcvkd0y1dywlk6hdmcnn8dfbxy6b050"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    ;; TODO: Optional packages not yet in Guix: packagekitqt5, AppStreamQt
    (inputs
     `(("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("knewstuff" ,knewstuff)
       ("knotificantions" ,knotifications)
       ("kpackage" ,kpackage)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; Make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "17mywcmgp1yzdipwwaypbkxiyybk1w0sps50jv1bl0f23ya19mlc"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("dbus" ,dbus)
       ("docbook-xml" ,docbook-xml-4.4) ; optional
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("perl" ,perl)
       ("perl-uri" ,perl-uri)
       ("pkg-config" ,pkg-config)
       ("shared-mime-info" ,shared-mime-info)
       ("kjobwidgets" ,kjobwidgets) ;; required for running the tests
       ("strace" ,strace)
       ("tzdata" ,tzdata)))
    (propagated-inputs
     ;; These are required to be installed along with this package, see
     ;; lib64/cmake/KF5KDELibs4Support/KF5KDELibs4SupportConfig.cmake
     `(("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdesignerplugin" ,kdesignerplugin)
       ("kdoctools" ,kdoctools)
       ("kemoticons" ,kemoticons)
       ("kguiaddons" ,kguiaddons)
       ("kiconthemes" ,kiconthemes)
       ("kinit" ,kinit)
       ("kitemmodels" ,kitemmodels)
       ("knotifications" ,knotifications)
       ("kparts" ,kparts)
       ("ktextwidgets" ,ktextwidgets)
       ("kunitconversion" ,kunitconversion)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)))
    (inputs
     `(("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kded" ,kded)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("libsm" ,libsm)
       ("networkmanager-qt" ,networkmanager-qt)
       ("openssl" ,openssl)
       ("qtsvg" ,qtsvg)
       ("qttools" ,qttools)
       ("qtx11extras" ,qtx11extras)))
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
                "xml/dtd/docbook\n"))
             #t))
         (delete 'check)
         (add-after 'install 'check-post-install
           (lambda* (#:key inputs tests? #:allow-other-keys)
             (setenv "HOME" (getcwd))
             (setenv "TZDIR"    ; KDateTimeTestsome needs TZDIR
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
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
             ;; kuniqueapptest hangs. FIXME: Make this test pass.
             (invoke "dbus-launch" "ctest" "."
                     "-E" "kstandarddirstest|kuniqueapptest"))))))
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
    (version "5.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "15f77r6dxkkvi2vxvxlwa7qz3whmz229g79bgadiwffmzxja0ywd"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("perl" ,perl)))
    (inputs
     `(("giflib" ,giflib)
       ("gperf" ,gperf)
       ("karchive" ,karchive)
       ("kcodecs" ,kcodecs)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kjs" ,kjs)
       ("knotifications" ,knotifications)
       ("kparts" ,kparts)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("openssl" ,openssl)
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; Make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "16zsan9c8kba5m70h34y544id7gig2bcbcs4s90s27pckijkkarp"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("pcre" ,pcre)
       ("qtbase" ,qtbase)))
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
    (version "5.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "00sr6lv1nai2y6aawvqwnx4pbx8pw9b1jrpa0klc014gc42l6i7b"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("qttools" ,qttools)))
    (inputs
     `(("ki18n" ,ki18n)
       ("kjs" ,kjs)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Frameworks 5 embedded Javascript engine for Qt")
    (description "KJSEmbed provides a method of binding Javascript objects to
QObjects, so you can script your applications.")
    (license license:lgpl2.1+)))

(define-public kmediaplayer
  (package
    (name "kmediaplayer")
    (version "5.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "19v53h3lkys3ryrjacrdng9ak91g03b9s986xhnw1r84zy242kdm"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("qttools" ,qttools)))
    (inputs
     `(("kcompletion" ,kcompletion)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kparts" ,kparts)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; Make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
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
    (version "5.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/frameworks/"
             (version-major+minor version) "/portingAids/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "0paipgkxbm6vwgqj38rlr37vgmf1rx0cl2mka8zdbndl76ng5hgc"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("qttools" ,qttools)))
    (inputs
     `(("kcompletion" ,kcompletion)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kparts" ,kparts)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtscript" ,qtscript)))
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
