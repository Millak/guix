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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cryptsetup)
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
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages kde-plasma)
  ;; Including this module breaks the build.
  ;#:use-module ((gnu packages kde-systemtools) #:select (dolphin))
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public baloo-widgets
  (package
    (name "baloo-widgets")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/baloo-widgets-" version ".tar.xz"))
       (sha256
        (base32 "06238jvb44118bapgkk1yg6mn1kgmn8xx3ayfxy36bx0pl7nii5p"))))
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

(define-public crow-translate
  (package
    (name "crow-translate")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/crow-translate/" version
                           "/crow-translate-v" version ".tar.gz"))
       (sha256
        (base32 "18f7i5sxrvqp6h7zj77sdxyy9rlbw0rv3w7akf1j14072ala9bwc"))))
    (build-system qt-build-system)
    (arguments '(#:tests? #f)) ; there are no tests.
    (inputs
     (list qtbase-5
           qtx11extras
           qtsvg-5
           qtmultimedia-5
           tesseract-ocr
           kwayland-5))
    (native-inputs
     (list pkg-config
           extra-cmake-modules
           qttools-5))
    (home-page "https://invent.kde.org/office/crow-translate")
    (synopsis "Application for translating text")
    (description
     "Crow Translate is an application written in C++/Qt for translating
and speaking text which relies on Mozhi to interface with various
translation engines.")
    (license license:gpl3+)))

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
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/grantleetheme-" version ".tar.xz"))
       (sha256
        (base32 "0fsp9698wh2h53qglfg6576m309yb91s5ix4sdzckyilh31y1j2y"))))
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
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akregator-" version ".tar.xz"))
       (sha256
        (base32 "07flc3617px9w1c729p0lsixf1g0h297hkbip259ykkbwxizn71q"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
             (add-after 'install 'wrap-qt-process-path
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (bin (string-append out "/bin/akregator"))
                        (qt-process-path
                         (search-input-file
                          inputs "/lib/qt6/libexec/QtWebEngineProcess")))
                   (wrap-program bin
                     `("QTWEBENGINEPROCESS_PATH" = (,qt-process-path)))))))))
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

(define-public kdenlive
  (package
    (name "kdenlive")
    (version "24.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/multimedia/kdenlive")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m1s27vska60qswrqfnjlrj9p787n5p8zx7gldn95sj1mdw9s7cr"))))
    (build-system qt-build-system)
    (arguments
     ;; XXX: there is a single test that spawns other tests and
     ;; 1/3 tests failed and 1/327 assertions failed.  It seems
     ;; that individual tests can't be skipped.
     (list
      #:qtbase qtbase
      #:configure-flags #~(list "-DBUILD_TESTING=off")
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-executable
            (lambda _
              (let* ((ffmpeg #$(this-package-input "ffmpeg"))
                     (frei0r #$(this-package-input "frei0r-plugins"))
                     (ladspa #$(this-package-input "ladspa"))
                     (qtbase #$(this-package-input "qtbase")))
                (wrap-program (string-append #$output "/bin/kdenlive")
                  `("PATH" ":" prefix
                    ,(list (string-append ffmpeg "/bin")))
                  `("FREI0R_PATH" ":" =
                    (,(string-append frei0r "/lib/frei0r-1")))
                  `("LADSPA_PATH" ":" =
                    (,(string-append ladspa "/lib/ladspa")))
                  `("QT_QPA_PLATFORM_PLUGIN_PATH" ":" =
                    (,(string-append qtbase "/lib/qt6/plugins/platforms")))
                  `("MLT_PREFIX" ":" =
                    (,#$(this-package-input "mlt"))))))))))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config qttools))
    (inputs
     (list bash-minimal
           breeze                       ; make dark them available easily
           breeze-icons                 ; recommended icon set
           ffmpeg
           frei0r-plugins
           karchive
           kcrash
           kdbusaddons
           kdeclarative
           kdoctools
           kfilemetadata
           kguiaddons
           kiconthemes
           kirigami
           knewstuff
           knotifications
           knotifyconfig
           kparts
           kplotting
           ktextwidgets
           ladspa
           mlt
           purpose
           qqc2-desktop-style
           qtbase
           qtdeclarative
           qtmultimedia
           qtnetworkauth
           qtsvg
           shared-mime-info))
    (home-page "https://kdenlive.org")
    (synopsis "Non-linear video editor")
    (description "Kdenlive is an acronym for KDE Non-Linear Video Editor.

Non-linear video editing is much more powerful than beginner's (linear)
editors, hence it requires a bit more organization before starting.  However,
it is not reserved to specialists and can be used for small personal
projects.")
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

(define-public kapptemplate
  (package
    (name "kapptemplate")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kapptemplate-" version ".tar.xz"))
       (sha256
        (base32 "0mgpk6879dprhpxmbdgbb6sz3ik9ycav4sihh20qmsgj09h8qp3g"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list "-DBUILD_TESTING=ON")))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kcompletion
           kconfigwidgets
           kcoreaddons
           ki18n
           kio))
    (home-page "https://apps.kde.org/kapptemplate/")
    (synopsis "Factory for easy creation of KDE/Qt components and programs")
    (description "KAppTemplate is an application to start development quickly
using existing templates providing basic repeatedly written code and a proper
structure.  It features:

@itemize
@item Templates for C++, Ruby, Python and PHP
@item Categories
@item Templates for different build-systems and frameworks
@item Templates especially for KDE-development (plugins for Plasma, QtQuick
 KTextEditor, KRunner, Akonadi)
@item New templates using space holders and a simple CMake-command
@item Integration into KDevelop
@end itemize")
    (license license:gpl2+)))

(define-public kdevelop
  (package
    (name "kdevelop")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdevelop-" version ".tar.xz"))
       (sha256
        (base32 "10z53ri4g3b199cv9394pflgdlcnr9y2gh7xr4isl2kpn26jiwh0"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config shared-mime-info qttools-5))
    (inputs (list boost
                  clang
                  grantlee
                  karchive-5
                  kcmutils-5
                  kcrash-5
                  kdeclarative-5
                  kdoctools-5
                  kguiaddons-5
                  ki18n-5
                  kiconthemes-5
                  kio-5 ;; not checked as requirement
                  kitemmodels-5
                  kitemviews-5
                  kjobwidgets-5
                  knotifications-5
                  knotifyconfig-5
                  kparts-5
                  kservice-5
                  ksyntaxhighlighting-5
                  ktexteditor-5
                  kwindowsystem-5
                  kxmlgui-5
                  libkomparediff2
                  breeze-icons
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols-5 ;; not checked as requirement
                  qtquickcontrols2-5 ;; not checked as requirement
                  qtwebengine-5
                  threadweaver-5
                  ;; recommendes
                  astyle
                  kdevelop-pg-qt

                  ;; optional
                  apr ; required for subversion support
                  apr-util ; required for subversion support
                  attica-5
                  kconfigwidgets-5
                  knewstuff-5
                  krunner-5
                  ;; TODO: OktetaGui, OktetaKastenControllers
                  plasma-framework
                  ;; TODO: purpose
                  sonnet-5
                  subversion))
    ;; run-time packages - TODO
    ;; ClazyStandalone
    ;; Cppcheck
    ;; heaptrack
    ;; heaptrack_gui
    ;; meson
    (arguments
     (list #:tests? #f ;; there are some issues with the test suite
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'add-include-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "plugins/clang/Locate_CLANG_BUILTIN_DIR.cmake"
                     (("\"\\$[{]CLANG_INCLUDE_DIRS[}]\"" line)
                      (string-append
                       line " \""
                       (assoc-ref inputs "clang") "/lib\""))))))))
    (home-page "https://kdevelop.org")
    (synopsis "IDE for C, C++, Python, Javascript and PHP")
    (description "The KDevelop IDE provides semantic syntax highlighting, as
well as code navigation and completion for C, C++ (using Clang/LLVM), QML,
JavaScript, Python and PHP.  It also integrates with a debugger, different
build systems (CMake, QMake, custom Makefiles) and version control
software (Git, Subversion, Mercurial, CVS and Bazaar).")
    (license license:lgpl2.1+)))

(define-public kdevelop-pg-qt
  (package
    (name "kdevelop-pg-qt")
    (version "2.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/KDE/kdevelop-pg-qt")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kdi12phrl5afv41dy856g2vv3bp0a1b1vwp90h08wbqsfyy1zlm"))))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list qtbase-5))
    (build-system cmake-build-system)
    (home-page "https://kde.org")
    (synopsis "Parser generator library for KDevplatform")
    (description "KDevelop-PG-Qt is the parser generator used in KDevplatform
for some KDevelop language plugins (Ruby, PHP, CSS...).")
    (license license:lgpl2.0+)))

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
    (inputs (list kdsoap-qt6))
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

(define-public kio-extras
  (package
    (name "kio-extras")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0h720wcgsdx9h5vlr4flxrd3djmhwvlwkrf0yzwsf4amcb9wds8r"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME" (getcwd))
                              (setenv "TMPDIR" (getcwd))
                              (invoke "ctest" "-E" "(thumbnailtest|testkioarchive)")))))))
    (native-inputs (list extra-cmake-modules dbus kdoctools pkg-config qttools))
    ;; TODO: libappimage
    (inputs (list gperf
                  imath
                  plasma-activities
                  plasma-activities-stats
                  karchive
                  kbookmarks
                  kcmutils
                  kconfig
                  kconfigwidgets
                  kcoreaddons
                  kdnssd
                  kdbusaddons
                  kdsoap-qt6
                  kdsoap-ws-discovery-client
                  kguiaddons
                  ktextwidgets
                  ki18n
                  kio
                  ksyntaxhighlighting
                  libimobiledevice
                  libkexiv2
                  libmtp
                  libplist
                  libssh
                  libtirpc
                  openexr
                  phonon
                  qtbase
                  qt5compat
                  qcoro-qt6
                  qtsvg
                  samba
                  shared-mime-info
                  solid
                  taglib
                  zlib))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Additional components to increase the functionality of KIO")
    (description
     "This package provides additional components to increase
the functionality of the KDE resource and network access abstractions.")
    (license license:lgpl2.0+)))

(define-public kio-fuse
  (package
    (name "kio-fuse")
    (version "5.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/kio-fuse/kio-fuse-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0jz9952dd20sw0c25pyn2l86nmc1s5l42gxk4js1jnkx4a0la43x"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")
      #:phases #~(modify-phases %standard-phases
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         (setenv "HOME" (getcwd))
                         (setenv "XDG_RUNTIME_DIR" (getcwd))
                         (setenv "QT_QPA_PLATFORM" "offscreen")
                         (invoke "dbus-launch" "ctest" "-E"
                                 "(fileopstest-cache|fileopstest-filejob)")))))))
    (native-inputs (list dbus extra-cmake-modules pkg-config))
    (inputs (list fuse kio kcoreaddons qtbase))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "FUSE Interface for KIO")
    (description "This package provides FUSE Interface for KIO.")
    (license license:lgpl2.1+)))

(define-public kirigami-addons
  (package
    (name "kirigami-addons")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/libraries/kirigami-addons")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w2pxrbvzzpafh1x38rmjqjrpyf1zhkwv354ihpbwkn7grr8d2hy"))))
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
     `(#:configure-flags (list "-DBUILD_TESTS=ON"))) ; disabled by default
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
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/de/kcolorchooser/")
    (synopsis "Color selector utility")
    (description "KColorChooser is a utility to select a color.")
    (license license:expat)))

(define-public kolourpaint
  (package
    (name "kolourpaint")
    (version "24.05.2")
    (source
     (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/release-service/" version
                            "/src/kolourpaint-" version ".tar.xz"))
        (sha256
         (base32 "1fi1y74s2lnrxdnr9ym4b1ilj9qi019gaavfv0sq1xg8ppbpgcbr"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kguiaddons
           ki18n
           kio
           kjobwidgets
           ktextwidgets
           kwidgetsaddons
           kxmlgui))
    (arguments (list #:qtbase qtbase))
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
       #:configure-flags (list "-DBUILD_TESTING=OFF -DCMAKE_CXX_FLAGS=-fPIC")
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
           quazip-0
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

(define-public massif-visualizer
  (package
    (name "massif-visualizer")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/massif-visualizer-" version ".tar.xz"))
       (sha256
        (base32 "09da7qlrq21rb2971wx43790ki6hk7xn255j82kfmx2kp9ilwvxm"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config shared-mime-info))
    (inputs
     (list karchive
           kcoreaddons
           kparts
           kdiagram
           kgraphviewer
           kio
           ki18n
           qtsvg
           qt5compat))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/massif_visualizer/")
    (synopsis "Visualize massif data generated by Valgrind")
    (description
     "Massif Visualizer is a tool that visualizes massif data.
You run your application in Valgrind with @code{--tool=massif} and then open
the generated @file{massif.out.%pid} in the visualizer.  Gzip or Bzip2
compressed massif files can also be opened transparently.")
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

(define-public libkomparediff2
  (package
    (name "libkomparediff2")
    (version "24.05.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/release-service/" version
                            "/src/libkomparediff2-" version ".tar.xz"))
        (sha256
         (base32 "1g4zjsdd49n2kh5m8ijm9cm95wfn2rglgnmvvr0ap1iidy4843hy"))))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kcodecs-5
           kconfig-5
           kcoreaddons-5
           ki18n-5
           kio-5
           kxmlgui-5
           qtbase-5))
    (build-system cmake-build-system)
    (home-page "https://kde.org")
    (synopsis "Library to compare files and strings, used in Kompare and KDevelop")
    (description "Libkomparediff2 is a library to work with diffs and patches,
used in KDE development tools Kompare and KDevelop.")

    ;; GPL, some files are also licensed under LGPL or BSD, see COPYING in the
    ;; source archive
    (license (list license:gpl2+ license:lgpl2.0+ license:bsd-3))))

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

(define-public kommit
  (package
    (name "kommit")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/"
                                  name "/" name "-"
                                  "v" version ".tar.xz"))
              (sha256
               (base32
                "14gr0ms99il76k3yrdff2z4fj5pi5c613gk9n60gg66rmr7m3pnx"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list "-DQT_MAJOR_VERSION=6")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; FIXME: many test is fail, but look likes it can works.
                     (invoke "ctest" "-E"
                             "(difftest|clonedialogtest|tagtest|indextest|\
branchestest|configtest|stashtest|filetest|overlaytest|remotetest|clonetest|\
submoduletest|cachetest|switchtest)")))))))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list ;; module cyclic referencing
      (module-ref
       (resolve-interface
        '(gnu packages kde-systemtools))
       'dolphin)         ;for dolphin plugin
      kconfigwidgets
      kcoreaddons
      kcrash
      kdbusaddons
      ki18n
      kxmlgui
      kio
      ktextwidgets
      ktexteditor
      ksyntaxhighlighting
      libgit2-1.8))
    (home-page "https://apps.kde.org/kommit/")
    (synopsis "Git client for KDE")
    (description
     "Kommit is a git client for KDE.")
    (license license:gpl3+)))

(define-public kompare
  (package
    (name "kompare")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kompare-" version ".tar.xz"))
              (sha256
               (base32
                "13kvxa3l5hp9fi6ijy8vyzzm4ackrf09k6rm0nicb5z1s3iyyvxh"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools-5))
    (inputs
     (list libkomparediff2
           kcodecs-5
           kconfig-5
           kcoreaddons-5
           kiconthemes-5
           kjobwidgets-5
           kparts-5
           ktexteditor-5
           kwidgetsaddons-5))
    (home-page "https://apps.kde.org/kompare/")
    (synopsis "Graphical file differences tool")
    (description
     "Kompare is a program to view the differences between files.  Features
include:
@itemize
@item comparison of files or directories via a graphical interface,
@item bezier-based connection widget letting you see both source and destination,
@item graphical viewing of patch files in normal, context, unified and diff
 formats,
@item interactive application of differences,
@item network transparency,
@item ability to view plain-text diff output in embedded viewer,
@item navigation of multiple-file diffs with dockable navigation tree,
@item graphical interface for commonly used diff command line options,
@item switch source and destination
@item and diff statistics.
@end itemize")
    (license license:gpl3+)))

(define-public kopeninghours
  (package
    (name "kopeninghours")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "132ihgsv1l8nz24ycddfp146czhnfcgrjfnffjb91w5l4a5wv3k4"))))
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
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1inch8vnh96lbklrj9w6d7vyqnbaig8f0kgfr5k7i5983vqpsvlm"))))
    (build-system cmake-build-system)
    (native-inputs (list bison extra-cmake-modules flex python-minimal))
    (inputs (list ki18n
                  kopeninghours
                  kpublictransport
                  qtbase
                  qtdeclarative
                  zlib))
    (home-page "https://invent.kde.org/libraries/kosmindoormap")
    (synopsis "Indoor map renderer")
    (description
     "This package provides facilities for rendering OpenStreetMap
multi-floor indoor maps.")
    (license license:lgpl2.0+)))

(define-public kpmcore
  (package
    (name "kpmcore")
    (version "24.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/release-service/" version
                    "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "19xfaqj7i8mi5iwkh8n5d5h3m15bny0mzg2skpgbjdlmzc773iga"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     `(("coreutils" ,coreutils)
       ("cryptsetup" ,cryptsetup)
       ("eudev" ,eudev)
       ("kauth" ,kauth)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("lvm2" ,lvm2)
       ("mdadm" ,mdadm)
       ("polkit-qt6" ,polkit-qt6)
       ("qtbase" ,qtbase)
       ("qca-qt6" ,qca-qt6)
       ("smartmontools" ,smartmontools)
       ("util-linux" ,util-linux)
       ("util-linux:lib" ,util-linux "lib")))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-polkit-action-path
            (lambda _
              (substitute* "src/util/CMakeLists.txt"
                (("DESTINATION \\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                 "DESTINATION share/polkit-1/actions"))
              (substitute* "src/backend/corebackend.cpp"
                  (("\\/usr") #$output))))
          (add-before 'configure 'patch-trustedprefixes-file
              (lambda* (#:key inputs #:allow-other-keys)
                (call-with-output-file "src/util/trustedprefixes"
                  (lambda (port)
                    (map (lambda (prefix)
                           (display prefix port)
                           (newline port))
                         (list (assoc-ref inputs "coreutils")
                               (assoc-ref inputs "util-linux")
                               (assoc-ref inputs "eudev")
                               (assoc-ref inputs "cryptsetup")
                               (assoc-ref inputs "lvm2")
                               (assoc-ref inputs "mdadm")
                               (assoc-ref inputs "smartmontools")
                               "/run/current-system/profile"
                               "/usr"
                               "/")))))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Library for managing partitions")
    (description "Library for managing partitions.")
    (license license:gpl3+)))

(define-public partitionmanager
  (package
    (name "partitionmanager")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/partitionmanager-" version ".tar.xz"))
       (sha256
        (base32 "17p63a9igpbcv0xdziaf3d30n88rj9474w9yx2cpvh0m2nrv3582"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kjobwidgets
           kpmcore
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           polkit-qt6))
    (home-page "https://apps.kde.org/partitionmanager/")
    (synopsis "Disk device, partition and file system manager")
    (description "KDE Partition Manager is a utility to help you manage the
disks, partitions, and file systems.  It allows you to easily create, copy,
move, delete, back up, restore, and resize them without losing data.  It
supports a large number of file systems, including ext2/3/4, btrfs, NTFS,
FAT16/32, JFS, XFS and more.")
    (license license:gpl3+)))

(define-public kpublictransport
  (package
    (name "kpublictransport")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kpublictransport-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1g78kmnqg9y4mvd7nmlb4nd02ch3p5gxhnbphcnisc3ym3w3q1jj"))))
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
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list ki18n
           sane-backends))
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
     (list #:qtbase qtbase))
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

(define-public kdeconnect
  (package
    (name "kdeconnect")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/kdeconnect-kde-"
                           version ".tar.xz"))
       (sha256
        (base32
         "05xbxcxg9byj3rilmqig8281rjjd59w2wk3qw4v1z8irhn8fpl28"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list "-DBUILD_TESTING=ON"
                   (string-append "-DQtWaylandScanner_EXECUTABLE="
                                  #$(this-package-native-input "qtwayland")
                                  "/lib/qt6/libexec/qtwaylandscanner")
                   "-DKDE_INSTALL_LIBEXECDIR=libexec"
                   ;; So kdeconnect.so isn't installed to lib/plugins
                   "-DPLUGIN_INSTALL_DIR=lib/qt6/plugins")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-dbus-autostart
                          (lambda _
                            ;; 'dbus-daemon' requires an absolute Exec path.
                            (substitute* "daemon/org.kde.kdeconnect.service.in"
                              (("kdeconnectd")
                               (string-append #$output "/bin/kdeconnectd"))))))
           #:tests? #f)) ; tests fail hard in our build environment
    (native-inputs
     (list extra-cmake-modules
           kdoctools
           libxtst
           pkg-config
           python-wrapper
           wayland-protocols
           qtwayland))
    (inputs
     (list dbus
           kcmutils
           kconfigwidgets
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kio
           kirigami
           kirigami-addons
           knotifications
           kpackage
           kpeople
           kstatusnotifieritem
           kwayland
           libfakekey
           openssl
           plasma-wayland-protocols
           pulseaudio-qt
           qca-qt6
           qqc2-desktop-style
           qtbase
           qtconnectivity
           qtdeclarative
           qtmultimedia
           qtwayland
           sonnet
           wayland
           modemmanager-qt
           libxkbcommon))
    (home-page "https://community.kde.org/KDEConnect")
    (synopsis "Enable your devices to communicate with each other")
    (description "KDE Connect is a project that enables all your devices to
communicate with each other.  Here's a few things KDE Connect can do:
@enumerate
@item Receive your phone notifications on your desktop computer and reply to messages
@item Control music playing on your desktop from your phone
@item Use your phone as a remote control for your desktop
@item Run predefined commands on your PC from connected devices
@item Check your phones battery level from the desktop
@item Ring your phone to help finding it
@item Share files and links between devices
@item Browse your phone from the desktop
@item Control the desktop's volume from the phone
@end enumerate")
    (properties `((upstream-name . "kdeconnect-kde")))
    (license (list license:gpl2 license:gpl3)))) ; dual licensed

(define-public labplot
  (package
    (name "labplot")
    (version "2.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/labplot"
                           "/labplot-" version ".tar.xz"))
       (sha256
        (base32 "1vyslapcjmq7bra3hbbkwrcy6z0cn8z5z2bvzzgy1dng2waihvib"))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags
       (list "-DENABLE_CANTOR=OFF" ;not packaged
             "-DENABLE_MQTT=OFF" ;not packaged (qtmqtt)
             ;; FIXME: readstat (optional dependency) is available in the
             ;; statistics module, but that module can't be used here.
             "-DENABLE_READSTAT=OFF"
             ;; This is a bundled library that is not packaged.
             "-DENABLE_LIBORIGIN=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; This test fails, I don't know why.
               (invoke "ctest" "-E" "(ParserTest|ReadStatFilterTest)")))))))
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

(define-public kdf
  (package
    (name "kdf")
    (version "24.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kdf-" version ".tar.xz"))
              (sha256
               (base32
                "1agv2bpz0gi2l759w4pkafb1pfqyh3m7dhfxpmpvlr8759z4skyv"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcmutils
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kiconthemes
           kio
           knotifications
           kwidgetsaddons
           kstatusnotifieritem
           kxmlgui
           qt5compat))
    (arguments (list #:qtbase qtbase))
    (home-page "https://kde.org/applications/system/kdk")
    (synopsis "View Disk Usage")
    (description "KDiskFree displays the available file devices (hard drive
partitions, floppy and CD drives, etc.) along with information on their
capacity, free space, type and mount point.  It also allows you to mount and
unmount drives and view them in a file manager.")
    (license license:gpl2+)))

(define-public ktimer
  (package
    (name "ktimer")
    (version "24.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/ktimer-" version ".tar.xz"))
              (sha256
               (base32
                "1kpz1hz0s32qc1cpbvrs9yw9w86ingc9sk03cykljsc493fhmy9m"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           kdoctools))
    (inputs
     (list kcrash
           kdbusaddons
           ki18n
           kio
           knotifications
           kconfigwidgets
           kstatusnotifieritem
           qt5compat))
    (arguments (list #:qtbase qtbase))
    (home-page "https://kde.org/applications/utilities/ktimer")
    (synopsis "Countdown Launcher")
    (description "KTimer is a little tool to execute programs after some time.
It allows you to enter several tasks and to set a timer for each of them.  The
timers for each task can be started, stopped, changed, or looped.")
    (license license:gpl2+)))

(define-public kcachegrind
  (package
    (name "kcachegrind")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kcachegrind-" version ".tar.xz"))
              (sha256
               (base32
                "1j0i8sigf21b3w4r0cahrdm046pcinsldqhcms2bhv14v9lirc19"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules perl python qttools kdoctools))
    (inputs
     (list qtbase karchive ki18n kio kxmlgui kdbusaddons))
    ;; Note: The 'hotshot2calltree' and 'pprof2calltree' scripts depend on
    ;; Python and PHP, respectively.  These are optional and we ignore them
    ;; for now.
    (home-page "https://kcachegrind.github.io/html/Home.html")
    (synopsis "Visualize profiles produces by Valgrind's Cachegrind tool")
    (description
     "The data files generated by the Callgrind of Valgrind, an application
profiler, can be loaded into KCachegrind for browsing the performance results.
There is also a command-line tool to get ASCII reports from data files without
the need to use KCachegrind.

The format of Callgrind output is documented.  With conversion scripts,
KCachegrind is able to visualize output of other profilers like OProfile, a
system-wide profiler for Linux using statistical sampling with hardware
performance counters.  There also exist converters for profiling output of
Python, PHP, and Perl.")
    (license license:gpl2)))

(define-public libkdegames
  (package
    (name "libkdegames")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkdegames-" version ".tar.xz"))
       (sha256
        (base32 "1r82gid4wpp70517zc0d5pbnm904c28iwd1sj1p4f3j5j2jxah6n"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list karchive
           kbookmarks
           kcodecs
           kcompletion
           kconfigwidgets
           kcrash
           kdbusaddons
           kdeclarative
           kdnssd
           kglobalaccel
           kguiaddons
           ki18n
           kiconthemes
           kitemviews
           kjobwidgets
           knewstuff
           kservice
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libsndfile
           openal
           qtdeclarative
           qtsvg))
    (home-page "https://apps.kde.org/categories/games/")
    (synopsis "Runtime library for kdegames")
    (description "Runtime library for kdegames")
    (license (list license:gpl2+  license:fdl1.2+))))

(define-public marble-qt
  (package
    (name "marble-qt")
    (version "24.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/education/marble.git/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x8s714ar2q35fzllkbd08wqx24xyvwfd7xm3w7r3wyndri3lfm3"))))
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
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1hcglnbw2ck864glgd4aag54826aycmncmizfqlpncfzwdayq204"))))
    (build-system cmake-build-system)
    (arguments (list #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kio qtbase qt5compat))
    (home-page "https://apps.kde.org/en/kdegraphics_mobipocket")
    (synopsis "KDE thumbnailer for Mobipocket files")
    (description "This package provides a KDE plugin that shows thumbnails of
Mobipocket e-books in Dolphin and other KDE apps.")
    (license license:gpl2+)))

(define-public libkexiv2
  (package
    (name "libkexiv2")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "04z36kvj9nwr9i8xs6hi0lqmxz7lqyhs9c1cngcb7p7lw9hmynch"))))
    (build-system cmake-build-system)
    (arguments (list #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list exiv2 qtbase))
    (home-page "https://invent.kde.org/graphics/libkexiv2")
    (synopsis "Manipulate the metadata of images")
    (description "Libkexiv2 wraps the Exiv2 library, allowing to manipulate
picture metadata as EXIF/IPTC and XMP.")
    (license license:gpl2+)))

(define-public kio-zeroconf
  (package
    (name "kio-zeroconf")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kio-zeroconf-" version ".tar.xz"))
       (sha256
        (base32 "058w37n7s8lbc8mjqjpqarhg8b2796x2yssrxnppib12dld9x4wb"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kdbusaddons kdnssd ki18n kio))
    (arguments (list #:qtbase qtbase
                     #:configure-flags
                     #~(list "-DQT_MAJOR_VERSION=6")))
    (home-page "https://apps.kde.org/kio_zeroconf/")
    (synopsis "DNS-SD Service Discovery Monitor")
    (description "Adds an entry to Dolphin's Network page to show local
services such as printers which advertise themselves with DNSSD (called Avahi
or Bonjour by other projects).")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))


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
