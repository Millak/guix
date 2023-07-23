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
;;; Copyright © 2020, 2021, 2022, 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2021 la snesne <lasnesne@lagunposprasihopre.org>
;;; Copyright © 2021, 2022, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
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
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public baloo-widgets
  (package
    (name "baloo-widgets")
    (version "22.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/baloo-widgets-" version ".tar.xz"))
       (sha256
        (base32 "02p4v8g4syk908mg7f0l5fpqn7ddsxqji1n8jqghsdkkdsvry7mn"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list baloo kconfig ki18n kio qtbase-5))
    (arguments
     `(#:tests? #f)) ;; tests fail
    (home-page "https://community.kde.org/Baloo")
    (synopsis "Wigets for use with Baloo")
    (description "Baloo is a framework for searching and managing metadata.
This package contains GUI widgets for baloo.")
    (license license:lgpl2.0+)))

(define-public grantleetheme
  (package
    (name "grantleetheme")
    (version "22.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/grantleetheme-" version ".tar.xz"))
       (sha256
        (base32 "01ls16x6ngi43lcwffav9qig3afakrs04v4wvyfb8lm7sd65hgf9"))))
    (build-system qt-build-system)
    (arguments `(#:tests? #f))  ; unexpected error in the test suite.
    (native-inputs
     (list extra-cmake-modules libxml2)) ;; xmllint required for tests
    (inputs
     (list grantlee
           kguiaddons
           ki18n
           kiconthemes
           knewstuff
           qtbase-5))
    (home-page "https://invent.kde.org/pim/grantleetheme")
    (synopsis "Library providing Grantlee theme support")
    (description "This library provides Grantlee theme support.")
    (license ;; LGPL for libraries, FDL for documentation
     (list license:lgpl2.1+ license:fdl1.2+))))

(define-public akregator
  (package
    (name "akregator")
    (version "22.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akregator-" version ".tar.xz"))
       (sha256
        (base32 "01rgyl2hwjprq4z5yjc99j6jk9vrhjy608ha72j470pw6g47ac5s"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-qt-process-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/akregator"))
                    (qt-process-path
                     (search-input-file
                      inputs "/lib/qt5/libexec/QtWebEngineProcess")))
               (wrap-program bin
                 `("QTWEBENGINEPROCESS_PATH" = (,qt-process-path)))))))))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list akonadi
           akonadi-contacts
           akonadi-mime
           boost
           breeze-icons
           gpgme
           grantlee
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
           ktexteditor
           kuserfeedback
           libkdepim
           libkleo
           qgpgme
           qtbase-5
           qtdeclarative-5
           qtwebchannel-5
           qtwebengine-5
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

(define-public kdenlive
  (package
    (name "kdenlive")
    (version "23.04.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/multimedia/kdenlive")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dgrgnnq38nphfzbapr7dkb21lv4idqynxqmzv9x3maijmp1jjfr"))))
    (build-system qt-build-system)
    (arguments
     ;; XXX: there is a single test that spawns other tests and
     ;; 1/3 tests failed and 1/327 assertions failed.  It seems
     ;; that individual tests can't be skipped.
     (list
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
                    (,(string-append qtbase "/lib/qt5/plugins/platforms")))
                  `("MLT_PREFIX" ":" =
                    (,#$(this-package-input "mlt"))))))))))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config qttools-5))
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
           knewstuff
           knotifications
           knotifyconfig
           kparts
           kplotting
           ladspa
           mlt
           purpose
           qtbase-5
           qtdeclarative-5
           qtgraphicaleffects
           qtmultimedia-5
           qtnetworkauth-5
           qtquickcontrols-5
           qtquickcontrols2-5
           qtscript
           qtsvg-5
           shared-mime-info))
    (home-page "https://kdenlive.org")
    (synopsis "Non-linear video editor")
    (description "Kdenlive is an acronym for KDE Non-Linear Video Editor.

Non-linear video editing is much more powerful than beginner's (linear)
editors, hence it requires a bit more organization before starting.  However,
it is not reserved to specialists and can be used for small personal
projects.")
    (license license:gpl2+)))

(define-public kdevelop
  (package
    (name "kdevelop")
    (version "22.08.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/release-service/" version
                            "/src/kdevelop-" version ".tar.xz"))
        (sha256
         (base32 "14a80z4sahxyzssrz605zp7ah5xdjbc22ccv0vwcnhr5lzr76v31"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config shared-mime-info qttools-5))
    (inputs (list boost
                  clang
                  grantlee
                  karchive
                  kcmutils
                  kcrash
                  kdeclarative
                  kdoctools
                  kguiaddons
                  ki18n
                  kiconthemes
                  kio ;; not checked as requirement
                  kitemmodels
                  kitemviews
                  kjobwidgets
                  knotifications
                  knotifyconfig
                  kparts
                  kservice
                  ksyntaxhighlighting
                  ktexteditor
                  kwindowsystem
                  kxmlgui
                  libkomparediff2
                  breeze-icons
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols-5 ;; not checked as requirement
                  qtquickcontrols2-5 ;; not checked as requirement
                  qtwebengine-5
                  threadweaver
                  ;; recommendes
                  astyle
                  kdevelop-pg-qt
                  libksysguard

                  ;; optional
                  apr ; required for subversion support
                  apr-util ; required for subversion support
                  attica
                  kconfigwidgets
                  knewstuff
                  krunner
                  ;; TODO: OktetaGui, OktetaKastenControllers
                  plasma-framework
                  ;; TODO: purpose
                  sonnet
                  subversion))
       ;; run-time packages - TODO
       ;; ClazyStandalone
       ;; Cppcheck
       ;; heaptrack
       ;; heaptrack_gui
       ;; meson
    (arguments
     `(#:tests? #f  ;; there are some issues with the test suite
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'add-include-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "cmake/modules/FindClang.cmake"
               (("^\\s*PATHS \"\\$\\{CLANG_LIBRARY_DIRS\\}\"" line)
                (string-append line " " (assoc-ref inputs "clang") "/lib"))))))))
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
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/KDE/kdevelop-pg-qt")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "3kfab4p717acbdkcdi41d98vwch7v431gb2qi6s38hmclsf8bf8g"))))
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
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kdiagram/" version
                           "/kdiagram-" version ".tar.xz"))
       (sha256
        (base32 "07s3kwv0mqvb64x8nz4w1yb3hbk28yzkw4qg1jibai7as4xsv7ap"))
       (patches (search-patches
                 "kdiagram-Fix-missing-link-libraries.patch"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (inputs
     (list qtbase-5 qtsvg-5))
    (home-page "https://invent.kde.org/graphics/kdiagram")
    (synopsis "Libraries for creating business diagrams")
    (description "This package provides libraries for integrating business
diagrams in Qt-based applications.

@code{KCharts} provides an implementation of the ODF Chart specification.  It
supports stock charts, box charts, and whisker charts.  @code{KGantt} provides
a module for implementing ODF Gantt charts, which are bar charts that
illustrate project schedules.")
    (license license:gpl2+)))

(define-public kio-extras
  (package
    (name "kio-extras")
    (version "22.08.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0gzna2ps2qd2js28c97kjpcbah7zz8n4s4932faggc2nz5z5wnyn"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME" (getcwd))
                              (setenv "TMPDIR" (getcwd))
                              (invoke "ctest" "-E" "testkioarchive")))))))
    (native-inputs (list extra-cmake-modules dbus kdoctools qttools-5))
    (inputs (list karchive
                  kconfig
                  kconfigwidgets
                  kcoreaddons
                  kdbusaddons
                  ki18n
                  kdnssd
                  kio
                  solid
                  kbookmarks
                  kguiaddons
                  ksyntaxhighlighting
                  qtbase-5
                  qtsvg-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Additional components to increase the functionality of KIO")
    (description
     "This package provides additional components to increase
the functionality of the KDE resource and network access abstractions.")
    (license license:lgpl2.0+)))

(define-public kio-fuse
  (package
    (name "kio-fuse")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/" name "/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1pb62h45c06dq3rml91xbf8j5y2c1l8z8j8lycchxrlgys5rlrv6"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME" (getcwd))
                              (setenv "XDG_RUNTIME_DIR" (getcwd))
                              (setenv "QT_QPA_PLATFORM" "offscreen")
                              (invoke "dbus-launch" "ctest" "-E"
                               "(fileopstest-cache|fileopstest-filejob)")))))))
    (native-inputs (list dbus extra-cmake-modules pkg-config))
    (inputs (list fuse kio kcoreaddons qtbase-5))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "FUSE Interface for KIO")
    (description "This package provides FUSE Interface for KIO.")
    (license license:lgpl2.1+)))

(define-public kirigami-addons
  (package
    (name "kirigami-addons")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://invent.kde.org/libraries/kirigami-addons")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bi6sc1fpycxj1ydd08gkbw1x1pi2ab6p9bk4175hm0mb0rykdir"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f)) ; failing test
    (native-inputs (list extra-cmake-modules))
    (inputs (list kirigami ki18n qtdeclarative-5 qtquickcontrols2-5))
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

(define-public krita
  (package
    (name "krita")
    (version "5.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/krita/" version "/krita-" version
             ".tar.gz"))
       (sha256
        (base32 "1lx4x4affkbh47b7w5qvahkkr4db0vcw6h24nykak6gpy2z5wxqw"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DBUILD_TESTING=OFF")))
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
     (list boost
           exiv2
           fftw
           giflib
           gsl
           imath
           karchive
           kcompletion
           kconfig
           kcoreaddons
           kcrash
           kguiaddons
           ki18n
           kiconthemes
           kio
           kitemviews
           kseexpr
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           lcms
           libjpeg-turbo
           libheif
           libmypaint
           libpng
           libraw
           libtiff
           libwebp
           libx11
           libxcb
           libxi
           opencolorio
           openexr
           openjpeg
           perl
           poppler-qt5
           qtbase-5
           qtdeclarative-5
           qtmultimedia-5
           qtsvg-5
           qtx11extras
           quazip-0
           zlib))
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
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/massif-visualizer/" version
             "/src/massif-visualizer-" version ".tar.xz"))
       (sha256
        (base32 "0v8z6r9gngzckvqyxjm9kp7hilwfqibyk2f9vag9l98ar0iwr97q"))))
    (build-system cmake-build-system)
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
           qtbase-5
           qtsvg-5
           qtxmlpatterns))
    (home-page "https://apps.kde.org/massif-visualizer/")
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
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/" name
                                  "/libqaccessibilityclient-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0a9lv2jb5gyqxvrkl5xl09gjnlp05b5yfzfb0wmaxz0l8d7qnxhw"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;TODO: Failing tests
       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                         ;
                        ;; make Qt render "offscreen", required for tests
                        (setenv "QT_QPA_PLATFORM" "offscreen")
                        ;; For missing '/etc/machine-id'
                        (setenv "DBUS_FATAL_WARNINGS" "0")
                        (setenv "HOME"
                                (getcwd))
                        (invoke "dbus-launch" "ctest")))))))
    (native-inputs (list dbus extra-cmake-modules))
    (inputs (list qtbase-5))
    (home-page "https://invent.kde.org/libraries/libqaccessibilityclient")
    (synopsis "Helper library to make writing accessibility tools easier")
    (description "This package provides library that is used when writing
accessibility clients such as screen readers.")
    (license license:lgpl2.1+)))

(define-public libkomparediff2
  (package
    (name "libkomparediff2")
    (version "22.08.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/release-service/" version
                            "/src/libkomparediff2-" version ".tar.xz"))
        (sha256
         (base32 "0hjxxhfv0ds05l821avq787sfdy0afr595xx266c20x8fxgm6kv9"))))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kcodecs
           kconfig
           kcoreaddons
           ki18n
           kio
           kxmlgui
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
    (version "2.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/qca/" version
                            "/qca-" version ".tar.xz"))
        (sha256
         (base32 "1i7m5y3dfwij9cyjp72ya5zd2skgp7mfmrmf7bvrbzg3ly0mhsbb"))))
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

(define-public kopeninghours
  (package
    (name "kopeninghours")
    (version "22.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "03hslgx4zgg7gsnz2xhx4wnchvqfc5n8c6ihgwz3972fkxsjfdvq"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases '(modify-phases %standard-phases
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
                  qtbase-5
                  qtdeclarative-5))
    (home-page "https://invent.kde.org/libraries/kopeninghours")
    (synopsis "Get opening hours from OpenStreetMap")
    (description
     "This package provides a library for parsing and evaluating OpenStreetMap
opening hours expressions.")
    (license license:lgpl2.0+)))

(define-public kosmindoormap
  (package
    (name "kosmindoormap")
    (version "22.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "042axwxa1497snr8f0m6a61gl9ypdkvllnhnlw4h5ffah7yl5n3s"))))
    (build-system cmake-build-system)
    (native-inputs (list bison extra-cmake-modules flex))
    (inputs (list ki18n
                  kopeninghours
                  kpublictransport
                  qtbase-5
                  qtdeclarative-5
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
    (version "22.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/release-service/" version
                    "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1y28dnmbnkkjar4kl033fkmcnazgczc3pgdac2q1ry2hjzkcbnpa"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kauth
           kcoreaddons
           ki18n
           kwidgetsaddons
           polkit-qt
           qtbase-5
           qca
           `(,util-linux "lib")))
    (arguments
     `(#:tests? #f ;; 4/6 tests fail do to no plugin instance
	   #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-cmake-install-directories
           (lambda _
             (substitute* "src/util/CMakeLists.txt"
               (("DESTINATION \\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                "DESTINATION share/polkit-1/actions")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Library for managing partitions")
    (description "Library for managing partitions.")
    (license license:gpl3+)))

(define-public kpublictransport
  (package
    (name "kpublictransport")
    (version "22.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/release-service/" version
                    "/src/kpublictransport-" version ".tar.xz"))
              (sha256
               (base32
                "0z7zyyiq4815m74s6p841k1c4pxbrss7hnkag8kr5qa3q4264kg9"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases '(modify-phases %standard-phases
                       (replace 'check
                         (lambda* (#:key tests? #:allow-other-keys)
                           (when tests?
                             (setenv "QT_QPA_PLATFORM" "offscreen")
                             (invoke "ctest" "-E"
                              "(mergeutiltest|departuretest|journeytest|networkconfigtest|locationhistorymodeltest|navitiaparsertest|otpparsertest|ivvassparsertest|cachetest)")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    ;; TODO: clipper and osmctools are not detected
    (inputs (list clipper osmctools protobuf qtdeclarative-5 zlib))
    (home-page "https://api.kde.org/kdepim/kpublictransport/html/index.html")
    (synopsis "Library for accessing realtime public transport data")
    (description
     "This package provides a library for accessing realtime public
transport data and for performing public transport journey queries.")
    (license (list license:lgpl2.0+))))

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
    (version "22.08.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/release-service/"
                            version "/src/kdeconnect-kde-"
                            version ".tar.xz"))
        (sha256
         (base32
          "1yzx49gcm7x2wdk53iznyjz09y2a6mrrhh68xilbcsafyiw3l3zr"))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_TESTING=ON"
                           "-DKDE_INSTALL_LIBEXECDIR=libexec"
                           ;; So kdeconnect.so isn't installed to lib/plugins
                           "-DPLUGIN_INSTALL_DIR=lib/qt5/plugins")
       #:tests? #f)) ; tests fail hard in our build environment
    (native-inputs
     (list extra-cmake-modules
           kdoctools
           libxtst
           pkg-config
           python-wrapper))
    (inputs
     (list kcmutils
           kconfigwidgets
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kio
           kirigami
           knotifications
           kpackage
           kpeople
           kpeoplevcard
           kwayland
           libfakekey
           plasma-wayland-protocols
           pulseaudio-qt
           qca
           qqc2-desktop-style
           qtbase-5
           qtdeclarative-5
           qtgraphicaleffects
           qtmultimedia-5
           qtquickcontrols-5
           qtquickcontrols2-5
           qtx11extras
           qtwayland-5
           wayland))
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
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/labplot"
                           "/" version "/labplot-"
                           version ".tar.xz"))
       (sha256
        (base32 "1wi19gj18yhrim1cb2dwgpnc2yvydm87h41fcg670ampy24i98z5"))))
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
     (list breeze ;for dark themes
           breeze-icons ;for icons
           gsl
           karchive
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdoctools
           ki18n
           kiconthemes
           kio
           knewstuff
           kparts
           kservice
           ksyntaxhighlighting
           ktextwidgets
           kuserfeedback
           kwidgetsaddons
           kxmlgui
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
    (version "22.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kdf-" version ".tar.xz"))
              (sha256
               (base32
                "0p7iqld2phc74pmhyb8bqqg9clnc7l2rh6hd0i6jcsp266cgg205"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcmutils
           kconfigwidgets
           kcoreaddons
           ki18n
           kiconthemes
           kio
           knotifications
           kwidgetsaddons
           kxmlgui
           qtbase-5))
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
    (version "20.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/ktimer-" version ".tar.xz"))
              (sha256
               (base32
                "12fz5v5ky03h18jl75fnq1zagjq8qzi1s1q7phjz5d1rbhwc57ls"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           kdoctools))
    (inputs
     (list kdbusaddons
           ki18n
           kio
           knotifications))
    (home-page "https://kde.org/applications/utilities/ktimer")
    (synopsis "Countdown Launcher")
    (description "KTimer is a little tool to execute programs after some time.
It allows you to enter several tasks and to set a timer for each of them.  The
timers for each task can be started, stopped, changed, or looped.")
    (license license:gpl2+)))

(define-public kcachegrind
  (package
    (name "kcachegrind")
    (version "22.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kcachegrind-" version ".tar.xz"))
              (sha256
               (base32
                "05wcnfqs0fdfhfpagn0pqky0l7014nblv8r9fv1khy4g5mdlm4hs"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules perl python qttools-5 kdoctools))
    (inputs
     (list qtbase-5 karchive ki18n kio kdbusaddons))
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
    (version "22.08.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/libkdegames-" version ".tar.xz"))
      (sha256
       (base32 "1aihbha073fw2bxmdk4l768716kvrlyjd72x2nfx0vvr0ngc5wx9"))))
    (build-system qt-build-system)
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
           ;("kio" ,kio)
           kitemviews
           kjobwidgets
           knewstuff
           kservice
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libsndfile
           openal
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/categories/games/")
    (synopsis "Runtime library for kdegames")
    (description "Runtime library for kdegames")
    (license (list license:gpl2+  license:fdl1.2+))))

(define-public marble-qt
  (package
    (name "marble-qt")
    (version "22.08.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/education/marble.git/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
		"0pn0mjh5cg7mgjj82bsj8zikzdyq2fmzckdcav2y0msxwi859crl"))))
    (build-system qt-build-system)
    (arguments
     ;; FIXME: libmarblewidget-qt5.so.28 not found.  Also enable the
     ;; corresponding configure flag to build tests.
     `(#:tests? #f
       #:configure-flags
       (list "-DBUILD_MARBLE_TOOLS=YES" ; file conversion tools
             "-DBUILD_TOUCH=YES"
             "-DBUILD_MARBLE_TESTS=FALSE")))
    (native-inputs
     (list extra-cmake-modules kdoctools qttools-5))
    ;; One optional dependency missing: libwlocate.
    (inputs
     (list gpsd
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
           qtbase-5
           qtdeclarative-5
           qtlocation
           qtserialport
           qtsvg-5
           qtwebchannel-5
           qtwebengine-5
           shapelib
           shared-mime-info
           zlib))
    (home-page "https://marble.kde.org/")
    (synopsis "Virtual globe and world atlas")
    (description "Marble is similar to a desktop globe.  At closer scale it
becomes a world atlas, while OpenStreetMap takes the user to street level.  It
supports searching for places of interest, viewing Wikipedia articles,
creating routes by drag and drop and more.")
    (license license:lgpl2.1+)))

(define-public okular
  (package
    (name "okular")
    (version "22.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0f98kfsb6sirpym27j2wwz4qr4p5vl4pbnckxd3gmgyfpz8mszln"))))
    (build-system qt-build-system)
    ;; The tests fail because they can't find the proper mimetype plugins:
    ;; "org.kde.okular.core: No plugin for mimetype '"image/jpeg"'."
    ;; The built program seems to work okay, so we skip the tests for now.
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "-DBUILD_TESTING=OFF")))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list ebook-tools
           breeze-icons
           discount
           djvulibre
           kactivities
           khtml
           chmlib
           kdegraphics-mobipocket
           karchive
           kbookmarks
           kcompletion
           kconfig
           qtbase-5
           libjpeg-turbo
           libtiff
           kirigami
           purpose
           freetype
           kiconthemes
           kio
           kparts
           kpty
           qtspeech
           kwallet
           kwindowsystem
           libkexiv2
           libspectre
           libzip
           phonon
           poppler-qt5
           qca
           qtdeclarative-5
           qtsvg-5
           qtwayland-5
           threadweaver
           kcrash
           kjs))
    (home-page "https://apps.kde.org/okular/")
    (synopsis "Document viewer")
    (description
     "Okular is a document viewer developed for KDE.  It can display files in
a variety of formats, including PDF, PostScript, DejaVu, and EPub.")
    (license license:gpl2+)))

(define-public poxml
  (package
    (name "poxml")
    (version "22.08.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kde/stable/release-service/" version
                              "/src/poxml-" version ".tar.xz"))
              (sha256
               (base32
                "13jp5g5la3kq9i3qybdvwfl4vgqz3hxf64qzmh7kl71ykas7s5vi"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list gettext-minimal qtbase-5))
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
    (version "22.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1kxvzgmq83hml10pc4j66r59dsar7h92w2i0cs1nhar781mz666n"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kio qtbase-5))
    (home-page "https://apps.kde.org/en/kdegraphics_mobipocket")
    (synopsis "KDE thumbnailer for Mobipocket files")
    (description "This package provides a KDE plugin that shows thumbnails of
Mobipocket e-books in Dolphin and other KDE apps.")
    (license license:gpl2+)))

(define-public libkexiv2
  (package
    (name "libkexiv2")
    (version "22.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0iwpy79ppv4bbsqrszp9kmghgjvkl13gdpnafsbikh4wy1ch4cv9"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list exiv2 qtbase-5))
    (home-page "https://invent.kde.org/graphics/libkexiv2")
    (synopsis "Manipulate the metadata of images")
    (description "Libkexiv2 wraps the Exiv2 library, allowing to manipulate
picture metadata as EXIF/IPTC and XMP.")
    (license license:gpl2+)))

(define-public zeroconf-ioslave
  (package
    (name "zeroconf-ioslave")
    (version "22.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/zeroconf-ioslave-" version ".tar.xz"))
       (sha256
        (base32 "0jbrdbphxn77dg2a4wzsm7q24455j4d1xhd4rj5iwhq4ywiig9i1"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kdbusaddons kdnssd ki18n kio qtbase-5))
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kuserfeedback/"
                           "/kuserfeedback-" version ".tar.xz"))
       (sha256
        (base32 "0r7jcc88n5b4rc0asjzh7m7g33i35k3z99l08qkrn92kn4ickakn"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           qttools-5
           ;; For optional component "Survey target expression parser"
           bison
           flex
           ;; For syntax checking and unit tests of PHP server code
           ;;("php" ,php)
           ;;("phpunit" ,phpunit)
           ))
    (inputs
     (list qtbase-5 qtcharts qtdeclarative-5 qtsvg-5))
    (arguments
     `(#:tests? #f))  ;; 4/17 fail
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
