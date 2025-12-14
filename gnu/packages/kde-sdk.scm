;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018-2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2020, 2023-2025 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023-2025 Sughosha <sughosha@disroot.org>
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

(define-module (gnu packages kde-sdk)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages code)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-graphics)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control))

(define-public poxml
  (package
    (name "poxml")
    (version "25.12.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kde/stable/release-service/" version
                              "/src/poxml-" version ".tar.xz"))
              (sha256
               (base32
                "1k682yib9nsjv076mazgg1jblq3s4pqrzq1fcnymkn119q09j57g"))))
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

(define-public libkomparediff2
  (package
    (name "libkomparediff2")
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkomparediff2-" version ".tar.xz"))
       (sha256
        (base32 "0yd0xa67n35ipcsj58lyb71zs62vcdbjy8p2lk2y0pzcqp16rylk"))))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kcodecs
           kconfig
           kcoreaddons
           ki18n
           kio
           kxmlgui
           qtbase))
    (build-system cmake-build-system)
    (home-page "https://kde.org")
    (synopsis "Library to compare files and strings, used in Kompare and KDevelop")
    (description "Libkomparediff2 is a library to work with diffs and patches,
used in KDE development tools Kompare and KDevelop.")

    ;; GPL, some files are also licensed under LGPL or BSD, see COPYING in the
    ;; source archive
    (license (list license:gpl2+ license:lgpl2.0+ license:bsd-3))))

(define-public kapptemplate
  (package
    (name "kapptemplate")
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kapptemplate-" version ".tar.xz"))
       (sha256
        (base32 "0zmvpjg3v8p9jghk2lk0zddn3ysh3rsvh1w71qc8glrkqm4rmmkh"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kcompletion
           kconfigwidgets
           kcoreaddons
           kiconthemes
           kirigami-addons
           ki18n
           kio
           qtwayland))
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

(define-public kcachegrind
  (package
    (name "kcachegrind")
    (version "25.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kcachegrind-" version ".tar.xz"))
              (sha256
               (base32
                "027wpnswp91q7zj4swm167p45k9rvqgcbg4i25ln5p8avpg6ysbn"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules perl python qttools kdoctools))
    (inputs
     (list qtbase karchive ki18n kio kxmlgui kdbusaddons qtwayland))
    (arguments (list #:tests? #f))
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

(define-public kdevelop-pg-qt
  (package
    (name "kdevelop-pg-qt")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kdevelop-pg-qt/" version
                           "/src/kdevelop-pg-qt-" version ".tar.xz"))
       (sha256
        (base32 "11mv4z8gi5bcsi3d3mhbjw392mg2wnzf667svhnsmmzmh8fbdgmc"))))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list bison flex qtbase))
    (build-system cmake-build-system)
    (home-page "https://kde.org")
    (synopsis "Parser generator library for KDevplatform")
    (description "KDevelop-PG-Qt is the parser generator used in KDevplatform
for some KDevelop language plugins (Ruby, PHP, CSS...).")
    (license license:lgpl2.0+)))

(define-public kdevelop
  (package
    (name "kdevelop")
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdevelop-" version ".tar.xz"))
       (sha256
        (base32 "0prz7zvlkslahdq5fvzwi4ynfip6pw538bwx6a15vbw6sn5pbzzg"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config shared-mime-info qttools))
    (inputs (list boost
                  clang
                  grantlee
                  karchive
                  kcmutils
                  kcrash
                  kdeclarative
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
                  ktexttemplate
                  ktextwidgets
                  kwindowsystem
                  kxmlgui
                  libkomparediff2
                  breeze-icons
                  qt5compat
                  qtdeclarative
                  qtwayland
                  qtwebengine
                  threadweaver
                  ;; recommendes
                  astyle
                  kdevelop-pg-qt

                  ;; optional
                  apr ; required for subversion support
                  apr-util ; required for subversion support
                  attica
                  kconfigwidgets
                  knewstuff
                  krunner
                  ;; TODO: OktetaGui, OktetaKastenControllers
                  libplasma
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
     (list #:qtbase qtbase
           #:tests? #f ;; there are some issues with the test suite
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
      libgit2-1.8
      qtwayland))
    (home-page "https://apps.kde.org/kommit/")
    (synopsis "Git client for KDE")
    (description
     "Kommit is a git client for KDE.")
    (license license:gpl3+)))

(define-public kompare
  (package
    (name "kompare")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kompare-" version ".tar.xz"))
              (sha256
               (base32
                "1qjg876mjjahlw52lfpxqhw7savh2nc4jzl8xcghs8cgxvihchyf"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list libkomparediff2
           kcodecs
           kconfig
           kcoreaddons
           ki18n
           kiconthemes
           kjobwidgets
           kparts
           ktexteditor
           kwidgetsaddons
           qtwayland))
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

(define-public massif-visualizer
  (package
    (name "massif-visualizer")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/massif-visualizer-" version ".tar.xz"))
       (sha256
        (base32 "0xb702s42n999v0fwcvhwrm9pw4nsljhfrisk0j95c6bwa7x2787"))))
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
           qt5compat
           qtwayland))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/massif_visualizer/")
    (synopsis "Visualize massif data generated by Valgrind")
    (description
     "Massif Visualizer is a tool that visualizes massif data.
You run your application in Valgrind with @code{--tool=massif} and then open
the generated @file{massif.out.%pid} in the visualizer.  Gzip or Bzip2
compressed massif files can also be opened transparently.")
    (license license:gpl2+)))
