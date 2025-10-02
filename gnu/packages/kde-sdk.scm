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
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control))

(define-public libkomparediff2
  (package
    (name "libkomparediff2")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkomparediff2-" version ".tar.xz"))
       (sha256
        (base32 "0jcb4iynv5yllx4hjahqb4qrpg871srsf0flyhzi5qn0cw4dm06p"))))
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kapptemplate-" version ".tar.xz"))
       (sha256
        (base32 "0hkv74r0v9c8qjr561cipz43x1agx6mg6pddx8av71pz5wslh98z"))))
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

(define-public kdevelop-pg-qt
  (package
    (name "kdevelop-pg-qt")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdevelop-" version ".tar.xz"))
       (sha256
        (base32 "09dc22an595wx5nhzdzn1qwgg70qk5y40g879hzwc2qdrpncgx52"))))
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

(define-public kompare
  (package
    (name "kompare")
    (version "25.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kompare-" version ".tar.xz"))
              (sha256
               (base32
                "05z04f71apmjym8xymy62h266p12drdsix69rv77kxvx6gw1gysm"))))
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
           kwidgetsaddons))
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/massif-visualizer-" version ".tar.xz"))
       (sha256
        (base32 "143xamgifbrqpk59l4p43kzxv792w9c4vdnaalpmzg21x5mgvs59"))))
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
