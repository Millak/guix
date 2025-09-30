;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024, 2025 Sughosha <sughosha@disroot.org>
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

(define-module (gnu packages kde-office)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-graphics)
  #:use-module (gnu packages kde-multimedia)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public calligra
  (package
    (name "calligra")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/calligra/"
                                  "calligra-" version ".tar.xz"))
              (sha256
               (base32
                "0pyri2ypzva4b4rnl2p3xp0ph5xcn181msj86l5xq6sg7zkza0fl"))
              (patches (search-patches "calligra-qt-6.9.patch"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           ;; XXX: 26/164 tests fail.
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools perl pkg-config))
    (inputs
     (list boost
           eigen
           fontconfig
           freetype
           gsl
           imath
           karchive
           kcmutils
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdiagram
           kguiaddons
           ki18n
           kiconthemes
           kio
           kitemviews
           kjobwidgets
           knotifications
           knotifyconfig
           ktextwidgets
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           lcms
           libdisplay-info
           libetonyek
           libgit2
           libodfgen
           librevenge
           libvisio
           libwpd
           libwpg
           libwps
           libxkbcommon
           okular
           openexr
           openssl
           phonon
           poppler-qt6
           pstoedit
           qca-qt6
           qtkeychain-qt6
           qtsvg
           qtwebengine
           shared-mime-info
           sonnet
           threadweaver
           zlib))
    (home-page "https://calligra.org/")
    (synopsis "Office and graphic art suite")
    (description
     "Calligra Suite is a collection of office applications linked together by
a common base.

The applications currently included in Calligra Suite are:

Office productivity:
@itemize
@item Words: Word processor
@item Sheets: Spreadsheet calculator
@item Stage: Presentation program
@end itemize

Graphics:
@itemize
@item Karbon: Vector graphics
@end itemize

Advanced plugins:
@itemize
@item Chart: Graphic data visualization
@item KFormula: Mathematical formulas
@end itemize")
    (license (list license:lgpl2.0+ license:gpl2+))))

(define-public crow-translate
  (package
    (name "crow-translate")
    (version "4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/crow-translate/" version
                           "/crow-translate-" version ".tar.gz"))
       (sha256
        (base32 "0lrpxdgicbg0wj2cf0lif99pz5kiqck53qkm5385vymzn1w8wjz2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled breeze-icons, espeak-ng and qhotkey.
           (for-each delete-file-recursively
                     '("data/icons/3rdparty/breeze-icons"
                       "src/3rdparty/espeak-ng"
                       "src/3rdparty/qhotkey"))
           ;; Use system libraries instead.
           (substitute* "CMakeLists.txt"
             ((".*icon-theme\\.qrc.*$") "")
             (("WITH_PIPER_TTS") "WITH_BUNDLED_ESPEAK_NG")
             (("(.*WITH_BUNDLED_ESPEAK_NG.* )ON" all start)
              (string-append start "OFF"))
             (("Enable Piper neural TTS provider.*\"")
              "Build bundled espeak-ng (requires onnxruntime)\"")
             (("Piper TTS support disabled.*\"")
              "Piper TTS support enabled with system espeak-ng\"")
             (("add_subdirectory.*qhotkey.*")
              (string-append "\nfind_package(PkgConfig)\n"
                             "pkg_check_modules(eSpeak_NG REQUIRED espeak-ng)"
                             "\nfind_package(QHotkey REQUIRED)\n"))
             (("QHotkey::QHotkey") "qhotkey")
             (("( *)Qt6::TextToSpeech" all indent)
              (string-append all "\n" indent "espeak-ng")))
           ;; Link Qt6::Widgets.
           (substitute* "CMakeLists.txt"
             (("Qt6::TextToSpeech" all) (string-append all "\n    Qt6::Widgets")))
           ;; Include QGuiApplication in main.cpp.
           (substitute* "src/main.cpp"
             (("#include <QtCore>" all)
              (string-append all "\n#include <QtGui/QGuiApplication>")))))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f ; no tests
           #:configure-flags
           #~(list (string-append "-DCMAKE_CXX_FLAGS=-isystem "
                                  #$(this-package-input "qtbase")
                                  "/include/qt6/QtWidgets"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unbundle-singleapplication
                 (lambda _
                   (delete-file-recursively "src/3rdparty/singleapplication")
                   (substitute* "CMakeLists.txt"
                     (("add_subdirectory.*singleapplication.*$") "")
                     (("SingleApplication::SingleApplication")
                      (string-append #$(this-package-input
                                        "single-application")
                                     "/lib/libSingleApplication.a"))))))))
    (inputs
     (list breeze-icons
           espeak-ng
           kwayland
           qhotkey
           qtbase
           qtsvg
           qtmultimedia
           qtscxml
           qtspeech
           single-application
           tesseract-ocr))
    (native-inputs
     (list pkg-config
           extra-cmake-modules
           qttools))
    (home-page "https://invent.kde.org/office/crow-translate")
    (synopsis "Application for translating text")
    (description
     "Crow Translate is an application written in C++/Qt for translating
and speaking text which relies on Mozhi to interface with various
translation engines.")
    (license license:gpl3+)))

(define-public ghostwriter
  (package
    (name "ghostwriter")
    (version "25.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/ghostwriter-" version ".tar.xz"))
              (sha256
               (base32 "0hlc039pkrn0l3k4vzvlvwnbzv46vnkacpaasn2lj5rfs3spxcmj"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list "-DQT_MAJOR_VERSION=6")))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config qttools))
    (inputs
     (list hunspell
           kconfigwidgets
           kcoreaddons
           kwidgetsaddons
           kxmlgui
           qt5compat
           qtsvg
           qtwebchannel
           qtwebengine
           sonnet))
    (home-page "https://ghostwriter.kde.org/")
    (synopsis "Text editor for Markdown")
    (description "@code{ghostwriter} is a text editor for Markdown, which is a plain
text markup format.  It features a live HTML preview as you type, theme
creation, focus mode, fullscreen mode, live word count, and document navigation
in an aesthetic writing environment.  It comes with the cmark-gfm Markdown
processor built in, and can integrate with Pandoc, MultiMarkdown, Discount, and
cmark processors if they are installed.")
    (license license:gpl3+)))

(define-public tellico
  (package
    (name "tellico")
    (version "4.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://invent.kde.org/office/tellico")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0930jh6b9cl3jvhmfv00l6566vsnpwkf6shz5w0hn67j0vpvhk7s"))
              (modules '((guix build utils)))
              (snippet
               ;; Fix including QtPrintSupport.
               '(substitute* (find-files "src" "\\.(h|cpp)$")
                  (("#include <QPrint") "#include <QtPrintSupport/QPrint")))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-home-directory
                 (lambda _
                   (setenv "HOME" "/tmp")))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "dbus-launch" "ctest")))))))
    (native-inputs
     (list dbus extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list exempi
           karchive
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kfilemetadata
           kguiaddons
           ki18n
           kiconthemes
           kio
           kitemmodels
           kjobwidgets
           knewstuff
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libcdio
           libcsv
           libkcddb
           libksane
           libxml2
           libxslt
           perl-text-bibtex
           poppler-qt6
           qtcharts
           qtsvg
           qtwayland
           qtwebengine
           solid
           sonnet
           taglib
           yaz))
    (home-page "https://tellico-project.org/")
    (synopsis "Collection manager")
    (description "Tellico is an application for organizing your collections.  It
provides default templates for books, bibliographies, videos, music, video
games, coins, stamps, trading cards, comic books, and wines.  It allows you to
enter your collection in a catalogue database, saving many different properties
like title, author, etc.

Features:

@itemize
@item Supports default collections of books, bibliographic entries, videos,
 music, video games, comic books, coins, stamps, trading cards, wines, board
 games, and file catalogs.
@item Supports user-defined custom collections.
@item Supports any number of user-defined fields, of several different types:
 text, paragraph, list, checkbox, number, URL, date, images, and combinations.
@item Handles entries with multiple authors, genres, keywords, etc.
#item Automatically formats titles and names.
@item Supports collection searching and view filtering.
@item Sorts and groups collection by various properties.
@item Allows customizable entry templates through XSLT.
@item Imports MODS, BibTeX, RIS, CSV, PDF metadata, and many other formats.
@item Exports to BibTeX, ONIX, CSV, HTML, and other formats.
@item Imports information directly from Amazon.com, IMDb, Z39.50 servers,
 PubMed, SRU servers, CrossRef.org, various other websites, and from external
 scripts.
@item Imports CDDB data for cataloging audio CDs.
@item Scans and imports audio file collections, such as mp3 or ogg.
@end itemize")
    (license license:gpl2+)))
