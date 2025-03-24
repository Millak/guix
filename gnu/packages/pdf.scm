;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2018, 2019, 2021, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016-2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2019,2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020-2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2024 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2024 dan <i@dan.games>
;;; Copyright © 2023 Benjamin Slade <slade@lambda-y.net>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Aaron Covrig <aaron.covrig.us@ieee.org>
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

(define-module (gnu packages pdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public capypdf
  (package
    (name "capypdf")
    (version "0.14.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jpakkane/capypdf")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "05rpicxw76z4q48ax0dx5rm1k4lhp4lbdr2aw58kly402w8kjdwb"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~(list "-Dcpp_std=c++23")
           #:test-options '(list "plainc")))
    (inputs (list freetype
                  gtk
                  lcms
                  libjpeg-turbo
                  libpng
                  zlib))
    (native-inputs (list font-google-noto
                         gcc-14         ; for std::format
                         ghostscript
                         pkg-config
                         python
                         python-pillow))
    (home-page "https://github.com/jpakkane/a4pdf")
    (synopsis "Color-managed PDF generator")
    (description "A4PDF is a low-level library for generating PDF files.
It does not have a document model and instead uses PDF primitives
directly.  It uses LittleCMS for color management but otherwise does not
convert data in any way.")
    (license license:asl2.0)))

(define-public a4pdf
  (deprecated-package "a4pdf" capypdf))

(define-public diffpdf
  (let ((commit "ba68231d3d05e0cb3a2d4a4fca8b70d4044f4303")
        (revision "1"))
    (package
      (name "diffpdf")
      (version (git-version "2.1.3.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/eang/diffpdf")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vwgv28b291lrcs9fljnlbnicv16lwj4vvl4bz6w3ldp9n5isjmf"))))
      (build-system qt-build-system)
      (arguments
       `(#:tests? #f))
      (inputs (list qtbase-5 qttools-5 qtwayland-5 poppler-qt5))
      (native-inputs (list pkg-config extra-cmake-modules))
      (home-page "http://www.qtrac.eu/diffpdf-foss.html")
      (synopsis "Compare two PDF files")
      (description
       "@command{diffpdf} lets you compare PDF files, offering three
comparison modes: words, characters, and appearance.")
      (license license:gpl2))))

(define-public extractpdfmark
  (package
    (name "extractpdfmark")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trueroad/extractpdfmark")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yzc3ajgdfb4ssxp49g2vrki45kl144j39bg0wdn6h9dc14kzmx4"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-home
                 ;; The test suite wants to write to /homeless-shelter
                 (lambda _ (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           ghostscript
           pkg-config
           (texlive-updmap.cfg)))
    (inputs
     (list poppler))
    (home-page "https://github.com/trueroad/extractpdfmark")
    (synopsis "Extract page mode and named destinations as PDFmark from PDF")
    (description
     "PDFmarks is a technique that accompanies PDF, and that is used to store
metadata such as author or title, but also structural information such as
bookmarks or hyperlinks.

When Ghostscript reads the main PDF generated by the TeX system with embedded
PDF files and outputs the final PDF, the PDF page mode and name targets
etc. are not preserved.  Therefore, when you open the final PDF, it is not
displayed correctly.  Also, remote PDF links do not work correctly.

This program is able to extract the page mode and named targets as PDFmark
from PDF.  In this way, you can obtain embedded PDF files that have kept this
information.")
    (license license:gpl3)))

(define-public flyer-composer
  (package
    (name "flyer-composer")
    (version "1.0rc2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flyer-composer" version))
       (sha256
        (base32 "17igqb5dlcgcq4nimjw6cf9qgz6a728zdx1d0rr90r2z0llcchsv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ;; TODO
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (qtbase (assoc-ref inputs "qtbase"))
                    (qml "/lib/qt5/qml"))
               (wrap-program (string-append out "/bin/flyer-composer-gui")
                 `("QT_PLUGIN_PATH" ":" =
                   (,(string-append qtbase "/lib/qt5/plugins")))
                 `("QT_QPA_PLATFORM_PLUGIN_PATH" ":" =
                   (,(string-append qtbase "/lib/qt5/plugins/platforms"))))))))))
    (inputs
     (list bash-minimal
           python-poppler-qt5
           python-pypdf2
           python-pyqt
           qtbase-5))
    (home-page "http://crazy-compilers.com/flyer-composer")
    (synopsis "Rearrange PDF pages to print as flyers on one sheet")
    (description "@command{flyer-composer} can be used to prepare one- or
two-sided flyers for printing on one sheet of paper.

Imagine you have designed a flyer in A6 format and want to print it using your
A4 printer.  Of course, you want to print four flyers on each sheet.  This is
where Flyer Composer steps in, creating a PDF which holds your flyer four
times.  If you have a second page, Flyer Composer can arrange it the same way
- even if the second page is in a separate PDF file.

This package contains both the command line tool and the gui too.")
    (license license:agpl3+)))

(define-public flyer-composer-cli
  (package/inherit flyer-composer
    (name "flyer-composer-cli")
    (arguments
     `(#:tests? #f ;; TODO
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-gui
           (lambda _
             (delete-file-recursively "flyer_composer/gui")
             (substitute* "setup.cfg"
               (("^\\s+flyer-composer-gui\\s*=.*") ""))
             #t)))))
    (inputs
     `(("python-pypdf2" ,python-pypdf2)))
    (description "@command{flyer-composer} can be used to prepare one- or
two-sided flyers for printing on one sheet of paper.

Imagine you have designed a flyer in A6 format and want to print it using your
A4 printer.  Of course, you want to print four flyers on each sheet.  This is
where Flyer Composer steps in, creating a PDF which holds your flyer four
times.  If you have a second page, Flyer Composer can arrange it the same way
- even if the second page is in a separate PDF file.

This package contains only the command line tool.  If you like to use the gui,
please install the @code{flyer-composer-gui} package.")))

(define-public poppler
  (package
   (name "poppler")
   (version "22.09.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://poppler.freedesktop.org/poppler-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0bhyli95h3dkirjc0ibh08s4nim6rn7f38sbfzdwln8k454gga6p"))))
   (build-system cmake-build-system)
   ;; FIXME:
   ;;  use libcurl:        no
   (inputs (list fontconfig
                 freetype
                 libjpeg-turbo
                 libpng
                 libtiff
                 lcms
                 nss                              ;for 'pdfsig'
                 openjpeg
                 poppler-data
                 zlib
                 ;; To build poppler-glib (as needed by Evince), we need Cairo and
                 ;; GLib.  But of course, that Cairo must not depend on Poppler.
                 cairo-sans-poppler))
   (propagated-inputs
    ;; As per poppler-cairo and poppler-glib.pc.
    ;; XXX: Ideally we'd propagate Cairo too, but that would require a
    ;; different solution to the circular dependency mentioned above.
    (list glib))
   (native-inputs
    (append
      (list pkg-config
            `(,glib "bin") ; glib-mkenums, etc.
            gobject-introspection
            python)
      (if (%current-target-system)
        (list pkg-config-for-build)
        '())))
   (arguments
    (list
     ;; The Poppler test suite needs to be downloaded separately and contains
     ;; non-free (and non-auditable) files, so we skip them.  See
     ;; <https://lists.gnu.org/archive/html/guix-devel/2022-06/msg00394.html>.
     #:tests? #f
     #:configure-flags
     #~(list "-DENABLE_UNSTABLE_API_ABI_HEADERS=ON" ;to install header files
             "-DENABLE_ZLIB=ON"
             "-DENABLE_BOOST=OFF"      ;disable Boost to save size
             (string-append "-DCMAKE_INSTALL_LIBDIR=" #$output "/lib")
             (string-append "-DCMAKE_INSTALL_RPATH=" #$output "/lib"))
     #:phases
     (if (%current-target-system) #~%standard-phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'set-PKG_CONFIG
               (lambda _
                 (setenv "PKG_CONFIG" #$(pkg-config-for-target))))))))
   (synopsis "PDF rendering library")
   (description
    "Poppler is a PDF rendering library based on the xpdf-3.0 code base.
Poppler gives access to the following binary programs:
@itemize
@item pdfattach
@item pdfdetach
@item pdffonts
@item pdfimages
@item pdfinfo
@item pdfseparate
@item pdfsig
@item pdftocairo
@item pdftohtml
@item pdftoppm
@item pdftops
@item pdftotext
@item pdfunite
@end itemize")
   (license license:gpl2+)
   (home-page "https://poppler.freedesktop.org/")))

(define-public poppler-data
  (package
    (name "poppler-data")
    (version "0.4.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://poppler.freedesktop.org/poppler-data"
                                  "-" version ".tar.gz"))
              (sha256
               (base32
                "137h4m48gc4v0srnr0gkwaqna6kfdqpy5886if5gjfmh3g6hbv1c"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no test suite
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; The package only provides some data files, so there is nothing to
         ;; build.
         (delete 'configure)
         (delete 'build))))
    (synopsis "Poppler encoding files for rendering of CJK and Cyrillic text")
    (description "This package provides optional encoding files for Poppler.
When present, Poppler is able to correctly render CJK and Cyrillic text.")
    (home-page (package-home-page poppler))
    ;; See COPYING in the source distribution for more information about
    ;; the licensing.
    (license (list license:bsd-3
                   license:gpl2))))

(define-public poppler-qt5
  (package/inherit poppler
   (name "poppler-qt5")
   (inputs `(("qtbase" ,qtbase-5)
             ,@(package-inputs poppler)))
   (synopsis "Qt5 frontend for the Poppler PDF rendering library")))

(define-public poppler-qt6
  (package/inherit poppler
    (name "poppler-qt6")
    (inputs (modify-inputs (package-inputs poppler)
              (append qtbase)))
    (synopsis "Qt6 frontend for the Poppler PDF rendering library")))

(define-public python-poppler-qt5
  (package
    (name "python-poppler-qt5")
    (version "21.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-poppler-qt5" version))
       (sha256
        (base32 "1q3gvmsmsq3llf9mcbhlkryrgprqrw2z7wmnvagy180f3y2fhxxl"))))
    (build-system pyproject-build-system)
    (arguments
     `(;; The sipbuild.api backend builder expects a Python dictionary as per
       ;; https://peps.python.org/pep-0517/#config-settings, but we
       ;; give it lists and it fails.  The next line is a workaround.
       #:configure-flags '#nil
       #:tests? #f))
    (native-inputs (list pkg-config))
    (inputs (list python-sip python-pyqt-builder python-pyqt poppler-qt5
                  qtbase-5))
    (home-page "https://github.com/frescobaldi/python-poppler-qt5")
    (synopsis "Python binding to Poppler-Qt5")
    (description
     "This package provides Python bindings for the Qt5 interface of the
Poppler PDF rendering library.")
    (license license:lgpl2.1+)))

(define-public libharu
  (package
    (name "libharu")
    (version "2.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libharu/libharu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00b89zqf0bxslx03ginzqdjg84zfmblq13p5f4xm0h05j7aq7ixz"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                  ; No tests
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-cmake
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("^install\\(FILES (README\\.md CHANGES) INSTALL DESTINATION .*\\)"
                       _ files)
                      (format #f "install(FILES ~a DESTINATION ~a/share/doc/~a-~a)"
                              files #$output #$name #$version))))))))
    (inputs
     (list libpng zlib))
    (home-page "http://libharu.org/")
    (synopsis "Library for generating PDF files")
    (description
     "libHaru is a library for generating PDF files.  libHaru does not support
reading and editing of existing PDF files.")
    (license license:zlib)))

(define-public xpdf
  (package
   (name "xpdf")
   (version "4.05")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "https://dl.xpdfreader.com/xpdf-" version ".tar.gz"))
      (sha256
       (base32 "1sjw5b7j404py1rblc8zxm6nar8km7yr2h7kffylyn5nmkapww4j"))))
   (build-system cmake-build-system)
   (inputs (list cups freetype libpng qtbase-5 zlib))
   (arguments
    `(#:tests? #f))                   ; there is no check target
   (synopsis "Viewer for PDF files based on the Motif toolkit")
   (description
    "Xpdf is a viewer for Portable Document Format (PDF) files.")
   (license license:gpl3)             ; or gpl2, but not gpl2+
   (home-page "https://www.xpdfreader.com/")))

(define-public zathura-cb
  (package
    (name "zathura-cb")
    (version "0.1.10")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-cb/download/zathura-cb-"
                              version ".tar.xz"))
              (sha256
               (base32
                "1j5v32f9ki35v1jc7a067anhlgqplzrp4fqvznlixfhcm0bwmc49"))))
    (native-inputs (list pkg-config))
    (inputs (list libarchive zathura))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; package does not contain tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-plugin-directory
           ;; Something of a regression in 0.1.10: the new Meson build system
           ;; now hard-codes an incorrect plugin directory.  Fix it.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "meson.build"
               (("(install_dir:).*" _ key)
                (string-append key
                               "'" (assoc-ref outputs "out") "/lib/zathura'\n")))
             #t)))))
    (home-page "https://pwmt.org/projects/zathura-cb/")
    (synopsis "Comic book support for zathura (libarchive backend)")
    (description "The zathura-cb plugin adds comic book support to zathura
using libarchive.")
    (license license:zlib)))

(define-public zathura-ps
  (package
    (name "zathura-ps")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-ps/download/zathura-ps-"
                              version ".tar.xz"))
              (sha256
               (base32
                "0ilf63wxn1yzis9m3qs8mxbk316yxdzwxrrv86wpiygm9hhgk5sq"))))
    (native-inputs (list pkg-config))
    (inputs (list libspectre zathura))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; package does not contain tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-plugin-directory
           ;; Something of a regression in 0.2.7: the new Meson build system
           ;; now hard-codes an incorrect plugin directory.  Fix it.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "meson.build"
               (("(install_dir:).*" _ key)
                (string-append key
                               "'" (assoc-ref outputs "out") "/lib/zathura'\n")))
             #t)))))
    (home-page "https://pwmt.org/projects/zathura-ps/")
    (synopsis "PS support for zathura (libspectre backend)")
    (description "The zathura-ps plugin adds PS support to zathura
using libspectre.")
    (license license:zlib)))

(define-public zathura-djvu
  (package
    (name "zathura-djvu")
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-djvu/download/zathura-djvu-"
                              version ".tar.xz"))
              (sha256
               (base32
                "0062n236414db7q7pnn3ccg5111ghxj3407pn9ri08skxskgirln"))))
    (native-inputs (list pkg-config))
    (inputs
     (list djvulibre zathura))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; package does not contain tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-plugin-directory
           ;; Something of a regression in 0.2.8: the new Meson build system
           ;; now hard-codes an incorrect plugin directory.  Fix it.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "meson.build"
               (("(install_dir:).*" _ key)
                (string-append key
                               "'" (assoc-ref outputs "out") "/lib/zathura'\n")))
             #t)))))
    (home-page "https://pwmt.org/projects/zathura-djvu/")
    (synopsis "DjVu support for zathura (DjVuLibre backend)")
    (description "The zathura-djvu plugin adds DjVu support to zathura
using the DjVuLibre library.")
    (license license:zlib)))

(define-public zathura-pdf-mupdf
  (package
    (name "zathura-pdf-mupdf")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-pdf-mupdf"
                              "/download/zathura-pdf-mupdf-" version ".tar.xz"))
              (sha256
               (base32
                "0xk7fxgx5fiafczwqlpb3hkfmfhhq2ljabxvi272m9vy13p89kwc"))))
    (native-inputs (list pkg-config))
    (inputs
     (list gumbo-parser
           jbig2dec
           libjpeg-turbo
           mujs
           mupdf
           openjpeg
           openssl
           tesseract-ocr
           zathura))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; package does not contain tests
       #:configure-flags (list (string-append "-Dplugindir="
                                              (assoc-ref %outputs "out")
                                              "/lib/zathura"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-libmupdfthird.a-requirement
           (lambda _
             ;; Ignore a missing (apparently superfluous) static library.
             (substitute* "meson.build"
               (("mupdfthird = .*")
                "")
               ((", mupdfthird")
                ""))))
         (add-after 'unpack 'fix-mupdf-detection
           (lambda _
             (substitute* "meson.build"
               (("dependency\\('mupdf', required: false\\)")
                "cc.find_library('mupdf')")))))))
    (home-page "https://pwmt.org/projects/zathura-pdf-mupdf/")
    (synopsis "PDF support for zathura (mupdf backend)")
    (description "The zathura-pdf-mupdf plugin adds PDF support to zathura
by using the @code{mupdf} rendering library.")
    (license license:zlib)))

(define-public zathura-pdf-poppler
  (package
    (name "zathura-pdf-poppler")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-pdf-poppler/download/zathura-pdf-poppler-"
                              version ".tar.xz"))
              (sha256
               (base32
                "049h8m9swxni7ar6fsbm0hb3fg4ffmjc3m6vyg78ilfi3kayxavi"))))
    (native-inputs (list pkg-config))
    (inputs
     (list poppler zathura))
    (build-system meson-build-system)
    (arguments
     (list #:tests? #f                ; package does not include tests
           #:configure-flags
           #~(list (string-append "-Dplugindir=" #$output "/lib/zathura"))))
    (home-page "https://pwmt.org/projects/zathura-pdf-poppler/")
    (synopsis "PDF support for zathura (poppler backend)")
    (description "The zathura-pdf-poppler plugin adds PDF support to zathura
by using the poppler rendering engine.")
    (license license:zlib)))

(define-public zathura
  (package
    (name "zathura")
    (version "0.5.6")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura/download/zathura-"
                              version ".tar.xz"))
              (sha256
               (base32
                "1nhhdww8z6i2cmj7n6qjgyh49dy4jf0xq4j13djpvrfchxgf6y5l"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'start-xserver
            ;; Tests require a running X server.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((display ":1"))
                (setenv "DISPLAY" display)

                ;; On busy machines, tests may take longer than
                ;; the default of four seconds.
                (setenv "CK_DEFAULT_TIMEOUT" "20")

                ;; Don't fail due to missing '/etc/machine-id'.
                (setenv "DBUS_FATAL_WARNINGS" "0")
                (zero? (system (string-append
                                (search-input-file inputs "/bin/Xvfb")
                                " " display " &")))))))))
    (native-inputs
     (list pkg-config
           gettext-minimal
           (list glib "bin")

           ;; For building documentation.
           python-sphinx

           ;; For building icons.
           (librsvg-for-system)

           ;; For tests.
           check
           xorg-server-for-tests))
    (inputs (list sqlite))
    ;; Listed in 'Requires.private' of 'zathura.pc'.
    (propagated-inputs (list cairo girara))
    (native-search-paths
     (list (search-path-specification
            (variable "ZATHURA_PLUGINS_PATH")
            (files '("lib/zathura")))))
    (home-page "https://pwmt.org/projects/zathura/")
    (synopsis "Lightweight keyboard-driven PDF viewer")
    (description "Zathura is a customizable document viewer.  It provides a
minimalistic interface and an interface that mainly focuses on keyboard
interaction.")
    (license license:zlib)))

(define-public podofo-0.9
  (package
    (name "podofo")
    (version "0.9.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/podofo/podofo")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fyv0zbl6zs93wy0qb3mjkfm99pgz5275nkzss115ww2w04h0ssl"))))
    (build-system cmake-build-system)
    (native-inputs
     (list cppunit pkg-config))
    (inputs
     (list fontconfig
           freetype
           libjpeg-turbo
           libpng
           libtiff
           lua-5.1
           openssl
           zlib))
    (arguments
     (list
       #:configure-flags
         #~(list "-DPODOFO_BUILD_SHARED=ON")
       #:phases
         #~(modify-phases %standard-phases
           (add-before 'configure 'patch
             (lambda _
               ;; Look for freetype include files in the correct place.
               (substitute* "cmake/modules/FindFREETYPE.cmake"
                 (("/usr/local") #$freetype)))))))
    (home-page "https://github.com/podofo/podofo")
    (synopsis "Tools to work with the PDF file format")
    (description
     "PoDoFo is a C++ library and set of command-line tools to work with the
PDF file format.  It can parse PDF files and load them into memory, and makes
it easy to modify them and write the changes to disk.  It is primarily useful
for applications that wish to do lower level manipulation of PDF, such as
extracting content or merging files.")
    (license license:lgpl2.0+)))

(define-public podofo
  (package
    (name "podofo")
    (version "0.10.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/podofo/podofo")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ffdx3ghz0an3fkz72m0q77g5ipcriapxnphha20gk4b575a93v5"))))
    (build-system cmake-build-system)
    (native-inputs
     (list cppunit pkg-config))
    (inputs
     (list fontconfig
           freetype
           libjpeg-turbo
           libpng
           libtiff
           libxml2
           lua-5.1
           openssl
           zlib))
    (home-page "https://github.com/podofo/podofo")
    (synopsis "Tools to work with the PDF file format")
    (description
     "PoDoFo is a C++ library and set of command-line tools to work with the
PDF file format.  It can parse PDF files and load them into memory, and makes
it easy to modify them and write the changes to disk.  It is primarily useful
for applications that wish to do lower level manipulation of PDF, such as
extracting content or merging files.")
    (license license:lgpl2.0+)))

(define-public python-pydyf
  (package
    (name "python-pydyf")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydyf" version))
       (sha256
        (base32 "18q43g5d9455msipcgd5fvnh8m4a2rz189slzfg80yycjw66rshs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~'("-c" "/dev/null")))
    (propagated-inputs (list python-pillow))
    (native-inputs
     (list ghostscript
           python-flit-core
           python-pytest))
    (home-page "https://github.com/CourtBouillon/pydyf")
    (synopsis "Low-level PDF generator")
    (description "@code{pydyf} is a low-level PDF generator written in Python
and based on PDF specification 1.7.")
    (license license:bsd-3)))

(define-public mupdf
  (package
    (name "mupdf")
    (version "1.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mupdf.com/downloads/archive/"
                           "mupdf-" version "-source.tar.lz"))
       (sha256
        (base32 "0lg45wp3ici2g2i49fmwa1k32bgkqqgl51nxnqqk0i8ilmdh8hnx"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        ;; Remove bundled software.  Keep patched variants.
        #~(with-directory-excursion "thirdparty"
            (let ((keep '("README" "extract" "freeglut" "lcms2")))
              (for-each delete-file-recursively
                        (lset-difference string=?
                                         (scandir ".")
                                         (cons* "." ".." keep))))))))
    (build-system gnu-build-system)
    (inputs
     (list curl
           libxrandr
           libxi
           freeglut                     ;for GL/gl.h
           freetype
           gumbo-parser
           harfbuzz
           jbig2dec
           libjpeg-turbo
           libx11
           libxext
           mujs
           openjpeg
           openssl
           zlib))
    (native-inputs
     (list pkg-config))
    (arguments
     (list
      #:tests? #f                       ;no check target
      #:make-flags
      #~(list "verbose=yes"
              (string-append "CC=" #$(cc-for-target))
              "XCFLAGS=-fpic"
              "USE_SYSTEM_FREETYPE=yes"
              "USE_SYSTEM_GUMBO=yes"
              "USE_SYSTEM_HARFBUZZ=yes"
              "USE_SYSTEM_JBIG2DEC=yes"
              "USE_SYSTEM_JPEGXR=no # not available"
              "USE_SYSTEM_LCMS2=no # lcms2mt is strongly preferred"
              "USE_SYSTEM_LIBJPEG=yes"
              "USE_SYSTEM_MUJS=yes"
              "USE_SYSTEM_OPENJPEG=yes"
              "USE_SYSTEM_ZLIB=yes"
              "USE_SYSTEM_GLUT=no"
              "USE_SYSTEM_CURL=yes"
              "USE_SYSTEM_LEPTONICA=yes"
              "USE_SYSTEM_TESSERACT=yes"
              "USE_SONAME=no"           ;install as libmupdf.so
              "shared=yes"
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
              (string-append "prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)))) ;no configure script
    (home-page "https://mupdf.com")
    (synopsis "Lightweight PDF viewer and toolkit")
    (description
     "MuPDF is a C library that implements a PDF and XPS parsing and
rendering engine.  It is used primarily to render pages into bitmaps,
but also provides support for other operations such as searching and
listing the table of contents and hyperlinks.

The library ships with a rudimentary X11 viewer, and a set of command
line tools for batch rendering @command{pdfdraw}, rewriting files
@command{pdfclean}, and examining the file structure @command{pdfshow}.")
    (license (list license:agpl3+
                   license:bsd-3        ;resources/cmaps
                   license:x11          ;thirdparty/lcms2
                   license:silofl1.1    ;resources/fonts/{han,noto,sil,urw}
                   license:asl2.0)))) ; resources/fonts/droid

(define-public mupdf-1.24 ; Needed for sioyek
  (package
    (inherit mupdf)
    (name "mupdf")
    (version "1.24.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mupdf.com/downloads/archive/"
                           "mupdf-" version "-source.tar.lz"))
       (sha256
        (base32 "0hydmp8sdnkrkpqyysa6klkxbwv9awf1xc753r27gcj7ds7375fj"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        ;; Remove bundled software.  Keep patched variants.
        #~(with-directory-excursion "thirdparty"
            (let ((keep '("README" "extract" "freeglut" "lcms2")))
              (for-each delete-file-recursively
                        (lset-difference string=?
                                         (scandir ".")
                                         (cons* "." ".." keep))))))))))


(define-public qpdf
  (package
    (name "qpdf")
    (version "11.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/qpdf/qpdf/" version
                                  "/qpdf-" version ".tar.gz"))
              (sha256
               (base32
                "0n8jfk4yf0m36rs9lg82pj9lv6pdqpfh8mhacc1ih9ahpigiycnr"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~'("-DBUILD_STATIC_LIBS=OFF")))
    (native-inputs
     (list perl pkg-config))
    (propagated-inputs
     ;; In Requires.private of libqpdf.pc.
     (list libjpeg-turbo zlib))
    (synopsis "Command-line tools and library for transforming PDF files")
    (description
     "QPDF is a command-line program that does structural, content-preserving
transformations on PDF files.  It could have been called something like
pdf-to-pdf.  It includes support for merging and splitting PDFs and to
manipulate the list of pages in a PDF file.  It is not a PDF viewer or a
program capable of converting PDF into other formats.")
    ;; Prior to the 7.0 release, QPDF was licensed under Artistic 2.0.
    ;; Users can still choose to use the old license at their option.
    (license (list license:asl2.0 license:clarified-artistic))
    (home-page "https://qpdf.sourceforge.io/")))

(define-public qpdfview
  (package
    (name "qpdfview")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/qpdfview/"
                           "trunk/" version "/+download/"
                           "qpdfview-" (version-major+minor version) ".tar.gz"))
       (sha256
        (base32 "16dy341927r2s1dza7g8ci1jyypfc4a6yfcvg9sxvjv1li0c9vs4"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list cups
           djvulibre
           libspectre
           poppler-qt5
           qtbase-5
           qtsvg-5))
    (arguments
     (list #:tests? #f ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (substitute* "qpdfview.pri"
                     (("/usr") #$output))
                   (invoke "qmake" "qpdfview.pro"))))))
    (home-page "https://launchpad.net/qpdfview")
    (synopsis "Tabbed document viewer")
    (description "@command{qpdfview} is a document viewer for PDF, PS and DJVU
files.  It uses the Qt toolkit and features persistent per-file settings,
configurable toolbars and shortcuts, continuous and multi‐page layouts,
SyncTeX support, and rudimentary support for annotations and forms.")
    (license license:gpl2+)))

(define-public unpaper
  (package
    (name "unpaper")
    (version "7.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.flameeyes.com/files/unpaper-"
                            version ".tar.xz"))
        (sha256
         (base32 "103awjdl2qrzi0qc32hi8zvwf04r5ih5jaw8rg8ij9y24szznx95"))))
    (native-inputs
     (list pkg-config python-sphinx))
    (inputs
     (list discount ffmpeg))
    (build-system meson-build-system)
    (home-page "https://www.flameeyes.com/projects/unpaper")
    (synopsis "Post-processing tool for scanned pages")
    (description "@command{unpaper} is a post-processing tool for
scanned sheets of paper, especially for book pages that have been
scanned from previously created photocopies.

Its main purpose is to make scanned book pages better readable on screen
after conversion to PDF.  Additionally, unpaper might be useful to
enhance the quality of scanned pages before performing
@acronym{OCR, optical character recognition}.")
    (license license:gpl2)))

(define-public xournal
  (package
    (name "xournal")
    (version "0.4.8.2016")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/xournal/xournal/" version
                           "/xournal-" version ".tar.gz"))
       (sha256
        (base32
         "09i88v3wacmx7f96dmq0l3afpyv95lh6jrx16xzm0jd1szdrhn5j"))))
    (build-system gnu-build-system)
    (inputs
     (list gtk+-2 pango poppler glib libgnomecanvas))
    (native-inputs
     (list pkg-config))
    (home-page "https://xournal.sourceforge.net/")
    (synopsis "Notetaking using a stylus")
    (description
     "Xournal is an application for notetaking, sketching, keeping a journal
using a stylus.")
    (license license:gpl2+)))

(define-public xournalpp
  (package
    (name "xournalpp")
    (version "1.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xournalpp/xournalpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wsks4wwv4d6y2drd64c0p8986l5sv09pnlvpd7hl4asszxmybjm"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DENABLE_GTEST=ON")
      #:imported-modules `((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
      #:modules '(((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build cmake-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          ;; Fix path to addr2line utility, which the crash reporter uses.
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/util/Stacktrace.cpp"
                ;; Match only the commandline.
                (("\"addr2line ")
                 (string-append "\""
                                (search-input-file inputs "/bin/addr2line")
                                " ")))))
          (add-after 'build 'prepare-tests
            (lambda _
              (invoke "cmake" "--build" "." "--target" "test-units")))
          (add-after 'install 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
          (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
            (assoc-ref glib-or-gtk:%standard-phases
                       'generate-gdk-pixbuf-loaders-cache-file))
          (add-after 'glib-or-gtk-wrap 'wrap-gdk-pixbuf
            ;; This phase is necessary for xournalpp to load SVG icons.
            (lambda _
              (let ((pixbuf-module-file (getenv "GDK_PIXBUF_MODULE_FILE")))
                (wrap-program (string-append #$output "/bin/xournalpp")
                  `("GDK_PIXBUF_MODULE_FILE" = (,pixbuf-module-file)))))))))
    (native-inputs
     (list cppunit gettext-minimal googletest help2man pkg-config))
    (inputs
     (list adwaita-icon-theme
           alsa-lib
           gtk+
           (librsvg-for-system)
           libsndfile
           libxml2
           libzip
           lua
           poppler
           portaudio))
    (home-page "https://github.com/xournalpp/xournalpp")
    (synopsis "Handwriting notetaking software with PDF annotation support")
    (description "Xournal++ is a hand note taking software written in
C++ with the target of flexibility, functionality and speed.  Stroke
recognizer and other parts are based on Xournal code.

Xournal++ features:

@itemize
@item Support for Pen pressure, e.g., Wacom Tablet
@item Support for annotating PDFs
@item Fill shape functionality
@item PDF Export (with and without paper style)
@item PNG Export (with and without transparent background)
@item Map different tools / colors etc. to stylus buttons /
mouse buttons
@item Sidebar with Page Previews with advanced page sorting, PDF
Bookmarks and Layers (can be individually hidden, editing layer can be
selected)
@item enhanced support for image insertion
@item Eraser with multiple configurations
@item LaTeX support
@item bug reporting, autosave, and auto backup tools
@item Customizable toolbar, with multiple configurations, e.g., to
optimize toolbar for portrait / landscape
@item Page Template definitions
@item Shape drawing (line, arrow, circle, rectangle)
@item Shape resizing and rotation
@item Rotation snapping every 45 degrees
@item Rect snapping to grid
@item Audio recording and playback alongside with handwritten notes
@item Multi Language Support, Like English, German, Italian...
@item Plugins using LUA Scripting
@end itemize")
    (license license:gpl2+)))

(define-public python-reportlab
  (package
    (name "python-reportlab")
    (version "4.0.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "reportlab" version))
              (sha256
               (base32
                "0lq8fibbgp7bfasxjf33s4hzqr405y655bkxggxmjxqsga0lb68n"))))
    (build-system python-build-system)
    (arguments
     (list
      #:test-target "tests"
      #:configure-flags '(list "--no-download-t1-files")
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'find-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((dlt1 (assoc-ref inputs "font-curve-files")))
               (substitute* "setup.py"
                 (("http://www.reportlab.com/ftp/pfbfer-20180109.zip")
                  (string-append "file://" dlt1)))))))))
    (inputs
     `(("font-curve-files"
        ,(origin
           (method url-fetch)
           (uri "http://www.reportlab.com/ftp/pfbfer-20180109.zip")
           (sha256
            (base32
             "1v0gy4mbx02ys96ssx89420y0njknlrxs2bx64bv4rp8a0al66w5"))))))
    (propagated-inputs
     (list python-chardet python-pillow))
    (home-page "https://www.reportlab.com")
    (synopsis "Python library for generating PDFs and graphics")
    (description "This is the ReportLab PDF Toolkit.  It allows rapid creation
of rich PDF documents, and also creation of charts in a variety of bitmap and
vector formats.")
    (license license:bsd-3)))

(define-public impressive
  (package
    (name "impressive")
    ;; (version "0.13.1")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/impressive/Impressive/"
                           version "/Impressive-" version ".tar.gz"))
       (sha256
        (base32
         ;; "0d1d2jxfl9vmy4swcdz660xd4wx91w1i3n07k522pccapwxig294"))))
         "0g15q67f992prkjndrk75hhd601iypfmkafhdx7hijs2byr26c83"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (delete 'build)
                   (delete 'configure)
                   (delete 'check)
                   (replace 'install
                     (lambda* (#:key inputs #:allow-other-keys)
                       ;; There's no 'setup.py' so install things manually.
                       (let* ((bin (string-append #$output "/bin"))
                              (impressive (string-append bin "/impressive"))
                              (man1 (string-append #$output "/share/man/man1")))
                         (mkdir-p bin)
                         (copy-file "impressive.py" impressive)
                         (chmod impressive #o755)
                         (wrap-program (string-append bin "/impressive")
                           `("PATH" ":" prefix ;for pdftoppm
                             (,(search-input-file inputs "bin/xpdf"))))
                         (install-file "impressive.1" man1)))))))
    ;; TODO: Add dependency on pdftk.
    (inputs (list bash-minimal python-pygame python-pillow sdl xpdf))
    (home-page "https://impressive.sourceforge.net")
    (synopsis "PDF presentation tool with visual effects")
    (description
     "Impressive is a tool to display PDF files that provides visual effects
such as smooth alpha-blended slide transitions.  It provides additional tools
such as zooming, highlighting an area of the screen, and a tool to navigate
the PDF pages.")
    (license license:gpl2)))

(define-public img2pdf
  (package
    (name "img2pdf")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "img2pdf" version))
       (sha256
        (base32 "158bgnk2jhjnpyld4z3jq8v2j8837vh4j0672g8mnjrg4i3px13k"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pikepdf python-pillow
           `(,python "tk")))
    (home-page "https://gitlab.mister-muffin.de/josch/img2pdf")
    (synopsis "Convert images to PDF via direct JPEG inclusion")
    (description
     "img2pdf converts images to PDF via direct JPEG inclusion.  That
conversion is lossless: the image embedded in the PDF has the exact same color
information for every pixel as the input.")
    (license license:lgpl3)))

(define-public fbida
  (package
    (name "fbida")
    (version "2.14")
    (home-page "https://www.kraxel.org/blog/linux/fbida/")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.kraxel.org/releases/fbida/"
                                  "fbida-" version ".tar.gz"))
              (sha256
               (base32
                "0f242mix20rgsqz1llibhsz4r2pbvx6k32rmky0zjvnbaqaw1dwm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-FHS-file-names
           (lambda _
             (substitute* "mk/Autoconf.mk"
               (("/bin/echo") "echo")
               (("/sbin/ldconfig -p") "echo lib")) #t))
         (add-before 'build 'set-fcommon
           (lambda _
             (setenv "CFLAGS" "-fcommon")))
         (delete 'configure))
        #:tests? #f
        #:make-flags
        (list (string-append "CC=" ,(cc-for-target))
              (string-append "prefix=" (assoc-ref %outputs "out")))))
    (inputs `(("libjpeg" ,libjpeg-turbo)
              ("curl" ,curl)
              ("libtiff" ,libtiff)
              ("libudev" ,eudev)
              ("libwebp" ,libwebp)
              ("libdrm" ,libdrm)
              ("giflib" ,giflib)
              ("glib" ,glib)
              ("cairo-xcb" ,cairo-xcb)
              ("freetype" ,freetype)
              ("fontconfig" ,fontconfig)
              ("libexif" ,libexif)
              ("mesa" ,mesa)
              ("libepoxy" ,libepoxy)
              ("libpng" ,libpng)
              ("poppler" ,poppler)))
    (native-inputs (list pkg-config))
    (synopsis "Framebuffer and drm-based image viewer")
    (description
      "fbida contains a few applications for viewing and editing images on
the framebuffer.")
    (license license:gpl2+)))

(define-public pdfcrack
  (package
    (name "pdfcrack")
    (version "0.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/pdfcrack/pdfcrack/"
                                  "pdfcrack-" version "/"
                                  "pdfcrack-" version ".tar.gz"))
              (sha256
               (base32
                "1d751n38cbagxqpw6ncvf3jfv7zhxl3fwh5nms2bjp6diyqjk2vv"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:make-flags #~(list (string-append "CC="
                                               #$(cc-for-target)))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure) ;no configure script
                        (replace 'install
                          (lambda _
                            (install-file "pdfcrack"
                                          (string-append #$output "/bin")))))))
    (home-page "https://pdfcrack.sourceforge.net/")
    (synopsis "Password recovery tool for PDF files")
    (description "PDFCrack is a simple tool for recovering passwords from PDF
documents that use the standard security handler.")
    (license license:gpl2+)))

(define-public pdf2svg
  (package
    (name "pdf2svg")
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dawbarton/pdf2svg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14ffdm4y26imq99wjhkrhy9lp33165xci1l5ndwfia8hz53bl02k"))))
    (build-system gnu-build-system)
    (inputs
     (list cairo poppler))
    (native-inputs
     (list pkg-config))
    (home-page "http://www.cityinthesky.co.uk/opensource/pdf2svg/")
    (synopsis "PDF to SVG converter")
    (description "@command{pdf2svg} is a simple command-line PDF to SVG
converter using the Poppler and Cairo libraries.")
    (license license:gpl2+)))

(define-public python-pypdf
  (package
    (name "python-pypdf")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/py-pdf/pypdf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dl3nmvsk43s2v6a5cwwvfwpyvhsl9wcrdnqbzjsp50zqibi23pz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Disable tests that use the network and non-free assets.
      #:test-flags
      #~(list "-m" "not samples and not enable_socket"
              "--numprocesses" (number->string (parallel-job-count))
              "-k" (string-join
                    ;; ValueError: cannot save mode RGBA
                    (list "not test_replace_image"
                          ;; assert None
                          "test_writer_xmp_metadata_samples")
                    " and not "))))
    (native-inputs
     (list python-flit
           python-pytest
           python-pytest-socket
           python-pytest-timeout
           python-pyyaml
           python-pytest-xdist))
    (propagated-inputs
     (list python-typing-extensions
           python-pillow))
    (home-page "https://github.com/py-pdf/pypdf")
    (synopsis "Python PDF library")
    (description
     "This package provides a PDF library capable of splitting, merging,
cropping, and transforming PDF files.")
    (license license:bsd-3)))

(define-public python-pypdf2
  (package
    (name "python-pypdf2")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PyPDF2" version))
              (sha256
               (base32
                "11a3aqljg4sawjijkvzhs3irpw0y67zivqpbjpm065ha5wpr13z2"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-test-suite
          (lambda _
            ;; The text-file needs to be opened in binary mode for Python 3,
            ;; so patch in the "b"
            (substitute* "Tests/tests.py"
              (("pdftext_file = open\\(.* 'crazyones.txt'\\), 'r" line)
               (string-append line "b")))
            #t))
         (replace 'check
           (lambda _
             (invoke "python" "-m" "unittest" "Tests.tests"))))))
    (home-page "http://mstamy2.github.com/PyPDF2")
    (synopsis "Pure Python PDF toolkit")
    (description "PyPDF2 is a pure Python PDF library capable of:

@enumerate
@item extracting document information (title, author, …)
@item splitting documents page by page
@item merging documents page by page
@item cropping pages
@item merging multiple pages into a single page
@item encrypting and decrypting PDF files
@end enumerate

By being pure Python, it should run on any Python platform without any
dependencies on external libraries.  It can also work entirely on
@code{StringIO} objects rather than file streams, allowing for PDF
manipulation in memory.  It is therefore a useful tool for websites that
manage or manipulate PDFs.")
    (license license:bsd-3)))

(define-public pdfarranger
  (package
    (name "pdfarranger")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jeromerobert/pdfarranger")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "045a6j5mh2ixrx3awrpfqh6l3x61i4jrv8r73xz1mvw0bc97lxbc"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-for-typelib
            (lambda _
              (let ((program (string-append #$output "/bin/pdfarranger")))
                (wrap-program program
                  `("GI_TYPELIB_PATH" ":" prefix
                    (,(getenv "GI_TYPELIB_PATH")))))))
          (add-before 'sanity-check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list intltool python-distutils-extra))
    (inputs
     (list bash-minimal gtk+ poppler))
    (propagated-inputs
     (list img2pdf
           python-dateutil
           python-pikepdf
           python-pycairo
           python-pygobject))
    (home-page "https://github.com/jeromerobert/pdfarranger")
    (synopsis "Merge, split and re-arrange pages from PDF documents")
    (description
     "PDF Arranger is a small application which allows one to merge or split
PDF documents and rotate, crop and rearrange their pages using an interactive
and intuitive graphical interface.

PDF Arranger was formerly known as PDF-Shuffler.")
    (license license:gpl3+)))

(define-public pdfposter
  (package
    (name "pdfposter")
    (version "0.7.post1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pdftools.pdfposter" version))
              (sha256
               (base32
                "0c1avpbr9q53yzq5ar2x485rmp9d0l3z27aham32bg7gplzd7w0j"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))  ; test-suite not included in source archive
    (inputs
     (list python-pypdf2))
    (home-page "https://pythonhosted.org/pdftools.pdfposter/")
    (synopsis "Scale and tile PDF images/pages to print on multiple pages")
    (description "@command{pdfposter} can be used to create a large poster by
building it from multiple pages and/or printing it on large media.  It expects
as input a PDF file, normally printing on a single page.  The output is again
a PDF file, maybe containing multiple pages together building the poster.  The
input page will be scaled to obtain the desired size.

This is much like @command{poster} does for Postscript files, but working with
PDF.  Since sometimes @command{poster} does not like your files converted from
PDF.  Indeed @command{pdfposter} was inspired by @command{poster}.")
    (license license:gpl3+)))

(define-public pdfgrep
  (package
    (name "pdfgrep")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pdfgrep.org/download/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jr0qqvkcb3xz0ps111cqwwxp1b5g5rrf75ab5whkvy0whqyaq86"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libgcrypt pcre poppler))
    (home-page "https://pdfgrep.org")
    (synopsis "Command-line utility to search text in PDF files")
    (description
     "Pdfgrep searches in pdf files for strings matching a regular expression.
Support some GNU grep options as file name output, page number output,
optional case insensitivity, count occurrences, color highlights and search in
multiple files.")
    (license license:gpl2+)))

(define-public pdfpc
  (package
    (name "pdfpc")
    (version "4.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pdfpc/pdfpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sx3ivnwyfr32hf1424aafpljhq5nm6pngl2zhvjsb24gnp45y3w"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))           ; no test target
    (inputs (list
             cairo
             discount ; libmarkdown
             qrencode
             gtk+
             gstreamer
             gst-plugins-base
             json-glib
             libgee
             poppler
             pango
             vala
             webkitgtk-for-gtk3))
    (native-inputs
     (list pkg-config))
    (home-page "https://pdfpc.github.io/")
    (synopsis "Presenter console with multi-monitor support for PDF files")
    (description
     "pdfpc is a presentation viewer application which uses multi-monitor
output to provide meta information to the speaker during the presentation.  It
is able to show a normal presentation window on one screen, while showing a
more sophisticated overview on the other one providing information like a
picture of the next slide, as well as the left over time till the end of the
presentation.  The input files processed by pdfpc are PDF documents.")
    (license license:gpl3+)))

(define-public paps
  (package
    (name "paps")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dov/paps/releases/download/v"
                           version "/paps-" version ".tar.gz"))
       (sha256
        (base32 "1z1w1fg2bvb8p92n1jlpqp3n9mq42szb2mqhh4xqmmnmfcdkpi9s"))))
    (build-system gnu-build-system)
    (inputs
     (list pango))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://github.com/dov/paps")
    (synopsis "Pango to PostScript converter")
    (description
     "Paps reads a UTF-8 encoded file and generates a PostScript language
rendering of the file through the Pango Cairo back end.")
    (license license:lgpl2.0+)))

(define-public stapler
  (package
    (name "stapler")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stapler" version))
       (sha256
        (base32
         "0b2lbm3f79cdxcsagwhzihbzwahjabxqmbws0c8ki25gpdnygdd7"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-more-itertools-version-requirement
           (lambda _
             ;; Tests require an version of the more-itertools module older
             ;; than the one we have packaged.
             (substitute* "setup.py"
               (("more-itertools>=2\\.2,<6\\.0\\.0") "more-itertools>=2.2"))
             #t)))))
    (propagated-inputs
     (list python-more-itertools python-pypdf2))
    (home-page "https://github.com/hellerbarde/stapler")
    (synopsis "PDF manipulation tool")
    (description "Stapler is a pure Python alternative to PDFtk, a tool for
manipulating PDF documents from the command line.  It supports

@itemize
@item cherry-picking pages and concatenating them into a new file
@item splitting a PDF document into single pages each in its own file
@item merging PDF documents with their pages interleaved
@item displaying metadata in a PDF document
@item displaying the mapping between logical and physical page numbers
@end itemize")
    (license license:bsd-3)))

(define-public weasyprint
  (package
    (name "weasyprint")
    (version "56.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kozea/WeasyPrint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08l0yaqg0rxnb2r3x4baf4wng5pxpjbyalnrl4glwh9l69740q7p"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-c" "/dev/null"
                           "-n" (number->string (parallel-job-count)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-library-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "weasyprint/text/ffi.py"
                (("'gobject-2.0-0'")
                 (format #f "~s"
                         (search-input-file inputs "lib/libgobject-2.0.so")))
                (("'pango-1.0-0'")
                 (format #f "~s"
                         (search-input-file inputs "lib/libpango-1.0.so")))
                (("'harfbuzz'")
                 (format #f "~s"
                         (search-input-file inputs "lib/libharfbuzz.so")))
                (("'fontconfig-1'")
                 (format #f "~s"
                         (search-input-file inputs "lib/libfontconfig.so")))
                (("'pangoft2-1.0-0'")
                 (format #f "~s"
                         (search-input-file inputs
                                            "lib/libpangoft2-1.0.so")))))))))
    (inputs (list fontconfig glib harfbuzz pango))
    (propagated-inputs
     (list gdk-pixbuf
           python-cairocffi
           python-cairosvg
           python-cffi
           python-cssselect2
           python-fonttools
           python-html5lib
           python-pillow
           python-pydyf
           python-pyphen
           python-tinycss2))
    (native-inputs
     (list font-dejavu                  ;tests depend on it
           ghostscript
           python-flit-core
           python-pytest
           python-pytest-xdist))
    (home-page "https://weasyprint.org/")
    (synopsis "Document factory for creating PDF files from HTML")
    (description "WeasyPrint helps web developers to create PDF documents.  It
turns simple HTML pages into gorgeous statistical reports, invoices, tickets,
etc.

From a technical point of view, WeasyPrint is a visual rendering engine for
HTML and CSS that can export to PDF and PNG.  It aims to support web standards
for printing.

It is based on various libraries but not on a full rendering engine like
WebKit or Gecko.  The CSS layout engine is written in Python, designed for
pagination, and meant to be easy to hack on.  Weasyprint can also be used as a
python library.

Keywords: html2pdf, htmltopdf")
    (license license:bsd-3)))

(define-public sioyek
  (package
    (name "sioyek")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ahrm/sioyek")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vmmp2s032ygh1byz77pg9aljmp8hx745fr7mmz11831f96mlmhq"))
       (modules '((guix build utils)))
       ;; libmupdf-third.so no longer available since mupdf 1.18.0.
       (snippet '(substitute* "pdf_viewer_build_config.pro"
                   (("-lmupdf-third") "")))
       ;; XXX: Fix build with mupdf-0.23.0+.
       ;; See also: https://github.com/ahrm/sioyek/issues/804
       (patches (search-patches "sioyek-fix-build.patch"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "PREFIX=" #$output))
      #:test-target "check"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* "pdf_viewer/main.cpp"
                (("/usr/share")
                 (string-append #$output "/share"))
                (("/etc")
                 (string-append #$output "/etc")))))
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (apply invoke "qmake" configure-flags)))
          (add-after 'install 'instal-man-page
            (lambda _
              (install-file "resources/sioyek.1"
                            (string-append #$output "/share/man/man1")))))))
    (inputs
     (list freetype
           gumbo-parser
           harfbuzz
           jbig2dec
           libjpeg-turbo
           mujs
           mupdf-1.24
           openjpeg
           qt3d-5
           qtbase-5
           qtwayland-5
           zlib))
    (home-page "https://sioyek.info/")
    (synopsis "PDF viewer with a focus on technical books and research papers")
    (description
     "Sioyek is a PDF viewer with a focus on textbooks and research papers.")
    (license license:gpl3+)))

(define-public pdftk
  (package
    (name "pdftk")
    (version "3.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.com/pdftk-java/pdftk/-/archive/v"
                           version "/pdftk-v" version ".tar.gz"))
       (sha256
        (base32 "11mj0phf78pkbdzvnfhl7n4z476fiv1zjfbf2cx9wlsq8vjpv54w"))))
    (build-system ant-build-system)
    (arguments
     (list
      #:jdk openjdk11
      #:tests? #f  ; no test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'copy-lib-files
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "lib")
              (for-each
               (lambda (lib)
                 (copy-file lib (string-append "lib/" (basename lib))))
               (append
                (find-files (assoc-ref inputs "java-bouncycastle") "\\.jar$")
                (find-files (assoc-ref inputs "java-commons-lang3") "\\.jar$")))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (share (string-append out "/share"))
                     (lib (string-append out "/lib"))
                     (doc (string-append share "/doc/pdftk"))
                     (man1 (string-append share "/man/man1")))
                (mkdir-p bin)
                (mkdir-p lib)
                (mkdir-p doc)
                (mkdir-p man1)
                (install-file "build/jar/pdftk.jar" lib)
                ;; Install dependencies.
                (for-each
                 (lambda (dep-jar)
                   (symlink dep-jar
                            (string-append lib "/" (basename dep-jar))))
                 (append
                  (find-files (assoc-ref inputs "java-bouncycastle") "\\.jar$")
                  (find-files (assoc-ref inputs "java-commons-lang3") "\\.jar$")))
                ;; Create wrapper script.
                (with-output-to-file (string-append bin "/pdftk")
                  (lambda _
                    (format #t
                           "#!~a/bin/bash~@
                            CLASSPATH=~a/lib/pdftk.jar:~a/lib/*~@
                            exec ~a/bin/java -cp $CLASSPATH com.gitlab.pdftk_java.pdftk \"$@\"~%"
                            (assoc-ref inputs "bash")
                            out
                            out
                            (assoc-ref inputs "openjdk"))))
                ;; Make the wrapper executable.
                (chmod (string-append bin "/pdftk") #o755)
                (copy-recursively "doc" doc)
                (install-file "pdftk.1" man1)))))))
    (inputs
     (list bash java-bouncycastle java-commons-lang3 openjdk11))
    (home-page "https://gitlab.com/pdftk-java/pdftk")
    (synopsis "Tool for manipulating PDF documents")
    (description
     "This package provides a tool for doing everyday things with PDF
documents.  It can:
@itemize
@item Merge PDF documents or collate PDF page scans
@item Split PDF pages into a new document
@item Rotate PDF documents or pages
@item Decrypt input as necessary (password required)
@item Encrypt output as desired
@item Fill PDF forms with X/FDF data and/or flatten forms
@item Generate FDF data stencils from PDF forms
@item Apply a background watermark or a foreground stamp
@item Report PDF metrics, bookmarks and metadata
@item Add/Update PDF bookmarks or metadata
@item Attach files to PDF pages or the PDF document
@item Unpack PDF attachments
@item Burst a PDF document into single pages
@item Uncompress and re-compress page streams
@item Repair corrupted PDF (where possible)
@end itemize")
    (license license:gpl2+)))
