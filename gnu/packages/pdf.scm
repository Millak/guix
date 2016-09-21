;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Coypright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Coypright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Coypright © 2016 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tls)
  #:use-module (srfi srfi-1))

(define-public poppler
  (package
   (name "poppler")
   (version "0.43.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://poppler.freedesktop.org/poppler-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0mi4zf0pz3x3fx3ir7szz1n57nywgbpd4mp2r7mvf47f4rmf4867"))))
   (build-system gnu-build-system)
   ;; FIXME:
   ;;  use libcurl:        no
   (inputs `(("fontconfig" ,fontconfig)
             ("freetype" ,freetype)
             ("libjpeg" ,libjpeg)
             ("libpng" ,libpng)
             ("libtiff" ,libtiff)
             ("lcms" ,lcms)
             ("openjpeg-1" ,openjpeg-1)
             ("zlib" ,zlib)

             ;; To build poppler-glib (as needed by Evince), we need Cairo and
             ;; GLib.  But of course, that Cairo must not depend on Poppler.
             ("cairo" ,(package (inherit cairo)
                         (inputs (alist-delete "poppler"
                                               (package-inputs cairo)))))
             ("glib" ,glib)))
   (native-inputs
      `(("pkg-config" ,pkg-config)
        ("glib" ,glib "bin")                      ; glib-mkenums, etc.
        ("gobject-introspection" ,gobject-introspection)))
   (arguments
    `(#:tests? #f ; no test data provided with the tarball
      #:configure-flags
      '("--enable-xpdf-headers" ; to install header files
        "--enable-zlib"

        ;; Saves 8 MiB of .a files.
        "--disable-static")
      #:phases
      (alist-cons-before
       'configure 'setenv
       (lambda _
         (setenv "CPATH"
                 (string-append (assoc-ref %build-inputs "openjpeg-1")
                                "/include/openjpeg-1.5"
                                ":" (or (getenv "CPATH") ""))))
        %standard-phases)))
   (synopsis "PDF rendering library")
   (description
    "Poppler is a PDF rendering library based on the xpdf-3.0 code base.")
   (license license:gpl2+)
   (home-page "http://poppler.freedesktop.org/")))

(define-public poppler-qt4
  (package (inherit poppler)
   (name "poppler-qt4")
   (inputs `(("qt-4" ,qt-4)
             ,@(package-inputs poppler)))
   (synopsis "Qt4 frontend for the Poppler PDF rendering library")))

(define-public poppler-qt5
  (package (inherit poppler)
   (name "poppler-qt5")
   (inputs `(("qtbase" ,qtbase)
             ,@(package-inputs poppler)))
   (arguments
    (substitute-keyword-arguments (package-arguments poppler)
     ((#:configure-flags flags)
       `(cons "CXXFLAGS=-std=gnu++11" ,flags))))
   (synopsis "Qt5 frontend for the Poppler PDF rendering library")))

(define-public python-poppler-qt4
  (package
    (name "python-poppler-qt4")
    (version "0.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://pypi.python.org/packages/source/p"
                            "/python-poppler-qt4/python-poppler-qt4-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0x63niylkk4q3h3ay8zrk3m1xiik0x3hlr4gvj7kswx48qi1vb99"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-poppler-include-paths
          (lambda _
            (substitute* (find-files "." "poppler-.*\\.sip")
              (("qt4/poppler-.*\\.h" header)
               (string-append "poppler/" header)))
            #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python-sip" ,python-sip)
       ("python-pyqt-4" ,python-pyqt-4)
       ("poppler-qt4" ,poppler-qt4)))
    (home-page "https://pypi.python.org/pypi/python-poppler-qt4")
    (synopsis "Python bindings for Poppler-Qt4")
    (description
     "This package provides Python bindings for the Qt4 interface of the
Poppler PDF rendering library.")
    (license license:lgpl2.1+)))

(define-public libharu
  (package
   (name "libharu")
   (version "2.3.0")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/libharu/libharu/archive/"
                                 "RELEASE_"
                                 (string-join (string-split version #\.) "_")
                                 ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1lm4v539y9cb1lvbq387j57sy7yxda3yv8b1pk8m6zazbp66i7lg"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (list (string-append "--with-zlib="
                           (assoc-ref %build-inputs "zlib"))
            (string-append "--with-png="
                           (assoc-ref %build-inputs "libpng")))
      #:phases
      (modify-phases %standard-phases
        (add-before 'configure 'autogen
          (lambda _ (zero? (system* "autoreconf" "-vif")))))))
   (inputs
    `(("zlib" ,zlib)
      ("libpng" ,libpng)))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("libtool" ,libtool)))
   (home-page "http://libharu.org/")
   (synopsis "Library for generating PDF files")
   (description
    "libHaru is a library for generating PDF files.  libHaru does not support
reading and editing of existing PDF files.")
   (license license:zlib)))

(define-public xpdf
  (package
   (name "xpdf")
   (version "3.04")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.foolabs.com/pub/xpdf/xpdf-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1rbp54mr3z2x3a3a1qmz8byzygzi223vckfam9ib5g1sfds0qf8i"))))
   (build-system gnu-build-system)
   (inputs `(("freetype" ,freetype)
             ("gs-fonts" ,gs-fonts)
             ("lesstif" ,lesstif)
             ("libpaper" ,libpaper)
             ("libx11" ,libx11)
             ("libxext" ,libxext)
             ("libxp" ,libxp)
             ("libxpm" ,libxpm)
             ("libxt" ,libxt)
             ("libpng" ,libpng)
             ("zlib" ,zlib)))
   (arguments
    `(#:tests? #f ; there is no check target
      #:parallel-build? #f ; build fails randomly on 8-way machines
      #:configure-flags
        (list (string-append "--with-freetype2-includes="
                             (assoc-ref %build-inputs "freetype")
                             "/include/freetype2"))
      #:phases
       (alist-replace
        'install
        (lambda* (#:key outputs inputs #:allow-other-keys #:rest args)
         (let* ((install (assoc-ref %standard-phases 'install))
                (out (assoc-ref outputs "out"))
                (xpdfrc (string-append out "/etc/xpdfrc"))
                (gs-fonts (assoc-ref inputs "gs-fonts")))
               (apply install args)
               (substitute* xpdfrc
                (("/usr/local/share/ghostscript/fonts")
                 (string-append gs-fonts "/share/fonts/type1/ghostscript"))
                (("#fontFile") "fontFile"))))
        %standard-phases)))
   (synopsis "Viewer for PDF files based on the Motif toolkit")
   (description
    "Xpdf is a viewer for Portable Document Format (PDF) files.")
   (license license:gpl3) ; or gpl2, but not gpl2+
   (home-page "http://www.foolabs.com/xpdf/")))

(define-public zathura-cb
  (package
    (name "zathura-cb")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-cb/download/zathura-cb-"
                              version ".tar.gz"))
              (sha256
               (base32
                "1zbazysdjwwnzw01qlnzyixwmsi8rqskc76mp81qcr3rpl96jprp"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("girara" ,girara)))
    (inputs `(("libarchive" ,libarchive)
              ("gtk+" ,gtk+)
              ("zathura" ,zathura)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "PLUGINDIR=" %output "/lib/zathura")
                          "CC=gcc")
       #:tests? #f ; Package does not contain tests.
       #:phases
       (alist-delete 'configure %standard-phases)))
    (home-page "https://pwmt.org/projects/zathura-cb/")
    (synopsis "Comic book support for zathura (libarchive backend)")
    (description "The zathura-cb plugin adds comic book support to zathura
using libarchive.")
    (license license:zlib)))

(define-public zathura-ps
  (package
    (name "zathura-ps")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-ps/download/zathura-ps-"
                              version ".tar.gz"))
              (sha256
               (base32
                "18wsfy8pqficdgj8wy2aws7j4fy8z78157rhqk17mj5f295zgvm9"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("girara" ,girara)))
    (inputs `(("libspectre" ,libspectre)
              ("gtk+" ,gtk+)
              ("zathura" ,zathura)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "PLUGINDIR=" %output "/lib/zathura")
                          "CC=gcc")
       #:tests? #f ; Package does not contain tests.
       #:phases
       (alist-delete 'configure %standard-phases)))
    (home-page "https://pwmt.org/projects/zathura-ps/")
    (synopsis "PS support for zathura (libspectre backend)")
    (description "The zathura-ps plugin adds PS support to zathura
using libspectre.")
    (license license:zlib)))

(define-public zathura-djvu
  (package
    (name "zathura-djvu")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-djvu/download/zathura-djvu-"
                              version ".tar.gz"))
              (sha256
               (base32
                "03cw54d2fipvbrnbqy0xccqkx6s77dyhyymx479aj5ryy4513dq8"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("girara" ,girara)))
    (inputs
     `(("djvulibre" ,djvulibre)
       ("gtk+" ,gtk+)
       ("zathura" ,zathura)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "PLUGINDIR=" %output "/lib/zathura")
                          "CC=gcc")
       #:tests? #f ; Package does not contain tests.
       #:phases
       (alist-delete 'configure %standard-phases)))
    (home-page "https://pwmt.org/projects/zathura-djvu/")
    (synopsis "DjVu support for zathura (DjVuLibre backend)")
    (description "The zathura-djvu plugin adds DjVu support to zathura
using the DjVuLibre library.")
    (license license:zlib)))

(define-public zathura-pdf-poppler
  (package
    (name "zathura-pdf-poppler")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-pdf-poppler/download/zathura-pdf-poppler-"
                              version ".tar.gz"))
              (sha256
               (base32
                "1maqiv7yv8d8hymlffa688c5z71v85kbzmx2j88i8z349xx0rsyi"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("girara" ,girara)))
    (inputs
     `(("poppler" ,poppler)
       ("gtk+" ,gtk+)
       ("zathura" ,zathura)
       ("cairo" ,cairo)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "PLUGINDIR=" %output "/lib/zathura")
                          "CC=gcc")
       #:tests? #f ; Package does not include tests.
       #:phases
       (alist-delete 'configure %standard-phases)))
    (home-page "https://pwmt.org/projects/zathura-pdf-poppler/")
    (synopsis "PDF support for zathura (poppler backend)")
    (description "The zathura-pdf-poppler plugin adds PDF support to zathura
by using the poppler rendering engine.")
    (license license:zlib)))

(define-public zathura
  (package
    (name "zathura")
    (version "0.3.6")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura/download/zathura-"
                              version ".tar.gz"))
              (sha256
               (base32
                "0fyb5hak0knqvg90rmdavwcmilhnrwgg1s5ykx9wd3skbpi8nsh8"))
              (patches (search-patches
                        "zathura-plugindir-environment-variable.patch"))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gettext" ,gnu-gettext)))
    (inputs `(("girara" ,girara)
              ("sqlite" ,sqlite)
              ("gtk+" ,gtk+)))
    (native-search-paths
     (list (search-path-specification
            (variable "ZATHURA_PLUGIN_PATH")
            (files '("lib/zathura")))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       `(,(string-append "PREFIX=" (assoc-ref %outputs "out"))
         "CC=gcc" "COLOR=0")
       #:tests? #f ; Tests fail: "Gtk cannot open display".
       #:test-target "test"
       #:phases
       (alist-delete 'configure %standard-phases)))
    (home-page "https://pwmt.org/projects/zathura/")
    (synopsis "Lightweight keyboard-driven PDF viewer")
    (description "Zathura is a customizable document viewer.  It provides a
minimalistic interface and an interface that mainly focuses on keyboard
interaction.")
    (license license:zlib)))

(define-public podofo
  (package
    (name "podofo")
    (version "0.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/podofo/podofo/" version
                                  "/podofo-" version ".tar.gz"))
              (sha256
               (base32
                "1n12lbq9x15vqn7dc0hsccp56l5jdff1xrhvlfqlbklxx0qiw9pc"))))
    (build-system cmake-build-system)
    (inputs                                      ; TODO: Add cppunit for tests
     `(("lua" ,lua-5.1)
       ("libpng" ,libpng)
       ("openssl" ,openssl)
       ("fontconfig" ,fontconfig)
       ("libtiff" ,libtiff)
       ("libjpeg" ,libjpeg-8)
       ("freetype" ,freetype)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags '("-DPODOFO_BUILD_SHARED=ON"
                           "-DPODOFO_BUILD_STATIC=ON")
       #:phases
         (alist-cons-before
         'configure 'patch
         (lambda* (#:key inputs #:allow-other-keys)
           (let ((freetype (assoc-ref inputs "freetype")))
             ;; Look for freetype include files in the correct place.
             (substitute* "cmake/modules/FindFREETYPE.cmake"
               (("/usr/local") freetype))))
         %standard-phases)))
    (home-page "http://podofo.sourceforge.net")
    (synopsis "Tools to work with the PDF file format")
    (description
     "PoDoFo is a C++ library and set of command-line tools to work with the
PDF file format.  It can parse PDF files and load them into memory, and makes
it easy to modify them and write the changes to disk.  It is primarily useful
for applications that wish to do lower level manipulation of PDF, such as
extracting content or merging files.")
    (license license:lgpl2.0+)))

(define-public mupdf
  (package
    (name "mupdf")
    (version "1.9a")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://mupdf.com/downloads/archive/"
                            name "-" version "-source.tar.gz"))
        (sha256
         (base32
          "1k64pdapyj8a336jw3j61fhn0rp4q6az7d0dqp9r5n3d9rgwa5c0"))
        (patches (search-patches "mupdf-build-with-openjpeg-2.1.patch"
                                 "mupdf-CVE-2016-6265.patch"
                                 "mupdf-CVE-2016-6525.patch"))
        (modules '((guix build utils)))
        (snippet
            ;; Delete all the bundled libraries except for mujs, which is
            ;; developed by the same team as mupdf and has no releases.
            ;; TODO Package mujs and don't use the bundled copy.
            '(for-each delete-file-recursively
                       '("thirdparty/curl"
                         "thirdparty/freetype"
                         "thirdparty/glfw"
                         "thirdparty/harfbuzz"
                         "thirdparty/jbig2dec"
                         "thirdparty/jpeg"
                         "thirdparty/openjpeg"
                         "thirdparty/zlib")))))
    (build-system gnu-build-system)
    (inputs
      `(("curl" ,curl)
        ("freetype" ,freetype)
        ("harfbuzz" ,harfbuzz)
        ("jbig2dec" ,jbig2dec)
        ("libjpeg" ,libjpeg)
        ("libx11" ,libx11)
        ("libxext" ,libxext)
        ("openjpeg" ,openjpeg)
        ("openssl" ,openssl)
        ("zlib" ,zlib)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
      '(#:tests? #f ; no check target
        #:make-flags (list "CC=gcc"
                           "XCFLAGS=-fpic"
                           (string-append "prefix=" (assoc-ref %outputs "out")))
        #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "http://mupdf.com")
    (synopsis "Lightweight PDF viewer and toolkit")
    (description
      "MuPDF is a C library that implements a PDF and XPS parsing and
rendering engine.  It is used primarily to render pages into bitmaps,
but also provides support for other operations such as searching and
listing the table of contents and hyperlinks.

The library ships with a rudimentary X11 viewer, and a set of command
line tools for batch rendering (pdfdraw), rewriting files (pdfclean),
and examining the file structure (pdfshow).")
    (license license:agpl3+)))

(define-public qpdf
  (package
   (name "qpdf")
   (version "5.1.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/qpdf/qpdf/" version
                                "/qpdf-" version ".tar.gz"))
            (sha256 (base32
                     "1lq1v7xghvl6p4hgrwbps3a13ad6lh4ib3myimb83hxgsgd4n5nm"))
            (modules '((guix build utils)))
            (snippet
             ;; Replace shebang with the bi-lingual shell/Perl trick to remove
             ;; dependency on Perl.
             '(substitute* "qpdf/fix-qdf"
                (("#!/usr/bin/env perl")
                 "\
eval '(exit $?0)' && eval 'exec perl -wS \"$0\" ${1+\"$@\"}'
  & eval 'exec perl -wS \"$0\" $argv:q'
    if 0;\n")))))
   (build-system gnu-build-system)
   (arguments
    `(#:disallowed-references (,perl)
      #:phases (alist-cons-before
                'configure 'patch-paths
                (lambda _
                  (substitute* "make/libtool.mk"
                    (("SHELL=/bin/bash")
                     (string-append "SHELL=" (which "bash"))))
                  (substitute* (append
                                '("qtest/bin/qtest-driver")
                                (find-files "." "\\.test"))
                    (("/usr/bin/env") (which "env"))))
                %standard-phases)))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("perl" ,perl)))
   (propagated-inputs
    `(("pcre" ,pcre)))
   (inputs
    `(("zlib" ,zlib)))
   (synopsis "Command-line tools and library for transforming PDF files")
   (description
    "QPDF is a command-line program that does structural, content-preserving
transformations on PDF files.  It could have been called something like
pdf-to-pdf.  It includes support for merging and splitting PDFs and to
manipulate the list of pages in a PDF file.  It is not a PDF viewer or a
program capable of converting PDF into other formats.")
   (license license:clarified-artistic)
   (home-page "http://qpdf.sourceforge.net/")))

(define-public xournal
  (package
    (name "xournal")
    (version "0.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/xournal/xournal/" version
                           "/xournal-" version ".tar.gz"))
       (sha256
        (base32
         "0c7gjcqhygiyp0ypaipdaxgkbivg6q45vhsj8v5jsi9nh6iqff13"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk" ,gtk+-2)
       ("pango" ,pango)
       ("poppler" ,poppler)
       ("glib" ,glib)
       ("libgnomecanvas" ,libgnomecanvas)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://xournal.sourceforge.net/")
    (synopsis "Notetaking using a stylus")
    (description
     "Xournal is an application for notetaking, sketching, keeping a journal
using a stylus.")
    (license license:gpl2+)))

(define-public python-reportlab
  (package
    (name "python-reportlab")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "reportlab" version))
              (sha256
               (base32
                "0rz2pg04wnzjjm2f5a8ik9v8s54mv4xrjhv5liqjijqv6awh12gl"))))
    (build-system python-build-system)
    (arguments
     ;; Prevent creation of the egg. Without this flag, various artifacts
     ;; from the build inputs end up in the final python3 output. It also
     ;; works around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20765 .
     `(#:configure-flags '("--single-version-externally-managed" "--root=/")))
    (propagated-inputs
     `(("python-pillow" ,python-pillow)))
    (home-page "http://www.reportlab.com")
    (synopsis "Python library for generating PDFs and graphics")
    (description "This is the ReportLab PDF Toolkit.  It allows rapid creation
of rich PDF documents, and also creation of charts in a variety of bitmap and
vector formats.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-reportlab))))))

(define-public python2-reportlab
  (package
    (inherit (package-with-python2
              (strip-python2-variant python-reportlab)))
    (native-inputs `(("python2-pip" ,python2-pip)))))

(define-public impressive
  (package
    (name "impressive")
    (version "0.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/impressive/Impressive/"
                    version "/Impressive-" version ".tar.gz"))
              (sha256
               (base32
                "0b3rmy6acp2vmf5nill3aknxvr9a5aawk1vnphkah61anxp62gsr"))))
    (build-system python-build-system)

    ;; TODO: Add dependency on pdftk.
    (inputs `(("python-pygame" ,python-pygame)
              ("python2-pillow" ,python2-pillow)
              ("sdl" ,sdl)
              ("xpdf" ,xpdf)))

    (arguments
     `(#:python ,python-2
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (delete 'configure)
                  (delete 'check)
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; There's no 'setup.py' so install things manually.
                      (let* ((out  (assoc-ref outputs "out"))
                             (bin  (string-append out "/bin"))
                             (man1 (string-append out "/share/man/man1"))
                             (sdl  (assoc-ref inputs "sdl"))
                             (xpdf (assoc-ref inputs "xpdf")))
                        (mkdir-p bin)
                        (copy-file "impressive.py"
                                   (string-append bin "/impressive"))
                        (wrap-program (string-append bin "/impressive")
                          `("LIBRARY_PATH" ":" prefix ;for ctypes
                            (,(string-append sdl "/lib")))
                          `("PATH" ":" prefix     ;for pdftoppm
                            (,(string-append xpdf "/bin"))))
                        (mkdir-p man1)
                        (install-file "impressive.1" man1)
                        #t))))))
    (home-page "http://impressive.sourceforge.net")
    (synopsis "PDF presentation tool with visual effects")
    (description
     "Impressive is a tool to display PDF files that provides visual effects
such as smooth alpha-blended slide transitions.  It provides additional tools
such as zooming, highlighting an area of the screen, and a tool to navigate
the PDF pages.")
    (license license:gpl2)))
