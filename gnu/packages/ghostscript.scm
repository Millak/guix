;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2013, 2015-2017, 2019, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018, 2019, 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020, 2022 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages ghostscript)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1))

(define-public lcms
  (package
    (name "lcms")
    (version "2.13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lcms/lcms/"
                                  (version-major+minor version)
                                  "/lcms2-" version ".tar.gz"))
              (sha256
               (base32
                "121v414bg2zk0fcwx0kigr2l6nxl88nmblfn3gq5lz5jwybffwyl"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (inputs
     (list libjpeg-turbo libtiff zlib))
    (synopsis "Little CMS, a small-footprint colour management engine")
    (description
     "Little CMS is a small-footprint colour management engine, with special
focus on accuracy and performance.  It uses the International Color
Consortium standard (ICC), approved as ISO 15076-1.")
    (license license:x11)
    (home-page "https://www.littlecms.com/")
    (properties '((cpe-name . "little_cms_color_engine")))))

(define-public libpaper
  (package
    (name "libpaper")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rrthomas/libpaper/releases"
                                  "/download/v" version "/libpaper-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0aipyaqp30cn919j7w5wvlgkw0v4aqsax82i2zw4wmgck8g6ax77"))))
    (build-system gnu-build-system)
    (native-inputs
     (list help2man))
    (arguments
     (list #:configure-flags ''("--disable-static"
                                 ;; Tests require a relocatable build.
                                 "--enable-relocatable")
           ;; --enable-relocate is broken on the Hurd
           #:tests? (not (or (target-hurd?)
                             (%current-target-system)))))
    (outputs '("out" "debug"))
    (home-page "https://github.com/rrthomas/libpaper")
    (synopsis "Library for handling paper sizes")
    (description
     "The paper library and accompanying files are intended to provide a simple
way for applications to take actions based on a system- or user-specified
paper size.")
    ;; The library is LGPL3+, everything else GPL3+.
    (license (list license:lgpl3+ license:gpl3+))))

(define-public psutils
  (package
    (name "psutils")
    (version "2.09")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rrthomas/psutils/releases"
                                  "/download/v" version "/psutils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1nmp0hb7c4a315vv1mqw2cbckvca8bzh1cv3gdvwwy24w9qba6p3"))))
    (build-system gnu-build-system)
    (inputs (list perl perl-ipc-run3))
    (native-inputs
     (list libpaper))
    (arguments
     (list
      #:tests? #f               ; FIXME: requires files not present in tarball
      #:configure-flags
      ;; Help the build system locate Perl when cross-compiling.
      (if (%current-target-system)
          #~(list (string-append "ac_cv_path_PERL="
                                 (search-input-file %build-inputs "bin/perl")))
          #~'())
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-scripts
            (lambda _
              (let ((perl5lib (getenv "PERL5LIB")))
                (for-each
                 (lambda (file)
                   (wrap-program file
                     `("PERL5LIB" ":" prefix
                       (,(string-append perl5lib ":" #$output
                                        "/lib/perl5/site_perl")))))
                 (find-files (string-append #$output "/bin") "."))))))))
    (synopsis "Collection of utilities for manipulating PostScript documents")
    (description
     "PSUtils is a collection of utilities for manipulating PostScript
documents.  Programs included are psnup, for placing out several logical pages
on a single sheet of paper, psselect, for selecting pages from a document,
pstops, for general imposition, psbook, for signature generation for booklet
printing, and psresize, for adjusting page sizes.")
    (home-page "https://github.com/rrthomas/psutils")
    (license (list license:gpl3+
                   ;; This file carries the "historical" psutils license (v1),
                   ;; which is "effectively BSD 3-clause" (a quote from the file).
                   (license:non-copyleft
                    "file://extractres.in.in"
                    "See extractres.in.in in the distribution.")))))

(define-public ghostscript
  (package
    (name "ghostscript")
    (version "9.56.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ArtifexSoftware/"
                           "ghostpdl-downloads/releases/download/gs"
                           (string-delete #\. version)
                           "/ghostscript-" version ".tar.xz"))
       (sha256
        (base32
         "1r5qash65m6ignki6z72q4rlai9ka99xrxnmqd19n02has00cd6l"))
       (patches (search-patches "ghostscript-no-header-creationdate.patch"
                                "ghostscript-no-header-id.patch"
                                "ghostscript-no-header-uuid.patch"
                                "ghostscript-CVE-2023-36664.patch"
                                "ghostscript-CVE-2023-36664-fixup.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries. The bundled OpenJPEG is a patched fork so
        ;; we leave it, at least for now.
        ;; TODO Try unbundling ijs, which is developed alongside Ghostscript.
        ;; Likewise for the thread-safe lcms2 fork called "lcms2art".
        '(begin
           (for-each delete-file-recursively '("freetype" "jbig2dec" "jpeg"
                                               "libpng" "tiff" "zlib"))))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))            ;19 MiB of HTML/PS doc + examples
    (arguments
     (list
      #:disallowed-references '("doc")
      #:configure-flags
      #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
              "--with-system-libtiff"
              "LIBS=-lz"
              (string-append "ZLIBDIR="
                             (dirname (search-input-file %build-inputs
                                                         "include/zlib.h")))
              "--enable-dynamic"
              "--disable-compile-inits"
              (string-append "--with-fontpath="
                             (search-input-directory
                              %build-inputs
                              "share/fonts/type1/ghostscript"))

              #$@(if (%current-target-system)
                     '(;; Specify the native compiler, which is used to build 'echogs'
                       ;; and other intermediary tools when cross-compiling; see
                       ;; <https://ghostscript.com/FAQ.html>.
                       "CCAUX=gcc"

                       ;; Save 'config.log' etc. of the native build under
                       ;; auxtmp/, useful for debugging.
                       "--enable-save_confaux")
                     '()))
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (target-hurd?)
                 #~((add-after 'unpack 'patch-leptonica
                      (lambda _
                        (let ((patch-file
                               #$(local-file
                                  (search-patch
                                   "ghostscript-leptonica-hurd.patch"))))
                          (with-directory-excursion "leptonica"
                            (invoke "patch" "--force" "-p1" "-i" patch-file))))))
                 #~())
          (add-before 'configure 'create-output-directory
            (lambda _
              ;; The configure script refuses to function if the directory
              ;; specified as -rpath does not already exist.
              (mkdir-p (string-append #$output "/lib"))))
          (add-after 'configure 'remove-doc-reference
            (lambda _
              ;; Don't retain a reference to the 'doc' output in 'gs'.
              ;; The only use of this definition is in the output of
              ;; 'gs --help', so this change is fine.
              (substitute* "base/gscdef.c"
                (("GS_DOCDIR")
                 "\"~/.guix-profile/share/doc/ghostscript\""))))
          (add-after 'configure 'patch-config-files
            (lambda _
              (substitute* "base/unixhead.mak"
                (("/bin/sh") (which "sh")))))
          #$@(if (%current-target-system)
                 '((add-after 'configure 'add-native-lz
                     (lambda _
                       ;; Add missing '-lz' for native tools such as 'mkromfs'.
                       (substitute* "Makefile"
                         (("^AUXEXTRALIBS=(.*)$" _ value)
                          (string-append "AUXEXTRALIBS = -lz " value "\n"))))))
                 '())
          (replace 'build
            (lambda _
              ;; Build 'libgs.so', but don't build the statically-linked 'gs'
              ;; binary (saves 22 MiB).
              (invoke "make" "so" "-j"
                      (number->string (parallel-job-count)))))
          (replace 'install
            (lambda _
              (invoke "make" "soinstall")))
          (add-after 'install 'create-gs-symlink
            (lambda _
              ;; Some programs depend on having a 'gs' binary available.
              (symlink "gsc" (string-append #$output "/bin/gs")))))))
    (native-inputs
     (append
      (list perl
            pkg-config                  ;needed for freetype
            python-minimal-wrapper
            tcl)
      ;; When cross-compiling, some of the natively-built tools require all
      ;; these libraries.
      (if (%current-target-system)
          (list zlib libjpeg-turbo)
          '())))
    (inputs
     (list fontconfig
           freetype
           font-ghostscript
           jbig2dec
           libjpeg-turbo
           libpaper
           libpng
           libtiff
           zlib))
    (synopsis "PostScript and PDF interpreter")
    (description
     "Ghostscript is an interpreter for the PostScript language and the PDF
file format.  It also includes a C library that implements the graphics
capabilities of the PostScript language.  It supports a wide variety of
output file formats and printers.")
    (home-page "https://www.ghostscript.com/")
    (license license:agpl3+)))

(define-public ghostscript/x
  (package/inherit ghostscript
    (name (string-append (package-name ghostscript) "-with-x"))
    (inputs (modify-inputs (package-inputs ghostscript)
              (prepend libxext libxt)))))

(define-public ghostscript/cups
  (package/inherit ghostscript
    (name "ghostscript-with-cups")
    (inputs (modify-inputs (package-inputs ghostscript)
              (prepend cups-minimal)))))

(define-public ijs
  (package
   (name "ijs")
   (version (package-version ghostscript))
   (source (package-source ghostscript))
   (build-system gnu-build-system)
   (native-inputs
    (append (if (target-riscv64?)
              (list config)
              '())
            (list libtool automake autoconf)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'autogen
          (lambda _
            ;; need to regenerate macros
            (system* "autoreconf" "-if")
            ;; do not run configure
            (substitute* "autogen.sh"
              (("^.*\\$srcdir/configure.*") ""))
            (system* "bash" "autogen.sh")
            ;; create configure script in ./ijs/
            (chdir "ijs")
            ;; do not run configure
            (substitute* "autogen.sh"
              (("^.*\\$srcdir/configure.*") "")
              (("^ + && echo Now type.*$")  ""))
            (invoke "bash" "autogen.sh")))
        ,@(if (target-riscv64?)
            `((add-after 'unpack 'update-config-scripts
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  (for-each (lambda (file)
                              (install-file
                               (search-input-file
                                (or native-inputs inputs)
                                (string-append "/bin/" file)) "ijs"))
                            '("config.guess" "config.sub")))))
            '()))))
   (synopsis "IJS driver framework for inkjet and other raster devices")
   (description
    "IJS is a protocol for transmission of raster page images.  This package
provides the reference implementation of the raster printer driver
architecture.")
   (license license:expat)
   (home-page (package-home-page ghostscript))))

(define-public font-ghostscript
  (package
   (name "font-ghostscript")
   (version "8.11")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/gs-fonts/gs-fonts/"
                                version
                                "%20%28base%2035%2C%20GPL%29/ghostscript-fonts-std-"
                                version
                                ".tar.gz"))
            (sha256 (base32
                     "00f4l10xd826kak51wsmaz69szzm2wp8a41jasr4jblz25bg7dhf"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; nothing to check, just files to copy

      #:modules ((guix build gnu-build-system)
                 (guix build utils)
                 (srfi srfi-1))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (dir (string-append out "/share/fonts/type1/ghostscript")))
              (mkdir-p dir)
              (for-each
                (lambda (file)
                  (copy-file file (string-append dir "/" file)))
                (find-files "." "pfb|afm"))
              #t))))))
   (synopsis "Free replacements for the PostScript fonts")
   (description
    "Ghostscript fonts provides fonts and font metrics customarily distributed with
Ghostscript.  It currently includes the 35 standard PostScript fonts.")
   (license license:gpl2)
   (home-page "https://sourceforge.net/projects/gs-fonts/")))

(define-public gs-fonts
  (deprecated-package "gs-fonts" font-ghostscript))

(define-public libspectre
  (package
   (name "libspectre")
   (version "0.2.10")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://libspectre.freedesktop.org/releases"
                                "/libspectre-" version ".tar.gz"))
            (sha256
             (base32
              "01sdaakrv5js8r6gj2r1ankyl304161z060f25mrmz3b1ylb4q6g"))))
   (build-system gnu-build-system)
   (inputs (list ghostscript))
   (native-inputs (list pkg-config))
   (synopsis "Postscript rendering library")
   (description
    "libspectre is a small library for rendering Postscript documents.
It provides a convenient easy to use API for handling and rendering
Postscript documents.")
   (license license:gpl2+)
   (home-page "https://www.freedesktop.org/wiki/Software/libspectre")))
