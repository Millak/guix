;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2018, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016-2018, 2020, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Thorsten Wilms <t_w_@freenet.de>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages gimp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg))

(define-public poly2tri-c
  (package
    (name "poly2tri-c")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://storage.googleapis.com/"
                       "google-code-archive-source/v2/code.google.com/"
                       "poly2tri-c/source-archive.zip"))
       (file-name
        (string-append name "-" version ".zip"))
       (sha256
        (base32 "17cw0zhbnf2gb59jm26z0wcarqgdwir9jr1fpi3v9lcvyb2s3mqj"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-strict-rules
           (lambda _
             (substitute* "configure.ac"
               (("\\$CFLAGS -Wall -ansi -pedantic")
                "$CFLAGS")
               (("\\$CFLAGS -Werror")
                "$CFLAGS"))
             #t))
         (add-after 'disable-strict-rules 'fix-build-errors
           (lambda _
             (substitute* "poly2tri-c/refine/Makefile.am"
               (("cdt.c")
                "rcdt.c")
               (("cdt.h")
                "rcdt.h")
               (("utils.c")
                "rutils.c")
               (("utils.h")
                "rutils.h"))
             #t))
         (add-before 'bootstrap 'configure-later
           (lambda _
             (setenv "NOCONFIGURE" "set")
             #t))
         (add-after 'build 'generate-doc
           (lambda _
             (invoke "doxygen")
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (copy-recursively
                "doc"
                (string-append doc "/share/doc/poly2tri-c"))
               #t))))))
    (native-inputs
     (list autoconf
           automake
           doxygen
           libtool
           pkg-config
           unzip
           which))
    (propagated-inputs
     (list glib))
    (synopsis "2D constrained Delaunay triangulation library")
    (description "Poly2Tri-C is a library for generating, refining and rendering
2-Dimensional Constrained Delaunay Triangulations.")
    (home-page "https://code.google.com/archive/p/poly2tri-c/")
    (license license:bsd-3)))

(define-public mrg
  (package
    (name "mrg")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/hodefoting/mrg")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "106qhh0c11576cc5kh90ds0ram72d3r6n9sadw0y4krnhap6dvwk"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))   ; To wrap binaries and/or compile schemas
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     `(("alsa" ,alsa-lib)
       ("cairo" ,cairo)
       ("gtk+" ,gtk+)
       ("mmm" ,mmm)
       ("x11" ,libx11)))
    (synopsis "Microraptor GUI")
    (description "MrG is is a C API for creating user interfaces.  It can be
used as an application writing environment or as an interactive canvas for part
of a larger interface.")
    (home-page "https://github.com/hodefoting/mrg")
    (license license:lgpl2.0+)))

(define-public babl
  (package
    (name "babl")
    (version "0.1.112")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://download.gimp.org/pub/babl/"
                                        (version-major+minor version)
                                        "/babl-" version ".tar.xz")
                         (string-append "https://ftp.gtk.org/pub/babl/"
                                        (version-major+minor version)
                                        "/babl-" version ".tar.xz")
                         (string-append "ftp://ftp.gtk.org/pub/babl/"
                                        (version-major+minor version)
                                        "/babl-" version ".tar.xz")))
              (sha256
               (base32
                "0yca08ay7jygj59bc6fi73pcipi1h6sams43rkzci1qp8a16csgv"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Dwith-docs=false")
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (target-aarch64?)
                 #~((add-after 'unpack 'disable-failing-test
                      (lambda _
                        (substitute* "tests/meson.build"
                          ;; float -> u8 1 failed #1[1]  got 76 expected 77
                          (("'float-to-8bit',") "")))))
                 '()))))
    (native-inputs
     (list gobject-introspection pkg-config vala))
    (propagated-inputs
     ;; Propagated to satisfy ‘babl.pc’.
     (list lcms))
    (home-page "https://gegl.org/babl/")
    (synopsis "Image pixel format conversion library")
    (description
     "Babl is a dynamic, any-to-any pixel format translation library.
It allows converting between different methods of storing pixels, known as
@dfn{pixel formats}, that have different bit depths and other data
representations, color models, and component permutations.

A vocabulary to formulate new pixel formats from existing primitives is
provided, as well as a framework to add new color models and data types.")
    (license license:lgpl3+)))

(define-public gegl
  (package
    (name "gegl")
    (version "0.4.56")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://download.gimp.org/pub/gegl/"
                                 (string-take version 3)
                                 "/gegl-" version ".tar.xz")
                  (string-append "https://ftp.gtk.org/pub/gegl/"
                                 (version-major+minor version)
                                 "/gegl-" version ".tar.xz")
                  (string-append "ftp://ftp.gtk.org/pub/gegl/"
                                 (version-major+minor version)
                                 "/gegl-" version ".tar.xz")))
       (sha256
        (base32 "0p6cscnjlpcv123rgkjj5ks43jxkbryiqh23aspcjnlv1ywn8jm0"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'extend-test-time-outs
           (lambda _
             ;; Multiply some poorly-chosen time-outs for busy build machines.
             (substitute* "tests/simple/test-node-exponential.c"
               (("G_TIME_SPAN_SECOND" match)
                (string-append "10 * " match)))
             (substitute* "tests/simple/test-buffer-sharing.c"
               (("g_timeout_add_seconds\\([0-9]+" match)
                (string-append match "0")))
             (substitute* (find-files "tests" "^meson\\.build$")
               (("timeout ?: [0-9]+" match)
                (string-append match "0"))))))))
    ;; These are propagated to satisfy 'gegl-0.4.pc'.
    (propagated-inputs
     (list babl glib json-glib))
    (inputs
     ;; All inputs except libjpeg and libpng are optional.
     (list cairo
           gdk-pixbuf
           gexiv2
           jasper
           libjpeg-turbo
           libnsgif
           libpng
           libraw
           (librsvg-for-system)
           libspiro
           libtiff
           libwebp
           maxflow
           openexr-2
           pango
           poppler
           sdl2))
    (native-inputs
     (list `(,glib "bin")               ; for gtester
           gobject-introspection
           intltool
           pkg-config
           vala))
    (home-page "https://gegl.org")
    (synopsis "Graph based image processing framework")
    (description "GEGL (Generic Graphics Library) provides infrastructure to
do demand based cached non destructive image editing on larger than RAM
buffers.")
    ;; The library itself is licensed under LGPL while the sample commandline
    ;; application and GUI binary gegl is licensed under GPL.
    (license (list license:lgpl3+ license:gpl3+))))

;; gnome-photos does not build against gegl 0.4.46 or newer yet.
;; See also <https://gitlab.gnome.org/GNOME/gnome-photos/-/issues/214>.
(define-public babl-0.1.96
  (package
    (inherit babl)
    (version "0.1.96")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://download.gimp.org/pub/babl/"
                                 (version-major+minor version)
                                 "/babl-" version ".tar.xz")
                  (string-append "https://ftp.gtk.org/pub/babl/"
                                 (version-major+minor version)
                                 "/babl-" version ".tar.xz")
                  (string-append "ftp://ftp.gtk.org/pub/babl/"
                                 (version-major+minor version)
                                 "/babl-" version ".tar.xz")))
       (sha256
        (base32 "1xj5hlmm834lb84rpjlfxbqnm5piswgzhjas4h8z90x9b7j3yrrk"))))))

(define-public gegl-0.4.44
  (package
    (inherit gegl)
    (version "0.4.44")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://download.gimp.org/pub/gegl/"
                                 (string-take version 3)
                                 "/gegl-" version ".tar.xz")
                  (string-append "https://ftp.gtk.org/pub/gegl/"
                                 (version-major+minor version)
                                 "/gegl-" version ".tar.xz")
                  (string-append "ftp://ftp.gtk.org/pub/gegl/"
                                 (version-major+minor version)
                                 "/gegl-" version ".tar.xz")))
       (sha256
        (base32 "09k1sn4h0bakgmq2hgd1iamprngpr81ky3fd9446lh2ycd0xnk0a"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs gegl)
       (replace "babl" babl-0.1.96)))))

(define-public gimp
  (package
    (name "gimp")
    (version "2.10.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.gimp.org/pub/gimp/v"
                           (version-major+minor version)
                           "/gimp-" version ".tar.bz2"))
       (sha256
        (base32 "0vl57w9w31cgz6nbkpqfycsnwi5qym87jw31hvz3320wq7p4ba2h"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                  ; 9 MiB of gtk-doc HTML
    (arguments
     (list
      #:modules `((ice-9 popen)
                  (ice-9 rdelim)
                  ,@%default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-gcc-reference
            ;; Avoid reference to GCC.
            (lambda _
              (let* ((port (open-input-pipe "gcc -v 2>&1 | tail -n 1"))
                     (cc-version (read-line port)))
                (close-pipe port)
                (substitute* "app/gimp-version.c"
                  (("CC_VERSION") (string-append "\"" cc-version "\"")))))))
      #:configure-flags
      #~(list (string-append "--with-html-dir=" #$output "/share/gtk-doc/html")

              ;; Prevent the build system from running 'gtk-update-icon-cache'
              ;; which is not needed during the build because Guix runs it at
              ;; profile creation time.
              "ac_cv_path_GTK_UPDATE_ICON_CACHE=true"

              ;; Disable automatic network request on startup to check for
              ;; version updates.
              "--disable-check-update"

              ;; Only Python 2 is supported; disable it.
              "--disable-python"

              ;; ./configure requests not to annoy upstream with packaging bugs.
              "--with-bug-report-url=https://bugs.gnu.org/guix")))
    (inputs
     (list at-spi2-core
           babl
           gegl
           gexiv2
           glib
           glib-networking
           gtk+-2
           libjpeg-turbo
           libmypaint
           libtiff
           libwebp
           mypaint-brushes-1.3
           libexif                      ;optional, EXIF + XMP support
           ghostscript                  ;optional, EPS + PS support
           lcms                         ;optional, color management
           libheif                      ;optional, HEIF + AVIF support
           libmng                       ;optional, MNG support
           libxpm                       ;optional, XPM support
           (librsvg-for-system)         ;optional, SVG support
           libxcursor                   ;optional, Mouse Cursor support
           openexr-2                    ;optional, EXR support
           openjpeg                     ;optional, JPEG 2000 support
           poppler                      ;optional, PDF support
           poppler-data))               ;optional, PDF support
    (native-inputs
     (list desktop-file-utils
           `(,glib "bin")        ;for glib-compile-resources and gdbus-codegen
           intltool
           pkg-config))
    (home-page "https://www.gimp.org")
    (synopsis "GNU Image Manipulation Program")
    (description
     "GIMP is an application for image manipulation tasks such as photo
retouching, composition and authoring.  It supports all common image formats
as well as specialized ones.  It features a highly customizable interface
that is extensible via a plugin system.")
    (license license:gpl3+))) ; some files are lgplv3

(define-public gimp-next
  (package
    (inherit gimp)
    (name "gimp-next")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.gimp.org/pub/gimp/v"
                           (version-major+minor version)
                           "/gimp-" version ".tar.xz"))
       (sha256
        (base32 "081d3n88fcly67qmp0zf4a1b5r5vdj4gnj9cwp0cmn0vklywmwck"))))
    (build-system meson-build-system)
    (arguments
     (list #:modules `((ice-9 popen)
                       (ice-9 rdelim)
                       (guix build meson-build-system)
                       (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-gcc-reference
                 ;; Avoid reference to GCC.
                 (lambda _
                   (let* ((port (open-input-pipe "gcc -v 2>&1 | tail -n 1"))
                          (cc-version (read-line port)))
                     (close-pipe port)
                     (substitute* "app/gimp-version.c"
                       (("CC_VERSION") (string-append "\"" cc-version "\""))))))
               (add-after 'install 'move-doc
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out"))
                         (doc (assoc-ref outputs "doc")))
                     (mkdir-p (string-append doc "/share"))
                     (rename-file (string-append out "/share/doc")
                                  (string-append doc "/share/doc"))))))))
    (inputs (modify-inputs (package-inputs gimp)
              (replace "gtk+" gtk+)
              (prepend libxmu libxt)
              (prepend python python-pygobject gjs)
              (prepend libxslt)))
    (native-inputs (modify-inputs (package-native-inputs gimp)
                     (prepend appstream-glib
                              gi-docgen
                              libarchive)))))

(define-public gimp-fourier
  (package
    (name "gimp-fourier")
    (version "0.4.3-2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://registry.gimp.org/files/fourier-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rpacyad678lqgxa3hh2n0zpg4azs8dpa8q079bqsl12812k9184"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: The gegl package only installs "gegl-0.4.pc", but
         ;; "gimp-2.0.pc" requires "gegl-0.3.pc", so we just copy it.
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "tmppkgconfig")
             (copy-file (search-input-file inputs
                                           "/lib/pkgconfig/gegl-0.4.pc")
                        "tmppkgconfig/gegl-0.3.pc")
             (setenv "PKG_CONFIG_PATH"
                     (string-append "tmppkgconfig:"
                                    (or (getenv "PKG_CONFIG_PATH") "")))
             #t))
         (add-after 'unpack 'set-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             ;; gimptool-2.0 does not allow us to install to any target
             ;; directory.
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/lib/gimp/"
                                          (car (string-split ,(package-version gimp) #\.))
                                          ".0/plug-ins")))
               (substitute* "Makefile"
                 (("\\$\\(PLUGIN_INSTALL\\) fourier")
                  (string-append "cp fourier " target)))
               (mkdir-p target))
             #t)))))
    (inputs
     (list fftw
           gimp
           ;; needed by gimp-2.0.pc
           gdk-pixbuf
           gegl
           cairo
           glib
           ;; needed by gimpui-2.0.pc
           gtk+-2))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.lprp.fr/gimp_plugin_en/#fourier")
    (synopsis "GIMP plug-in to edit image in fourier space")
    (description
     "This package provides a simple plug-in to apply the fourier transform on
an image, allowing you to work with the transformed image inside GIMP.  You
can draw or apply filters in fourier space and get the modified image with an
inverse fourier transform.")
    (license license:gpl3+)))

(define-public libmypaint
  (package
    (name "libmypaint")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mypaint/libmypaint/"
                                  "releases/download/v" version "/libmypaint-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0priwpmc7dizccqvn21ig6d649bprl3xl1hmjj7nddznjgr585vl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    ;; As needed by 'libmypaint.pc'.
    (propagated-inputs
     (list json-c gobject-introspection))
    (inputs
     (list glib))
    (synopsis "Artistic brushes library")
    (description "Libmypaint, also called \"brushlib\", is a library for making
brushstrokes which is used by MyPaint and GIMP.")
    (home-page "http://mypaint.org")
    (license license:isc)))

(define-public mypaint-brushes
  (package
    (name "mypaint-brushes")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mypaint/mypaint-brushes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kcqz13vzpy24dhmrx9hbs6s7hqb8y305vciznm15h277sabpmw9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (synopsis "Default brushes for MyPaint")
    (description "This package provides the default set of brushes for
MyPaint.")
    (home-page "https://github.com/mypaint/mypaint-brushes/")
    ;; Scripts are distributed under GPL2+ terms, brushes are provided as
    ;; public domain or under CC0 terms.
    (license (list license:gpl2+ license:cc0 license:public-domain))))

(define-public mypaint-brushes-1.3
  (package
    (inherit mypaint-brushes)
    (name "mypaint-brushes")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mypaint/mypaint-brushes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c95l1vfz7sbrdlzrbz7h1p6s1k113kyjfd9wfnxlm0p6562cz3j"))))))

(define-public gimp-resynthesizer
  ;; GIMP does not respect any plugin search path environment variable, so after
  ;; installation users have to edit their GIMP settings to include
  ;; "$HOME/.guix-profile/lib/gimp/2.0/plug-ins/" in
  ;; “Edit->Preferences->Folders->Plug Ins”.
  (package
    (name "gimp-resynthesizer")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/bootchk/resynthesizer")
              (commit (string-append "v" version))))
       (sha256
        (base32
         "1jwc8bhhm21xhrgw56nzbma6fwg59gc8anlmyns7jdiw83y0zx3j"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `( ;; Turn off tests to avoid:
       ;; make[1]: *** No rule to make target '../src/resynth-gui.c', needed by 'resynthesizer.pot'.  Stop.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _
             (setenv "CONFIG_SHELL" (which "sh"))
             #t))
         (add-after 'configure 'set-prefix
           ;; Install plugin under $prefix, not under GIMP's libdir.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/lib/gimp/"
                                          ,(version-major
                                            (package-version gimp))
                                          ".0")))
               (substitute* (list "src/resynthesizer/Makefile"
                                  "src/resynthesizer-gui/Makefile")
                 (("GIMP_LIBDIR = .*")
                  (string-append "GIMP_LIBDIR = " target "\n")))
               (mkdir-p target)
               #t))))))
    (native-inputs
     ;; avoid ./autogen.sh: ./configure: /bin/sh: bad interpreter:
     ;; No such file or directory
     `(("autoconf" ,autoconf-wrapper)
       ("automake" ,automake)
       ("glib" ,glib "bin")                       ; glib-gettextize
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list gimp
           gdk-pixbuf ; needed by gimp-2.0.pc
           cairo
           gegl
           gtk+-2 ; needed by gimpui-2.0.pc
           glib))
    (home-page "https://github.com/bootchk/resynthesizer")
    (synopsis "GIMP plugins for texture synthesis")
    (description
     "This package provides resynthesizer plugins for GIMP, which encompasses
tools for healing selections (content-aware fill), enlarging the canvas and
healing the border, increasing the resolution while adding detail, and
transferring the style of an image.")
    (license license:gpl3+)))
