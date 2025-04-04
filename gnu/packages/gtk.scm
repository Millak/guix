;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2017, 2018, 2019, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2020-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Patrick Hetu <patrick.hetu@auf.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020, 2021, 2022, 2023, 2024, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021 Simon Streit <simon@netpanic.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Benjamin Slade <slade@lambda-y.net>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Sergiu Ivanov <sivanov@colimite.fr>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2025 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2025 Remco van 't Veer <remco@remworks.net>
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

(define-module (gnu packages gtk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix bzr-download)
  #:use-module (guix git-download)
  #:use-module (guix search-paths)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages pulseaudio)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  ;; Export cairo to break a dependency cycle, as gtk-doc is needed to build
  ;; the documentation.  Use cairo for other packages and
  ;; cairo-with-documentation as the public package.
  #:export (cairo))

(define-public appmenu-gtk-module
  (package
    (name "appmenu-gtk-module")
    (version "0.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/vala-panel-project/vala-panel-appmenu")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ywpygjwlbli65203ja2f8wwxh5gbavnfwcxwg25v061pcljaqmm"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-install-gtk-module
            (lambda* _
              (substitute*
                  "subprojects/appmenu-gtk-module/src/gtk-3.0/meson.build"
                (("gtk3.get_pkgconfig_variable\\('libdir'\\)")
                 #$output)))))))
    (native-inputs
     (list `(,glib "bin") vala pkg-config))
    (inputs
     (list gtk+ libwnck))
    (synopsis "Application Menu applet")
    (description
     "This package provides a global menu applet for use with desktop panels
such as mate-panel and xfce4-panel.")
    (home-page "https://gitlab.com/vala-panel-project/vala-panel-appmenu")
    (license (list license:lgpl3))))

(define cairo
  (package
    (name "cairo")
    (version "1.18.2")
    (replacement cairo-1.18.4)
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://cairographics.org/releases/cairo-"
                       version ".tar.xz"))
       (sha256
        (base32 "0nnli5cghygbl9bvlbjls7nspnrrzx1y1pbd7p649s154js9nax6"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f ; see http://lists.gnu.org/archive/html/bug-guix/2013-06/msg00085.html
       #:glib-or-gtk? #t
       #:configure-flags
       ,#~(list "-Dtests=disabled")
       ,@(if (%current-target-system)
             (list
              #:phases
              #~(modify-phases %standard-phases
                  (add-after 'unpack 'fix-cross-compilation
                    (lambda _
                      ;; XXX: Let meson-build-system customize the property
                      (substitute* "meson.build"
                        (("'ipc_rmid_deferred_release', 'auto'")
                         ;; see https://github.com/NixOS/nixpkgs/blob/df51f2293e935e85f6a2e69bcf89a40cb31bbc3d/pkgs/development/libraries/cairo/default.nix#L65
                         ;; XXX: check it on hurd.
                         "'ipc_rmid_deferred_release', 'true'"))))))
             '())))
    (native-inputs
     (append (list pkg-config
                   python-wrapper)
             (if (target-hurd?)
                 '()
                 (list gobject-introspection))))
    (inputs
     (append
      (list bash-minimal                ;for glib-or-gtk-wrap
            ghostscript
            libspectre)
      (if (target-hurd?)
          '()
          (list libdrm
                poppler))))
    (propagated-inputs
     (list fontconfig
           freetype
           glib
           libpng
           pixman
           libx11
           libxcb
           libxext
           libxrender))
    (synopsis "Multi-platform 2D graphics library")
    (description "Cairo is a 2D graphics library with support for multiple output
devices.  Currently supported output targets include the X Window System (via
both Xlib and XCB), Quartz, Win32, image buffers, PostScript, PDF, and SVG file
output.  Experimental backends include OpenGL, BeOS, OS/2, and DirectFB.")
    (home-page "https://cairographics.org/")
    (license
     ;; This project is dual-licensed.
     (list
      license:lgpl2.1+
      license:mpl1.1))
    ;; Hide and have cairo-with-documentation public.
    (properties '((hidden? . #t)))))

;;; TODO: This newer version resolves an issue when writing PDFs.  Remove
;;; after ungrafting cairo.
(define cairo-1.18.4
  (package
    (inherit cairo)
    (version "1.18.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://cairographics.org/releases/cairo-"
                       version ".tar.xz"))
       (sha256
        (base32 "1jrcqfcna0358aqrk7rnys1hwq6k36ilr9r62bg26j3fi8hdhpj4"))))))

(define-public cairo-with-documentation
  ;; cairo's docs must be built in a separate package since it requires
  ;; gtk-doc, which in turn depends on cairo.
  (package/inherit cairo
    (properties (alist-delete 'hidden? (package-properties cairo)))
    (outputs (cons "doc" (package-outputs cairo)))
    (native-inputs
     (modify-inputs (package-native-inputs cairo)
       (prepend gtk-doc)))
    (arguments
     (substitute-keyword-arguments (package-arguments cairo)
       ((#:configure-flags flags ''())
        #~(cons "-Dgtk_doc=true" #$flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'move-doc
              (lambda* (#:key outputs #:allow-other-keys)
                (mkdir-p (string-append #$output:doc "/share"))
                (rename-file
                 (string-append #$output "/share/gtk-doc")
                 (string-append #$output:doc "/share/gtk-doc"))
                ;; This directory is now empty so remove it.
                (rmdir (string-append #$output "/share"))))))))))

(define-public cairo-sans-poppler
  ;; Variant used to break the dependency cycle between Poppler and Cairo.
  (package/inherit cairo
    (inputs (alist-delete "poppler" (package-inputs cairo)))))

(define-public cairo-xcb
  (package/inherit cairo
    (properties (alist-delete 'hidden? (package-properties cairo)))
    (name "cairo-xcb")
    (inputs
     `(("mesa" ,mesa)
       ,@(package-inputs cairo)))
    (arguments
     (substitute-keyword-arguments (package-arguments cairo)
       ((#:configure-flags flags ''())
        #~(cons "-Dxlib-xcb=enabled" #$flags))))
    (synopsis "2D graphics library (with X11 support)")))

(define-public harfbuzz
  (package
    (name "harfbuzz")
    (version "8.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/harfbuzz/harfbuzz"
                                  "/releases/download/" version "/harfbuzz-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0izq2lpqxrf1l755nxrxkkiarywkx5j43asznankxplbxgm0358h"))))
    (build-system meson-build-system)
    (outputs '("out"
               "bin"))                  ;160K, only hb-view depend on cairo
    (inputs
     (list cairo))
    (propagated-inputs
     ;; There are all in the Requires or Requires.private field of '.pc'.
     (list glib graphite2 icu4c))
    (native-inputs
     (append (list `(,glib "bin"))      ;for glib-mkenums
             (if (target-hurd?)
                 '()
                 (list gobject-introspection))
             (list pkg-config
                   python-wrapper
                   which)))
    (arguments
     (list #:configure-flags
           #~(list "-Dgraphite2=enabled")))
    (synopsis "OpenType text shaping engine")
    (description
     "HarfBuzz is an OpenType text shaping engine.")
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))
    (home-page "https://www.freedesktop.org/wiki/Software/HarfBuzz/")))

(define-public libdatrie
  (package
    (name "libdatrie")
    (version "0.2.13")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://linux.thai.net/pub/ThaiLinux/software/"
                       "libthai/libdatrie-" version ".tar.xz"))
       (sha256
        (base32 "1gplcx9ddglpxmqm10qn38kjmvdh4hnhj14rzgqag095psr1n8qj"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-html-docdir=" #$output:doc
                                  "/share/doc/datrie/html"))

           ;; Several tests refer to the 'test.tri' file, leading to race
           ;; conditions when running tests in parallel.
           #:parallel-tests? #f))
    (native-inputs
     (list doxygen pkg-config))
    (synopsis "Double-Array Trie Library")
    (description "Libdatrie is an implementation of double-array structure for
representing trie.  Trie is a kind of digital search tree.")
    (home-page "https://linux.thai.net/~thep/datrie/datrie.html")
    (license license:lgpl2.1+)))

(define-public libthai
  (package
    (name "libthai")
    (version "0.1.29")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://linux.thai.net/pub/thailinux/software/"
                       "libthai/libthai-" version ".tar.xz"))
       (sha256
        (base32 "1kab1w6i1fb76807xrwjfac8n2nky8jbvkhpnh117qahrdywr07w"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        (string-append "--with-html-docdir="
                       (assoc-ref %outputs "doc")
                       "/share/doc/libthai/html"))))
    (native-inputs
     `(("datrie" ,libdatrie)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("datrie" ,libdatrie)))
    (synopsis "Thai language support library")
    (description "LibThai is a set of Thai language support routines aimed to
ease developers’ tasks to incorporate Thai language support in their
applications.")
    (home-page "https://linux.thai.net/projects/libthai")
    (license license:lgpl2.1+)))

(define-public pango
  (package
    (name "pango")
    (version "1.54.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/pango/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "pango-skip-libthai-test.patch"))
              (sha256
               (base32
                "1n0y5l5wfq2a86dimraazvz1v9dvqdjkmpqgzkbk9rqy09syv7la"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t             ; To wrap binaries and/or compile schemas
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-cantarell-tests
                    (lambda _
                      (substitute* "tests/meson.build"
                        ;; XXX FIXME: These tests require "font-abattis-cantarell", but
                        ;; adding it here would introduce a circular dependency.
                        (("\\[ 'test-layout'.*") "")
                        (("\\[ 'test-itemize'.*") "")
                        (("\\[ 'test-font'.*") "")
                        (("\\[ 'test-harfbuzz'.*") "")))))))
    (propagated-inputs
     ;; These are all in Requires or Requires.private of the '.pc' files.
     (list cairo
           fontconfig
           freetype
           fribidi
           glib
           harfbuzz
           libthai
           ;; Some packages, such as Openbox, expect Pango to be built with the
           ;; optional libxft support.
           libxft
           libxrender))
    (inputs
     (list bash-minimal
           zlib))
    (native-inputs
     (append (list `(,glib "bin"))      ;glib-mkenums, etc.
             (if (target-hurd?)
                 '()
                 (list gobject-introspection)) ;g-ir-compiler, etc.
             (list
              help2man
              perl
              pkg-config
              python-wrapper)))
    (synopsis "Text and font handling library")
    (description "Pango is a library for laying out and rendering of text, with
an emphasis on internationalization.  Pango can be used anywhere that text
layout is needed, though most of the work on Pango so far has been done in the
context of the GTK+ widget toolkit.  Pango forms the core of text and font
handling for GTK+-2.x.")
    (home-page "https://pango.gnome.org/")
    (license license:lgpl2.0+)))

(define-public pango-1.42
  (package
   (inherit pango)
   (version "1.42.4")
   (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/pango/"
                                 (version-major+minor version) "/"
                                 "pango-" version ".tar.xz"))
             (sha256
              (base32
               "17bwb7dgbncrfsmchlib03k9n3xaalirb39g3yb43gg8cg6p8aqx"))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases (modify-phases %standard-phases
                 (add-after 'configure 'disable-layout-test
                   (lambda _
                     ;; This test requires that fontconfig uses bitmap fonts
                     ;; such as "font-ghostscript"; however providing such a
                     ;; package alone is not enough, as the requirement comes
                     ;; from deeper in the font stack.  Since this version of
                     ;; Pango is only used for librsvg, simply disable the
                     ;; test.
                     (substitute* "tests/Makefile"
                       (("test-layout\\$\\(EXEEXT\\)") ""))
                     #t)))))))

(define-public pango-1.90
  (package
    (inherit pango)
    (name "pango")
    (version "1.90.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/pango/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "pango-skip-libthai-test.patch"))
              (sha256
               (base32
                "1zqif72jxa819bwi4jv2vgac574qas3w37f7qvn8l31rm1jgjf7i"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (substitute* "pango/pangocairo-font.c"
                     (("cairo_user_font_face_set_render_color_glyph_func")
                      "cairo_user_font_face_set_render_glyph_func"))
                   ;; Disable a failing test
                   (substitute* "tests/testmisc.c"
                     (("\
g_test_add_func \\(\"/layout/gravity-metrics2\", test_gravity_metrics2\\);")
                      ""))))))))

(define-public pangox-compat
  (package
    (name "pangox-compat")
    (version "0.0.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "0ip0ziys6mrqqmz4n71ays0kf5cs1xflj1gfpvs4fgy2nsrr482m"))))
    (build-system gnu-build-system)
    (inputs
     (list glib pango-1.42))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://developer.gnome.org/pango")
    (synopsis "Obsolete pango functions")
    (description  "Pangox was a X backend to pango.  It is now obsolete and no
longer provided by recent pango releases.  pangox-compat provides the
functions which were removed.")
    (license license:lgpl2.0+)))

(define-public ganv
  (package
    (name "ganv")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/ganv-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0pik2d3995z0rjcjhb4hsj5fsph3m8khg6j10k6mx4j2j727aq6l"))))
    (build-system waf-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-flags
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Allow 'bin/ganv_bench' to find libganv-1.so.
             (setenv "LDFLAGS"
                     (string-append "-Wl,-rpath="
                                    (assoc-ref outputs "out") "/lib"))
             #t)))
       #:tests? #f)) ; no check target
    (inputs
     `(("gtk" ,gtk+-2)
       ("gtkmm" ,gtkmm-2)))
    (native-inputs
     (list `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config))
    (home-page "https://drobilla.net/software/ganv/")
    (synopsis "GTK+ widget for interactive graph-like environments")
    (description
     "Ganv is an interactive GTK+ widget for interactive “boxes and lines” or
graph-like environments, e.g. modular synths or finite state machine
diagrams.")
    (license license:gpl3+)))

(define-public gtksourceview-2
  (package
    (name "gtksourceview")
    (version "2.10.5") ; This is the last version which builds against gtk+2
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "07hrabhpl6n8ajz10s0d960jdwndxs87szxyn428mpxi8cvpg1f5"))
              (patches
                (search-patches
                  "gtksourceview-2-add-default-directory.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool
           `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config
           ;; For testing.
           xorg-server-for-tests
           shared-mime-info))
    (propagated-inputs
     ;; As per the pkg-config file.
     `(("gtk" ,gtk+-2)
       ("libxml2" ,libxml2)))
    (arguments
     `(#:phases
       ;; Unfortunately, some of the tests in "make check" are highly dependent
       ;; on the environment therefore, some black magic is required.
       (modify-phases %standard-phases
         (add-before 'check 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server"))
                   (mime (assoc-ref inputs "shared-mime-info")))

               ;; There must be a running X server and make check doesn't start one.
               ;; Therefore we must do it.
               (system (format #f "~a/bin/Xvfb :1 &" xorg-server))
               (setenv "DISPLAY" ":1")

               ;; The .lang files must be found in $XDG_DATA_HOME/gtksourceview-2.0
               (system "ln -s gtksourceview gtksourceview-2.0")
               (setenv "XDG_DATA_HOME" (getcwd))

               ;; Finally, the mimetypes must be available.
               (setenv "XDG_DATA_DIRS" (string-append mime "/share/")))
             #t)))))
    (synopsis "Widget that extends the standard GTK+ 2.x 'GtkTextView' widget")
    (description
     "GtkSourceView is a portable C library that extends the standard GTK+
framework for multiline text editing with support for configurable syntax
highlighting, unlimited undo/redo, search and replace, a completion framework,
printing and other features typical of a source code editor.")
    (license license:lgpl2.0+)
    (home-page "https://developer.gnome.org/gtksourceview/")))

(define-public gtksourceview
  (package
    (name "gtksourceview")
    (version "5.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gtksourceview/"
                                  (version-major+minor version) "/"
                                  "gtksourceview-" version ".tar.xz"))
              (sha256
               (base32
                "07rcnhwqyiqs9icld3965g41wd3n9a808y7agjpasnjwk2njmj44"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-gtk-update-icon-cache
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false"))))
         (add-before 'check 'pre-check
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (let ((Xvfb (search-input-file (or native-inputs inputs)
                                            "bin/Xvfb")))
               ;; Tests require a running X server.
               (system (string-append Xvfb " :1 &"))
               (setenv "DISPLAY" ":1")
               ;; Use an X11 setup to find the display.
               (setenv "GDK_BACKEND" "x11")
               ;; Avoid spawning (and failing to connect to) the accessiblity
               ;; bus.
               (setenv "GTK_A11Y" "none")
               ;; For the missing /etc/machine-id.
               (setenv "DBUS_FATAL_WARNINGS" "0")))))))
    (native-inputs
     (list `(,glib "bin")               ; for glib-genmarshal, etc.
           gettext-minimal
           gi-docgen
           gobject-introspection
           pkg-config
           vala
           ;; For testing.
           xorg-server-for-tests
           shared-mime-info))
    (propagated-inputs
     ;; gtksourceview-5.pc refers to all these.
     (list fontconfig
           fribidi
           glib
           gtk
           libxml2
           pango
           pcre2))
    (home-page "https://wiki.gnome.org/Projects/GtkSourceView")
    (synopsis "GNOME source code widget")
    (description "GtkSourceView is a text widget that extends the standard
GTK+ text widget GtkTextView.  It improves GtkTextView by implementing syntax
highlighting and other features typical of a source code editor.")
    (license license:lgpl2.1+)))

;;; This older version is used by tepl.
(define-public gtksourceview-4
  (package
    (inherit gtksourceview)
    (version "4.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gtksourceview/"
                                  (version-major+minor version) "/"
                                  "gtksourceview-" version ".tar.xz"))
              (sha256
               (base32
                "0zd84229bddvp8zpnn86q34i16mhf9x3pzry795gilc3na7x3jby"))))
    (native-inputs
     (modify-inputs (package-native-inputs gtksourceview)
       (replace "gobject-introspection" gobject-introspection)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs gtksourceview)
       (replace "gtk" gtk+)
       (replace "glib" glib)))))

(define-public gtksourceview-3
  (package
    (inherit gtksourceview-4)
    (name "gtksourceview")
    (version "3.24.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zbpj283b5ycz767hqz5kdq02wzsga65pp4fykvhg8xj6x50f6v9"))))
    (build-system gnu-build-system)
    (arguments (substitute-keyword-arguments (package-arguments gtksourceview)
                 ((#:phases phases)
                  `(modify-phases ,phases
                     (delete 'disable-gtk-update-icon-cache)))))))

(define-public gdk-pixbuf
  (package
    (name "gdk-pixbuf")
    (version "2.42.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1iz392vrlrnb92hrak697bgndsbkrcxhqxilxn6f99xr8ls5nl5r"))
              (patches
               (search-patches
                "gdk-pixbuf-honor-GUIX_GDK_PIXBUF_MODULE_FILES.patch"))))
    (build-system meson-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:glib-or-gtk? #t             ; To wrap binaries and/or compile schemas
       #:configure-flags '("-Dinstalled_tests=false"
                           ,@(if (%current-target-system)
                                 '()
                                 '("-Dgtk_doc=true"))
                           ;; GTK+ 3 needs the XPM loader, see
                           ;; <https://gitlab.gnome.org/GNOME/gtk/-/issues/7143>.
                           "-Dothers=enabled")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-failing-tests
           (lambda _
             ;; The test for the fix for issue 205 causes failures.
             ;; https://gitlab.gnome.org/GNOME/gdk-pixbuf/-/issues/215
             (substitute* "tests/pixbuf-jpeg.c"
               ((".*/pixbuf/jpeg/issue205.*")
                ""))))
         ;; The slow tests take longer than the specified timeout.
         ,@(if (target-arm? (%current-system))
               '((replace 'check
                   (lambda* (#:key tests? #:allow-other-keys)
                     (when tests?
                       (invoke "meson" "test" "--timeout-multiplier" "5")))))
               '()))))
    (propagated-inputs
     (list glib                         ;in Requires of gdk-pixbuf-2.0.pc

           ;; These are in Requires.private of gdk-pixbuf-2.0.pc
           libjpeg-turbo
           libpng
           libtiff
           shared-mime-info))           ;required at runtime, too
    (inputs
     (if (%current-target-system)
         (list bash-minimal)            ;for glib-or-gtk-wrap
         '()))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ;glib-mkenums, etc.
           gobject-introspection        ;g-ir-compiler, etc.
           perl
           pkg-config

           ;; For the documentation.
           gi-docgen
           python-docutils))
    (native-search-paths
     ;; This file is produced by the gdk-pixbuf-loaders-cache-file
     ;; profile hook.
     (list (search-path-specification
            (variable "GUIX_GDK_PIXBUF_MODULE_FILES")
            (files (list %gdk-pixbuf-loaders-cache-file))
            (file-type 'regular))))
    (synopsis "Image loading library")
    (description "GdkPixbuf is a library that loads image data in various
formats and stores it as linear buffers in memory.  The buffers can then be
scaled, composited, modified, saved, or rendered.")
    (home-page "https://wiki.gnome.org/Projects/GdkPixbuf")
    (license license:lgpl2.1+)))

(define-public gdk-pixbuf-xlib
  (package
    (name "gdk-pixbuf-xlib")
    (version "2.40.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://gitlab.gnome.org/Archive/gdk-pixbuf-xlib.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vwnvqxap3r9zw932jwasazy9sxw49j78x2g650xkn70iili90bg"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Dgtk_doc=true")))
    (native-inputs (list pkg-config gtk-doc/stable))
    (inputs (list gdk-pixbuf libx11))
    (synopsis "Deprecated Xlib integration for GdkPixbuf")
    (description
     "GdkPixbuf-Xlib contains the deprecated API for integrating GdkPixbuf with
Xlib data types.  This library was originally shipped by gdk-pixbuf, and has
since been moved out of the original repository.  No newly written code should
ever use this library.")
    (home-page "https://gitlab.gnome.org/Archive/gdk-pixbuf-xlib")
    (license license:lgpl2.1+)))

;;; A minimal variant used to prevent a cycle with Inkscape.
(define-public at-spi2-core
  (hidden-package
   (package
     (name "at-spi2-core")
     (version "2.52.0")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnome/sources/" name "/"
                                   (version-major+minor version)  "/"
                                   name "-" version ".tar.xz"))
               (sha256
                (base32
                 "1azmbzik0gl2s03c9lq3dff3h1iql1zvlwn28yhizl68421zrhqa"))))
     (build-system meson-build-system)
     (arguments
      (list
       #:glib-or-gtk? #t              ;to wrap binaries and/or compile schemas
       #:tests? (not (or (target-ppc32?)
                         (%current-target-system)))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'check)
           (add-after 'install 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 ;; xfconfd requires a writable HOME
                 (setenv "HOME" (getenv "TMPDIR"))
                 ;; dbus-run-session may crash if XDG_DATA_DIRS has too
                 ;; many entries, maybe related to
                 ;; https://gitlab.freedesktop.org/dbus/dbus/-/issues/481.
                 (setenv "XDG_DATA_DIRS"
                         (string-append
                          #$output "/share:"
                          #$(this-package-native-input
                             "gsettings-desktop-schemas")
                          "/share"))
                 ;; Don't fail on missing  '/etc/machine-id'.
                 (setenv "DBUS_FATAL_WARNINGS" "0")
                 (with-directory-excursion (string-append "../at-spi2-core-"
                                                          #$version "")
                   (invoke "dbus-run-session" "--" "ci/run-registryd-tests.sh")
                   (substitute* "tests/atspi/meson.build"
                     ;; Remove a timeout that caused aarch64 build failures.
                     ((", timeout: [0-9]+") ""))
                   (substitute* "ci/run-tests.sh"
                     (("ps auxwww") "")   ;avoid a dependency on procps
                     (("meson test -C _build")
                      "meson test -C ../build")) ;adjust build directory
                   (invoke "dbus-run-session" "--" "ci/run-tests.sh"))))))))
     (inputs
      (list bash-minimal libxml2))
     (propagated-inputs
      ;; atspi-2.pc refers to all these.
      (list dbus glib libx11 libxi libxtst))
     (native-inputs
      (list findutils
            gettext-minimal
            `(,glib "bin")
            gobject-introspection
            gsettings-desktop-schemas
            pkg-config
            python-dbusmock-minimal
            python-pytest
            python-wrapper))
     (synopsis "Assistive Technology Service Provider Interface, core components")
     (description
      "The Assistive Technology Service Provider Interface, core components,
is part of the GNOME accessibility project.")
     (license license:lgpl2.1+)
     (home-page "https://wiki.gnome.org/Accessibility/"))))

(define-public at-spi2-core-with-documentation
  (package/inherit at-spi2-core
    (outputs (cons "doc" (package-outputs at-spi2-core)))
    (arguments
     (substitute-keyword-arguments (package-arguments at-spi2-core)
       ((#:configure-flags flags ''())
        #~(cons #$(if (%current-target-system)
                      "-Ddocs=false"
                      "-Ddocs=true")
                #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'set-documentation-path
              (lambda _
                ;; Ensure that the cross-references point to the "doc" output.
                (substitute* "doc/meson.build"
                  (("docs_dir =.*")
                   (string-append "docs_dir = '" #$output:doc
                                  "/share/doc'\n")))))))))
    (native-inputs
     (modify-inputs (package-native-inputs at-spi2-core)
       (append gi-docgen python python-sphinx)
       (replace "python-dbusmock" python-dbusmock)))
    (properties (alist-delete 'hidden?
                              (package-properties at-spi2-core)))))

(define-public at-spi2-atk
  (deprecated-package "at-spi2-atk" at-spi2-core))

(define-public atk
  (deprecated-package "atk" at-spi2-core))

(define-public gtk+-2
  (package
    (name "gtk+")
    (version "2.24.33")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1nn6kks1zyvb5xikr9y2k7r9bwjy1g4b0m0s66532bclymbwfamc"))
              (patches (search-patches "gtk2-respect-GUIX_GTK2_PATH.patch"
                                       "gtk2-respect-GUIX_GTK2_IM_MODULE_FILE.patch"
                                       "gtk2-harden-list-store.patch"
                                       "gtk2-theme-paths.patch"
                                       "gtk2-fix-builder-test.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "bin" "doc" "debug"))
    (propagated-inputs
     (list at-spi2-core
           cairo
           glib
           (librsvg-for-system)
           pango))
    (inputs
     (list cups
           libx11
           libxcomposite
           libxcursor
           libxext
           libxdamage
           libxi
           libxinerama
           libxkbcommon
           libxrandr
           libxrender
           libxshmfence))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           intltool
           perl
           pkg-config
           python-wrapper
           xorg-server-for-tests))
    (arguments
     (list
      #:parallel-tests? #f
      #:configure-flags
      #~(list "--with-xinput=yes"
              (string-append "--with-html-dir=" #$output
                             "/share/gtk-doc/html"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* "gtk/Makefile.in"
                (("aliasfilescheck\\.sh") ""))
              (substitute* "gtk/tests/recentmanager.c"
                (("g_test_add_func \\(\"/recent-manager.*;") ""))
              (substitute* "gtk/tests/defaultvalue.c"
                (("return g_test_run\\(\\);") ""))
              ;; These require XPM support in Gdk-Pixbuf which is obsolete.
              (substitute* "gtk/tests/textbuffer.c"
                (("g_test_add_func.*test_fill_empty\\);")
                 "")
                (("g_test_add_func.*test_tag\\);")
                 ""))))
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a running X server.
              (system "Xvfb :1 +extension GLX &")
              (setenv "DISPLAY" ":1")
              ;; Tests write to $HOME.
              (setenv "HOME" (getcwd))
              ;; Tests look for $XDG_RUNTIME_DIR.
              (setenv "XDG_RUNTIME_DIR" (getcwd))
              ;; For missing '/etc/machine-id'.
              (setenv "DBUS_FATAL_WARNINGS" "0")))
          (add-after 'install 'remove-cache
            (lambda _
	      (for-each delete-file
	                (find-files #$output "immodules.cache")))))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_GTK2_PATH")
            (files '("lib/gtk-2.0")))))
    (search-paths native-search-paths)
    (synopsis "Cross-platform toolkit for creating graphical user interfaces")
    (description
     "GTK+, or the GIMP Toolkit, is a multi-platform toolkit for creating
graphical user interfaces.  Offering a complete set of widgets, GTK+ is
suitable for projects ranging from small one-off tools to complete
application suites.")
    (license license:lgpl2.0+)
    (home-page "https://www.gtk.org/")))

(define-public gtk+
  (package
    (inherit gtk+-2)
    (name "gtk+")
    (version "3.24.43")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1izky8dxaxp4bg5nii4n58dgpkw79mvmvbkldf04n0qmhmjg013y"))
       (patches (search-patches "gtk3-respect-GUIX_GTK3_PATH.patch"
                                "gtk3-respect-GUIX_GTK3_IM_MODULE_FILE.patch"))))
    ;; There is no "doc" output, because adding gtk-doc here would introduce a
    ;; dependency cycle with itself.
    (outputs '("out" "bin"))
    (build-system meson-build-system)
    (propagated-inputs
     (list at-spi2-core
           cairo
           fribidi
           fontconfig
           freetype
           (librsvg-for-system)
           glib
           libcloudproviders-minimal
           libepoxy
           libx11
           libxcomposite
           libxcursor
           libxdamage
           libxext
           libxfixes
           libxi
           libxinerama
           libxkbcommon
           libxrandr
           libxrender
           mesa
           pango
           wayland
           wayland-protocols))
    (inputs
     (list colord-minimal               ;to prevent a cycle with inkscape
           cups
           graphene
           harfbuzz
           iso-codes/pinned
           json-glib-minimal
           libxml2
           rest))
    (native-inputs
     (list docbook-xml-4.3
           docbook-xsl
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           hicolor-icon-theme
           pkg-config
           python-wrapper
           sassc
           ;; By using a special xorg-server for GTK+'s tests, we reduce the impact
           ;; of updating xorg-server directly on the master branch.
           xorg-server-for-tests
           libxslt))
    (arguments
     (list
      #:imported-modules `((guix build glib-or-gtk-build-system)
                           ,@%meson-build-system-modules)
      #:modules '((guix build utils)
                  (guix build meson-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
      #:disallowed-references (list xorg-server-for-tests)
      #:configure-flags
      #~(list "-Dcloudproviders=true"   ;for cloud-providers support
              "-Dcolord=yes"            ;for color printing support
              "-Dbroadway_backend=true"
              "-Dman=true")
      ;; Use the same test options as upstream uses for their CI (see the
      ;; .gitlab-ci/run-tests.sh file).
      #:test-options '(list "--suite=gtk"
                            "--no-suite=failing"
                            "--no-suite=flaky"
                            "--no-suite=gsk-compare-broadway")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              ;; These tests fail only in the containerized environment, for
              ;; unknown reasons.
              (substitute* "testsuite/gtk/meson.build"
                ((".*\\['defaultvalue'],.*") "")
                ((".*\\['objects-finalize',.*") ""))))
          (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
            (assoc-ref glib-or-gtk:%standard-phases
                       'generate-gdk-pixbuf-loaders-cache-file))
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a running X server.
              (system "Xvfb :1 +extension GLX &")
              (setenv "DISPLAY" ":1")
              ;; Tests write to $HOME.
              (setenv "HOME" (getcwd))
              ;; Tests look for $XDG_RUNTIME_DIR.
              (setenv "XDG_RUNTIME_DIR" (getcwd))
              ;; For missing '/etc/machine-id'.
              (setenv "DBUS_FATAL_WARNINGS" "0")))
          (add-after 'install 'move-desktop-files
            ;; Move desktop files into 'bin' to avoid cycle references.
            (lambda* (#:key outputs #:allow-other-keys)
              (mkdir-p (string-append #$output:bin "/share"))
              (rename-file (string-append #$output "/share/applications")
                           (string-append #$output:bin
                                          "/share/applications")))))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_GTK3_PATH")
            (files '("lib/gtk-3.0")))))))

(define-public gtk
  (package
    (name "gtk")
    (version "4.16.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "1624c9hjp47rlnybhm9vym3hd3dpav5db4fi8nlkk0c45ghxkwyx"))
       (patches
        (search-patches "gtk4-respect-GUIX_GTK4_PATH.patch"))
       (modules '((guix build utils)))))
    (build-system meson-build-system)
    (outputs '("out" "bin" "doc"))
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build meson-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
      #:configure-flags
      #~(list
         "-Dbroadway-backend=true"      ;for broadway display-backend
         "-Dcloudproviders=enabled"     ;for cloud-providers support
         "-Dtracker=enabled"            ;for filechooser search support
         "-Dcolord=enabled"             ;for color printing support
         "-Ddocumentation=true"
         "-Dman-pages=true")
      #:test-options
      #~(list "--setup=x11"  ;defaults to wayland
              ;; Use the same test options as upstream uses for
              ;; their CI.
              "--suite=gtk"
              "--no-suite=failing"
              "--no-suite=flaky"
              "--no-suite=headless"     ; requires mutter…
              "--no-suite=gsk-compare-broadway"
              "--no-suite=needs-udmabuf"
              ;; These seem to fail on aarch64, and Debian has
              ;; also disabled these, see:
              ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1050075
              "--no-suite=wayland_failing"
              "--no-suite=wayland_gles2_failing")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
            (assoc-ref glib-or-gtk:%standard-phases
                       'generate-gdk-pixbuf-loaders-cache-file))
          (add-after 'unpack 'patch-rst2man
            (lambda _
              (substitute* "docs/reference/gtk/meson.build"
                (("find_program\\('rst2man'")
                 "find_program('rst2man.py'"))))
          (add-after 'unpack 'patch
            (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
              ;; Disable building of icon cache.
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))
              (substitute* (find-files "testsuite/gsk/nodeparser/"
                                       "^text-color.*\\.node$")
                (("Noto Sans") "DejaVu Sans"))
              ;; Disable failing tests.
              (substitute* (find-files "testsuite" "meson.build")
                (("[ \t]*'empty-text.node',") "")
                (("[ \t]*'testswitch.node',") "")
                (("[ \t]*'widgetfactory.node',") "")
                ;; This test, 'gtk:tools / validate', started failing for
                ;; unknown reasons after updating mesa to 23.3.1 and xorgproto
                ;; to 2023.2.
                ((" 'validate',") "")
                ;; XXX: Figure out why this fails and report upstream.
                ((".*'memorytexture',.*") "")
                ;; Some mask-half-pixel variant tests of the gsk-compare-gl
                ;; suite are failing starting with 4.16.13.
                ;; TODO: Reinstate in 4.18.
                ((".*'mask-half-pixel',.*") "")
                ;; The 'gtk:gsk / scaling' test fails starting with 4.16.13.
                ;; TODO: Reinstate in 4.18.
                ((".*'scaling',.*") ""))
              (substitute* "testsuite/reftests/meson.build"
                (("[ \t]*'label-wrap-justify.ui',") ""))
              ;; These tests fail on an Apple M1 (aarch64) with the following errors:
              ;; - MESA: error: ZINK: failed to choose pdev
              ;; - libEGL warning: egl: failed to create dri2 screen
              ;; - MESA: error: ZINK: failed to choose pdev
              ;; - glx: failed to create drisw screen
              #$@(if (target-aarch64?)
                     #~((substitute* "testsuite/gsk/meson.build"
                          (("'border-bottom-right',") "")
                          (("'border-one-rounded',") "")
                          (("'border-opacity',") "")
                          (("'border-zero-width-color',") "")
                          (("'borders-rotated',") "")
                          (("'borders-scaled',") "")
                          (("'clip-in-smaller-rounded-clip',") "")
                          (("'css-background',") "")
                          (("'empty-border',") "")
                          (("'empty-inset-shadow',") "")
                          (("'empty-outset-shadow',") "")
                          (("'inset-shadow-multiple',") "")
                          (("'outset-shadow-scale-offset',") "")
                          (("'outset_shadow_offset_both',") "")
                          (("'outset_shadow_offset_x',") "")
                          (("'outset_shadow_offset_y',") "")
                          (("'outset_shadow_rounded_top',") "")
                          (("'outset_shadow_simple',") "")
                          (("'shadow-offset-clip',") "")
                          (("'shrink-rounded-border',") ""))
                        (substitute* "testsuite/css/parser/meson.build"
                          ((".*color-mix.*") "")))
                     #~())
              ;; XXX: These failures appear specific to i686 – investigate them.
              #$@(if (target-x86-32?)
                     #~((substitute* "testsuite/gsk/meson.build"
                          (("'empty-(fill|stroke)\\.node',") "")
                          (("'fill2?\\.node',") "")
                          (("'stroke\\.node',") "")
                          (("'fill-fractional-([a-z-]*)-nogl',") "")
                          (("\\[ 'path-special-cases' \\],") "")
                          (("\\[ '(path|curve)-special-cases' \\],") "")
                          (("\\[ 'path-private' \\],") ""))
                        (substitute* "testsuite/a11y/meson.build"
                          (("\\{ 'name': 'text(view)?' \\},") "")))
                     #~())))
          (add-before 'build 'set-cache
            (lambda _
              (setenv "XDG_CACHE_HOME" (getcwd))))
          (add-before 'check 'pre-check
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Tests require a running X server.
              (system "Xvfb :1 +extension GLX &")
              (setenv "DISPLAY" ":1")
              ;; Tests write to $HOME.
              (setenv "HOME" (getcwd))
              ;; Tests look for those variables.
              (setenv "XDG_RUNTIME_DIR" (getcwd))
              ;; For missing '/etc/machine-id'.
              (setenv "DBUS_FATAL_WARNINGS" "0")
              ;; Required for the calendar test.
              (setenv "TZDIR" (search-input-directory inputs
                                                      "share/zoneinfo"))))
          (add-after 'install 'move-files
            (lambda _
              (for-each mkdir-p
                        (list
                         (string-append #$output:bin "/share/applications")
                         (string-append #$output:bin "/share/icons")
                         (string-append #$output:bin "/share/man")
                         (string-append #$output:bin "/share/metainfo")
                         (string-append #$output:doc "/share/doc")))
              ;; Move programs and related files to output 'bin'.
              (for-each (lambda (dir)
                          (rename-file
                           (string-append #$output dir)
                           (string-append #$output:bin dir)))
                        (list
                         "/share/applications"
                         "/share/icons"
                         "/share/man"
                         "/share/metainfo"))
              ;; Move HTML documentation to output 'doc'.
              (rename-file
               (string-append #$output "/share/doc")
               (string-append #$output:doc "/share/doc")))))))
    (native-inputs
     (list docbook-xml-4.3
           docbook-xsl
           gettext-minimal
           `(,glib "bin")
           gobject-introspection        ;for building introspection data
           graphene
           gtk-doc                      ;for building documentation
           intltool
           libxslt                      ;for building man-pages
           pkg-config
           python-pygobject
           ;; These python modules are required for building documentation.
           python-docutils
           python-jinja2
           python-markdown
           python-markupsafe
           python-pygments
           python-toml
           python-typogrify
           sassc                        ;for building themes
           shaderc
           tzdata-for-tests
           vala
           xorg-server-for-tests))
    (inputs
     (list colord                       ;for color printing support
           cups                         ;for CUPS print-backend
           ffmpeg                       ;for ffmpeg media-backend
           fribidi
           gi-docgen
           gstreamer                    ;for gstreamer media-backend
           gst-plugins-bad              ;provides gstreamer-player
           gst-plugins-base             ;provides gstreamer-gl
           harfbuzz
           iso-codes/pinned
           json-glib
           libcloudproviders            ;for cloud-providers support
           libgudev                     ;for gstreamer-gl
           libjpeg-turbo
           libpng
           libtiff
           python
           rest
           tracker))                    ;for filechooser search support
    (propagated-inputs
     ;; Following dependencies are referenced in .pc files.
     (list cairo
           fontconfig
           (librsvg-for-system)
           glib
           graphene
           libepoxy
           libx11                       ;for x11 display-backend
           libxcomposite
           libxcursor
           libxdamage
           libxext
           libxfixes
           libxi
           libxinerama                  ;for xinerama support
           libxkbcommon
           libxrandr
           libxrender
           pango
           vulkan-headers
           vulkan-loader                ;for vulkan graphics API support
           wayland                      ;for wayland display-backend
           wayland-protocols))
    (native-search-paths
     (list
      (search-path-specification
       (variable "GUIX_GTK4_PATH")
       (files '("lib/gtk-4.0")))))
    (search-paths native-search-paths)
    (home-page "https://www.gtk.org/")
    (synopsis "Cross-platform widget toolkit")
    (description "GTK is a multi-platform toolkit for creating graphical user
interfaces.  Offering a complete set of widgets, GTK is suitable for projects
ranging from small one-off tools to complete application suites.")
    (license license:lgpl2.1+)))

;;;
;;; Guile bindings.
;;;

(define-public guile-cairo
  (package
    (name "guile-cairo")
    (version "1.11.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-cairo/guile-cairo-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0yx0844p61ljd4d3d63qrawiygiw6ks02fwv2cqx7nav5kfd8ck2"))
              (modules '((guix build utils)))
              (snippet
               (begin
                 '(begin
                    ;; Install Scheme files in …/guile/site/X.Y.
                    (substitute* (find-files "." "^Makefile\\.in$")
                      (("^(.*)dir = (.*)/guile/site(.*)" _ name prefix suffix)
                       (string-append name "dir = " prefix
                                      "/guile/site/@GUILE_EFFECTIVE_VERSION@"
                                      suffix)))
                    #t)))))
    (build-system gnu-build-system)
    (arguments
     ;; Uses of 'scm_t_uint8' & co. are deprecated; don't stop the build
     ;; because of them.
     `(#:configure-flags '("--disable-Werror")
       #:make-flags '("GUILE_AUTO_COMPILE=0") ; to prevent guild warnings
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 rdelim)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-go-files
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            "guile" "-c"
                                            "(display (effective-version))")))
                    (module-dir (string-append out "/share/guile/site/"
                                               effective))
                    (object-dir (string-append out "/lib/guile/" effective
                                               "/site-ccache"))
                    (prefix     (string-length module-dir)))
               ;; compile to the destination
               (for-each (lambda (file)
                           (let* ((base (string-drop (string-drop-right file 4)
                                                     prefix))
                                  (go   (string-append object-dir base ".go")))
                             (invoke "guild" "compile" "-L" module-dir
                                     file "-o" go)))
                         (find-files module-dir "\\.scm$"))
               #t))))))
    (inputs
     (list guile-lib guile-3.0))
    (propagated-inputs
     ;; The .pc file refers to 'cairo'.
     (list cairo))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.nongnu.org/guile-cairo/")
    (synopsis "Cairo bindings for GNU Guile")
    (description
     "Guile-Cairo wraps the Cairo graphics library for Guile Scheme.
Guile-Cairo is complete, wrapping almost all of the Cairo API.  It is API
stable, providing a firm base on which to do graphics work.  Finally, and
importantly, it is pleasant to use.  You get a powerful and well-maintained
graphics library with all of the benefits of Scheme: memory management,
exceptions, macros, and a dynamic programming environment.")
    (license license:lgpl3+)))

(define-public guile2.2-cairo
  (package
    (inherit guile-cairo)
    (name "guile2.2-cairo")
    (inputs
     `(("guile" ,guile-2.2)
       ("guile-lib" ,guile2.2-lib)
       ,@(fold alist-delete (package-inputs guile-cairo)
               '("guile" "guile-lib"))))))

(define-public guile-cairo-next
  ;; A commit with cairo-pointer->context, missing from guile-cairo@1.11.2
  ;; and needed by animated-paintable from g-golf-gtk-4-examples.
  (let ((commit "30da459d7a4380174ff243b1560d5512a4bca86e")
        (revision "0"))
    (package
      (inherit guile-cairo)
      (name "guile-cairo-next")
      (version (git-version "1.11.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/guile-cairo.git/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0dslfldzgxis8g0g3xaffcqnd1njzz23fjy0v3lc0r2694ra4ny4"))))
      (arguments
       (substitute-keyword-arguments (package-arguments guile-cairo)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; To allow running the check phase before install, add two phases
             ;; similar to David Pirotte's suggested patch:
             ;; <https://lists.gnu.org/archive/html/guile-user/2023-03/msg00028.html>.
             (add-after 'build 'fix-dynamic-link-path
               (lambda _
                 ;; Dynamic-link libguile-cairo foreign extension by name, not
                 ;; path.
                 (substitute* "cairo/config.scm"
                   (("\\(define \\*cairo-lib-path\\* .*")
                    "\(define *cairo-lib-path* \"libguile-cairo\")\n"))))
             (add-before 'check 'set-libtool-path
               (lambda _
                 ;; Use appropriate pre-install libtool path in tests.
                 (setenv "LTDL_LIBRARY_PATH" "../../guile-cairo/.libs")))))))
      (inputs
       (list gettext-minimal guile-3.0 guile-lib))
      (native-inputs
       (list autoconf automake libtool pkg-config texinfo)))))

(define-public guile-rsvg
  ;; Use a recent snapshot that supports Guile 2.2 and beyond.
  (let ((commit "05c6a2fd67e4fea1a7c3ff776729dc931bae6678")
        (revision "0"))
    (package
      (name "guile-rsvg")
      (version (string-append "2.18.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/wingo/guile-rsvg/")
                      (commit commit)))
                (sha256
                 (base32
                  "0cnbl40df2sbhpc32cma6j6w312rfvcgbxxqaixgf0ymim3fb248"))
                (patches (search-patches "guile-rsvg-pkgconfig.patch"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (substitute* (find-files "." "Makefile\\.am")
                      (("/share/guile/site")
                       "/share/guile/site/@GUILE_EFFECTIVE_VERSION@"))
                    #t))
                (file-name (string-append name "-" version ".tar.gz"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 rdelim)
                    (ice-9 popen))
         #:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (invoke "autoreconf" "-vfi")))
           (add-after 'install 'install-go-files
             (lambda* (#:key outputs inputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (effective (read-line
                                  (open-pipe* OPEN_READ
                                              "guile" "-c"
                                              "(display (effective-version))")))
                      (module-dir (string-append out "/share/guile/site/"
                                                 effective))
                      (object-dir (string-append out "/lib/guile/" effective
                                                 "/site-ccache"))
                      (prefix     (string-length module-dir)))
                 ;; compile to the destination
                 (for-each (lambda (file)
                             (let* ((base (string-drop (string-drop-right file 4)
                                                       prefix))
                                    (go   (string-append object-dir base ".go")))
                               (invoke "guild" "compile" "-L" module-dir
                                       file "-o" go)))
                           (find-files module-dir "\\.scm$"))
                 #t))))))
      (native-inputs (list pkg-config autoconf automake libtool texinfo))
      (inputs (list guile-3.0
                    (librsvg-for-system) guile-lib))        ;for (unit-test)
      (propagated-inputs (list guile-cairo))
      (synopsis "Render SVG images using Cairo from Guile")
      (description
       "Guile-RSVG wraps the RSVG library for Guile, allowing you to render SVG
images onto Cairo surfaces.")
      (home-page "https://wingolog.org/projects/guile-rsvg/")
      (license license:lgpl2.1+))))

(define-public guile2.2-rsvg
  (package
    (inherit guile-rsvg)
    (name "guile2.2-rsvg")
    (inputs
     `(("guile" ,guile-2.2)
       ("guile-lib" ,guile2.2-lib)
       ,@(fold alist-delete (package-inputs guile-rsvg)
               '("guile" "guile-lib"))))
    (propagated-inputs `(("guile-cairo" ,guile2.2-cairo)))))

(define-public guile-present
  (package
    (name "guile-present")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://wingolog.org/pub/guile-present/"
                                  "guile-present-" version ".tar.gz"))
              (sha256
               (base32
                "1qam447m05sxxv6x8dlzg7qnyfc4dh8apjw1idpfhpns671gfr6m"))
              (patches (search-patches "guile-present-coding.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("2\\.2 2\\.0")
                     "3.0 2.2 2.0"))

                  ;; Install .go files in the right place.
                  (substitute* "Makefile.in"
                    (("/ccache") "/site-ccache"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (guile (assoc-ref inputs "guile"))
                    (version
                     ,(match (assoc "guile" (package-inputs this-package))
                        (("guile" guile)
                         (version-major+minor (package-version guile))))))
               (substitute* (find-files bin ".*")
                 (("guile")
                  (string-append guile "/bin/guile -L "
                                 out "/share/guile/site/" version " -C "
                                 out "/lib/guile/" version "/site-ccache "))))
             #t)))))
    (native-inputs (list pkg-config))
    (inputs (list guile-3.0))
    (propagated-inputs
     ;; These are used by the (present …) modules.
     (list guile-lib guile-cairo guile-rsvg))
    (home-page "https://wingolog.org/software/guile-present/")
    (synopsis "Create SVG or PDF presentations in Guile")
    (description
     "Guile-Present defines a declarative vocabulary for presentations,
together with tools to render presentation documents as SVG or PDF.
Guile-Present can be used to make presentations programmatically, but also
includes a tools to generate PDF presentations out of Org mode and Texinfo
documents.")
    (license license:lgpl3+)))

(define-public guile2.2-present
  (package
    (inherit guile-present)
    (name "guile2.2-present")
    (inputs (list guile-2.2))
    (propagated-inputs
     `(("guile-lib" ,guile2.2-lib)
       ("guile-cairo" ,guile2.2-cairo)
       ("guile-rsvg" ,guile2.2-rsvg)))))

(define-public guile-gnome
   (package
    (name "guile-gnome")
    (version "2.16.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://gnu/" name
                              "/guile-gnome-platform/guile-gnome-platform-"
                              version ".tar.gz"))
             (sha256
              (base32
               "1gnf3j96nip5kl99a268i0dy1hj7s1cfs66sps3zwysnkd7qr399"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           at-spi2-core
           ;;("corba" ,corba) ; not packaged yet
           gconf
           gobject-introspection
           ;;("gthread" ,gthread) ; not packaged yet
           gnome-vfs
           gdk-pixbuf
           gtk+-2
           libglade
           libgnome
           libgnomecanvas
           libgnomeui
           pango
           libffi
           glib))
    (inputs (list guile-2.2))
    (propagated-inputs
     `(("guile-cairo" ,guile2.2-cairo)
       ("g-wrap" ,g-wrap)
       ("guile-lib" ,guile2.2-lib)))
    (arguments
      `(#:tests? #f                               ;FIXME
        #:phases (modify-phases %standard-phases
                   (add-before 'configure 'pre-configure
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (substitute* (find-files "." "^Makefile.in$")
                           (("guilesite :=.*guile/site" all)
                            (string-append all "/@GUILE_EFFECTIVE_VERSION@")))
                         #t))))))
    (outputs '("out" "debug"))
    (synopsis "Guile interface for GTK+ programming for GNOME")
    (description
     "Includes guile-clutter, guile-gnome-gstreamer,
guile-gnome-platform (GNOME developer libraries), and guile-gtksourceview.")
    (home-page "https://www.gnu.org/software/guile-gnome/")
    (license license:gpl2+)
    (properties '((upstream-name . "guile-gnome-platform")
                  (ftp-directory . "/gnu/guile-gnome/guile-gnome-platform")))))

;;;
;;; C++ bindings.
;;;

(define-public cairomm
  (package
    (name "cairomm")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.cairographics.org/releases/"
                                  "cairomm-" version ".tar.xz"))
              (sha256
               (base32
                "1yrwy14k1lh74kmr41pnms6zr1c9z8md4xkji2mfia1y9qwma4mq"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dbuild-documentation=true"
        "-Dboost-shared=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))))))))
    (native-inputs
     (list boost
           doxygen
           graphviz
           mm-common
           perl
           pkg-config
           libxslt))
    (inputs
     (list fontconfig))
    (propagated-inputs
     (list libsigc++ cairo))
    (home-page "https://cairographics.org/")
    (synopsis "C++ bindings to the Cairo 2D graphics library")
    (description
     "Cairomm provides a C++ programming interface to the Cairo 2D graphics
library.")
    (license license:lgpl2.0+)))

(define-public cairomm-1.14
  (package
    (inherit cairomm)
    (name "cairomm")
    (version "1.14.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.cairographics.org/releases/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1qwdj9xw1w651kqwh82nipbryimm1ir5n3c6q34nphsx576bj9h1"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs cairomm)
       (prepend libsigc++-2)))))

(define-public pangomm
  (package
    (name "pangomm")
    (version "2.54.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1kvs9vbqikwlcfm7v8hhzc48xjpamfsq5skpabs1lyn4nz8iynsa"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:glib-or-gtk? #t              ; To wrap binaries and/or compile schemas
      #:configure-flags #~(list "-Dbuild-documentation=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'move-doc
            (lambda _
              (mkdir-p (string-append #$output:doc "/share"))
              (rename-file (string-append #$output "/share/doc")
                           (string-append #$output:doc "/share/doc")))))))
    (native-inputs
     (list graphviz
           doxygen
           m4
           mm-common
           perl
           pkg-config
           python
           libxslt))
    (propagated-inputs
     (list cairo
           cairomm
           glibmm
           pango))
    (home-page "https://pango.gnome.org//")
    (synopsis "C++ interface to the Pango text rendering library")
    (description "Pangomm provides a C++ programming interface to the Pango
text rendering library.")
    (license license:lgpl2.1+)))

(define-public pangomm-2.46
  (package
    (inherit pangomm)
    (name "pangomm")
    (version "2.46.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "08nvd36s2fqksrkh573cn4gz90cpyl91azrpp7j4shi62mk1c85r"))))
    (propagated-inputs
     (list cairomm-1.14 glibmm-2.66 pango))))

(define-public atkmm
  (package
    (name "atkmm")
    (version "2.36.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0x2rdjmgmxza83lznss69nz7z6ny1cbh1ih2fbdhmpn4l3m69hkf"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t             ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dbuild-documentation=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))))))))
    (native-inputs
     (list graphviz
           doxygen
           m4
           mm-common
           perl
           pkg-config
           python
           libxslt))
    (propagated-inputs
     (list glibmm at-spi2-core))
    (synopsis "C++ bindings for ATK")
    (description "ATKmm is the C++ binding for the ATK library.")
    (home-page "https://wiki.gnome.org/Accessibility")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Tools
      license:gpl2+))))

(define-public atkmm-2.28
  (package
    (inherit atkmm)
    (name "atkmm")
    (version "2.28.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1cysiz908phkagwnls44xxa60xls7r3fw540zcg00g7q520jl50a"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs atkmm)
       (replace "glibmm" glibmm-2.66)))))

(define-public gtkmm
  (package
    (name "gtkmm")
    (version "4.14.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1npcf6i07riw20rg5x6rnqi0jlh7cwdvsvjqd7fa6k3l9d2a0l4k"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags '("-Dbuild-documentation=true")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-cache
           (lambda _
             (setenv "XDG_CACHE_HOME" (getcwd))))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))))))))
    (native-inputs
     (list graphviz
           doxygen
           `(,glib "bin")
           m4
           mm-common
           perl
           pkg-config
           python
           libxslt
           xorg-server-for-tests))
    (propagated-inputs
     (list cairomm glibmm gtk pangomm))
    (synopsis "C++ Interfaces for GTK+ and GNOME")
    (description "GTKmm is the official C++ interface for the popular GUI
library GTK+.  Highlights include typesafe callbacks, and a comprehensive set
of widgets that are easily extensible via inheritance.  You can create user
interfaces either in code or with the Glade User Interface designer, using
libglademm.  There's extensive documentation, including API reference and a
tutorial.")
    (home-page "https://gtkmm.org/")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Tools
      license:gpl2+))))

(define-public gtkmm-3
  (package
    (inherit gtkmm)
    (name "gtkmm")
    (version "3.24.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1kj4mla3z9kxhdby5w88nl744xkmq6xchf79m1kfa72p0kjbzm9h"))))
    (propagated-inputs
     (list atkmm-2.28
           cairomm-1.14
           glibmm
           gtk+
           pangomm-2.46))))

(define-public gtkmm-2
  (package
    (inherit gtkmm)
    (name "gtkmm")
    (version "2.24.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0wkbzvsx4kgw16f6xjdc1dz7f77ldngdila4yi5lw2zrgcxsb006"))))
    (build-system gnu-build-system)
    (arguments
     (strip-keyword-arguments
      '(#:configure-flags) (package-arguments gtkmm)))
    (propagated-inputs
     (list atkmm-2.28 cairomm-1.14 glibmm-2.66 gtk+-2 pangomm-2.46))))

(define-public gtksourceviewmm
  (package
    (name "gtksourceviewmm")
    (version "3.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32 "0fgvmhm4h4qmxig87qvangs6ijw53mi40siz7pixlxbrsgiil22i"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; In 'Requires' of gtksourceviewmm-3.0.pc.
     (list glibmm gtkmm-3 gtksourceview-3))
    (synopsis "C++ interface to the GTK+ 'GtkTextView' widget")
    (description
     "gtksourceviewmm is a portable C++ library that extends the standard GTK+
framework for multiline text editing with support for configurable syntax
highlighting, unlimited undo/redo, search and replace, a completion framework,
printing and other features typical of a source code editor.")
    (license license:lgpl2.1+)
    (home-page "https://developer.gnome.org/gtksourceview/")))

;;;
;;; Python bindings.
;;;

(define-public python-pycairo
  (package
    (name "python-pycairo")
    (version "1.26.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/pygobject/pycairo/releases/download/v"
                          version "/pycairo-" version ".tar.gz"))
      (sha256
       (base32
        "1sybz43sj4ynjahlkidrcdpdrq8yi1avkndc2hgb5pgvfjld1p9d"))))
    (build-system python-build-system)
    (native-inputs
     (list pkg-config python-pytest))
    (propagated-inputs                  ;pycairo.pc references cairo
     (list cairo))
    (home-page "https://cairographics.org/pycairo/")
    (synopsis "Python bindings for cairo")
    (description
     "Pycairo is a set of Python bindings for the Cairo graphics library.")
    (properties
     '((upstream-name . "pycairo")))
    (license license:lgpl3+)))

;; Pycairo no longer supports Python 2 since version 1.19.0, so we stick

(define-public perl-cairo
  (package
    (name "perl-cairo")
    (version "1.109")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/X/XA/XAOC/Cairo-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0zq78dv22arg35ma6kah9cwfd1zx8gg7amsibzd128qw81p766c2"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (propagated-inputs
     (list cairo))
    (home-page "https://metacpan.org/release/Cairo")
    (synopsis "Perl interface to the cairo 2d vector graphics library")
    (description "Cairo provides Perl bindings for the vector graphics library
cairo.  It supports multiple output targets, including PNG, PDF and SVG.  Cairo
produces identical output on all those targets.")
    (license license:lgpl2.1+)))

(define-public perl-cairo-gobject
  (package
    (name "perl-cairo-gobject")
    (version "1.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/"
                           "Cairo-GObject-" version ".tar.gz"))
       (sha256
        (base32 "0l2wcz77ndmbgvxx34gdm919a3dxh9fixqr47p50n78ysx2692cd"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (propagated-inputs
     (list perl-cairo perl-glib))
    (home-page "https://metacpan.org/dist/Cairo-GObject")
    (synopsis "Integrate Cairo into the Glib type system")
    (description "Cairo::GObject registers Cairo's types with Glib's type systems,
so that they can be used normally in signals and properties.")
    (license license:lgpl2.1+)))

(define-public perl-gtk2
  (package
    (name "perl-gtk2")
    (version "1.24993")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/Gtk2-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ry9jfvfgdwzalxcvwsgr7plhk3agx7p40l0fqdf3vrf7ds47i29"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig pkg-config))
    (inputs
     (list gtk+-2 gdk-pixbuf))
    (propagated-inputs
     (list perl-pango))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-tests
            (lambda _
              ;; XXX: by default it uses 'loaders.cache' from librsvg, which supports SVG only.
              (setenv "GDK_PIXBUF_MODULE_FILE"
                      (string-append #$(this-package-input "gdk-pixbuf")
                                     "/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"))
              ;; See https://gitlab.gnome.org/GNOME/perl-gtk2/issues/3.
              (substitute* "t/GdkPixbuf.t"
                (("tests => 112") "tests => 111")
                (("ok \\(defined \\$pixbuf, \"Don't crash on partial pixmap data\"\\);")
                 "# ok (defined $pixbuf, \"Don't crash on partial pixmap data\");")))))))
    (home-page "https://metacpan.org/release/Gtk2")
    (synopsis "Perl interface to the 2.x series of the Gimp Toolkit library")
    (description "Perl bindings to the 2.x series of the Gtk+ widget set.
This module allows you to write graphical user interfaces in a Perlish and
object-oriented way, freeing you from the casting and memory management in C,
yet remaining very close in spirit to original API.")
    (license license:lgpl2.1+)))

(define-public perl-gtk3
  (package
    (name "perl-gtk3")
    (version "0.038")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/Gtk3-"
                           version ".tar.gz"))
       (sha256
        (base32 "1k3sfcvxxx7ir7ail7w1lkmr4np0k3criljzw5wir63lmbr4pp3h"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1"))))))
    (native-inputs
     (list adwaita-icon-theme
           `(,gtk+ "bin")
           gobject-introspection
           perl-extutils-depends
           perl-extutils-pkgconfig
           perl-test-simple
           xorg-server-for-tests))
    (propagated-inputs
     (list gtk+
           perl-cairo-gobject
           perl-carp
           perl-exporter
           perl-glib-object-introspection))
    (home-page "https://metacpan.org/dist/Gtk3")
    (synopsis "Perl interface to the 3.x series of the gtk+ toolkit")
    (description "Perl bindings to the 3.x series of the gtk+ toolkit.
This module allows you to write graphical user interfaces in a Perlish and
object-oriented way, freeing you from the casting and memory management in C,
yet remaining very close in spirit to original API.")
    (license license:lgpl2.1+)))

(define-public perl-pango
  (package
    (name "perl-pango")
    (version "1.227")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/Pango-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0wdcidnfnb6nm79fzfs39ivawj3x8m98a147fmcxgv1zvwia9c1l"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (inputs
     (list pango))
    (propagated-inputs
     (list perl-cairo perl-glib))
    (home-page "https://metacpan.org/release/Pango")
    (synopsis "Layout and render international text")
    (description "Pango is a library for laying out and rendering text, with an
emphasis on internationalization.  Pango can be used anywhere that text layout
is needed, but using Pango in conjunction with Cairo and/or Gtk2 provides a
complete solution with high quality text handling and graphics rendering.

Dynamically loaded modules handle text layout for particular combinations of
script and font backend.  Pango provides a wide selection of modules, including
modules for Hebrew, Arabic, Hangul, Thai, and a number of Indic scripts.
Virtually all of the world's major scripts are supported.

In addition to the low level layout rendering routines, Pango includes
@code{Pango::Layout}, a high level driver for laying out entire blocks of text,
and routines to assist in editing internationalized text.")
    (license license:lgpl2.1+)))

(define-public girara
  ;; TODO: Move propagated inputs to inputs after core-updates is merged (as
  ;; of 2024-03)
  (package
    (name "girara")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.pwmt.org/pwmt/girara")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cbcs3810frgdmal5ia9pf3rk3k5h4xyzw1d2ia3rcg4nms5gcpx"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'start-xserver
            ;; Tests require a running X server.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((xorg-server (assoc-ref inputs "xorg-server"))
                    (display ":1"))
                (setenv "DISPLAY" display)

                ;; On busy machines, tests may take longer than
                ;; the default of four seconds.
                (setenv "CK_DEFAULT_TIMEOUT" "20")

                ;; Don't fail due to missing '/etc/machine-id'.
                (setenv "DBUS_FATAL_WARNINGS" "0")
                (zero? (system (string-append xorg-server "/bin/Xvfb "
                                              display " &")))))))))
    (native-inputs
     (list pkg-config
           check
           gettext-minimal
           `(,glib "bin")
           xorg-server-for-tests))
    ;; Listed in 'Requires.private' of 'girara.pc'.
    (propagated-inputs (list gtk+ json-glib))
    (build-system meson-build-system)
    (home-page "https://pwmt.org/projects/girara/")
    (synopsis "Library for minimalistic gtk+3 user interfaces")
    (description "Girara is a library that implements a user interface that
focuses on simplicity and minimalism.  Currently based on GTK+, a
cross-platform widget toolkit, it provides an interface that focuses on three
main components: a so-called view widget that represents the actual
application, an input bar that is used to execute commands of the
application and the status bar which provides the user with current
information.")
    (license license:zlib)))

(define-public gtk-doc
  (package
    (name "gtk-doc")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0746lwxgybc5ss3hzdd0crjjghk0ck0x9jbmz73iig405arp42xj"))
              (patches
               (search-patches "gtk-doc-respect-xml-catalog.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'install 'wrap-executables
           (lambda _
             (let ((docbook-xsl-catalog
                    #$(let ((docbook-xsl (this-package-input "docbook-xsl")))
                        (file-append docbook-xsl
                                     "/xml/xsl/" (package-name docbook-xsl)
                                     "-" (package-version docbook-xsl)
                                     "/catalog.xml"))))
               (for-each (lambda (prog)
                           (wrap-program prog
                             `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
                             `("XML_CATALOG_FILES" " " suffix (,docbook-xsl-catalog))))
                         (find-files (string-append #$output "/bin")))))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           itstool
           perl
           pkg-config
           python-wrapper))
    (inputs
     (list bash-minimal
           bc
           dblatex
           docbook-xml-4.3
           docbook-xsl
           glib
           libxml2
           libxslt
           python
           python-anytree
           python-lxml
           python-parameterized
           python-pygments
           source-highlight
           yelp-tools))
    ;; xsltproc's search paths, to avoid propagating libxslt.
    (native-search-paths %libxslt-search-paths)
    (home-page "https://wiki.gnome.org/DocumentationProject/GtkDoc")
    (synopsis "GTK+ DocBook Documentation Generator")
    (description "GtkDoc is a tool used to extract API documentation from C-code
like Doxygen, but handles documentation of GObject (including signals and
properties) that makes it very suitable for GTK+ apps and libraries.  It uses
docbook for intermediate files and can produce html by default and pdf/man-pages
with some extra work.")
    (license
     (list
      ;; Docs.
      license:fdl1.1+
      ;; Others.
      license:gpl2+))))

;; This is a variant of the 'gtk-doc' package that is not updated often.  It
;; is intended to be used as a native-input at build-time only.  This allows
;; the main 'gtk-doc', 'dblatex' and 'imagemagick' packages to be freely
;; updated on the 'master' branch without triggering an excessive number of
;; rebuilds.
(define-public gtk-doc/stable
  (hidden-package
   (package/inherit gtk-doc
     (inputs (modify-inputs (package-inputs gtk-doc)
               (replace "dblatex" dblatex/stable))))))

(define-public gtk-engines
  (package
    (name "gtk-engines")
    (version "2.20.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1db65pb0j0mijmswrvpgkdabilqd23x22d95hp5kwxvcramq1dhm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--enable-animation")))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     ;; Don't propagate GTK+ to reduce "profile pollution".
     (list gtk+-2)) ; required by gtk-engines-2.pc
    (home-page "https://live.gnome.org/GnomeArt")
    (synopsis "Theming engines for GTK+ 2.x")
    (description
     "This package contains the standard GTK+ 2.x theming engines including
Clearlooks, Crux, High Contrast, Industrial, LighthouseBlue, Metal, Mist,
Redmond95 and ThinIce.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public murrine
  (package
    (name "murrine")
    (version "0.98.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "129cs5bqw23i76h3nmc29c9mqkm9460iwc8vkl7hs4xr07h8mip9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--enable-animation"
         "--enable-animationrtl")))
    (native-inputs
     (list pkg-config intltool))
    (propagated-inputs
     (list gtk+-2))
    (home-page "https://live.gnome.org/GnomeArt")
    (synopsis "Cairo-based theming engine for GTK+ 2.x")
    (description
     "Murrine is a cairo-based GTK+ theming engine.  It is named after the
glass artworks done by Venicians glass blowers.")
    (license license:gpl2+)))

(define-public gtkspell3
  (package
    (name "gtkspell3")
    (version "3.0.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gtkspell/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0cjp6xdcnzh6kka42w9g0w2ihqjlq8yl8hjm9wsfnixk6qwgch5h"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config vala))
    (inputs
     (list gobject-introspection gtk+ pango))
    (propagated-inputs
     (list enchant))           ; gtkspell3-3.0.pc refers to it
    (home-page "https://gtkspell.sourceforge.net")
    (synopsis "Spell-checking addon for GTK's TextView widget")
    (description
     "GtkSpell provides word-processor-style highlighting and replacement of
misspelled words in a GtkTextView widget.")
    (license license:gpl2+)))

(define-public clipit
  (package
    (name "clipit")
    (version "1.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CristianHenzel/ClipIt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05xi29v2y0rvb33fmvrz7r9j4l858qj7ngwd7dp4pzpkkaybjln0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake intltool pkg-config))
    (inputs
     (list gtk+-2))
    (home-page "https://github.com/CristianHenzel/ClipIt")
    (synopsis "Lightweight GTK+ clipboard manager")
    (description
     "ClipIt is a clipboard manager with features such as a history, search
thereof, global hotkeys and clipboard item actions.  It was forked from
Parcellite and adds bugfixes and features.")
    (license license:gpl2+)))

(define-public graphene
  (package
    (name "graphene")
    (version "1.10.8")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/ebassi/graphene.git")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18a0icpr003y0bh1kghgh2nqrm8a8s5lak8lbnc9wdfkc760vlw8"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dinstalled_tests=false"
        ;; Armhf with neon in graphene segfaulting is a known issue.
        ;; https://github.com/ebassi/graphene/issues/215
        ,@(if (target-arm32?)
              '("-Darm_neon=false")
              '())
        ,@(if (%current-target-system)
              ;; Introspection requires running binaries for 'host' on 'build'.
              '("-Dintrospection=disabled")
              '()))))
    (native-inputs
     `(("git" ,git-minimal/pinned)
       ("gobject-introspection" ,gobject-introspection)
       ("mutest" ,mutest)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list glib python))
    (synopsis "Thin layer of graphic data types")
    (description "Graphene provides graphic types and their relative API; it
does not deal with windowing system surfaces, drawing, scene graphs, or input.")
    (home-page "https://ebassi.github.io/graphene/")
    (license license:expat)))

(define-public spread-sheet-widget
  (package
    (name "spread-sheet-widget")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://alpha.gnu.org/gnu/ssw/"
                           "spread-sheet-widget-" version ".tar.gz"))
       (sha256
        (base32 "0jwmx5i02jwmkp6gci2mapqglh2g3a0092wns185hfygiwlxi2c5"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--disable-static")))
    (native-inputs
     (list `(,glib "bin")               ; for glib-genmarshal, etc.
           pkg-config))
    ;; In 'Requires' of spread-sheet-widget.pc.
    (propagated-inputs
     (list glib gtk+))
    (home-page "https://www.gnu.org/software/ssw/")
    (synopsis "Gtk+ widget for dealing with 2-D tabular data")
    (description
     "GNU Spread Sheet Widget is a library for Gtk+ which provides a widget for
viewing and manipulating 2 dimensional tabular data in a manner similar to many
popular spread sheet programs.")
    (license license:gpl3+)))

(define-public pnmixer
  (package
    (name "pnmixer")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nicklan/pnmixer/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0416pa933ddf4b7ph9zxhk5jppkk7ppcq1aqph6xsrfnka4yb148"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ;no check target
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list alsa-lib glib gtk+ libnotify libx11))
    (home-page "https://github.com/nicklan/pnmixer/")
    (synopsis "Simple mixer application designed to run in system tray")
    (description
     "PNMixer is a simple mixer application designed to run in system tray.
It integrates nicely into desktop environments that don't have a panel that
supports applets and therefore can't run a mixer applet.  In particular, it's
been used quite a lot with fbpanel and tint2 but should run fine in any system
tray.

PNMixer is designed to work on systems that use ALSA for sound management.
Any other sound driver like OSS or FFADO are, currently, not supported.  There
is no official PulseAudio support, at the moment, but it seems that PNMixer
behaves quite well anyway when PA is running.")
    (license license:gpl3)))

(define-public volumeicon
  (package
    (name "volumeicon")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://nullwise.com/files/volumeicon/volumeicon-"
                           version ".tar.gz"))
       (sha256
        (base32 "182xl2w8syv6ky2h2bc9imc6ap8pzh0p7rp63hh8nw0xm38c3f14"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-notify")))       ; optional libnotify support
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list alsa-lib gtk+ libnotify))
    (home-page "http://nullwise.com/volumeicon.html")
    (synopsis "System tray volume applet")
    (description
     "Volume Icon is a volume indicator and control applet for @acronym{the
Advanced Linux Sound Architecture, ALSA}.  It sits in the system tray,
independent of your desktop environment, and supports global key bindings.")
    (license (list license:expat        ; src/{bind.c,keybinder.h}
                   license:isc          ; src/alsa_volume_mapping.c
                   license:gpl3))))     ; the rest & combined work

(define-public yad
  (package
    (name "yad")
    (version "12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/v1cont/yad")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nbbw4vwlxjlp83d35w54952b6rrn8qlr3d053lisqwl0hfcm7if"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       ;; Passing --enable-foo will silently disable foo if prerequisite
       ;; inputs are missing, not abort the build as one might expect.
       ;; ‘--enable-html’ adds a huge webkitgtk dependency.  It was never
       ;; present in the past and nobody complained.
       '("--enable-icon-browser"
         "--enable-spell")              ; gspell checking support
       #:phases
       (modify-phases %standard-phases
         (add-after 'bootstrap 'intltoolize
           (lambda _
             (invoke "intltoolize" "--force" "--automake"))))))
    (inputs
     (list gspell gtk+))
    (native-inputs
     (list autoconf automake intltool pkg-config))
    (home-page "https://sourceforge.net/projects/yad-dialog/")
    (synopsis "GTK+ dialog boxes for shell scripts")
    (description
     "This program allows you to display GTK+ dialog boxes from command line or
shell scripts.  Example of how to use @code{yad} can be consulted at
@url{https://sourceforge.net/p/yad-dialog/wiki/browse_pages/}.")
    (license license:gpl3+)))

(define-public dragon-drop
  (package
   (name "dragon-drop")
   (version "1.2.0")
   (source (origin
             (method git-fetch)
             (uri
              (git-reference
               (url "https://github.com/mwh/dragon")
               (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1cspkrr976mqi6cjkgrckdmjbgahnxmq267wn1zd7gamv64vm8f2"))))
   (build-system gnu-build-system)
   (inputs (list gtk+))
   (native-inputs (list pkg-config))
   (arguments
    (list
     #:tests? #f                        ; no check target
     #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                          (string-append "PREFIX=" #$output))
     #:phases
     #~(modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
   (synopsis "Drag and drop source/target for X")
   (description
    "Dragon is a lightweight drag-and-drop source for X where you can run:

@example
dragon file.tar.gz
@end example

to get a window with just that file in it, ready to be dragged where you need it.
What if you need to drag into something? Using:

@example
dragon --target
@end example

you get a window you can drag files and text into.  Dropped items are
printed to standard output.")
   (home-page "https://github.com/mwh/dragon")
   (license license:gpl3+)))

(define-public libdbusmenu
  (let ((bzr-revision "496"))
    (package
      (name "libdbusmenu")
      (version (string-append "16.04.0" "-" bzr-revision))
      (source
       (origin
         (method bzr-fetch)
         (uri (bzr-reference
               (url "lp:libdbusmenu")
               (revision bzr-revision)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1rnp86r8f2xjcbk6jjl6np1qdhc3d7fj1c3ggn0gbv2kksc8r1bx"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "--sysconfdir=/etc"
                "--localstatedir=/var"
                ;; The shebang of the generated test files should be patched
                ;; before enabling tests.
                "--disable-tests"
                "--disable-dumper")
        #:make-flags
        #~(list (string-append "typelibdir=" #$output "/lib/girepository-1.0"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'remove-deprecated-gnome-common-macros
              (lambda _
                ;; Adapted from a Debian patch to remove deprecated macros.
                (substitute* "autogen.sh"
                  (("^USE_GNOME2_MACROS.*") "")
                  (("^USE_COMMON_DOC_BUILD.*") ""))))
            (add-after 'unpack 'patch-paths
              (lambda _
                (substitute* "libdbusmenu-glib/Makefile.am"
                  (("/bin/false")
                   "false")
                  ;; (("\\$\\(srcdir)/clean-namespaces.xslt")
                  ;;  "clean-namespaces.xslt")
                  )))
            (add-before 'configure 'do-not-treat-warnings-as-errors
              (lambda _
                ;; Prevent the build from failing due to deprecation warnings
                ;; from newer GLib and GTK versions.
                (substitute* (find-files "." "^Makefile.in$")
                  ((" -Werror")
                   ""))))
            (add-before 'configure 'set-environment
              (lambda _
                (setenv "HAVE_VALGRIND_TRUE" "")
                (setenv "HAVE_VALGRIND_FALSE" "#"))))))
      (inputs
       (list glib
             gtk+))
      (native-inputs
       (list autoconf
             automake
             `(,glib "bin")
             gobject-introspection
             gnome-common
             gtk-doc                    ;FIXME: propagate by gnome-common?
             intltool
             json-glib
             libtool
             libxslt
             pkg-config
             python-wrapper
             which
             vala))
      (home-page "https://launchpad.net/libdbusmenupython")
      (synopsis "Library for passing menus over DBus")
      (description "@code{libdbusmenu} passes a menu structure across DBus so
that a program can create a menu simply without worrying about how it is
displayed on the other side of the bus.")

      ;; Dual-licensed under either LGPLv2.1 or LGPLv3.
      (license (list license:lgpl2.1 license:lgpl3)))))

(define-public gtk-layer-shell
  (package
    (name "gtk-layer-shell")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wmww/gtk-layer-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05p9pmgzn8wacvlp7v4f1d8qqdnhr4l6jyclzfijnh1rsva0857n"))))
    (build-system meson-build-system)
    (arguments `(#:configure-flags (list "-Dtests=true")))
    (native-inputs (list pkg-config gobject-introspection vala))
    (inputs (list wayland gtk+))
    (home-page "https://github.com/wmww/gtk-layer-shell")
    (synopsis "Library to create Wayland desktop components using the Layer
Shell protocol")
    (description "Layer Shell is a Wayland protocol for desktop shell
components, such as panels, notifications and wallpapers.  It can be used to
anchor windows to a corner or edge of the output, or stretch them across the
entire output.  It supports all Layer Shell features including popups and
popovers.")
    (license license:expat)))

(define-public goocanvas
  (package
    (name "goocanvas")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/goocanvas/"
                           (version-major+minor version)
                           "/goocanvas-" version ".tar.xz"))
       (sha256
        (base32 "141fm7mbqib0011zmkv3g8vxcjwa7hypmq71ahdyhnj2sjvy4a67"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           gtk-doc/stable
           pkg-config
           python))
    (inputs
     (list cairo glib gtk+ python-pygobject))
    (arguments
     `(#:configure-flags '("--disable-rebuilds"
                           "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "configure"
               (("\\(gi._overridesdir\\)")
                (string-append "((gi._overridesdir).replace(\\\""
                               (assoc-ref inputs "python-pygobject")
                               "\\\", \\\""
                               (assoc-ref outputs "out")
                               "\\\"))"))))))))
    (synopsis "Canvas widget for GTK+")
    (description "GooCanvas is a canvas widget for GTK+ that uses the cairo 2D
library for drawing.")
    (home-page "https://wiki.gnome.org/GooCanvas")
    (license license:lgpl2.0)))

(define-public gtksheet
  (package
    (name "gtksheet")
    (version "4.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fpaquet/gtksheet")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13jwr1vly4ga3f09dajwky1cdrz5bmggwga3vnnd6j6zzia7dpyr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-glade"
                               "--enable-introspection"
                               "CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         ;; The "configure" script is present, but otherwise the project is
         ;; not bootstrapped properly. Delete configure so the bootstrap phase
         ;; will take over.
         (add-after 'unpack 'delete-configure
           (lambda _
             (delete-file "configure")
             #t))
         (add-after 'unpack 'patch-for-compatibility
           (lambda _
             (substitute* "glade/glade-gtksheet-editor.c"
               (("GladeEditableIface") "GladeEditableInterface"))
             ;; Glade 3.37 renamed the macro GWA_GET_CLASS to
             ;; GLADE_WIDGET_ADAPTOR_GET_ADAPTOR_CLASS.
             (substitute* "glade/glade-gtksheet-editor.c"
               (("GWA_GET_CLASS") "GLADE_WIDGET_ADAPTOR_GET_ADAPTOR_CLASS"))))
         ;; Fix glade install directories.
         (add-before 'bootstrap 'configure-glade-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure.ac"
               (("`\\$PKG_CONFIG --variable=catalogdir gladeui-2.0`")
                (string-append (assoc-ref outputs "out") "/share/glade/catalogs"))
               (("`\\$PKG_CONFIG --variable=moduledir gladeui-2.0`")
                (string-append (assoc-ref outputs "out") "/lib/glade/modules"))
               (("`\\$PKG_CONFIG --variable=pixmapdir gladeui-2.0`")
                (string-append (assoc-ref outputs "out") "/share/pixmaps")))
             #t)))))
    (inputs
     (list glade3 glib gtk+ libxml2))
    (native-inputs
     (list autoconf automake gobject-introspection libtool pkg-config))
    (home-page "https://fpaquet.github.io/gtksheet/")
    (synopsis "Spreadsheet widget for GTK+")
    (description "GtkSheet is a matrix widget for GTK+.  It consists of an
scrollable grid of cells where you can allocate text.  Cell contents can be
edited interactively through a specially designed entry, GtkItemEntry.  It is
also a container subclass, allowing you to display buttons, images and any
other widget in it.  You can also set many attributes such as border,
foreground and background colors, text justification and more.")
    (native-search-paths
     (list
      (search-path-specification
       (variable "GLADE_CATALOG_SEARCH_PATH")
       (files '("share/glade/catalogs")))
      (search-path-specification
       (variable "GLADE_MODULE_SEARCH_PATH")
       (files '("lib/glade/modules")))))
    (license license:lgpl2.0+)))

(define-public gtkdatabox
  (package
    (name "gtkdatabox")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gtkdatabox/gtkdatabox-1/"
                           "gtkdatabox-" version ".tar.gz"))
       (sha256
        (base32 "1qykm551bx8j8pfgxs60l2vhpi8lv4r8va69zvn2594lchh71vlb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+))
    (synopsis "Display widget for dynamic data")
    (description "GtkDatabox is a widget for live display of large amounts of
fluctuating numerical data.  It enables data presentation (for example, on
linear or logarithmic scales, as dots or lines, with markers/labels) as well as
user interaction (e.g.  measuring distances).")
    (home-page "https://sourceforge.net/projects/gtkdatabox/")
    (license license:lgpl2.1+)))

(define-public volctl
  (package
    (name "volctl")
    (version "0.9.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/buzz/volctl")
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0anrwz8rvbliskmcgpw2zabgjj5c72hpi7cf0jg05vvmlpnbsd4g"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "volctl/xwrappers.py"
               (("libXfixes.so")
                (string-append (search-input-file inputs
                                                  "/lib/libXfixes.so")))))))))
    (inputs
     (list libxfixes))
    (propagated-inputs
     (list python-click
           python-pycairo
           python-pygobject
           python-pyyaml
           python-pulsectl
           gtk+))
    (home-page "https://buzz.github.io/volctl/")
    (synopsis "Per-application volume control and on-screen display")
    (description "Volctl is a PulseAudio-enabled tray icon volume control and
OSD applet for graphical desktops.  It's not meant to be an replacement for a
full-featured mixer application.  If you're looking for that check out the
excellent pavucontrol.")

    ;; XXX: 'setup.py' says "GPLv2" but nothing says "version 2 only".  Is
    ;; GPLv2+ intended?
    (license license:gpl2)))

(define-public gromit-mpx
  (package
    (name "gromit-mpx")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bk138/gromit-mpx.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12ginc9rpn66g1n0w5lxxfzyq4zlpvrpp8a87pvr8zkgcrbkhz4c"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; No tests.
    (native-inputs (list pkg-config gettext-minimal))
    (inputs (list gtk+ libappindicator lz4))
    (home-page "https://github.com/bk138/gromit-mpx")
    (synopsis "On-screen annotation tool")
    (description
     "Gromit-MPX is an on-screen annotation tool that works with any
Unix desktop environment under X11 as well as Wayland.")
    (license license:gpl2+)))

(define-public webp-pixbuf-loader
  (package
    (name "webp-pixbuf-loader")
    (version "0.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aruiz/webp-pixbuf-loader")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cp51qv8mjjhs3x40d1p31c5f2ilajs4m15da5z5l240bmwk1490"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dgdk_pixbuf_moduledir="
                             #$output "/lib/gdk-pixbuf-2.0/2.10.0/loaders"))))
    (inputs (list gdk-pixbuf glib gtk+ libwebp))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/aruiz/webp-pixbuf-loader")
    (synopsis "WebP GdkPixbuf loader library")
    (description "Webp-pixbuf-loader is a WebP format loader of GdkPixbuf.")
    (license license:lgpl2.0+)))

(define-public libpanel
  (package
    (name "libpanel")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/libpanel")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09lgzjydnahi14xn5rchb91d6m3idzgb68xm3lklwnkdda7dz0w8"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (add-before 'build 'set-home
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list gi-docgen
           `(,glib "bin")
           gobject-introspection
           pkg-config
           vala))
    (inputs (list glib gtk libadwaita))
    (home-page "https://gitlab.gnome.org/GNOME/libpanel")
    (synopsis "Dock and panel library for GTK 4")
    (description "Libpanel provides a library to create IDE-like applications
using GTK and @code{libadwaita}.  It has widgets for panels, docks, columns
and grids of pages.  Primarily, its design and implementation focus around the
GNOME Builder and Drafting projects.")
    (license license:lgpl3)))
