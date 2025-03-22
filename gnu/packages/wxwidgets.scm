;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016, 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2023 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 Ekaitz Zarraga <ekaitz@elenq.tech>
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

(define-module (gnu packages wxwidgets)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public wxwidgets
  (package
    (name "wxwidgets")
    (version "3.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/wxWidgets/wxWidgets/"
                           "releases/download/v" version
                           "/wxWidgets-" version ".tar.bz2"))
       (sha256
        (base32 "1ik8x3708c8wg6agfb3qj0lb1yp097gp4avlkqfcv4adhwpp58b9"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        '(begin
           ;; wxWidgets bundles third-party code in the "3rdparty" directory as
           ;; well as the "src" directory.  Remove external components that are
           ;; not required.
           (let ((preserved-3rdparty '("nanosvg"))
                 ;; The src directory contains a mixture of third party libraries
                 ;; and similarly-named integration code.  Cautiously use a
                 ;; blacklist approach here.
                 (bundled-src '("expat" "jpeg" "png" "tiff" "zlib")))
             (with-directory-excursion "3rdparty"
               (for-each delete-file-recursively
                         (scandir "." (negate (cut member <>
                                                   (append '("." "..")
                                                           preserved-3rdparty))))))
             (with-directory-excursion "src"
               (for-each delete-file-recursively bundled-src)))))))
    (outputs '("out" "debug"))
    (build-system glib-or-gtk-build-system)
    (inputs
     (list catch-framework
           curl
           expat
           glu
           gstreamer
           gst-plugins-base
           gtk+
           libjpeg-turbo
           libmspack
           libnotify
           libpng
           libsecret
           libsm
           libtiff
           libxtst                      ;for wxUIActionSimulator
           mesa
           pcre2
           sdl2
           shared-mime-info
           webkitgtk-with-libsoup2
           xdg-utils
           zlib))
    (native-inputs
     (list pkg-config))
    (arguments
     (list
      #:configure-flags #~'("--with-libmspack"
                            "--with-regex"
                            "--with-sdl"
                            "--enable-debug_info"
                            "--enable-gui"
                            "--enable-mediactrl"
                            "--enable-webview")
      #:make-flags
      #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
      #:tests? #f                       ;TODO
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'refer-to-inputs
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((catch (search-input-file inputs "include/catch.hpp"))
                    (mime (search-input-directory inputs "share/mime"))
                    (xdg-open (search-input-file inputs "bin/xdg-open")))
                (install-file catch "3rdparty/catch/include/")
                (substitute* "src/unix/utilsx11.cpp"
                  (("wxExecute\\(xdg_open \\+")
                   (string-append "wxExecute(\"" xdg-open "\"")))
                (substitute* "src/unix/mimetype.cpp"
                  (("/usr(/local)?/share/mime")
                   mime)))))
          (replace 'configure
            (lambda* (#:key native-inputs inputs configure-flags
                      #:allow-other-keys)
              (let ((sh (search-input-file (or native-inputs inputs)
                                           "bin/sh")))
                ;; The configure script does not understand some of the default
                ;; options of gnu-build-system, so run it "by hand".
                (apply invoke "./configure"
                       (string-append "SHELL=" sh)
                       (string-append "CONFIG_SHELL=" sh)
                       (string-append "--prefix=" #$output)
                       configure-flags)))))))
    (home-page "https://www.wxwidgets.org/")
    (synopsis "Widget toolkit for creating graphical user interfaces")
    (description
     "wxWidgets is a C++ library that lets developers create applications with
a graphical user interface.  It has language bindings for Python, Perl, Ruby
and many other languages.")
    (license (list l:lgpl2.0+ (l:fsf-free "file://doc/license.txt")))))

(define-public wxwidgets-gtk2
  (package/inherit wxwidgets
    (name "wxwidgets-gtk2")
    (inputs (modify-inputs (package-inputs wxwidgets)
              (delete "gtk+")
              (prepend gtk+-2)))
    (arguments
     (substitute-keyword-arguments (package-arguments wxwidgets)
       ((#:configure-flags flags #~'())
        #~(append #$flags '("--with-gtk=2")))))))

(define-public wxwidgets-sans-egl
  ;; This is needed for prusaslicer:
  ;; <https://github.com/NixOS/nixpkgs/issues/193135>.)
  ;; and KiCAD:
  ;; <https://forum.kicad.info/t/kicad-8-0-x-could-not-use-opengl-debian-guix/53203/7>
  ;; Relevant debian bug report:
  ;; <https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1024147;msg=5>
  ;; Tl;dr; wxWidgets uses EGL but Glew 2.2 doesn't. Leading to errors.
  (package/inherit wxwidgets
    (name "wxwidgets-sans-egl")
    (arguments
       (substitute-keyword-arguments (package-arguments wxwidgets)
         ((#:configure-flags flags)
          #~(cons "--disable-glcanvasegl" #$flags))))))

(define-public wxwidgets-3.0
  (package
    (inherit wxwidgets)
    (version "3.0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/wxWidgets/wxWidgets/"
                                  "releases/download/v" version
                                  "/wxWidgets-" version ".tar.bz2"))
              (sha256
               (base32
                "01y89999jw5q7njrhxajincx7lydls6yq37ikazjryssrxrnw3s4"))))
    (arguments
     `(#:configure-flags
       '("--with-regex" "--with-libmspack"
         "--with-sdl"
         "--enable-webview"
         "--enable-webkit"
         "--enable-webviewwebkit"
         ,@(if (string=? "aarch64-linux"
                         (%current-system))
             '("--build=aarch64-unknown-linux-gnu")
             '()))
       #:make-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       ;; No 'check' target.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'refer-to-inputs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((mime (search-input-directory inputs "share/mime"))
                   (xdg-open (search-input-file inputs "bin/xdg-open")))
               (substitute* "src/unix/utilsx11.cpp"
                 (("wxExecute\\(xdg_open \\+")
                  (string-append "wxExecute(\"" xdg-open "\"")))
               (substitute* "src/unix/mimetype.cpp"
                 (("/usr(/local)?/share/mime") mime))))))))))

(define-public wxwidgets-gtk2-3.0
  (package/inherit wxwidgets-3.0
    (name "wxwidgets-gtk2")
    (inputs (modify-inputs (package-inputs wxwidgets-3.0)
              (delete "gtk+")
              (prepend gtk+-2)))
    (arguments
     (substitute-keyword-arguments (package-arguments wxwidgets-3.0)
       ((#:configure-flags flags #~'())
        #~(append #$flags '("--with-gtk=2")))))))

(define-public wxwidgets-2
  (package
    (inherit wxwidgets)
    (version "2.8.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/wxWidgets/wxWidgets/"
                           "releases/download/v" version
                           "/wxGTK-" version ".tar.gz"))
       (sha256
        (base32 "1gjs9vfga60mk4j4ngiwsk9h6c7j22pw26m3asxr1jwvqbr8kkqk"))))
    (inputs
     `(("gtk" ,gtk+-2)
       ("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("libmspack" ,libmspack)
       ("sdl" ,sdl)
       ("unixodbc" ,unixodbc)))
    (arguments
     `(#:configure-flags
       '("--enable-unicode" "--with-regex=sys" "--with-sdl")
       #:make-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       ;; No 'check' target.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'ignore-narrowing-errors
           (lambda _
             (substitute* "configure"
               (("-Wall") "-Wall -Wno-narrowing"))
             #t)))))))

(define-public prusa-wxwidgets
  ;; There is no proper tag/release, all patches are in separate branches based on
  ;; the wxWidgets release (e.g. this commit is taken from "v3.2.0-patched" branch".)
  (let ((commit "78aa2dc0ea7ce99dc19adc1140f74c3e2e3f3a26")
        (revision "0"))
    (package
      (inherit wxwidgets-sans-egl)
      (name "prusa-wxwidgets")
      (version (git-version "3.2.0" revision commit))
      (home-page "https://github.com/prusa3d/wxWidgets")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         ;; The patch is taken from the NixOS nixpkgs repository (see
         ;; <https://github.com/NixOS/nixpkgs/commit/0e724ac89f3dbf6ed31d647290a371b44a85e5ad>.)
         (patches (search-patches "prusa-wxwidgets-makefile-fix.patch"))
         (sha256
          (base32
           "1xk6w7q4xv4cj906xa5dwam5q51mc8bszbkkz7l8d3wjmsz73rwv"))))
      (native-inputs (modify-inputs (package-native-inputs wxwidgets)
                       (prepend nanosvg)))
      (arguments
       (substitute-keyword-arguments (package-arguments wxwidgets)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'copy-nanosvg-source
                (lambda _
                  (copy-recursively #$(package-source nanosvg) "3rdparty/nanosvg/"))))))))))

(define-public python-wxpython
  (package
    (name "python-wxpython")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wxPython" version))
       (sha256
        (base32
         "1iw6xp76b3fmdqwbqmsx9i1razzpfki5z1hq6l8mszlxa32fng36"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled wxwidgets
           (delete-file-recursively "ext/wxWidgets")))
       (patches (search-patches "python-wxwidgets-type-errors.patch"))))
    (build-system python-build-system)
    (outputs '("out" "debug"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Configure the build options provided to the 'build.py' build
              ;; script.
              (setenv "WXPYTHON_BUILD_ARGS"
                      (string-join '("--debug"        ;include debug symbols
                                     "--use_syswx"))) ;use system wxwidgets
              (setenv "WXWIN" #$(this-package-input "wxwidgets"))
              ;; Copy the waf executable to the source directory since it needs
              ;; to be in a writable directory.
              (copy-file (search-input-file inputs "/bin/waf") "bin/waf")
              (setenv "WAF" "bin/waf")
              ;; The build script tries to copy license files from the
              ;; wxwidgets source tree. Prevent it.
              (substitute* "wscript"
                (("updateLicenseFiles\\(cfg\\)" all)
                 (string-append "#" all)))
              ;; The build script tries to write to demo/version.py. So, we set
              ;; correct write permissions.
              (chmod "demo/version.py" #o644))))))
    (inputs
     (list gtk+ wxwidgets))
    (native-inputs
     (list pkg-config python-waf))
    (propagated-inputs
     (list python-attrdict3 python-numpy python-pillow python-six))
    (home-page "https://wxpython.org/")
    (synopsis "Cross platform GUI toolkit for Python")
    (description "wxPython is a cross-platform GUI toolkit for the Python
programming language.  It is implemented as a set of Python extension modules
that wrap the GUI components of the popular wxWidgets cross platform C++
library.  In most cases, wxPython uses the native widgets on each platform to
provide a 100% native look and feel for the application.")
    (license l:wxwindows3.1+)))

(define-public wxsvg
  (package
    (name "wxsvg")
    (version "1.5.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/wxsvg/wxsvg/"
                            version "/wxsvg-" version ".tar.bz2"))
       (sha256
        (base32 "10i4bv1bfbfgrrpxvfdjrr5svgn64v471lkcl2pzx9fhz28k4ixf"))))
    (build-system glib-or-gtk-build-system)
    (inputs
     (list wxwidgets cairo ffmpeg))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; In Requires.private of libwxsvg.pc.
     (list libexif pango))
    (synopsis "C++ library to create, manipulate and render SVG files")
    (description "wxSVG is a C++ library to create, manipulate and render
@dfn{Scalable Vector Graphics} (SVG) files with the wxWidgets toolkit.")
    (home-page "https://wxsvg.sourceforge.net")

    ;; wxSVG is licenced under the "wxWindows library licence", which is
    ;; the LGPL2.0+, with a few extra permissions.
    (license (list l:lgpl2.0+ (l:fsf-free "file://COPYING")))))

(define-public perl-alien-wxwidgets
  (package
    (name "perl-alien-wxwidgets")
    (version "0.69")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MD/MDOOTSON/Alien-wxWidgets-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0jg2dmkzhj03f6b0vmv597yryfw9cclsdn9ynvvlrzzgpd5lw8jk"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-lwp-protocol-https
       perl-module-build
       perl-test-pod
       perl-test-pod-coverage
       wxwidgets))
    (propagated-inputs (list perl-module-pluggable))
    (home-page "https://metacpan.org/release/Alien-wxWidgets")
    (synopsis "Perl module for wxWidgets binaries")
    (description "Alien::wxWidgets is a Perl module for detecting and
getting configuration settings from an installed wxWidgets package.")
    (license l:perl-license)))
