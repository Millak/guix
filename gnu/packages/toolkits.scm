;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2025 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020-2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Andy Tai <atai@atai.org>
;;; Copyright © 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022-2023, 2026 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2023 Simon South <simon@simonsouth.net>
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

(define-module (gnu packages toolkits)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compiler-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system waf)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public fltk-1.3
  (package
    (name "fltk")
    (version "1.3.11")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
              (url "https://github.com/fltk/fltk")
              (commit (string-append "release-" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0pnifyhhvcqfjd6iaa4m14kvfyqhjjdw0aqbcizcdhhqrl6q4pjg"))))
   (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;TODO: compile programs in "test" dir
      #:configure-flags
      #~(list "--enable-shared"
              (string-append "DSOFLAGS=-Wl,-rpath=" %output "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-makeinclude
            (lambda _
              (substitute* "makeinclude.in"
                (("/bin/sh")
                 (which "sh")))))
          (add-after 'install 'patch-config
            ;; Provide -L flags for image libraries when querying fltk-config to
            ;; avoid propagating inputs.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((conf (string-append #$output "/bin/fltk-config"))
                    (libjpeg-so (search-input-file inputs "lib/libjpeg.so"))
                    (libpng-so (search-input-file inputs "lib/libpng.so"))
                    (libz-so (search-input-file inputs "lib/libz.so")))
                (substitute* conf
                  (("-ljpeg")
                   (string-append "-L" (dirname libjpeg-so) " -ljpeg"))
                  (("-lpng")
                   (string-append "-L" (dirname libpng-so) " -lpng"))
                  (("-lz")
                   (string-append "-L" (dirname libz-so) " -lz")))))))))
    (native-inputs (list autoconf automake pkg-config))
    (inputs
     (list libjpeg-turbo
           libpng
           libx11
           libxft
           mesa
           zlib))
    (home-page "https://www.fltk.org")
    (synopsis "3D C++ GUI library")
    (description
     "FLTK is a C++ GUI toolkit providing modern GUI functionality without the
bloat.  It supports 3D graphics via OpenGL and its built-in GLUT emulation.
FLTK is designed to be small and modular enough to be statically linked, but
works fine as a shared library.  FLTK also includes an excellent UI builder
called FLUID that can be used to create applications in minutes.")
    (license license:lgpl2.0))) ; plus certain additional permissions

(define-public fltk
  (package
    (inherit fltk-1.3)
    (version "1.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fltk/fltk")
             (commit (string-append "release-" version))))
       (sha256
        (base32 "19bl0ryarg31wkhw01ryilp9iwwh6vwnsq9mnzmarzm0dkwhjc7c"))
       (modules '((guix build utils)))
       (snippet #~(for-each delete-file-recursively '("jpeg" "zlib" "png")))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; The tests are GUI programs that need manual verification.
      #:tests? #f
      #:configure-flags
      #~(list "-DFLTK_BUILD_SHARED_LIBS=ON"
              "-DFLTK_BUILD_TEST=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-config
            ;; Avoid propagating inputs to consumers that rely on fltk-config.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (string-append #$output "/bin/fltk-config")
                ;; Take care to patch "-lGLU" before "-lGL".
                (("-lGLU") (search-input-file inputs "lib/libGLU.so"))
                (("-lGL") (search-input-file inputs "lib/libGL.so")))))
          (add-after 'install 'delete-static-libraries
            (lambda _
              (for-each delete-file
                        (find-files (string-append #$output "/lib")
                                    "\\.a$")))))))
    (native-inputs (list))
    (inputs
     (list glu
           libjpeg-turbo
           libpng
           libx11
           libxft
           mesa
           zlib))))

(define-public imgui
  (package
    (name "imgui")
    (version "1.92.9b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocornut/imgui")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m35zy4j1m7pvlymxhmg8gcll58acil0il1x79iyyakksylvwd92"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove bundled fonts.
               #~(for-each delete-file
                           (find-files "misc/fonts" "\\.ttf")))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26))
      ;; The build phase does not use make but we will use make-flags in a
      ;; similar fashion to make inheritance for older imgui versions easier.
      #:make-flags
      ;; This first option is necessary at least for OpenBoardView, otherwise
      ;; it would fail with the "Too many vertices in ImDrawList using 16-bit
      ;; indices".
      #~(list "-DImDrawIdx=unsigned int"
              #$@(if (this-package-input "freetype")
                     #~("-DIMGUI_ENABLE_FREETYPE"
                        "-I" (search-input-directory %build-inputs
                                                     "include/freetype2")
                        "-lfreetype")
                     #~())
              #$@(if (this-package-input "glfw")
                     #~("-lglfw")
                     #~())
              #$@(if (this-package-input "sdl2")
                     #~("-lSDL2")
                     #~())
              #$@(if (this-package-input "sdl3")
                     #~("-lSDL3")
                     #~())
              "-I" (string-append (getcwd) "/source")
              "-g" "-O2" "-fPIC" "-shared"
              "-lGL"
              "-o" "libimgui.so")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              ;; Build main library.
              (define (has-lib? soname)
                (false-if-exception
                 (search-input-file inputs (string-append "lib/" soname))))
              (define freetype?
                (has-lib? "libfreetype.so"))
              (define glfw?
                (has-lib? "libglfw.so"))
              (define sdl2?
                (has-lib? "libSDL2.so"))
              (define sdl3?
                (has-lib? "libSDL3.so"))
              (unless freetype?
                (delete-file-recursively "misc/freetype"))
              (unless sdl3?
                (substitute* (find-files "." "(\\.cpp|\\.mm)$")
                  (("#include <SDL")
                   "#include <SDL2/SDL")))
              (define args
                (append make-flags
                        `("imgui.cpp"
                          "imgui_draw.cpp"
                          "imgui_tables.cpp"
                          "imgui_widgets.cpp"
                          ;; Include the supported backends.
                          ,@(if glfw?
                                '("backends/imgui_impl_glfw.cpp")
                                '())
                          ,@(if sdl3?
                                '("backends/imgui_impl_sdl3.cpp")
                                '())
                          ,@(if sdl2?
                                '("backends/imgui_impl_sdl2.cpp")
                                '())
                          "backends/imgui_impl_opengl2.cpp"
                          "backends/imgui_impl_opengl3.cpp"
                          ;; Include wrappers for C++ standard library
                          ;; (STL) and fontconfig.
                          ,@(find-files "misc" "\\.cpp$"))))
              (format #t "invoking ~s with arguments: ~s~%"
                      #$(cc-for-target) args)
              (apply invoke #$(cc-for-target) args)))
          (replace 'install
            (lambda _
              (let* ((header? (cut string-suffix? ".h" <>))
                     (imgui-headers (scandir "." header?))
                     (backend-headers (find-files
                                       "backends"
                                       "(glfw|opengl|sdl|vulkan).*\\.h$"))
                     (misc-headers (find-files "misc" "\\.h$")))
                (install-file "libimgui.so" (string-append #$output "/lib"))
                ;; Install headers.
                (for-each (lambda (f)
                            (install-file f (string-append #$output
                                                           "/include/imgui")))
                          imgui-headers)
                (for-each (lambda (f)
                            (install-file f (string-append
                                             #$output
                                             "/include/imgui/backends")))
                          backend-headers)
                (for-each (lambda (f)
                            (install-file f (string-append #$output
                                                           "/include/imgui/"
                                                           (dirname f))))
                          misc-headers)
                ;; Install examples.
                (copy-recursively "examples"
                                  (string-append #$output:doc
                                                 "/share/imgui/examples"))))))))
    (inputs (list fontconfig freetype glfw mesa sdl3))
    (home-page "https://github.com/ocornut/imgui")
    (synopsis "Immediate-mode C++ GUI library with minimal dependencies")
    (description "@code{dear imgui} (also know as ImGui) is a graphical user
interface library for C++.  It creates optimized vertex buffers that you can
render anytime in your 3D-pipeline-enabled application.  It's fast, portable,
renderer-agnostic, and self-contained, without external dependencies.

ImGui is aimed at content creation, visualization, and debugging tools as
opposed to average end-user interfaces.  Hence it favors simplicity and
productivity but lacks certain features often found in higher-level libraries.
It is particularly suited to integration in game engine tooling, real-time 3D
applications, full-screen applications, and embedded platforms without
standard operating system features.")
    (license license:expat)))

(define-public imgui-with-sdl2
  (package/inherit imgui
    (name "imgui-with-sdl2")
    (inputs (modify-inputs inputs
              (replace "sdl3" sdl2)))))

(define-public imgui-1.91
  (package
    (inherit imgui)
    (name "imgui")
    (version "1.91.9b")
    (source (origin
              (inherit (package-source imgui))
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocornut/imgui")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ji0fw0xb01c60hqzmvqi0ca5cnbm94x4v371f3w03q7zl6a8jvn"))))))

;;; A more minimal variant of imgui that only depends on mesa, avoiding
;;; transitively depending on libx11 and is thus less likely to run into
;;; problems in FHS environments.
(define-public imgui-for-mangohud
  (package
    (inherit imgui-1.91)
    (arguments
     (substitute-keyword-arguments arguments
       ((#:disallowed-references _ ''())
        (list libx11))
       ((#:make-flags flags ''())
        ;; Remove the "-DImDrawIdx=unsigned int" flag as it corrupts the
        ;; rendering of MangoHud.
        #~(delete "-DImDrawIdx=unsigned int" #$flags))))
    (inputs (list mesa))))

(define-public motif
  ;; This commit is from February 2023 and v2.3.8 from 2017.
  (let ((commit "0f556b0873c72ba1152a12fd54c3198ee039e413")
        (revision "1"))
    (package
      (name "motif")
      (version (git-version "2.3.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://git.code.sf.net/p/motif/code")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1g28i4978p9dpcdxv2yx4m17bchdypm3yy6m6gzchdlrv2iklhl9"))))
      (build-system gnu-build-system)
      (inputs
       (list libx11 xorgproto))
      (propagated-inputs
       (list fontconfig freetype libxext libxft libxpm libxt xbitmaps))
      (native-inputs
       ;; This fails to build with GCC 14 due to missing header includes and
       ;; other C semantics issues.
       (list gcc-11
             autoconf
             automake
             byacc
             flex
             libtool
             pkg-config))
      (home-page "https://motif.ics.com/motif")
      (synopsis "Toolkit for the X window system")
      (description
       "Motif is a standard graphical user interface (as defined by the IEEE
1295 specification), used on more than 200 hardware and software platforms.
It provides application developers, end users, and system vendors with a
widely used environment for standardizing application presentation on a wide
range of platforms.")
      (license license:lgpl2.1+))))

(define-public ntk
  (package
    (name "ntk")
    (version "1.3.1000")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "git://git.tuxfamily.org/gitroot/non/fltk.git")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "0j38mhnfqy6swcrnc5zxcwlqi8b1pgklyghxk6qs1lf4japv2zc0"))
              (file-name (git-file-name name version))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:configure-flags '("--enable-gl")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setup-waf
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((waf (assoc-ref inputs "waf")))
               (delete-file "waf")
               (copy-file (string-append waf "/bin/waf") "waf"))
             #t))
         (add-before 'configure 'set-ldflags
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "LDFLAGS"
                     (string-append "-Wl,-rpath="
                                    (assoc-ref outputs "out") "/lib"))
             #t)))))
    (inputs
     `(("libjpeg" ,libjpeg-turbo)
       ("glu" ,glu)
       ("waf" ,waf)))
    ;; ntk.pc lists "x11" and "xft" in Requires.private, and "cairo" in
    ;; Requires.
    (propagated-inputs
     (list cairo libxft libx11))
    (native-inputs
     (list pkg-config))
    (home-page "https://non.tuxfamily.org/ntk/")
    (synopsis "Fork of FLTK with graphics rendering via Cairo")
    (description
     "The Non Tool Kit (NTK) is a fork of the Fast Light ToolKit library,
adding improved graphics rendering via Cairo, a streamlined and enhanced
widget set, and other features designed to improve the appearance and
performance of the Non applications.")
    (license license:lgpl2.0+))) ; plus certain additional permissions

(define-public nuklear
  (package
    (name "nuklear")
    (version "4.12.0")
    (home-page "https://github.com/Immediate-Mode-UI/Nuklear")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13cswwdys4hqdvbm4g4b9l269i16s7c4204j16v67ghj3b4mjifg"))
       (snippet #~(begin
                    (delete-file "nuklear.h")
                    (delete-file "doc/index.html")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (with-directory-excursion "src"
                (invoke "./paq.sh"))
              (with-directory-excursion "doc"
                (with-input-from-file "../nuklear.h"
                  (lambda _
                    (with-output-to-file "index.html"
                      (lambda _
                        (invoke "stddoc"))))))))
          (replace 'install
            (lambda _
              (install-file "nuklear.h"
                            (string-append #$output "/include"))
              (install-file "doc/index.html"
                            (string-append #$output "/share/doc")))))))
    (native-inputs (list stddoc python-setuptools))
    (synopsis "Graphical user interface toolkit written in ANSI C")
    (description
     "This package provides an immediate-mode graphical user interface
toolkit.  It was designed as an embeddable user interface for applications and
does not have any dependencies, a default render backend or OS window/input
handling.  The library is self contained in one single header file and can be
used either in header only mode or in implementation mode.")
    (license (list license:unlicense license:expat))))

(define-public implot
  (package
    (name "implot")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/epezent/implot")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pcq6nn82qpkwk27kyiaxky4pnaz9kj3fc3q25laj154r83ppikl"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (delete 'configure)
          (replace 'build
            ;; Build a shared library from the source code.
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke #$(cc-for-target) "-I" (getcwd)
                      "-I" (search-input-directory inputs "include/imgui")
                      "-g" "-O2" "-fPIC" "-shared"
                      "-o" "libimplot.so"
                      "implot.cpp"
                      "implot_items.cpp")))
          (replace 'install
            ;; Copy the library and header files to the output.
            (lambda _
              (install-file "libimplot.so" (string-append #$output "/lib"))
              (for-each (lambda (file-name)
                          (install-file file-name
                                        (string-append #$output
                                                       "/include/implot")))
                        '("implot.h" "implot_internal.h")))))
      #:tests? #f))                     ; no test suite
    (inputs (list imgui))
    (home-page "https://github.com/epezent/implot")
    (synopsis "Immediate-mode C++ plotting library for ImGui")
    (description "ImPlot is an immediate-mode, GPU-accelerated plotting
library in C++ for the ImGui GUI library, suitable for creating real-time
visualizations or interactive plots.  Like ImGui, ImPlot is self-contained and
requires only minimal code to integrate with existing applications.")
    (license license:expat)))

(define-public implot-0
  (package
    (inherit implot)
    (name "implot")
    (version "0.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/epezent/implot")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rp4lxgr11yc3z3cmlrpyaf916n74wp7401r21v7xbybfd2cvp0w"))))))

(define-public implot-for-mangohud
  (package
    (inherit implot-0)
    (inputs (modify-inputs inputs
              (replace "imgui" imgui-for-mangohud)))))
