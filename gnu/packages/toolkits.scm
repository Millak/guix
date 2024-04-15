;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
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
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages sdl)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix git-download))

(define-public imgui
  (package
    (name "imgui")
    (version "1.89.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocornut/imgui")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0db11pin6kdzyd07dnwch8sf0z3386h42ibki1lnzb2ln8m66kyj"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove bundled fonts.
               '(delete-file-recursively "misc/fonts"))))
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
              "-DIMGUI_ENABLE_FREETYPE"
              "-I" (string-append (getcwd) "/source")
              "-I" (search-input-directory %build-inputs "include/freetype2")
              "-g" "-O2" "-fPIC" "-shared"
              "-lGL" "-lSDL2" "-lglfw"
              "-o" "libimgui.so")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-includes
            (lambda _
              (substitute* (find-files "." "(\\.cpp|\\.mm)$")
                (("#include <SDL")
                 "#include <SDL2/SDL"))))
          (delete 'configure)
          (replace 'build
            (lambda* (#:key make-flags #:allow-other-keys)
              ;; Build main library.
              (apply invoke #$(cc-for-target)
                     (append make-flags
                             `("imgui.cpp"
                               "imgui_draw.cpp"
                               "imgui_tables.cpp"
                               "imgui_widgets.cpp"
                               ;; Include the supported backends.
                               "backends/imgui_impl_glfw.cpp"
                               ,(if (file-exists? "backends/imgui_impl_sdl2.cpp")
                                    "backends/imgui_impl_sdl2.cpp"
                                    "backends/imgui_impl_sdl.cpp")
                               "backends/imgui_impl_opengl2.cpp"
                               "backends/imgui_impl_opengl3.cpp"
                               ;; Include wrappers for C++ standard library (STL) and
                               ;; fontconfig.
                               ,@(find-files "misc" "\\.cpp$"))))))
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
    (inputs (list fontconfig freetype glfw mesa sdl2))
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

(define-public imgui-1.87
  (package
    (inherit imgui)
    (name "imgui")
    (version "1.87")
    (source (origin
              (inherit (package-source imgui))
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocornut/imgui")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10qil22s5qak3as41787iz273sibpq1bq66bakgn7yvhj5fym6hz"))))))

(define-public imgui-1.86
  (package
    (inherit imgui)
    (name "imgui")
    (version "1.86")
    (source (origin
              (inherit (package-source imgui))
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocornut/imgui")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02a7b05zrka20jhzag2jb4jl624i1m456bsv69jb9zgys2p9dv1n"))))
    (arguments
     (substitute-keyword-arguments (package-arguments imgui)
       ((#:make-flags flags ''())
        ;; Remove "-DImDrawIdx=unsigned int" and "-DIMGUI_ENABLE_FREETYPE"
        ;; from make-flags as this breaks MangoHud, the only user of this
        ;; version.
        #~(filter (negate (lambda (x) (string-prefix? "-D" x)))
                  #$flags))))
    (inputs
     (modify-inputs (package-inputs imgui)
       (delete "freetype")))))

(define-public nuklear
  (package
    (name "nuklear")
    (version "4.12.0")
    (home-page "https://github.com/Immediate-Mode-UI/Nuklear")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13cswwdys4hqdvbm4g4b9l269i16s7c4204j16v67ghj3b4mjifg"))
              (snippet #~(begin (delete-file "nuklear.h")
                                (delete-file "doc/index.html")))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f ;no tests
           #:phases #~(modify-phases %standard-phases
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
                          (lambda* (#:key outputs #:allow-other-keys)
                            (install-file "nuklear.h"
                                          (string-append #$output "/include"))
                            (install-file "doc/index.html"
                                          (string-append #$output
                                                         "/share/doc")))))))
    (native-inputs (list stddoc))
    (synopsis "Graphical user interface toolkit written in ANSI C")
    (description "This package provides an immediate-mode graphical user
interface toolkit.  It was designed as an embeddable user interface
for applications and does not have any dependencies, a default render backend
or OS window/input handling.  The library is self contained in one single header
file and can be used either in header only mode or in implementation mode.")
    (license (list license:unlicense license:expat))))
