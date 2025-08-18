;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Sughosha <Sughosha@disroot.org>
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

(define-module (gnu packages mruby-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public mruby-zest
  (package
    (name "mruby-zest")
    (version "3.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mruby-zest/mruby-zest-build")
                    (commit version)
                    ;; Clone recursively for the mruby sources and gems that
                    ;; are needed for compilation.  The rest of the bundled
                    ;; dependencies will be deleted in the snippet.
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dz4zv1km9805lji2q2qqdd8s8hgfd723dxdzcivbhm612szm1mc"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; This package contains a custom "pugl".  Replacing it with
                  ;; the system "pugl" fails to build.
                  ;; Delete the bundled "libuv", "nanovg" and "rtosc".
                  (with-directory-excursion "deps"
                    (for-each delete-file-recursively
                     (list "libuv" "nanovg" "rtosc")))
                  ;; Do not compile "nanovg" and "libuv" and replace them with
                  ;; the respective system libraries.
                  (substitute* "Makefile"
                    (("cd deps/nanovg") "#cd deps/nanovg")
                    (("\\$\\(AR\\) rc deps/libnanovg\\.a")
                     "#$(AR) rc deps/libnanovg.a")
                    (("\\./deps/libnanovg\\.a")
                     "-lnanovg")
                    (("cd deps/libuv") "#cd deps/libuv")
                    (("\\./deps/libuv/\\.libs/libuv\\.a")
                     "-luv")
                    (("\\./deps/libuv/\\.libs/libuv-win\\.a")
                     "-luv-win"))
                  ;; Do not include the bundled "nanovg" and "uv" and replace
                  ;; them with the respective system libraries.
                  (substitute* "build_config.rb"
                    (("cc\\.include_paths.*\\./deps/nanovg/.*$" all)
                     (string-append "#" all))
                    (("cc\\.include_paths.*\\./deps/libuv/.*$" all)
                     (string-append "#" all))
                    (("#\\{`pwd`\\.strip\\}/\\.\\./deps/libnanovg\\.a")
                     "-lnanovg")
                    (("#\\{`pwd`\\.strip\\}/\\.\\./deps/libuv\\.a")
                     "-luv"))
                  ;; Fix including "nanovg" headers.
                  (substitute* (find-files "deps/mruby-nanovg" "\\.(c|h)")
                    (("nanovg\\.h") "nanovg/nanovg.h")
                    (("nanovg_gl\\.h") "nanovg/nanovg_gl.h")
                    (("\"\\.\\./\\.\\./nanovg/.*/stb_image_write\\.h\"")
                     "<nanovg/stb_image_write.h>"))
                  ;; Fix including "rtosc" headers.
                  (substitute* "src/mruby-widget-lib/src/gem.c"
                    (("\"\\.\\./\\.\\./\\.\\./deps/.*/rtosc\\.h\"")
                     "<rtosc/rtosc.h>"))))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no test suite
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                    "CONFIG_SHELL=bash")
           #:modules
           `(,@%default-gnu-modules
             (srfi srfi-26))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure) ;no configure script
               (add-after 'unpack 'patch-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Patch paths to the missing fonts.
                   (substitute*
                    "src/mruby-widget-lib/mrblib/script.rb"
                     (("sans = \\[")
                      (string-append
                       "sans = [\""
                       (search-input-file inputs
                        "/share/fonts/truetype/Roboto-Regular.ttf")
                        "\", "))
                     (("bold = \\[")
                      (string-append
                       "bold = [\""
                       (search-input-file inputs
                        "/share/fonts/truetype/Roboto-Bold.ttf")
                        "\", ")))
                   ;; Patch paths to the zyn-fusion files.
                   (substitute* "src/osc-bridge/src/bridge.c"
                     (("fopen\\(\\\"schema/")
                      (string-append
                       "fopen(\"" #$output "/share/zyn-fusion/schema/")))
                   (substitute* "test-libversion.c"
                     (("./libzest.so")
                      (string-append #$output "/lib/libzest.so")))
                   (substitute* "src/mruby-widget-lib/src/api.c"
                     ((", \\\"\\./qml/") ", \"../share/zyn-fusion/qml/")
                     (("\\./qml/")
                      (string-append #$output "/share/zyn-fusion/qml")))))
               ;; No install target.
               (replace 'install
                 (lambda _
                   (let* ((bin (string-append #$output "/bin"))
                          (lib (string-append #$output "/lib"))
                          (share (string-append #$output "/share"))
                          (qml (string-append share "/zyn-fusion/qml"))
                          (schema (string-append share "/zyn-fusion/schema"))
                          (completions
                           (string-append share
                                          "/bash-completion/completions")))
                     ;; zynaddsubfx looks for "zyn-fusion" instead of "zest".
                     (rename-file "zest" "zyn-fusion")
                     (install-file "zyn-fusion" bin)
                     (install-file "libzest.so" lib)
                     (for-each (cut install-file <> qml)
                               (append (find-files "src/mruby-zest/qml"
                                                   "\\.qml$")
                                       (find-files "src/mruby-zest/example"
                                                   "\\.qml$")))
                     (install-file "src/osc-bridge/schema/test.json" schema)
                     (install-file "completions/zyn-fusion" completions)))))))
    (native-inputs
     (list pkg-config ruby))
    (inputs
     (list font-google-roboto
           libuv
           libx11
           mesa
           nanovg
           rtosc))
    (home-page "https://github.com/mruby-zest/mruby-zest-build")
    (synopsis "Zyn-Fusion user interface")
    (description
     "This package provides Zyn-Fusion user interface for
@command{zynaddsubfx}.")
    (license
     (list license:lgpl2.1))))
