;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 L  p R n  d n <guix@lprndn.info>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2023 Ahmad Draidi <a.r.draidi@redscript.org>
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

(define-module (gnu packages arcan)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public arcan
  (package
    (name "arcan")
    (version "0.6.2.1")
    (source (origin
              (method git-fetch)
              (file-name (git-file-name name version))
              (uri (git-reference
                    (url "https://github.com/letoram/arcan")
                    (commit version)))
              (sha256
               (base32
                "14wwb7mgq8ab39dfprps7hzdz7a37r3cl8dc5q6m1r8n5daxyzgc"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove some bundled packages.
               #~(begin
                   (delete-file-recursively "external/git")
                   (delete-file-recursively "external/lua")
                   (delete-file-recursively "external/sqlite")))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~'("-DBUILD_PRESET=everything"
                                 "-DDISTR_TAG='Guix'")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-paths
                          (lambda* (#:key outputs #:allow-other-keys)
                            (substitute* "src/platform/posix/paths.c"
                              (("/usr/local")
                               (assoc-ref outputs "out")))))
                        ;; Normally, it tries to fetch patched openal with git
                        ;; but copying files manually in the right place seems
                        ;; to work too.
                        (add-after 'unpack 'prepare-static-openal
                          (lambda* (#:key inputs #:allow-other-keys)
                            (let ((arcan-openal (assoc-ref inputs
                                                           "arcan-openal")))
                              (copy-recursively arcan-openal
                                                "external/git/openal")) #t))
                        (add-after 'prepare-static-openal 'generate-man
                          (lambda _
                            (with-directory-excursion "doc"
                              (invoke "ruby" "docgen.rb" "mangen")) #t))
                        (add-before 'configure 'chdir
                          (lambda _
                            (chdir "src") #t)))
           #:tests? #f))
    (inputs `(("bash-minimal" ,bash-minimal)
              ("espeak" ,espeak)
              ("ffmpeg" ,ffmpeg)
              ("freetype" ,freetype)
              ("gumbo-parser" ,gumbo-parser)
              ("harfbuzz" ,harfbuzz)
              ("jbig2dec" ,jbig2dec)
              ("leptonica" ,leptonica)
              ("libdrm" ,libdrm)
              ("libjpeg-turbo" ,libjpeg-turbo)
              ("libseccomp" ,libseccomp)
              ("libusb" ,libusb)
              ("libvnc" ,libvnc)
              ("libxkbcommon" ,libxkbcommon)
              ("luajit" ,luajit)
              ("mupdf" ,mupdf)
              ("openal" ,openal)
              ("openjpeg" ,openjpeg)
              ("sdl2" ,sdl2)
              ("sqlite" ,sqlite)
              ("tesseract-ocr" ,tesseract-ocr)
              ("vlc" ,vlc)
              ("wayland" ,wayland)
              ("wayland-protocols" ,wayland-protocols)
              ("xcb-util" ,xcb-util)
              ("xcb-util-wm" ,xcb-util-wm)
              ("zlib" ,zlib)
              ;; To build arcan_lwa, we need a patched version of openal.
              ;; https://github.com/letoram/arcan/wiki/packaging
              ("arcan-openal"
               ,(origin
                  (method git-fetch)
                  (file-name "arcan-openal-0.6.2")
                  (uri (git-reference (url
                                       "https://github.com/letoram/openal")
                                      (commit "0.6.2")))
                  (sha256
                   (base32
                    "0vg3fda47q2dk1n43ijcc64q39z044pa8h6scmfyi22g6r6bfw2z"))))))
    (native-inputs (list pkg-config ruby)) ;For documentation and testing
    (home-page "https://arcan-fe.com")
    (synopsis "Display server, multimedia framework and game engine")
    (description
     "Arcan is a development framework for creating virtually
anything from user interfaces for specialized embedded applications
all the way to full-blown desktop environments.  At its heart lies a multimedia
engine with a Lua scripting interface.")
    ;; https://github.com/letoram/arcan/blob/master/COPYING
    (license (list license:asl2.0
                   license:bsd-3
                   license:cc-by3.0
                   license:expat
                   license:gpl2+
                   license:lgpl2.0
                   license:lgpl2.0+
                   license:public-domain
                   license:silofl1.1
                   license:zlib))))

(define-public arcan-sdl
  (package
    (inherit arcan)
    (name "arcan-sdl")
    (inputs
     (modify-inputs (package-inputs arcan)
       (delete "libdrm")
       (prepend sdl)))
    (arguments
     `(,@(ensure-keyword-arguments
          (package-arguments arcan)
          '(#:configure-flags
            '("-DCMAKE_C_FLAGS=-fcommon"
              "-DVIDEO_PLATFORM=sdl" "-DBUILTIN_LUA=off"
              "-DSTATIC_OPENAL=off" "-DDISABLE_JIT=off"
              "-DENABLE_LWA=on" "-DSTATIC_SQLITE3=off"
              "-DSTATIC_FREETYPE=off" "-DSHMIF_TUI_ACCEL=on")))))
    (synopsis "Combined display server, multimedia framework and game engine (SDL)")))

(define-public xarcan
  (package
    (name "xarcan")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name name version))
       (uri (git-reference
             (url "https://github.com/letoram/xarcan")
             (commit version)))
       (sha256
        (base32 "1z4sf101i2y6rg2vcxfwmp1nkzfa3rw1pp48ym1ds1ka513vy128"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "-Dglamor=true" "-Dint10=false"
         "-Dipv6=true"
         "-Dsystemd_logind=false"

         ;; The following arguments were taken from the Xwayland package.

         ;; The build system insist on providing a default font path; give
         ;; that of dejavu, the same used for our fontconfig package.
         (string-append "-Ddefault_font_path="
                        #$(this-package-input "font-dejavu") "/share/fonts")
         (string-append "-Dxkb_dir=" #$(this-package-input "xkeyboard-config")
                        "/share/X11/xkb")
         (string-append "-Dxkb_bin_dir=" #$(this-package-input "xkbcomp")
                        "/bin")
         (format #f "-Dbuilder_string=\"Build ID: ~a ~a\"" #$name #$version))))
    (native-inputs
     (list pkg-config autoconf automake libtool util-macros))
    (inputs
     (list arcan
           font-dejavu
           font-util
           libdrm
           libepoxy
           libtirpc
           libx11
           libxfont2
           libxkbfile
           libxshmfence
           mesa
           openssl
           pixman
           xcb-util
           xcb-util-wm
           xkbcomp
           xkeyboard-config
           xorgproto
           xtrans))
    (home-page "https://arcan-fe.com")
    (synopsis "Patched Xserver that bridges connections to Arcan")
    (description "Patched Xserver with a KDrive backend that uses the arcan-shmif
 to map Xlib/Xcb/X clients to a running arcan instance.  It allows running an X session
as a window under Arcan.")
    (license (list license:bsd-3 license:expat))))

(define-public arcan-wayland
  (package
    (inherit arcan)
    (name "arcan-wayland")
    (native-inputs
     (list pkg-config))
    (inputs
     (list arcan
           libseccomp
           libxkbcommon
           mesa
           wayland
           wayland-protocols))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "src/tools/waybridge")
             #t))
         (add-after 'unpack 'fix-cmake-find-shmif
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/platform/cmake/modules/Findarcan_shmif.cmake"
               (("/usr/local") (assoc-ref inputs "arcan")))
             #t)))))
    (synopsis "Wayland protocol service for Arcan")
    (description "Arcan-wayland (waybridge) bridges Wayland connections
with an Arcan connection point.  It allows Wayland compatible clients
to connect and render using Arcan.")
    (license license:bsd-3)))
