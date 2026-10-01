;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages fltk)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk) ;for "cairo"
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system waf)
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
    (description "The Non Tool Kit (NTK) is a fork of the Fast Light ToolKit
library, adding improved graphics rendering via Cairo, a streamlined and
enhanced widget set, and other features designed to improve the appearance and
performance of the Non applications.")
    (license license:lgpl2.0+))) ; plus certain additional permissions
