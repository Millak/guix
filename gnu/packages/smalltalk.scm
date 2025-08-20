;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2024 Jorge Acereda <jacereda@gmail.com>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages smalltalk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public pharo-vm
  (package
    (name "pharo-vm")
    ;; Use the latest release made available from
    ;; <https://files.pharo.org/vm/pharo-spur64-headless/Linux-x86_64/source/>.
    (version "10.3.5+19.5c89251")
    (source
     (origin
       (method url-fetch)
       ;; These source distributions of Pharo VM include the pre-generated C
       ;; source files that are used to bootstrap Pharo.
       (uri (string-append "https://files.pharo.org/vm/pharo-spur64-headless/"
                           "Linux-x86_64/source/PharoVM-v" version
                           "-Linux-x86_64-c-src.tar.gz"))
       (sha256
        (base32 "1w0m25x52p94zfv9gq2v011s0c260m2prpi4zlcrwyi0yxxbz27j"))
       (patches (search-patches "pharo-vm-cmake.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; The test suite requires a pre-built binary image (that it fetches
      ;; from the network), along a VM archive that is also requires
      ;; downloading online resources to be produced.
      #:tests? #f
      #:configure-flags
      #~(list "-DBUILD_BUNDLE=OFF"
              ;; Relax a warning turned error with GCC 14.
              "-DCMAKE_C_FLAGS=-Wno-error=incompatible-pointer-types"
              ;; The 'GENEERATE_SOURCES=OFF' is to tell the build system to
              ;; use the pre-generated C source files, avoiding the need for a
              ;; pharo bootstrap binary.
              "-DGENERATE_SOURCES=OFF"
              "-DGENERATED_SOURCE_DIR=."
              ;; This ensures the plugins can be found in RUNPATH.
              (string-append "-DPHARO_LIBRARY_PATH=" #$output "/lib")
              "-DVERBOSE_BUILD=ON"
              "-DVERSION_UPDATE_FROM_GIT=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-ld-library-path
            ;; The following libraries are dlopen'd.
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/pharo")
                `("LD_LIBRARY_PATH" ":" prefix

                  ,(map (lambda (name)
                          (string-append (assoc-ref inputs name)
                                         "/lib"))
                        '("cairo" "freetype" "libgit2" "pixman" "sdl2"
                          "util-linux"))))))
          (add-after 'wrap-ld-library-path 'workaround-#1674
            ;; pharo crashes when argv[0] is not an absolute file name. This
            ;; can be removed after
            ;; <https://codeberg.org/guix/guix/issues/1674> is resolved.
            (lambda _
              (substitute* (string-append #$output "/bin/pharo")
                (("\\$\\{0##\\*/}") "$0")))))))
    (inputs
     (list bash-minimal
           cairo
           freetype
           libffi
           libgit2
           libpng
           openssl
           pixman
           sdl2
           `(,util-linux "lib")))       ;for libuuid
    (synopsis "Clean and innovative Smalltalk-inspired environment")
    (home-page "https://github.com/pharo-project/pharo-vm")
    (description
     "Pharo aims to provide a clean and innovative Smalltalk-inspired
environment.  With a stable and small core system, advanced development tools,
and maintained releases, the Pharo platform can be used to build and deploy
mission critical applications.")
    ;; The "spur64" C source bootstrap is only for 64 bit platforms.  The
    ;; "spur32" variant is no longer maintained.
    (supported-systems %64bit-supported-systems)
    (license license:expat)))

(define-public smalltalk
  (package
    (name "smalltalk")
    (version "3.2.91")
    (source
     (origin
      (method url-fetch)
      ;; XXX: Revert to mirror://gnu with the next release of Smalltalk.
      (uri (string-append "https://alpha.gnu.org/gnu/smalltalk/smalltalk-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1zb2h5cbz1cwybqjl24lflw359lwj7sjvvhwb4x6miypzhwq4qh0"))
      ;; XXX: To be removed with the next release of Smalltalk.
      (patches (search-patches "smalltalk-multiplication-overflow.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           (libc-utf8-locales-for-target)
           ;; XXX: To be removed with the next release of Smalltalk.
           autoconf
           automake
           libtool
           zip))
    ;; TODO: These optional dependencies raise the closure size to ~1 GiB
    ;; from the current ~100 MiB, although some of them might be very
    ;; useful for end users:
    ;;  - freeglut
    ;;  - glib
    ;;  - gobject-introspection
    ;;  - gtk+-2
    ;;  - tcl/tk
    ;;  - SDL (sdl-union)
    ;;  - sqlite
    ;;  - zlib
    (inputs
     (list gmp libffi libltdl libsigsegv lightning))
    (arguments
     `(#:make-flags '("CFLAGS=-Wno-incompatible-pointer-types")
       ;; FIXME: Tests fail on x86-64 in the build container, but they pass
       ;; in a regular shell.
       #:tests? ,(not (target-x86-64?))
       #:phases
       (modify-phases %standard-phases
         ;; XXX: To be removed with the next release of Smalltalk.
         ;; The overflow patch modifies configure.ac, therefore remove
         ;; old configure script and enforce an autoreconf.
         (add-before 'bootstrap 'remove-unpatched-configure
           (lambda _
             (delete-file "configure")
             #t))
         ;; XXX: To be removed with the next release of Smalltalk.
         ;; We don't want to regenerate the info files.
         (add-after 'build 'keep-generated-info-manual
           (lambda _
             (for-each (lambda (file)
                         (invoke "touch" file))
                       (find-files "doc" "\\.info"))
             #t))
         (add-before 'configure 'fix-libc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libc (or (assoc-ref inputs "libc")
                             ;; When cross-compiling, the input
                             ;; is named "cross-libc" instead of
                             ;; simply "libc".
                             (assoc-ref inputs "cross-libc"))))
               (substitute* "libc.la.in"
                 (("@LIBC_SO_NAME@") "libc.so")
                 (("@LIBC_SO_DIR@")  (string-append libc "/lib"))))
             #t)))))
    (home-page "https://smalltalk.gnu.org/")
    (synopsis "Programming language environment")
    (description
     "GNU Smalltalk is a free implementation of the Smalltalk language.  It
implements the ANSI standard for the language and also includes extra classes
such as ones for networking and GUI programming.")
    (license license:gpl2+)))

(define-public squeak-vm
  (package
    (name "squeak-vm")
    (version "4.10.2.2614")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://squeakvm.org/unix/release/"
                           "Squeak-" version "-src.tar.gz"))
       (sha256
        (base32 "0bpwbnpy2sb4gylchfx50sha70z36bwgdxraym4vrr93l8pd3dix"))
       (modules '((guix build utils)))
       (snippet
        ;; Make builds bit-reproducible.
        '(begin
           (substitute* "unix/cmake/verstamp"
             (("vm_date=.*")
              "vm_date = \"1970-01-01\";\n")
             (("ux_version=.*")
              "ux_version = \"GNU\";\n"))
           (substitute* "unix/vm/config.cmake"
             (("\\(VM_BUILD_STRING.*")
              "(VM_BUILD_STRING \\\"Built with GNU Guix\\\")"))
           #t))))
    (inputs
     (list alsa-lib
           dbus
           freetype
           libffi
           libxrender
           mesa
           pulseaudio))
    (native-inputs
     (list pkg-config))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-hardcoded-PATH
           (lambda _
             ;; Remove hard-coded FHS PATH entries.
             (substitute* '("unix/cmake/squeak.in"
                            "unix/cmake/squeak.sh.in")
               (("^PATH=.*") ""))
             #t))
         (add-before 'configure 'enter-build-directory
           (lambda _
             (mkdir "build")
             (chdir "build")
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "../unix/cmake/configure"
                       (string-append "--prefix=" out)
                       "--without-quartz")
               #t))))))
    (synopsis "Smalltalk programming language and environment")
    (description "Squeak is a full-featured implementation of the Smalltalk
programming language and environment based on (and largely compatible with)
the original Smalltalk-80 system.  Squeak has very powerful 2- and 3-D
graphics, sound, video, MIDI, animation and other multimedia capabilities.  It
also includes a customisable framework for creating dynamic HTTP servers and
interactively extensible Web sites.")
    (home-page "http://squeakvm.org/")
    (license license:x11)))
