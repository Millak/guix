;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Noé Lopez <noelopez@free.fr>
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

(define-module (gnu packages appimage)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public appimage-type2-runtime
  (let ((revision "0")
        ;; No releases, just the latest commit.
        (commit "47b665594856b4e8928f8932adcf6d13061d8c30"))
    (package
      (name "appimage-type2-runtime")
      (version (git-version "continuous" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AppImage/type2-runtime")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0954crhlbapxis96g1s0vfpf78ybr64zvjalak387ksxj560g44x"))))
      (arguments
       (list
        #:make-flags
        #~(list "-Csrc/runtime" "runtime-fuse3"
                (string-append "CC=" #$(cc-for-target))
                (string-append
                 "CFLAGS=" "-I" #$(this-package-input "fuse") "/include/fuse/"
                 " -DGIT_COMMIT='\"" "guix-" #$version "\"'"
                 " -D_FILE_OFFSET_BITS=64"
                 " -static"
                 " -Wno-int-conversion"))
        #:modules
        `((guix build gnu-build-system)
          (guix build utils)
          (ice-9 binary-ports))
        #:phases #~(modify-phases %standard-phases
                     (delete 'configure)
                     (delete 'check)    ; No tests.
                     (replace 'install
                       (lambda _
                         (install-file "src/runtime/runtime-fuse3"
                                       (string-append #$output "/bin"))))
                     ;; Must be after all elf reliant phases.  Used to identify the
                     ;; executable as an AppImage as per the specification.
                     (add-after 'make-dynamic-linker-cache 'set-magic-bytes
                       (lambda _
                         (let ((port (open (string-append #$output
                                            "/bin/runtime-fuse3")
                                           (logior O_WRONLY))))
                           (seek port 8 SEEK_SET)
                           (put-bytevector port #vu8(#x41 #x49 #x02))
                           (close-port port)))))
        #:disallowed-references (list squashfuse-for-appimage
                                      fuse-for-appimage
                                      (gexp-input zstd "static")
                                      (gexp-input zlib "static"))))
      ;; Only needed at build time.
      (inputs (list squashfuse-for-appimage
                    fuse-for-appimage
                    `(,zstd "static")
                    `(,zlib "static")))
      (build-system gnu-build-system)
      (home-page "https://github.com/AppImage/type2-runtime")
      (synopsis "Runtime for executing AppImages")
      (description "The runtime is the executable part of every AppImage.  It mounts
the payload via @acronym{FUSE} and executes the entrypoint, allowing users to run
applications in a portable manner without the need for installation.  This runtime
ensures that the AppImage can access its bundled libraries and resources seamlessly,
providing a consistent environment across different Linux distributions.  In the
absence of @acronym{FUSE}, the AppImage can still be started using the
@option{--appimage-extract-and-run} flag.")
      (license license:expat))))
