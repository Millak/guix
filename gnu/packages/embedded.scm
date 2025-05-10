;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2023, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2021 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020, 2021, 2022 Simon South <simon@simonsouth.net>
;;; Copyright © 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages embedded)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix memoization)
  #:use-module (guix svn-download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix build utils) #:select (alist-replace delete-file-recursively))
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:export (make-gcc-arm-none-eabi-4.9
            make-gcc-arm-none-eabi-6
            make-gcc-arm-none-eabi-7-2018-q2-update
            make-gcc-arm-none-eabi-9-2020-q2-update
            make-gcc-arm-none-eabi-12.3.rel1

            make-gcc-vc4

            make-newlib-arm-none-eabi
            make-newlib-arm-none-eabi-7-2018-q2-update
            make-newlib-arm-none-eabi-9-2020-q2-update
            make-newlib-arm-none-eabi-12.3.rel1

            make-newlib-nano-arm-none-eabi
            make-newlib-nano-arm-none-eabi-7-2018-q2-update
            make-newlib-nano-arm-none-eabi-9-2020-q2-update
            make-newlib-nano-arm-none-eabi-12.3.rel1

            make-arm-none-eabi-toolchain-4.9
            make-arm-none-eabi-toolchain-6
            make-arm-none-eabi-toolchain-7-2018-q2-update
            make-arm-none-eabi-toolchain-9-2020-q2-update
            make-arm-none-eabi-toolchain-12.3.rel1

            make-arm-none-eabi-nano-toolchain-4.9
            make-arm-none-eabi-nano-toolchain-6
            make-arm-none-eabi-nano-toolchain-7-2018-q2-update
            make-arm-none-eabi-nano-toolchain-9-2020-q2-update
            make-arm-none-eabi-nano-toolchain-12.3.rel1

            make-gdb-arm-none-eabi

            make-propeller-gcc
            make-propeller-gcc-4
            make-propeller-gcc-6
            make-propeller-toolchain
            make-propeller-development-suite))

;;; Commentary:
;;;
;;; This modules contains toolchain generators as well as packages for use in
;;; embedded contexts.  Note: the toolchain and specialized packages are
;;; procedures, so as to delay their references to top level bindings such as
;;; 'gcc' or 'cross-gcc', etc.
;;;

;; We must not use the released GCC sources here, because the cross-compiler
;; does not produce working binaries.  Instead we take the very same SVN
;; revision from the branch that is used for a release of the "GCC ARM
;; embedded" project on launchpad.
;; See https://launchpadlibrarian.net/218827644/release.txt
(define make-gcc-arm-none-eabi-4.9
  (mlambda ()
    (let ((xgcc (cross-gcc "arm-none-eabi"
                           #:xgcc gcc-4.9
                           #:xbinutils (cross-binutils "arm-none-eabi")))
          (revision "1")
          (svn-revision 227977))
      (package
        (inherit xgcc)
        (version (string-append (package-version xgcc) "-"
                                revision "." (number->string svn-revision)))
        (source
         (origin
           (method svn-fetch)
           (uri (svn-reference
                 (url "svn://gcc.gnu.org/svn/gcc/branches/ARM/\
embedded-4_9-branch/")
                 (revision svn-revision)))
           (file-name (string-append "gcc-arm-embedded-" version "-checkout"))
           (sha256
            (base32
             "113r98kygy8rrjfv2pd3z6zlfzbj543pq7xyq8bgh72c608mmsbr"))

           (patches (cons (search-patch "gcc-4.9-inline.patch")
                          ;; Remove the one patch that doesn't apply to this
                          ;; 4.9 snapshot (the patch is for 4.9.4 and later
                          ;; but this svn snapshot is older).
                          (remove (lambda (patch)
                                    (string=? (basename patch)
                                              "gcc-arm-bug-71399.patch"))
                                  (origin-patches (package-source xgcc)))))))
        (native-inputs
         `(("flex" ,flex)
           ("gcc@5" ,gcc-5)
           ,@(package-native-inputs xgcc)))
        (arguments
         (substitute-keyword-arguments (package-arguments xgcc)
           ((#:phases phases)
            #~(modify-phases #$phases
                (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((gcc (assoc-ref inputs  "gcc")))
                      ;; Remove the default compiler from CPLUS_INCLUDE_PATH
                      ;; to prevent header conflict with the GCC from
                      ;; native-inputs.
                      (setenv "CPLUS_INCLUDE_PATH"
                              (string-join
                               (delete (string-append gcc "/include/c++")
                                       (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                     #\:))
                               ":"))
                      (format #t
                              "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                              (getenv "CPLUS_INCLUDE_PATH")))))
                (add-after 'unpack 'fix-genmultilib
                  (lambda _
                    (substitute* "gcc/genmultilib"
                      (("#!/bin/sh") (string-append "#!" (which "sh"))))))))
           ((#:configure-flags flags)
            ;; The configure flags are largely identical to the flags used by the
            ;; "GCC ARM embedded" project.
            #~(append (list "--enable-multilib"
                            "--with-newlib"
                            "--with-multilib-list=armv6-m,armv7-m,armv7e-m"
                            "--with-host-libstdcxx=-static-libgcc \
-Wl,-Bstatic,-lstdc++,-Bdynamic -lm"
                            "--enable-plugins"
                            "--disable-decimal-float"
                            "--disable-libffi"
                            "--disable-libgomp"
                            "--disable-libmudflap"
                            "--disable-libquadmath"
                            "--disable-libssp"
                            "--disable-libstdcxx-pch"
                            "--disable-nls"
                            "--disable-shared"
                            "--disable-threads"
                            "--disable-tls")
                      (delete "--disable-multilib" #$flags)))))
        (native-search-paths
         (list (search-path-specification
                (variable "CROSS_C_INCLUDE_PATH")
                (files '("arm-none-eabi/include")))
               (search-path-specification
                (variable "CROSS_CPLUS_INCLUDE_PATH")
                (files '("arm-none-eabi/include/c++"
                         "arm-none-eabi/include/c++/arm-none-eabi"
                         "arm-none-eabi/include")))
               (search-path-specification
                (variable "CROSS_LIBRARY_PATH")
                (files '("arm-none-eabi/lib")))))))))

(define make-gcc-arm-none-eabi-6
  (mlambda ()
    (package
      (inherit (make-gcc-arm-none-eabi-4.9))
      (version (package-version gcc-6))
      (source (origin
                (inherit (package-source gcc-6))
                (patches
                 (append
                  (origin-patches (package-source gcc-6))
                  (search-patches "gcc-6-cross-environment-variables.patch"
                                  "gcc-6-arm-none-eabi-multilib.patch"))))))))

(define make-newlib-arm-none-eabi
  (mlambda ()
    (package
      (name "newlib")
      (version "2.4.0")
      (source (origin
                (method url-fetch)
                (uri (string-append "ftp://sourceware.org/pub/newlib/newlib-"
                                    version ".tar.gz"))
                (sha256
                 (base32
                  "01i7qllwicf05vsvh39qj7qp5fdifpvvky0x95hjq39mbqiksnsl"))))
      (build-system gnu-build-system)
      (arguments
       `(#:out-of-source? #t
         ;; The configure flags are identical to the flags used by the "GCC ARM
         ;; embedded" project.
         #:configure-flags '("--target=arm-none-eabi"
                             "--enable-newlib-io-long-long"
                             "--enable-newlib-register-fini"
                             "--disable-newlib-supplied-syscalls"
                             "--disable-nls")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-references-to-/bin/sh
             (lambda _
               (substitute* (find-files "libgloss" "^Makefile\\.in$")
                 (("/bin/sh") (which "sh")))
               #t)))))
      (native-inputs
       `(("xbinutils" ,(cross-binutils "arm-none-eabi"))
         ("xgcc" ,(make-gcc-arm-none-eabi-4.9))
         ("texinfo" ,texinfo)))
      (home-page "https://www.sourceware.org/newlib/")
      (synopsis "C library for use on embedded systems")
      (description "Newlib is a C library intended for use on embedded
systems.  It is a conglomeration of several library parts that are easily
usable on embedded products.")
      (license (license:non-copyleft
                "https://www.sourceware.org/newlib/COPYING.NEWLIB")))))

(define make-newlib-nano-arm-none-eabi
  (mlambda ()
    (let ((base (make-newlib-arm-none-eabi)))
      (package
        (inherit base)
        (name "newlib-nano")
        (arguments
         (substitute-keyword-arguments (package-arguments base)
           ;; The configure flags are identical to the flags used by the "GCC
           ;; ARM embedded" project.  They optimize newlib for use on small
           ;; embedded systems with limited memory.
           ((#:configure-flags _)
            ''("--target=arm-none-eabi"
               "--enable-multilib"
               "--disable-newlib-supplied-syscalls"
               "--enable-newlib-reent-small"
               "--disable-newlib-fvwrite-in-streamio"
               "--disable-newlib-fseek-optimization"
               "--disable-newlib-wide-orient"
               "--enable-newlib-nano-malloc"
               "--disable-newlib-unbuf-stream-opt"
               "--enable-lite-exit"
               "--enable-newlib-global-atexit"
               "--enable-newlib-nano-formatted-io"
               "--disable-nls"))
           ((#:phases phases)
            `(modify-phases ,phases
               ;; XXX: Most arm toolchains offer both *.a and *_nano.a as
               ;; newlib and newlib-nano respectively.  The headers are
               ;; usually arm-none-eabi/include/newlib.h for newlib and
               ;; arm-none-eabi/include/newlib-nano/newlib.h for newlib-nano.
               ;; We have two different toolchain packages for each which
               ;; works but is a little strange.
               (add-after 'install 'hardlink-newlib
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     ;; The nano.specs file says that newlib-nano files should
                     ;; end in "_nano.a" instead of just ".a".  Note that this
                     ;; applies to all the multilib folders too.
                     (for-each
                      (lambda (file)
                        (link file
                              (string-append
                               ;; Strip ".a" off the end
                               (substring file 0 (- (string-length file) 2))
                               ;; Add "_nano.a" onto the end
                               "_nano.a")))
                      (find-files
                       out
                       "^(libc.a|libg.a|librdimon.a|libstdc\\+\\+.a|\
libsupc\\+\\+.a)$"))

                     ;; newlib.h is usually in this location instead so both
                     ;; newlib and newlib-nano can be in the toolchain at the
                     ;; same time
                     (mkdir (string-append
                             out "/arm-none-eabi/include/newlib-nano"))
                     (symlink
                      "../newlib.h"
                      (string-append
                       out
                       "/arm-none-eabi/include/newlib-nano/newlib.h")))))))))
        (synopsis "Newlib variant for small systems with limited memory")))))


;;; The following definitions are for the "7-2018-q2-update" variant of the
;;; ARM cross toolchain as offered on https://developer.arm.com
(define make-gcc-arm-none-eabi-7-2018-q2-update
  (mlambda ()
    (let ((xgcc (cross-gcc "arm-none-eabi"
                           #:xgcc gcc-7
                           #:xbinutils (cross-binutils "arm-none-eabi")))
          (revision "1")
          (svn-revision 261907))
      (package (inherit xgcc)
               (version (string-append "7-2018-q2-update-"
                                       revision "."
                                       (number->string svn-revision)))
               (source
                (origin
                  (method svn-fetch)
                  (uri (svn-reference
                        (url "svn://gcc.gnu.org/svn/gcc/branches/ARM/\
embedded-7-branch/")
                        (revision svn-revision)))
                  (file-name (string-append "gcc-arm-embedded-" version
                                            "-checkout"))
                  (sha256
                   (base32
                    "192ggs63bixf3irpijgfkjks73yx1r3a4i6grk1y0i0iny76pmx5"))
                  (patches
                   (append
                    (origin-patches (package-source gcc-7))
                    (search-patches
                     "gcc-7-cross-environment-variables.patch")))))
               (native-inputs
                (modify-inputs (package-native-inputs xgcc)
                  (delete "isl")
                  (prepend flex isl-0.18)))
               (arguments
                (substitute-keyword-arguments (package-arguments xgcc)
                  ((#:phases phases)
                   #~(modify-phases #$phases
                       (add-after 'unpack 'expand-version-string
                         (lambda _
                           (make-file-writable "gcc/DEV-PHASE")
                           (with-output-to-file "gcc/DEV-PHASE"
                             (lambda ()
                               (display "7-2018-q2-update")))))
                       (add-after 'unpack 'fix-genmultilib
                         (lambda _
                           (substitute* "gcc/genmultilib"
                             (("#!/bin/sh")
                              (string-append "#!" (which "sh"))))))
                       (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
                         (lambda* (#:key inputs #:allow-other-keys)
                           (let ((gcc (assoc-ref inputs  "gcc")))
                             ;; Remove the default compiler from
                             ;; CPLUS_INCLUDE_PATH to prevent header conflict
                             ;; with the GCC from native-inputs.
                             (setenv "CPLUS_INCLUDE_PATH"
                                     (string-join
                                      (delete (string-append gcc "/include/c++")
                                              (string-split
                                               (getenv "CPLUS_INCLUDE_PATH")
                                               #\:))
                                      ":"))
                             (format #t
                                     "environment variable `CPLUS_INCLUDE_PATH'\
 changed to ~a~%"
                                     (getenv "CPLUS_INCLUDE_PATH")))))))
                  ((#:configure-flags flags)
                   ;; The configure flags are largely identical to the flags
                   ;; used by the "GCC ARM embedded" project.
                   #~(append (list "--enable-multilib"
                                   "--with-newlib"
                                   "--with-multilib-list=rmprofile"
                                   "--with-host-libstdcxx=-static-libgcc \
-Wl,-Bstatic,-lstdc++,-Bdynamic -lm"
                                   "--enable-plugins"
                                   "--disable-decimal-float"
                                   "--disable-libffi"
                                   "--disable-libgomp"
                                   "--disable-libmudflap"
                                   "--disable-libquadmath"
                                   "--disable-libssp"
                                   "--disable-libstdcxx-pch"
                                   "--disable-nls"
                                   "--disable-shared"
                                   "--disable-threads"
                                   "--disable-tls")
                             (delete "--disable-multilib" #$flags)))))
               (native-search-paths
                (list (search-path-specification
                       (variable "CROSS_C_INCLUDE_PATH")
                       (files '("arm-none-eabi/include")))
                      (search-path-specification
                       (variable "CROSS_CPLUS_INCLUDE_PATH")
                       (files '("arm-none-eabi/include/c++"
                                "arm-none-eabi/include/c++/arm-none-eabi"
                                "arm-none-eabi/include")))
                      (search-path-specification
                       (variable "CROSS_LIBRARY_PATH")
                       (files '("arm-none-eabi/lib")))))))))

(define make-base-newlib-arm-none-eabi-7-2018-q2-update
  ;; This is the same commit as used for the 7-2018-q2-update release
  ;; according to the release.txt.
  (mlambda (base)
    (let ((commit "3ccfb407af410ba7e54ea0da11ae1e40b554a6f4")
          (revision "0"))
      (package
        (inherit base)
        (version (git-version "3.0.0" revision commit))
        (source
         (origin
           (method git-fetch)
           (uri (git-reference
                 (url "http://sourceware.org/git/newlib-cygwin.git")
                 (commit commit)))
           (file-name (git-file-name "newlib" commit))
           (sha256
            (base32
             "1dq23fqrk75g1a4v7569fvnnw5q440zawbxi3w0g05n8jlqsmvcy"))))
        (arguments
         (substitute-keyword-arguments (package-arguments base)
           ;; The configure flags are identical to the flags used by the "GCC
           ;; ARM embedded" project.
           ((#:configure-flags flags)
            `(cons* "--enable-newlib-io-c99-formats"
                    "--enable-newlib-retargetable-locking"
                    "--with-headers=yes"
                    ,flags))))
        (native-inputs
         `(("xbinutils" ,(cross-binutils "arm-none-eabi"))
           ("xgcc" ,(make-gcc-arm-none-eabi-7-2018-q2-update))
           ("texinfo" ,texinfo)))))))

(define make-newlib-arm-none-eabi-7-2018-q2-update
  (mlambda ()
    (make-base-newlib-arm-none-eabi-7-2018-q2-update (make-newlib-arm-none-eabi))))

(define make-newlib-nano-arm-none-eabi-7-2018-q2-update
  (mlambda ()
    (make-base-newlib-arm-none-eabi-7-2018-q2-update (make-newlib-nano-arm-none-eabi))))


;;; The following definitions are for the "9-2020-q2-update" variant of the
;;; ARM cross toolchain as offered on https://developer.arm.com
(define make-gcc-arm-none-eabi-9-2020-q2-update
  (mlambda ()
    (let ((xgcc (cross-gcc "arm-none-eabi"
                           #:xgcc gcc-9
                           #:xbinutils (cross-binutils "arm-none-eabi")))
          (commit "13861a80750d118fbdca6006ab175903bacbb7ec")
          (revision "1"))
      (package (inherit xgcc)
               (version (git-version "9-2020-q2-update" revision commit))
               (source
                (origin
                  (inherit (package-source xgcc))
                  (method git-fetch)
                  (uri (git-reference
                        (url "git://gcc.gnu.org/git/gcc.git")
                        (commit commit)))
                  (file-name (git-file-name "gcc-arm-embedded" version))
                  (sha256
                   (base32
                    "1cshb1wbynjac3icbf2na633k4d8r7nqn4nv0d65q6f78w295h97"))
                  (patches
                   (append
                    (origin-patches (package-source gcc-9))
                    (search-patches
                     "gcc-10-cross-environment-variables.patch")))))
               (native-inputs
                (modify-inputs (package-native-inputs xgcc)
                  (delete "isl")
                  (prepend flex isl-0.18)))
               (arguments
                (substitute-keyword-arguments (package-arguments xgcc)
                  ((#:phases phases)
                   #~(modify-phases #$phases
                       (add-after 'unpack 'expand-version-string
                         (lambda _
                           (make-file-writable "gcc/DEV-PHASE")
                           (with-output-to-file "gcc/DEV-PHASE"
                             (lambda ()
                               (display "9-2020-q2-update")))))
                       (add-after 'unpack 'fix-genmultilib
                         (lambda _
                           (substitute* "gcc/genmultilib"
                             (("#!/bin/sh")
                              (string-append "#!" (which "sh"))))))
                       (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
                         (lambda* (#:key inputs #:allow-other-keys)
                           (let ((gcc (assoc-ref inputs  "gcc")))
                             ;; Remove the default compiler from
                             ;; CPLUS_INCLUDE_PATH to prevent header conflict
                             ;; with the GCC from native-inputs.
                             (setenv "CPLUS_INCLUDE_PATH"
                                     (string-join
                                      (delete (string-append gcc "/include/c++")
                                              (string-split
                                               (getenv "CPLUS_INCLUDE_PATH")
                                               #\:))
                                      ":"))
                             (format #t
                                     "environment variable `CPLUS_INCLUDE_PATH'\
 changed to ~a~%"
                                     (getenv "CPLUS_INCLUDE_PATH")))))))
                  ((#:configure-flags flags)
                   ;; The configure flags are largely identical to the flags
                   ;; used by the "GCC ARM embedded" project.
                   #~(append (list "--enable-multilib"
                                   "--with-newlib"
                                   "--with-multilib-list=rmprofile"
                                   "--with-host-libstdcxx=-static-libgcc \
-Wl,-Bstatic,-lstdc++,-Bdynamic -lm"
                                   "--enable-plugins"
                                   "--disable-decimal-float"
                                   "--disable-libffi"
                                   "--disable-libgomp"
                                   "--disable-libmudflap"
                                   "--disable-libquadmath"
                                   "--disable-libssp"
                                   "--disable-libstdcxx-pch"
                                   "--disable-nls"
                                   "--disable-shared"
                                   "--disable-threads"
                                   "--disable-tls")
                             (delete "--disable-multilib" #$flags)))))
               (native-search-paths
                (list (search-path-specification
                       (variable "CROSS_C_INCLUDE_PATH")
                       (files '("arm-none-eabi/include")))
                      (search-path-specification
                       (variable "CROSS_CPLUS_INCLUDE_PATH")
                       (files '("arm-none-eabi/include/c++"
                                "arm-none-eabi/include/c++/arm-none-eabi"
                                "arm-none-eabi/include")))
                      (search-path-specification
                       (variable "CROSS_LIBRARY_PATH")
                       (files '("arm-none-eabi/lib")))))))))

(define make-base-newlib-arm-none-eabi-9-2020-q2-update
  ;; This is the same commit as used for the 9-2020-q2-update release
  ;; according to the release.txt.
  (mlambda (base)
    (let ((commit "6d79e0a58866548f435527798fbd4a6849d05bc7")
          (revision "0"))
      (package
        (inherit base)
        (version (git-version "3.3.0" revision commit))
        (source
         (origin
           (method git-fetch)
           (uri (git-reference
                 (url "http://sourceware.org/git/newlib-cygwin.git")
                 (commit commit)))
           (file-name (git-file-name "newlib" version))
           (sha256
            (base32
             "095j23mg928rmf4yqmj39wc0nsd207liqrdw4ygh58nygsm4gpmh"))))
        (arguments
         (substitute-keyword-arguments (package-arguments base)
           ;; The configure flags are identical to the flags used by the "GCC
           ;; ARM embedded" project.
           ((#:configure-flags flags)
            `(cons* "--enable-newlib-io-c99-formats"
                    "--enable-newlib-retargetable-locking"
                    "--with-headers=yes"
                    ,flags))))
        (native-inputs
         `(("xbinutils" ,(cross-binutils "arm-none-eabi"))
           ("xgcc" ,(make-gcc-arm-none-eabi-9-2020-q2-update))
           ("texinfo" ,texinfo)))))))

(define make-newlib-arm-none-eabi-9-2020-q2-update
  (mlambda ()
    (make-base-newlib-arm-none-eabi-9-2020-q2-update (make-newlib-arm-none-eabi))))

(define make-newlib-nano-arm-none-eabi-9-2020-q2-update
  (mlambda ()
    (make-base-newlib-arm-none-eabi-9-2020-q2-update (make-newlib-nano-arm-none-eabi))))


;;; The following definitions are for the "12.3.rel1" variant of the
;;; ARM cross toolchain as offered on https://developer.arm.com
(define-public make-gcc-arm-none-eabi-12.3.rel1
  (mlambda ()
    (let ((base (make-gcc-arm-none-eabi-7-2018-q2-update))
          (xgcc-base (cross-gcc "arm-none-eabi"
                                #:xgcc gcc-12
                                #:xbinutils (cross-binutils "arm-none-eabi"))))
      (package
        (inherit base)
        (version "12.3.rel1")
        (source
         (origin
           (inherit (package-source xgcc-base))
           (method git-fetch)
           (uri (git-reference
                 (url "git://gcc.gnu.org/git/gcc.git")
                 (commit "0f54a73b998b72f7c8452a63730ec3b16fc47854")))
           (sha256
            (base32 "0r6q0m3d8g3k3rkmnqjw8aw5fcnsrmywf4ispdkxmk1al3whk1vk"))))
        (arguments
         (substitute-keyword-arguments (package-arguments base)
           ((#:phases phases)
            #~(modify-phases #$phases
                (replace 'expand-version-string
                  (lambda _
                    (make-file-writable "gcc/DEV-PHASE")
                    (with-output-to-file "gcc/DEV-PHASE"
                      (lambda ()
                        (display "12.3.rel1")))))))
           ((#:configure-flags flags)
            #~(cons* "--with-multilib-list=aprofile,rmprofile"
                     "--with-headers=yes"
                     "--enable-checking=release"
                     "--with-gnu-as"
                     "--with-gnu-ld"
                     (filter
                      (lambda (flag)
                        (not (member flag
                                     '("--with-multilib-list=rmprofile"
                                       "--enable-plugins"
                                       "--disable-libffi"))))
                      #$flags)))))))))

(define make-base-newlib-arm-none-eabi-12.3.rel1
  (mlambda (original-base)
    (let ((base (make-base-newlib-arm-none-eabi-7-2018-q2-update original-base))
          (commit "4c7d0dfec5793cbf5cf3930b91f930479126d8ce")
          (revision "0"))
      (package
        (inherit base)
        (version (git-version "4.3.0" revision commit))
        (source
         (origin
           (method git-fetch)
           (uri (git-reference
                 (url "http://sourceware.org/git/newlib-cygwin.git")
                 (commit commit)))
           (sha256
            (base32
             "0drs9v8avh4y2h5bs0ixjn9x662jzkkikx8z034wgl41dxmn6786"))))
        (arguments (substitute-keyword-arguments (package-arguments base)
                     ((#:configure-flags flags)
                      #~(cons* "--enable-newlib-mb"
                               "--enable-newlib-reent-check-verify"
                               "--enable-newlib-register-fini"
                               #$flags))))))))

(define make-newlib-arm-none-eabi-12.3.rel1
  (mlambda ()
    (make-base-newlib-arm-none-eabi-12.3.rel1 (make-newlib-arm-none-eabi))))

(define make-newlib-nano-arm-none-eabi-12.3.rel1
  (mlambda ()
    (make-base-newlib-arm-none-eabi-12.3.rel1 (make-newlib-nano-arm-none-eabi))))


(define make-libstdc++-arm-none-eabi
  (mlambda (xgcc newlib)
    (let* ((libstdc++ (make-libstdc++ xgcc))
           (src (package-source libstdc++)))
      (package
        (inherit libstdc++)
        (source
         (origin
           (inherit src)
           (patches (append
                     ; libstdc++ cannot be linked with since the configure phase
                     ; cannot detect properly the presence of getentropy function.
                     ; The function is inside of a header, but it's not present in the resulting
                     ; newlib. configure will conclude getentropy is present,
                     ; random will use getentropy, and any linking with random will fail.
                     (if (version>=? (package-version xgcc) "12.0")
                         (search-patches "newlib-getentropy.patch")
                         '())
                     (origin-patches src)))))
        (name "libstdc++-arm-none-eabi")
        (arguments
         (substitute-keyword-arguments (package-arguments libstdc++)
           ((#:make-flags flags #f)
            #~(cons* "CFLAGS=-g -O2 -fdata-sections -ffunction-sections"
                     "CXXFLAGS=-g -O2 -fdata-sections -ffunction-sections"
                     (or #$flags '())))
           ((#:configure-flags _)
            ``(; This is more of a hack. This option doesn't really seem
               ; to change what subdir is used eventually, but without it there is
               ; error: Link tests are not allowed after GCC_NO_EXECUTABLES with
               ; The 12.3 toolchain
               "--with-target-subdir=\".\""
               "--target=arm-none-eabi"
               "--host=arm-none-eabi"
               "--disable-libstdcxx-pch"
               "--enable-multilib"
               "--with-multilib-list=armv6-m,armv7-m,armv7e-m"
               "--disable-shared"
               "--disable-tls"
               "--disable-plugin"
               "--with-newlib"
               ,(string-append "--libdir="
                               (assoc-ref %outputs "out")
                               "/arm-none-eabi/lib")
               ,(string-append "--with-gxx-include-dir="
                               (assoc-ref %outputs "out")
                               "/arm-none-eabi/include/c++")))
           ((#:strip-directories _ #f)
            ''("arm-none-eabi/lib"))))
        (native-inputs
         `(("newlib" ,newlib)
           ("xgcc" ,xgcc)
           ,@(package-native-inputs libstdc++)))))))

(define make-libstdc++-nano-arm-none-eabi
  (mlambda (xgcc newlib-nano)
    (let ((base (make-libstdc++-arm-none-eabi xgcc newlib-nano)))
      (package
        (inherit base)
        (name "libstdc++-nano-arm-none-eabi")
        (arguments (substitute-keyword-arguments (package-arguments base)
                     ((#:make-flags flags)
                      #~(map (lambda (flag)
                               (if (or (string-prefix? "CFLAGS=" flag)
                                       (string-prefix? "CXXFLAGS=" flag))
                                   (string-append flag " -fno-exceptions")
                                   flag))
                             #$flags))
                     ((#:phases phases)
                      #~(modify-phases #$phases
                          (add-after 'install 'hardlink-libstdc++
                            ;; XXX: Most arm toolchains offer both *.a and *_nano.a as
                            ;; newlib and newlib-nano respectively.  The headers are
                            ;; usually arm-none-eabi/include/newlib.h for newlib and
                            ;; arm-none-eabi/include/newlib-nano/newlib.h for newlib-nano.
                            ;; We have two different toolchain packages for each which
                            ;; works but is a little strange.
                            (lambda* (#:key outputs #:allow-other-keys)
                              (let ((out (assoc-ref outputs "out")))
                                ;; The nano.specs file says that newlib-nano files should
                                ;; end in "_nano.a" instead of just ".a".  Note that this
                                ;; applies to all the multilib folders too.
                                (for-each
                                 (lambda (file)
                                   (link file
                                         (string-append
                                          ;; Strip ".a" off the end
                                          (substring file 0 (- (string-length file) 2))
                                          ;; Add "_nano.a" onto the end
                                          "_nano.a")))
                                 (find-files
                                  out "^(libstdc\\+\\+.a|libsupc\\+\\+.a)$")))))))))))))

(define make-arm-none-eabi-toolchain
  (mlambda (xgcc newlib)
    "Produce a cross-compiler toolchain package with the compiler XGCC and the
C library variant NEWLIB."
    (let* ((nano? (string=? (package-name newlib)
                            "newlib-nano"))
           (newlib-with-xgcc
            (package
              (inherit newlib)
              (native-inputs
               (alist-replace "xgcc" (list xgcc)
                              (package-native-inputs newlib)))))
           (libstdc++
            (if nano?
                (make-libstdc++-nano-arm-none-eabi xgcc newlib-with-xgcc)
                (make-libstdc++-arm-none-eabi xgcc newlib-with-xgcc))))
      (package
        (name (string-append "arm-none-eabi"
                             (if nano? "-nano" "")
                             "-toolchain"))
        (version (package-version xgcc))
        (source #f)
        (build-system trivial-build-system)
        (arguments
         '(#:modules ((guix build union))
           #:builder
           (begin
             (use-modules (ice-9 match)
                          (guix build union))
             (match %build-inputs
               (((names . directories) ...)
                (union-build (assoc-ref %outputs "out")
                             directories))))))
        (propagated-inputs
         `(("binutils" ,(cross-binutils "arm-none-eabi"))
           ("libstdc++" ,libstdc++)
           ("gcc" ,xgcc)
           ("newlib" ,newlib-with-xgcc)))
        (synopsis "Complete GCC tool chain for ARM bare metal development")
        (description "This package provides a complete GCC tool chain for ARM
bare metal development.  This includes the GCC arm-none-eabi cross compiler
and newlib (or newlib-nano) as the C library.  The supported programming
languages are C and C++.")
        (home-page (package-home-page xgcc))
        (license (package-license xgcc))))))

(define make-arm-none-eabi-toolchain-4.9
  (mlambda ()
    (make-arm-none-eabi-toolchain (make-gcc-arm-none-eabi-4.9)
                                  (make-newlib-arm-none-eabi))))

(define make-arm-none-eabi-nano-toolchain-4.9
  (mlambda ()
    (make-arm-none-eabi-toolchain (make-gcc-arm-none-eabi-4.9)
                                  (make-newlib-nano-arm-none-eabi))))

(define make-arm-none-eabi-toolchain-6
  (mlambda ()
    (make-arm-none-eabi-toolchain (make-gcc-arm-none-eabi-6)
                                  (make-newlib-arm-none-eabi))))

(define make-arm-none-eabi-nano-toolchain-6
  (mlambda ()
    (make-arm-none-eabi-toolchain (make-gcc-arm-none-eabi-6)
                                  (make-newlib-nano-arm-none-eabi))))

(define make-arm-none-eabi-toolchain-7-2018-q2-update
  (mlambda ()
    (make-arm-none-eabi-toolchain
     (make-gcc-arm-none-eabi-7-2018-q2-update)
     (make-newlib-arm-none-eabi-7-2018-q2-update))))

(define make-arm-none-eabi-nano-toolchain-7-2018-q2-update
  (mlambda ()
    (make-arm-none-eabi-toolchain
     (make-gcc-arm-none-eabi-7-2018-q2-update)
     (make-newlib-nano-arm-none-eabi-7-2018-q2-update))))

(define make-arm-none-eabi-toolchain-9-2020-q2-update
  (mlambda ()
    (make-arm-none-eabi-toolchain
     (make-gcc-arm-none-eabi-9-2020-q2-update)
     (make-newlib-arm-none-eabi-9-2020-q2-update))))

(define make-arm-none-eabi-nano-toolchain-9-2020-q2-update
  (mlambda ()
    (make-arm-none-eabi-toolchain
     (make-gcc-arm-none-eabi-9-2020-q2-update)
     (make-newlib-nano-arm-none-eabi-9-2020-q2-update))))

(define make-arm-none-eabi-toolchain-12.3.rel1
  (mlambda ()
    (make-arm-none-eabi-toolchain
     (make-gcc-arm-none-eabi-12.3.rel1)
     (make-newlib-arm-none-eabi-12.3.rel1))))

(define make-arm-none-eabi-nano-toolchain-12.3.rel1
  (mlambda ()
    (make-arm-none-eabi-toolchain
     (make-gcc-arm-none-eabi-12.3.rel1)
     (make-newlib-nano-arm-none-eabi-12.3.rel1))))

(define make-gdb-arm-none-eabi
  (mlambda ()
    (package
      (inherit gdb)
      (name "gdb-arm-none-eabi")
      (arguments
       (substitute-keyword-arguments (package-arguments gdb)
         ((#:configure-flags flags '())
          #~(cons* "--target=arm-none-eabi"
                   "--enable-multilib"
                   "--enable-interwork"
                   "--enable-languages=c,c++"
                   "--disable-nls"
                   #$flags)))))))

(define-public libjaylink
  (package
    (name "libjaylink")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://repo.or.cz/libjaylink.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wps72ir2kwdr7dphx4vp6cy0d46dm3nkwbk0mpryn9la09l7lm1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list libusb))
    (home-page "https://repo.or.cz/w/libjaylink.git")
    (synopsis "Library to interface Segger J-Link devices")
    (description "libjaylink is a shared library written in C to access
SEGGER J-Link and compatible devices.")
    (license license:gpl2+)))

(define-public jimtcl
  (package
    (name "jimtcl")
    (version "0.82")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/msteveb/jimtcl")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01nxqzn41797ypph1vpwjfh3zqgks0l8ihh6932b4kb83apy6f08"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 ;; This package doesn't use autoconf.
                 (lambda _
                   (invoke "./configure"
                           (string-append "--prefix=" #$output))))
               (add-before 'check 'delete-failing-tests
                 (lambda _
                   ;; XXX All but 1 SSL tests fail (tries connecting to Google
                   ;; servers).
                   (delete-file "tests/ssl.test")))
               #$@(if (not (target-64bit?))
                      #~((add-after 'unpack 'delete-failing-tests/32bit
                           (lambda _
                             (delete-file "tests/file.test"))))
                      #~()))))
    (inputs (list openssl))
    (native-inputs
     ;; For tests.
     (list inetutils))       ; for hostname
    (home-page "http://jim.tcl.tk/index.html")
    (synopsis "Small footprint Tcl implementation")
    (description "Jim is a small footprint implementation of the Tcl programming
language.")
    (license license:bsd-2)))

(define-public openocd
  (package
    (name "openocd")
    (version "0.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.code.sf.net/p/openocd/code")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "09wb11zlmrw6rx1bql3kafgi3ilzp9mhvb6j6rql216by06csing"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           libtool
           which
           pkg-config
           texinfo))
    (inputs
     (list hidapi jimtcl libftdi libjaylink openssl))
    (arguments
     '(#:configure-flags
       (append (list "LIBS=-lutil -lcrypto -lssl"
                     "--disable-werror"
                     "--enable-sysfsgpio"
                     "--disable-internal-jimtcl"
                     "--disable-internal-libjaylink")
               (map (lambda (programmer)
                      (string-append "--enable-" programmer))
                    '("amtjtagaccel" "armjtagew" "buspirate" "ftdi"
                      "gw16012" "jlink" "opendous" "osbdm"
                      "parport" "aice" "cmsis-dap" "dummy" "jtag_vpi"
                      "remote-bitbang" "rlink" "stlink" "ti-icdi" "ulink"
                      "usbprog" "vsllink" "usb-blaster-2" "usb_blaster"
                      "presto" "openjtag" "rshim" "ft232r" "xds110"
                      "cmsis-dap-v2" "nulink" "kitprog" "jtag_dpi"
                      "bcm2835gpio" "imx_gpio" "ep93xx" "at91rm9200"
                      "sysfsgpio" "xlnx-pcie-xvc")))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             ;; Make build reproducible.
             (substitute* "src/Makefile.am"
               (("-DPKGBLDDATE=") "-DDISABLED_PKGBLDDATE="))
             (patch-shebang "bootstrap")
             (invoke "./bootstrap" "nosubmodule")))
         (add-after 'unpack 'change-udev-group
           (lambda _
             (substitute* "contrib/60-openocd.rules"
               (("plugdev") "dialout"))))
         (add-after 'install 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "contrib/60-openocd.rules"
                           (string-append
                            (assoc-ref outputs "out")
                            "/lib/udev/rules.d/")))))))
    (home-page "https://openocd.org/")
    (synopsis "On-Chip Debugger")
    (description "OpenOCD provides on-chip programming and debugging support
with a layered architecture of JTAG interface and TAP support.")
    (license license:gpl2+)))

;; The commits for all propeller tools are the stable versions published at
;; https://github.com/propellerinc/propgcc in the release_1_0.  According to
;; personal correspondence with the developers in July 2017, more recent
;; versions are currently incompatible with the "Simple Libraries".

(define make-propeller-binutils
  (mlambda ()
    (let ((xbinutils (cross-binutils "propeller-elf"))
          (commit "4c46ecbe79ffbecd2ce918497ace5b956736b5a3")
          (revision "2"))
      (package
        (inherit xbinutils)
        (name "propeller-binutils")
        (version (string-append "0.0.0-" revision "." (string-take commit 9)))
        (source (origin
                  (inherit (package-source xbinutils))
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/parallaxinc/propgcc")
                        (commit commit)))
                  (file-name (string-append name "-" commit "-checkout"))
                  (sha256
                   (base32
                    "0w0dff3s7wv2d9m78a4jhckiik58q38wx6wpbba5hzbs4yxz35ck"))
                  (patches '())))
        (arguments
         (list
          ;; FIXME: For some reason there are many test failures.  It's not
          ;; obvious how to fix the failures.
          #:tests? #f
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'chdir
                (lambda _ (chdir "binutils") #t))
              #$@(substitute-keyword-arguments (package-arguments xbinutils)
                   ((#:configure-flags flags)
                    #~(cons "--disable-werror" #$flags))))))
        (native-inputs
         `(("bison" ,bison)
           ("flex" ,flex)
           ("texinfo" ,texinfo)
           ("dejagnu" ,dejagnu)
           ,@(package-native-inputs xbinutils)))))))

(define make-propeller-gcc-6
  (mlambda ()
    (let ((xgcc (cross-gcc "propeller-elf"
                           #:xbinutils (make-propeller-binutils)))
          (commit "b4f45a4725e0b6d0af59e594c4e3e35ca4105867")
          (revision "1"))
      (package
        (inherit xgcc)
        (name "propeller-gcc")
        (version (string-append "6.0.0-" revision "." (string-take commit 9)))
        (source (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/totalspectrum/gcc-propeller")
                        (commit commit)))
                  (file-name (string-append name "-" commit "-checkout"))
                  (sha256
                   (base32
                    "0d9kdxm2fzanjqa7q5850kzbsfl0fqyaahxn74h6nkxxacwa11zb"))
                  (patches
                   (append
                    (origin-patches (package-source gcc-6))
                    (search-patches "gcc-cross-environment-variables.patch")))))
        (native-inputs
         (modify-inputs (package-native-inputs xgcc)
           (prepend flex)))
        ;; All headers and cross libraries of the propeller toolchain are
        ;; installed under the "propeller-elf" prefix.
        (native-search-paths
         (list (search-path-specification
                (variable "CROSS_C_INCLUDE_PATH")
                (files '("propeller-elf/include")))
               (search-path-specification
                (variable "CROSS_LIBRARY_PATH")
                (files '("propeller-elf/lib")))))
        (home-page "https://github.com/totalspectrum/gcc-propeller")
        (synopsis "GCC for the Parallax Propeller")))))

(define make-propeller-gcc-4
  (mlambda ()
    (let ((xgcc (make-propeller-gcc-6))
          (commit "4c46ecbe79ffbecd2ce918497ace5b956736b5a3")
          (revision "2"))
      (package
        (inherit xgcc)
        (name "propeller-gcc")
        (version (string-append "4.6.1-" revision "." (string-take commit 9)))
        (source (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/parallaxinc/propgcc")
                        (commit commit)))
                  (file-name (string-append name "-" commit "-checkout"))
                  (sha256
                   (base32
                    "0w0dff3s7wv2d9m78a4jhckiik58q38wx6wpbba5hzbs4yxz35ck"))
                  (patch-flags (list "-p1" "--directory=gcc"))
                  (patches
                   (append
                    (origin-patches (package-source gcc-4.7))
                    (search-patches
                     "gcc-4.6-gnu-inline.patch"
                     "gcc-cross-environment-variables.patch")))))
        (arguments
         (substitute-keyword-arguments (package-arguments xgcc)
           ((#:phases phases)
            #~(modify-phases #$phases
                (add-after 'unpack 'chdir
                  (lambda _ (chdir "gcc")))))))
        (native-inputs
         (modify-inputs (package-native-inputs xgcc)
           (prepend gcc-4.9)))
        (home-page "https://github.com/parallaxinc/propgcc")
        (supported-systems (delete "aarch64-linux" %supported-systems))))))

;; Version 6 is experimental and may not work correctly.  This is why we
;; default to version 4, which is also used in the binary toolchain bundle
;; provided by Parallax Inc.
(define make-propeller-gcc make-propeller-gcc-4)


;; FIXME: We do not build the tiny library because that would require C++
;; headers, which are not available.  This may require adding a propeller-elf
;; variant of the libstdc++ package.
(define-public proplib
  (let ((commit "4c46ecbe79ffbecd2ce918497ace5b956736b5a3")
        (revision "2"))
    (package
      (name "proplib")
      (version (string-append "0.0.0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/parallaxinc/propgcc")
                      (commit commit)))
                (file-name (string-append name "-" commit "-checkout"))
                (sha256
                 (base32
                  "0w0dff3s7wv2d9m78a4jhckiik58q38wx6wpbba5hzbs4yxz35ck"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
               (string-append "BUILD="  (getcwd) "/build"))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'chdir
             (lambda _ (chdir "lib") #t))
           (add-after 'chdir 'fix-Makefile
             (lambda _
               (substitute* "Makefile"
                 ;; Control the installation time of the headers.
                 ((" install-includes") ""))
               #t))
           ;; The Makefile does not separate building from installation, so we
           ;; have to create the target directories at build time.
           (add-before 'build 'create-target-directories
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make" "install-dirs" make-flags)))
           (add-before 'build 'set-cross-environment-variables
             (lambda* (#:key outputs #:allow-other-keys)
               (setenv "CROSS_LIBRARY_PATH"
                       (string-append (assoc-ref outputs "out")
                                      "/propeller-elf/lib:"
                                      (or (getenv "CROSS_LIBRARY_PATH") "")))
               (setenv "CROSS_C_INCLUDE_PATH"
                       (string-append (assoc-ref outputs "out")
                                      "/propeller-elf/include:"
                                      (or (getenv "CROSS_C_INCLUDE_PATH") "")))
               #t))
           (add-before 'install 'install-includes
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make" "install-includes" make-flags))))))
      (native-inputs
       (list (make-propeller-gcc) (make-propeller-binutils) perl))
      (home-page "https://github.com/parallaxinc/propgcc")
      (synopsis "C library for the Parallax Propeller")
      (description "This is a C library for the Parallax Propeller
micro-controller.")
      ;; Most of the code is released under the Expat license.  Some of the
      ;; included code is public domain and some changes are BSD licensed.
      (license license:expat))))

(define make-propeller-toolchain
  (mlambda ()
    (let ((propeller-gcc (make-propeller-gcc)))
      (package
        (name "propeller-toolchain")
        (version (package-version propeller-gcc))
        (source #f)
        (build-system trivial-build-system)
        (arguments '(#:builder (begin (mkdir %output) #t)))
        (propagated-inputs
         `(("binutils" ,(make-propeller-binutils))
           ("libc" ,proplib)
           ("gcc" ,propeller-gcc)))
        (synopsis "Complete GCC tool chain for Propeller micro-controllers")
        (description "This package provides a complete GCC tool chain for
Propeller micro-controller development.")
        (home-page (package-home-page propeller-gcc))
        (license (package-license propeller-gcc))))))

(define-public openspin
  (package
    (name "openspin")
    (version "1.00.78")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/parallaxinc/OpenSpin")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ghk8hj4717ydhqzx2pfs6737s1cxng6sgg2xgbkwvcfclxdbrd0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'remove-timestamp
           (lambda _
             (substitute* "SpinSource/openspin.cpp"
               ((" Compiled on.*$") "\\n\");"))
             #t))
         ;; Makefile does not include "install" target
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (mkdir-p bin)
               (install-file "build/openspin" bin)
               #t))))))
    (home-page "https://github.com/parallaxinc/OpenSpin")
    (synopsis "Spin/PASM compiler for the Parallax Propeller")
    (description "OpenSpin is a compiler for the Spin/PASM language of the
Parallax Propeller.  It was ported from Chip Gracey's original x86 assembler
code.")
    (license license:expat)))

(define-public propeller-load
  (let ((commit "4c46ecbe79ffbecd2ce918497ace5b956736b5a3")
        (revision "2"))
    (package
      (name "propeller-load")
      (version "3.4.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/parallaxinc/propgcc")
                      (commit commit)))
                (file-name (string-append name "-" commit "-checkout"))
                (sha256
                 (base32
                  "0w0dff3s7wv2d9m78a4jhckiik58q38wx6wpbba5hzbs4yxz35ck"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #false                 ;no tests
        #:parallel-build? #false        ;not supported
        #:make-flags
        #~(list "OS=linux"
                (string-append "TARGET=" #$output))
        #:phases
        '(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "loader")))
           (delete 'configure))))
      (native-inputs
       (list openspin (make-propeller-toolchain)))
      (home-page "https://github.com/parallaxinc/propgcc")
      (synopsis "Loader for Parallax Propeller micro-controllers")
      (description "This package provides the tool @code{propeller-load} to
upload binaries to a Parallax Propeller micro-controller.")
      (license license:expat))))

(define-public spin2cpp
  (package
    (name "spin2cpp")
    (version "3.6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/totalspectrum/spin2cpp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wznqvsckzzz4hdy2rpvj6jqpxw4yn7i0c7zxfm6i46k8gg9327b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; The tests assume that a micro-controller is connected.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'set-cross-environment-variables
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CROSS_LIBRARY_PATH"
                     (string-append (assoc-ref inputs "propeller-toolchain")
                                    "/propeller-elf/lib"))
             (setenv "CROSS_C_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "propeller-toolchain")
                                    "/propeller-elf/include"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (for-each (lambda (file)
                           (install-file (string-append "build/" file)
                                         bin))
                         '("testlex" "spin2cpp" "fastspin")))
             #t)))))
    (native-inputs
     (list bison propeller-load (make-propeller-toolchain)))
    (home-page "https://github.com/totalspectrum/spin2cpp")
    (synopsis "Convert Spin code to C, C++, or PASM code")
    (description "This is a set of tools for converting the Spin language for
the Parallax Propeller micro-controller into C or C++ code, into PASM, or even
directly into an executable binary.  The binaries produced use LMM PASM, so
they are much faster than regular Spin bytecodes (but also quite a bit
larger).")
    (license license:expat)))

(define-public spinsim
  (let ((commit "66915a7ad1a3a2cf990a725bb341fab8d11eb620")
        (revision "1"))
    (package
      (name "spinsim")
      (version (string-append "0.75-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/parallaxinc/spinsim")
                      (commit commit)))
                (file-name (string-append name "-" commit "-checkout"))
                (sha256
                 (base32
                  "1n9kdhlxsdx7bz6c80w8dhi96zp633gd6qs0x9i4ii8qv4i7sj5k"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out")
                                         "/bin")))
                 (install-file "build/spinsim" bin))
               #t)))))
      (home-page "https://github.com/parallaxinc/spinsim")
      (synopsis "Spin simulator")
      (description "This package provides the tool @code{spinsim}, a simulator
and simple debugger for Spin programs written for a Parallax Propeller
micro-controller.  Spinsim supports execution from cog memory and hub
execution, but it does not support multi-tasking.  It supports about
two-thirds of the opcodes in the P2 instruction set.")
      (license license:expat))))

(define make-propeller-development-suite
  (mlambda ()
    (let ((propeller-gcc (make-propeller-gcc)))
      (package
        (name "propeller-development-suite")
        (version (package-version propeller-gcc))
        (source #f)
        (build-system trivial-build-system)
        (arguments '(#:builder (begin (mkdir %output) #t)))
        (propagated-inputs
         `(("toolchain" ,(make-propeller-toolchain))
           ("openspin" ,openspin)
           ("propeller-load" ,propeller-load)
           ("spin2cpp" ,spin2cpp)
           ("spinsim" ,spinsim)))
        (synopsis "Complete development suite for Propeller micro-controllers")
        (description "This meta-package provides a complete environment for the
development with Parallax Propeller micro-controllers.  It includes the GCC
toolchain, the loader, the Openspin compiler, the Spin2cpp tool, and the Spin
simulator.")
        (home-page (package-home-page propeller-gcc))
        (license (package-license propeller-gcc))))))

(define-public binutils-vc4
  (let ((commit "708acc851880dbeda1dd18aca4fd0a95b2573b36"))
    (package
      (name "binutils-vc4")
      (version (string-append "2.23.51-0." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/puppeh/binutils-vc4")
                       (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1kdrz6fki55lm15rwwamn74fnqpy0zlafsida2zymk76n3656c63"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags '("--target=vc4-elf"
                             "--disable-werror"
                             "--enable-cgen-maint")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-cgen
             (lambda* (#:key inputs #:allow-other-keys)
               (copy-recursively (string-append (assoc-ref inputs "cgen")
                                                "/cgen") "cgen")
               #t))
           (add-after 'unpack-cgen 'fix-cgen-guile
             (lambda _
               (substitute* "opcodes/Makefile.in"
                 (("guile\\{,-\\}1.8") "guile"))
               (invoke "which" "guile"))))))
      (native-inputs
       `(("cgen"
          ,(origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/puppeh/cgen")
                       (commit "d8e2a9eb70425f180fdd5bfd032884b0855f2032")))
                (sha256
                 (base32
                  "14b3h2ji740s8zq5vwm4qdcxs4aa4wxi6wb9di3bv1h39x14nyr9"))))
         ("texinfo" ,texinfo)
         ("flex" ,flex)
         ("bison" ,bison)
         ("guile-1.8" ,guile-1.8)
         ("which" ,which)))
      (synopsis "Binutils for VC4")
      (description "This package provides @code{binutils} for VideoCore IV,
the Raspberry Pi chip.")
      (license license:gpl3+)
      (home-page "https://github.com/puppeh/vc4-toolchain/"))))

(define make-gcc-vc4
  (mlambda ()
    (let ((commit "0fe4b83897341742f9df65797474cb0feab4b377")
          (xgcc (cross-gcc "vc4-elf" #:xgcc gcc-6 #:xbinutils binutils-vc4)))
      (package
        (inherit xgcc)
        (name "gcc-vc4")
        (source (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/puppeh/gcc-vc4")
                        (commit commit)))
                  (file-name (string-append name
                                            "-"
                                            (package-version xgcc)
                                            "-checkout"))
                  (sha256
                   (base32
                    "0kvaq4s0assvinmmicwqp07d0wwldcw0fv6f4k13whp3q5909jnr"))
                  (patches
                   (search-patches "gcc-6-fix-buffer-size.patch"
                                   "gcc-6-fix-isl-includes.patch"))))
        (native-inputs
         (modify-inputs (package-native-inputs xgcc)
           (prepend flex)))
        (synopsis "GCC for VC4")
        (description "This package provides @code{gcc} for VideoCore IV,
the Raspberry Pi chip.")))))

(define-public imx-usb-loader
  ;; There are no proper releases.
  (let ((commit "30b43d69770cd69e84c045dc9dcabb1f3e9d975a")
        (revision "0"))
    (package
      (name "imx-usb-loader")
      ;; For the version string, see IMX_LOADER_VERSION in imx_loader.h.
      (version (git-version "0.2pre" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/boundarydevices/imx_usb_loader")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jdxbg63qascyl8x32njs9k9gzy86g209q7hc0jp74qyh0i6fwwc"))))
      (build-system gnu-build-system)
      (arguments
       (list #:test-target "tests"
             #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                  (string-append "prefix=" #$output))
             #:phases #~(modify-phases %standard-phases
                          (delete 'configure))))
      (native-inputs (list pkg-config))
      (inputs (list libusb))
      (home-page "https://github.com/boundarydevices/imx_usb_loader")
      (synopsis "USB and UART loader for i.MX5/6/7/8 series")
      (description "This utility downloads and executes code on Freescale
i.MX5/i.MX6/i.MX7 and Vybrid SoCs through the Serial Download Protocol (SDP).
Depending on the board, there is usually some kind of recovery button to bring
the SoC into serial download boot mode; check the documentation of your
hardware.  The utility support USB and UART as serial link.")
      (license license:lgpl2.1+))))

(define-public python-libmpsse
  (package
    (name "python-libmpsse")
    (version "1.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/daym/libmpsse")
              (commit (string-append "v" version))))
        (file-name "libmpsse-checkout")
        (sha256
          (base32
            "1rypfb96k2szqgygp3jnwg2zq9kwmfz0460dsahn3r2vkzml8wn7"))))
    (build-system gnu-build-system)
    (inputs
     (list libftdi python))
    (native-inputs
     (list pkg-config swig which))
    (arguments
     `(#:tests? #f ; No tests exist.
       #:parallel-build? #f  ; Would be buggy.
       #:make-flags
       (list (string-append "CFLAGS=-Wall -fPIC -fno-strict-aliasing -g -O2 "
                            "$(shell pkg-config --cflags libftdi1)"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environment-up
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((python (assoc-ref inputs "python")))
               (chdir "src")
               (setenv "PYDEV" (string-append python
                               "/include/python"
                               ,(version-major+minor (package-version python))))
               #t)))
         (replace 'install
           (lambda* (#:key inputs outputs make-flags #:allow-other-keys #:rest args)
             (let* ((out (assoc-ref outputs "out"))
                    (out-python (string-append out
                                               "/lib/python"
                                               ,(version-major+minor (package-version python))
                                               "/site-packages"))
                    (install (assoc-ref %standard-phases 'install)))
               (install #:make-flags (cons (string-append "PYLIB=" out-python)
                                           make-flags))))))))
    (home-page "https://code.google.com/archive/p/libmpsse/")
    (synopsis "Python library for MPSSE SPI I2C JTAG adapter by FTDI")
    (description "This package provides a library in order to support the
MPSSE (Multi-Protocol Synchronous Serial Engine) adapter by FTDI that can do
SPI, I2C, JTAG.")
    (license license:gpl2+)))

(define-public picprog
  (package
    (name "picprog")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.iki.fi/hyvatti/pic/picprog-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r04hg1n3v2jf915qr05la3q9cxy7a5jnh9cc98j04lh6c9p4x85"))
              (patches (search-patches "picprog-non-intel-support.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests exist.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr/local") (assoc-ref outputs "out"))
               ((" -o 0 -g 0 ") " ")
               (("testport") ""))
             #t))
         (add-before 'install 'mkdir
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/man/man1"))
               #t)))
         (delete 'configure))))
    (synopsis "Programs Microchip's PIC microcontrollers")
    (description "This program programs Microchip's PIC microcontrollers.")
    (home-page "https://hyvatti.iki.fi/~jaakko/pic/picprog.html")
    (license license:gpl3+)))

(define-public fc-host-tools
  (package
    (name "fc-host-tools")
    (version "15")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.freecalypso.org/pub/GSM/"
                                  "FreeCalypso/fc-host-tools-r" version ".tar.bz2"))
              (sha256
               (base32
                "17v3xc44mmlvp0irwm1p55zdgzd31ic3nsjxnv8y28a1i85103cv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests exist.
       #:make-flags
       (list (string-append "INSTALL_PREFIX=" %output)
             (string-append "INCLUDE_INSTALL_DIR=" %output "include/rvinterf"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-installation-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (cons* "miscutil/c139explore"
                                 "miscutil/pirexplore"
                                 (find-files "." "^(.*\\.[ch]|Makefile)$"))
               (("/opt/freecalypso/bin/fc-simtool")
                "fc-simtool")
               (("/opt/freecalypso/bin/fc-uicc-tool")
                "fc-uicc-tool")
               (("/opt/freecalypso/loadtools")
                (string-append (assoc-ref outputs "out") "/lib/freecalypso/loadtools"))
               (("\\$\\{INSTALL_PREFIX\\}/loadtools")
                (string-append (assoc-ref outputs "out") "/lib/freecalypso/loadtools"))
               (("\\$\\{INSTALL_PREFIX\\}/target-bin")
                (string-append (assoc-ref outputs "out") "/lib/freecalypso/target-bin"))
               (("/opt/freecalypso")
                (assoc-ref outputs "out")))
             #t))
         (delete 'configure))))
    (inputs
     (list libx11))
    (synopsis "Freecalypso host tools")
    (description "This package provides some tools for debugging FreeCalypso phones and the FreeCalypso FCDEV3B dev board.

@enumerate
@item fc-e1decode: Decodes a binary Melody E1 file into an ASCII source file.
@item fc-e1gen: Encodes an ASCII Melody E1 file into a binary Melody E1 file.
@item fc-fr2tch: Converts a GSM 06.10 speech recording from libgsm to hex
strings of TCH bits to be fed to the GSM 05.03 channel encoder of a TI
Calypso GSM device.
@item fc-tch2fr: Converts hex strings of TCH bits to libgsm.
@item fc-gsm2vm: utility converts a GSM 06.10 speech sample from the libgsm
source format into a voice memo file that can be uploaded into the FFS of a
FreeCalypso device and played with the audio_vm_play_start() API or the
AT@@VMP command that invokes the latter.
@item fc-rgbconv: Convers RGB 5:6:5 to RGB 8:8:8 and vice versa.
@item rvinterf: Communicates with a TI Calypso GSM device via RVTMUX.
@item rvtdump: produces a human-readable dump of all output emitted by a
TI-based GSM fw on the RVTMUX binary packet interface.
@item fc-shell: FreeCalypso firmwares have a feature of our own invention
(not present in any pre-existing ones) to accept AT commands over the RVTMUX
interface.  It is useful when no second UART is available for a dedicated
standard AT command interface.  fc-shell is the tool that allows you to send
AT commands to the firmware in this manner.
@item fc-memdump: Captures a memory dump from a GSM device.
@item fc-serterm: Trivial serial terminal.  Escapes binary chars.
@item fc-fsio: Going through rvinterf, this tool connects to GSM devices and
allows you to manipulate the device's flash file system.
@item tiaud-compile: Compiles an audio mode configuration table for TI's
Audio Service from our own ASCII source format into the binary format for
uploading into FreeCalypso GSM device FFS with fc-fsio.
@item tiaud-decomp: Decodes TI's audio mode configuration files read out of
FFS into our own ASCII format.
@item tiaud-mkvol: Generates the *.vol binary files which need to accompany
the main *.cfg ones.
@item fc-compalram: Allows running programs on the device without writing
them to flash storage.
@item fc-xram: Allows running programs on the device without writing them
to flash storage.
@item fc-iram: Allows running programs on the device without writing them
to flash storage.
@item fc-loadtool: Writes programs to the device's flash storage.
@item fc-simint: Loads and runs simagent on the phone, then calls fc-simtool
(see @url{https://www.freecalypso.org/hg/fc-sim-tools,fc-sim-tools
repository}) on the host to connect to it.
@item pirffs: Allows listing and extracting FFS content captured as a raw
flash image from Pirelli phones.
@item mokoffs: Allows listing and extracting FFS content captured as a raw
flash image from OpenMoko phones.
@item tiffs: Allows listing and extracting FFS content captured as a raw
flash image from TI phones.
@item c139explore: Run-from-RAM program for C139 phones that
exercises their peripheral hardware: LCD, keypad backlight, buzzer, vibrator.
@item pirexplore: Run-from-RAM program for Pirelli DP-L10 phones that
exercises their peripheral hardware, primarily their LCD.
@item tfc139: Breaks into Mot C1xx phones via shellcode injection, allowing
you to reflash locked phones with new firmware with fc-loadtool.
@item ctracedec: GSM firmwares built in TI's Windows environment have a
compressed trace misfeature whereby many of the ASCII strings
in debug trace messages get replaced with numeric indices at
build time, and these numeric indices are all that gets emitted
on the RVTMUX serial channel.  This tools decodes these numeric indices
back to strings in trace output.
@item fc-cal2text: This utility takes a dump of TI's /gsm/rf flash file system
directory subtree as input (either extracted in vitro with tiffs
or read out in vivo with fc-fsio) and converts all RF tables
found therein into a readable ASCII format.
@item imei-luhn: Computes or verifies the Luhn check digit of an IMEI number.
@item fc-dspapidump: Reads and dumps the contents of the DSP API RAM in a
target Calypso GSM device.
@item fc-vm2hex: Converts the old-fashioned (non-AMR) voice memo files read
out of FFS into hex strings.
@item fc-buzplay: Plays piezoelectic buzzer melodies on an actual
Calypso device equipped with such a buzzer (Mot C1xx, TI's D-Sample board,
our planned future HSMBP) by loading a buzplayer agent onto the target and
feeding melodies to be played to it.
@item fc-tmsh: TI-based GSM firmwares provide a rich set of Test Mode commands
that can be issued through the RVTMUX (debug trace) serial channel.
This program is our test mode shell for sending Test Mode commands to targets
and displaying decoded target responses.
@item fcup-smsend: Send a short message via SMS
@item fcup-smsendmult: Send multiple short messages via SMS in one go
@item fcup-smsendpdu: Send multiple short messages given in PDU format via SMS
@item sms-pdu-decode: Decode PDU format messages
@item fc-dspromdump: Dump DSP ROM.
@item pcm-sms-decode: Decode /pcm/SMS binary files read out of FFS maintained
by Pirelli DP-L10.  Display the SMS in human-readable form.
@item srec-regions: Parse S-record (TI's *.m0), identify the set of
discontiguous regions into which this SREC image deposits bits, and list
these identified regions.
@end enumerate")
    (home-page "https://www.freecalypso.org/")
    (license license:public-domain)))

(define-public stcgal
  (package
    (name "stcgal")
    (version "1.10")
    (source (origin
              ;; The "doc" subdirectory referred to by stcgal's setup.py is
              ;; missing from the source distribution on PyPI so we fetch
              ;; directly from the project's git repository instead.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grigorig/stcgal")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04hsj49sw5mb6swhd3sdsm7dzwp1frnzpmq70wgsn5vmjavb1ka8"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pyserial python-pyusb python-tqdm))
    (native-inputs
     ;; For tests.
     (list python-pyyaml))
    (home-page "https://github.com/grigorig/stcgal")
    (synopsis "Programmer for STC 8051-compatible microcontrollers")
    (description "stcgal is a command-line flash-programming tool for STC
MCU's line of Intel 8051-compatible microcontrollers, including those in the
STC89, STC90, STC10, STC11, STC12, STC15, STC8 and STC32 series.")
    (license license:expat)))

(define-public stlink
  (package
    (name "stlink")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/stlink-org/stlink")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1g5ahnj400sdf75k3xafawa6x0pzz7s86nqnfd65gqjr3bdlhlc6"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                      ;no tests
           #:configure-flags
           #~(let* ((etc (in-vicinity #$output "etc"))
                    (modprobe (in-vicinity etc "modprobe.d"))
                    (udev-rules (in-vicinity etc "udev/rules.d")))
               (list (string-append "-DSTLINK_UDEV_RULES_DIR=" udev-rules)
                     (string-append "-DSTLINK_MODPROBED_DIR=" modprobe)))))
    (inputs
     (list libusb))
    (synopsis "Programmer for STM32 Discovery boards")
    (description "This package provides a firmware programmer for the STM32
Discovery boards.  It supports two versions of the chip: ST-LINK/V1 (on
STM32VL discovery kits) and ST-LINK/V2 (on STM32L discovery and later kits).
Two different transport layers are used: ST-LINK/V1 uses SCSI passthru
commands over USB, and ST-LINK/V2 and ST-LINK/V2-1 (seen on Nucleo boards) use
raw USB commands.")
    (home-page "https://github.com/stlink-org/stlink")
    ;; The flashloaders/stm32l0x.s and flashloaders/stm32lx.s source files are
    ;; licensed under the GPLv2+.
    (license (list license:bsd-3 license:gpl2+))))

(define-public west
  (package
    (name "west")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "west" version))
       (sha256
        (base32
         "1hw9qas8ry8prn24iqka8kw2nv7ndxr95mvwr5lww53w2sr7p807"))))
    (propagated-inputs
     (list python-colorama
           python-packaging
           python-pykwalify
           python-pyyaml))
    (build-system python-build-system)
    (home-page "https://github.com/zephyrproject-rtos/west")
    (synopsis "Zephyr RTOS Project meta-tool")
    (description "West is the swiss-army knife command line tool of the Zephyr
project.  Its built-in commands provide a multiple repository management
system with features inspired by Google’s Repo tool and Git submodules.  West
simplifies configuration and is also pluggable: you can write your own west
\"extension commands\" which add additional features to west.  Zephyr uses
this feature to provide conveniences for building applications, flashing and
debugging them, and more.")
    (license license:expat)))

(define-public ebusd
  (package
    (name "ebusd")
    (version "25.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/john30/ebusd")
                     (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1k85vzjhhya7r41nid5yylr7jyvl09455hpny6wrjkipz68icgdf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/ebusd/main.h"
              (("#define CONFIG_PATH .*")
               (string-append "#define CONFIG_PATH \"" (assoc-ref inputs "config")
                              "/ebusd-2.1.x/\"\n")))))
         (add-after 'install 'install-config
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((config-destination
                    (string-append (assoc-ref outputs "out")
                                   "/share/ebusd")))
               (copy-recursively (string-append (assoc-ref inputs "config")
                                                "/ebusd-2.1.x")
                                 config-destination)
               #t))))))
    (inputs
     (list mosquitto))
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("config"
        ,(origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/john30/ebusd-configuration")
                     (commit "666c0f6b9c4d7545eff7f43ab28a1c7baeab7913")))
              (file-name "config-checkout")
              (sha256
               (base32
                "0yxnx8p4lbk614l16854r9s9d8s9c7ixgczfs8mph94xz0wkda7x"))))))
    (synopsis "Daemon for communicating with eBUS devices")
    (description "This package provides @command{ebusd}, a daemon for
handling communication with eBUS devices connected to a 2-wire bus system
(\"energy bus\" used by numerous heating systems).")
    (home-page "https://ebusd.eu/")
    (license license:gpl3+)))

(define-public ucsim
  (package
    (name "ucsim")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://mazsola.iit.uni-miskolc.hu/ucsim/download/"
                    "v" (version-major+minor version) ".x/"
                    "ucsim_" version "_orig.tar.gz"))
              (sha256
               (base32
                "1zdvzfhdsbydyyjy5rf2934bn06skdlnky6l9ngbp2k645g0ynlh"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-makefiles
            (lambda _
              (substitute* (find-files "." "(\\.mk$|\\.in$)")
                (("/bin/sh") (which "sh")))

              ;; Ensure the documentation is installed to the correct path,
              ;; without a duplicate "ucsim" segment (necessary as we are
              ;; building μCsim outside of SDCC).
              (substitute* "doc/Makefile.in"
                (("@docdir@/ucsim") "@docdir@")))))
      ;; μCsim's regression-test suite is of little use in this context since
      ;; it doesn't stop or return an error code when it encounters a problem.
      #:tests? #f))
    (inputs
     (list ncurses))
    (native-inputs
     (list bison flex))
    (outputs '("out" "doc"))
    (home-page "http://mazsola.iit.uni-miskolc.hu/ucsim/")
    (synopsis "Simulators for various microcontroller families")
    (description "μCsim is a collection of software simulators for
microcontrollers in the Atmel AVR; Fairchild F8; Intel MCS-51 (8051) and 8085;
MOS Technology 6502; Motorola 6800, 6809, 68HC08, 68HC11 and 68HC12; P1516;
Padauk PDK13, PDK14 and PDK15; STMicroelectronics ST7 and STM8; Xilinx
PicoBlaze; and Zilog Z80 families, plus many of their variants.")
    (license license:gpl2+)))

(define-public sdcc
  (package
    (name "sdcc")
    (version "4.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/sdcc/sdcc"
                    "/" version "/sdcc-src-" version ".tar.bz2"))
              (sha256
               (base32
                "0xbaj3vx5cp3na1kmyhy4jvhcqwrg648scjbykgq0xmibqb1535f"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   ;; Remove non-free source files.
                   (delete-file-recursively "device/non-free")
                   ;; Remove bundled μCsim source.
                   (delete-file-recursively "sim")))
              (patches (search-patches "sdcc-disable-non-free-code.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; GPUTILS is required for the PIC ports, but the licensing status of
         ;; some of the files contained in its distribution is unclear (see
         ;; https://issues.guix.gnu.org/44557).  For this reason it is not yet
         ;; available as a package in Guix.
         "--disable-pic14-port"
         "--disable-pic16-port"

         ;; Do not build or install the bundled copy of μCsim, for which Guix
         ;; has its own package.
         "--disable-ucsim")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-makefiles
            (lambda _
              (substitute* (find-files "." "(\\.mk$|\\.in$)")
                (("/bin/sh") (which "sh")))
              ;; --disable-ucsim disables sdcc-misc, patch it back in.
              (substitute* "Makefile.in"
                (("debugger/mcs51" line)
                 (string-append line  "\n"
                                "TARGETS += sdcc-misc\n"
                                "PKGS += $(SDCC_MISC)")))))
          (add-after 'patch-makefiles 'embed-absolute-ucsim-reference
            (lambda _
              ;; Embed in the debugger an absolute reference to the MCS-51
              ;; simulator from Guix's μCsim package to ensure it is always
              ;; available.
              (substitute* "debugger/mcs51/sdcdb.c"
                (("s51")
                 (string-append #$(this-package-input "ucsim")
                                "/bin/s51"))))))))
    (inputs
     (list readline ucsim))
    (native-inputs
     (list bison boost flex python-2 texinfo zlib))
    (home-page "https://sdcc.sourceforge.net/")
    (synopsis "C compiler suite for 8-bit microcontrollers")
    (description "SDCC is a retargetable, optimizing Standard C compiler suite
that targets 8-bit microcontrollers in the Intel MCS-51 (8051); MOS Technology
6502; Motorola 68HC08; Padauk PDK13, PDK14 and PDK15; STMicroelectronics STM8;
and Zilog Z80 families, plus many of their variants.")
    (license (list license:gpl2+
                   license:gpl3+
                   license:lgpl2.0+
                   license:lgpl2.1+
                   license:lgpl3+
                   license:public-domain
                   license:zlib))))

(define-public python-psptool
  (package
    (name "python-psptool")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "psptool" version))
              (sha256
               (base32
                "1kx0xpfx67m4zclk4gs97wiwjms8i7z4f6b6m68y8sfgpshy4rf3"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; IPython is not used by the package at all
                  (substitute* '("psptool/directory.py" "psptool/entry.py")
                    (("from IPython.*") ""))))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cryptography python-prettytable))
    (home-page "https://github.com/PSPReverse/psptool")
    (synopsis "Tool for dealing with AMD binary blobs")
    (description "PSPTool is a tool for dealing with AMD binary blobs")
    (license license:gpl3+)))

(define-public agent-proxy
  (let ((commit "8927798a71d246871ea8fc22b4512296a3fa1765")
        (revision "0"))
    (package
      (name "agent-proxy")
      (version (git-version "1.98" revision commit))
      (home-page
       "https://git.kernel.org/pub/scm/utils/kernel/kgdb/agent-proxy.git")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bxkzwsqfld4pknmiq8j3k55pv90n8s6kzh0xh42bhy2jv1wxz2z"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'build 'build-kdmx
             (lambda _
               (invoke "make" "-C" "kdmx")
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "agent-proxy" bin)
                 (install-file "kdmx/kdmx" bin)
                 #t))))))
      (synopsis "Proxies to run kgdb/gdbserver and console on a serial port")
      (description "These programs are proxies allowing to run kgdb/gdbserver
and console on a single serial port.  agent-proxy creates network sockets,
whereas kdmx creates pseudo-ttys.")
      (license license:gpl2))))

(define-public mbed-tools
  (package
    (name "mbed-tools")
    (version "7.53.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mbed-tools" version))
       (sha256
        (base32
         "0gdmyxy97bqr9bmkg90v3axmrr2db734nwzq2l05z84x9qiarc9i"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               (("\"Click>=7.1,<8\"")
                "\"Click>=7.1\""))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Remove this failing test.
               (delete-file "tests/ci_scripts/test_sync_board_db.py")
               (invoke "pytest" "-vv")))))))
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-factory-boy
           python-requests-mock
           python-semver))
    (propagated-inputs
     (list python-dotenv
           python-click
           python-pdoc3
           python-gitpython
           python-tqdm
           python-tabulate
           python-requests
           python-psutil
           python-pyudev
           python-typing-extensions
           python-jinja2
           python-pyserial))
    (build-system python-build-system)
    (home-page "https://github.com/ARMmbed/mbed-tools")
    (synopsis "ARM Mbed command line tools")
    (description "This package is the successor of @code{mbed-cli}.  It
provides command line tools for Mbed OS to detect Mbed enabled devices
connected by USB, checkout Mbed projects and perform builds amongst other
operations.")
    (license license:asl2.0)))

(define-public ts4900-utils
  ;; There are no proper release nor tag; use the latest commit.
  (let ((revision "0")
        (commit "e10a12f8050d1d1229e711c7cfab8a0d5d93ee58"))
    (package
      (name "ts4900-utils")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/embeddedTS/ts4900-utils")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vr8i425qijbwgbc10av3wr35p3x11wy6y442w0ja0yny7si8wp8"))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf automake))
      (home-page "https://github.com/embeddedTS/ts4900-utils")
      (synopsis "Utilities for the TS-4900 board family")
      (description "This package contains utilities useful for boards of the
TS-4900 family.  The included commands are:
@itemize @code
@item adc8390
@item gpioctl
@item isl12020rtc
@item load_fpga
@item nvramctl
@item tshwctl
@item tsmicroctl
@item tsmicroupdate
@item tssilomon
@end itemize")
      (license license:bsd-2))))
