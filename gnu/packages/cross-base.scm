;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2018, 2020, 2023-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2019, 2023-2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Carl Dong <contact@carldong.me>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
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

(define-module (gnu packages cross-base)
  #:use-module (gnu packages)
  #:use-module (gnu packages avr)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages mingw)
  #:use-module (guix memoization)
  #:use-module (guix platform)
  #:use-module (guix packages)
  #:use-module (guix diagnostics)
  #:use-module (guix download)
  #:use-module (guix i18n)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (cross-binutils
            cross-libc
            cross-gcc
            cross-mig
            cross-kernel-headers
            cross-gcc-toolchain))

(define-syntax %xgcc
  ;; GCC package used as the basis for cross-compilation.  It doesn't have to
  ;; be 'gcc' and can be a specific variant such as 'gcc-4.8'.
  ;;
  ;; Note: This is a macro so that we do not refer to 'gcc' from the top
  ;; level, which would lead to circular-dependency issues.
  (identifier-syntax gcc-14))

(define %gcc-include-paths
  ;; Environment variables for header search paths.
  ;; Note: See <http://bugs.gnu.org/22186> for why not 'CPATH'.
  '("C_INCLUDE_PATH"
    "CPLUS_INCLUDE_PATH"
    "OBJC_INCLUDE_PATH"
    "OBJCPLUS_INCLUDE_PATH"))

(define %gcc-cross-include-paths
  ;; Search path for target headers when cross-compiling.
  (map (cut string-append "CROSS_" <>) %gcc-include-paths))

(define (cross p target)
  (package (inherit p)
    (name (string-append (package-name p) "-cross-" target))
    (arguments
     (substitute-keyword-arguments (package-arguments p)
       ((#:configure-flags flags #~'())
        #~(cons #$(string-append "--target=" target)
                #$flags))))))

(define (contains-keyword? args)
  "Check if ARGS contains a keyword object."
  (find keyword? args))

(define* (cross-binutils . args)
  (if (or (= (length args) 1) (contains-keyword? args))
      (apply cross-binutils* args)
      (apply cross-binutils/deprecated args)))

(define* (cross-binutils/deprecated target #:optional (binutils binutils))
  (warning (G_ "'cross-binutils' must be used with keyword arguments~%"))
  (cross-binutils* target #:binutils binutils))

(define (cross-binutils-package target)
  "Returns the default package to use for a cross-Binutils for TARGET."
  (cond
    ;; The xtensa-ath9k-elf target is used solely to build the firmware for
    ;; ath9k devices, the patches to binutils have not been updated and
    ;; only apply to binutils@2.33.
    ((string=? target "xtensa-ath9k-elf") binutils-2.33)
    (else binutils)))

(define* (cross-binutils* target
                          #:key
                          (binutils (cross-binutils-package target)))
  "Return a cross-Binutils for TARGET using BINUTILS."
  (let ((binutils (package
                    (inherit binutils)
                    (arguments
                     (substitute-keyword-arguments (package-arguments
                                                    binutils)
                       ((#:configure-flags flags)
                        ;; Build with `--with-sysroot' so that ld honors
                        ;; DT_RUNPATH entries when searching for a needed
                        ;; library.  This works because as a side effect
                        ;; `genscripts.sh' sets `USE_LIBPATH=yes', which tells
                        ;; elf32.em to use DT_RUNPATH in its search list.
                        ;; See <http://sourceware.org/ml/binutils/2013-05/msg00312.html>.
                        ;;
                        ;; In theory choosing / as the sysroot could lead ld
                        ;; to pick up native libs instead of target ones.  In
                        ;; practice the RUNPATH of target libs only refers to
                        ;; target libs, not native libs, so this is safe.
                        #~(cons "--with-sysroot=/" #$flags)))))))

    ;; For xtensa-ath9k-elf, apply Qualcomm's patch.
    (cross (cond ((string=? target "xtensa-ath9k-elf")
                  (package-with-patches binutils
                                        (search-patches
                                         "ath9k-htc-firmware-binutils.patch")))
                 ((target-mingw? target)
                  (package-with-extra-patches
                   (package-with-extra-configure-variable
                    ;; mingw binutils does not work correctly when configured
                    ;; with `--enable-compressed-debug-sections`. An error
                    ;; like the following will occur whenever you try to link:
                    ;;
                    ;;   x86_64-w64-mingw32-ld: final link failed: bad value
                    ;;
                    ;; TODO: This seems like a deeper problem that warrants
                    ;; deeper investigation.
                    binutils "--enable-compressed-debug-sections" "no")
                   (search-patches "binutils-mingw-w64-timestamp.patch"
                                   "binutils-mingw-w64-deterministic.patch")))
                 (else binutils))
           target)))

(define (cross-gcc-arguments target xgcc libc)
  "Return build system arguments for a cross-gcc for TARGET, using XGCC as the
base compiler and using LIBC (which may be either a libc package or #f.)"
  ;; Set the current target system so that 'glibc-dynamic-linker' returns the
  ;; right name.
  (parameterize ((%current-target-system target))
    ;; Disable stripping as this can break binaries, with object files of
    ;; libgcc.a showing up as having an unknown architecture.  See
    ;; <http://lists.fedoraproject.org/pipermail/arm/2010-August/000663.html>
    ;; for instance.
    (let ((args `(#:strip-binaries? #f
                  ,@(package-arguments xgcc))))
      (substitute-keyword-arguments args
        ((#:configure-flags flags)
         #~(append (list #$(string-append "--target=" target)
                         #$@(if libc
                                #~( ;; Disable libcilkrts because it is not
                                   ;; ported to GNU/Hurd.
                                   "--disable-libcilkrts"
                                   ;; When building a cross compiler, --with-sysroot is
                                   ;; implicitly set to "$gcc_tooldir/sys-root".  This does
                                   ;; not work for us, because --with-native-system-header-dir
                                   ;; is searched for relative to this location.  Thus, we set
                                   ;; it to "/" so GCC is able to find the target libc headers.
                                   ;; This is safe because in practice GCC uses CROSS_CPATH
                                   ;; & co to separate target and host libraries.
                                   "--with-sysroot=/")
                                #~( ;; Disable features not needed at this stage.
                                   "--disable-shared" "--enable-static"
                                   "--enable-languages=c,c++"

                                   ;; libstdc++ cannot be built at this stage
                                   ;; ("Link tests are not allowed after
                                   ;; GCC_NO_EXECUTABLES.").
                                   "--disable-libstdc++-v3"

                                   "--disable-threads" ;libgcc, would need libc
                                   "--disable-libatomic"
                                   "--disable-libmudflap"
                                   "--disable-libgomp"
                                   "--disable-libmpx"
                                   "--disable-libssp"
                                   "--disable-libquadmath"
                                   "--disable-decimal-float" ;would need libc
                                   "--disable-libcilkrts"

                                   ;; When target is any OS other than 'none' these
                                   ;; libraries will fail if there is no libc
                                   ;; present. See
                                   ;; <https://lists.gnu.org/archive/html/guix-devel/2016-02/msg01311.html>
                                   "--disable-libitm"
                                   "--disable-libvtv"
                                   "--disable-libsanitizer"
                                   ))

                         ;; Install cross-built libraries such as libgcc_s.so in
                         ;; the "lib" output.
                         #$@(if libc
                                #~((string-append "--with-toolexeclibdir="
                                                  (assoc-ref %outputs "lib")
                                                  "/" #$target "/lib"))
                                #~())


                         #$@(if (target-avr? target)
                                #~("--enable-multilib")
                                #~())


                         #$@(if (and libc (target-avr? target))
                                #~(;; By default GCC will attemp to compile
                                   ;; some libraries for other languages (objc,
                                   ;; fortran) but compilation fails for AVR.
                                   "--enable-languages=c,c++"
                                   (string-append "--with-native-system-header-dir="
                                                  #$libc "/" #$target "/include"))
                                #~()))

                   (remove
                     (lambda (flag)
                       (or (string-prefix? "--enable-languages" flag)
                           (and #$libc
                                #$(target-avr? target)
                                (string-prefix? "--with-native-system-header-dir"
                                                flag))
                           (and #$(target-avr? target)
                                (string=? flag "--disable-multilib"))))
                     #$flags)))
        ((#:make-flags flags)
         (if libc
             #~(let ((libc (assoc-ref %build-inputs "libc"))
                     (lib-prefix (if #$(target-avr? target)
                                     (string-append "/" #$target)
                                     "")))
                ;; FLAGS_FOR_TARGET are needed for the target libraries to receive
                ;; the -Bxxx for the startfiles.
                 (cons (string-append "FLAGS_FOR_TARGET=-B"
                                      libc lib-prefix "/lib")
                       #$flags))
             flags))
        ((#:phases phases)
         #~(cross-gcc-build-phases #$target #$phases))))))

(define (cross-gcc-patches xgcc target)
  "Return GCC patches needed for XGCC and TARGET."
  (cond ((string-prefix? "xtensa-" target)
         ;; Patch by Qualcomm needed to build the ath9k-htc firmware.
         (search-patches "ath9k-htc-firmware-gcc.patch"))
        ((target-mingw? target)
         (append (if (not (version>=? (package-version xgcc) "13.0"))
                     (search-patches "gcc-4.9.3-mingw-gthr-default.patch")
                     '())
                 (if (version>=? (package-version xgcc) "7.0")
                     (search-patches "gcc-7-cross-mingw.patch")
                     '())))
        (else '())))

(define (cross-gcc-snippet target)
  "Return GCC snippet needed for TARGET."
  `(begin
     ,@(if (target-mingw? target)
           '((copy-recursively "libstdc++-v3/config/os/mingw32-w64"
                               "libstdc++-v3/config/os/newlib"))
           '())
     ;; TOOLDIR_BASE_PREFIX is erroneous when using a separate "lib"
     ;; output. Specify it correctly, otherwise GCC won't find its shared
     ;; libraries installed in the "lib" output.  See:
     ;; https://lists.gnu.org/archive/html/bug-guix/2020-03/msg00196.html.
     (substitute* "gcc/Makefile.in"
       (("-DTOOLDIR_BASE_PREFIX=[^ ]*")
        "-DTOOLDIR_BASE_PREFIX=\\\"../../../../\\\""))
     #t))

(define (cross-gcc-search-paths target)
  "Return list of GCC search path specifications needed for TARGET."
  (cons (search-path-specification
          (variable "CROSS_LIBRARY_PATH")
          (files `("lib" "lib64"
                   ,@(list (string-append target "/lib")
                           (string-append target "/lib64")))))

        (map (lambda (variable)
               (search-path-specification
                 (variable variable)

                 ;; Add 'include/c++' here so that <cstdlib>'s
                 ;; "#include_next <stdlib.h>" finds GCC's
                 ;; <stdlib.h>, not libc's.
                 (files (match variable
                          ("CROSS_CPLUS_INCLUDE_PATH"
                           `("include/c++" "include"
                             ,@(list (string-append target "/include/c++")
                                     (string-append target "/include"))))
                          (_
                           `("include"
                             ,(string-append target "/include")))))))
             %gcc-cross-include-paths)))

(define* (cross-gcc target
                    #:key
                    (xgcc %xgcc)
                    (xbinutils (cross-binutils target))
                    (libc #f))
  "Return a cross-compiler for TARGET, where TARGET is a GNU triplet.  Use
XGCC as the base compiler.  Use XBINUTILS as the associated cross-Binutils.
If LIBC is false, then build a GCC that does not target a libc; otherwise,
target that libc."
  (package
    (inherit xgcc)
    (name (string-append "gcc-cross-"
                         (if libc "" "sans-libc-")
                         target))
    (source
     (origin
       (inherit
        (package-source xgcc))
       (patches
        (append
         (origin-patches (package-source xgcc))
         (append (cond
                  ((version>=? (package-version xgcc) "13.0")
                   (search-patches "gcc-13-cross-system-header-dir.patch"
                                   "gcc-12-cross-environment-variables.patch"
                                   "gcc-cross-gxx-include-dir.patch"))
                  ((version>=? (package-version xgcc) "12.0")
                   (search-patches "gcc-12-cross-environment-variables.patch"
                                   "gcc-cross-gxx-include-dir.patch"))
                  ((version>=? (package-version xgcc) "10.0")
                   (search-patches "gcc-10-cross-environment-variables.patch"
                                   "gcc-cross-gxx-include-dir.patch"))
                  ((version>=? (package-version xgcc) "8.0")
                   (search-patches "gcc-8-cross-environment-variables.patch"))
                  ((version>=? (package-version xgcc) "6.0")
                   (search-patches "gcc-7-cross-toolexeclibdir.patch"
                                   "gcc-6-cross-environment-variables.patch"))
                  (else
                   (search-patches "gcc-cross-environment-variables.patch")))
                 (cross-gcc-patches xgcc target))))
       (modules '((guix build utils)))
       (snippet
        (cross-gcc-snippet target))))

    (outputs '("out" "lib"))

    (arguments
     `(#:implicit-inputs? #f
       #:imported-modules ((gnu build cross-toolchain)
                           ,@%default-gnu-imported-modules)
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (gnu build cross-toolchain)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 regex))

       ,@(cross-gcc-arguments target xgcc libc)))

    (native-inputs
     `(("ld-wrapper-cross" ,(make-ld-wrapper
                             (string-append "ld-wrapper-" target)
                             #:target (const target)
                             #:binutils xbinutils))
       ("binutils-cross" ,xbinutils)

       ,@(let ((inputs (append (package-inputs xgcc)
                               (fold alist-delete (%final-inputs)
                                     '("libc" "libc:static"))

                               ;; Call it differently so that the builder can
                               ;; check whether the "libc" input is #f.
                               `(("libc-native"
                                  ,@(assoc-ref (%final-inputs) "libc"))
                                 ("libc-native:static"
                                  ,@(assoc-ref (%final-inputs)
                                               "libc:static"))))))
           (cond
            ((target-mingw? target)
             (if libc
                 `(,@inputs
                   ("libc" ,libc))
                 `(,@inputs
                   ("mingw-source" ,(package-source mingw-w64)))))
            ((and libc (target-avr? target))
             `(,@inputs
               ("libc" ,libc)))
            (libc
             `(,@inputs
               ("libc" ,libc)
               ("libc:static" ,libc "static")
               ("xkernel-headers"       ;the target headers
                ,@(assoc-ref (package-propagated-inputs libc)
                             "kernel-headers"))))
            (else inputs)))))

    (inputs '())

    ;; Only search target inputs, not host inputs.
    (search-paths (cross-gcc-search-paths target))
    (native-search-paths '())))

(define* (cross-kernel-headers . args)
  (if (or (= (length args) 1) (contains-keyword? args))
      (apply cross-kernel-headers* args)
      (apply cross-kernel-headers/deprecated args)))

(define* (cross-kernel-headers/deprecated target
                                          #:optional
                                          (linux-headers
                                           (if (target-loongarch64? target)
                                               linux-libre-headers-5.19.17
                                               linux-libre-headers))
                                          (xgcc (cross-gcc target))
                                          (xbinutils (cross-binutils target)))
  (warning (G_ "'cross-kernel-headers' must be used with keyword arguments~%"))
  (cross-kernel-headers* target
                         #:linux-headers linux-headers
                         #:xgcc xgcc
                         #:xbinutils xbinutils))

(define* (cross-gnumach-headers target
                                #:key
                                (xgcc (cross-gcc target))
                                (xbinutils (cross-binutils target)))
  (package
    (inherit gnumach-headers)
    (name (string-append (package-name gnumach-headers)
                         "-cross-" target))
    (arguments
     (substitute-keyword-arguments (package-arguments gnumach-headers)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            ;; Cheat by setting the host_cpu variable manually, since using
            ;; --host= would require a working cross-compiler, which we don't
            ;; have yet.
            (add-after 'unpack 'substitute-host-cpu
              (lambda _
                (substitute* "configure.ac"
                  (("AC_CANONICAL_HOST")
                   #$(string-append
                      "host_cpu="
                      (match target
                        ((? target-x86-32?)
                         "i386")
                        ((? target-x86-64?)
                         "x86_64")))))))))))
    (native-inputs
     (modify-inputs (package-native-inputs gnumach-headers)
       (prepend xgcc xbinutils)))))

(define* (cross-mig target
                    #:key
                    (xgcc (cross-gcc target))
                    (xbinutils (cross-binutils target)))
  "Return a cross-mig for TARGET, where TARGET is a GNU triplet.  Use XGCC as
the base compiler.  Use XBINUTILS as the associated cross-Binutils."
  (define xgnumach-headers
    (cross-gnumach-headers target
                           #:xgcc xgcc
                           #:xbinutils xbinutils))
  (package
    (inherit mig)
    (name (string-append "mig-cross"))
    (arguments
     (substitute-keyword-arguments (package-arguments mig)
       ((#:configure-flags flags #~'())
        #~(list #$(string-append "--target=" target)))
       ((#:tests? _ #f)
        #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'configure 'set-cross-headers-path
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((mach #+xgnumach-headers)
                       (cpath (string-append mach "/include")))
                  (for-each (lambda (variable)
                              (setenv variable cpath))
                            '#$%gcc-cross-include-paths))))))))
    (propagated-inputs
     (list xgnumach-headers))
    (native-inputs
     (modify-inputs (package-native-inputs mig)
       (prepend xgcc xbinutils)))))

(define* (cross-kernel-headers* target
                                #:key
                                (linux-headers (if (target-loongarch64? target)
                                                   linux-libre-headers-5.19.17
                                                   linux-libre-headers))
                                (xgcc (cross-gcc target))
                                (xbinutils (cross-binutils target)))
  "Return headers depending on TARGET."

  (define xlinux-headers
    (package
      (inherit linux-headers)
      (name (string-append (package-name linux-headers)
                           "-cross-" target))
      (arguments
       (substitute-keyword-arguments
           `(#:implicit-cross-inputs? #f
             ,@(package-arguments linux-headers))
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               (lambda _
                 (setenv "ARCH" ,(platform-linux-architecture
                                  (lookup-platform-by-target target)))
                 (format #t "`ARCH' set to `~a' (cross compiling)~%"
                         (getenv "ARCH"))

                 (invoke "make" ,(system->defconfig target))
                 (invoke "make" "mrproper"
                         ,@(if (version>=? (package-version linux-headers) "5.3")
                               '("headers")
                               '("headers_check")))))))))
      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(package-native-inputs linux-headers)))))

  (define xmig
    (cross-mig target #:xgcc xgcc #:xbinutils xbinutils))

  (define xgnumach-headers
    (cross-gnumach-headers target #:xgcc xgcc #:xbinutils xbinutils))

  (define xhurd-headers
    (package
      (inherit hurd-headers)
      (name (string-append (package-name hurd-headers)
                           "-cross-" target))

      (arguments
       (substitute-keyword-arguments (package-arguments hurd-headers)
         ((#:configure-flags flags)
          `(cons* ,(string-append "--build=" (%current-system))
                  ,(string-append "--host=" target)
                  ,flags))))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(alist-delete "mig" (package-native-inputs hurd-headers))))))

  (define xglibc/hurd-headers
    (package
      (inherit glibc/hurd-headers)
      (name (string-append (package-name glibc/hurd-headers)
                           "-cross-" target))

      (arguments
       (substitute-keyword-arguments
           `(#:modules ((guix build gnu-build-system)
                        (guix build utils)
                        (srfi srfi-26))
             ,@(package-arguments glibc/hurd-headers))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'set-cross-headers-path
               (lambda* (#:key inputs #:allow-other-keys)
                 (let* ((mach (assoc-ref inputs "gnumach-headers"))
                        (hurd (assoc-ref inputs "hurd-headers"))
                        (cpath (string-append mach "/include:"
                                              hurd "/include")))
                   (for-each (cut setenv <> cpath)
                             ',%gcc-cross-include-paths)
                   #t)))))
         ((#:configure-flags flags)
          `(cons* ,(string-append "--build=" (%current-system))
                  ,(string-append "--host=" target)
                  ,flags))))

      (propagated-inputs `(("gnumach-headers" ,xgnumach-headers)
                           ("hurd-headers" ,xhurd-headers)))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(alist-delete "mig"(package-native-inputs glibc/hurd-headers))))))

  (define xhurd-minimal
    (package
      (inherit hurd-minimal)
      (name (string-append (package-name hurd-minimal)
                           "-cross-" target))
      (arguments
       (substitute-keyword-arguments
           `(#:modules ((guix build gnu-build-system)
                        (guix build utils)
                        (srfi srfi-26))
             ,@(package-arguments hurd-minimal))
         ((#:configure-flags flags)
          `(cons* ,(string-append "--build=" (%current-system))
                  ,(string-append "--host=" target)
                  ,flags))
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'delete-shared-target
                ;; Cannot create shared libraries due to missing crt1.o
                (lambda _
                  (substitute* "Makeconf"
                    (("(targets := \\$\\(libname\\)\\.a) \\$\\(libname\\)\\.so" all static)
                     static)
                    (("\\$\\(DESTDIR\\)\\$\\(libdir\\)/\\$\\(libname\\)\\.so\\.\\$\\(hurd-version\\)")
                     "")
                    (("^libs: .*\\.so\\..*" all)
                     (string-append "# " all)))))
             (add-before 'configure 'set-cross-headers-path
               (lambda* (#:key inputs #:allow-other-keys)
                 (let* ((glibc-headers (assoc-ref inputs "cross-glibc-hurd-headers"))
                        (mach-headers (assoc-ref inputs "cross-gnumach-headers"))
                        (cpath (string-append glibc-headers "/include"
                                              ":" mach-headers "/include")))
                   (for-each (cut setenv <> cpath)
                             '#$%gcc-cross-include-paths)
                   #t)))))))

      (inputs `(("cross-glibc-hurd-headers" ,xglibc/hurd-headers)
                ("cross-gnumach-headers" ,xgnumach-headers)))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(alist-delete "mig"
                                       (package-native-inputs hurd-minimal))))))

  (define xhurd-core-headers
    (package
      (inherit hurd-core-headers)
      (name (string-append (package-name hurd-core-headers)
                           "-cross-" target))

      (inputs `(("gnumach-headers" ,xgnumach-headers)
                ("hurd-headers" ,xhurd-headers)
                ("hurd-minimal" ,xhurd-minimal)))))

  (if (target-hurd? target)
      xhurd-core-headers
      xlinux-headers))

(define* (cross-libc . args)
  (if (or (= (length args) 1) (contains-keyword? args))
      (apply cross-libc* args)
      (apply cross-libc/deprecated args)))

(define* (cross-libc/deprecated target
                                #:optional
                                (libc (libc-for-target target))
                                (xgcc (cross-gcc target))
                                (xbinutils (cross-binutils target))
                                (xheaders (cross-kernel-headers target)))
  (warning (G_ "'cross-libc' must be used with keyword arguments~%"))
  (cross-libc* target
               #:libc libc
               #:xgcc xgcc
               #:xbinutils xbinutils
               #:xheaders xheaders))

(define* (cross-libc* target
                      #:key
                      (libc (libc-for-target target))
                      (xgcc (cross-gcc target))
                      (xbinutils (cross-binutils target))
                      (xheaders (cross-kernel-headers target))
                      (with-winpthreads? #t))
  "Return LIBC cross-built for TARGET, a GNU triplet. Use XGCC and XBINUTILS
and the cross tool chain.  If TARGET doesn't have a standard C library #f is
returned."
  (match target
   ((? target-mingw?)
    (let ((machine (substring target 0 (string-index target #\-))))
      (make-mingw-w64 machine
                      #:xgcc xgcc
                      #:xbinutils xbinutils
                      #:with-winpthreads? #t)))
   ((or (? target-linux?) (? target-hurd?))
    (package
      (inherit libc)
      (name (string-append "glibc-cross-" target))
      (arguments
       (substitute-keyword-arguments
         `(;; Disable stripping (see above.)
           #:strip-binaries? #f

           ;; This package is used as a target input, but it should not have
           ;; the usual cross-compilation inputs since that would include
           ;; itself.
           #:implicit-cross-inputs? #f

           ;; We need SRFI 26.
           #:modules ((guix build gnu-build-system)
                      (guix build utils)
                      (srfi srfi-26))

               ,@(package-arguments libc))
           ((#:configure-flags flags)
            `(cons ,(string-append "--host=" target)
                   ,(if (target-hurd? target)
                        `(append (list "--disable-werror")
                                 ,flags)
                        flags)))
           ((#:phases phases)
            `(modify-phases ,phases
               (add-before 'configure 'set-cross-kernel-headers-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((kernel (assoc-ref inputs "kernel-headers"))
                          (cpath (string-append kernel "/include")))
                     (for-each (cut setenv <> cpath)
                               ',%gcc-cross-include-paths)
                     (setenv "CROSS_LIBRARY_PATH"
                             (string-append kernel "/lib")) ; for Hurd's libihash
                     #t)))
               (add-before 'configure 'add-cross-binutils-to-PATH
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   ;; Add BINUTILS/TARGET/bin to $PATH so that 'gcc
                   ;; -print-prog-name=objdump' returns the correct name.  See
                   ;; <https://inbox.sourceware.org/libc-alpha/d72f5f6f-cc3a-bd89-0800-ffb068928e0f@linaro.org/t/>.
                   (define cross-objdump
                     (search-input-file
                      (or native-inputs inputs)
                      (string-append ,target "/bin/objdump")))

                   (define cross-binutils
                     (dirname cross-objdump))

                   (format #t "adding '~a' to the front of 'PATH'~%"
                           cross-binutils)
                   (setenv "PATH" (string-append cross-binutils ":"
                                                 (getenv "PATH")))))

               ;; This phase would require running 'localedef' built for
               ;; TARGET, which is impossible by definition.
               (delete 'install-utf8-c-locale)

               ,@(if (target-hurd? target)
                     `((add-after 'install 'augment-libc.so
                         (lambda* (#:key outputs #:allow-other-keys)
                           (let ((out (assoc-ref outputs "out")))
                             (substitute* (string-append out "/lib/libc.so")
                               (("/[^ ]+/lib/libc.so.0.3")
                                (string-append out "/lib/libc.so.0.3"
                                               " libmachuser.so libhurduser.so"))))))
                       (add-after 'install 'create-machine-symlink
                         (lambda* (#:key outputs #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (cpu ,(match target
                                          ((? target-x86-32?)
                                           "i386")
                                          ((? target-x86-64?)
                                           "x86_64")))
                                  (machine (string-append
                                            out "/include/mach/machine")))
                             (unless (file-exists? machine)
                               (symlink cpu machine))))))
                     '())))))

      ;; Shadow the native "kernel-headers" because glibc's recipe expects the
      ;; "kernel-headers" input to point to the right thing.
      (propagated-inputs `(("kernel-headers" ,xheaders)))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(if (target-hurd? target)
                             `(("cross-mig"
                                ,(cross-mig target
                                            #:xgcc xgcc
                                            #:xbinutils xbinutils)))
                             '())
                       ,@(package-inputs libc) ;FIXME: static-bash
                       ,@(package-native-inputs libc)))))
   ((? target-avr?)
    (make-avr-libc #:xbinutils xbinutils
                   #:xgcc xgcc))
   (else #f)))

(define* (cross-gcc-toolchain/implementation target
                                             #:key
                                             (base-gcc %xgcc)
                                             (xbinutils (cross-binutils target))
                                             (with-winpthreads? #t)
                                             (libc (cross-libc
                                                     target
                                                     #:xgcc (cross-gcc target #:xgcc base-gcc)
                                                     #:xbinutils xbinutils
                                                     #:with-winpthreads? with-winpthreads?))
                                             (xgcc (cross-gcc target
                                                              #:xgcc base-gcc
                                                              #:libc libc
                                                              #:xbinutils xbinutils)))
  "Returns PACKAGE that contains a cross-compilation tool chain for TARGET
with XBINUTILS, XGCC and LIBC (if exists for TARGET)."
  (package
    ;; Using PACKAGE-NAME of XGCC is avoided here as there are platforms that
    ;; still need a toolchain but don't have a libc (e.g. or1k-elf).
    (name (string-append "gcc-cross-" target "-toolchain"))
    (version (package-version xgcc))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build union))
           #:builder
           #~(begin
               (use-modules (ice-9 match)
                            (guix build union))

               (match %build-inputs
                 (((names . directory) ...)
                  (union-build #$output directory))))))
    (inputs `(,xbinutils ,xgcc ,@(if libc (list libc) '())))
    (native-search-paths (package-search-paths xgcc))
    (home-page (package-home-page xgcc))
    (synopsis
     (format #f "Complete GCC tool chain for C/C++ development (~a)" target))
    (description "This package provides a complete GCC cross toolchain for
C/C++ development to be installed in user profiles.  This includes GCC, as
well as libc (headers and binariesl), and Binutils.  GCC is the GNU Compiler
Collection.")
    (license (delete-duplicates `(,(package-license xgcc)
                                  ,(package-license xbinutils)
                                  ,@(if libc
                                        (list (package-license libc))
                                        '()))))))

(define cross-gcc-toolchain
  (memoize cross-gcc-toolchain/implementation))


;;; Concrete cross tool chains are instantiated like this:
;;
;; (define-public xgcc-armhf
;;   (let ((triplet "arm-linux-gnueabihf"))
;;     (cross-gcc triplet
;;                #:xbinutils (cross-binutils triplet)
;;                #:libc (cross-libc triplet))))
;;
;;; We don't do that here because we'd be referring to bindings from (gnu
;;; packages gcc) from the top level, which doesn't play well with circular
;;; dependencies among modules.
