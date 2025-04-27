;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015-2018, 2020-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Carlos Sánchez de La Lama <csanchezdll@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2020 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2024, 2025 Zheng Junjie <z572@z572.online>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Nguyễn Gia Phong <mcsinyx@disroot.org>
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

(define-module (gnu packages gcc)
  #:use-module ((guix licenses)
                #:select (gpl3+ gpl2+ lgpl2.1+ lgpl2.0+ fdl1.3+))
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages perl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

(define %gcc-infrastructure
  ;; Base URL for GCC's infrastructure.
  "mirror://gcc/infrastructure/")

(define (gcc-configure-flags-for-triplet target)
  "Return a list of additional GCC `configure' flags for TARGET, a GNU triplet.

The purpose of this procedure is to translate extended GNU triplets---e.g.,
where the OS part is overloaded to denote a specific ABI---into GCC
`configure' options.  We take extended GNU triplets that glibc recognizes."
  (cond ((string-match "^mips64el.*gnuabin?64$" target)
         ;; Triplets recognized by glibc as denoting the N64 ABI; see
         ;; ports/sysdeps/mips/preconfigure.
         '("--with-abi=64"))

        ((string-match "^arm.*-gnueabihf$" target)
         '("--with-arch=armv7-a"
           "--with-float=hard"
           "--with-mode=thumb"
           "--with-fpu=neon"))

        ((string-match "x86_64-linux-gnux32" target)
         '("--with-abi=mx32"))

        ((and (string-suffix? "-gnu" target)
              (not (string-contains target "-linux")))
         ;; Cross-compilation of libcilkrts in GCC 5.5.0 to GNU/Hurd fails
         ;; with:
         ;;   libcilkrts/runtime/os-unix.c:388:2: error: #error "Unknown architecture"
         ;; Cilk has been removed from GCC 8 anyway.
         '("--disable-libcilkrts"))

        ;; glibc needs the 128-bit long double type on these architectures.
        ((or (string-prefix? "powerpc64le-" target)
             (string-prefix? "powerpc-" target))
         '("--with-long-double-128"))

        ;; GCC 11.3.0's <libgcov.h> includes <sys/mman.h>, which MinGW lacks:
        ;; <https://bugs.gentoo.org/show_bug.cgi?id=843989>.
        ((target-mingw? target)
         '("--disable-gcov"))

        (else
         ;; TODO: Add `arm.*-gnueabi', etc.
         '())))

(define-public gcc-4.7
  (let* ((stripped? #t)      ;whether to strip the compiler, not the libraries
         (maybe-target-tools
          (lambda ()
            ;; Return the `_FOR_TARGET' variables that are needed when
            ;; cross-compiling GCC.
            (let ((target (%current-target-system)))
              (if target
                  (map (lambda (var tool)
                         (string-append (string-append var "_FOR_TARGET")
                                        "=" target "-" tool))
                       '("CC"  "CXX" "LD" "AR" "NM" "OBJDUMP" "RANLIB" "STRIP")
                       '("gcc" "g++" "ld" "ar" "nm" "objdump" "ranlib" "strip"))
                  '()))))
         (libdir
          (let ((base '(or (assoc-ref outputs "lib")
                           (assoc-ref outputs "out"))))
            (lambda ()
              ;; Return the directory that contains lib/libgcc_s.so et al.
              (if (%current-target-system)
                  `(string-append ,base "/" ,(%current-target-system))
                  base))))
         (configure-flags
          (lambda ()
            ;; This is terrible.  Since we have two levels of quasiquotation,
            ;; we have to do this convoluted thing just so we can insert the
            ;; contents of (maybe-target-tools).
            (list 'quasiquote
                  (append
                   '("--enable-plugin"
                     "--enable-languages=c,c++,objc,obj-c++"
                     "--disable-multilib"
                     "--with-system-zlib"

                     ;; No pre-compiled libstdc++ headers, to save space.
                     "--disable-libstdcxx-pch"

                     "--with-local-prefix=/no-gcc-local-prefix"

                     ;; With a separate "lib" output, the build system
                     ;; incorrectly guesses GPLUSPLUS_INCLUDE_DIR, so force
                     ;; it.  (Don't use a versioned sub-directory, that's
                     ;; unnecessary.)
                     ,(string-append "--with-gxx-include-dir="
                                     (assoc-ref %outputs "out")
                                     "/include/c++")

                     ,(let ((libc (assoc-ref %build-inputs "libc")))
                        (if libc
                            (string-append "--with-native-system-header-dir=" libc
                                           "/include")
                            "--without-headers")))

                   ;; Pass the right options for the target triplet.
                   (let ((triplet
                          (or (%current-target-system)
                              (nix-system->gnu-triplet (%current-system)))))
                     (gcc-configure-flags-for-triplet triplet))

                   (maybe-target-tools))))))
    (hidden-package
     (package
       (name "gcc")
       (version "4.7.4")
       (source (origin
                 (method url-fetch)
                 (uri (string-append "mirror://gnu/gcc/gcc-"
                                     version "/gcc-" version ".tar.bz2"))
                 (sha256
                  (base32
                   "10k2k71kxgay283ylbbhhs51cl55zn2q38vj5pk4k950qdnirrlj"))
                 (patches (search-patches "gcc-4-compile-with-gcc-5.patch"
                                          "gcc-fix-texi2pod.patch"))))
       (build-system gnu-build-system)

       ;; Separate out the run-time support libraries because all the
       ;; dynamic-linked objects depend on it.
       (outputs '("out"                    ;commands, etc. (60+ MiB)
                  "lib"                    ;libgcc_s, libgomp, etc. (15+ MiB)
                  "debug"))                ;debug symbols of run-time libraries

       (inputs (list gmp mpfr mpc elfutils zlib))

       ;; GCC < 5 is one of the few packages that doesn't ship .info files.
       ;; Newer texinfos fail to build the manual, so we use an older one.
       (native-inputs (list perl ;for manpages
                            texinfo-5))

       (arguments
        `(#:out-of-source? #t
          #:configure-flags ,(let ((flags (configure-flags))
                                   (version (package-version this-package)))
                               ;; GCC 11.3.0 defaults to C++17 which is partly
                               ;; incompatible with some earlier versions.
                               ;; Force an earlier C++ standard while building.
                               (cond
                                 ((version-prefix? "4.8" version)
                                  `(cons "CXX=g++ -std=c++03" ,flags))
                                 ((or (version-prefix? "4.9" version)
                                      (version-prefix? "5" version))
                                  `(cons "CXX=g++ -std=c++11" ,flags))
                                 (else flags)))

          #:make-flags
          ;; None of the flags below are needed when doing a Canadian cross.
          ;; TODO: Simplify this.
          ,(if (%current-target-system)
               (if stripped?
                   ''("CFLAGS=-g0 -O2")
                   ''())
               `(let* ((libc        (assoc-ref %build-inputs "libc"))
                       (libc-native (or (assoc-ref %build-inputs "libc-native")
                                        libc)))
                  `(,@(if libc
                          (list (string-append "LDFLAGS_FOR_TARGET="
                                               "-B" libc "/lib "
                                               "-Wl,-dynamic-linker "
                                               "-Wl," libc
                                               ,(glibc-dynamic-linker)))
                          '())

                    ;; Native programs like 'genhooks' also need that right.
                    ,(string-append "LDFLAGS="
                                    "-Wl,-rpath=" libc-native "/lib "
                                    "-Wl,-dynamic-linker "
                                    "-Wl," libc-native ,(glibc-dynamic-linker))
                    ,(string-append "BOOT_CFLAGS=-O2 "
                                    ,(if stripped? "-g0" "-g")))))

          #:tests? #f

          #:phases
          (modify-phases %standard-phases
            (add-before 'configure 'pre-configure
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((libdir ,(libdir))
                      (libc   (assoc-ref inputs "libc")))
                  (when libc
                    ;; The following is not performed for `--without-headers'
                    ;; cross-compiler builds.

                    ;; Join multi-line definitions of GLIBC_DYNAMIC_LINKER* into a
                    ;; single line, to allow the next step to work properly.
                    (for-each
                     (lambda (x)
                       (substitute* (find-files "gcc/config"
                                                "^(linux|gnu|sysv4)(64|-elf|-eabi)?\\.h$")
                         (("(#define (GLIBC|GNU_USER)_DYNAMIC_LINKER.*)\\\\\n$" _ line)
                          line)))
                     '(1 2 3))

                    ;; Fix the dynamic linker's file name.
                    (substitute* (find-files "gcc/config"
                                             "^(linux|gnu|sysv4)(64|-elf|-eabi)?\\.h$")
                      (("#define (GLIBC|GNU_USER)_DYNAMIC_LINKER([^ \t]*).*$"
                        _ gnu-user suffix)
                       (format #f "#define ~a_DYNAMIC_LINKER~a \"~a\"~%"
                               gnu-user suffix
                               (string-append libc ,(glibc-dynamic-linker)))))

                    ;; Tell where to find libstdc++, libc, and `?crt*.o', except
                    ;; `crt{begin,end}.o', which come with GCC.
                    (substitute* (find-files "gcc/config"
                                             "^gnu-user.*\\.h$")
                      (("#define GNU_USER_TARGET_LIB_SPEC (.*)$" _ suffix)
                       ;; Help libgcc_s.so be found (see also below.)  Always use
                       ;; '-lgcc_s' so that libgcc_s.so is always found by those
                       ;; programs that use 'pthread_cancel' (glibc dlopens
                       ;; libgcc_s.so when pthread_cancel support is needed, but
                       ;; having it in the application's RUNPATH isn't enough; see
                       ;; <http://sourceware.org/ml/libc-help/2013-11/msg00023.html>.)
                       ;;
                       ;; NOTE: The '-lgcc_s' added below needs to be removed in a
                       ;; later phase of %gcc-static.  If you change the string
                       ;; below, make sure to update the relevant code in
                       ;; %gcc-static package as needed.
                       (format #f "#define GNU_USER_TARGET_LIB_SPEC \
\"-L~a/lib %{!static:-rpath=~a/lib %{!static-libgcc:-rpath=~a/lib -lgcc_s}} \" ~a"
                               libc libc libdir suffix))
                      (("#define GNU_USER_TARGET_STARTFILE_SPEC.*$" line)
                       (format #f "#define STANDARD_STARTFILE_PREFIX_1 \"~a/lib\"
#define STANDARD_STARTFILE_PREFIX_2 \"\"
~a"
                               libc line)))

                    ;; The rs6000 (a.k.a. powerpc) config in GCC does not use
                    ;; GNU_USER_* defines.  Do the above for this case.
                    (substitute*
                        "gcc/config/rs6000/sysv4.h"
                      (("#define LIB_LINUX_SPEC (.*)$" _ suffix)
                       (format #f "#define LIB_LINUX_SPEC \
\"-L~a/lib %{!static:-rpath=~a/lib %{!static-libgcc:-rpath=~a/lib -lgcc_s}} \" ~a"
                               libc libc libdir suffix))
                      (("#define	STARTFILE_LINUX_SPEC.*$" line)
                       (format #f "#define STANDARD_STARTFILE_PREFIX_1 \"~a/lib\"
#define STANDARD_STARTFILE_PREFIX_2 \"\"
~a"
                               libc line))))

                  (when (file-exists? "gcc/config/rs6000")
                    ;; Force powerpc libdir to be /lib and not /lib64
                    (substitute* (find-files "gcc/config/rs6000")
                      (("/lib64") "/lib")))

                  ;; Don't retain a dependency on the build-time sed.
                  (substitute* "fixincludes/fixincl.x"
                    (("static char const sed_cmd_z\\[\\] =.*;")
                     "static char const sed_cmd_z[] = \"sed\";"))

                  ;; Aarch64 support didn't land in GCC until the 4.8 series.
                  (when (file-exists? "gcc/config/aarch64")
                    ;; Force Aarch64 libdir to be /lib and not /lib64
                    (substitute* "gcc/config/aarch64/t-aarch64-linux"
                      (("lib64") "lib")))

                  ;; The STARTFILE_PREFIX_SPEC prevents gcc from finding the
                  ;; gcc:lib output, which causes ld to not find -lgcc_s.
                  (when (file-exists? "gcc/config/riscv")
                    (substitute* '("gcc/config/riscv/linux.h"
                                   "gcc/config/riscv/riscv.h")  ; GCC < 10
                      (("define STARTFILE_PREFIX_SPEC")
                      "define __STARTFILE_PREFIX_SPEC")))

                  (when (file-exists? "libbacktrace")
                    ;; GCC 4.8+ comes with libbacktrace.  By default it builds
                    ;; with -Werror, which fails with a -Wcast-qual error in glibc
                    ;; 2.21's stdlib-bsearch.h.  Remove -Werror.
                    (substitute* "libbacktrace/configure"
                      (("WARN_FLAGS=(.*)-Werror" _ flags)
                       (string-append "WARN_FLAGS=" flags)))

                    (when (file-exists? "libsanitizer/libbacktrace")
                      ;; Same in libsanitizer's bundled copy (!) found in 4.9+.
                      (substitute* "libsanitizer/libbacktrace/Makefile.in"
                        (("-Werror")
                         ""))))

                  ;; Add a RUNPATH to libstdc++.so so that it finds libgcc_s.
                  ;; See <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=32354>
                  ;; and <http://bugs.gnu.org/20358>.
                  (substitute* "libstdc++-v3/src/Makefile.in"
                    (("^OPT_LDFLAGS = ")
                     "OPT_LDFLAGS = -Wl,-rpath=$(libdir) "))

                  ;; Move libstdc++*-gdb.py to the "lib" output to avoid a
                  ;; circularity between "out" and "lib".  (Note:
                  ;; --with-python-dir is useless because it imposes $(prefix) as
                  ;; the parent directory.)
                  (substitute* "libstdc++-v3/python/Makefile.in"
                    (("pythondir = .*$")
                     (string-append "pythondir = " libdir "/share"
                                    "/gcc-$(gcc_version)/python\n")))

                  ;; Avoid another circularity between the outputs: this #define
                  ;; ends up in auto-host.h in the "lib" output, referring to
                  ;; "out".  (This variable is used to augment cpp's search path,
                  ;; but there's nothing useful to look for here.)
                  (substitute* "gcc/config.in"
                    (("PREFIX_INCLUDE_DIR")
                     "PREFIX_INCLUDE_DIR_isnt_necessary_here")))))

            (add-after 'configure 'post-configure
              (lambda _
                ;; Don't store configure flags, to avoid retaining references to
                ;; build-time dependencies---e.g., `--with-ppl=/gnu/store/xxx'.
                (substitute* "Makefile"
                  (("^TOPLEVEL_CONFIGURE_ARGUMENTS=(.*)$" _ rest)
                   "TOPLEVEL_CONFIGURE_ARGUMENTS=\n")))))))
       (native-search-paths %gcc-search-paths)
       (properties `((gcc-libc . ,(assoc-ref inputs "libc"))))
       (synopsis "GNU Compiler Collection")
       (description
        "GCC is the GNU Compiler Collection.  It provides compiler front-ends
for several languages, including C, C++, Objective-C, Fortran, Java, Ada, and
Go.  It also includes runtime support libraries for these languages.")
       (license gpl3+)
       (supported-systems (delete "aarch64-linux" %supported-systems))
       (home-page "https://gcc.gnu.org/")))))

(define-public gcc-4.8
  (package (inherit gcc-4.7)
    (version "4.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.bz2"))
              (sha256
               (base32
                "08yggr18v373a1ihj0rg2vd6psnic42b518xcgp3r9k81xz1xyr2"))
              (patches (search-patches "gcc-arm-link-spec-fix.patch"
                                       "gcc-4.8-libsanitizer-fix.patch"
                                       "gcc-asan-missing-include.patch"
                                       "gcc-fix-texi2pod.patch"))
              (modules '((guix build utils)))
              ;; This is required for building with glibc-2.26.
              ;; https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81712
              (snippet
               '(for-each
                 (lambda (dir)
                   (substitute* (string-append "libgcc/config/"
                                               dir "/linux-unwind.h")
                     (("struct ucontext") "ucontext_t")))
                 '("aarch64" "alpha" "bfin" "i386" "m68k"
                   "pa" "sh" "tilepro" "xtensa")))))
    (arguments
     ;; Since 'arguments' is a function of the package's version, define
     ;; 'parent' such that the 'arguments' thunk gets to see the right
     ;; version.
     (let ((parent (package
                     (inherit gcc-4.7)
                     (version (package-version this-package)))))
       (if (%current-target-system)
           (package-arguments parent)
           ;; For native builds of some GCC versions the C++ include path needs to
           ;; be adjusted so it does not interfere with GCC's own build processes.
           (substitute-keyword-arguments (package-arguments parent)
             ((#:modules modules %default-gnu-modules)
              `((srfi srfi-1)
                ,@modules))
             ((#:phases phases)
              `(modify-phases ,phases
                 (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((libc (assoc-ref inputs "libc"))
                           (gcc (assoc-ref inputs  "gcc")))
                       (setenv "CPLUS_INCLUDE_PATH"
                               (string-join (fold delete
                                                  (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                                #\:)
                                                  (list (string-append libc "/include")
                                                        (string-append gcc "/include/c++")))
                                            ":"))
                       (format #t
                               "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                               (getenv "CPLUS_INCLUDE_PATH")))))))))))
    (supported-systems %supported-systems)
    (inputs
     (modify-inputs (package-inputs gcc-4.7)
       (prepend isl-0.11 cloog)))))

(define-public gcc-4.9
  (package (inherit gcc-4.8)
    (version "4.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.bz2"))
              (sha256
               (base32
                "14l06m7nvcvb0igkbip58x59w3nq6315k6jcz3wr9ch1rn9d44bc"))
              (patches (search-patches "gcc-4.9-libsanitizer-fix.patch"
                                       "gcc-4.9-libsanitizer-ustat.patch"
                                       "gcc-4.9-libsanitizer-mode-size.patch"
                                       "gcc-arm-bug-71399.patch"
                                       "gcc-asan-missing-include.patch"
                                       "gcc-libvtv-runpath.patch"
                                       "gcc-fix-texi2pod.patch"))
              (modules '((guix build utils)))
              ;; This is required for building with glibc-2.26.
              ;; https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81712
              (snippet
               '(for-each
                 (lambda (dir)
                   (substitute* (string-append "libgcc/config/"
                                               dir "/linux-unwind.h")
                     (("struct ucontext") "ucontext_t")))
                 '("aarch64" "alpha" "bfin" "i386" "m68k" "nios2"
                   "pa" "sh" "tilepro" "xtensa")))))
    ;; Override inherited texinfo-5 with latest version.
    (native-inputs (list perl ;for manpages
                         texinfo))))

(define gcc-canadian-cross-objdump-snippet
  ;; Fix 'libcc1/configure' error when cross-compiling GCC.  Without that,
  ;; 'libcc1/configure' wrongfully determines that '-rdynamic' support is
  ;; missing because $gcc_cv_objdump is empty:
  ;;
  ;;   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=67590
  ;;   http://cgit.openembedded.org/openembedded-core/commit/?id=f6e47aa9b12f9ab61530c40e0343f451699d9077
  #~(substitute* "libcc1/configure"
      (("\\$gcc_cv_objdump -T")
       "$OBJDUMP_FOR_TARGET -T")))

(define-public gcc-5
  ;; Note: GCC >= 5 ships with .info files but 'make install' fails to install
  ;; them in a VPATH build.
  (package (inherit gcc-4.9)
    (version "5.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.xz"))
              (sha256
               (base32
                "11zd1hgzkli3b2v70qsm2hyqppngd4616qc96lmm9zl2kl9yl32k"))
              (patches (search-patches "gcc-arm-bug-71399.patch"
                                       "gcc-libsanitizer-ustat.patch"
                                       "gcc-strmov-store-file-names.patch"
                                       "gcc-5.0-libvtv-runpath.patch"
                                       "gcc-5-source-date-epoch-1.patch"
                                       "gcc-5-source-date-epoch-2.patch"
                                       "gcc-5.5.0-libstdc++-xmlcatalog.patch"
                                       "gcc-13.2.0-libstdc++-docbook-xsl-uri.patch"
                                       "gcc-13.2.0-libstdc++-info-install-fix.patch"
                                       "gcc-7-libsanitizer-fsconfig-command.patch"
                                       "gcc-6-libsanitizer-mode-size.patch"
                                       "gcc-fix-texi2pod.patch"
                                       "gcc-5-hurd.patch"
                                       ;; See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=86162
                                       "gcc-5-fix-powerpc64le-build.patch"))
              (modules '((guix build utils)))
              (snippet gcc-canadian-cross-objdump-snippet)))
    (inputs
     (modify-inputs (package-inputs gcc-4.7)
       (prepend ;; GCC5 needs <isl/band.h> which is removed in later versions.
                isl-0.18)))))

(define-public gcc-6
  (package
    (inherit gcc-5)
    (version "6.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.xz"))
              (sha256
               (base32
                "0i89fksfp6wr1xg9l8296aslcymv2idn60ip31wr9s4pwin7kwby"))
              (patches (search-patches "gcc-strmov-store-file-names.patch"
                                       "gcc-7-libsanitizer-fsconfig-command.patch"
                                       "gcc-6-libsanitizer-mode-size.patch"
                                       "gcc-6-source-date-epoch-1.patch"
                                       "gcc-6-source-date-epoch-2.patch"
                                       "gcc-5.0-libvtv-runpath.patch"))))

    ;; GCC 4.9 and 5 has a workaround that is not needed for GCC 6 and later.
    (arguments (package-arguments gcc-4.7))

    (inputs
     `(("isl" ,isl)

       ;; XXX: This gross hack allows us to have libstdc++'s <bits/c++config.h>
       ;; in the search path, thereby avoiding misconfiguration of libstdc++:
       ;; <https://bugs.gnu.org/42392>.
       ("libstdc++" ,libstdc++-headers)

       ,@(package-inputs gcc-4.7)))))

(define %gcc-7.5-aarch64-micro-architectures
  ;; Suitable '-march' values for GCC 7.5 (info "(gcc) AArch64 Options").
  ;; TODO: Allow dynamically adding feature flags.
  '("armv8-a" "armv8.1-a" "armv8.2-a" "armv8.3-a"))

(define %gcc-7.5-armhf-micro-architectures
  ;; Suitable '-march' values for GCC 7.5 (info "(gcc) ARM Options").
  ;; TODO: Allow dynamically adding feature flags.
  '("armv7" "armv7-a" "armv7-m" "armv7-r" "armv7e-m" "armv7ve"
    "armv8-a" "armv8-a+crc" "armv8.1-a" "armv8.1-a+crc"
    "armv8-m.base" "armv8-m.main" "armv8-m.main+dsp"
    "iwmmxt" "iwmmxt2" "armv8.2-a"))

(define %gcc-7.5-x86_64-micro-architectures
  ;; Suitable '-march' values for GCC 7.5 (info "(gcc) x86 Options").
  '("core2" "nehalem" "westmere" "sandybridge" "ivybridge"
    "haswell" "broadwell" "skylake" "bonnell" "silvermont"
    "knl" "skylake-avx512"

    "k8" "k8-sse3" "barcelona"
    "bdver1" "bdver2" "bdver3" "bdver4"
    "znver1"
    "btver1" "btver2" "geode"))

(define %gcc-10-aarch64-micro-architectures
  ;; Suitable '-march' values for GCC 10.
  ;; TODO: Allow dynamically adding feature flags.
  (append %gcc-7.5-aarch64-micro-architectures
          '("armv8.4-a" "armv8.5-a" "armv8.6-a")))

(define %gcc-10-armhf-micro-architectures
  ;; Suitable '-march' values for GCC 10.
  ;; TODO: Allow dynamically adding feature flags.
  (append %gcc-7.5-armhf-micro-architectures
          '("armv8.3-a" "armv8.4-a" "armv8.5-a" "armv8.6-a"
            "armv8-r" "armv8.1-m.main")))

(define %gcc-10-ppc64le-micro-architectures
  '("power8" "power9" "power10" "powerpc64le"))

(define %gcc-10-x86_64-micro-architectures
  ;; Suitable '-march' values for GCC 10.
  (append %gcc-7.5-x86_64-micro-architectures
      '("goldmont" "goldmont-plus" "tremont"
        "knm" "cannonlake" "icelake-client" "icelake-server"
        "cascadelake" "cooperlake" "tigerlake"

        "znver2" "znver3")))

(define %gcc-11-aarch64-micro-architectures
  ;; Suitable '-march' values for GCC 11.
  (append %gcc-10-aarch64-micro-architectures
          '("armv8-r")))

(define %gcc-11-armhf-micro-architectures
  %gcc-10-armhf-micro-architectures)

(define %gcc-11-x86_64-micro-architectures
  ;; Suitable '-march' values for GCC 11.
  (append %gcc-10-x86_64-micro-architectures
          '("sapphirerapids" "alderlake" "rocketlake" ;Intel

            "btver1" "btver2"                     ;AMD

            ;; psABI micro-architecture levels
            "x86-64" "x86-64-v2" "x86-64-v3" "x86-64-v4")))

;; Suitable '-march' values for GCC 12.
(define %gcc-12-aarch64-micro-architectures
  (append %gcc-11-aarch64-micro-architectures
          '("armv8.7-a" "armv8.8-a" "armv9-a")))

(define %gcc-12-armhf-micro-architectures
  (append %gcc-11-armhf-micro-architectures
          '("armv9-a")))

(define %gcc-12-x86_64-micro-architectures
  (append %gcc-11-x86_64-micro-architectures
          '("znver4")))                           ;AMD

;; Suitable '-march' values for GCC 13.
(define %gcc-13-aarch64-micro-architectures
  (append %gcc-12-aarch64-micro-architectures
          '("armv9.1-a" "armv9.2-a" "armv9.3-a")))

(define %gcc-13-armhf-micro-architectures
  %gcc-12-armhf-micro-architectures)

(define %gcc-13-x86_64-micro-architectures
  (append %gcc-12-x86_64-micro-architectures
          '("graniterapids")))                    ;Intel

(define %gcc-14-x86_64-micro-architectures
  (append %gcc-13-x86_64-micro-architectures
          '("znver5")))                           ;AMD

(define-public gcc-7
  (package
    (inherit gcc-6)
    (version "7.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.xz"))
              (sha256
               (base32
                "0qg6kqc5l72hpnj4vr6l0p69qav0rh4anlkk3y55540zy3klc6dq"))
              (patches (search-patches "gcc-strmov-store-file-names.patch"
                                       "gcc-7-libsanitizer-mode-size.patch"
                                       "gcc-7-libsanitizer-fsconfig-command.patch"
                                       "gcc-5.0-libvtv-runpath.patch"))))
    (description
     "GCC is the GNU Compiler Collection.  It provides compiler front-ends
for several languages, including C, C++, Objective-C, Fortran, Ada, and Go.
It also includes runtime support libraries for these languages.")
    (properties
     `((compiler-cpu-architectures
        ("aarch64" ,@%gcc-7.5-aarch64-micro-architectures)
        ("armhf" ,@%gcc-7.5-armhf-micro-architectures)
        ("i686" ,@%gcc-7.5-x86_64-micro-architectures)
        ("x86_64" ,@%gcc-7.5-x86_64-micro-architectures))
       ,@(package-properties gcc-6)))))

(define-public gcc-8
  (package
    (inherit gcc-7)
    (version "8.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.xz"))
              (sha256
               (base32
                "0l7d4m9jx124xsk6xardchgy2k5j5l2b15q322k31f0va4d8826k"))
              (patches (search-patches "gcc-8-strmov-store-file-names.patch"
                                       "gcc-7-libsanitizer-fsconfig-command.patch"
                                       "gcc-5.0-libvtv-runpath.patch"
                                       "gcc-8-sort-libtool-find-output.patch"))
              (modules '((guix build utils)))
              (snippet gcc-canadian-cross-objdump-snippet)))))

(define-public gcc-9
  (package
   (inherit gcc-8)
   (version "9.5.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gcc/gcc-"
                                version "/gcc-" version ".tar.xz"))
            (sha256
             (base32
              "13ygjmd938m0wmy946pxdhz9i1wq7z4w10l6pvidak0xxxj9yxi7"))
            (patches (search-patches "gcc-7-libsanitizer-fsconfig-command.patch"
                                     "gcc-13.2.0-libstdc++-docbook-xsl-uri.patch"
                                     "gcc-13.2.0-libstdc++-info-install-fix.patch"
                                     "gcc-9-strmov-store-file-names.patch"
                                     "gcc-9-asan-fix-limits-include.patch"
                                     "gcc-5.0-libvtv-runpath.patch"))
            (modules '((guix build utils)))
            (snippet gcc-canadian-cross-objdump-snippet)))))

(define-public gcc-10
  (package
   (inherit gcc-8)
   (version "10.5.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gcc/gcc-"
                                version "/gcc-" version ".tar.xz"))
            (sha256
             (base32
              "1h87lcfaga0ydsf4pkhwlnjr8mky5ix8npbv6iy3jvzlzm1ra415"))
            (patches (search-patches "gcc-9-strmov-store-file-names.patch"
                                     "gcc-5.0-libvtv-runpath.patch"
                                     "gcc-10-libsanitizer-no-crypt.patch"))
            (modules '((guix build utils)))
            (snippet gcc-canadian-cross-objdump-snippet)))
   (properties
    `((compiler-cpu-architectures
       ("aarch64" ,@%gcc-10-aarch64-micro-architectures)
       ("armhf" ,@%gcc-10-armhf-micro-architectures)
       ("powerpc64le" ,@%gcc-10-ppc64le-micro-architectures)
       ("i686" ,@%gcc-10-x86_64-micro-architectures)
       ("x86_64" ,@%gcc-10-x86_64-micro-architectures))
      ,@(package-properties gcc-8)))))

(define-public gcc-11
  (package
   (inherit gcc-8)
   (version "11.4.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gcc/gcc-"
                                version "/gcc-" version ".tar.xz"))
            (sha256
             (base32
              "1ncd7akww0hl5kkmw1dj3qgqp3phdrr5dfnm7jia9s07n0ib4b9z"))
            (patches (search-patches "gcc-9-strmov-store-file-names.patch"
                                     "gcc-5.0-libvtv-runpath.patch"
                                     "gcc-10-libsanitizer-no-crypt.patch"
                                     "gcc-11-libstdc++-hurd-libpthread.patch"))
            (modules '((guix build utils)))
            (snippet gcc-canadian-cross-objdump-snippet)))
   (properties
    `((compiler-cpu-architectures
       ("aarch64" ,@%gcc-11-aarch64-micro-architectures)
       ("armhf" ,@%gcc-11-armhf-micro-architectures)
       ("i686" ,@%gcc-11-x86_64-micro-architectures)
       ("powerpc64le" ,@%gcc-10-ppc64le-micro-architectures)
       ("x86_64" ,@%gcc-11-x86_64-micro-architectures))
      ,@(package-properties gcc-8)))))

(define-public gcc-12
  (package
    (inherit gcc-11)
    (version "12.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.xz"))
              (sha256
               (base32
                "0xcida8l2wykvvzvpcrcn649gj0ijn64gwxbplacpg6c0hk6akvh"))
              (patches (search-patches "gcc-12-strmov-store-file-names.patch"
                                       "gcc-5.0-libvtv-runpath.patch"
                                       "gcc-12-libsanitizer-no-crypt.patch"
                                       "gcc-11-libstdc++-hurd-libpthread.patch"))
              (modules '((guix build utils)))
              (snippet gcc-canadian-cross-objdump-snippet)))
   (properties
    `((compiler-cpu-architectures
       ("aarch64" ,@%gcc-12-aarch64-micro-architectures)
       ("armhf" ,@%gcc-12-armhf-micro-architectures)
       ("i686" ,@%gcc-12-x86_64-micro-architectures)
       ("powerpc64le" ,@%gcc-10-ppc64le-micro-architectures)
       ("x86_64" ,@%gcc-12-x86_64-micro-architectures))
      ,@(package-properties gcc-11)))))

(define-public gcc-13
  (package
    (inherit gcc-11)
    (version "13.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.xz"))
              (sha256
               (base32
                "10y0l1hx1haz4cj4d4g9f2ci5h7z9555i52f90zs2hwm3iifji88"))
              (patches (search-patches "gcc-12-strmov-store-file-names.patch"
                                       "gcc-5.0-libvtv-runpath.patch"
                                       "gcc-13-libsanitizer-no-crypt.patch"))
              (modules '((guix build utils)))
              (snippet gcc-canadian-cross-objdump-snippet)))
    (arguments
     (substitute-keyword-arguments (package-arguments gcc-11)
       ((#:phases phases #~%standard-phases)
       (if (target-hurd?)
           #~(modify-phases #$phases
               (delete 'patch-hurd-libpthread))
           phases))))
    (properties
     `((compiler-cpu-architectures
        ("aarch64" ,@%gcc-13-aarch64-micro-architectures)
        ("armhf" ,@%gcc-13-armhf-micro-architectures)
        ("i686" ,@%gcc-13-x86_64-micro-architectures)
        ("powerpc64le" ,@%gcc-10-ppc64le-micro-architectures)
        ("x86_64" ,@%gcc-13-x86_64-micro-architectures))
       ,@(package-properties gcc-11)))))

(define-public gcc-14
  (package
    (inherit gcc-13)
    (version "14.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.xz"))
              (sha256
               (base32
                "1j9wdznsp772q15w1kl5ip0gf0bh8wkanq2sdj12b7mzkk39pcx7"))
              (patches (search-patches "gcc-12-strmov-store-file-names.patch"
                                       "gcc-5.0-libvtv-runpath.patch"))
              (modules '((guix build utils)))
              (snippet gcc-canadian-cross-objdump-snippet)))
    (arguments (substitute-keyword-arguments (package-arguments gcc-13)
                 ((#:phases phases #~%standard-phases)
                  #~(modify-phases #$phases
                      (add-before 'configure 'pre-x86-configure
                        (lambda _
                          (substitute* '("gcc/config/i386/t-linux64"
                                         "gcc/config/i386/t-gnu64")
                            (("\\.\\./lib64") "../lib"))))))))
    (properties
     `((compiler-cpu-architectures
        ("aarch64" ,@%gcc-13-aarch64-micro-architectures)
        ("armhf" ,@%gcc-13-armhf-micro-architectures)
        ("i686" ,@%gcc-13-x86_64-micro-architectures)
        ("powerpc64le" ,@%gcc-10-ppc64le-micro-architectures)
        ("x86_64" ,@%gcc-14-x86_64-micro-architectures))
       ,@(package-properties gcc-11)))))

(define-public gcc-15
  (package
    (inherit gcc-14)
    (version "15.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.xz"))
              (sha256
               (base32
                "1skcy1a3wwb8k25f9l1qy11nj8b5089f05dpzzn1zw302v19xc72"))
              (patches (search-patches "gcc-12-strmov-store-file-names.patch"
                                       "gcc-5.0-libvtv-runpath.patch"))
              (modules '((guix build utils)))
              (snippet gcc-canadian-cross-objdump-snippet)))
    (arguments
     (substitute-keyword-arguments (package-arguments gcc-14)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'adjust-modules-file
              ;; Avoid cycle dependencies
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((lib (assoc-ref outputs "lib"))
                      (out (assoc-ref outputs "out")))
                  (when lib
                    (let ((modfile (string-append
                                    lib "/lib/libstdc++.modules.json"))
                          (origin (string-append out "/include/c++/bits"))
                          (modpath (string-append lib "/include/c++/bits")))

                      (for-each (lambda (file) (install-file file modpath))
                                (find-files origin "\\.cc$"))
                      (substitute* modfile
                        ;; Relative path to out output
                        (("\\.\\./\\.\\./.*/include")
                         (string-append lib "/include"))))))))))))
    (properties
     `((compiler-cpu-architectures
        ("aarch64" ,@%gcc-13-aarch64-micro-architectures)
        ("armhf" ,@%gcc-13-armhf-micro-architectures)
        ("i686" ,@%gcc-13-x86_64-micro-architectures)
        ("powerpc64le" ,@%gcc-10-ppc64le-micro-architectures)
        ("x86_64" ,@%gcc-14-x86_64-micro-architectures))
       ,@(package-properties gcc-11)))))


;; Note: When changing the default gcc version, update
;;       the gcc-toolchain-* definitions.
(define-public gcc
  (if (host-hurd64?)
      gcc-14
      gcc-11))


;;;
;;; Historical version.
;;;

(define-public gcc-2.95
  ;; Note: 'gcc-core-mesboot0' in commencement.scm provides 2.95 as well, but
  ;; with additional tricks to support compilation with TinyCC and Mes-libc.
  (package
    (inherit gcc)
    (version "2.95.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-2.95.3/gcc-core-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xvfy4pqhrd5v2cv8lzf63iqg92k09g6z9n2ah6ndd4h17k1x0an"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Do not build the bundled Texinfo.
                  (delete-file-recursively "texinfo")
                  (substitute* "configure"
                    (("host_tools=(.*)texinfo" _ before)
                     (string-append "host_tools=" before)))
                  ;; Fix building on arm* with gcc-4+
                  (substitute* "gcc/config/arm/arm.c"
                    (("arm_prog_mode") "arm_prgmode"))))))
    (supported-systems (fold delete %supported-systems
                             '("powerpc64le-linux" "riscv64-linux")))
    (native-inputs (list texinfo dejagnu))
    (inputs '())
    (propagated-inputs '())
    (outputs '("out"))
    (arguments
     (let ((matching-system
             (match (%current-system)
               ;; This package predates our 64-bit architectures.
               ;; Force a 32-bit build targeting a similar architecture.
               ("aarch64-linux"
                "armhf-linux")
               ("x86_64-linux"
                "i686-linux")
               (_
                (%current-system)))))
       (list #:system matching-system
             #:configure-flags #~'("--disable-werror")

             #:phases
             #~(modify-phases %standard-phases
                 (add-before 'configure 'set-dynamic-linker-file-name
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Tell GCC what the real loader file name is.
                     (substitute* '("gcc/config/alpha/linux-elf.h"
                                    "gcc/config/m68k/linux.h"
                                    "gcc/config/mips/linux.h"
                                    "gcc/config/rs6000/linux.h")
                       (("/lib/ld\\.so\\.1")
                        (search-input-file
                          inputs #$(glibc-dynamic-linker matching-system))))
                     (substitute* "gcc/config/i386/gnu.h"
                       (("/lib/ld\\.so")
                        (search-input-file
                          inputs #$(glibc-dynamic-linker matching-system))))
                     (substitute* '("gcc/config/alpha/linux-elf.h"
                                    "gcc/config/arm/linux-elf.h"
                                    "gcc/config/i386/linux.h"
                                    "gcc/config/m68k/linux.h"
                                    "gcc/config/sparc/linux.h"
                                    "gcc/config/sparc/linux64.h")
                       (("/lib(64)?/ld-linux\\.so\\.[12]")
                        (search-input-file
                          inputs #$(glibc-dynamic-linker matching-system))))))
                 (replace 'configure
                   (lambda* (#:key outputs build configure-flags
                             #:allow-other-keys)
                     ;; It's an old 'configure' script so it needs some help.
                     (setenv "CONFIG_SHELL" (which "sh"))
                     (apply invoke "./configure"
                            (string-append "--prefix=" #$output)
                            (string-append "--build=" build)
                            (string-append "--host=" build)
                            configure-flags)))
                 (add-before 'configure 'remove-bundled-texinfo
                   (lambda _
                     ;; Go ahead despite the many warnings.
                     (substitute* '("Makefile.in" "gcc/Makefile.in")
                       (("^MAKEINFOFLAGS =.*")
                        "MAKEINFOFLAGS = --force\n"))))))))
    (native-search-paths
     ;; This package supports nothing but the C language.
     (list $C_INCLUDE_PATH
           $LIBRARY_PATH))))


(define-public (make-libstdc++ gcc)
  "Return a libstdc++ package based on GCC.  The primary use case is when
using compilers other than GCC."
  (package
    (inherit gcc)
    (name "libstdc++")
    (arguments
     (list
      #:out-of-source? #t
      #:modules `((srfi srfi-1)
                  (srfi srfi-26)
                  ,@%default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (version>=? (package-version gcc) "11")
                 #~((add-after 'unpack 'hide-gcc-headers
                      (lambda* (#:key native-inputs inputs #:allow-other-keys)
                        (let ((gcc (assoc-ref (or native-inputs inputs)
                                              #$(if (%current-target-system)
                                                    "cross-gcc"
                                                    "gcc"))))
                          ;; Fix a regression in GCC 11 where the GCC headers
                          ;; shadows glibc headers when building libstdc++.  An
                          ;; upstream fix was added in GCC 11.3.0, but it only
                          ;; hides system include directories, not those on
                          ;; CPLUS_INCLUDE_PATH.  See discussion at
                          ;; <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100017>
                          ;; and the similar adjustment in GCC-FINAL.
                          (substitute* "libstdc++-v3/src/c++17/Makefile.in"
                            (("AM_CXXFLAGS = ")
                             (string-append #$(if (%current-target-system)
                                                  "CROSS_CPLUS_INCLUDE_PATH = "
                                                  "CPLUS_INCLUDE_PATH = ")
                                            (string-join
                                             (remove (cut string-prefix? gcc <>)
                                                     (string-split
                                                      (getenv
                                                       #$(if (%current-target-system)
                                                             "CROSS_CPLUS_INCLUDE_PATH"
                                                             "CPLUS_INCLUDE_PATH"))
                                                      #\:))
                                             ":")
                                            "\nAM_CXXFLAGS = ")))))))
                 '())
          #$@(let ((version (package-version gcc)))
               (if (and (target-ppc64le?)
                       (version>=? version "11")
                       (not (version>=? version "12")))
                   ;; TODO: Drop the 'else' branch below on next rebuild
                   ;; cycle.
                   (if (%current-target-system)
                       #~((add-after 'unpack 'patch-powerpc ;correct
                            (lambda* (#:key native-inputs inputs #:allow-other-keys)
                              (invoke "patch" "--force" "-p1" "-i"
                                      (assoc-ref (or native-inputs inputs)
                                                 "powerpc64le-patch")))))
                       #~((add-after 'unpack 'patch-powerpc ;wrong
                            (lambda* (#:key inputs #:allow-other-keys)
                              (invoke "patch" "--force" "-p1" "-i"
                                      (assoc-ref inputs "powerpc64le-patch"))))))
                   '()))
          ;; Force rs6000 (i.e., powerpc) libdir to be /lib and not /lib64.
          (add-after 'unpack 'fix-rs6000-libdir
            (lambda _
              (when (file-exists? "gcc/config/rs6000")
                (substitute* (find-files "gcc/config/rs6000")
                  (("/lib64") "/lib")))))
          (add-before 'configure 'chdir
            (lambda _
              (chdir "libstdc++-v3")))
          #$@(let ((version (package-version gcc)))
               (if (target-hurd64?)
                   #~((add-after 'unpack 'patch-hurd64
                        (lambda _
                          (substitute* "libstdc++-v3/src/c++20/tzdb.cc"
                            (("#if ! defined _GLIBCXX_ZONEINFO_DIR")
                             "#if __GNU__ || ! defined _GLIBCXX_ZONEINFO_DIR")))))
                   '())))

      #:configure-flags '`("--disable-libstdcxx-pch"
                           ,(string-append "--with-gxx-include-dir="
                                           (assoc-ref %outputs "out")
                                           "/include"))))
    (outputs '("out" "debug"))
    (inputs '())
    (native-inputs
     `(,@(if (and (target-ppc64le?)
                  (let ((version (package-version gcc)))
                    (and
                     (version>=? version "11")
                     (not (version>=? version "12")))))
             `(("powerpc64le-patch" ,(search-patch "gcc-11-libstdc++-powerpc.patch")))
             '())))
    (propagated-inputs '())
    (synopsis "GNU C++ standard library")))

(define libstdc++
  ;; Libstdc++ matching the default GCC.
  (make-libstdc++ gcc))

(define libstdc++-headers
  ;; XXX: This package is for internal use to work around
  ;; <https://bugs.gnu.org/42392> (see above).  The main difference compared
  ;; to the libstdc++ headers that come with 'gcc' is that <bits/c++config.h>
  ;; is right under include/c++ and not under
  ;; include/c++/x86_64-unknown-linux-gnu (aka. GPLUSPLUS_TOOL_INCLUDE_DIR).
  (package
    (inherit libstdc++)
    (name "libstdc++-headers")
    (outputs '("out"))
    (build-system trivial-build-system)
    (arguments
     '(#:builder (let* ((out       (assoc-ref %outputs "out"))
                        (libstdc++ (assoc-ref %build-inputs "libstdc++")))
                   (mkdir out)
                   (mkdir (string-append out "/include"))
                   (symlink (string-append libstdc++ "/include")
                            (string-append out "/include/c++")))))
    (inputs `(("libstdc++" ,libstdc++)))
    (synopsis "Headers of GNU libstdc++")))

(define-public libstdc++-4.9
  (make-libstdc++ gcc-4.9))

(define (make-libiberty gcc)
  "Return a libiberty package based on GCC."
  (package
    (inherit gcc)
    (name "libiberty")
    (arguments
     `(#:out-of-source? #t
       #:make-flags '("CFLAGS=-O2 -g -fPIC")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "libiberty")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (lib     (string-append out "/lib/"))
                    (include (string-append out "/include/")))
               (install-file "libiberty.a" lib)
               (install-file "../include/demangle.h" include)
               (install-file "../include/libiberty.h" include)))))))
    (inputs '())
    (outputs '("out"))
    (native-inputs '())
    (propagated-inputs '())
    (properties '())
    (synopsis "Collection of subroutines used by various GNU programs")))

(define-public libiberty
  (make-libiberty gcc))

(define* (custom-gcc gcc name languages
                     #:optional
                     (search-paths (package-native-search-paths gcc))
                     #:key (separate-lib-output? #t))
  "Return a custom version of GCC that supports LANGUAGES.  Use SEARCH-PATHS
as the 'native-search-paths' field."
  (package (inherit gcc)
    (name name)
    (outputs (if separate-lib-output?
                 (package-outputs gcc)
                 (delete "lib" (package-outputs gcc))))
    (native-search-paths search-paths)
    (arguments
     (substitute-keyword-arguments (package-arguments gcc)
       ((#:modules modules %default-gnu-modules)
        `(,@modules
          (srfi srfi-1)
          (srfi srfi-26)
          (ice-9 regex)))
       ((#:configure-flags flags)
        #~(cons (string-append "--enable-languages="
                               #$(string-join languages ","))
                (remove (cut string-match "--enable-languages.*" <>)
                        #$flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-broken-or-conflicting-files
              (lambda* (#:key outputs #:allow-other-keys)
                (for-each
                 delete-file
                 (find-files (string-append (assoc-ref outputs "out") "/bin")
                             ".*(c\\+\\+|cpp|g\\+\\+|gcov|gcc|lto)(-.*)?$"))))))))
    (properties `((upstream-name . "gcc")
                  ,@(alist-delete 'hidden? (package-properties gcc))))))

(define %generic-search-paths
  ;; This is the language-neutral search path for GCC.  Entries in $CPATH are
  ;; not considered "system headers", which means GCC can raise warnings for
  ;; issues in those headers.  'CPATH' is the only one that works for
  ;; front-ends not in the C family.
  (list (search-path-specification
         (variable "CPATH")
         (files '("include")))
        (search-path-specification
         (variable "LIBRARY_PATH")
         (files '("lib" "lib64")))))

(define-public gfortran-13
  (hidden-package
   (custom-gcc gcc-13
               "gfortran" '("fortran")
               %generic-search-paths)))

(define-public gfortran
  (hidden-package
   (custom-gcc gcc
               "gfortran" '("fortran")
               %generic-search-paths)))

(define-public gfortran-9
  (hidden-package
   (custom-gcc gcc-9
               "gfortran" '("fortran")
               %generic-search-paths)))

(define-public gfortran-7
  (hidden-package
   (custom-gcc gcc-7
               "gfortran" '("fortran")
               %generic-search-paths)))

(define-public gdc-10
  (hidden-package
   (custom-gcc gcc-10 "gdc" '("d")
               %generic-search-paths)))

(define-public gdc-11
  (hidden-package
   (custom-gcc gcc-11 "gdc" '("d")
               %generic-search-paths)))

;;; Alias tracking the latest GDC version.
(define-public gdc
  (hidden-package
   (custom-gcc gcc "gdc" '("d")
               %generic-search-paths)))

(define-public (make-libgccjit gcc)
  (package
    (inherit gcc)
    (name "libgccjit")
    (outputs (delete "lib" (package-outputs gcc)))
    (properties (alist-delete 'hidden? (package-properties gcc)))
    (arguments
     (substitute-keyword-arguments (package-arguments gcc)
       ((#:modules _ '())
        '((guix build gnu-build-system)
          (guix build utils)
          (ice-9 regex)
          (srfi srfi-1)
          (srfi srfi-26)))
       ((#:configure-flags flags)
        #~(cons* "--disable-bootstrap"
                 "--disable-libatomic"
                 "--disable-libgomp"
                 "--disable-libquadmath"
                 "--disable-libssp"
                 "--enable-host-shared"
                 "--enable-checking=release"
                 "--enable-languages=jit"
                 (remove (cut string-match "--enable-languages.*" <>)
                         #$flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-broken-or-conflicting-files
              (lambda* (#:key outputs #:allow-other-keys)
                (for-each delete-file
                          (find-files
                           (string-append (assoc-ref outputs "out") "/bin")
                           ".*(c\\+\\+|cpp|g\\+\\+|gcov|gcc|gcc-.*)"))))))))
    (inputs (modify-inputs (package-inputs gcc)
              (delete "libstdc++")))
    (native-inputs (modify-inputs (package-native-inputs gcc)
                     (prepend gcc)))
    (synopsis "GCC library generating machine code on-the-fly at runtime")
    (description
     "This package is part of the GNU Compiler Collection and provides an
embeddable library for generating machine code on-the-fly at runtime.  This
shared library can then be dynamically-linked into bytecode interpreters and
other such programs that want to generate machine code on-the-fly at run-time.
It can also be used for ahead-of-time code generation for building standalone
compilers.  The just-in-time (jit) part of the name is now something of a
misnomer.")))

(define-public libgccjit-9 (make-libgccjit gcc-9))
(define-public libgccjit-10 (make-libgccjit gcc-10))
(define-public libgccjit-11 (make-libgccjit gcc-11))
(define-public libgccjit-12 (make-libgccjit gcc-12))
(define-public libgccjit-14 (make-libgccjit gcc-14))
(define-public libgccjit-15 (make-libgccjit gcc-15))

;; This must match the 'gcc' variable, but it must also be 'eq?' to one of the
;; libgccjit-* packages above.
(define-public libgccjit libgccjit-11)

(define (make-gccgo gcc)
  "Return a gccgo package based on GCC."
  (let ((gccgo (custom-gcc gcc "gccgo" '("go") %generic-search-paths)))
    (package
      (inherit gccgo)
      (synopsis "Go frontend to GCC")
      (description
        "This package is part of the GNU Compiler Collection and
provides the GNU compiler for the Go programming language.")
      (arguments
       (substitute-keyword-arguments (package-arguments gccgo)
         ((#:phases phases)
          #~(modify-phases #$phases
              #$@(if (and (version>=? (package-version gccgo) "12.0")
                          ;; This somehow breaks gccgo@12 on riscv64-linux.
                          (not (and (target-riscv64?)
                                    (string=? (version-prefix
                                                (package-version gccgo) 1)
                                               "12"))))
                     #~((add-after 'unpack 'adjust-libgo-dependencies
                          (lambda _
                            (substitute* "Makefile.in"
                              ;; libgo.la depends on libbacktrace.la but the
                              ;; current dependency rules don't have libbacktrace
                              ;; building early enough for libgo.  When built
                              ;; with more than 1 core this issue doesn't appear.
                              ;; see commit 5fee5ec362f7a243f459e6378fd49dfc89dc9fb5.
                              (("all-target-libgo: maybe-all-target-libffi")
                               (string-append
                                 "all-target-libgo: maybe-all-target-libbacktrace\n"
                                 "all-target-libgo: maybe-all-target-libffi\n"
                                 "all-target-libgo: maybe-all-target-libatomic"))))))
                     #~())
              (add-after 'install 'wrap-go-with-tool-path
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (exedir (string-append out "/libexec/gcc"))
                         (tooldir (dirname (car (find-files exedir "^cgo$")))))
                    (wrap-program (string-append out "/bin/go")
                      `("GCCGOTOOLDIR" =
                        (,(string-append "${GCCGOTOOLDIR:-" tooldir "}")))
                      `("GOROOT" =
                        (,(string-append "${GOROOT:-" out "}")))))))
              (add-before 'configure 'fix-gotools-runpath
                (lambda _
                  (substitute* "gotools/Makefile.in"
                    (("AM_LDFLAGS =" all)
                     (string-append all " -Wl,-rpath=$(libdir) ")))))
              (add-before 'configure 'remove-tool-reference-from-libgo
                (lambda _
                  (substitute* "libgo/Makefile.in"
                    (("(GccgoToolDir = \\\")[^\\\"]+" _ start)
                     (string-append start "/nonexistent"))
                    #$@(if (version>=? (package-version gccgo) "12.0")
                           '((("(defaultGOROOT = `)[^`]+" _ start)
                              (string-append start "/nonexistent")))
                           '((("(DefaultGoroot = \\\")[^\\\"]+" _ start)
                              (string-append start "/nonexistent"))))
                    (("(defaultGOROOTValue.*?return `)[^`]+" _ start)
                     (string-append start "/nonexistent"))))))))))))

(define-public gccgo-4.9
  (custom-gcc (package
                (inherit gcc-4.9)
                (synopsis "Go frontend to GCC")
                (description
                 "This package is part of the GNU Compiler Collection and
provides the GNU compiler for the Go programming language."))
              "gccgo" '("go")
              %generic-search-paths
              ;; Suppress the separate "lib" output, because otherwise the
              ;; "lib" and "out" outputs would refer to each other, creating
              ;; a cyclic dependency.  <http://debbugs.gnu.org/18101>
              #:separate-lib-output? #f))

;; Provides go-1.14.6
(define-public gccgo-10
  (make-gccgo gcc-10))

;; Provides go-1.16.5
(define-public gccgo-11
  (make-gccgo gcc-11))

;; Provides go-1.18
(define-public gccgo-12
  (make-gccgo gcc-12))

;; Provides go-1.18
(define-public gccgo-13
  (make-gccgo gcc-13))

;; Provides go-1.18
(define-public gccgo-14
  (make-gccgo gcc-14))

;; Provides go-1.18
(define-public gccgo-15
  (make-gccgo gcc-15))

(define (make-libstdc++-doc gcc)
  "Return a package with the libstdc++ documentation for GCC."
  (package
    (inherit gcc)
    (name "libstdc++-doc")
    (version (package-version gcc))
    (synopsis "GNU libstdc++ documentation")
    (outputs '("out"))
    (native-inputs (list doxygen
                         texinfo
                         libxml2
                         libxslt
                         docbook-xml
                         docbook-xsl
                         docbook2x
                         graphviz)) ;for 'dot', invoked by 'doxygen'
    (inputs '())
    (propagated-inputs '())
    (arguments
     (list
      #:out-of-source? #t
      #:tests? #f                                ;it's just documentation
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'chdir
            (lambda _
              (chdir "libstdc++-v3")))
          (replace 'build
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke `("make" ,@make-flags
                              "doc-info"
                              "doc-html"
                              "doc-man"))))
          (replace 'install
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke `("make" ,@make-flags
                              "doc-install-info"
                              "doc-install-html"
                              "doc-install-man")))))))
    (properties (alist-delete 'hidden? (package-properties gcc)))))

(define-public libstdc++-doc-5
  (make-libstdc++-doc gcc-5))

(define-public libstdc++-doc-9
  (make-libstdc++-doc gcc-9))

(define-public isl
  (package
    (name "isl")
    (version "0.24")
    (source (origin
             (method url-fetch)
             (uri (list (string-append "mirror://sourceforge/libisl/isl-"
                                       version ".tar.xz")
                        (string-append %gcc-infrastructure
                                       "isl-" version ".tar.xz")))
             (sha256
              (base32
               "1bgbk6n93qqn7w8v21kxf4x6dc3z0ypqrzvgfd46nhagak60ac84"))))
    (build-system gnu-build-system)
    (outputs '("out" "static"))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'move-static-library
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (static (assoc-ref outputs "static"))
                             (source (string-append out "/lib/libisl.a"))
                             (target (string-append static "/lib/libisl.a")))
                        (mkdir-p (dirname target))
                        (link source target)
                        (delete-file source)

                        ;; Remove reference to libisl.a from the .la file so
                        ;; libtool looks for it in the usual locations.
                        (substitute* (string-append out "/lib/libisl.la")
                          (("^old_library=.*")
                           "old_library=''\n")))))
                  ,@(if (target-loongarch64?)
                        `((add-after 'unpack 'update-config-scripts
                            (lambda* (#:key inputs native-inputs #:allow-other-keys)
                              ;; Replace outdated config.guess and config.sub.
                              (for-each (lambda (file)
                                          (install-file
                                           (search-input-file
                                            (or native-inputs inputs)
                                            (string-append "/bin/" file)) "."))
                                        '("config.guess" "config.sub")))))
                        '()))))
    (native-inputs (if (target-loongarch64?)
                       (list config)
                       '()))
    (inputs (list gmp))
    (home-page "https://libisl.sourceforge.io/") ;https://repo.or.cz/w/isl.git
    (properties `((release-monitoring-url . ,home-page)))
    (synopsis
     "Manipulating sets and relations of integer points \
bounded by linear constraints")
    (description
     "isl is a library for manipulating sets and relations of integer points
bounded by linear constraints.  Supported operations on sets include
intersection, union, set difference, emptiness check, convex hull, (integer)
affine hull, integer projection, computing the lexicographic minimum using
parametric integer programming, coalescing and parametric vertex
enumeration.  It also includes an ILP solver based on generalized basis
reduction, transitive closures on maps (which may encode infinite graphs),
dependence analysis and bounds on piecewise step-polynomials.")
    (license lgpl2.1+)))

(define-public isl-0.18
  (package
    (inherit isl)
    (version "0.18")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "mirror://sourceforge/libisl/isl-"
                                        version ".tar.bz2")
                         (string-append %gcc-infrastructure
                                        "isl-" version ".tar.bz2")))
              (sha256
               (base32
                "06ybml6llhi4i56q90jnimbcgk1lpcdwhy9nxdxra2hxz3bhz2vb"))))))

(define-public isl-0.11
  (package
    (inherit isl)
    (name "isl")
    (version "0.11.1")
    (source (origin
             (method url-fetch)
             (uri (list (string-append "mirror://sourceforge/libisl/isl-"
                                       version ".tar.bz2")
                        (string-append %gcc-infrastructure
                                       "isl-" version ".tar.bz2")))
             (sha256
              (base32
               "13d9cqa5rzhbjq0xf0b2dyxag7pqa72xj9dhsa03m8ccr1a4npq9"))
             (patches (search-patches "isl-0.11.1-aarch64-support.patch"))))))

(define-public cloog
  (package
    (name "cloog")
    (version "0.18.0")
    (source
     (origin
      (method url-fetch)
      (uri (list (string-append
                  "http://www.bastoul.net/cloog/pages/download/count.php3?url=cloog-"
                  version
                  ".tar.gz")
                 (string-append %gcc-infrastructure
                                name "-" version ".tar.gz")))
      (sha256
       (base32
        "0a12rwfwp22zd0nlld0xyql11cj390rrq1prw35yjsw8wzfshjhw"))
      (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs (list gmp isl-0.11))
    (arguments '(#:configure-flags '("--with-isl=system")))
    (home-page "http://www.cloog.org/")
    (synopsis "Library to generate code for scanning Z-polyhedra")
    (description
     "CLooG is a free software library to generate code for scanning
Z-polyhedra.  That is, it finds a code (e.g., in C, FORTRAN...) that
reaches each integral point of one or more parameterized polyhedra.
CLooG has been originally written to solve the code generation problem
for optimizing compilers based on the polytope model.  Nevertheless it
is used now in various area e.g., to build control automata for
high-level synthesis or to find the best polynomial approximation of a
function.  CLooG may help in any situation where scanning polyhedra
matters.  While the user has full control on generated code quality,
CLooG is designed to avoid control overhead and to produce a very
effective code.")
    (license gpl2+)))

(define-public gnu-c-manual
  (package
    (name "gnu-c-manual")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gnu-c-manual/gnu-c-manual-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1sfsj9256w18qzylgag2h5h377aq8in8929svblfnj9svfriqcys"))))
    (build-system gnu-build-system)
    (native-inputs (list texinfo))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'check)
                  (replace 'build
                           (lambda _
                             (invoke "make"
                                     "gnu-c-manual.info"
                                     "gnu-c-manual.html")))
                  (replace 'install
                           (lambda* (#:key outputs #:allow-other-keys)
                             (let* ((out (assoc-ref outputs "out"))
                                    (info (string-append out "/share/info"))
                                    (html (string-append
                                           out "/share/doc/gnu-c-manual")))
                               (mkdir-p info)
                               (mkdir-p html)

                               (for-each (lambda (file)
                                           (copy-file file
                                                      (string-append info "/"
                                                                     file)))
                                         (find-files "." "\\.info(-[0-9])?$"))
                               (for-each (lambda (file)
                                           (copy-file file
                                                      (string-append html "/"
                                                                     file)))
                                         (find-files "." "\\.html$"))))))))
    (synopsis "Reference manual for the C programming language")
    (description
     "This is a reference manual for the C programming language, as
implemented by the GNU C Compiler (gcc).  As a reference, it is not intended
to be a tutorial of the language.  Rather, it outlines all of the constructs
of the language.  Library functions are not included.")
    (home-page "https://www.gnu.org/software/gnu-c-manual/")
    (license fdl1.3+)))
