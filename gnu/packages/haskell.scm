;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Tonton <tonton@riseup.net>
;;; Copyright © 2018, 2019, 2020 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;; Copyright © 2019 Jacob MacDonald <jaccarmac@gmail.com>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages haskell)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

(define-public cl-yale-haskell
  (let ((commit "85f94c72a16c5f70301dd8db04cde9de2d7dd270")
        (revision "1"))
    (package
      (name "cl-yale-haskell")
      (version (string-append "2.0.5-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/yale-haskell.git")
                      (commit commit)))
                (file-name (string-append "yale-haskell-" commit "-checkout"))
                (sha256
                 (base32
                  "0bal3m6ryrjamz5p93bhs9rp5msk8k7lpcqr44wd7xs9b9k8w74g"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no tests
         ;; Stripping binaries leads to a broken executable lisp system image.
         #:strip-binaries? #f
         #:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda _
               (setenv "PRELUDE" "./progs/prelude")
               (setenv "HASKELL_LIBRARY" "./progs/lib")
               (setenv "PRELUDEBIN" "./progs/prelude/clisp")
               (setenv "HASKELLPROG" "./bin/clisp-haskell")
               #t)))))
      (inputs
       (list clisp))
      (home-page "https://git.elephly.net/software/yale-haskell.git")
      (synopsis "Port of the Yale Haskell system to CLISP")
      (description "This package provides the Yale Haskell system running on
top of CLISP.")
      (license license:bsd-4))))

;; This package contains lots of generated .hc files containing C code to
;; bootstrap the compiler without a Haskell compiler.  The included .hc files
;; cover not just the compiler sources but also all Haskell libraries.
(define-public nhc98
  (package
    (name "nhc98")
    (version "1.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/nhc98/nhc98src-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0fkgxgsd2iqxvwcgnad1702kradwlbcal6rxdrgb22vd6dnc3i8l"))
       (patches (search-patches "nhc98-c-update.patch"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     (list
      #:tests? #false                   ;there is no test target
      #:system "i686-linux"
      #:parallel-build? #false          ;not supported
      #:strip-binaries? #false          ;doesn't work
      #:make-flags '(list "all-gcc")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (setenv "SHELL" (which "sh"))
              (setenv "CPATH" (string-append
                               (getcwd) "/src/runtime/Kernel:"
                               (or (getenv "C_INCLUDE_PATH") "")))
              (substitute* "configure"
                (("echo '#!/bin/sh'")
                 (string-append "echo '#!" (which "sh") "'")))
              (with-fluids ((%default-port-encoding #f))
                (substitute* '("script/greencard.inst"
                               "script/harch.inst"
                               "script/hi.inst"
                               "script/hmake-config.inst"
                               ;; TODO: can't fix this with substitute*
                                        ;"script/hmake.inst"
                               "script/hood.inst"
                               "script/hsc2hs.inst"
                               "script/nhc98-pkg.inst"
                               "script/nhc98.inst")
                  (("^MACHINE=.*") "MACHINE=ix86-Linux\n")))
              (invoke "sh" "configure"
                      (string-append "--prefix=" #$output)
                      ;; Remove -m32 from compiler/linker invocation
                      "--ccoption="
                      "--ldoption=")))
          (replace 'install
            (lambda _
              (invoke "sh" "configure"
                      (string-append "--prefix=" #$output)
                      ;; Remove -m32 from compiler/linker invocation
                      "--ccoption="
                      "--ldoption="
                      "--install"))))))
    (home-page "https://www.haskell.org/nhc98")
    (synopsis "Nearly a Haskell Compiler")
    (description
     "nhc98 is a small, standards-compliant compiler for Haskell 98, the lazy
functional programming language.  It aims to produce small executables that
run in small amounts of memory.  It produces medium-fast code, and compilation
is itself quite fast.")
    (license
     (license:non-copyleft
      "https://www.haskell.org/nhc98/copyright.html"))))

(define-public ghc-4
  (package
    (name "ghc")
    (version "4.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/" name "-" version "-src.tar.bz2"))
       (sha256
        (base32
         "0ar4nxy4cr5vwvfj71gmc174vx0n3lg9ka05sa1k60c8z0g3xp1q"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     (list
      #:system "i686-linux"
      #:strip-binaries? #f
      #:parallel-build? #f
      #:implicit-inputs? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-generated-c-code
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((tarball
                     (match inputs
                       (((_ . locations) ...)
                        (let ((suffix (string-append "ghc-"
                                                     #$(package-version this-package)
                                                     "-x86-hc.tar.bz2")))
                          (find (lambda (location)
                                  (string-suffix? suffix location))
                                locations))))))
                (invoke "tar" "-xvf" tarball
                        "--strip-components=1"))))
          (replace 'bootstrap
            (lambda* (#:key inputs #:allow-other-keys)
              (delete-file "configure")
              (delete-file "config.sub")
              (install-file (search-input-file inputs
                                               "/bin/config.sub")
                            ".")

              ;; Avoid dependency on "happy"
              (substitute* "configure.in"
                (("FPTOOLS_HAPPY") "echo sure\n"))

              (let ((bash (which "bash")))
                (substitute* '("configure.in"
                               "ghc/configure.in"
                               "ghc/rts/gmp/mpn/configure.in"
                               "ghc/rts/gmp/mpz/configure.in"
                               "ghc/rts/gmp/configure.in"
                               "distrib/configure-bin.in")
                  (("`/bin/sh") (string-append "`" bash))
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash))
                  (("^#! /bin/sh") (string-append "#! " bash)))

                (substitute* '("mk/config.mk.in"
                               "ghc/rts/gmp/mpz/Makefile.in"
                               "ghc/rts/gmp/Makefile.in")
                  (("^SHELL.*=.*/bin/sh") (string-append "SHELL = " bash)))
                (substitute* "aclocal.m4"
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash)))
                (substitute* '("ghc/lib/std/cbits/system.c"
                               "hslibs/posix/cbits/execvpe.c")
                  (("/bin/sh") bash)
                  (("\"sh\"") (string-append "\"" bash "\"")))

                (setenv "CONFIG_SHELL" bash)
                (setenv "SHELL" bash))

              ;; The 'hscpp' script invokes GCC 2.95's 'cpp' (RAWCPP), which
              ;; segfaults unless passed '-x c'.
              (substitute* "mk/config.mk.in"
                (("-traditional")
                 "-traditional -x c"))

              (setenv "CPP" (which "cpp"))
              (invoke "autoreconf" "--verbose" "--force")))
          (add-before 'configure 'configure-gmp
            (lambda _
              (with-directory-excursion "ghc/rts/gmp"
                (invoke "./configure"))))
          (replace 'configure
            (lambda* (#:key build #:allow-other-keys)
              (call-with-output-file "config.cache"
                (lambda (port)
                  ;; GCC 2.95 fails to deal with anonymous unions in glibc's
                  ;; 'struct_rusage.h', so skip that.
                  (display "ac_cv_func_getrusage=no\n" port)))

              ;; CLK_TCK has been removed from recent libc.
              (substitute* "ghc/interpreter/nHandle.c"
                (("CLK_TCK") "sysconf (_SC_CLK_TCK)"))
              ;; Avoid duplicate definitions of execvpe
              (substitute* "ghc/lib/std/cbits/stgio.h"
                (("^int.*execvpe.*") ""))
              ;; gid_t is an undefined type
              (substitute* "hslibs/posix/PosixProcEnv.lhs"
                (("gid_t") "int"))

              ;; This is needed so that ghc/includes/Stg.h can see config.h,
              ;; which defines values that are important for
              ;; ghc/includes/StgTypes.h and others.
              (setenv "CPATH"
                      (string-append (getcwd) "/ghc/includes:"
                                     (getcwd) "/ghc/rts/gmp:"
                                     (getcwd) "/mk:"
                                     (or (getenv "CPATH") "")))

              (with-output-to-file "mk/build.mk"
                (lambda ()
                  (display "
ProjectsToBuild = glafp-utils hslibs ghc
GhcLibWays=
GhcHcOpts=-DDEBUG
GhcLibHcOpts= -O
GhcRtsHcOpts=-optc-D_GNU_SOURCE=1 -optc-DDEBUG
GhcRtsCcOpts=-optc-D_GNU_SOURCE=1 -optc-DDEBUG
SplitObjs=YES
GhcWithHscBuiltViaC=YES
")))
              (invoke "./configure"
                      "--enable-hc-boot" ; boot from C "source" files
                      ;; Embed the absolute file name of GCC 2.95 in the GHC
                      ;; driver script.
                      (string-append "--with-gcc=" (which "gcc"))
                      (string-append "--prefix=" #$output)
                      (string-append "--build=" build)
                      (string-append "--host=" build))))
          ;; Build hsc
          (add-before 'build 'make-boot
            (lambda _
              ;; Avoid calling happy
              (invoke "touch" "ghc/compiler/rename/ParseIface.hs")
              (invoke "touch" "ghc/compiler/parser/Parser.hs")
              (invoke "make" "boot" "all")))
          ;; Build libraries
          (replace 'build
            (lambda _
              ;; Build these from their Haskell sources.
              (invoke "sh" "-c" "echo GhcWithHscBuiltViaC=NO >>mk/build.mk")
              (with-directory-excursion "ghc/lib"
                (invoke "make" "clean" "boot" "all"))
              (with-directory-excursion "hslibs"
                (invoke "make" "clean" "boot" "all"))))
          (add-before 'install 'do-not-strip
            (lambda _
              (substitute* '("install-sh"
                             "ghc/rts/gmp/install.sh")
                (("^stripprog=.*") "stripprog=echo\n"))
              (substitute* "mk/opts.mk"
                (("^SRC_INSTALL_BIN_OPTS.*") "")))))))
    (native-inputs
     (modify-inputs (%final-inputs)
       (delete "binutils" "gcc")
       (prepend
           autoconf-2.13
           bison                        ;for parser.y
           config

           ;; Use an older assembler to work around this error in GMP:
           ;;   Error: `%edx' not allowed with `testb'
           binutils-2.33

           ;; Needed to support lvalue casts.
           gcc-2.95

           ;; Perl used to allow setting $* to enable multi-line matching.  If
           ;; we want to use a more recent Perl we need to patch all
           ;; expressions that require multi-line matching.  Hard to tell.
           perl-5.6

           ;; This is the secret sauce.  These files are macro-heavy C
           ;; "source" files that are used to build hsc from C.  They are
           ;; presumably the output of previous versions of GHC.  Note that
           ;; this is the "registerized" variant for x86.  An "unreg" variant
           ;; of the *.hc files also exists for building GHC for other
           ;; architectures.  The default "way" (see GhcLibWays above) to
           ;; build and link the GHC binaries, however, is not the
           ;; unregisterized variant.  Using the unregisterized *.hc files
           ;; with a standard build will result in segfaults.
           (origin
             (method url-fetch)
             (uri (string-append "http://downloads.haskell.org/~ghc/"
                                 version "/ghc-" version "-x86-hc.tar.bz2"))
             (sha256
              (base32
               "0fi60bj0ak391x31cq5wp1ffwavl5w9jffyf62yv9rhxa915596b"))))))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-6.0
  (package
    (name "ghc")
    (version "6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.haskell.org/~ghc/"
                           version "/" name "-" version "-src.tar.bz2"))
       (sha256
        (base32
         "06hpl8wyhhs1vz9dcdf0vbybwyzb5ifh27d59rx42q1vjs0m8zdv"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     (list
      #:system "i686-linux"
      #:tests? #false ;no check target
      #:implicit-inputs? #false
      #:parallel-build? #false ;not supported
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (srfi srfi-1))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'bootstrap
            (lambda* (#:key inputs #:allow-other-keys)
              (delete-file "configure")
              (delete-file "config.sub")
              (install-file (search-input-file inputs
                                               "/bin/config.sub")
                            ".")
              (let ((bash (which "bash")))
                (substitute* '("configure.in"
                               "ghc/configure.in"
                               "ghc/rts/gmp/configure.in"
                               "distrib/configure-bin.in")
                  (("`/bin/sh") (string-append "`" bash))
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash))
                  (("^#! /bin/sh") (string-append "#! " bash)))
                (substitute* "glafp-utils/runstdtest/runstdtest.prl"
                  (("^#! /bin/sh") (string-append "#! " bash))
                  (("TimeCmd /bin/sh")
                   (string-append "TimeCmd " bash)))
                (substitute* '("mk/config.mk.in"
                               "ghc/rts/gmp/Makefile.in")
                  (("^SHELL.*=.*/bin/sh") (string-append "SHELL = " bash)))
                (substitute* "aclocal.m4"
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash)))
                (substitute* '"ghc/compiler/Makefile"
                  (("#!/bin/sh") (string-append "#!" bash)))
                (substitute* '("libraries/base/cbits/system.c"
                               "libraries/unix/cbits/execvpe.c")
                  (("/bin/sh") bash)
                  (("\"sh\"") (string-append "\"" bash "\"")))

                (setenv "CONFIG_SHELL" bash)
                (setenv "SHELL" bash))
              (invoke "autoreconf" "--verbose" "--force")))
          (replace 'configure
            (lambda* (#:key build #:allow-other-keys)
              (setenv "CPATH"
                      (string-append (getcwd) "/ghc/includes:"
                                     (getcwd) "/ghc/rts/gmp:"
                                     (getcwd) "/mk:"
                                     (or (getenv "CPATH") "")))
              (call-with-output-file "config.cache"
                (lambda (port)
                  ;; GCC 2.95 fails to deal with anonymous unions in glibc's
                  ;; 'struct_rusage.h':
                  ;; Stats.c: In function `pageFaults':
                  ;; Stats.c:270: structure has no member named `ru_majflt'
                  ;; Stats.c:272: warning: control reaches end of non-void function
                  (display "ac_cv_func_getrusage=no\n" port)))

              ;; Socket.hsc:887: sizeof applied to an incomplete type
              ;; Socket.hsc:893: dereferencing pointer to incomplete type
              (substitute* "libraries/network/Network/Socket.hsc"
                (("ifdef SO_PEERCRED")
                 "ifdef SO_PEERCRED_NEVER"))
              (invoke "./configure"
                      "--enable-src-tree-happy"
                      (string-append "--with-gcc=" (which "gcc"))
                      (string-append "--prefix=" #$output)
                      (string-append "--build=" build)
                      (string-append "--host=" build)))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (native-inputs
     (modify-inputs (%final-inputs)
       (delete "gcc")
       (prepend autoconf-2.13
                config
                flex
                ;; Perl used to allow setting $* to enable multi-line matching.  If
                ;; we want to use a more recent Perl we need to patch all
                ;; expressions that require multi-line matching.  Hard to tell.
                perl-5.6
                ghc-4
                gcc-2.95)))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-6.6
  (package
    (name "ghc")
    (version "6.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.haskell.org/~ghc/"
                           version "/" name "-" version "-src.tar.bz2"))
       (sha256
        (base32
         "0znc9myxyfg9zmvdlg09sf0dq11kc2bq4616llh82v6m6s8s5ckr"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     (list
      #:system "i686-linux"
      #:tests? #false ;no check target
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (srfi srfi-1))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'bootstrap
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bash (which "bash")))
                (substitute* '("configure"
                               "rts/gmp/configure"
                               "distrib/configure-bin.ac")
                  (("`/bin/sh") (string-append "`" bash))
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash))
                  (("^#! /bin/sh") (string-append "#! " bash)))
                (substitute* "utils/runstdtest/runstdtest.prl"
                  (("^#! /bin/sh") (string-append "#! " bash))
                  (("TimeCmd /bin/sh")
                   (string-append "TimeCmd " bash)))
                (substitute* '("mk/config.mk.in"
                               "rts/gmp/Makefile.in")
                  (("^SHELL.*=.*/bin/sh") (string-append "SHELL = " bash)))
                (substitute* "aclocal.m4"
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash)))
                (substitute* "compiler/Makefile"
                  (("#!/bin/sh") (string-append "#!" bash)))
                (substitute* '("libraries/base/cbits/execvpe.c"
                               "libraries/Cabal/Distribution/attic"
                               "libraries/Cabal/Distribution/Simple/Register.hs"
                               "libraries/base/System/Process/Internals.hs")
                  (("/bin/sh") bash)
                  (("\"sh\"") (string-append "\"" bash "\"")))

                (setenv "CONFIG_SHELL" bash)
                (setenv "SHELL" bash))))
          (replace 'configure
            (lambda* (#:key build #:allow-other-keys)
              (setenv "CPATH"
                      (string-append (getcwd) "/includes:"
                                     (getcwd) "/rts/gmp:"
                                     (getcwd) "/mk:"
                                     (or (getenv "CPATH") "")))
              (invoke "./configure"
                      (string-append "--with-hc=" (which "ghc"))
                      (string-append "--with-gcc=" (which "gcc"))
                      (string-append "--prefix=" #$output)
                      (string-append "--build=" build)
                      (string-append "--host=" build)))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (native-inputs
     (modify-inputs (%final-inputs)
       (delete "gcc")
       (prepend m4
                ;; Perl used to allow setting $* to enable multi-line matching.  If
                ;; we want to use a more recent Perl we need to patch all
                ;; expressions that require multi-line matching.  Hard to tell.
                perl-5.6
                ghc-6.0
                gcc-4.9)))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-6.10
  (package
    (name "ghc")
    (version "6.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.haskell.org/~ghc/"
                           version "/" name "-" version "-src.tar.bz2"))
       (sha256
        (base32
         "0kakv05kqi92qbfgmhr57rvag10yvp338kjwzqczhkrgax98wsnn"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "libffi"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     (list
      #:system "i686-linux"
      #:tests? #false ;no check target
      #:parallel-build? #false ;fails when building libraries/*
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (srfi srfi-1))
      #:configure-flags
      #~(list
         (string-append "--with-gmp-libraries="
                        (assoc-ref %build-inputs "gmp") "/lib")
         (string-append "--with-gmp-includes="
                        (assoc-ref %build-inputs "gmp") "/include"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'bootstrap
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bash (which "bash")))
                ;; Use our libffi package
                (substitute* "rts/Makefile"
                  (("-I../libffi/build/include")
                   (string-append "-I"#$(this-package-input "libffi") "/include"))
                  (("-L../libffi/build/include")
                   (string-append "-L"#$(this-package-input "libffi") "/lib")))
                (substitute* '("Makefile"
                               "distrib/Makefile")
                  (("SUBDIRS = gmp libffi")
                   "SUBDIRS = gmp")
                  (("\\$\\(MAKE\\) -C libffi.*") ""))
                (substitute* "compiler/ghc.cabal.in"
                  (("../libffi/build/include")
                   (string-append #$(this-package-input "libffi") "/include")))

                ;; Do not use libbfd, because it complicates the build and
                ;; requires more patching.  Disable all debug and profiling
                ;; builds.
                (substitute* "mk/config.mk.in"
                  (("GhcRTSWays \\+= debug") "")
                  (("GhcRTSWays \\+= debug_dyn thr_dyn thr_debug_dyn")
                   "GhcRTSWays += thr_dyn")
                  (("thr thr_p thr_debug") "thr")
                  (("GhcLibWays=p") "GhcLibWays="))

                ;; Replace /bin/sh.
                (substitute* '("configure"
                               "distrib/configure-bin.ac")
                  (("`/bin/sh") (string-append "`" bash))
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash))
                  (("#! /bin/sh") (string-append "#! " bash)))
                (substitute* '("mk/config.mk.in")
                  (("^SHELL.*=.*/bin/sh") (string-append "SHELL = " bash)))
                (substitute* "aclocal.m4"
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash)))
                (substitute* '("libraries/unix/cbits/execvpe.c"
                               "libraries/Cabal/Distribution/Simple/Register.hs"
                               "libraries/process/System/Process/Internals.hs")
                  (("/bin/sh") bash)
                  (("\"sh\"") (string-append "\"" bash "\"")))))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (inputs
     (list gmp libffi))
    (native-inputs
     (list perl ghc-6.6))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-7.0
  (package
    (inherit ghc-6.10)
    (name "ghc")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.haskell.org/~ghc/"
                           version "/" name "-" version "-src.tar.bz2"))
       (sha256
        (base32
         "1vfhdvf9nls4pn1vy48ndy2s81klp1my6ch9dkg2373csvcpi6qs"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "utils/ghc-pwd/dist-boot")))))
    (arguments
     (list
      #:system "i686-linux"
      #:test-target "test"
      #:tests? #false           ;not yet
      #:parallel-build? #false  ;fails when building libraries/*
      ;; Don't pass --build=<triplet>, because the configure script
      ;; auto-detects slightly different triplets for --host and --target and
      ;; then complains that they don't match.
      #:build #f
      #:validate-runpath? #f    ; libraries can't find each other.
      #:configure-flags
      #~(list
         (string-append "--with-gmp-libraries="
                        (assoc-ref %build-inputs "gmp") "/lib")
         (string-append "--with-gmp-includes="
                        (assoc-ref %build-inputs "gmp") "/include"))
      #:make-flags
      #~(list (string-append "CONFIG_SHELL=" (assoc-ref %build-inputs "bash")
                             "/bin/bash"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'bootstrap
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bash (which "bash")))
                ;; Use our libffi package
                (substitute* "rts/ghc.mk"
                  (("-I../libffi/build/include")
                   (string-append "-I" #$(this-package-input "libffi") "/include"))
                  (("-L../libffi/build/include")
                   (string-append "-L" #$(this-package-input "libffi") "/lib"))
                  (("-DDEBUG") ""))
                (substitute* '("Makefile"
                               "distrib/Makefile")
                  (("SUBDIRS = gmp libffi")
                   "SUBDIRS = gmp")
                  (("\\$\\(MAKE\\) -C libffi.*") ""))
                (substitute* "compiler/ghc.cabal.in"
                  (("../libffi/build/include")
                   (string-append #$(this-package-input "libffi") "/include")))

                ;; Do not use libbfd, because it complicates the build and
                ;; requires more patching.  Disable all debug and profiling
                ;; builds.
                (substitute* "mk/config.mk.in"
                  (("GhcRTSWays \\+= debug") "")
                  (("thr thr_debug thr_l") "thr thr_l")
                  (("dyn debug_dyn") "dyn")
                  (("thr_dyn thr_debug_dyn") "thr_dyn")
                  (("GhcLibWays += p") "GhcLibWays +="))

                ;; Replace /bin/sh.
                (substitute* '("configure"
                               "distrib/configure.ac")
                  (("`/bin/sh") (string-append "`" bash))
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash))
                  (("#! /bin/sh") (string-append "#! " bash)))
                (substitute* '("mk/config.mk.in")
                  (("^SHELL.*=.*/bin/sh") (string-append "SHELL = " bash)))
                (substitute* "aclocal.m4"
                  (("SHELL=/bin/sh") (string-append "SHELL=" bash)))
                (substitute* "utils/ghc-pkg/ghc.mk"
                  (("#!/bin/sh") (string-append "#!" bash)))
                (substitute* '("libraries/unix/cbits/execvpe.c"
                               "libraries/Cabal/Distribution/Simple/Hugs.hs"
                               "libraries/Cabal/Distribution/Simple/Program/Script.hs"
                               "libraries/process/System/Process/Internals.hs")
                  (("/bin/sh") bash)
                  (("\"sh\"") (string-append "\"" bash "\"")))))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (inputs
     (list gmp libffi ncurses perl))
    (native-inputs
     (list perl ghc-6.10))))

(define ghc-bootstrap-x86_64-7.8.4
  (origin
    (method url-fetch)
    (uri
     "https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz")
    (sha256
     (base32
      "13azsl53xgj20mi1hj9x0xb32vvcvs6cpmvwx6znxhas7blh0bpn"))))

(define ghc-bootstrap-i686-7.8.4
  (origin
    (method url-fetch)
    (uri
     "https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-i386-unknown-linux-deb7.tar.xz")
    (sha256
     (base32
      "0wj5s435j0zgww70bj1d3f6wvnnpzlxwvwcyh2qv4qjq5z8j64kg"))))

;; 43 tests out of 3965 fail.
;;
;; Most of them do not appear to be serious:
;;
;; - some tests generate files referring to "/bin/sh" and "/bin/ls". I've not
;;   figured out how these references are generated.
;;
;; - Some tests allocate more memory than expected (ca. 3% above upper limit)
;;
;; - Some tests try to load unavailable libriries: Control.Concurrent.STM,
;;   Data.Vector, Control.Monad.State.
;;
;; - Test posix010 tries to check the existence of a user on the system:
;;   getUserEntryForName: does not exist (no such user)
(define-public ghc-7
  (package
    (name "ghc")
    (version "7.10.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.haskell.org/ghc/dist/"
                          version "/" name "-" version "-src.tar.xz"))
      (sha256
       (base32
        "1vsgmic8csczl62ciz51iv8nhrkm72lyhbz7p7id13y2w7fcx46g"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (outputs '("out" "doc"))
    (inputs
     `(("gmp" ,gmp)
       ("ncurses" ,ncurses)

       ;; Use a LibFFI variant without static trampolines to work around
       ;; <https://gitlab.haskell.org/ghc/ghc/-/issues/20051>.
       ("libffi" ,libffi-sans-static-trampolines)

       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32
             "0fk4xjw1x5lk2ifvgqij06lrbf1vxq9qfix86h9r16c0bilm3hah"))))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)                ; for tests (fails with python-3)
       ("ghostscript" ,ghostscript)        ; for tests
       ("patchelf" ,patchelf)
       ;; GHC is built with GHC. Therefore we need bootstrap binaries.
       ("ghc-binary"
        ,(if (string-match "x86_64" (or (%current-target-system) (%current-system)))
             ghc-bootstrap-x86_64-7.8.4
             ghc-bootstrap-i686-7.8.4))))
    (arguments
     (list
       #:test-target "test"
       ;; We get a smaller number of test failures by disabling parallel test
       ;; execution.
       #:parallel-tests? #f

       ;; Don't pass --build=<triplet>, because the configure script
       ;; auto-detects slightly different triplets for --host and --target and
       ;; then complains that they don't match.
       #:build #f

       #:modules '((guix build gnu-build-system)
                   (guix build utils)
                   (srfi srfi-26)
                   (srfi srfi-1))
       #:configure-flags
       #~(list
           (string-append "--with-gmp-libraries="
                          (assoc-ref %build-inputs "gmp") "/lib")
           (string-append "--with-gmp-includes="
                          (assoc-ref %build-inputs "gmp") "/include")
           "--with-system-libffi"
           (string-append "--with-ffi-libraries="
                          (assoc-ref %build-inputs "libffi") "/lib")
           (string-append "--with-ffi-includes="
                          (assoc-ref %build-inputs "libffi") "/include"))
       ;; FIXME: The user-guide needs dblatex, docbook-xsl and docbook-utils.
       ;; Currently we do not have the last one.
       ;; #:make-flags
       ;; (list "BUILD_DOCBOOK_HTML = YES")
       #:phases
       #~(let* ((ghc-bootstrap-path
                  (string-append (getcwd) "/" #$name "-" #$version "/ghc-bin"))
                (ghc-bootstrap-prefix
                  (string-append ghc-bootstrap-path "/usr" )))
           (modify-phases %standard-phases
             (add-after 'unpack 'unpack-bin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                (mkdir-p ghc-bootstrap-prefix)
                (with-directory-excursion ghc-bootstrap-path
                  (invoke "tar" "xvf" (assoc-ref inputs "ghc-binary")))))
             (add-after 'unpack-bin 'unpack-testsuite-and-fix-bins
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (with-directory-excursion ".."
                   (invoke "tar" "xvf" (assoc-ref inputs "ghc-testsuite")))
                 (substitute*
                   (list "testsuite/timeout/Makefile"
                         "testsuite/timeout/timeout.py"
                         "testsuite/timeout/timeout.hs"
                         "testsuite/tests/rename/prog006/Setup.lhs"
                         "testsuite/tests/programs/life_space_leak/life.test"
                         "libraries/process/System/Process/Internals.hs"
                         "libraries/unix/cbits/execvpe.c")
                   (("/bin/sh") (search-input-file inputs "/bin/sh"))
                   (("/bin/rm") "rm"))))
             (add-before 'configure 'install-bin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (with-directory-excursion
                   (string-append ghc-bootstrap-path "/ghc-7.8.4")
                   (invoke "make" "install"))))
             (add-before 'install-bin 'configure-bin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((binaries
                          (list
                            "./utils/ghc-pwd/dist-install/build/tmp/ghc-pwd"
                            "./utils/hpc/dist-install/build/tmp/hpc"
                            "./utils/haddock/dist/build/tmp/haddock"
                            "./utils/hsc2hs/dist-install/build/tmp/hsc2hs"
                            "./utils/runghc/dist-install/build/tmp/runghc"
                            "./utils/ghc-cabal/dist-install/build/tmp/ghc-cabal"
                            "./utils/hp2ps/dist/build/tmp/hp2ps"
                            "./utils/ghc-pkg/dist-install/build/tmp/ghc-pkg"
                            "./utils/unlit/dist/build/tmp/unlit"
                            "./ghc/stage2/build/tmp/ghc-stage2"))
                        (gmp (assoc-ref inputs "gmp"))
                        (gmp-lib (string-append gmp "/lib"))
                        (gmp-include (string-append gmp "/include"))
                        (ncurses-lib
                         (dirname (search-input-file inputs "/lib/libncurses.so")))
                        (ld-so (search-input-file inputs #$(glibc-dynamic-linker)))
                        (libtinfo-dir
                         (string-append ghc-bootstrap-prefix
                                        "/lib/ghc-7.8.4/terminfo-0.4.0.0")))
                   (with-directory-excursion
                     (string-append ghc-bootstrap-path "/ghc-7.8.4")
                     (setenv "CONFIG_SHELL" (which "bash"))
                     (setenv "LD_LIBRARY_PATH" gmp-lib)
                     ;; The binaries have "/lib64/ld-linux-x86-64.so.2" hardcoded.
                     (for-each
                      (cut invoke "patchelf" "--set-interpreter" ld-so <>)
                      binaries)
                     ;; The binaries include a reference to libtinfo.so.5 which
                     ;; is a subset of libncurses.so.5.  We create a symlink in a
                     ;; directory included in the bootstrap binaries rpath.
                     (mkdir-p libtinfo-dir)
                     (symlink
                      (string-append ncurses-lib "/libncursesw.so."
                                     ;; Extract "6.0" from "6.0-20170930" if a
                                     ;; dash-separated version tag exists.
                                     #$(let* ((v (package-version ncurses))
                                              (d (or (string-index v #\-)
                                                     (string-length v))))
                                         (version-major+minor (string-take v d))))
                      (string-append libtinfo-dir "/libtinfo.so.5"))

                     (setenv "PATH"
                             (string-append (getenv "PATH") ":"
                                            ghc-bootstrap-prefix "/bin"))
                     (invoke
                      (string-append (getcwd) "/configure")
                      (string-append "--prefix=" ghc-bootstrap-prefix)
                      (string-append "--with-gmp-libraries=" gmp-lib)
                      (string-append "--with-gmp-includes=" gmp-include))))))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-8.0
  (package
    (name "ghc")
    (version "8.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.haskell.org/ghc/dist/"
                          version "/" name "-" version "-src.tar.xz"))
      (sha256
       (base32 "1c8qc4fhkycynk4g1f9hvk53dj6a1vvqi6bklqznns6hw59m8qhi"))
      (patches
       (search-patches "ghc-8.0-fall-back-to-madv_dontneed.patch"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (outputs '("out" "doc"))
    (inputs
     ;; Use a LibFFI variant without static trampolines to work around
     ;; <https://gitlab.haskell.org/ghc/ghc/-/issues/20051>.
     (list gmp ncurses libffi-sans-static-trampolines))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)                ; for tests
       ("ghostscript" ,ghostscript)        ; for tests
       ;; GHC is built with GHC.
       ("ghc-bootstrap" ,ghc-7)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                  "https://www.haskell.org/ghc/dist/"
                  version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32 "1wjc3x68l305bl1h1ijd3yhqp2vqj83lkp3kqbr94qmmkqlms8sj")))) ))
    (arguments
     (list
       #:test-target "test"
       ;; We get a smaller number of test failures by disabling parallel test
       ;; execution.
       #:parallel-tests? #f

       ;; Don't pass --build=<triplet>, because the configure script
       ;; auto-detects slightly different triplets for --host and --target and
       ;; then complains that they don't match.
       #:build #f

       #:configure-flags
       #~(list
           (string-append "--with-gmp-libraries="
                          (assoc-ref %build-inputs "gmp") "/lib")
           (string-append "--with-gmp-includes="
                          (assoc-ref %build-inputs "gmp") "/include")
           "--with-system-libffi"
           (string-append "--with-ffi-libraries="
                          (assoc-ref %build-inputs "libffi") "/lib")
           (string-append "--with-ffi-includes="
                          (assoc-ref %build-inputs "libffi") "/include")
           (string-append "--with-curses-libraries="
                          (assoc-ref %build-inputs "ncurses") "/lib")
           (string-append "--with-curses-includes="
                          (assoc-ref %build-inputs "ncurses") "/include"))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'unpack-testsuite
             (lambda* (#:key inputs #:allow-other-keys)
               (with-directory-excursion ".."
                 (invoke "tar" "xvf" (assoc-ref inputs "ghc-testsuite")))))
           (add-before 'build 'fix-lib-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute*
                 (list "libraries/process/System/Process/Posix.hs"
                       "libraries/process/tests/process001.hs"
                       "libraries/process/tests/process002.hs"
                       "libraries/unix/cbits/execvpe.c")
                 (("/bin/sh") (search-input-file inputs "/bin/sh"))
                 (("/bin/ls") (search-input-file inputs "/bin/ls")))))
           (add-before 'build 'fix-environment
             (lambda _
               (unsetenv "GHC_PACKAGE_PATH")
               (setenv "CONFIG_SHELL" (which "bash"))))
           (add-before 'check 'fix-testsuite
             (lambda _
               (substitute*
                 (list "testsuite/timeout/Makefile"
                       "testsuite/timeout/timeout.py"
                       "testsuite/timeout/timeout.hs"
                       "testsuite/tests/programs/life_space_leak/life.test")
                 (("/bin/sh") (which "sh"))
                 (("/bin/rm") "rm")))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-8.4
  (package (inherit ghc-8.0)
    (name "ghc")
    (version "8.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/" name "-" version "-src.tar.xz"))
       (sha256
        (base32 "1ch4j2asg7pr52ai1hwzykxyj553wndg7wq93i47ql4fllspf48i"))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python)               ; for tests
       ("ghostscript" ,ghostscript)     ; for tests
       ;; GHC 8.4.4 is built with GHC >= 8.0.
       ("ghc-bootstrap" ,ghc-8.0)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32
             "0s8lf9sxj7n89pjagi58b3fahnp34qvmwhnn0j1fbg6955vbrfj6"))
           (modules '((guix build utils)))
           (snippet
            ;; collections.Iterable was moved to collections.abc in Python 3.10.
            '(substitute* "testsuite/driver/testlib.py"
               (("collections\\.Iterable")
                "collections.abc.Iterable")))))))
    (arguments
     (substitute-keyword-arguments (package-arguments ghc-8.0)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; This phase patches the 'ghc-pkg' command so that it sorts the list
            ;; of packages in the binary cache it generates.
            (add-before 'build 'fix-ghc-pkg-nondeterminism
              (lambda _
                (substitute* "utils/ghc-pkg/Main.hs"
                  (("confs = map \\(path </>\\) \\$ filter \\(\".conf\" `isSuffixOf`\\) fs")
                   "confs = map (path </>) $ filter (\".conf\" `isSuffixOf`) (sort fs)"))))
            (add-after 'unpack-testsuite 'fix-shell-wrappers
              (lambda _
                (substitute* '("driver/ghci/ghc.mk"
                               "utils/mkdirhier/ghc.mk"
                               "rules/shell-wrapper.mk")
                  (("echo '#!/bin/sh'")
                   (format #f "echo '#!~a'" (which "sh"))))))
            ;; This is necessary because the configure system no longer uses
            ;; “AC_PATH_” but “AC_CHECK_”, setting the variables to just the
            ;; plain command names.
            (add-before 'configure 'set-target-programs
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((binutils (assoc-ref inputs "binutils"))
                      (gcc (assoc-ref inputs "gcc"))
                      (ld-wrapper (assoc-ref inputs "ld-wrapper")))
                  (setenv "CC" (string-append gcc "/bin/gcc"))
                  (setenv "CXX" (string-append gcc "/bin/g++"))
                  (setenv "LD" (string-append ld-wrapper "/bin/ld"))
                  (setenv "NM" (string-append binutils "/bin/nm"))
                  (setenv "RANLIB" (string-append binutils "/bin/ranlib"))
                  (setenv "STRIP" (string-append binutils "/bin/strip"))
                  ;; The 'ar' command does not follow the same pattern.
                  (setenv "fp_prog_ar" (string-append binutils "/bin/ar")))))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))))

(define-public ghc-8.6
  (package (inherit ghc-8.4)
    (name "ghc")
    (version "8.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/" name "-" version "-src.tar.xz"))
       (sha256
        (base32 "0qg3zsmbk4rkwkc3jpas3zs74qaxmw4sp4v1mhsbj0a0dzls2jjd"))))
    (native-inputs
     `(;; GHC 8.6.5 must be built with GHC >= 8.2.
       ("ghc-bootstrap" ,ghc-8.4)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (patches (search-patches "ghc-testsuite-dlopen-pie.patch"
                                    "ghc-testsuite-grep-compat.patch"
                                    "ghc-testsuite-recomp015-execstack.patch"))
           (sha256
            (base32
             "0pw9r91g2np3i806g2f4f8z4jfdd7mx226cmdizk4swa7av1qf91"))
           (modules '((guix build utils)))
           (snippet
            ;; collections.Iterable was moved to collections.abc in Python 3.10.
            '(substitute* "testsuite/driver/testlib.py"
               (("collections\\.Iterable")
                "collections.abc.Iterable")))))
       ,@(filter (match-lambda
                   (("ghc-bootstrap" . _) #f)
                   (("ghc-testsuite" . _) #f)
                   (_ #t))
                 (package-native-inputs ghc-8.4))))
    (arguments
     (substitute-keyword-arguments (package-arguments ghc-8.4)
       ((#:make-flags make-flags ''())
        #~(cons "EXTRA_RUNTEST_OPTS=--skip-perf-tests"
                #$make-flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
           (add-after 'install 'remove-unnecessary-references
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* (find-files (string-append (assoc-ref outputs "out") "/lib/")
                                        "settings")
                 (("/gnu/store/.*/bin/(.*)" m program) program))

               ;; Remove references to "doc" output from "out" by rewriting
               ;; the "haddock-interfaces" fields and removing the optional
               ;; "haddock-html" field in the generated .conf files.
               (let ((doc (assoc-ref outputs "doc"))
                     (out (assoc-ref outputs "out")))
                 (with-fluids ((%default-port-encoding #f))
                   (for-each (lambda (config-file)
                               (substitute* config-file
                                 (("^haddock-html: .*") "\n")
                                 (((format #f "^haddock-interfaces: ~a" doc))
                                  (string-append "haddock-interfaces: " out))))
                             (find-files (string-append out "/lib") ".conf")))
                 ;; Move the referenced files to the "out" output.
                 (for-each (lambda (haddock-file)
                             (let* ((subdir (string-drop haddock-file (string-length doc)))
                                    (new    (string-append out subdir)))
                               (mkdir-p (dirname new))
                               (rename-file haddock-file new)))
                           (find-files doc "\\.haddock$")))))
           (add-after 'unpack-testsuite 'skip-tests
             (lambda _
               ;; These two tests refer to the root user, which doesn't exist
               ;; (see <https://bugs.gnu.org/36692>).
               (substitute* "libraries/unix/tests/all.T"
                 (("^test\\('T8108'") "# guix skipped: test('T8108'"))
               (substitute* "libraries/unix/tests/libposix/all.T"
                 (("^test\\('posix010'") "# guix skipped: test('posix010'"))))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))))

(define-public ghc-8.8
  (package (inherit ghc-8.6)
    (name "ghc")
    (version "8.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/ghc-" version "-src.tar.xz"))
       (sha256
        (base32 "0bgwbxxvdn56l91bp9p5d083gzcfdi6z8l8b17qzjpr3n8w5wl7h"))))
    (native-inputs
     `(("ghc-bootstrap" ,ghc-8.6)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/ghc-" version "-testsuite.tar.xz"))
           (patches (search-patches "ghc-testsuite-dlopen-pie.patch"
                                    "ghc-testsuite-grep-compat.patch"
                                    "ghc-testsuite-recomp015-execstack.patch"))
           (sha256
            (base32
             "0c55pj2820q26rikhpf636sn4mjgqsxjrl94vsywrh79dxp3k14z"))
           (modules '((guix build utils)))
           (snippet
            ;; collections.Iterable was moved to collections.abc in Python 3.10.
            '(substitute* "testsuite/driver/testlib.py"
               (("collections\\.Iterable")
                "collections.abc.Iterable")))))
       ("git" ,git-minimal/pinned)                 ; invoked during tests
       ,@(filter (match-lambda
                   (("ghc-bootstrap" . _) #f)
                   (("ghc-testsuite" . _) #f)
                   (_ #t))
                 (package-native-inputs ghc-8.6))))
    (arguments
     (substitute-keyword-arguments (package-arguments ghc-8.6)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
           (add-before 'build 'fix-cc-reference
             (lambda _
               (substitute* "utils/hsc2hs/Common.hs"
                 (("\"cc\"") "\"gcc\""))))
           (add-after 'unpack-testsuite 'skip-more-tests
             (lambda _
               ;; XXX: This test fails because our ld-wrapper script
               ;; mangles the response file passed to the linker.
               (substitute* "testsuite/tests/hp2ps/all.T"
                 (("^test\\('T15904'") "# guix skipped: test('T15904'"))))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))))

(define-public ghc-8.10
  (package
    (inherit ghc-8.8)
    (name "ghc")
    (version "8.10.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/ghc-" version "-src.tar.xz"))
       (sha256
        (base32 "179ws2q0dinl1a39wm9j37xzwm84zfz3c5543vz8v479khigdvp3"))))
    (native-inputs
     `(;; GHC 8.10.7 must be built with GHC >= 8.6.
       ("ghc-bootstrap" ,ghc-8.6)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/ghc-" version "-testsuite.tar.xz"))
           (patches (search-patches "ghc-testsuite-dlopen-pie.patch"
                                    "ghc-testsuite-grep-compat.patch"
                                    "ghc-testsuite-recomp015-execstack.patch"))
           (sha256
            (base32
             "1zl25gg6bpx5601k8h3cqnns1xfc0nqgwnh8jvn2s65ra3f2g1nz"))
           (modules '((guix build utils)))
           (snippet
            ;; collections.Iterable was moved to collections.abc in Python 3.10.
            '(substitute* "testsuite/driver/testlib.py"
               (("collections\\.Iterable")
                "collections.abc.Iterable")))))
       ("git" ,git-minimal/pinned)                 ; invoked during tests
       ,@(filter (match-lambda
                   (("ghc-bootstrap" . _) #f)
                   (("ghc-testsuite" . _) #f)
                   (_ #t))
                 (package-native-inputs ghc-8.8))))
    (arguments
     (substitute-keyword-arguments (package-arguments ghc-8.8)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
           (add-after 'unpack-testsuite 'patch-more-shebangs
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((bash (assoc-ref inputs "bash")))
                 (substitute* '("testsuite/tests/driver/T8602/T8602.script")
                   (("/bin/sh")
                    (string-append bash "/bin/sh"))))))
           ;; Mark failing tests as broken. Reason for failure is unknown.
           (add-after 'skip-more-tests 'skip-even-more-tests
             (lambda _
               (substitute* '("testsuite/tests/driver/T16521/all.T")
                 (("extra_files" all) (string-append "[" all))
                 (("\\]\\), " all)
                  (string-append all "expect_broken(0)], ")))))
           (add-after 'skip-more-tests 'skip-failing-tests-i686
             (lambda _
               (substitute* '("testsuite/tests/codeGen/should_compile/all.T")
                 (("(test\\('T15155l', )when\\(unregisterised\\(\\), skip\\)" all before)
                  (string-append before "when(arch('i386'), skip)")))
               ;; Unexpected failures:
               ;;    quasiquotation/T14028.run  T14028 [bad stderr] (dyn)
               (substitute* '("testsuite/tests/quasiquotation/all.T")
                 (("unless\\(config.have_ext_interp, skip\\),")
                  "unless(config.have_ext_interp, skip), when(arch('i386'), skip),"))))
           ;; i686 fails on CI, but (sometimes and with generous timeouts) completes
           ;; locally. The issue seems to be that the testsuite tries to run some very
           ;; broad regular expressions on output files of several megabytes in size,
           ;; which takes a long time. Since the expressions never match anything on
           ;; our builds anyways, remove them.
           ;; TODO: Merge with 'skip-failing-tests-i686 or move into snippets on
           ;; next rebuild. Note that they are required for GHC 8.10 and 9.2.
           #$@(if (string-prefix? "i686" (or (%current-target-system)
                                             (%current-system)))
               #~((add-after 'skip-failing-tests-i686 'skip-more-failing-tests-i686
                    (lambda _
                      (substitute* '("testsuite/tests/profiling/should_run/all.T")
                        (("test\\('T11627a', \\[ ")
                         "test('T11627a', [ when(arch('i386'), skip), "))
                      (substitute* '("testsuite/driver/testlib.py")
                        ((".*changes being made to the file will invalidate the code signature.*")
                         "")
                        ((".*warning: argument unused during compilation:.*")
                         "")))))
               #~())))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))))

(define-public ghc-8 ghc-8.10)

(define-public ghc-9.0
  (package
    (inherit ghc-8.10)
    (name "ghc")
    (version "9.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.haskell.org/ghc/dist/" version
                                  "/ghc-" version "-src.tar.xz"))
              (sha256
               (base32
                "15wii8can2r3dcl6jjmd50h2jvn7rlmn05zb74d2scj6cfwl43hl"))
              (patches (search-patches "ghc-9-StgCRunAsm-only-when-needed.patch"))))
    (native-inputs
     `(;; GHC 9.0.2 must be built with GHC >= 8.8
       ("ghc-bootstrap" ,ghc-8.10)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                  "https://www.haskell.org/ghc/dist/"
                  version "/ghc-" version "-testsuite.tar.xz"))
           (sha256
            (base32
             "1m5fzhr4gjn9ni8gxx7ag3fkbw1rspjzgv39mnfb0nkm5mw70v3s"))
           (patches (search-patches "ghc-9.2-grep-warnings.patch"
                                    "ghc-testsuite-recomp015-execstack.patch"))
           (modules '((guix build utils)))
           (snippet
            ;; collections.Iterable was moved to collections.abc in Python 3.10.
            '(substitute* "testsuite/driver/testlib.py"
               (("collections\\.Iterable")
                "collections.abc.Iterable")))))
       ,@(filter (match-lambda
                   (("ghc-bootstrap" . _) #f)
                   (("ghc-testsuite" . _) #f)
                   (_ #t))
                 (package-native-inputs ghc-8.10))))
    (native-search-paths
     (list (search-path-specification
            (variable "GHC_PACKAGE_PATH")
            (files (list (string-append "lib/ghc-" version)))
            (file-pattern ".*\\.conf\\.d$")
            (file-type 'directory))))))

(define-public ghc-9.2
  ;; Use 8.10 to shorten the build chain.
  (let ((base ghc-8.10))
    (package
      (inherit base)
      (name "ghc")
      (version "9.2.8")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://www.haskell.org/ghc/dist/" version
                                    "/ghc-" version "-src.tar.xz"))
                (sha256
                 (base32
                  "18b7ln4gx2vy62jpv3z5slv3zfxmxnmkgajznks15zglddwd24sz"))
                (patches (search-patches "ghc-9.2-cabal-support-package-path.patch"
                                         "ghc-9-StgCRunAsm-only-when-needed.patch"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              ;; File Common.hs has been moved to src/ in this release.
              (replace 'fix-cc-reference
                (lambda _
                  (substitute* "utils/hsc2hs/src/Common.hs"
                    (("\"cc\"") "\"gcc\""))))
              (add-after 'skip-more-tests 'skip-T21694-i686
                (lambda _
                  (substitute* '("testsuite/tests/simplCore/should_compile/all.T")
                    (("^test\\('T21694', \\[ " all)
                     (string-append all "when(arch('i386'), skip), ")))))))
         ;; Increase verbosity, so running the test suite does not time out on CI.
         ((#:make-flags make-flags ''())
          #~(cons "VERBOSE=4" #$make-flags))))
      (properties '((max-silent-time . 36000))) ; 10 hours, for i686.
      (native-inputs
       `(;; GHC 9.2 must be built with GHC >= 8.6.
         ("ghc-bootstrap" ,base)
         ("ghc-testsuite"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://www.haskell.org/ghc/dist/"
                   version "/ghc-" version "-testsuite.tar.xz"))
             (sha256
              (base32
               "0cmmwhcwv9fjzvmgjj85d354858qqbmqfzaz5160xqj4yl9zk225"))
             (patches (search-patches "ghc-9.2-grep-warnings.patch"
                                      "ghc-testsuite-recomp015-execstack.patch"))))
         ,@(filter (match-lambda
                     (("ghc-bootstrap" . _) #f)
                     (("ghc-testsuite" . _) #f)
                     (_ #t))
                   (package-native-inputs base))))
      (native-search-paths
       (list (search-path-specification
              (variable "GHC_PACKAGE_PATH")
              (files (list (string-append "lib/ghc-" version)))
              (file-pattern ".*\\.conf\\.d$")
              (file-type 'directory)))))))

;; Versions newer than ghc defined below (i.e. the compiler
;; haskell-build-system uses) should use ghc-next as their name to
;; ensure ghc (without version specification) and ghc-* packages are
;; always compatible. See https://issues.guix.gnu.org/issue/47335.
(define-public ghc ghc-9.2)

;; 9.4 is the last version to support the make-based build system,
;; but it boot with 9.2, only 9.0 is supported.
(define ghc-bootstrap-for-9.4 ghc-9.0)

;; We need two extra dependencies built with ghc-bootstrap-for-9.4,
;; which are duplicated here from haskell-xyz to make sure the
;; bootstraping process always works.
(define ghc-alex-bootstrap-for-9.4
  (hidden-package
    (package
     (name "ghc-alex")
     (version "3.2.6")
     (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "alex" version))
        (sha256
         (base32
          "042lrkn0dbpjn5ivj6j26jzb1fwrj8c1aj18ykxja89isg0hiali"))))
     (build-system haskell-build-system)
     (arguments
       (list #:tests? #f
             #:haskell ghc-bootstrap-for-9.4))
     (native-inputs
      (list which))
     (home-page "https://www.haskell.org/alex/")
     (synopsis
      "Tool for generating lexical analysers in Haskell")
     (description
      "Alex is a tool for generating lexical analysers in Haskell.  It takes a
 description of tokens based on regular expressions and generates a Haskell
 module containing code for scanning text efficiently.  It is similar to the
 tool lex or flex for C/C++.")
     (license license:bsd-3))))

(define ghc-happy-bootstrap-for-9.4
  (hidden-package
    (package
     (name "ghc-happy")
     (version "1.20.0")
     (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "happy" version))
        (sha256
         (base32
          "1346r2x5ravs5fqma65bzjragqbb2g6v41wz9maknwm2jf7kl79v"))))
     (build-system haskell-build-system)
     (arguments
       (list #:haskell ghc-bootstrap-for-9.4
             #:tests? #f))
     (home-page "https://hackage.haskell.org/package/happy")
     (synopsis "Parser generator for Haskell")
     (description "Happy is a parser generator for Haskell.  Given a grammar
 specification in BNF, Happy generates Haskell code to parse the grammar.
 Happy works in a similar way to the yacc tool for C.")
     (license license:bsd-3))))

(define-public ghc-9.4
  ;; Inherit from 9.2, which added a few fixes, but boot from 9.0 (see above).
  (let ((base ghc-9.2))
    (package
      (inherit base)
      (name "ghc-next")
      (version "9.4.4")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://www.haskell.org/ghc/dist/" version
                                    "/ghc-" version "-src.tar.xz"))
                (sha256
                 (base32
                  "1qk7rlqf02s3b6m6sqqngmjq1mxnrz88h159lz6k25gddmdg5kp8"))
                (patches (search-patches "ghc-9-StgCRunAsm-only-when-needed.patch"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
             ;; Files don’t exist any more.
             (delete 'skip-tests)))))
      (native-inputs
       `(;; GHC 9.4 must be built with GHC >= 9.0.
         ("ghc-bootstrap" ,ghc-bootstrap-for-9.4)
         ("ghc-testsuite"
          ,(origin
             (method url-fetch)
             (uri (string-append
                    "https://www.haskell.org/ghc/dist/"
                    version "/ghc-" version "-testsuite.tar.xz"))
             (sha256
              (base32
               "04p2lawxxg3nyv6frzhyjyh3arhqqyh5ka3alxa2pxhcd2hdcja3"))
             (patches (search-patches "ghc-testsuite-recomp015-execstack.patch"))))
         ("ghc-alex" ,ghc-alex-bootstrap-for-9.4)
         ("ghc-happy" ,ghc-happy-bootstrap-for-9.4)
         ,@(filter (match-lambda
                     (("ghc-bootstrap" . _) #f)
                     (("ghc-testsuite" . _) #f)
                     (_ #t))
                   (package-native-inputs base))))
      (native-search-paths
       (list (search-path-specification
              (variable "GHC_PACKAGE_PATH")
              (files (list (string-append "lib/ghc-" version)))
              (file-pattern ".*\\.conf\\.d$")
              (file-type 'directory)))))))

;;; haskell.scm ends here
