;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015, 2018 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2017 Frederick Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2019, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Esther Flashner <esther@flashner.co.il>
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

(define-module (gnu packages dlang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:hide (delete which))
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public d-tools
  (package
    (name "d-tools")
    (version "2.100.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlang/tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jbn0hyskv4ykcckw0iganpyrm0bq2lggswspw21r4hgnxkmjbyw"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (replace 'build
                 (lambda _
                   (mkdir-p "bin")
                   (setenv "CC" #$(cc-for-target))
                   (setenv "LD" #$(ld-for-target))
                   (invoke "ldc2" "rdmd.d" "--of" "bin/rdmd")
                   (apply invoke "ldc2" "--of=bin/dustmite"
                          (find-files "DustMite" ".*\\.d"))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "bin/rdmd" "rdmd_test.d" "bin/rdmd"
                             "--rdmd-default-compiler" "ldmd2"))))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin"))
                          (man (string-append out "/man")))
                     (for-each delete-file (find-files "bin" "\\.o$"))
                     (copy-recursively "bin" bin)
                     (copy-recursively "man" man)))))))
    (native-inputs
     (list ldc
           (module-ref (resolve-interface
                        '(gnu packages commencement))
                       'ld-gold-wrapper)))
    (home-page "https://github.com/dlang/tools")
    (synopsis "Useful D-related tools")
    (description
     "@code{d-tools} provides two useful tools for the D language: @code{rdmd},
which runs D source files as scripts, and @code{dustmite}, which reduces D code
to a minimal test case.")
    (license license:boost1.0)))

(define-public gdmd
  (let ((commit "ff2c97a47408fb71c18a2d453294d18808a97cc5")
        (revision "1"))
    (package
      (name "gdmd")
      (version (git-version "0.1.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/D-Programming-GDC/gdmd")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0pd70clk70069xcjysaas7zszzmigrcw1zl2xxv8kzdg7y7xrzvm"))))
      (build-system copy-build-system)
      (arguments
       (list
         #:install-plan
         #~'(("dmd-script" "bin/gdmd")
             ("dmd-script.1" "share/man/man1/gdmd.1"))
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'adjust-gdc-location
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "dmd-script"
                   (("my \\$gdc_dir.*")
                    (string-append "my $gdc_dir = \""
                                   (dirname (search-input-file inputs "/bin/gdc"))
                                   "\";\n"))))))))
      (inputs
       (list gdc-10 perl))
      (home-page "https://github.com/D-Programming-GDC/gdmd")
      (synopsis "DMD-like wrapper for GDC")
      (description "This package provides a DMD-like wrapper for the
@acronym{GNU D Compiler,GDC}.")
      (license license:gpl3+))))

;; We use GDC, the D frontend for GCC, to bootstrap ldc.  We then use
;; ldc to bootstrap itself so that no reference remains to GDC.
(define ldc-bootstrap
  (package
    (name "ldc")
    (version "1.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ldc-developers/ldc/releases"
                           "/download/v" version "/ldc-" version "-src.tar.gz"))
       (sha256
        (base32 "1775001ba6n8w46ln530kb5r66vs935ingnppgddq8wqnc0gbj4k"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                  ;skip in the bootstrap
       #:build-type "Release"
       #:configure-flags
        (list "-GNinja")
       #:make-flags                 ;used as build targets
        (list "all")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "runtime/phobos/std/process.d"
               (("/bin/sh") (which "sh"))
               (("echo") (which "echo")))))
         (replace 'build
           ;; Building with Make would result in "make: *** [Makefile:166:
           ;; all] Error 2".
           (lambda* (#:key make-flags parallel-tests? #:allow-other-keys)
             (let ((job-count (number->string (or (and parallel-tests?
                                                       (parallel-job-count))
                                                  1))))
               (apply invoke "cmake" "--build" "." "-j" job-count
                      "--target" make-flags))))
         (replace 'install
           (lambda _
             (invoke "cmake" "--install" "."))))))
    (inputs
     `(("libconfig" ,libconfig)
       ("libedit" ,libedit)
       ("tzdata" ,tzdata)
       ("zlib" ,zlib)))
    (native-inputs
     ;; Importing (gnu packages commencement) would introduce a cycle.
     `(("ld-gold-wrapper" ,(module-ref (resolve-interface
                                        '(gnu packages commencement))
                                       'ld-gold-wrapper))
       ("llvm" ,llvm-11)
       ("ldc" ,gdmd)
       ("ninja" ,ninja)
       ("python-wrapper" ,python-wrapper)
       ("unzip" ,unzip)))
    (home-page "http://wiki.dlang.org/LDC")
    (synopsis "LLVM-based compiler for the D programming language")
    (description
     "LDC is an LLVM compiler for the D programming language.  It is based on
the latest DMD compiler that was written in C and is used for
bootstrapping more recent compilers written in D.")
    (properties
     ;; Some of the tests take a very long time on ARMv7.  See
     ;; <https://lists.gnu.org/archive/html/guix-devel/2018-02/msg00312.html>.
     `((max-silent-time . ,(* 3600 3))))
    ;; Most of the code is released under BSD-3, except for code originally
    ;; written for GDC, which is released under GPLv2+, and the DMD frontend,
    ;; which is released under the "Boost Software License version 1.0".
    (license (list license:bsd-3
                   license:gpl2+
                   license:boost1.0))))

(define-public ldc
  (package
    (inherit ldc-bootstrap)
    (arguments
     (substitute-keyword-arguments (package-arguments ldc-bootstrap)
       ((#:make-flags _ #f)
        '(list "all"
               ;; Also build the test runner binaries.
               "ldc2-unittest" "all-test-runners"))
       ((#:configure-flags flags)
        `(,@flags "-DBUILD_SHARED_LIBS=ON"
                  "-DLDC_LINK_MANUALLY=OFF"
                  "-DLDC_DYNAMIC_COMPILE=OFF"))
       ((#:tests? _) #t)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-compiler-rt-library-discovery
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((clang-runtime (assoc-ref inputs "clang-runtime"))
                     (system ,(or (%current-target-system)
                                  (%current-system))))
                 (define (gnu-triplet->clang-arch system)
                   (letrec-syntax
                       ((matches (syntax-rules (=>)
                                   ((_ (system-prefix => target) rest ...)
                                    (if (string-prefix? system-prefix system)
                                        target
                                        (matches rest ...)))
                                   ((_)
                                    (error "Clang target for system is unknown"
                                           system)))))
                     (matches ("x86_64"      => "x86_64")
                              ("i686"        => "i386")
                              ("armhf"       => "armhf")
                              ("aarch64"     => "aarch64"))))
                 ;; Coax LLVM into agreeing with Clang about system target
                 ;; naming.
                 (substitute* "driver/linker-gcc.cpp"
                   (("triple.getArchName\\(\\)")
                    (format #f "~s" (gnu-triplet->clang-arch system))))
                 ;; Augment the configuration of the ldc2 binaries so they can
                 ;; find the compiler-rt libraries they need to be linked with
                 ;; for the tests.
                 (substitute* (find-files "." "^ldc2.*\\.conf\\.in$")
                   ((".*lib-dirs = \\[\n" all)
                    (string-append all
                                   "        \"" clang-runtime
                                   "/lib/linux\",\n"))))))
           (add-after 'unpack 'patch-paths-in-tests
             (lambda _
               (substitute* "tests/d2/dmd-testsuite/Makefile"
                 (("/bin/bash") (which "bash")))
               (substitute* "tests/linking/linker_switches.d"
                 (("echo") (which "echo")))
               (substitute* "tests/d2/dmd-testsuite/dshell/test6952.d"
                 (("/usr/bin/env bash")
                  (which "bash")))))
           (add-after 'unpack 'disable-problematic-tests
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Disable unittests in the following files.
               (substitute* '("runtime/phobos/std/net/curl.d"
                              "runtime/phobos/std/datetime/systime.d"
                              "runtime/phobos/std/datetime/timezone.d")
                 (("version(unittest)") "version(skipunittest)")
                 ((" unittest") " version(skipunittest) unittest"))
               ;; The following tests plugins we don't have.
               (delete-file "tests/plugins/addFuncEntryCall/testPlugin.d")
               ;; The following tests requires AVX instruction set in the CPU.
               (substitute* "tests/d2/dmd-testsuite/runnable/cdvecfill.sh"
                 (("^// DISABLED: ") "^// DISABLED: linux64 "))
               ;; This unit test requires networking, fails with
               ;; "core.exception.RangeError@std/socket.d(778): Range
               ;; violation".
               (substitute* "runtime/phobos/std/socket.d"
                 (("assert\\(ih.addrList\\[0\\] == 0x7F_00_00_01\\);.*")
                  ""))
               ;; The GDB tests suite fails; there are a few bug reports about
               ;; it upstream.
               (for-each delete-file (find-files "tests" "gdb.*\\.(d|sh)$"))
               (delete-file "tests/d2/dmd-testsuite/runnable/debug_info.d")
               (delete-file "tests/d2/dmd-testsuite/runnable/b18504.d")
               (substitute* "runtime/druntime/test/exceptions/Makefile"
                 ((".*TESTS\\+=rt_trap_exceptions_drt_gdb.*")
                  ""))
               ;; The following tests fail on some systems, not all of
               ;; which are tested upstream.
               (with-directory-excursion "tests"
                 (cond
                   (,(or (target-x86-32?)
                         (target-arm32?))
                     (for-each delete-file
                               '("PGO/profile_rt_calls.d"
                                 "codegen/mangling.d"
                                 "instrument/xray_check_pipeline.d"
                                 "instrument/xray_link.d"
                                 "instrument/xray_simple_execution.d"
                                 "sanitizers/msan_noerror.d"
                                 "sanitizers/msan_uninitialized.d"
                                 "d2/dmd-testsuite/runnable_cxx/cppa.d")))
                   (,(target-aarch64?)
                     (for-each delete-file
                               '("d2/dmd-testsuite/runnable/ldc_cabi1.d"
                                 "sanitizers/fuzz_basic.d"
                                 "sanitizers/msan_noerror.d"
                                 "sanitizers/msan_uninitialized.d")))
                   (#t '())))))
           (add-before 'configure 'set-cc-and-cxx-to-use-clang
             ;; The tests require to be built with Clang; build everything
             ;; with it, for simplicity.
             (lambda _
               (setenv "CC" (which "clang"))
               (setenv "CXX" (which "clang++"))))
           (replace 'check
             (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
               (when tests?
                 (let ((job-count (number->string
                                   (or (and parallel-tests?
                                            (parallel-job-count))
                                       1))))
                   ;; The test targets are tested separately to provide
                   ;; finer-grained diagnostics (see:
                   ;; https://raw.githubusercontent.com/ldc-developers/
                   ;; ldc/master/.azure-pipelines/3-posix-test.yml)
                   (display "running the ldc2 unit tests...\n")
                   (invoke "ctest" "--output-on-failure" "-j" job-count
                           "-R" "ldc2-unittest")
                   (display "running the lit test suite...\n")
                   (invoke "ctest" "--output-on-failure" "-j" job-count
                           "-R" "lit-tests")
                   (display "running the dmd test suite...\n")
                   (invoke "ctest" "--output-on-failure" "-j" job-count
                           "-R" "dmd-testsuite")
                   (display "running the defaultlib unit tests and druntime \
integration tests...\n")
                   (invoke "ctest" "--output-on-failure" "-j" job-count
                           "-E" "dmd-testsuite|lit-tests|ldc2-unittest")))))))))
    (native-inputs
     (append (delete "llvm"
                     (alist-replace "ldc" (list ldc-bootstrap)
                                    (package-native-inputs ldc-bootstrap)))
         `(("clang" ,clang-11)          ;propagates llvm and clang-runtime
           ("python-lit" ,python-lit))))))

(define-public dub
  (package
    (name "dub")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlang/dub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06a4whsl1m600k096nwif83n7za3vr7pj1xwapncy5fcad1gmady"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; tests try to install packages
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ; no configure script
               (replace 'build
                 (lambda _
                   (setenv "CC" #$(cc-for-target))
                   (setenv "LD" #$(ld-for-target))
                   (invoke "./build.d")))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin")))
                     (install-file "bin/dub" bin)))))))
    (inputs
     (list curl))
    (native-inputs
     (list d-tools
           ldc
           (module-ref (resolve-interface
                        '(gnu packages commencement))
                       'ld-gold-wrapper)))
    (home-page "https://code.dlang.org/getting_started")
    (synopsis "Package and build manager for D projects")
    (description
     "DUB is a package and build manager for applications and
libraries written in the D programming language.  It can
automatically retrieve a project's dependencies and integrate
them in the build process.

The design emphasis is on maximum simplicity for simple projects,
while providing the opportunity to customize things when
needed.")
    (license license:expat)))

(define-public gtkd
  (package
    (name "gtkd")
    (version "3.9.0")
    (source
     (origin
      (method url-fetch/zipbomb)
      (uri (string-append "https://gtkd.org/Downloads/sources/GtkD-"
                          version ".zip"))
      (sha256
       (base32 "0qv8qlpwwb1d078pnrf0a59vpbkziyf53cf9p6m8ms542wbcxllp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("unzip" ,unzip)
       ("ldc" ,ldc)
       ("pkg-config" ,pkg-config)
       ("xorg-server-for-tests" ,xorg-server-for-tests)))
    (arguments
     `(#:test-target "test"
       #:make-flags
       `("DC=ldc2"
         ,(string-append "prefix=" (assoc-ref %outputs "out"))
         ,(string-append "libdir=" (assoc-ref %outputs "out")
                         "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-makefile
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "GNUmakefile"
               ;; We do the tests ourselves.
               (("default-goal: libs test") "default-goal: libs")
               (("all: libs shared-libs test") "all: libs shared-libs")
               ;; Work around upstream bug.
               (("\\$\\(prefix\\)\\/\\$\\(libdir\\)") "$(libdir)"))
             #t))
         (add-before 'check 'prepare-x
           (lambda _
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (home-page "https://gtkd.org/")
    (synopsis "D binding and OO wrapper of GTK+")
    (description "This package provides bindings to GTK+ for D.")
    (license license:lgpl2.1)))
