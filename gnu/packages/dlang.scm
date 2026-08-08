;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015, 2018 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2017 Frederick Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2019, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2021-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021, 2024 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Esther Flashner <esther@flashner.co.il>
;;; Copyright © 2025-2026 Jonas Meeuws <jonas.meeuws@gmail.com>
;;; Copyright © 2026 Daniel Littlewood <dan@danielittlewood.xyz>
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
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))


;; Compilers and tooling for the D programming language.
;; Note: The GNU D compiler is defined in (gnu packages gcc) instead.

;; Removes references to druntime and standard library source paths that may be
;; printed in backtraces.
(define-public (remove-d-include-references-phase compiler)
  #~(lambda* (#:key outputs #:allow-other-keys)
      (for-each
       (lambda (file)
         ;; Set the last character of the hash to *.
         (let* ((compiler #$compiler)
                (char-offset (+ (string-length (%store-directory))
                                (string-length "/")
                                %store-hash-string-length
                                -1))
                (patched (string-append
                          (string-take compiler char-offset)
                          "*"
                          (string-drop compiler (+ char-offset 1))))
                (sources-regex (string-append
                                "[[:alnum:]/_.\\-]*"
                                "/include/"
                                "[[:alnum:]/_.\\-]*"
                                "\\.d"))
                (command (list "sed" "-i"
                               (format #f "s,~a\\(~a\\),~a\\1,g"
                                       compiler sources-regex patched)
                               file)))
           ;; XXX: Use sed, as substitute* fails on binary files.
           (apply invoke command)))
       (apply append (map find-files
                          (map cdr outputs))))))


;; LLVM-based D compiler

(define-public ldc-bootstrap
  (package
    ;; This package is purposefully named just "ldc" and not "ldc-bootstrap",
    ;; as the final ldc package rewrites references from this one to itself,
    ;; and their names must have the same length to avoid corrupting the
    ;; binary.
    (name "ldc")
    (version "1.42.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ldc-developers/ldc")
                     (commit (string-append "v" version))
                     ;; Note: ldc-developers/phobos checked out in
                     ;; runtime/phobos.
                     ;; TODO: capture in own variable.
                     (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zrfdhm9yw9bsd55k5x89pyj3zyxyly3hs3mpj3bnvkzvki3bl39"))
              (patches (search-patches "ldc-i686-int128-alignment.patch"
                                       "ldc-phobos-support-TZDIR.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:disallowed-references (list tzdata-for-tests)
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-1))
      #:generator "Ninja"
      #:configure-flags
      #~(list "-DD_COMPILER_FLAGS=-fPIC"
              "-DBUILD_SHARED_LIBS=OFF" ; see .github/actions/2-build-bootstrap
              "-DLDC_DYNAMIC_COMPILE=OFF" ; likewise
              (format #f "-DCOMPILER_RT_LIBDIR_CONFIG=~a/lib/linux"
                      #$(this-package-input "clang-runtime"))
              (format #f "-DCMAKE_INSTALL_RPATH=~a/lib"
                      (assoc-ref %outputs "lib"))
              #$@(if (target-riscv64?)
                     #~(("-DCMAKE_EXE_LINKER_FLAGS=-latomic"))
                     #~())
              (format #f "-DINCLUDE_INSTALL_DIR=~a/include/d/ldc"
                      (assoc-ref %outputs "out")))
      #:build-type "Debug"
      #:make-flags #~(list "all")       ; used as build targets
      #:tests? #f                       ; skip in the bootstrap
      #:phases
      (let* ((target-file
              (lambda (pkg path)
                (file-append (this-package-input pkg) path)))
             (native-file
              (lambda (pkg path)
                (file-append (this-package-native-input pkg) path)))
             (target-bin-sh (target-file "bash-minimal" "/bin/sh"))
             (target-bin-clang (target-file "clang" "/bin/clang"))
             (target-clang-runtime (target-file "clang-runtime" ""))
             (target-lib-curl (target-file "curl" "/lib/libcurl.so"))
             (native-bin-clang (native-file "clang" "/bin/clang"))
             (native-bin-clang++ (native-file "clang" "/bin/clang++")))
        #~(modify-phases %standard-phases
            ;; LDC needs a C compiler as a linker wrapper.
            ;; Change the default fallback "cc" to clang.
            ;; Discovery implemented in ldc v1.19.0.
            (add-after 'patch-usr-bin-file 'patch-default-cc
              (lambda _
                (substitute* "driver/tool.h"
                  (("\"cc\"")
                   (format #f "~s" #$target-bin-clang)))))
            (add-after 'unpack 'patch-compiler-rt-library-discovery
              ;; See also the -DCOMPILER_RT_LIBDIR_CONFIG configure flag.
              (lambda _
                (let* ((system #$(or (%current-target-system)
                                     (%current-system)))
                       (arch (car (string-split system #\-)))
                       (clang-arch (cond
                                    ((string-suffix? "86" arch) "i386")
                                    (#t arch))))
                  ;; Coax LLVM into agreeing with Clang about system target
                  ;; naming.
                  (substitute* "driver/linker-gcc.cpp"
                    (("triple.getArchName\\(\\)")
                     (format #f "~s" clang-arch))))))
            ;; Using ImportC will always emit warnings when using gcc 14+
            ;; as its preprocessor, causing tests that read stderr to
            ;; fail.
            ;; Introduced in ldc v1.29.0.
            ;; Fixed (like this) in ldc v1.40.0.
            (add-after 'unpack 'patch-importc-system-header
              (lambda _
                (substitute* "runtime/druntime/src/importc.h"
                  (("^#define __IMPORTC__ 1.*$" all)
                   (string-append
                    all
                    "\n"
                    "#ifdef __GNUC__\n"
                    "#pragma GCC system_header\n"
                    "#endif\n")))))
            ;; Using ImportC with clang as preprocessor will cause
            ;; ImportC to fail on glibc float headers.
            ;; Introduced in ldc v1.33.0.
            (add-after 'unpack 'patch-importc-float128
              (lambda _
                (substitute* "runtime/druntime/src/importc.h"
                  (("^#ifndef __clang__.*$")
                   (string-append
                    "#ifdef __clang__\n"
                    "#define __float128 long double\n"
                    "#else\n")))))
            (add-after 'unpack 'patch-paths-in-phobos
              (lambda _
                (with-directory-excursion "runtime/phobos"
                  (substitute* "std/net/curl.d"
                    (("\"libcurl\\.so\"")
                     (format #f "~s" #$target-lib-curl)))
                  (substitute* "std/process.d"
                    (("return \"/bin/sh\";")
                     (format #f "return ~s;" #$target-bin-sh))
                    (("#!/bin/sh")
                     (string-append "#!" #$target-bin-sh))))))
            (add-after 'unpack 'patch-getInstalledTZNames-infinite-symlink
              (lambda _
                ;; Disable following directory symlinks when iterating tzdata.
                (substitute* "runtime/phobos/std/datetime/timezone.d"
                  (("SpanMode\\.depth\\)") "SpanMode.depth, false)"))))
            (add-after 'unpack 'patch-tests
              (lambda _
                ;; Fails often. Relies on guessing the test binary size,
                ;; sleeps, and file timestamps.
                ;; Introduced in ldc v1.1.0.
                (delete-file "tests/linking/ir2obj_cache_pruning2.d")
                ;; Very unreliable.
                ;; Introduced in ldc v1.4.0.
                (delete-file "tests/sanitizers/fuzz_asan.d")
                ;; These 2 tests try to build a Makefile on their own.
                ;; Introduced in ldc v1.8.0.
                (delete-file-recursively "tests/plugins")
                ;; This test doesn't expect the linker to demangle D symbols.
                ;; Introduced in ldc v1.8.1.
                (substitute* "tests/dmd/fail_compilation/needspkgmod.d"
                  (("_D7imports9pkgmod3133mod3barFZv")
                   "imports.pkgmod313.mod.bar()"))
                ;; These CTFE tests fail on riscv64-linux.
                ;; Test for signbit introduced in ldc v1.19.0.
                ;; Test for getNaNPayload introduced in ldc v1.25.0.
                ;; std.math was split into modules in ldc v1.27.0.
                #$@(if (target-riscv64?)
                       #~((substitute* "runtime/phobos/std/math/operations.d"
                            (("static assert\\(getNaNPayload\\(a\\)" line)
                             (string-append "//" line)))
                          (substitute* "runtime/phobos/std/math/traits.d"
                            (("static assert\\(signbit\\(-.*\\.nan" line)
                             (string-append "//" line))))
                       #~())
                ;; This test creates a shell script and runs it.
                ;; Introduced in ldc v1.22.0.
                (substitute* "tests/dmd/dshell/test6952.d"
                  (("/usr/bin/env bash") #$target-bin-sh))
                ;; Fails to detect the race condition for some reason.
                ;; Introduced in ldc v1.23.0.
                (for-each delete-file
                          '("tests/sanitizers/tsan_tiny_race.d"
                            "tests/sanitizers/tsan_tiny_race_TLS.d"))
                ;; Likewise.  Introduced in ldc v1.27.0.
                (for-each delete-file
                          '("tests/sanitizers/msan_noerror.d"
                            "tests/sanitizers/msan_uninitialized.d"))
                ;; Likewise.  Introduced in ldc v1.30.0.
                (for-each delete-file
                          '("tests/sanitizers/lsan_memleak.d"))
                ;; Patch a shell path in the druntime profile test Makefile.
                ;; Introduced in ldc v1.34.0.
                (substitute* "runtime/druntime/test/profile/Makefile"
                  (("SHELL=/bin/bash")
                   (string-append "SHELL=" #$target-bin-sh)))
                ;; Since the implementation of SOURCE_DATE_EPOCH support in
                ;; Ddoc, this test fails, as it expects Ddoc timestamps to
                ;; match the output of the `date` command.
                ;; Introduced in ldc v1.36.0.
                (substitute*
                    "tests/dmd/compilable/extra-files/ddocYear-postscript.sh"
                  (("^YEAR=.*$") "YEAR=1970\n"))
                ;; This tests how the CC env var is handled by the compiler,
                ;; by setting it to cc, which we don't have.
                ;; Introduced in ldc v1.37.0.
                (substitute* "tests/driver/cli_CC_envvar.d"
                  (("\\bcc\\b") #$native-bin-clang))
                ;; One of these tests hangs when a modern llvm opt is applied.
                ;; Fix by only running debug builds.
                ;; Introduced in ldc v1.41.0.
                (substitute* "runtime/druntime/test/exceptions/Makefile"
                  (("TESTS\\+=memoryerror.*$" all)
                   (string-append "ifeq ($(BUILD),debug)\n" all "endif\n")))
                ;; The following tests fail on some systems, not all of
                ;; which are tested upstream.
                (for-each
                 (lambda (path) (false-if-file-not-found (delete-file path)))
                 (list
                  #$@(if (or (target-x86-32?)
                             (target-arm32?))
                         #~("tests/codegen/mangling.d"
                            "tests/dmd/runnable_cxx/cppa.d"
                            "tests/instrument/xray_check_pipeline.d"
                            "tests/instrument/xray_link.d"
                            "tests/instrument/xray_simple_execution.d"
                            "tests/PGO/profile_rt_calls.d"
                            "tests/sanitizers/msan_noerror.d"
                            "tests/sanitizers/msan_uninitialized.d")
                         #~())
                  #$@(if (target-riscv64?)
                         #~("tests/dmd/codegen/simd_alignment.d"
                            "tests/dmd/compilable/test23705.d"
                            "tests/dmd/fail_compilation/diag7420.d"
                            "tests/dmd/runnable/argufilem.d"
                            "tests/dmd/runnable_cxx/cppa.d")
                         #~())))))
            ;; The tests require to be built with Clang; build everything
            ;; with it, for simplicity.
            (add-before 'configure 'set-cc
              (lambda _
                (setenv "CC" #$native-bin-clang)
                (setenv "CXX" #$native-bin-clang++)))
            ;; The test targets are tested separately to provide
            ;; finer-grained diagnostics (see the `.github/actions/4*`
            ;; files in the source).
            (replace 'check
              (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
                (define* (run-tests name includes excludes
                                    #:key
                                    (job-count (if parallel-tests?
                                                   (parallel-job-count)
                                                   1)))
                  (define (regex-flags prefix patterns)
                    (if (> (length patterns) 0)
                        (list prefix
                              (format #f "(~a)" (string-join patterns "|")))
                        '()))
                  (format #t "running the ~a...\n" name)
                  (apply invoke
                         `("ctest"
                           "--output-on-failure"
                           "-j" ,(number->string job-count)
                           ,@(regex-flags "-R" includes)
                           ,@(regex-flags "-E" excludes))))
                (when tests?
                  (run-tests "ldc2 unit tests"
                             (list "ldc2-unittest")
                             (list))
                  (run-tests "lit test suite"
                             (list "lit-tests")
                             (list))
                  ;; This test has a race condition so run it with 1 core.
                  (run-tests "dmd test suite"
                             (list "dmd-testsuite")
                             (list)
                             #:job-count 1)
                  (run-tests "druntime unit tests"
                             (list "druntime-test-runner"
                                   "^core\\."
                                   "^etc\\.linux" "etc\\.valgrind"
                                   "^ldc\\."
                                   "^object"
                                   "^rt\\.")
                             (list #$@(if (target-riscv64?)
                                          ;; These hang forever
                                          #~("core.thread.fiber-.*shared"
                                             "core.thread.osthread-.*shared")
                                          #~())))
                  (run-tests "druntime integration tests"
                             (list "druntime-test")
                             (list "druntime-test-runner"
                                   #$@(if (target-aarch64?)
                                          #~("druntime-test-exceptions-debug")
                                          #~())))
                  ;; Building these tests is very resource intensive, so
                  ;; limit the job count.
                  (run-tests
                   "phobos unit tests"
                   (list "phobos"
                         "etc\\.c\\."
                         "^std")
                   (list #$@(if (target-aarch64?)
                                #~("std.internal.math.gammafunction-.*shared"
                                   "std.math.exponential-shared")
                                #~())
                         #$@(if (target-riscv64?)
                                #~("std.internal.math.errorfunction-.*shared"
                                   "std.internal.math.gammafunction-.*shared"
                                   "std.math.exponential-.*shared"
                                   "std.math.operations-debug-shared"
                                   "std.math.traits-debug-shared"
                                   "std.math.trigonometry-.*shared"
                                   "std.mathspecial-.*shared"
                                   "std.socket-debug-shared"
                                   "std.socket-shared")
                                #~()))
                   #:job-count 1))))
            (add-after 'install 'create-lib-output
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (out/etc (string-append out "/etc"))
                       (out/lib (string-append out "/lib"))
                       (lib (assoc-ref outputs "lib"))
                       (lib/lib (string-append lib "/lib"))
                       (libs (find-files out/lib "\\.so")))
                  (mkdir-p lib/lib)
                  (for-each (lambda (original)
                              (install-file original lib/lib)
                              (delete-file original))
                            libs)
                  ;; Append to default lib-dirs and override rpath.
                  (with-output-to-file
                      (string-append out/etc "/ldc2.conf/80-guix.conf")
                    (lambda ()
                      (format #t "\"default\":~%")
                      (format #t "{~%")
                      (format #t "    lib-dirs ~~= [\"~a\"];~%" lib/lib)
                      (format #t "    rpath = \"~a\";~%" lib/lib)
                      (format #t "};~%"))))))))))
    (inputs
     (list clang-runtime-21
           libconfig
           llvm-21
           zlib
           clang                        ; used as a linker wrapper
           curl                         ; std.net.curl
           bash-minimal))               ; std.process
    (native-inputs
     (list gdmd
           clang                        ; propagates llvm and clang-runtime
           lld-as-ld-wrapper
           ;; For testing
           tzdata-for-tests             ; std.datetime.timezone
           gdb
           python-wrapper
           python-setuptools
           python-lit))
    (outputs '("out" "lib" "debug"))
    (home-page "http://wiki.dlang.org/LDC")
    (synopsis "LLVM-based compiler for the D programming language")
    (description "The LDC project provides a portable D programming language
compiler with modern optimization and code generation capabilities.  The
compiler uses the official DMD frontend to support the latest version of D2,
and relies on the LLVM Core libraries for code generation.

This compiler is based on the DMD frontend version 2.112.1.")
    ;; Most of the code is released under BSD-3, except for code originally
    ;; written for GDC, which is released under GPLv2+, and the DMD frontend
    ;; and the druntime and phobos libraries which are released under the
    ;; "Boost Software License version 1.0".
    (license (list license:bsd-3
                   license:gpl2+
                   license:boost1.0))
    (properties
     `((hidden? . #t)
       ;; Some of the tests take a very long time on ARMv7.  See
       ;; https://lists.gnu.org/archive/html/guix-devel/2018-02/msg00312.html.
       ,@(if (target-arm32?) `((max-silent-time . ,(* 3600 3))) '())))))

(define-public ldc
  (let ((base ldc-bootstrap))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments
           (strip-keyword-arguments
            '(#:tests?)                 ; reinstate tests
            (package-arguments base))
         ((#:disallowed-references _ ''())
          (list (lookup-package-native-input base "gdmd")
                tzdata-for-tests))
         ((#:configure-flags flags #~'())
          #~(append
             (fold delete #$flags '("-DD_COMPILER_FLAGS=-fPIC"
                                    "-DBUILD_SHARED_LIBS=OFF"))
             '("-DBUILD_SHARED_LIBS=ON")))
         ((#:build-type _ ''())
          "RelWithDebInfo")
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (add-after 'create-lib-output 'rewrite-references-to-bootstrap
                ;; D compilers can keep references to the include files used to
                ;; build a binary in exception messages. For ldc, rewrite the
                ;; references to ldc-bootstrap to itself, to reduce its closure
                ;; size.
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((in-ldc-bootstrap #$(this-package-native-input "ldc"))
                         (out (assoc-ref outputs "out"))
                         (out/bin (string-append out "/bin")))
                    ;; XXX: Use sed, as replace-store-references wouldn't
                    ;; replace the references, while substitute* throws an
                    ;; error.
                    (apply invoke "sed" "-i"
                           (format #f "s,~a,~a,g" in-ldc-bootstrap out)
                           (find-files out/bin)))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (delete "gdmd")
         (append base)))
      (properties
       (alist-delete 'hidden? (package-properties base))))))


;; Reference D compiler
;; Note: Has limited supported-systems.

;; DMD built with GDC as the bootstrap D compiler (via the gdmd wrapper).
;; Shared libraries are not built, tests are disabled.
(define-public dmd-bootstrap
  (package
    ;; This package is purposefully named just "dmd" and not "dmd-bootstrap",
    ;; as the final dmd package rewrites references from this one to itself,
    ;; and their names must have the same length to avoid corrupting the
    ;; binary.
    (name "dmd")
    (version "2.112.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dlang/dmd")
                     (commit (string-append "v" version))))
              (file-name (git-file-name "dmd" version))
              ;; Note: When updating, also update the hash of phobos in the
              ;; native-inputs below.
              (sha256
               (base32
                "0qvg2fb73kyng8k1wj482g07ar2qw5laa5fynwx7pdd610n0pjpc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:disallowed-references (list (gexp-input (canonical-package gcc) "lib")
                                    tzdata-for-tests)
      ;; Disable tests, as gdmd cannot cope with some arguments used such as
      ;; '-conf'.
      #:tests? #f
      #:out-of-source? #t
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "HOST_CXX=" #$(cxx-for-target))
              "HOST_DMD=gdmd"
              (string-append "INSTALL_DIR=" #$output)
              (string-append "SYSCONFDIR=" #$output "/etc")
              "ENABLE_RELEASE=1"
              ;; Do not build the shared libphobos2.so library, to avoid
              ;; retaining a reference to gcc:lib.
              "SHARED=0"
              "DIFFABLE=1"              ;constant timestamp
              "VERBOSE=1")
      #:modules
      `(,@%default-gnu-modules
        (srfi srfi-1)
        (srfi srfi-26))
      #:phases
      (let* ((wrap-in-directory-excursion
              (lambda (sub-dir phase . extra-args)
                #~(lambda args
                    (with-directory-excursion #$sub-dir
                      (apply (assoc-ref %standard-phases '#$phase)
                             (append args '#$extra-args))))))
             (target-file (lambda (pkg-name path)
                            (file-append (this-package-input pkg-name) path)))
             (target-bin-sh (target-file "bash-minimal" "/bin/sh"))
             (target-lib-curl (target-file "curl" "/lib/libcurl.so")))
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (let ((dmd-source source)
                      (phobos-source #$(this-package-native-input
                                        (git-file-name "phobos" version))))
                  (mkdir "source")
                  (chdir "source")
                  (copy-recursively dmd-source    "dmd"    #:keep-mtime? #t)
                  (copy-recursively phobos-source "phobos" #:keep-mtime? #t)
                  (for-each (lambda (f)
                              (false-if-exception (make-file-writable f)))
                            (find-files ".")))))
            (add-after 'unpack 'patch-git-ls-tree
              ;; The druntime Makefile tries to use git ls-tree to get all
              ;; source files in dmd/druntime/. We replace the command with a
              ;; listing of those files.
              (lambda _
                (with-directory-excursion "dmd/druntime"
                  (substitute* "Makefile"
                    (("^MANIFEST *:*=.*$")
                     (string-append "MANIFEST := "
                                    (string-join (map (cut string-drop <> 2)
                                                      (find-files "./")))
                                    "\n"))))))
            (add-after 'unpack 'patch-paths-in-phobos
              (lambda _
                (with-directory-excursion "phobos"
                  (substitute* "std/net/curl.d"
                    (("\"libcurl\\.so\"")
                     (format #f "~s" #$target-lib-curl)))
                  (substitute* "std/process.d"
                    (("return \"/bin/sh\";")
                     (format #f "return ~s;" #$target-bin-sh))
                    (("#!/bin/sh")
                     (string-append "#!" #$target-bin-sh))))))
            (add-after 'unpack 'patch-tests
              (lambda _
                ;; Since the implementation of SOURCE_DATE_EPOCH support in
                ;; Ddoc, this test fails, as it expects Ddoc timestamps to
                ;; match the output of the `date` command.
                ;; XXX: Report upstream.
                (substitute* (string-append
                              "dmd/compiler/test/compilable"
                              "/extra-files/ddocYear-postscript.sh")
                  (("^YEAR=.*$") "YEAR=1970\n"))

                ;; This test creates a shell script and runs it.
                (substitute* "dmd/compiler/test/dshell/test6952.d"
                  (("/usr/bin/env bash") #$target-bin-sh))

                ;; In the sarif json output, the compiler version string ends
                ;; with a raw newline for some reason, causing these tests to
                ;; fail.
                (for-each
                 delete-file
                 '("dmd/compiler/test/compilable/sarif_success_test.d"
                   "dmd/compiler/test/fail_compilation/sarif_test.d"
                   "dmd/compiler/test/fail_compilation/sarifmultiple_test.d"))

                ;; tries to debug 64bit executable
                ;; not in executable format: file format not recognized
                #$@(if (target-32bit?)
                       #~(delete-file
                          (string-append "dmd/compiler/test/runnable"
                                         "/gdb_slice_debuginfo_64.d"))
                       #~())

                ;; Locations in stack traces are broken for some reason,
                ;; causing these tests to fail.
                ;; XXX: Report upstream.
                (for-each
                 delete-file
                 '("dmd/compiler/test/runnable/test17559.d"
                   "dmd/compiler/test/runnable/test19086.d"))
                (substitute* "dmd/druntime/test/exceptions/Makefile"
                  (((string-append "line_trace line_trace_21656 "
                                   "long_backtrace_trunc rt_trap_exceptions "))
                   ""))
                (substitute* "dmd/druntime/test/gc/Makefile"
                  ((" invariant ") " "))

                ;; Skip a std.process unittest that fails due to "kill" not
                ;; working properly in the build environment.
                (substitute* "phobos/std/process.d"
                  (("^.*sleep.*10000" all)
                   (string-append "    return;\n" all)))))
            (delete 'bootstrap)
            (delete 'configure)
            (replace 'build
              #$(wrap-in-directory-excursion "dmd" 'build))
            (add-after 'build 'build-phobos
              #$(wrap-in-directory-excursion "phobos" 'build))
            (add-after 'build-phobos 'build-man
              (lambda* (#:key make-flags #:allow-other-keys)
                (with-directory-excursion "dmd/compiler/docs"
                  (let ((dmd (or (which "gdmd")
                                 (which "dmd"))))
                    ((assoc-ref %standard-phases 'build)
                     #:make-flags (cons
                                   (string-append "DMD=" dmd)
                                   make-flags))))))
            (replace 'check
              #$(wrap-in-directory-excursion "dmd" 'check
                                             #:test-target "test"))
            (add-after 'check 'check-phobos
              #$(wrap-in-directory-excursion "phobos" 'check
                                             #:test-target "unittest"))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((platform (cond (#$(target-linux?) "linux")))
                       (bits (if #$(target-64bit?) 64 32))
                       (build-sub-dir (format #f "generated/~a/release/~a"
                                              platform bits))
                       (out (assoc-ref outputs "out"))
                       (lib (assoc-ref outputs "lib"))
                       (out-bin (string-append out "/bin"))
                       (out-etc (string-append out "/etc"))
                       (out-include (string-append out "/include/d/dmd"))
                       (out-include-etc (string-append out-include "/etc"))
                       (out-include-std (string-append out-include "/std"))
                       (out-lib (string-append out "/lib"))
                       (lib-lib (string-append lib "/lib"))
                       (out-man (string-append out "/share/man")))
                  (with-directory-excursion "dmd"
                    (with-directory-excursion build-sub-dir
                      (install-file "dmd" out-bin)
                      (install-file "libdruntime.a" out-lib)
                      (for-each (cut install-file <> lib-lib)
                                (find-files "." "^libdruntime\\.so[.0-9]*$")))
                    (copy-recursively "druntime/import" out-include)
                    (copy-recursively "generated/docs/man" out-man))
                  (with-directory-excursion "phobos"
                    (with-directory-excursion build-sub-dir
                      (install-file "libphobos2.a" out-lib)
                      (for-each (cut install-file <> lib-lib)
                                (find-files "." "^libphobos2\\.so[.0-9]*$")))
                    (copy-recursively "etc" out-include-etc)
                    (copy-recursively "std" out-include-std))
                  (mkdir-p out-etc)
                  (with-output-to-file (string-append out-etc "/dmd.conf")
                    (lambda _
                      (format #t "[Environment]\n")
                      (format #t "DFLAGS=")
                      (format #t " -I~a" out-include)
                      (format #t " -L-L~a" out-lib)
                      (format #t " -L-L~a" lib-lib)
                      (format #t " -L--export-dynamic")
                      (format #t " -fPIC")
                      (format #t "\n"))))))
            (replace 'install-license-files
              ;; Phobos license is identical.
              #$(wrap-in-directory-excursion "dmd" 'install-license-files))))))
    (inputs
     (list curl                         ; std.net.curl
           bash-minimal))               ; std.process
    (native-inputs
     (list gdmd which
           gdb/pinned tzdata-for-tests  ; for tests
           (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/dlang/phobos")
                    (commit (string-append "v" version))))
             (file-name (git-file-name "phobos" version))
             (sha256
              (base32
               "0afi5glnf96242cbnr10ccjvfsgkh4k5y7qnmxv4ph5g0izvi1dc"))
             (patches (search-patches "dmd-phobos-support-TZDIR.patch")))))
    (outputs '("out" "lib" "debug"))
    (home-page "https://github.com/dlang/dmd")
    (synopsis "Reference D Programming Language compiler")
    (description "@acronym{DMD, Digital Mars D compiler} is the reference
compiler for the D programming language.")
    (license license:boost1.0)
    ;; As reported by upstream: https://wiki.dlang.org/Compilers#Comparison
    ;; Note: aarch64-linux is currently unsupported as the build system doesn't
    ;; work with gdmd yet. Attempting to force through anyways gives assembler
    ;; errors.
    (supported-systems '("i686-linux" "x86_64-linux"))

    ;; This variant exists only for bootstrapping purposes.
    (properties '((hidden? . #t)))))

;; DMD built with dmd-bootstrap as the bootstrap D compiler.
;; Shared libraries are built now, tests are no longer disabled.
(define-public dmd
  (package
    (inherit dmd-bootstrap)
    (arguments
     (substitute-keyword-arguments
         (strip-keyword-arguments
          '(#:tests?)                   ;reinstate tests
          (package-arguments dmd-bootstrap))
       ((#:disallowed-references _ ''())
        (list dmd-bootstrap tzdata-for-tests))
       ((#:make-flags flags ''())
        #~(fold delete #$flags '("HOST_DMD=gdmd"
                                 "SHARED=0")))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'rewrite-references-to-bootstrap
              ;; D compilers can keep references to the include files used to
              ;; build a binary in exception messages. For dmd, rewrite the
              ;; references to dmd-bootstrap to itself, to reduce its closure
              ;; size.
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((in-dmd-bootstrap #$(this-package-native-input "dmd"))
                       (out (assoc-ref outputs "out"))
                       (out-bin-dmd (string-append out "/bin/dmd")))
                  ;; XXX: Use sed, as replace-store-references wouldn't replace
                  ;; the references, while substitute* throws an error.
                  (invoke "sed" "-i"
                          (format #f "s,~a,~a,g" in-dmd-bootstrap out)
                          out-bin-dmd))))))))
    (native-inputs
     (modify-inputs native-inputs
       (delete "gdmd")
       (append dmd-bootstrap)))
    (properties
     (alist-delete 'hidden? (package-properties dmd-bootstrap)))))


;; D related tools

(define-public dub
  (package
    (name "dub")
    (version "1.42.0-beta.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dlang/dub")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g2v7bf6sjqdwif64lic0l7jd6r0bididyc9balk8xnhiq5q65mz"))))
    (build-system gnu-build-system)     ; not really, uses a custom build script
    (arguments
     (list
      #:modules
      `(,@%default-gnu-modules
        (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              ;; These fail for various reasons. Some try to fetch a package
              ;; from code.dlang.org. Some try to clone a git repo. Some try to
              ;; run /bin/sh.
              (for-each delete-file-recursively
                        '("test/dpath-variable"
                          "test/dub-as-a-library-cwd"
                          "test/git-dependency"
                          "test/issue502-root-import"
                          "test/issue1408-inherit-linker-files"
                          "test/issue1551-var-escaping"
                          "test/issue1775"
                          "test/issue2192-environment-variables"
                          "test/issue2452"
                          "test/issue2698-cimportpaths-broken-with-dmd-ldc"
                          "test/pr2642-cache-db"
                          "test/pr2644-describe-artifact-path"
                          "test/pr2647-build-deep"))))
          (replace 'configure
            (lambda _
              (setenv "CC" #$(cc-for-target))))
          (replace 'build
            (lambda _
              (invoke "ldmd2" "-run" "build.d")))
          (add-after 'build 'prepare-post-build
            (lambda _
              (setenv "DC" "ldc2")
              (setenv "DUB" "bin/dub")
              ;; Don't store cache in $HOME, we have no home.
              (setenv "DUB_HOME" "/tmp/dub-test-home")))
          (add-after 'prepare-post-build 'generate-man
            (lambda _
              (setenv "DIFFABLE" "1")   ; replaces currentTime with a static one
              (invoke "bin/dub"  "--single" "./scripts/man/gen_man.d")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "sh" "./test/run-unittest.sh"))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (man1 (string-append out "/share/man/man1"))
                     (bash-comp (string-append out "/etc/bash_completion.d"))
                     (fish-comp (string-append
                                 out "/share/fish/vendor_completions.d"))
                     (zsh-comp (string-append out "/share/zsh/site-functions")))
                (install-file "bin/dub" bin)
                (with-directory-excursion "scripts/man"
                  (for-each (cut install-file <> man1)
                            (find-files "." "\\.1$")))
                (install-file "scripts/bash-completion/dub.bash" bash-comp)
                (install-file "scripts/fish-completion/dub.fish" fish-comp)
                (install-file "scripts/zsh-completion/_dub" zsh-comp))))
          (add-after 'install 'remove-d-include-references
            #$(remove-d-include-references-phase
               (this-package-native-input "ldc"))))))
    (inputs
     (list curl))
    (native-inputs
     (list ldc))
    (home-page "https://dub.pm/")
    (synopsis "Package and build manager for D projects")
    (description
     "DUB is a package and build manager for applications and
libraries written in the D programming language.  It can
automatically retrieve a project's dependencies and integrate
them in the build process.

The design emphasis is on maximum simplicity for simple projects,
while providing the opportunity to customize things when
needed.")
    (license license:boost1.0)))

(define-public d-tools
  (package
    (name "d-tools")
    (version "2.112.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dlang/tools")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z23ivy6nq3q45wgclp5y6cjm8awdamma491818qimgffzgiay2q"))))
    (outputs '("out" "internal"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules
      `(,@%default-gnu-modules
        (srfi srfi-26))
      #:phases
      #~(let ((sub-packages-out
               '("ddemangle"
                 ;;"dman"               ; skip as it depends “../dlang.org“
                 "dustmite"
                 "rdmd"))
              (sub-packages-internal
               '("catdoc"
                 "changed"
                 "checkwhitespace"
                 "contributors"
                 "detab"
                 "dget"
                 ;;"tests_extractor"    ; skip as it depends on libdparse
                 "tolf")))
          (modify-phases %standard-phases
            (add-after 'unpack 'patch-tests
              (lambda _
                ;; Skip broken make ONESHELL shell test.
                (substitute* "rdmd_test.d"
                  (("makeVersion = .*$") "makeVersion = \"skip\";"))))
            (replace 'configure
              (lambda _
                (setenv "CC" #$(cc-for-target))
                (setenv "DC" (which "ldc2"))
                (setenv "DUB_HOME" "/tmp/.dub")))
            (replace 'build
              (lambda _
                (for-each
                 (lambda (sub-package)
                   (invoke "dub" "build" "--build=release"
                           (string-append ":" sub-package)))
                 (append sub-packages-out sub-packages-internal))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "./dtools_rdmd" "rdmd_test.d" "dtools_rdmd"
                          "--rdmd-default-compiler" "ldmd2"))))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (out-bin (string-append out "/bin"))
                       (out-man (string-append out "/man"))
                       (internal (assoc-ref outputs "internal"))
                       (internal-bin (string-append internal "/bin"))
                       (install-sub-package
                        (lambda (pkg dir)
                          (mkdir-p dir)
                          (copy-file (string-append "dtools_" pkg)
                                     (string-append dir "/" pkg))))
                       (install-sub-packages
                        (lambda (pkgs dir)
                          (for-each (cut install-sub-package <> dir)
                                    pkgs))))
                  (install-sub-packages sub-packages-out out-bin)
                  (install-sub-packages sub-packages-internal internal-bin)
                  (copy-recursively "man" out-man))))
            (add-after 'install 'remove-d-include-references
              #$(remove-d-include-references-phase
                 (this-package-native-input "ldc")))))))
    (native-inputs
     (list ldc
           dub))
    (home-page "https://github.com/dlang/tools")
    (synopsis "Useful D-related tools")
    (description
     "@code{d-tools} provides some useful tools for working with the D
programming language. In the output @code{out} are:
@itemize
@item @code{ddemangle} (demangles D symbols)
@item @code{dustmite} (reduces D-like source code to mimimally reproduce some
test case)
@item @code{rdmd} (runs D source files as scripts)
@end itemize
In the @code{internal} output are:
@itemize
@item @code{catdoc} (concatenates Ddoc files)
@item @code{changed} (change log generator)
@item @code{checkwhitespace} (checks for correct whitespace usage in source
files)
@item @code{contributors} (query contributors between two D releases)
@item @code{detab} (replace tabs with spaces, and remove trailing whitespace
from lines)
@item @code{dget} (D source code downloader)
@item @code{tolf} (line endings converter)
@end itemize")
    (license license:boost1.0)))


;; D libraries

(define-public gtkd
  (package
    (name "gtkd")
    (version "3.10.0")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://gtkd.org/Downloads/sources/GtkD-"
                                  version ".zip"))
              (sha256
               (base32
                "0vc5ssb3ar02mg2pngmdi1xg4qjaya8332a9mk0sv97x6b4ddy3g"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags
      #~(list "DC=ldc2"
              (string-append "prefix=" (assoc-ref %outputs "out"))
              (string-append "libdir=" (assoc-ref %outputs "out") "/lib")
              "pkgconfigdir=lib/pkgconfig")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              (for-each
               (lambda (file)
                 (substitute* file (("debug\\(1\\)") "debug")))
               '("demos/gstreamer/mediaplayer/gst_mediaplayer.d"
                 "demos/gstreamer/mediaplayer/gst_mediaplayer.d"
                 "demos/gtkD/TestWindow/TestImage.d"
                 "demos/gtkD/TestWindow/TestStock.d"
                 "demos/gtkD/TestWindow/TestText.d"
                 "demos/gtkD/TestWindow/TestThemes.d"
                 "demos/gtkD/TestWindow/TestScales.d"
                 "demos/gtkD/TestWindow/TestIdle.d"))))
          (delete 'configure)
          (add-before 'build 'patch-makefile
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "GNUmakefile"
                ;; We do the tests ourselves.
                (("default-goal: libs test") "default-goal: libs")
                (("all: libs shared-libs test") "all: libs shared-libs")
                ;; Work around upstream bug.
                (("\\$\\(prefix\\)\\/\\$\\(libdir\\)") "$(libdir)"))))
          (add-before 'check 'pre-check
            (lambda _
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1")
              (setenv "CC" #$(cc-for-target))))
          (add-after 'install 'remove-d-include-references
            #$(remove-d-include-references-phase
               (this-package-native-input "ldc"))))))
    (native-inputs
     (list unzip
           ldc
           pkg-config
           xorg-server-for-tests))
    (home-page "https://gtkd.org/")
    (synopsis "D binding and OO wrapper of GTK+")
    (description "This package provides bindings to GTK+ for D.")
    (license license:lgpl2.1)))

(define-public d-demangler
  (package
    (name "d-demangler")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/lievenhey/d_demangler")
                     (commit (string-append "version-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13lbbxlaa1mffjs57xchl1g6kyr5lxi0z5x7snyvym0knslxwx2g"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no test suite
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           "d_demangle")
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (replace 'install
                     (lambda _
                       (install-file "libd_demangle.so"
                                     (string-append #$output "/lib"))))
                   (add-after 'install 'remove-d-include-references
                     #$(remove-d-include-references-phase
                        (this-package-native-input "dmd"))))))
    (native-inputs (list dmd))
    (home-page "https://github.com/lievenhey/d_demangler")
    (synopsis "D symbol demangling library")
    (description "@code{libd_demangle.so} is a small shared library that can be
used to demangle D symbols.  It exposes a C interface that wraps D's
@code{std.demangle}.")
    (license license:gpl3+)))
