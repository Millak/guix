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
  #:use-module (gnu packages bash)
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


;; Compilers and tooling for the D programming language.
;; Note: The GNU D compiler is defined in (gnu packages gcc) instead.


;; LLVM-based D compiler

;; We use GDC, the D frontend for GCC, to bootstrap ldc.  We then use
;; ldc to bootstrap itself so that no reference remains to GDC.
(define-public ldc-bootstrap
  (package
    (name "ldc")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ldc-developers/ldc/releases"
                           "/download/v" version "/ldc-" version "-src.tar.gz"))
       (sha256
        (base32 "13pkg69wjj4ali4ikijicccpg8y6f2hghhb70z9lrqr2w3pkhqna"))
       (patches (search-patches "ldc-i686-int128-alignment.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:disallowed-references (,tzdata-for-tests)
       #:tests? #f                  ;skip in the bootstrap
       #:build-type "Release"
       #:configure-flags
        (list "-GNinja"
              ;; see .github/actions/2-build-bootstrap/action.yml
              "-DBUILD_SHARED_LIBS=OFF")
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
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (let ((job-count (number->string (or (and parallel-build?
                                                       (parallel-job-count))
                                                  1))))
               (apply invoke "cmake" "--build" "." "-j" job-count
                      "--target" make-flags))))
         (replace 'install
           (lambda _
             (invoke "cmake" "--install" "."))))))
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("lld-wrapper" ,(make-lld-wrapper lld-17 #:lld-as-ld? #t))
       ("llvm" ,llvm-17)
       ("ldc" ,gdmd)
       ("ninja" ,ninja)
       ("python-wrapper" ,python-wrapper)
       ("tzdata" ,tzdata-for-tests)
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
     `((max-silent-time . ,(* 3600 3))

       ;; This variant exists solely for bootstrapping purposes.
       (hidden? . #t)))
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
     (substitute-keyword-arguments
       (strip-keyword-arguments '(#:tests?) (package-arguments ldc-bootstrap))
       ((#:make-flags _ #f)
        '(list "all"
               ;; Also build the test runner binaries.
               "ldc2-unittest" "all-test-runners"))
       ((#:configure-flags _ #~'())
        `(list "-GNinja"
               "-DBUILD_SHARED_LIBS=ON"
               ,@(if (target-riscv64?)
                     `("-DCMAKE_EXE_LINKER_FLAGS=-latomic")
                     '())))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-compiler-rt-library-discovery
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((clang-runtime (assoc-ref inputs "clang-runtime"))
                     (system ,(or (%current-target-system)
                                  (%current-system))))
                 (define (gnu-triplet->clang-arch system)
                   (let ((system-prefix
                           (car (string-tokenize
                                  system (char-set-complement (char-set #\-))))))
                     (cond
                       ((equal? system-prefix "i686") "i386")
                       (#t system-prefix))))
                 ;; Coax LLVM into agreeing with Clang about system target
                 ;; naming.
                 (substitute* "driver/linker-gcc.cpp"
                   (("triple.getArchName\\(\\)")
                    (format #f "~s" (gnu-triplet->clang-arch system))))
                 ;; Augment the configuration of the ldc2 binaries so they can
                 ;; find the compiler-rt libraries they need to be linked with
                 ;; for the tests.
                 (substitute* (find-files "." "^ldc2.*\\.conf\\.in$")
                   ((".*LIB_SUFFIX.*" all)
                    (string-append all
                                   "        \"" clang-runtime
                                   "/lib/linux\",\n"))))))
           (add-after 'unpack 'patch-paths-in-tests
             (lambda _
               (substitute* "runtime/druntime/test/profile/Makefile"
                 (("/bin/bash") (which "bash")))
               (substitute* "tests/driver/cli_CC_envvar.d"
                 (("cc") (which "clang")))
               (substitute* "tests/linking/linker_switches.d"
                 (("echo") (which "echo")))
               (substitute* "tests/dmd/dshell/test6952.d"
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
               (delete-file "tests/plugins/addFuncEntryCall/testPluginLegacy.d")
               ;; This unit test requires networking, fails with
               ;; "core.exception.RangeError@std/socket.d(778): Range
               ;; violation".
               (substitute* "runtime/phobos/std/socket.d"
                 (("assert\\(ih.addrList\\[0\\] == 0x7F_00_00_01\\);.*")
                  ""))

               ;; These tests fail on riscv64-linux.
               (substitute* "runtime/phobos/std/math/operations.d"
                 (("static assert\\(getNaNPayload\\(a\\)" all )
                  (string-append "// " all)))
               (substitute* "runtime/phobos/std/math/traits.d"
                 (("static assert\\(signbit\\(-.*\\.nan" all)
                  (string-append "// " all)))

               ;; The GDB tests suite fails; there are a few bug reports about
               ;; it upstream.
               (for-each delete-file (find-files "tests" "gdb.*\\.(c|d|sh)$"))
               (delete-file "tests/dmd/runnable/b18504.d")
               (substitute* "runtime/druntime/test/exceptions/Makefile"
                 ((".*TESTS\\+=rt_trap_exceptions_drt_gdb.*")
                  ""))
               ;; Unsupported with glibc-2.35.
               (delete-file "tests/dmd/compilable/stdcheaders.c")
               (delete-file "tests/dmd/compilable/test23958.c")
               (delete-file "tests/dmd/runnable/test23889.c")
               (delete-file "tests/dmd/runnable/test23402.d")
               (delete-file "tests/dmd/runnable/helloc.c")
               ;; Only works in 2024 and without SOURCE_DATE_EPOCH
               (delete-file "tests/dmd/compilable/ddocYear.d")
               ;; Drop gdb_dflags from the test suite.
               (substitute* "tests/dmd/CMakeLists.txt"
                 (("\\$\\{gdb_dflags\\}") ""))
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
                                 "dmd/runnable_cxx/cppa.d")))
                   (,(target-riscv64?)
                     (for-each delete-file
                               '("codegen/simd_alignment.d"
                                 "dmd/runnable/argufilem.d"
                                 "dmd/compilable/test23705.d"
                                 "dmd/fail_compilation/diag7420.d")))
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
                   ;; This test has a race condition so run it with 1 core.
                   (invoke "ctest" "--output-on-failure" "-j" "1"
                           "-R" "dmd-testsuite")
                   (display "running the defaultlib unit tests and druntime \
integration tests...\n")
                   (invoke
                     "ctest" "--output-on-failure" "-j" job-count "-E"
                     (string-append
                       "dmd-testsuite|lit-tests|ldc2-unittest"
                       ,@(cond
                           ((target-aarch64?)
                            `(,(string-append
                                 "|std.internal.math.gammafunction-shared"
                                 "|std.math.exponential-shared"
                                 "|std.internal.math.gammafunction-debug-shared"
                                 "|druntime-test-exceptions-debug")))
                           ((target-riscv64?)
                            `(,(string-append
                                 "|std.internal.math.errorfunction-shared"
                                 "|std.internal.math.gammafunction-shared"
                                 "|std.math.exponential-shared"
                                 "|std.math.trigonometry-shared"
                                 "|std.mathspecial-shared"
                                 "|std.socket-shared"
                                 "|std.internal.math.errorfunction-debug-shared"
                                 "|std.internal.math.gammafunction-debug-shared"
                                 "|std.math.operations-debug-shared"
                                 "|std.math.exponential-debug-shared"
                                 "|std.math.traits-debug-shared"
                                 "|std.mathspecial-debug-shared"
                                 "|std.math.trigonometry-debug-shared"
                                 "|std.socket-debug-shared"
                                 ;; These four hang forever
                                 "|core.thread.fiber-shared"
                                 "|core.thread.osthread-shared"
                                 "|core.thread.fiber-debug-shared"
                                 "|core.thread.osthread-debug-shared")))
                           (#t `("")))))))))))))
    (native-inputs
     (append (delete "llvm"
                     (alist-replace "ldc" (list ldc-bootstrap)
                                    (package-native-inputs ldc-bootstrap)))
         `(("clang" ,clang-17)          ;propagates llvm and clang-runtime
           ("python-lit" ,python-lit))))
    (properties
     (alist-delete 'hidden? (package-properties ldc-bootstrap)))))


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
    (version "2.111.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dlang/dmd")
                     (commit (string-append "v" version))))
              (file-name (git-file-name "dmd" version))
              (sha256
               (base32
                "0cz5qdd1j89w2s7nzw9ahzwsqiraidg4ajy7syy7qkk7mwcyrf6r"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:disallowed-references (list (gexp-input (canonical-package gcc)
                                                "lib"))
      ;; Disable tests, as gdmd cannot cope with some arguments used such as
      ;; '-conf'.
      #:tests? #f
      #:out-of-source? #t
      #:test-target "test"
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
      #~(let* ((phase-in-sub-dir (lambda (phase sub-dir)
                                   (lambda args
                                     (with-directory-excursion sub-dir
                                       (apply
                                        (assoc-ref %standard-phases phase)
                                        args)))))
               (target-bin-sh (string-append
                               #$(this-package-input "bash-minimal")
                               "/bin/sh")))
          (modify-phases %standard-phases
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
                  ;; (substitute* "std/datetime/timezone.d"
                  ;;   (("\"/usr/share/zoneinfo/\"")
                  ;;    (format #f "~s" target-zoneinfo)))
                  ;; (substitute* "std/net/curl.d"
                  ;;   (("\"libcurl\\.so\"")
                  ;;    (format #f "~s" target-lib-curl)))
                  (substitute* "std/process.d"
                    (("return \"/bin/sh\";")
                     (format #f "return ~s;" target-bin-sh))
                    (("#!/bin/sh")
                     (string-append "#!" target-bin-sh))))))
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
                  (("/usr/bin/env bash") target-bin-sh))

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
                       '((delete-file
                          "dmd/compiler/test/runnable/gdb_slice_debuginfo_64.d"))
                       '())

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
                  ((" invariant ") " "))))
            (delete 'bootstrap)
            (delete 'configure)
            (replace 'build
              (phase-in-sub-dir 'build "dmd"))
            (add-after 'build 'build-phobos
              (phase-in-sub-dir 'build "phobos"))
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
              (phase-in-sub-dir 'check "dmd"))
            (add-after 'check 'check-phobos
              (phase-in-sub-dir 'check "phobos"))
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
                    (copy-recursively "etc" (string-append out-include "/etc"))
                    (copy-recursively "std" (string-append out-include "/std")))
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
              (phase-in-sub-dir 'install-license-files "dmd"))))))
    (inputs
     (list bash-minimal))
    (native-inputs
     (list gdmd which
           gdb/pinned   ; for tests
           (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/dlang/phobos")
                    (commit (string-append "v" version))))
             (file-name (git-file-name "phobos" version))
             (sha256
              (base32
               "1ydls3ar6d3f7ffqvidr46x3zrz3wlzjln5qa0nbz843ndjr4g7n")))))
    (outputs '("out" "lib" "debug"))
    (synopsis "Reference D Programming Language compiler")
    (description "@acronym{DMD, Digital Mars D compiler} is the reference
compiler for the D programming language.")
    (license license:boost1.0)
    (home-page "https://github.com/dlang/dmd")
    ;; As reported by upstream:
    ;; https://wiki.dlang.org/Compilers#Comparison
    (supported-systems '("i686-linux" "x86_64-linux" "aarch64-linux"))

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
        (list dmd-bootstrap))
       ((#:make-flags flags ''())
        #~(fold delete #$flags '("HOST_DMD=gdmd"
                                 "SHARED=0")))
       ((#:phases phases '%standard-phases)
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
    (version "1.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlang/dub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09p3rvsv11f8lgqgxgz2zj0szsw5lzrsc7y7471hswksc7nmmj70"))))
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

(define-public d-tools
  (package
    (name "d-tools")
    (version "2.105.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlang/tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hvz786k0pi8697x1vk9x5bx52jiy7pvi13wmfkx15ddvv0x5j33"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-tests
                 (lambda _
                   ;; Skip broken make ONESHELL shell test.
                   (substitute* "rdmd_test.d"
                     (("makeVersion = .*$") "makeVersion = \"skip\";"))))
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
                             "-m" (if #$(target-64bit?) "64" "32")
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


;; D libraries

(define-public gtkd
  (package
    (name "gtkd")
    (version "3.10.0")
    (source
     (origin
      (method url-fetch/zipbomb)
      (uri (string-append "https://gtkd.org/Downloads/sources/GtkD-"
                          version ".zip"))
      (sha256
       (base32 "0vc5ssb3ar02mg2pngmdi1xg4qjaya8332a9mk0sv97x6b4ddy3g"))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip
           ldc
           pkg-config
           xorg-server-for-tests))
    (arguments
     `(#:test-target "test"
       #:make-flags
       `("DC=ldc2"
         ,(string-append "prefix=" (assoc-ref %outputs "out"))
         ,(string-append "libdir=" (assoc-ref %outputs "out") "/lib")
         "pkgconfigdir=lib/pkgconfig")
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
               (("\\$\\(prefix\\)\\/\\$\\(libdir\\)") "$(libdir)"))))
         (add-before 'check 'pre-check
           (lambda _
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             (setenv "CC" ,(cc-for-target)))))))
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
      #:tests? #f                       ;no test suite
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           "d_demangle")
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (replace 'install
                     (lambda _
                       (install-file "libd_demangle.so"
                                     (string-append #$output "/lib")))))))
    (native-inputs (list dmd))
    (synopsis "D symbol demangling library")
    (description "@code{libd_demangle.so} is a small shared library that can be used to
demangle D symbols.  It exposes a C interface that wraps D's @code{std.demangle}.")
    (license license:gpl3+)
    (home-page "https://github.com/lievenhey/d_demangler")))
