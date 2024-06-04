;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015, 2018 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2017 Frederick Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2019, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2021-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
      (inputs (list gdc perl))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ldc-developers/ldc/releases"
                           "/download/v" version "/ldc-" version "-src.tar.gz"))
       (sha256
        (base32 "13pkg69wjj4ali4ikijicccpg8y6f2hghhb70z9lrqr2w3pkhqna"))))
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
     `(("libconfig" ,libconfig)
       ("libedit" ,libedit)
       ("zlib" ,zlib)))
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
           ("python-lit" ,python-lit))))))

;;; Bootstrap version of phobos that is built with GDC, using GDC's standard
;;; library.
(define dmd-bootstrap
  (package
    ;; This package is purposefully named just "dmd" and not "dmd-bootstrap",
    ;; as the final dmd package rewrites references from this one to itself,
    ;; and their names must have the same length to avoid corrupting the
    ;; binary.
    (name "dmd")
    (version "2.106.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dlang/dmd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "dmd" version))
              (sha256
               (base32
                "1bq4jws1vns2jjzfz7biyngrx9y5pvvgklymhrvb5kvbzky1ldmy"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:disallowed-references (list (gexp-input (canonical-package gcc)
                                                "lib"))
      ;; Disable tests, as gdmd cannot cope with some arguments used such as
      ;; '-conf'.
      #:tests? #f
      #:test-target "test"
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              ;; XXX: Proceed despite conflicts from symbols provided by both
              ;; the source built and GDC.
              "DFLAGS=-L--allow-multiple-definition"
              "ENABLE_RELEASE=1"
              (string-append "HOST_CXX=" #$(cxx-for-target))
              "HOST_DMD=gdmd"
              (string-append "INSTALL_DIR=" #$output)
              ;; Do not build the shared libphobos2.so library, to avoid
              ;; retaining a reference to gcc:lib.
              "SHARED=0"
              (string-append "SYSCONFDIR=" #$output "/etc")
              "VERBOSE=1"
              "-f" "posix.mak")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-phobos-source-and-chdir
            ;; Start with building phobos, which in turns will automatically
            ;; build druntime and dmd.  A minimal dmd command is still
            ;; required to do so, which is why we need dmd-bootstrap-0.
            (lambda _
              (symlink "." "dmd")  ;to please the build system expected layout
              (copy-recursively
               #$(origin
                   (method git-fetch)
                   (uri (git-reference
                         (url "https://github.com/dlang/phobos")
                         (commit (string-append "v" version))))
                   (file-name (git-file-name "phobos" version))
                   (sha256
                    (base32
                     "1yw7nb5d78cx9m7sfibv7rfc7wj3w0dw9mfk3d269qpfpnwzs4n9")))
               "phobos")
              (chdir "phobos")))
          (add-after 'copy-phobos-source-and-chdir 'adjust-phobos-install-dirs
            (lambda _
              (substitute* "posix.mak"
                ;; Install to lib directory, not to e.g. 'linux/lib64'.
                (("\\$\\(INSTALL_DIR)/\\$\\(OS)/\\$\\(lib_dir)")
                 (string-append #$output "/lib"))
                ;; Do not install license file, already done by the gnu build
                ;; system.
                ((".*\\$\\(INSTALL_DIR)/phobos-LICENSE.txt.*") ""))))
          (delete 'configure)
          (add-after 'install 'install-druntime
            (lambda args
              (chdir "../druntime")
              (apply (assoc-ref %standard-phases 'install) args)
              (chdir "..")))
          (add-after 'install-druntime 'install-includes
            (lambda _
              ;; Normalize the include files prefix to include/dmd.
              (let ((include-dir (string-append #$output "/include/dmd")))
                (mkdir-p include-dir)
                (rename-file (string-append #$output "/src/phobos")
                             (string-append include-dir))
                (copy-recursively "druntime/import" include-dir))
              (delete-file-recursively (string-append #$output "/src"))))
          (add-after 'install-druntime 'install-dmd
            (assoc-ref %standard-phases 'install))
          (add-after 'install-license-files 'refine-install-layout
            (lambda _
              (let* ((docdir (string-append #$output "/share/doc/"
                                            (strip-store-file-name #$output)))
                     ;; The dmd binary gets installed to
                     ;; e.g. /linux/bin64/dmd.
                     (dmd (car (find-files #$output "^dmd$")))
                     (dmd.conf (car (find-files #$output "^dmd.conf$")))
                     (os-dir (dirname (dirname dmd))))
                ;; Move samples from root to the doc directory.
                (rename-file (string-append #$output "/samples")
                             (string-append docdir "/samples"))
                ;; Remove duplicate license file.
                (delete-file (string-append #$output
                                            "/dmd-boostlicense.txt"))
                ;; Move dmd binary and dmd.conf.
                (install-file dmd (string-append #$output "/bin"))
                (install-file dmd.conf (string-append #$output "/etc"))
                (delete-file-recursively os-dir))))
          (add-after 'refine-install-layout 'patch-dmd.conf
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* (search-input-file outputs "etc/dmd.conf")
                (("lib(32|64)")
                 "lib")
                (("\\.\\./src/(phobos|druntime/import)")
                 "include/dmd")))))))
    (native-inputs (list gdmd which))
    (home-page "https://github.com/dlang/dmd")
    (synopsis "Reference D Programming Language compiler")
    (description "@acronym{DMD, Digital Mars D compiler} is the reference
compiler for the D programming language.")
    ;; As reported by upstream:
    ;; https://wiki.dlang.org/Compilers#Comparison
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:boost1.0)))

;;; Second bootstrap of DMD, built using dmd-bootstrap, with its shared
;;; libraries preserved.
(define-public dmd
  (package
    (inherit dmd-bootstrap)
    (arguments
     (substitute-keyword-arguments
         (strip-keyword-arguments
          '(#:tests?)                   ;reinstate tests
          (package-arguments dmd-bootstrap))
       ((#:disallowed-references  _ ''())
        (list dmd-bootstrap))
       ((#:modules _ ''())
        '((guix build gnu-build-system)
          (guix build utils)
          (srfi srfi-1)))               ;for fold
       ((#:make-flags flags ''())
        #~(fold delete #$flags '("DFLAGS=-L--allow-multiple-definition"
                                 "HOST_DMD=gdmd"
                                 "SHARED=0")))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'patch-dmd.conf 'rewrite-references-to-bootstrap
              ;; DMD keeps references to include files used to build a
              ;; binary.  Rewrite those of dmd-bootstrap to itself, to reduce
              ;; its closure size.
              (lambda* (#:key native-inputs inputs outputs
                        #:allow-other-keys)
                (let ((dmd (search-input-file outputs "bin/dmd"))
                      (dmd-bootstrap (dirname
                                      (dirname
                                       (search-input-file
                                        (or native-inputs inputs)
                                        "bin/dmd")))))
                  ;; XXX: Use sed, as replace-store-references wouldn't
                  ;; replace the references, while substitute* throws an
                  ;; error.
                  (invoke "sed" "-i"
                          (format #f "s,~a,~a,g" dmd-bootstrap #$output)
                          dmd))))))))
    (native-inputs (modify-inputs (package-native-inputs dmd-bootstrap)
                     (replace "gdmd" dmd-bootstrap)))))

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
    (home-page "https://github.com/lievenhey/d_demangler")
    (synopsis "Utility to demangle D symbols")
    (description "@command{d_demangle} is a small utility that can be used to
demangle D symbols.  A shared library is also provided.")
    (license license:gpl3+)))
