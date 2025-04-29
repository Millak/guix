;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Dennis Mungai <dmngaie@gmail.com>
;;; Copyright © 2016, 2018, 2019, 2020, 2021, 2023, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018–2022 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2021-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Arm Ltd <David.Truby@arm.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021, 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2020-2022, 2024-2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022, 2024 Greg Hogan <code@greghogan.com>
;;; Copyright © 2022, 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023-2025 Zheng Junjie <z572@z572.online>
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

(define-module (gnu packages llvm)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix memoization)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages bootstrap)           ;glibc-dynamic-linker
  #:use-module (gnu packages check)               ;python-lit
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (make-lld-wrapper
            system->llvm-target))

;; Lazily resolve the gcc-toolchain to avoid a circular dependency.
(define gcc-toolchain*
  (delay (module-ref (resolve-interface '(gnu packages commencement))
                     'gcc-toolchain)))

(define* (system->llvm-target #:optional
                              (system (or (and=> (%current-target-system)
                                                 gnu-triplet->nix-system)
                                          (%current-system))))
  "Return the LLVM target name that corresponds to SYSTEM, a system type such
as \"x86_64-linux\"."
  ;; See the 'lib/Target' directory of LLVM for a list of supported targets.
  (match (system->llvm-target-arch system)
    ("RISCV64" "RISCV")
    ("X86_64" "X86")
    (x x)))

(define* (system->llvm-target-arch #:optional
                                   (system (or (and=> (%current-target-system)
                                                      gnu-triplet->nix-system)
                                               (%current-system))))
  "Return the LLVM target arch name that corresponds to SYSTEM, a system type such
as \"x86_64-linux\"."
  ;; See the 'cmake/config-ix.cmake' file of LLVM for a list of supported targets arch.
  ;; start with # Determine the native architecture.
  (letrec-syntax ((matches (syntax-rules (=>)
                             ((_ (system-prefix => target) rest ...)
                              (if (string-prefix? system-prefix system)
                                  target
                                  (matches rest ...)))
                             ((_) #f))))
    (matches ("aarch64"     => "AArch64")
             ("armhf"       => "ARM")
             ("mips64el"    => "Mips")
             ("powerpc"     => "PowerPC")
             ("riscv64"     => "RISCV64")
             ("x86_64"      => "X86_64")
             ("i686"        => "X86")
             ("i586"        => "X86")
             ("avr"         => "AVR")
             ("loongarch64" => "LoongArch"))))

(define (llvm-uri component version)
  ;; LLVM release candidate file names are formatted 'tool-A.B.C-rcN/tool-A.B.CrcN.src.tar.xz'
  ;; so we specify the version as A.B.C-rcN and delete the hyphen when referencing the file name.
  (string-append "https://github.com/llvm/llvm-project/releases/download"
                 "/llvmorg-" version "/" component "-" (string-delete #\- version) ".src.tar.xz"))

(define %llvm-release-monitoring-url
  "https://github.com/llvm/llvm-project/releases")

(define* (clang-runtime-from-llvm llvm
                                  #:optional
                                  hash
                                  (patches '()))
  (package
    (name "clang-runtime")
    (version (package-version llvm))
    (source
     (if hash
         (origin
           (method url-fetch)
           (uri (llvm-uri "compiler-rt" version))
           (sha256 (base32 hash))
           (patches (map search-patch patches)))
         (llvm-monorepo (package-version llvm))))
    (build-system cmake-build-system)
    (native-inputs
     (cond ((version>=? version "19")
            ;; TODO: Remove this when GCC 14 is the default.
            ;; libfuzzer fails to build with GCC 13
            (modify-inputs (package-native-inputs llvm)
              (prepend gcc-14)))
           ((version>=? version "18")
            ;; TODO: Remove this when GCC 13 is the default.
            ;; libfuzzer fails to build with GCC 12
            (modify-inputs (package-native-inputs llvm)
              (prepend gcc-13)))
           ((version>=? version "15")
            ;; TODO: Remove this when GCC 12 is the default.
            ;; libfuzzer fails to build with GCC 11
            (modify-inputs (package-native-inputs llvm)
              (prepend gcc-12)))
           (else (package-native-inputs llvm))))
    (inputs
     (append
      (list llvm)
      (if (version>=? version "15")
          (list libffi)
          '())
      (if (member (version-major version) (list "10" "11"))
          (list libxcrypt)
          '())))
    (arguments
     `(;; Don't use '-g' during the build to save space.
       #:build-type "Release"
       #:tests? #f                      ; Tests require gtest
       #:modules ((srfi srfi-1)
                  (ice-9 match)
                  ,@%cmake-build-system-modules)
       #:phases (modify-phases (@ (guix build cmake-build-system) %standard-phases)
                  ,@(if hash
                        '()
                        '((add-after 'unpack 'change-directory
                            (lambda _
                              (chdir "compiler-rt")))))
                  (add-after 'set-paths 'hide-glibc
                    ;; Work around https://issues.guix.info/issue/36882.  We need to
                    ;; remove glibc from CPLUS_INCLUDE_PATH so that the one hardcoded
                    ;; in GCC, at the bottom of GCC include search-path is used.
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((filters '("libc"))
                             (input-directories
                              (filter-map (lambda (input)
                                            (match input
                                              ((name . dir)
                                               (and (not (member name filters))
                                                    dir))))
                                          inputs)))
                        (set-path-environment-variable "CPLUS_INCLUDE_PATH"
                                                       '("include")
                                                       input-directories)
                        #t))))))
    (home-page "https://compiler-rt.llvm.org")
    (synopsis "Runtime library for Clang/LLVM")
    (description
     "The \"clang-runtime\" library provides the implementations of run-time
functions for C and C++ programs.  It also provides header files that allow C
and C++ source code to interface with the \"sanitization\" passes of the clang
compiler.  In LLVM this library is called \"compiler-rt\".")
    (license (package-license llvm))
    (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)
                  (upstream-name . "compiler-rt")))

    ;; <https://compiler-rt.llvm.org/> doesn't list MIPS as supported.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define* (clang-from-llvm llvm clang-runtime
                          #:optional hash
                          #:key (patches '()) tools-extra
                          (properties
                           (append `((release-monitoring-url
                                      . ,%llvm-release-monitoring-url))
                                   (clang-properties (package-version llvm))))
                          (legacy-build-shared-libs? #f))
  "Produce Clang with dependencies on LLVM and CLANG-RUNTIME, and applying the
given PATCHES.  When TOOLS-EXTRA is given, it must point to the
'clang-tools-extra' tarball, which contains code for 'clang-tidy', 'pp-trace',
'modularize', and other tools.  LEGACY-BUILD-SHARED-LIBS? is used to configure
the package to use the legacy BUILD_SHARED_LIBS CMake option, which was used
until LLVM/Clang 14."
  (package
    (name "clang")
    (version (package-version llvm))
    (source
     (if hash
         (origin
           (method url-fetch)
           (uri (llvm-uri (if (version>=? version "9.0.1")
                              "clang"
                              "cfe")
                          version))
           (sha256 (base32 hash))
           (patches (map search-patch patches)))
         (llvm-monorepo (package-version llvm))))
    ;; Using cmake allows us to treat llvm as an external library.  There
    ;; doesn't seem to be any way to do this with clang's autotools-based
    ;; build system.
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("libxml2" ,libxml2)
       ("gcc-lib" ,gcc "lib")
       ,@(package-inputs llvm)
       ,@(if tools-extra
             `(("clang-tools-extra" ,tools-extra))
             '())))
    (propagated-inputs
     (list llvm clang-runtime))
    (arguments
     `(#:configure-flags
       (list "-DCLANG_INCLUDE_TESTS=True"

             ;; TODO: Use --gcc-install-dir when GCC_INSTALL_PREFIX is
             ;; removed.  See: https://github.com/llvm/llvm-project/pull/77537
             ,@(if (version>=? version "19")
                   '("-DUSE_DEPRECATED_GCC_INSTALL_PREFIX=ON")
                   '())
             ;; Find libgcc_s, crtbegin.o, and crtend.o.
             (string-append "-DGCC_INSTALL_PREFIX="
                            (assoc-ref %build-inputs "gcc-lib"))

             ;; Use a sane default include directory.
             (string-append "-DC_INCLUDE_DIRS="
                            (assoc-ref %build-inputs "libc")
                            "/include")
             ,@(if (target-riscv64?)
                   (list "-DLIBOMP_LIBFLAGS=-latomic"
                         "-DCMAKE_SHARED_LINKER_FLAGS=-latomic")
                   `())
             ,@(if legacy-build-shared-libs?
                   '()
                   (list "-DCLANG_LINK_CLANG_DYLIB=ON")))

       ,@(if (target-riscv64?)
             `(#:make-flags '("LDFLAGS=-latomic"))
             '())

       ;; Don't use '-g' during the build to save space.
       #:build-type "Release"

       #:phases (modify-phases %standard-phases
                  ,@(if tools-extra
                        `((add-after 'unpack 'add-tools-extra
                            (lambda* (#:key inputs #:allow-other-keys)
                              ;; Unpack the 'clang-tools-extra' tarball under
                              ;; tools/.
                              (let ((extra (assoc-ref inputs
                                                      "clang-tools-extra")))
                                (invoke "tar" "xf" extra)
                                (rename-file ,(string-append
                                               "clang-tools-extra-"
                                               (string-delete #\- (package-version llvm))
                                               ".src")
                                             "tools/extra")
                                ,@(if legacy-build-shared-libs?
                                      ;; Build and link to shared libraries.
                                      '((substitute* "cmake/modules/AddClang.cmake"
                                          (("BUILD_SHARED_LIBS") "True")))
                                      '())
                                #t))))
                        '())
                  (add-after 'unpack 'add-missing-triplets
                    (lambda _
                      ;; Clang iterates through known triplets to search for
                      ;; GCC's headers, but does not recognize some of the
                      ;; triplets that are used in Guix.
                      (substitute* ,@(if (version>=? version "6.0")
                                         '("lib/Driver/ToolChains/Gnu.cpp")
                                         '("lib/Driver/ToolChains.cpp"))
                        (("\"aarch64-linux-gnu\"," all)
                         (string-append "\"aarch64-unknown-linux-gnu\", "
                                        all))
                        (("\"arm-linux-gnueabihf\"," all)
                         (string-append all
                                        " \"arm-unknown-linux-gnueabihf\","))
                        (("\"i686-pc-linux-gnu\"," all)
                         (string-append "\"i686-unknown-linux-gnu\", "
                                        all)))
                      #t))
                  (add-after 'unpack 'set-glibc-file-names
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((libc (assoc-ref inputs "libc"))
                            (compiler-rt (assoc-ref inputs "clang-runtime"))
                            (gcc (assoc-ref inputs "gcc")))
                        ,@(cond
                           ((version>=? version "6.0")
                            `(;; Link to libclang_rt files from clang-runtime.
                              (substitute* "lib/Driver/ToolChain.cpp"
                                (("getDriver\\(\\)\\.ResourceDir")
                                 (string-append "\"" compiler-rt "\"")))

                              ;; Make "LibDir" refer to <glibc>/lib so that it
                              ;; uses the right dynamic linker file name.
                              (substitute* "lib/Driver/ToolChains/Linux.cpp"
                                (("(^[[:blank:]]+LibDir = ).*" _ declaration)
                                 (string-append declaration "\"" libc "/lib\";\n"))

                                ;; Make clang look for libstdc++ in the right
                                ;; location.
                                (("LibStdCXXIncludePathCandidates\\[\\] = \\{")
                                 (string-append
                                  "LibStdCXXIncludePathCandidates[] = { \"" gcc
                                  "/include/c++\","))

                                ;; Make sure libc's libdir is on the search path, to
                                ;; allow crt1.o & co. to be found.
                                (("@GLIBC_LIBDIR@")
                                 (string-append libc "/lib")))))
                           (else
                            `((substitute* "lib/Driver/Tools.cpp"
                                ;; Patch the 'getLinuxDynamicLinker' function so that
                                ;; it uses the right dynamic linker file name.
                                (("/lib64/ld-linux-x86-64.so.2")
                                 (string-append libc
                                                ,(glibc-dynamic-linker))))

                              ;; Link to libclang_rt files from clang-runtime.
                              ;; This substitution needed slight adjustment in 3.8.
                              ,@(if (version>=? version "3.8")
                                    '((substitute* "lib/Driver/Tools.cpp"
                                        (("TC\\.getDriver\\(\\)\\.ResourceDir")
                                         (string-append "\"" compiler-rt "\""))))
                                    '((substitute* "lib/Driver/ToolChain.cpp"
                                        (("getDriver\\(\\)\\.ResourceDir")
                                         (string-append "\"" compiler-rt "\"")))))

                              ;; Make sure libc's libdir is on the search path, to
                              ;; allow crt1.o & co. to be found.
                              (substitute* "lib/Driver/ToolChains.cpp"
                                (("@GLIBC_LIBDIR@")
                                 (string-append libc "/lib"))))))
                        #t)))
                  ,@(if (version>=? version "17")
                        '((add-after 'unpack 'include-test-runner
                            (lambda _
                              (substitute* "CMakeLists.txt"
                                ((".*llvm_gtest" line)
                                 (string-append
                                  "add_subdirectory(${LLVM_THIRD_PARTY_DIR}/uni\
ttest third-party/unittest)\n" line))))))
                        '())
                  ;; The build daemon goes OOM on i686-linux on this phase.
                  ,@(if (and (version>=? version "15")
                             (target-x86-32?))
                        '((delete 'make-dynamic-linker-cache))
                        '())
                  ;; Awkwardly, multiple phases added after the same phase,
                  ;; e.g. unpack, get applied in the reverse order.  In other
                  ;; words, adding 'change-directory last means it occurs
                  ;; first after the unpack phase.
                  ,@(if (version>=? version "14")
                        '((add-after 'unpack 'change-directory
                            (lambda _
                              (chdir "clang"))))
                        '())
                  ,@(if (version>=? version "10")
                        `((add-after 'install 'adjust-cmake-file
                            (lambda* (#:key outputs #:allow-other-keys)
                              (let ((out (assoc-ref outputs "out")))
                                ;; Clang generates a CMake file with "targets"
                                ;; for each installed library file.  Downstream
                                ;; consumers of the CMake interface can use this
                                ;; to get absolute library locations.  Including
                                ;; this file will needlessly assert that _all_
                                ;; libraries are available, which causes problems
                                ;; in Guix because some are removed (see the
                                ;; move-extra-tools phase).  Thus, remove the
                                ;; asserts so that the main functionality works.
                                (substitute*
                                    (string-append
                                     out
                                     "/lib/cmake/clang/ClangTargets-release.cmake")
                                  (("list\\(APPEND _IMPORT_CHECK_TARGETS.*" all)
                                   (string-append "# Disabled by Guix.\n#" all)))
                                #t))))
                        '())
                  ,@(if (version>? version "3.8")
                        `((add-after 'install 'symlink-cfi_ignorelist
                            (lambda* (#:key inputs outputs #:allow-other-keys)
                              (let* ((out (assoc-ref outputs "out"))
                                     (lib-share (string-append out "/lib/clang/"
                                                               ,version "/share"))
                                     (compiler-rt (assoc-ref inputs "clang-runtime"))
                                     (file-name ,(if (version>=? version "13")
                                                     "cfi_ignorelist.txt"
                                                     "cfi_blacklist.txt"))
                                     ;; The location varies between Clang versions.
                                     (cfi-ignorelist
                                      (cond
                                       ((file-exists?
                                         (string-append compiler-rt "/" file-name))
                                        (string-append compiler-rt "/" file-name))
                                       (else (string-append compiler-rt
                                                            "/share/" file-name)))))
                                (mkdir-p lib-share)
                                ;; Symlink the ignorelist to where Clang expects
                                ;; to find it.
                                ;; Not all architectures support CFI.
                                ;; see: compiler-rt/cmake/config-ix.cmake
                                (when (file-exists? cfi-ignorelist)
                                  (symlink cfi-ignorelist
                                           (string-append lib-share "/" file-name)))))))
                        '())
                  (add-after 'install 'install-clean-up-/share/clang
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (compl-dir (string-append
                                         out "/etc/bash_completion.d")))
                        (with-directory-excursion (string-append out
                                                                 "/share/clang")
                          (for-each
                            (lambda (file)
                              (when (file-exists? file)
                                (delete-file file)))
                            ;; Delete extensions for proprietary text editors.
                            '("clang-format-bbedit.applescript"
                              "clang-format-sublime.py"
                              ;; Delete Emacs extensions: see their respective Emacs
                              ;; Guix package instead.
                              "clang-rename.el" "clang-format.el"))
                          ;; Install bash completion.
                          (when (file-exists?  "bash-autocomplete.sh")
                            (mkdir-p compl-dir)
                            (rename-file "bash-autocomplete.sh"
                                         (string-append compl-dir "/clang")))))
                      #t)))))

    ;; Clang supports the same environment variables as GCC.
    (native-search-paths %gcc-search-paths)

    (home-page "https://clang.llvm.org")
    (synopsis "C language family frontend for LLVM")
    (description
     "Clang is a compiler front end for the C, C++, Objective-C and
Objective-C++ programming languages.  It uses LLVM as its back end.  The Clang
project includes the Clang front end, the Clang static analyzer, and several
code analysis tools.")
    (properties properties)
    (license (if (version>=? version "9.0")
                 license:asl2.0         ;with LLVM exceptions
                 license:ncsa))))

(define (clang-properties version)
  "Return package properties for Clang VERSION."
  `((clang-compiler-cpu-architectures version)))

(define-public (make-clang-toolchain clang libomp)
  (package
    (name (string-append (package-name clang) "-toolchain"))
    (version (package-version clang))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (srfi srfi-26)
                                (guix build union))

                   (let ((out (assoc-ref %outputs "out")))

                     (match %build-inputs
                       (((names . directories) ...)
                        (union-build out directories)))

                     ;; Create 'cc' and 'c++' so that one can use it as a
                     ;; drop-in replacement for the default tool chain and
                     ;; have configure scripts find the compiler.
                     (symlink "clang" (string-append out "/bin/cc"))
                     (symlink "clang++" (string-append out "/bin/c++"))

                     (union-build (assoc-ref %outputs "debug")
                                  (list (assoc-ref %build-inputs
                                                   "libc-debug")))
                     (union-build (assoc-ref %outputs "static")
                                  (list (assoc-ref %build-inputs
                                                   "libc-static")))
                     #t))))

    (native-search-paths
     (append (package-native-search-paths clang)
             (list (search-path-specification     ;copied from glibc
                    (variable "GUIX_LOCPATH")
                    (files '("lib/locale"))))))
    (search-paths (package-search-paths clang))

    (license (package-license clang))
    (properties (package-properties clang))  ;for 'compiler-cpu-architectures'
    (home-page "https://clang.llvm.org")
    (synopsis "Complete Clang toolchain for C/C++ development")
    (description "This package provides a complete Clang toolchain for C/C++
development to be installed in user profiles.  This includes Clang, as well as
libc (headers and binaries, plus debugging symbols in the @code{debug}
output), and Binutils.")
    (outputs '("out" "debug" "static"))
    (inputs `(("clang" ,clang)
              ("ld-wrapper" ,(car (assoc-ref (%final-inputs) "ld-wrapper")))
              ("binutils" ,binutils)
              ("libomp" ,libomp)            ;used when linking with '-fopenmp'
              ("libc" ,glibc)
              ("libc-debug" ,glibc "debug")
              ("libc-static" ,glibc "static")))))

(define %llvm-monorepo-hashes
  '(("14.0.6" . "14f8nlvnmdkp9a9a79wv67jbmafvabczhah8rwnqrgd5g3hfxxxx")
    ("15.0.7" . "12sggw15sxq1krh1mfk3c1f07h895jlxbcifpwk3pznh4m1rjfy2")
    ("16.0.6" . "0jxmapg7shwkl88m4mqgfjv4ziqdmnppxhjz6vz51ycp2x4nmjky")
    ("17.0.6" . "1a7rq3rgw5vxm8y39fyzr4kv7w97lli4a0c1qrkchwk8p0n07hgh")
    ("18.1.8" . "1l9wm0g9jrpdf309kxjx7xrzf13h81kz8bbp0md14nrz38qll9la")
    ("19.1.7" . "18hkfhsm88bh3vnj21q7f118vrcnf7z6q1ylnwbknyb3yvk0343i")
    ("20.1.3" . "039cg9xy55pb5x7zxcwng7lp7hpmryddhfwv321kz5214k97vsdh")))

(define %llvm-patches
  '(("14.0.6" . ("clang-14.0-libc-search-path.patch"
                 "clang-runtime-14-glibc-2.36-compat.patch"
                 "clang-14-remove-crypt-interceptors.patch"))
    ("15.0.7" . ("clang-15.0-libc-search-path.patch"
                 "clang-16-remove-crypt-interceptors.patch"))
    ("16.0.6" . ("clang-16.0-libc-search-path.patch"
                 "clang-16-remove-crypt-interceptors.patch"))
    ("17.0.6" . ("clang-17.0-libc-search-path.patch"
                 "clang-17.0-link-dsymutil-latomic.patch"))
    ("18.1.8" . ("clang-18.0-libc-search-path.patch"
                 "clang-17.0-link-dsymutil-latomic.patch"))
    ("19.1.7" . ("clang-18.0-libc-search-path.patch"
                 "clang-17.0-link-dsymutil-latomic.patch"))
    ("20.1.3" . ("clang-18.0-libc-search-path.patch"
                 "clang-17.0-link-dsymutil-latomic.patch"))))

(define (llvm-monorepo version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/llvm/llvm-project")
          (commit (string-append "llvmorg-" version))))
    (file-name (git-file-name "llvm-project" version))
    (sha256 (base32 (assoc-ref %llvm-monorepo-hashes version)))
    (patches (map search-patch (assoc-ref %llvm-patches version)))))

;; A base llvm package that can be used for creating other llvm packages.
(define make-llvm
  (mlambda (version)
    (package
      (name "llvm")
      (version version)
      (source (llvm-monorepo version))
      (build-system cmake-build-system)
      (outputs '("out" "opt-viewer"))
      (arguments
       (list
        #:configure-flags
        #~(list
           ;; These options are required for cross-compiling LLVM according
           ;; to <https://llvm.org/docs/HowToCrossCompileLLVM.html>.
           #$@(if (%current-target-system)
                  (or (and=>
                       (system->llvm-target-arch)
                       (lambda (llvm-target-arch)
                         #~((string-append "-DLLVM_TABLEGEN="
                                    #+(file-append this-package
                                                   "/bin/llvm-tblgen"))
                            #$@(if (version>=? version "16.0")
                                   #~((string-append
                                        "-DLLVM_NATIVE_TOOL_DIR="
                                        #+(file-append this-package "/bin")))
                                   #~())
                            #$@(if (version>=? version "17.0")
                                 #~((string-append "-DLLVM_HOST_TRIPLE="
                                                   #$(%current-target-system)))
                                 #~((string-append "-DLLVM_DEFAULT_TARGET_TRIPLE="
                                                   #$(%current-target-system))
                                    (string-append "-DLLVM_TARGET_ARCH="
                                                   #$llvm-target-arch)))
                            #$(string-append "-DLLVM_TARGETS_TO_BUILD="
                                             (system->llvm-target)))))
                      (raise (condition
                              (&package-unsupported-target-error
                               (package this-package)
                               (target (%current-target-system))))))
                  '())
           ;; Note: sadly, the build system refuses the use of
           ;; -DBUILD_SHARED_LIBS=ON and the large static archives are needed to
           ;; build clang-runtime, so we cannot delete them.
           "-DLLVM_BUILD_LLVM_DYLIB=ON"
           "-DLLVM_LINK_LLVM_DYLIB=ON"
           "-DLLVM_ENABLE_FFI=ON"
           "-DLLVM_ENABLE_RTTI=ON"        ;for some third-party utilities
           "-DLLVM_INSTALL_UTILS=ON"      ;needed for rustc
           "-DLLVM_PARALLEL_LINK_JOBS=1") ;cater to smaller build machines
        ;; Don't use '-g' during the build, to save space.
        #:build-type "Release"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'change-directory
              (lambda _
                (chdir "llvm")))
            (add-after 'install 'install-opt-viewer
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((opt-viewer-share (string-append #$output:opt-viewer
                                                        "/share")))
                  (mkdir-p opt-viewer-share)
                  (rename-file (string-append #$output "/share/opt-viewer")
                               opt-viewer-share))))
            ;; The build daemon goes OOM on i686-linux on this phase.
            #$@(if (and (version>=? version "15.0")
                        (target-x86-32?))
                   #~((delete 'make-dynamic-linker-cache))
                   #~()))))
      (native-inputs (list python-wrapper perl))
      (inputs (list libffi))
      (propagated-inputs (list zlib))     ;to use output from llvm-config
      (home-page "https://www.llvm.org")
      (synopsis "Optimizing compiler infrastructure")
      (description
       "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.  It currently supports compilation of C and C++ programs, using
front-ends derived from GCC 4.0.1.  A new front-end for the C family of
languages is in development.  The compiler infrastructure includes mirror sets
of programming tools as well as libraries with equivalent functionality.")
      (license license:asl2.0)
      (properties `((release-monitoring-url . ,%llvm-release-monitoring-url))))))

(define-public llvm-15
  (make-llvm "15.0.7"))

(define-public llvm-14
  (package
    (inherit llvm-15)
    (version "14.0.6")
    (source (llvm-monorepo version))
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; These options are required for cross-compiling LLVM according
         ;; to <https://llvm.org/docs/HowToCrossCompileLLVM.html>.
         #$@(if (%current-target-system)
                (or (and=>
                     (system->llvm-target-arch)
                     (lambda (llvm-target-arch)
                       #~((string-append "-DLLVM_TABLEGEN="
                                         #+(file-append this-package
                                                        "/bin/llvm-tblgen"))
                          #$(string-append "-DLLVM_DEFAULT_TARGET_TRIPLE="
                                           (%current-target-system))
                          #$(string-append "-DLLVM_TARGET_ARCH=" llvm-target-arch)
                          #$(string-append "-DLLVM_TARGETS_TO_BUILD="
                                           (system->llvm-target)))))
                    (raise (condition
                            (&package-unsupported-target-error
                             (package this-package)
                             (target (%current-target-system))))))
                '())
         ;; undefined reference to `__atomic_fetch_add_8' in lib/libLLVMOrcJIT.so.14
         #$@(if (target-ppc32?)
                (list "-DCMAKE_SHARED_LINKER_FLAGS=-latomic")
                `())
         "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
         "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
         "-DBUILD_SHARED_LIBS:BOOL=TRUE"
         "-DLLVM_ENABLE_FFI:BOOL=TRUE"
         "-DLLVM_ENABLE_RTTI:BOOL=TRUE" ;for some third-party utilities
         "-DLLVM_INSTALL_UTILS=ON")     ;needed for rustc
      ;; Don't use '-g' during the build, to save space.
      #:build-type "Release"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'change-directory
            (lambda _
              (chdir "llvm")))
          (add-after 'install 'install-opt-viewer
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (opt-viewer-out (assoc-ref outputs "opt-viewer"))
                     (opt-viewer-share-dir (string-append opt-viewer-out "/share"))
                     (opt-viewer-dir (string-append opt-viewer-share-dir "/opt-viewer")))
                (mkdir-p opt-viewer-share-dir)
                (rename-file (string-append out "/share/opt-viewer")
                             opt-viewer-dir)))))))

    (native-inputs
     `(("python" ,python-wrapper)
       ("perl"   ,perl)))))

(define-public clang-runtime-15
  (clang-runtime-from-llvm llvm-15))

(define-public clang-runtime-14
  (clang-runtime-from-llvm llvm-14))

(define-public clang-15
  (clang-from-llvm
   llvm-15 clang-runtime-15
   #:tools-extra
   (origin
     (method url-fetch)
     (uri (llvm-uri "clang-tools-extra"
                    (package-version llvm-15)))
     (sha256
      (base32
       "1lagnspm5limxh1cp5jlixnzlhf09905d4rqra1kpgj6dps2x6l0")))))

(define-public clang-14
  (clang-from-llvm
   llvm-14 clang-runtime-14
   #:legacy-build-shared-libs? #t
   #:tools-extra
   (origin
     (method url-fetch)
     (uri (llvm-uri "clang-tools-extra"
                    (package-version llvm-14)))
     (sha256
      (base32
       "0rhq4wkmvr369nkk059skzzw7jx6qhzqhmiwmqg4sp66avzviwvw")))))

(define-public libomp-15
  (package
    (name "libomp")
    (version (package-version llvm-15))
    (source (llvm-monorepo version))
    (build-system cmake-build-system)
    ;; XXX: Note this gets built with GCC because building with Clang itself
    ;; fails (missing <atomic>, even when libcxx is added as an input.)
    (arguments
     (list
      #:configure-flags
      #~(list "-DLIBOMP_USE_HWLOC=ON"
              "-DOPENMP_TEST_C_COMPILER=clang"
              "-DOPENMP_TEST_CXX_COMPILER=clang++")
      #:test-target "check-libomp"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-source-and-install-license
            (lambda _
              (chdir "openmp")
              (install-file "LICENSE.TXT"
                            (string-append #$output "/share/doc")))))))
    (native-inputs (list clang-15 llvm-15 perl pkg-config python))
    (inputs (list `(,hwloc "lib")))
    (home-page "https://openmp.llvm.org")
    (synopsis "OpenMP run-time support library")
    (description "This package provides the run-time support library developed
by the LLVM project for the OpenMP multi-theaded programming extension.  This
package notably provides @file{libgomp.so}, which is has a binary interface
compatible with that of libgomp, the GNU Offloading and Multi Processing
Library.")
    (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)
                  (upstream-name . "openmp")))
    (license license:expat)))

(define-public clang-toolchain-15
  (make-clang-toolchain clang-15 libomp-15))

(define-public libomp-14
  (package
    (inherit libomp-15)
    (version (package-version llvm-14))
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "openmp" version))
              (sha256
               (base32
                "07zby3gwy5c8jssabrhjk3nsxlwipnm6sk4dsvck1l5d0br1ywsg"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libomp-15)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'chdir-to-source-and-install-license
              (lambda _
                (chdir #$(string-append "../openmp-" version ".src"))
                (install-file "LICENSE.TXT"
                              (string-append #$output "/share/doc"))))))))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-15)
       (replace "clang" clang-14)
       (replace "llvm" llvm-14)))))

(define-public clang-toolchain-14
  (make-clang-toolchain clang-14 libomp-14))

(define-public llvm-13
  (package
    (inherit llvm-14)
    (version "13.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "0d681xiixmx9inwvz14vi3xsznrcryk06a8rvk9cljiq5kc80szc"))))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-14)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (delete 'change-directory)))))
    (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)))))

(define-public clang-runtime-13
  (clang-runtime-from-llvm
   llvm-13
   "1z2xr9nn4mgc3hn9ark2k5y4wznpk47xppkp63bcbagr6589acvv"
   '("clang-runtime-13-glibc-2.36-compat.patch"
     "clang-13-remove-crypt-interceptors.patch")))

(define-public clang-13
  (clang-from-llvm llvm-13 clang-runtime-13
                   "1j8pr5kk8iqyb4jds3yl7c6x672617h4ngkpl4575j7mk4nrwykq"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-13.0-libc-search-path.patch")
                   #:tools-extra
                   (origin
                     (method url-fetch)
                     (uri (llvm-uri "clang-tools-extra"
                                    (package-version llvm-13)))
                     (sha256
                      (base32
                       "1l4jjdqfl9hrh0fwzv27hc263zc6x61h09vs4ni3yla8i1cwhayc")))))

(define-public libomp-13
  (package
    (inherit libomp-14)
    (version (package-version llvm-13))
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "openmp" version))
              (sha256
               (base32
                "0kvbr4j6ldpssiv7chgqra5y77n7jwbyxlwcl7z32v31f49jcybb"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (arguments
     '(#:configure-flags '("-DLIBOMP_USE_HWLOC=ON"
                           "-DOPENMP_TEST_C_COMPILER=clang"
                           "-DOPENMP_TEST_CXX_COMPILER=clang++"

                           ;; Work around faulty target detection, fixed in 14:
                           ;; https://github.com/llvm/llvm-project/issues/52910
                           "-DLIBOMPTARGET_BUILD_AMDGCN_BCLIB=OFF")
       #:test-target "check-libomp"))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-14)
       (replace "clang" clang-13)
       (replace "llvm" llvm-13)))))

(define-public clang-toolchain-13
  (make-clang-toolchain clang-13 libomp-13))

(define-public llvm-12
  (package
    (inherit llvm-13)
    (version "12.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "1pzx9zrmd7r3481sbhwvkms68fwhffpp4mmz45dgrkjpyl2q96kx"))))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-13)
       ((#:phases phases)
        #~(modify-phases #$phases
           #$@(if (assoc "config" (package-native-inputs this-package))
                #~((add-after 'unpack 'update-config
                     (lambda* (#:key inputs native-inputs #:allow-other-keys)
                       (let ((config.guess (search-input-file
                                             (or inputs native-inputs)
                                             "/bin/config.guess")))
                         (copy-file config.guess "cmake/config.guess")))))
                #~())
         (add-before 'build 'shared-lib-workaround
           ;; Even with CMAKE_SKIP_BUILD_RPATH=FALSE, llvm-tblgen
           ;; doesn't seem to get the correct rpath to be able to run
           ;; from the build directory.  Set LD_LIBRARY_PATH as a
           ;; workaround.
           (lambda _
             (setenv "LD_LIBRARY_PATH"
                     (string-append (getcwd) "/lib"))))))))))

(define-public clang-runtime-12
  (clang-runtime-from-llvm
   llvm-12
   "1950rg294izdwkaasi7yjrmadc9mzdd5paf0q63jjcq2m3rdbj5l"
   '("clang-runtime-13-glibc-2.36-compat.patch" "clang-runtime-12-remove-crypt-interceptors.patch")))

(define-public clang-12
  (clang-from-llvm llvm-12 clang-runtime-12
                   "0px4gl27az6cdz6adds89qzdwb1cqpjsfvrldbz9qvpmphrj34bf"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-12.0-libc-search-path.patch")
                   #:tools-extra
                   (origin
                     (method url-fetch)
                     (uri (llvm-uri "clang-tools-extra"
                                    (package-version llvm-12)))
                     (sha256
                      (base32
                       "1r9a4fdz9ci58b5z2inwvm4z4cdp6scrivnaw05dggkxz7yrwrb5")))))

(define-public libomp-12
  (package
    (inherit libomp-13)
    (version (package-version llvm-12))
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "openmp" version))
              (sha256
               (base32
                "14dh0r6h2xh747ffgnsl4z08h0ri04azi9vf79cbz7ma1r27kzk0"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (arguments
     '(#:configure-flags '("-DLIBOMP_USE_HWLOC=ON"
                           "-DOPENMP_TEST_C_COMPILER=clang"
                           "-DOPENMP_TEST_CXX_COMPILER=clang++")
       #:test-target "check-libomp"))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-13)
       (replace "clang" clang-12)
       (replace "llvm" llvm-12)))))

(define-public clang-toolchain-12
  (make-clang-toolchain clang-12 libomp-12))

(define-public llvm-11
  (package
    (inherit llvm-12)
    (version "11.1.0")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (patches (search-patches "llvm-8-missing-include.patch"))
      (sha256
       (base32
        "199yq3a214avcbi4kk2q0ajriifkvsr0l2dkx3a666m033ihi1ff"))))))

(define-public clang-runtime-11
  (clang-runtime-from-llvm
   llvm-11
   "0x1j8ngf1zj63wlnns9vlibafq48qcm72p4jpaxkmkb4qw0grwfy"
   '("clang-runtime-13-glibc-2.36-compat.patch")))

(define-public clang-11
  (clang-from-llvm llvm-11 clang-runtime-11
                   "12sm91qx2m79cvj75a9aazf2x8xybjbd593dv6v7rxficpq8i0ha"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-11.0-libc-search-path.patch")
                   #:tools-extra
                   (origin
                     (method url-fetch)
                     (uri (llvm-uri "clang-tools-extra"
                                    (package-version llvm-11)))
                     (sha256
                      (base32
                       "18n1w1hkv931xzq02b34wglbv6zd6sd0r5kb8piwvag7klj7qw3n")))))

(define-public libomp-11
  (package
    (inherit libomp-12)
    (version (package-version llvm-11))
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "openmp" version))
              (sha256
               (base32
                "0bh5cswgpc79awlq8j5i7hp355adaac7s6zaz0zwp6mkflxli1yi"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-12)
       (replace "clang" clang-11)
       (replace "llvm" llvm-11)))))

(define-public clang-toolchain-11
  (make-clang-toolchain clang-11 libomp-11))

(define-public llvm-10
  (package
    (inherit llvm-11)
    (version "10.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (patches (search-patches "llvm-8-missing-include.patch"))
      (sha256
       (base32
        "1wydhbp9kyjp5y0rc627imxgkgqiv3dfirbqil9dgpnbaw5y7n65"))))
    (native-inputs
     `(("python" ,python-wrapper)
       ("perl"   ,perl)
       ;; In llvm-11 riscv64 support was added manually to config.guess.
       ,@(if (target-riscv64?)
           `(("config" ,config))
           '())))))

(define-public clang-runtime-10
  (clang-runtime-from-llvm
   llvm-10
   "1yjqjri753w0fzmxcyz687nvd97sbc9rsqrxzpq720na47hwh3fr"
   '("clang-runtime-13-glibc-2.36-compat.patch")))

(define-public clang-10
  (clang-from-llvm llvm-10 clang-runtime-10
                   "091bvcny2lh32zy8f3m9viayyhb2zannrndni7325rl85cwgr6pr"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-10.0-libc-search-path.patch")
                   #:tools-extra
                   (origin
                     (method url-fetch)
                     (uri (llvm-uri "clang-tools-extra"
                                    (package-version llvm-10)))
                     (sha256
                      (base32
                       "06n1yp638rh24xdxv9v2df0qajxbjz4w59b7dd4ky36drwmpi4yh")))))

(define-public libomp-10
  (package
    (inherit libomp-11)
    (version (package-version llvm-10))
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "openmp" version))
              (sha256
               (base32
                "0i4bn84lkpm5w3qkpvwm5z6jdj8fynp7d3bcasa1xyq4is6757yi"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-11)
       (replace "clang" clang-10)
       (replace "llvm" llvm-10)))))

(define-public clang-toolchain-10
  (make-clang-toolchain clang-10 libomp-10))

(define-public llvm-9
  (package
    (inherit llvm-10)
    (version "9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri "llvm" version))
       (sha256
        (base32
         "16hwp3qa54c3a3v7h8nlw0fh5criqh0hlr1skybyk0cz70gyx880"))
       (patches (search-patches
                 "llvm-8-missing-include.patch"
                 "llvm-9-fix-bitcast-miscompilation.patch"
                 "llvm-9-fix-scev-miscompilation.patch"
                 "llvm-9-fix-lpad-miscompilation.patch"))))
    (arguments
     (if (target-riscv64?)
       (substitute-keyword-arguments (package-arguments llvm-10)
         ((#:phases phases)
          #~(modify-phases #$phases
             (add-after 'unpack 'patch-dsymutil-link
               (lambda _
                 (substitute* "tools/dsymutil/CMakeLists.txt"
                   (("endif\\(APPLE\\)")
                    (string-append
                      "endif(APPLE)\n\n"
                      "if (CMAKE_HOST_SYSTEM_PROCESSOR MATCHES \"riscv64\")\n"
                      "  target_link_libraries(dsymutil PRIVATE atomic)\n"
                      "endif()"))))))))
       (package-arguments llvm-10)))))

(define-public clang-runtime-9
  (clang-runtime-from-llvm
   llvm-9
   "0xwh79g3zggdabxgnd0bphry75asm1qz7mv3hcqihqwqr6aspgy2"
   '("clang-runtime-9-libsanitizer-mode-field.patch"
     "clang-runtime-9-glibc-2.36-compat.patch")))

(define-public clang-9
  (clang-from-llvm llvm-9 clang-runtime-9
                   "0ls2h3iv4finqyflyhry21qhc9cm9ga7g1zq21020p065qmm2y2p"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-9.0-libc-search-path.patch")))

(define-public libomp-9
  (package
    (inherit libomp-10)
    (version (package-version llvm-9))
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "openmp" version))
              (sha256
               (base32
                "1knafnpp0f7hylx8q20lkd6g1sf0flly572dayc5d5kghh7hd52w"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-10)
       (replace "clang" clang-9)
       (replace "llvm" llvm-9)))))

(define-public clang-toolchain-9
  (make-clang-toolchain clang-9 libomp-9))

(define-public llvm-8
  (package
    (inherit llvm-9)
    (version "8.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "llvm" version))
              (sha256
               (base32
                "1rvm5gqp5v8hfn17kqws3zhk94w4kxndal12bqa0y57p09nply24"))
              (patches (search-patches "llvm-8-fix-build-with-gcc-10.patch"
                                       "llvm-8-missing-include.patch"))))
    (license license:ncsa)))

(define-public clang-runtime-8
  (clang-runtime-from-llvm
   llvm-8
   "0dqqf8f930l8gag4d9qjgn1n0pj0nbv2anviqqhdi1rkhas8z0hi"
   '("clang-runtime-9-libsanitizer-mode-field.patch"
     "clang-runtime-9-glibc-2.36-compat.patch")))

(define-public clang-8
  (clang-from-llvm llvm-8 clang-runtime-8
                   "0ihnbdl058gvl2wdy45p5am55bq8ifx8m9mhcsgj9ax8yxlzvvvh"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-8.0-libc-search-path.patch")))

(define-public libomp-8
  (package
    (inherit libomp-9)
    (version (package-version llvm-8))
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "openmp" version))
              (sha256
               (base32
                "0b3jlxhqbpyd1nqkpxjfggm5d9va5qpyf7d4i5y7n4a1mlydv19y"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-9)
       (replace "clang" clang-8)
       (replace "llvm" llvm-8)))
    (license license:ncsa)))

(define-public clang-toolchain-8
  (make-clang-toolchain clang-8 libomp-8))

(define-public llvm-7
  (package
    (inherit llvm-8)
    (version "7.1.0")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "llvm" version))
              (sha256
               (base32
                "0r1p5didv4rkgxyvbkyz671xddg6i3dxvbpsi1xxipkla0l9pk0v"))))))

(define-public clang-runtime-7
  (clang-runtime-from-llvm
   llvm-7
   "1n48p8gjarihkws0i2bay5w9bdwyxyxxbpwyng7ba58jb30dlyq5"
   '("clang-runtime-9-libsanitizer-mode-field.patch"
     "clang-runtime-9-glibc-2.36-compat.patch")))

(define-public clang-7
  (clang-from-llvm llvm-7 clang-runtime-7
                   "0vc4i87qwxnw9lci4ayws9spakg0z6w5w670snj9f8g5m9rc8zg9"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-7.0-libc-search-path.patch")))

(define-public libomp-7
  (package
    (inherit libomp-8)
    (version (package-version llvm-7))
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "openmp" version))
              (sha256
               (base32
                "1dg53wzsci2kra8lh1y0chh60h2l8h1by93br5spzvzlxshkmrqy"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-8)
       (replace "clang" clang-7)
       (replace "llvm" llvm-7)))))

(define-public clang-toolchain-7
  (make-clang-toolchain clang-7 libomp-7))

(define-public llvm-6
  (package
    (inherit llvm-7)
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "llvm" version))
              (sha256
               (base32
                "1qpls3vk85lydi5b4axl0809fv932qgsqgdgrk098567z4jc7mmn"))))))

(define-public clang-runtime-6
  (clang-runtime-from-llvm
   llvm-6
   "1fcr3jn24yr8lh36nc0c4ikli4744i2q9m1ik67p1jymwwaixkgl"
   '("clang-runtime-9-libsanitizer-mode-field.patch"
     "clang-runtime-9-glibc-2.36-compat.patch")))

(define-public clang-6
  (clang-from-llvm llvm-6 clang-runtime-6
                   "0rxn4rh7rrnsqbdgp4gzc8ishbkryhpl1kd3mpnxzpxxhla3y93w"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-6.0-libc-search-path.patch")))

(define-public libomp-6
  (package
    (inherit libomp-7)
    (version (package-version llvm-6))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.llvm.org/"
                                  version  "/openmp-" version
                                  ".src.tar.xz"))
              (sha256
               (base32
                "0nhwfba9c351r16zgyjyfwdayr98nairky3c2f0b2lc360mwmbv6"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-7)
       (replace "clang" clang-6)
       (replace "llvm" llvm-6)))))

(define-public clang-toolchain-6
  (make-clang-toolchain clang-6 libomp-6))

(define-public llvm-3.9.1
  (package (inherit llvm-6)
    (name "llvm")
    (version "3.9.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "1vi9sf7rx1q04wj479rsvxayb6z740iaz3qniwp266fgp5a07n8z"))))
    (outputs '("out"))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'build 'shared-lib-workaround
              ;; Even with CMAKE_SKIP_BUILD_RPATH=FALSE, llvm-tblgen
              ;; doesn't seem to get the correct rpath to be able to run
              ;; from the build directory.  Set LD_LIBRARY_PATH as a
              ;; workaround.
              (lambda _
                (setenv "LD_LIBRARY_PATH"
                        (string-append (getcwd) "/lib"))))
            (delete 'install-opt-viewer)))))))

(define-public clang-runtime-3.9.1
  (clang-runtime-from-llvm
   llvm-3.9.1
   "16gc2gdmp5c800qvydrdhsp0bzb97s8wrakl6i8a4lgslnqnf2fk"
   '("clang-runtime-3.9-libsanitizer-mode-field.patch"
     "clang-runtime-asan-build-fixes.patch"
     "clang-runtime-esan-build-fixes.patch"
     "clang-3.5-libsanitizer-ustat-fix.patch")))

(define-public clang-3.9.1
  (clang-from-llvm llvm-3.9.1 clang-runtime-3.9.1
                   "0qsyyb40iwifhhlx9a3drf8z6ni6zwyk3bvh0kx2gs6yjsxwxi76"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-3.8-libc-search-path.patch")))

(define-public llvm-3.8
  (package (inherit llvm-3.9.1)
    (name "llvm")
    (version "3.8.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "1ybmnid4pw2hxn12ax5qa5kl1ldfns0njg8533y3mzslvd5cx0kf"))
      (patches (search-patches "llvm-3.x.1-fix-build-with-gcc.patch"))))))

(define-public clang-runtime-3.8
  (clang-runtime-from-llvm
   llvm-3.8
   "0p0y85c7izndbpg2l816z7z7558axq11d5pwkm4h11sdw7d13w0d"
   '("clang-runtime-asan-build-fixes.patch"
     "clang-runtime-3.8-libsanitizer-mode-field.patch"
     "clang-3.5-libsanitizer-ustat-fix.patch")))

(define-public clang-3.8
  (clang-from-llvm llvm-3.8 clang-runtime-3.8
                   "1prc72xmkgx8wrzmrr337776676nhsp1qd3mw2bvb22bzdnq7lsc"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-3.8-libc-search-path.patch")))

(define-public llvm-3.7
  (package (inherit llvm-3.8)
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri "llvm" version))
       (sha256
        (base32
         "1masakdp9g2dan1yrazg7md5am2vacbkb3nahb3dchpc1knr8xxy"))
      (patches (search-patches "llvm-3.x.1-fix-build-with-gcc.patch"))))))

(define-public clang-runtime-3.7
  (clang-runtime-from-llvm
   llvm-3.7
   "10c1mz2q4bdq9bqfgr3dirc6hz1h3sq8573srd5q5lr7m7j6jiwx"
   '("clang-runtime-asan-build-fixes.patch"
     "clang-runtime-3.8-libsanitizer-mode-field.patch"
     "clang-3.5-libsanitizer-ustat-fix.patch"
     "clang-runtime-3.7-fix-build-with-python3.patch")))

(define-public clang-3.7
  (clang-from-llvm llvm-3.7 clang-runtime-3.7
                   "0x065d0w9b51xvdjxwfzjxng0gzpbx45fgiaxpap45ragi61dqjn"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-3.5-libc-search-path.patch")))

(define-public llvm-3.6
  (package (inherit llvm-3.7)
    (version "3.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri "llvm" version))
       (patches
        (search-patches "llvm-3.6-fix-build-with-gcc-10.patch"))
       (sha256
        (base32
         "153vcvj8gvgwakzr4j0kndc0b7wn91c2g1vy2vg24s6spxcc23gn"))))))

(define-public llvm-3.5
  (package (inherit llvm-3.6)
    (version "3.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri "llvm" version))
       (patches
        (search-patches "llvm-3.5-fix-clang-build-with-gcc5.patch"))
       (sha256
        (base32
         "0xf5q17kkxsrm2gsi93h4pwlv663kji73r2g4asb97klsmb626a4"))))))

(define-public clang-runtime-3.5
  (let ((runtime (clang-runtime-from-llvm
                  llvm-3.5
                  "1hsdnzzdr5kglz6fnv3lcsjs222zjsy14y8ax9dy6zqysanplbal"
                  '("clang-runtime-asan-build-fixes.patch"
                    "clang-runtime-3.5-libsanitizer-mode-field.patch"
                    "clang-3.5-libsanitizer-ustat-fix.patch"))))
    (package/inherit runtime
      (arguments
       (substitute-keyword-arguments (package-arguments runtime)
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             ;; glibc no longer includes rpc/xdr.h, so we use the headers from
             ;; libtirpc.
             (add-after 'unpack 'find-rpc-includes
               (lambda* (#:key inputs #:allow-other-keys)
                 (setenv "CPATH"
                         (string-append
                          (search-input-directory inputs "/include/tirpc")
                          ":" (or (getenv "CPATH") "")))
                 (setenv "CPLUS_INCLUDE_PATH"
                         (string-append
                          (search-input-directory inputs "/include/tirpc")
                          ":" (or (getenv "CPLUS_INCLUDE_PATH") "")))))))))
      (inputs
       `(("libtirpc" ,libtirpc)
         ("llvm" ,llvm-3.5))))))

(define-public clang-3.5
  (clang-from-llvm llvm-3.5 clang-runtime-3.5
                   "0846h8vn3zlc00jkmvrmy88gc6ql6014c02l4jv78fpvfigmgssg"
                   #:legacy-build-shared-libs? #t
                   #:patches '("clang-3.5-libc-search-path.patch")))

(define-public llvm-16
  (make-llvm "16.0.6"))

(define-public clang-runtime-16
  (clang-runtime-from-llvm llvm-16))

(define-public clang-16
  (clang-from-llvm
   llvm-16 clang-runtime-16
   #:tools-extra
   (origin
     (method url-fetch)
     (uri (llvm-uri "clang-tools-extra"
                    (package-version llvm-16)))
     (sha256
      (base32
       "0cbgffciql06a1i0ybyyqbnkkr4g7x8cxaar5a5v3415vd27hk0p")))))

(define-public libomp-16
  (package
    (inherit libomp-15)
    (version (package-version llvm-16))
    (source (llvm-monorepo version))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-15)
       (replace "clang" clang-16)
       (replace "llvm" llvm-16)))))

(define-public clang-toolchain-16
  (make-clang-toolchain clang-16 libomp-16))

(define-public llvm-17
    (make-llvm "17.0.6"))

(define-public clang-runtime-17
  (clang-runtime-from-llvm llvm-17))

(define-public clang-17
  (clang-from-llvm
   llvm-17 clang-runtime-17
   #:tools-extra
   (origin
     (method url-fetch)
     (uri (llvm-uri "clang-tools-extra"
                    (package-version llvm-17)))
     (sha256
      (base32
       "1f8szx762c325916gjxb5lw7zxyidynwnvx6fxxqscsx8514cxxa")))))

(define-public libomp-17
  (package
    (inherit libomp-15)
    (version (package-version llvm-17))
    (source (llvm-monorepo version))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-15)
       (replace "clang" clang-17)
       (replace "llvm" llvm-17)))))

(define-public clang-toolchain-17
  (make-clang-toolchain clang-17 libomp-17))

(define-public llvm-18
  (package
    (inherit llvm-15)
    (version "18.1.8")
    (source (llvm-monorepo version))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-15)
       ((#:modules modules '((guix build cmake-build-system)
                             (guix build utils)))
        (if (%current-target-system)
            `((ice-9 regex)
              (srfi srfi-1)
              (srfi srfi-26)
              ,@modules)
            modules))
       ((#:configure-flags cf #~'())
        (if (%current-target-system)
            ;; Use a newer version of llvm-tblgen and add the new
            ;; configure-flag needed for cross-building.
            #~(cons* (string-append "-DLLVM_TABLEGEN="
                                    #+(file-append this-package
                                                   "/bin/llvm-tblgen"))
                     (string-append "-DLLVM_NATIVE_TOOL_DIR="
                                    #+(file-append this-package "/bin"))
                     (string-append "-DLLVM_HOST_TRIPLE="
                                    #$(%current-target-system))
                     (remove
                       (cut string-match
                            "-DLLVM_(DEFAULT_TARGET|TARGET_ARCH|TABLEGEN).*" <>)
                       #$cf))
            cf))
       ;; The build daemon goes OOM on i686-linux on this phase.
       ((#:phases phases #~'%standard-phases)
        (if (target-x86-32?)
            #~(modify-phases #$phases
                (delete 'make-dynamic-linker-cache))
            phases))))))

(define-public clang-runtime-18
  (clang-runtime-from-llvm llvm-18))

(define-public clang-18
  (clang-from-llvm
   llvm-18 clang-runtime-18
   #:tools-extra
   (origin
     (method url-fetch)
     (uri (llvm-uri "clang-tools-extra"
                    (package-version llvm-18)))
     (sha256
      (base32
       "1wd7y1a0db4y51swlq6dmm9hrv8pvmv158yi9f10dlayv7y7g275")))))

(define-public libomp-18
  (package
    (inherit libomp-15)
    (version (package-version llvm-18))
    (source (llvm-monorepo version))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-15)
       (replace "clang" clang-18)
       (replace "llvm" llvm-18)))))

(define-public clang-toolchain-18
  (make-clang-toolchain clang-18 libomp-18))

(define-public llvm-19
  (make-llvm "19.1.7"))

(define-public clang-runtime-19
  (clang-runtime-from-llvm llvm-19))

(define-public clang-19
  (clang-from-llvm
   llvm-19 clang-runtime-19
   #:tools-extra
   (origin
     (method url-fetch)
     (uri (llvm-uri "clang-tools-extra"
                    (package-version llvm-19)))
     (sha256
      (base32
       "1rqv769nvr07a9n1sgwbq5rm411x31kyq3ngls800m90z25hh2s3")))))

(define-public libomp-19
  (package
    (inherit libomp-15)
    (version (package-version llvm-19))
    (source (llvm-monorepo version))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-15)
       (replace "clang" clang-19)
       (replace "llvm" llvm-19)))))

(define-public clang-toolchain-19
  (make-clang-toolchain clang-19 libomp-19))

(define-public llvm-20
  (make-llvm "20.1.3"))

(define-public clang-runtime-20
  (clang-runtime-from-llvm llvm-20))

(define-public clang-20
  (clang-from-llvm
   llvm-20 clang-runtime-20
   #:tools-extra
   (origin
     (method url-fetch)
     (uri (llvm-uri "clang-tools-extra"
                    (package-version llvm-20)))
     (sha256
      (base32
       "0srv9rz420vv2li697qy7hhs3lfmx9shi79p9s1r59fh14i0yin4")))))

(define-public libomp-20
  (package
    (inherit libomp-15)
    (version (package-version llvm-20))
    (source (llvm-monorepo version))
    (native-inputs
     (modify-inputs (package-native-inputs libomp-15)
       (replace "clang" clang-20)
       (replace "llvm" llvm-20)))))

(define-public clang-toolchain-20
  (make-clang-toolchain clang-20 libomp-20))

;; Default LLVM and Clang version.
(define-public libomp libomp-13)
(define-public llvm llvm-13)
(define-public clang-runtime clang-runtime-13)
(define-public clang clang-13)
(define-public clang-toolchain clang-toolchain-13)

(define-public llvm-for-rocm
  (package
    ;; Based on LLVM 14 as of v5.0.0
    (inherit llvm-14)
    (name "llvm-for-rocm")
    (version "5.6.0")                         ;this must match '%rocm-version'
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/llvm-project.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kg6q6aqijjrwaznj0gr3nd01gykrnqqnk8vz8wyfifr18l9jrgx"))))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-14)
       ((#:configure-flags flags)
        #~(list"-DLLVM_ENABLE_PROJECTS=llvm;clang;lld"
           "-DLLVM_TARGETS_TO_BUILD=AMDGPU;X86"
           "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
           "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
           "-DBUILD_SHARED_LIBS:BOOL=TRUE"
           "-DLLVM_VERSION_SUFFIX="))))
    (properties `((hidden? . #t)
                  ,@(package-properties llvm-14)))))



(define-public libunwind-headers
  (package
    (name "libunwind-headers")
    (version "13.0.0")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "libunwind" version))
              (sha256
               (base32
                "1qb5ickp7qims5q7sxacj3fwq1kklvnl94k3v9hpl5qn284iky1n"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases (modify-phases (map (lambda (phase)
                                      (assq phase %standard-phases))
                                    '(set-paths unpack))
                  (add-after 'unpack 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (mkdir out)
                        (copy-recursively "include"
                                          (string-append out "/include"))))))))
    (home-page "https://clang.llvm.org/docs/Toolchain.html")
    (synopsis "LLVM libunwind header files")
    (description
     "This package contains header files for the LLVM C++ unwinding library.")
    (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)))
    (license license:asl2.0)))          ;with LLVM exceptions

(define-public lld-15
  (package
    (name "lld")
    (version (package-version llvm-15))
    (source (llvm-monorepo version))
    (build-system cmake-build-system)
    (inputs
     (list llvm-15))
    (arguments
     '(#:build-type "Release"
       ;; TODO: Tests require the lit tool, which isn't installed by the LLVM
       ;; package.
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'change-directory
                    (lambda _
                      (chdir "lld"))))))
    (home-page "https://lld.llvm.org/")
    (synopsis "Linker from the LLVM project")
    (description "LLD is a high-performance linker, built as a set of reusable
components which highly leverage existing libraries in the larger LLVM Project.")
    (license license:asl2.0))) ; With LLVM exception

(define-public lld-14
  (package
    (inherit lld-15)
    (version "14.0.6")
    (source (llvm-monorepo version))
    (inputs
     (list llvm-14))))

(define-public lld-13
  (package
    (inherit lld-14)
    (version "13.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "lld" version))
              (sha256
               (base32
                "1yscckcszfr234k4svhybdbsnz6w65x8pldl6c2nhyxzx12zfsk6"))))
    (native-inputs
     ;; Note: check <https://bugs.llvm.org/show_bug.cgi?id=49228> to see
     ;; whether this is still necessary.
     (list libunwind-headers))
    (inputs
     (list llvm-13))
    (arguments
     '(#:build-type "Release"
       ;; TODO: Tests require the lit tool, which isn't installed by the LLVM
       ;; package.
       #:tests? #f))
    (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)))))

(define-public lld-12
  (package
    (inherit lld-13)
    (version "12.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "lld" version))
              (sha256
               (base32
                "0qg3fgc7wj34hdkqn21y03zcmsdd01szhhm1hfki63iifrm3y2v9"))))
    (inputs (modify-inputs (package-inputs lld)
              (replace "llvm" llvm-12)))))

(define-public lld-11
  (package
    (inherit lld-12)
    (version "11.0.0")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "lld" version))
              (sha256
               (base32
                "077xyh7sij6mhp4dc4kdcmp9whrpz332fa12rwxnzp3wgd5bxrzg"))))
    (inputs (modify-inputs (package-inputs lld)
              (replace "llvm" llvm-11)))))

(define-public lld-16
  (package
    (inherit lld-15)
    (version (package-version llvm-16))
    (source (llvm-monorepo version))
    (inputs (list llvm-16))))

(define-public lld-17
  (package
    (inherit lld-15)
    (version (package-version llvm-17))
    (source (llvm-monorepo version))
    (inputs (list llvm-17))))

(define-public lld-18
  (package
    (inherit lld-15)
    (version (package-version llvm-18))
    (source (llvm-monorepo version))
    (inputs (list llvm-18))))

(define-public lld-19
  (package
    (inherit lld-15)
    (version (package-version llvm-19))
    (source (llvm-monorepo version))
    (inputs (list llvm-19))))

(define-public lld-20
  (package
    (inherit lld-15)
    (version (package-version llvm-20))
    (source (llvm-monorepo version))
    (inputs (list llvm-20))))

(define-public lld lld-14)

(define* (make-lld-wrapper lld #:key lld-as-ld?)
  "Return a LLD wrapper.  When LLD-AS-LD? is true, create a 'ld' symlink that
points to 'lld'."
  (package
    (inherit lld)
    (name (if lld-as-ld? "lld-as-ld-wrapper" "lld-wrapper"))
    (source #f)
    (native-inputs '())
    (inputs (list (make-ld-wrapper "ld.lld-wrapper" #:binutils lld
                                   #:linker "ld.lld")
                  (make-ld-wrapper "lld-wrapper" #:binutils lld #:linker
                                   "lld")))
    (propagated-inputs '())
    (build-system trivial-build-system)
    (arguments
     (list #:builder
           #~(let ((ld.lld (string-append #$(this-package-input
                                             "ld.lld-wrapper")
                                          "/bin/ld.lld"))
                   (lld (string-append #$(this-package-input "lld-wrapper")
                                       "/bin/lld")))
               (mkdir #$output)
               (mkdir (string-append #$output "/bin"))
               (symlink ld.lld (string-append #$output "/bin/ld.lld"))
               (symlink lld (string-append #$output "/bin/lld"))
               (when #$lld-as-ld?
                 (symlink ld.lld (string-append #$output "/bin/ld"))))))
    (synopsis "LLD linker wrapper")
    (description "This is a linker wrapper for LLD; like @code{ld-wrapper}, it
wraps the linker to add any missing @code{-rpath} flags, and to detect any
misuse of libraries outside of the store.")))

;;; A LLD wrapper suitable to use with -fuse-ld and GCC or with Clang.
(define-public lld-wrapper
  (make-lld-wrapper lld))

;;; A LLD wrapper that can be used as a (near) drop-in replacement to GNU ld.
(define-public lld-as-ld-wrapper-15
  (make-lld-wrapper lld-15 #:lld-as-ld? #t))

(define-public lld-as-ld-wrapper
  (make-lld-wrapper lld #:lld-as-ld? #t))

(define-public lldb
  (package
    (name "lldb")
    (version (package-version llvm-17))
    (source (llvm-monorepo version))
    (build-system cmake-build-system)
    (arguments
     (list
       #:configure-flags #~(list "-DOPENMP_TEST_CXX_COMPILER=clang++")
       #:phases
       #~(modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-source
           (lambda _
             (chdir "lldb"))))))
    (native-inputs
     (list pkg-config swig))
    (inputs
     (list clang-17
           llvm-17
           ;; Optional (but recommended) inputs.
           ncurses
           libedit
           xz
           libxml2
           lua
           python))
    (home-page "https://lldb.llvm.org/")
    (synopsis "Low level debugger")
    (description
     "LLDB is a high performance debugger built as a set of reusable components
which highly leverage existing libraries in the larger LLVM project.")
    (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)))
    (license license:asl2.0))) ;with LLVM exceptions

(define-public libcxx
  (package
    (name "libcxx")
    (version (package-version llvm-19))
    (source (llvm-monorepo version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:test-target "check-cxx"
      #:tests? #f                       ;prohibitively expensive to run
      #:implicit-inputs? #f             ;to avoid conflicting GCC headers
      #:configure-flags
      #~(list "-DLLVM_ENABLE_RUNTIMES=libcxx;libcxxabi;libunwind"
              "-DCMAKE_C_COMPILER=clang"
              "-DCMAKE_CXX_COMPILER=clang++"
              ;; libc++.so is actually a GNU ld style linker script, however,
              ;; CMake still tries to fix the RUNPATH of it during the install
              ;; step. This argument tells CMake to use the install directory
              ;; as RUNPATH and don't attempt to patch it.
              ;; See also: https://gitlab.kitware.com/cmake/cmake/-/issues/22963
              "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-subdirectory
            (lambda _
              (chdir "runtimes"))))))
    (native-inputs
     (modify-inputs (standard-packages)
       ;; Remove GCC from the build environment, to avoid its C++
       ;; headers (include/c++), which would interfere and cause build
       ;; failures.
       (delete "gcc")
       (prepend clang-19 python-minimal)))
    (home-page "https://libcxx.llvm.org")
    (synopsis "C++ standard library")
    (description
     "This package provides an implementation of the C++ standard library for
use with Clang, targeting C++11, C++14 and above.")
    (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)))
    (license license:expat)))

;; Libcxx files specifically used by PySide2.
(define-public libcxx-6
  (package
    (inherit libcxx)
    (version (package-version llvm-6))
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri "libcxx" version))
       (sha256
        (base32
         "0rzw4qvxp6qx4l4h9amrq02gp7hbg8lw4m0sy3k60f50234gnm3n"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gcc (assoc-ref inputs  "gcc")))
                ;; Hide GCC's C++ headers so that they do not interfere with
                ;; the ones we are attempting to build.
                (setenv "CPLUS_INCLUDE_PATH"
                        (string-join (delete (string-append gcc "/include/c++")
                                             (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                           #\:))
                                     ":"))
                (format #t
                        "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                        (getenv "CPLUS_INCLUDE_PATH"))
                #t))))))
    (native-inputs
     (list clang-6 llvm-6))))

(define-public libcxxabi-6
  (package
    (name "libcxxabi")
    (version "6.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/llvm/llvm-project")
             (commit (string-append "llvmorg-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ki6796b5z08kh3a3rbysr5wwb2dkl6wal5dzd03i4li5xfkvx1g"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DLIBCXXABI_LIBCXX_INCLUDES="
                             #$(this-package-native-input "libcxx")
                             "/include")
              "-DCMAKE_C_COMPILER=clang"
              "-DCMAKE_CXX_COMPILER=clang++")
      #:phases
      #~(modify-phases (@ (guix build cmake-build-system) %standard-phases)
          (add-after 'unpack 'chdir
            (lambda _ (chdir "libcxxabi")))
          (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gcc (assoc-ref inputs  "gcc")))
                ;; Hide GCC's C++ headers so that they do not interfere with
                ;; the ones we are attempting to build.
                (setenv "CPLUS_INCLUDE_PATH"
                        (string-join
                         (cons (string-append
                                (assoc-ref inputs "libcxx") "/include/c++/v1")
                               (delete (string-append gcc "/include/c++")
                                       (string-split
                                        (getenv "CPLUS_INCLUDE_PATH")
                                        #\:)))
                         ":"))
                (format
                 #true
                 "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                 (getenv "CPLUS_INCLUDE_PATH")))))
          (add-after 'install 'install-headers
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((include-dir (string-append
                                  (assoc-ref outputs "out") "/include")))
                (install-file "../libcxxabi/include/__cxxabi_config.h"
                              include-dir)
                (install-file "../libcxxabi/include/cxxabi.h"
                              include-dir)))))))
    (native-inputs
     (list clang-6 llvm-6 libcxx-6))
    (home-page "https://libcxxabi.llvm.org")
    (synopsis "C++ standard library support")
    (description
     "This package provides an implementation of low level support for a
standard C++ library.")
    (license license:expat)))

(define-public libcxx+libcxxabi-6
  (package
    (inherit libcxx-6)
    (name "libcxx+libcxxabi")
    (version (package-version libcxx-6))
    (arguments
     (list
      #:configure-flags
      #~(list "-DLIBCXX_CXX_ABI=libcxxabi"
              (string-append "-DLIBCXX_CXX_ABI_INCLUDE_PATHS="
                             #$(this-package-native-input "libcxxabi")
                             "/include"))
      #:phases
      #~(modify-phases (@ (guix build cmake-build-system) %standard-phases)
          (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gcc (assoc-ref inputs  "gcc")))
                ;; Hide GCC's C++ headers so that they do not interfere with
                ;; the ones we are attempting to build.
                (setenv "CPLUS_INCLUDE_PATH"
                        (string-join
                         (delete (string-append gcc "/include/c++")
                                 (string-split (getenv "CPLUS_INCLUDE_PATH")
                                               #\:))
                         ":"))
                (format
                 #true
                 "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                 (getenv "CPLUS_INCLUDE_PATH"))))))))
    (native-inputs
     (list clang-6 llvm-6 libcxxabi-6))))

;; WARNING: This package is a dependency of mesa.
(define-public libclc
  (package
    (name "libclc")
    (version (package-version llvm-18))
    (source (llvm-monorepo version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DLLVM_CLANG="
                             (assoc-ref %build-inputs "clang")
                             "/bin/clang")
              (string-append "-DLLVM_SPIRV="
                             (assoc-ref %build-inputs "spirv-llvm-translator")
                             "/bin/llvm-spirv"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-subdirectory
            (lambda _
              (chdir "libclc")))
          (add-after 'enter-subdirectory 'skip-clspv-tests
            (lambda _
              (substitute* "CMakeLists.txt"
                (("ptx\\.\\*") "[ptx|clspv].*")))))))
    (propagated-inputs
     (list spirv-llvm-translator spirv-tools))
    (native-inputs
     (list clang-18 llvm-18 python))
    (home-page "https://libclc.llvm.org")
    (synopsis "Libraries for the OpenCL programming language")
    (description
     "This package provides an implementation of the OpenCL library
requirements according to version 1.1 of the OpenCL specification.")
    (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)))
    ;; Apache license 2.0 with LLVM exception
    (license license:asl2.0)))

(define-public python-llvmlite
  (package
    (name "python-llvmlite")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "llvmlite" version))
       (sha256
        (base32
         "1m4lzja9xy82bhwa914p49pkbjckjc5nraspj7nsnl6ilmk7srh7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-compiler/linker-flags
            (lambda _
              (let ((llvm #$(this-package-input "llvm")))
                ;; Refer to ffi/Makefile.linux.
                (setenv "CPPFLAGS" "-fPIC")
                (setenv "LDFLAGS" (string-append "-Wl,-rpath="
                                                 llvm "/lib"))))))))
    (native-inputs (list python-setuptools python-wheel))
    (inputs
     (list
      (let* ((patches-commit
              "c52dd17d97aa5f3698cf7f8152b8f6551c10132a")
             (patch-uri (lambda (name)
                          (string-append
                           "https://raw.githubusercontent.com/numba/"
                           "llvmlite/"
                           patches-commit
                           "/conda-recipes/"
                           name)))
             (patch-origin (lambda (name hash)
                             (origin (method url-fetch)
                                     (uri (patch-uri name))
                                     (sha256 (base32 hash)))))
             (arch-independent-patches
              (list (patch-origin
                     "llvm15-clear-gotoffsetmap.patch"
                     "097iypk4l1shyrhb72msjnl7swlc78nsnb7lv507rl0vs08n6j94")
                    (patch-origin
                     "llvm15-remove-use-of-clonefile.patch"
                     "01qxzr15q3wh1ikbfi8jcs83fh27fs2w6damf7giybs6gx4iynnd")
                    (patch-origin
                     "llvm15-svml.patch"
                     "15n5vph2m7nd0jlf75n3h63h89m1kf4zn4s2jd1xmjrs848lgg87"))))
        (package
          (inherit llvm-15)
          (source
           (origin
             (inherit (package-source llvm-15))
             (patches (append arch-independent-patches
                              (origin-patches (package-source llvm-15))))))))))
    (home-page "https://llvmlite.pydata.org")
    (synopsis "Wrapper around basic LLVM functionality")
    (description
     "This package provides a Python binding to LLVM for use in Numba.")
    (license license:bsd-3)))

(define-public (clang-python-bindings clang)
  "Return a package for the Python bindings of CLANG."
  (package
    (inherit clang)
    (name "python-clang")
    (build-system python-build-system)
    (outputs '("out"))
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'change-directory
                     (lambda _
                       (chdir "bindings/python")))
                   (add-before 'build 'create-setup-py
                     (lambda _
                       ;; Generate a basic "setup.py", enough so it can be
                       ;; built and installed.
                       (with-output-to-file "setup.py"
                         (lambda ()
                           (format #true "from setuptools import setup
setup(name=\"clang\", version=\"~a\", packages=[\"clang\"])\n"
                                   #$(package-version this-package))))))
                   (add-before 'build 'set-libclang-file-name
                     (lambda* (#:key inputs #:allow-other-keys)
                       ;; Record the absolute file name of libclang.so.
                       (let ((libclang (search-input-file inputs
                                                          "/lib/libclang.so")))
                         (substitute* "clang/cindex.py"
                           (("libclang\\.so") libclang))))))))
    (inputs (list clang))
    (synopsis "Python bindings to libclang")))

(define-public python-clang-10
  (clang-python-bindings clang-10))

(define-public python-clang-11
  (clang-python-bindings clang-11))

(define-public python-clang-12
  (clang-python-bindings clang-12))

(define-public python-clang-13
  (clang-python-bindings clang-13))

(define-public emacs-clang-format
  (package
    (inherit clang)
    (name "emacs-clang-format")
    (build-system emacs-build-system)
    (inputs
     (list clang))
    (propagated-inputs '())
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang-format (search-input-file inputs "/bin/clang-format")))
               (copy-file "tools/clang-format/clang-format.el" "clang-format.el")
               (emacs-substitute-variables "clang-format.el"
                 ("clang-format-executable"
                  clang-format))))))))
    (synopsis "Format code using clang-format")
    (description "This package filters code through @code{clang-format}
to fix its formatting.  @code{clang-format} is a tool that formats
C/C++/Obj-C code according to a set of style options, see
@url{https://clang.llvm.org/docs/ClangFormatStyleOptions.html}.")))

(define-public emacs-clang-rename
  (package
    (inherit clang)
    (name "emacs-clang-rename")
    (build-system emacs-build-system)
    (inputs
     (list clang))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang-rename (search-input-file inputs "/bin/clang-rename")))
               (copy-file "tools/clang-rename/clang-rename.el" "clang-rename.el")
               (emacs-substitute-variables "clang-rename.el"
                 ("clang-rename-binary"
                  clang-rename))))))))
    (synopsis "Rename every occurrence of a symbol using clang-rename")
    (description "This package renames every occurrence of a symbol at point
using @code{clang-rename}.")))


;;;
;;; LLVM variants.
;;;

(define-public llvm-for-mesa
  ;; Note: update the 'clang' input of mesa-opencl when bumping this.
  (let ((base-llvm llvm-18))
    (package
      (inherit base-llvm)
      (name "llvm-for-mesa")
      (arguments
       (substitute-keyword-arguments (package-arguments base-llvm)
         ((#:modules modules '((guix build cmake-build-system)
                               (guix build utils)))
          `((ice-9 regex)
            (srfi srfi-1)
            (srfi srfi-26)
            ,@modules))
         ((#:configure-flags cf ''())
          #~(cons*
             #$@(if (%current-target-system)
                    '("-DBUILD_SHARED_LIBS:BOOL=TRUE"
                      "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE")
                    '())
             ;; Skipping tools and utils decreases the output by ~100 MiB.
             "-DLLVM_BUILD_TOOLS=NO"
             (remove
              (cut string-match
                   #$(if (%current-target-system)
                         "-DLLVM_(LINK_LLVM_DYLIB|INSTALL_UTILS).*"
                         "-DLLVM_INSTALL_UTILS.*") <>)
              #$cf)))
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              #$@(if (%current-target-system)
                     '()
                     #~((add-after 'install 'delete-static-libraries
                          ;; If these are just relocated then llvm-config
                          ;; can't find them.
                          (lambda* (#:key outputs #:allow-other-keys)
                            (for-each delete-file
                                      (find-files
                                       (string-append
                                        (assoc-ref outputs "out") "/lib")
                                       "\\.a$"))))))
              ;; llvm-config is how mesa and others find the various
              ;; libraries and headers they use.
              (add-after 'install 'build-and-install-llvm-config
                (lambda* (#:key outputs #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    (substitute*
                      "tools/llvm-config/CMakeFiles/llvm-config.dir/link.txt"
                      (((string-append (getcwd) "/build/lib"))
                       (string-append out "/lib")))
                    (invoke "make" "llvm-config")
                    (install-file "bin/llvm-config"
                                  (string-append out "/bin"))))))))))))

(define make-ocaml-llvm
  ;; Make it a memoizing procedure so its callers below don't end up defining
  ;; two equal-but-not-eq "ocaml-llvm" packages for the default LLVM.
  (mlambdaq (llvm)
    (package
      (inherit llvm)
      (name "ocaml-llvm")
      (outputs '("out"))
      (arguments
       `(#:configure-flags
         (list
          (string-append "-DLLVM_OCAML_EXTERNAL_LLVM_LIBDIR="
                         (assoc-ref %build-inputs "llvm") "/lib")
          "-DBUILD_SHARED_LIBS=TRUE"
          "-DLLVM_OCAML_OUT_OF_TREE=TRUE"
          (string-append "-DLLVM_OCAML_INSTALL_PATH="
                         (assoc-ref %outputs "out") "/lib/ocaml/site-lib"))
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda _
               (invoke "make" "ocaml_all")))
           (replace 'install
             (lambda _
               (invoke "cmake" "-P" "bindings/ocaml/cmake_install.cmake"))))))
      (inputs
       (list llvm))
      (native-inputs
       (list ocaml ocaml-findlib ocaml-ounit python))
      (propagated-inputs
       (list ocaml-integers ocaml-ctypes))
      (synopsis "OCaml bindings to LLVM")
      (description "This package contains the OCaml bindings distributed with
LLVM."))))

(define-public ocaml-llvm (make-ocaml-llvm llvm))
(define-public ocaml-llvm-9 (make-ocaml-llvm llvm-9))
(define-public ocaml-llvm-10 (make-ocaml-llvm llvm-10))
(define-public ocaml-llvm-11 (make-ocaml-llvm llvm-11))

(define-public wllvm
  (package
    (name "wllvm")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wllvm" version))
       (sha256
        (base32 "0cf31hixzq5bzkxv91rvadlhrpxzy934134scv4frj85bxbpl19y"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/SRI-CSL/whole-program-llvm")
    (synopsis "Whole Program LLVM")
    (description "This package provides a toolkit for building whole-program
LLVM bitcode files.")
    (license license:expat)))

(define-public llvm-julia
  (package
    (inherit llvm-13)
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-13)
       ((#:configure-flags flags ''())
        #~(cons* "-DLLVM_BUILD_LLVM_DYLIB=ON"
                 "-DLLVM_LINK_LLVM_DYLIB=ON"
                 ;; "-DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=NVPTX"
                 "-DLLVM_VERSION_SUFFIX:STRING=jl"  ; Perhaps not needed.
                 #$(string-append "-DLLVM_TARGETS_TO_BUILD="
                                  (system->llvm-target))
                 (delete "-DBUILD_SHARED_LIBS:BOOL=TRUE" #$flags)))
       ((#:build-type _) "Release")))
    (properties `((hidden? . #t)
                  ,@(package-properties llvm-13)))))

(define llvm-cling-base llvm-16)

(define llvm-cling
  (let ((base llvm-cling-base))
    (package/inherit base
      (name "llvm-cling")
      ;; Use the latest tag for the major LLVM version currently targeted by
      ;; Cling (often mentioned in Cling's release notes).
      (version "16-20240621-02")
      (source
       (origin
         (inherit (package-source base))
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/root-project/llvm-project")
               (commit (string-append "cling-llvm" version))))
         (file-name (git-file-name "llvm-cling" version))
         (sha256
          (base32
           "05libb4mc385n8sq0bilalvidwzzrcyiqsfkn7j179kkx66a8rzy"))))
      (arguments
       ;; This reduces the package size on disk from 547 MiB to 311 MiB.
       ;; Cling is intended to be used as a REPL on the host machine, not as a
       ;; cross-compiling toolchain.
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags cf ''())
          #~(cons* "-DLLVM_TARGETS_TO_BUILD=host;NVPTX" #$cf)))))))

(define clang-cling-runtime
  (let ((base clang-runtime-16))
    (package/inherit base
      (name "clang-cling-runtime")
      (version (package-version llvm-cling))
      (source (package-source llvm-cling))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'install 'delete-static-libraries
                ;; This reduces the size from 22 MiB to 4 MiB.
                (lambda _
                  (for-each delete-file (find-files #$output "\\.a$"))))))))
      (inputs (modify-inputs (package-inputs base)
                (replace "llvm" llvm-cling))))))

(define clang-cling
  (let ((base clang-16))
    (package/inherit base
      (name "clang-cling")
      (version (package-version llvm-cling))
      (source (package-source llvm-cling))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (replace "llvm" llvm-cling)
         (replace "clang-runtime" clang-cling-runtime))))))

(define-public cling
  (package
    (name "cling")
    (version "1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/root-project/cling")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13ghbqjppvbmkhjgfk9xggxh17xpmx18ghdqgkkg9a3mh19hf69h"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"            ;keep the build as lean as possible
      ;; FIXME: 79 tests fail, out of ~200 (see:
      ;; https://github.com/root-project/cling/issues/534)
      #:tests? #f
      #:test-target "check-cling"
      #:configure-flags
      #~(list (string-append "-DCLING_CXX_PATH="
                             (search-input-file %build-inputs "bin/g++"))
              ;; XXX: The AddLLVM.cmake module expects LLVM_EXTERNAL_LIT to
              ;; be a Python script, not a shell executable.
              (string-append "-DLLVM_EXTERNAL_LIT="
                             (search-input-file %build-inputs "bin/.lit-real")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              (make-file-writable "VERSION")
              (call-with-output-file "VERSION"
                (lambda (port)
                  (format port "~a~%" #$version)))))
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "lib/Interpreter/CIFactory.cpp"
                (("\\bsed\\b")
                 (which "sed"))
                ;; Cling uses libclang's CompilerInvocation::GetResourcesPath
                ;; to resolve Clang's library prefix, but this fails on Guix
                ;; because it is relative to the output of cling rather than
                ;; clang (see:
                ;; https://github.com/root-project/cling/issues/434).  Fully
                ;; shortcut the logic in this method to return the correct
                ;; static location.
                (("static std::string getResourceDir.*" all)
                 (string-append all
                                "    return std::string(\""
                                #$(this-package-input "clang-cling")
                                "/lib/clang/"
                                #$(first
                                   (take (string-split
                                          (package-version clang-cling) #\-)
                                         1)) ;e.g. 16.0.6 -> 16
                                "\");")))
              ;; Check for the 'lit' command for the tests, not 'lit.py'
              ;; (see: https://github.com/root-project/cling/issues/432).
              (substitute* "CMakeLists.txt"
                (("lit.py")
                 "lit"))))
          (add-after 'unpack 'adjust-lit.cfg
            ;; See: https://github.com/root-project/cling/issues/435.
            (lambda _
              (substitute* "test/lit.cfg"
                (("config.llvm_tools_dir \\+ '")
                 "config.cling_obj_root + '/bin"))))
          (add-before 'check 'set-CLANG
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              ;; Otherwise, lit fails with "fatal: couldn't find 'clang'
              ;; program, try setting CLANG in your environment".
              (setenv "CLANG" (search-input-file (or native-inputs inputs)
                                                 "bin/clang"))))
          (add-after 'install 'delete-static-libraries
            ;; This reduces the size from 17 MiB to 5.4 MiB.
            (lambda _
              (for-each delete-file (find-files #$output "\\.a$"))))
          (add-after 'install 'wrap-with-include-paths
            ;; Cling is sensitive to miss-matched include directives; ensure
            ;; the GCC includes used match that of the GCC used to build
            ;; Cling.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gcc-toolchain #$(this-package-input "gcc-toolchain")))
                (wrap-program (string-append #$output "/bin/cling")
                  `("C_INCLUDE_PATH" prefix
                   (,(string-append gcc-toolchain "/include")))
                  `("CPLUS_INCLUDE_PATH" prefix
                    (,(string-append gcc-toolchain "/include/c++")
                     ,(string-append gcc-toolchain "/include")))))))
          (add-after 'wrap-with-include-paths 'fix-wrapper
            (lambda _
              ;; When -a $0 is used, the cling executable segfauts (see:
              ;; https://issues.guix.gnu.org/73405).
              (substitute* (string-append #$output "/bin/cling")
                (("\"\\$0\"")
                 "\"${0##*/}\"")))))))
    (native-inputs (list clang-cling python python-lit))
    (inputs (list clang-cling (force gcc-toolchain*) llvm-cling libxcrypt))
    (home-page "https://root.cern/cling/")
    (synopsis "Interactive C++ interpreter")
    (description "Cling is an interactive C++17 standard compliant
interpreter, built on top of LLVM and Clang.  Cling can be used as a
read-eval-print loop (REPL) to assist with rapid application development.
Here's how to print @samp{\"Hello World!\"} using @command{cling}:

@example
cling '#include <stdio.h>' 'printf(\"Hello World!\\n\");'
@end example")
    (license license:lgpl2.1+)))     ;for the combined work
