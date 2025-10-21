;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu packages swift)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml))

(define %swift-bootstrap-version "5.7.3")

(define %swift-bootstrap-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/apple/swift.git")
          (commit (string-append "swift-" %swift-bootstrap-version
                                 "-RELEASE"))))
    (file-name (git-file-name "swift" %swift-bootstrap-version))
    (sha256
     (base32
      "012m91yp2d69l0k6s0gjz6gckxq4hvid197a8kpc5mi9wbchzjvs"))
    (patches (search-patches "swift-5.7.3-sdk-path.patch"
                             "swift-5.7.3-sourcekit-rpath.patch"))))

(define %swift-6.2-version "6.2")

(define %swift-6.2-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/apple/swift.git")
          (commit (string-append "swift-" %swift-6.2-version
                                 "-RELEASE"))))
    (file-name (git-file-name "swift" %swift-6.2-version))
    (sha256
     (base32
      "1615yxdjlglfq0skrj0kfxzlp6riig8nkn07qddh2r89whj3gv2g"))
    (patches (search-patches "swift-5.7.3-sdk-path.patch"
                             "swift-5.7.3-sourcekit-rpath.patch"
                             "swift-6.2-cplus-include-path.patch"
                             "swift-6.2-exclude-scan-test.patch"))))

(define-public swift-cmark
  (package
    (name "swift-cmark")
    (version %swift-bootstrap-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apple/swift-cmark.git")
                    (commit (string-append "swift-" %swift-bootstrap-version
                                           "-RELEASE"))))
              (file-name (git-file-name "swift-cmark" %swift-bootstrap-version))
              (sha256
               (base32
                "0340j9x2n40yx61ma2pgqfbn3a9ijrh20iwzd1zxqq87rr76hh3z"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:build-type "Release"
      #:configure-flags
      #~(list (string-append "-DCMAKE_INSTALL_PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-cmake-exports
            (lambda _
              (install-file "src/cmarkTargets.cmake"
                            (string-append #$output "/src"))
              (substitute* (string-append #$output "/src/cmarkTargets.cmake")
                (("/tmp/guix-build-swift-cmark-[^/]+/source/src")
                 #$output)
                (("/tmp/guix-build-swift-cmark-[^/]+/build/src")
                 (string-append #$output "/lib")))
              #t)))))
    (native-inputs
     (list cmake ninja python-3))
    (home-page "https://swift.org/")
    (synopsis "CommonMark parsing and rendering library for Swift")
    (description
     "This is Apple's fork of cmark (CommonMark implementation) with
Swift-specific modifications, required to build Swift 4.2.4.")
    (license license:bsd-2)))

(define-public swift-cmark-6.2
  (package
    (inherit swift-cmark)
    (version %swift-6.2-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apple/swift-cmark.git")
                    (commit (string-append "swift-" %swift-6.2-version
                                           "-RELEASE"))))
              (file-name (git-file-name "swift-cmark" %swift-6.2-version))
              (sha256
               (base32
                "1405irbglx933i6jc8546gcrgb3y3703h66jm1jnd6acgfyg74ly"))))
    (arguments
     (substitute-keyword-arguments (package-arguments swift-cmark)
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'install-cmake-exports)))))))

(define %swift-libdispatch-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/apple/swift-corelibs-libdispatch.git")
          (commit (string-append "swift-" %swift-bootstrap-version
                                 "-RELEASE"))))
    (file-name "swift-corelibs-libdispatch")
    (sha256
     (base32
      "0skg1azbhbg7y0ql2a5sx6lmfip8l1rajqm95zzf9xv45n4dg9nn"))
    (patches (search-patches "swift-corelibs-libdispatch-5.6.3-lock-cpp.patch"
                             "swift-corelibs-libdispatch-5.7.3-modulemap.patch"))))

(define %swift-libdispatch-6.2-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/apple/swift-corelibs-libdispatch.git")
          (commit (string-append "swift-" %swift-6.2-version
                                 "-RELEASE"))))
    (file-name "swift-corelibs-libdispatch")
    (sha256
     (base32
      "1nrm69zwf7i5rxgc9gzdknl6p9aggfnzcrydh1qsvqhga3s8dvaf"))
    (patches (search-patches "swift-corelibs-libdispatch-5.6.3-lock-cpp.patch"))))

(define %swift-syntax-6.2-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/swiftlang/swift-syntax.git")
          (commit (string-append "swift-" %swift-6.2-version
                                 "-RELEASE"))))
    (file-name "swift-syntax")
    (sha256
     (base32
      "17z2c7kign0cjvnsm2m75c4nsjr3wbxzlzybwb5pnpxnbvmmyxf9"))))

(define-public swift-bootstrap
  (package
    (name "swift-bootstrap")
    (version %swift-bootstrap-version)
    (source %swift-bootstrap-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; we don't have the compatible (old) googletest
      #:modules '((ice-9 match)
                  (guix build cmake-build-system)
                  (guix build utils))
      #:configure-flags
      #~(list "-GNinja"
              "-DCMAKE_BUILD_TYPE=Release"
              ;"-DCMAKE_BUILD_WITH_INSTALL_RPATH=ON"
              "-DCMAKE_C_FLAGS=-Wno-unknown-warning-option -Werror=unguarded-availability-new -fno-stack-protector"
              "-DCMAKE_CXX_FLAGS=-Wno-unknown-warning-option -Werror=unguarded-availability-new -fno-stack-protector"
              "-DCMAKE_C_FLAGS_RELWITHDEBINFO=-O2 -DNDEBUG"
              "-DCMAKE_CXX_FLAGS_RELWITHDEBINFO=-O2 -DNDEBUG"
              (string-append "-DCMAKE_C_COMPILER="
                             (assoc-ref %build-inputs "swift-llvm")
                             "/bin/clang")
              (string-append "-DCMAKE_CXX_COMPILER="
                             (assoc-ref %build-inputs "swift-llvm")
                             "/bin/clang++")
              "-DSWIFT_STDLIB_BUILD_TYPE=Release"
              "-DSWIFT_STDLIB_ASSERTIONS=TRUE"
              "-DSWIFT_ENABLE_DISPATCH=TRUE"
              "-DSWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS=TRUE"
              "-DLLVM_ENABLE_ASSERTIONS=TRUE"
              ;; Python swift.py product class (not build-script-impl).
              "-DCMAKE_EXPORT_COMPILE_COMMANDS=TRUE"
              ;; Python swift.py product class (not build-script-impl).
              "-DSWIFT_FORCE_OPTIMIZED_TYPECHECKER=FALSE"
              ;; Python swift.py product class (not build-script-impl).
              "-DSWIFT_STDLIB_ENABLE_STDLIBCORE_EXCLUSIVITY_CHECKING=FALSE"
              ;; The build-script wrapper passes --swift-analyze-code-coverage false
              ;; to build-script-impl.
              "-DSWIFT_ANALYZE_CODE_COVERAGE=FALSE"
              "-DSWIFT_INCLUDE_TOOLS=TRUE"
              "-DSWIFT_BUILD_REMOTE_MIRROR=TRUE"
              "-DSWIFT_STDLIB_SIL_DEBUGGING=FALSE"
              "-DSWIFT_CHECK_INCREMENTAL_COMPILATION=FALSE"
              "-DSWIFT_REPORT_STATISTICS=FALSE"
              (string-append "-DSWIFT_NATIVE_LLVM_TOOLS_PATH="
                             (assoc-ref %build-inputs "swift-llvm")
                             "/bin")
              ;(string-append "-DSWIFT_NATIVE_CLANG_TOOLS_PATH="
              ;               (assoc-ref %build-inputs "swift-clang")
              ;               "/bin")
              "-DSWIFT_BUILD_DYNAMIC_STDLIB=TRUE"
              "-DSWIFT_BUILD_STATIC_STDLIB=FALSE"
              "-DSWIFT_BUILD_DYNAMIC_SDK_OVERLAY=TRUE"
              "-DSWIFT_BUILD_STATIC_SDK_OVERLAY=FALSE"
              ; not there "-DSWIFT_BUILD_EXAMPLES=TRUE"
              "-DSWIFT_INCLUDE_TESTS=FALSE"
              "-DSWIFT_INCLUDE_TEST_BINARIES=FALSE"
              "-DSWIFT_INSTALL_EXCLUDE_TESTSUITE_TOOLS=TRUE"
              "-DSWIFT_EMBED_BITCODE_SECTION=FALSE"
              "-DSWIFT_TOOLS_ENABLE_LTO="
              "-DSWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER=FALSE"
              ; not there "-DLIBDISPATCH_CMAKE_BUILD_TYPE=Release"
              "-DSWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING="
              ; not there "-DSWIFT_BUILD_PERF_TESTSUITE:BOOL=TRUE"
              ; not there "-DSWIFT_BUILD_EXTERNAL_PERF_TESTSUITE:BOOL=FALSE"
              "-DLLVM_LIT_ARGS=-sv -j 16"
              "-DCOVERAGE_DB="
              ; not there "-DSWIFT_SOURCEKIT_USE_INPROC_LIBRARY:BOOL=TRUE"
              "-DSWIFT_DARWIN_XCRUN_TOOLCHAIN:STRING=default"
              "-DSWIFT_AST_VERIFIER:BOOL=TRUE"
              "-DSWIFT_SIL_VERIFY_ALL:BOOL=FALSE"
              "-DSWIFT_RUNTIME_ENABLE_LEAK_CHECKER:BOOL=FALSE"
              "-DSWIFT_SDKS:STRING=LINUX"
              (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
              (string-append "-DCMAKE_INSTALL_RPATH=" #$output "/lib/swift/linux;" #$output "/lib")
              (string-append "-DSWIFT_PATH_TO_CLANG_BUILD="
                             (assoc-ref %build-inputs "swift-llvm"))
              (string-append "-DLLVM_BUILD_LIBRARY_DIR="
                             (assoc-ref %build-inputs "swift-llvm")
                             "/lib")
              (string-append "-DLLVM_BUILD_MAIN_INCLUDE_DIR="
                             (assoc-ref %build-inputs "swift-llvm")
                             "/include")
              (string-append "-DLLVM_BUILD_BINARY_DIR="
                             (assoc-ref %build-inputs "swift-llvm"))
              (string-append "-DLLVM_BUILD_MAIN_SRC_DIR="
                             (assoc-ref %build-inputs "swift-llvm"))
              (string-append "-DSWIFT_PATH_TO_CMARK_BUILD="
                             (assoc-ref %build-inputs "swift-cmark"))
              (string-append "-DSWIFT_CMARK_LIBRARY_DIR="
                             (assoc-ref %build-inputs "swift-cmark")
                             "/lib")
              (string-append "-DSWIFT_PATH_TO_LIBDISPATCH_SOURCE="
                             (assoc-ref %build-inputs "swift-corelibs-libdispatch"))
              (string-append "-DLLVM_DIR="
                             (assoc-ref %build-inputs "swift-llvm")
                             "/lib/cmake/llvm")
              (string-append "-DClang_DIR="
                             (assoc-ref %build-inputs "swift-llvm")
                             "/lib/cmake/clang")
              ;; Disable gold linker - ld.gold would bypass Guix's ld-wrapper
              ;; which automatically adds -rpath flags for store libraries.
              "-DSWIFT_USE_LINKER=")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (let ((platform (match #$(or (%current-target-system)
                                             (%current-system))
                                  ("x86_64-linux" "linux-x86_64")
                                  ("aarch64-linux" "linux-aarch64")
                                  ("i686-linux" "linux-i686")
                                  (system (error "Unsupported system" system)))))
                  (invoke "ninja" (string-append "check-swift-" platform))))))
          (add-after 'install 'install-dispatch-libs
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib-dir (string-append out "/lib/swift/linux")))
                (install-file "lib/swift/linux/libdispatch.so" lib-dir)
                (install-file "lib/swift/linux/libBlocksRuntime.so" lib-dir))))
          (add-after 'unpack 'setup
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The BFD linker has issues wrt relocation of the protocol
              ;; conformance section on arm and thumb targets.
              ;;
              ;; It also generates COPY relocations for final executables.
              ;; The BFD linker has issues wrt relocations against protected
              ;; symbols.
              ;;
              ;; Nevertheless, we use BFD linker for easier bootstrapping
              ;; (since we use ld-wrapper to wrap the BFD linker).
              (substitute* "lib/Driver/UnixToolChains.cpp"
               (("return \"gold\";")
                "return \"\";"))
              ;; New gcc needs explicit #include <cstdint>.
              (substitute* "include/swift/SIL/SILLinkage.h"
                (("#include \"llvm/Support/ErrorHandling.h\"")
                 "#include <cstdint>\n#include \"llvm/Support/ErrorHandling.h\""))
              ;; New gcc needs explicit #include <cstdint>.
              (substitute* "include/swift/Basic/ExternalUnion.h"
                (("#include \"llvm/Support/ErrorHandling.h\"")
                 "#include <cstdint>\n#include \"llvm/Support/ErrorHandling.h\""))
              ;; Take clang from swift-llvm package (clang is built alongside llvm).
              (substitute* "stdlib/CMakeLists.txt"
                (("set\\(CMAKE_CXX_COMPILER \"\\$\\{SWIFT_NATIVE_LLVM_TOOLS_PATH\\}/clang\\+\\+\"\\)")
                 (string-append "set(CMAKE_CXX_COMPILER \""
                                (assoc-ref %build-inputs "swift-llvm")
                                "/bin/clang++\")"))
                (("set\\(CMAKE_C_COMPILER \"\\$\\{SWIFT_NATIVE_LLVM_TOOLS_PATH\\}/clang\"\\)")
                 (string-append "set(CMAKE_C_COMPILER \""
                                (assoc-ref %build-inputs "swift-llvm")
                                "/bin/clang\")")))
              ;; Fix hardcoded /usr/include path for glibc headers.
              (substitute* "stdlib/public/Platform/CMakeLists.txt"
                (("set\\(GLIBC_SYSROOT_RELATIVE_INCLUDE_PATH \"/usr/include\"\\)")
                 (string-append "set(GLIBC_SYSROOT_RELATIVE_INCLUDE_PATH \""
                                (assoc-ref %build-inputs "glibc")
                                "/include\")")))
              ;; Fix GenerateVersionFromVCS.cmake path
              (substitute* "lib/Basic/CMakeLists.txt"
                (("\\$\\{LLVM_MAIN_SRC_DIR\\}/cmake/modules/GenerateVersionFromVCS.cmake")
                 "${LLVM_CMAKE_DIR}/GenerateVersionFromVCS.cmake"))
              ;; Fix features.json path - use installed location instead of source
              (substitute* "lib/Option/CMakeLists.txt"
                (("\\$\\{LLVM_MAIN_SRC_DIR\\}/\\.\\./clang/tools/driver/features\\.json")
                 "${LLVM_BUILD_BINARY_DIR}/share/clang/features.json"))
              ;; cmarkTargets.cmake is already at the correct path via install-cmake-exports phase
              ;;; clang is built as part of swift-llvm.
              (substitute* "lib/IRGen/CMakeLists.txt"
                (("    clangCodeGen")
                 "    ${SWIFT_PATH_TO_CLANG_BUILD}/lib/libclangCodeGen.a")
                (("    clangAST")
                 "    ${SWIFT_PATH_TO_CLANG_BUILD}/lib/libclangAST.a"))
              (substitute* "lib/Markup/CMakeLists.txt"
                (("    libcmark_static")
                 "    ${SWIFT_PATH_TO_CMARK_BUILD}/lib/libcmark.a"))
              (substitute* "lib/AST/CMakeLists.txt"
                (("    clangAPINotes")
                 "    ${SWIFT_PATH_TO_CLANG_BUILD}/lib/libclangAPINotes.a")
                (("    clangBasic")
                 "    ${SWIFT_PATH_TO_CLANG_BUILD}/lib/libclangBasic.a"))
              (substitute* "lib/FrontendTool/CMakeLists.txt"
                (("    clangAPINotes")
                 "    ${SWIFT_PATH_TO_CLANG_BUILD}/lib/libclangAPINotes.a")
                (("    clangBasic")
                 "    ${SWIFT_PATH_TO_CLANG_BUILD}/lib/libclangBasic.a")))))))
    (native-inputs
     (list cmake
           ninja
           perl
           pkg-config
           python-3
           swift-cmark
           %swift-libdispatch-source))
    (inputs
     (list glibc icu4c libedit libxml2 swift-llvm `(,util-linux "lib")))
    (home-page "https://swift.org/")
    (synopsis "Swift programming language (bootstrap from C++)")
    (description
     "Swift is a general-purpose programming language built using a modern
approach to safety, performance, and software design patterns.  This package
provides a bootstrap build of Swift 4.2.4 compiled from C++ without requiring
a previous Swift compiler.")
    (license license:asl2.0)))

(define-public swift-6.2
  (package
    (inherit swift-bootstrap)
    (name "swift")
    (version %swift-6.2-version)
    (source %swift-6.2-source)
    (arguments
     (substitute-keyword-arguments (package-arguments swift-bootstrap)
       ((#:configure-flags flags)
        #~(append (cons (string-append "-DCMAKE_INSTALL_RPATH="
                                       #$output "/lib/swift/linux:"
                                       #$output "/lib/swift/host/compiler:"
                                       #$output "/lib")
                        (filter (lambda (flag)
                                  (not (string-prefix? "-DCMAKE_INSTALL_RPATH=" flag)))
                                #$flags))
                  (list "-DSWIFT_BUILD_SWIFT_SYNTAX=TRUE"
                        (string-append "-DSWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE="
                                       (assoc-ref %build-inputs "swift-syntax"))
                        "-DSWIFT_MODULE_CACHE_PATH=/tmp/module-cache"
                        "-DCMAKE_Swift_FLAGS=-module-cache-path /tmp/module-cache"
                        (string-append "-DCMAKE_Swift_COMPILER="
                                       (assoc-ref %build-inputs "swift-bootstrap")
                                       "/bin/swiftc")
                        "-DBOOTSTRAPPING_MODE=BOOTSTRAPPING"
                        ;; Disable Swift-in-Swift compiler components because Swift 6.2's
                        ;; CMake forces BOOTSTRAPPING_MODE=HOSTTOOLS when
                        ;; SWIFT_ENABLE_SWIFT_IN_SWIFT is enabled on Linux, which requires
                        ;; Swift 5.8+. We only have Swift 5.7.3 for bootstrap.
                        ;;
                        ;; What we lose by disabling SWIFT_ENABLE_SWIFT_IN_SWIFT:
                        ;;
                        ;; 1. 48 Swift-based optimizer passes:
                        ;;    - CopyToBorrowOptimization, AssumeSingleThreaded,
                        ;;      BooleanLiteralFolding, DestroyHoisting,
                        ;;      ComputeEscapeEffects, ComputeSideEffects,
                        ;;      DiagnoseInfiniteRecursion, InitializeStaticGlobals,
                        ;;      MandatoryRedundantLoadElimination,
                        ;;      EarlyRedundantLoadElimination, RedundantLoadElimination,
                        ;;      DeadStoreElimination, LifetimeDependenceDiagnostics,
                        ;;      LifetimeDependenceInsertion, LifetimeDependenceScopeFixup,
                        ;;      MergeCondFails, ObjCBridgingOptimization, ObjectOutliner,
                        ;;      DeinitDevirtualizer, ReleaseDevirtualizer,
                        ;;      LetPropertyLowering, FunctionStackProtection, Simplification,
                        ;;      OnoneSimplification, LateOnoneSimplification,
                        ;;      CleanupDebugSteps, NamedReturnValueOptimization,
                        ;;      StripObjectHeaders, StackPromotion, UpdateBorrowedFrom,
                        ;;      ExperimentalSwiftBasedClosureSpecialization,
                        ;;      AutodiffClosureSpecialization, AsyncDemotion,
                        ;;      MandatoryPerformanceOptimizations, ReadOnlyGlobalVariablesPass,
                        ;;      StackProtection, DiagnoseUnknownConstValues,
                        ;;      and various dumper/test passes
                        ;;
                        ;; 2. 2 C++ optimizer passes that are disabled without Swift-in-Swift
                        ;;    (they cause verification failures in C++-only mode):
                        ;;    - LoopRotate
                        ;;    - SimplifyCFG
                        ;;
                        ;; 3. Swift macro implementations (@OptionSet, @DebugDescription,
                        ;;    @TaskLocal, @Swiftify, @DistributedResolvable)
                        ;;    The stdlib has fallbacks with #if hasFeature(Macros), so it
                        ;;    builds without them.
                        ;;
                        ;; The compiler and stdlib will still build and work for all language
                        ;; features. Core functionality (parsing, type checking, SIL
                        ;; generation, code generation) is unaffected. However, generated code
                        ;; will be less optimized (159 C++ optimizer passes still work).
                        "-DSWIFT_ENABLE_SWIFT_IN_SWIFT=FALSE")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'setup
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "lib/Driver/UnixToolChains.cpp"
                 (("return \"gold\";")
                  "return \"\";"))
                (substitute* "include/swift/SIL/SILLinkage.h"
                  (("#include \"llvm/Support/ErrorHandling.h\"")
                   "#include <cstdint>\n#include \"llvm/Support/ErrorHandling.h\""))
                (substitute* "include/swift/Basic/ExternalUnion.h"
                  (("#include \"llvm/Support/ErrorHandling.h\"")
                   "#include <cstdint>\n#include \"llvm/Support/ErrorHandling.h\""))
                (substitute* "stdlib/CMakeLists.txt"
                  (("set\\(CMAKE_CXX_COMPILER \"\\$\\{SWIFT_NATIVE_LLVM_TOOLS_PATH\\}/clang\\+\\+\"\\)")
                   (string-append "set(CMAKE_CXX_COMPILER \""
                                  (assoc-ref %build-inputs "swift-llvm")
                                  "/bin/clang++\")"))
                  (("set\\(CMAKE_C_COMPILER \"\\$\\{SWIFT_NATIVE_LLVM_TOOLS_PATH\\}/clang\"\\)")
                   (string-append "set(CMAKE_C_COMPILER \""
                                  (assoc-ref %build-inputs "swift-llvm")
                                  "/bin/clang\")")))
                (substitute* "stdlib/public/Platform/CMakeLists.txt"
                  (("set\\(GLIBC_SYSROOT_RELATIVE_INCLUDE_PATH \"/usr/include\"\\)")
                   (string-append "set(GLIBC_SYSROOT_RELATIVE_INCLUDE_PATH \""
                                  (assoc-ref %build-inputs "glibc")
                                  "/include\")")))
                (substitute* "lib/Basic/CMakeLists.txt"
                  (("\\$\\{LLVM_MAIN_SRC_DIR\\}/cmake/modules/GenerateVersionFromVCS.cmake")
                   "${LLVM_CMAKE_DIR}/GenerateVersionFromVCS.cmake"))
                (substitute* "lib/Option/CMakeLists.txt"
                  (("\\$\\{LLVM_MAIN_SRC_DIR\\}/\\.\\./clang/tools/driver/features\\.json")
                   "${LLVM_BUILD_BINARY_DIR}/share/clang/features.json"))
                ;; swiftBasic uses clangBasic symbols in Platform.cpp (DarwinSDKInfo)
                ;; but doesn't declare it as a PUBLIC dependency. Add clangBasic to
                ;; executables that link swiftBasic but not swiftAST (which provides it).
                (substitute* "tools/swift-scan-test/CMakeLists.txt"
                  (("swiftBasic\n" all)
                   (string-append all "                      clangBasic\n")))
                (substitute* "unittests/Remangler/CMakeLists.txt"
                  (("swiftBasic\n" all)
                   (string-append all "  clangBasic\n")))
                ;; Fix cmark-gfm path - use CMake-installed export with correct include paths
                (substitute* "cmake/modules/SwiftSharedCMakeConfig.cmake"
                  (("\\$\\{PATH_TO_CMARK_BUILD\\}/src/cmarkTargets\\.cmake")
                   "${PATH_TO_CMARK_BUILD}/lib/cmake/cmark-gfm/cmark-gfm.cmake"))
                ))))))
    (native-inputs
     (list cmake
           ninja
           perl
           pkg-config
           python-3
           swift-bootstrap
           swift-cmark-6.2
           %swift-libdispatch-6.2-source
           %swift-syntax-6.2-source))
    (inputs
     (list glibc icu4c libedit libxml2 swift-llvm-6.2 `(,util-linux "lib")))
    (synopsis "Swift programming language")
    (description
     "Swift is a general-purpose programming language built using a modern
approach to safety, performance, and software design patterns.")))
