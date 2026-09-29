;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Cayetano Santos <csantosb@disroot.org>
;;; Copyright © 2026 Goran Vukoman <g@odyss3us.net>
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

(define-module (gnu packages oneapi)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compiler-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; Built against LLVM 17, not 22 -- IGC's bf16 support probes for
;; and no-ops against LLVM 17, which Intel's own docs mark Production.
(define-public intel-graphics-compiler
  (package
    (name "intel-graphics-compiler")
    (version "2.40.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/intel-graphics-compiler")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c9lh4g8kwf2sgl9nrmqcvqyph2z94c98mnsj21ykdc8xrwkg1wj"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            ;; IRBuilderGeneratorCodeGen.cmake picks the first of several
            ;; candidate version directories that EXISTS; only patch ones
            ;; v2.40.13 actually ships (just "17.0.0"), never create one
            ;; -- an empty mkdir-p'd candidate would get picked first and
            ;; break the build a different way (missing stddef.h etc).
            (for-each
              (lambda (v)
                (let* ((headers-dir (string-append
                                      "external/llvm/releases/" v
                                      "/clang/lib/Headers"))
                       (stdint.h (string-append headers-dir "/stdint.h")))
                  (when (file-exists? headers-dir)
                    (when (file-exists? stdint.h)
                      (delete-file stdint.h))
                    (call-with-output-file stdint.h
                      (lambda (port)
                        (display "#ifndef IGC_PORTABLE_STDINT_H
#define IGC_PORTABLE_STDINT_H
typedef __INT8_TYPE__ int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;
#endif
" port))))))
              '("17" "17.0.0" "17.0.6"))
            ;; dllInterfaceCompute.cpp unconditionally calls
            ;; LLVMModule->dump(), which only exists when LLVM is built
            ;; with assertions/LLVM_ENABLE_DUMP -- Guix's llvm-17 isn't,
            ;; so it's an undefined symbol at link time. Not needed for
            ;; normal compilation.
            (substitute* "IGC/AdaptorOCL/dllInterfaceCompute.cpp"
              (("LLVMModule->dump\\(\\);") ";"))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(let ((clang #$(this-package-native-input "clang"))
              (lld #$(this-package-native-input "lld"))
              (llvm #$(this-package-native-input "llvm"))
              (opencl-clang #$(this-package-native-input "opencl-clang"))
              (spirv-llvm-translator
               #$(this-package-native-input "spirv-llvm-translator")))
          (list "-DIGC_OPTION__LLVM_MODE=Prebuilds"
                "-DIGC_OPTION__LLVM_PREFERRED_VERSION=17.0.6"
                (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
                ;; libigc.so/libigdfcl.so link against libLLVM-17.so,
                ;; libz.so.1 and libopencl-clang.so.17; CMake's automatic
                ;; RPATH tracking doesn't pick these up.
                "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON"
                (string-append "-DCMAKE_INSTALL_RPATH="
                               llvm "/lib;"
                               #$(this-package-input "zlib") "/lib;"
                               opencl-clang "/lib;"
                               spirv-llvm-translator "/lib")
                (string-append "-DCMAKE_PREFIX_PATH="
                               llvm ";"
                               spirv-llvm-translator ";" opencl-clang)
                ;; lldELF/lldCommon default to a nonexistent Windows-
                ;; prebuilt path; point them at lld-17 directly.
                (string-append "-DIGC_OPTION__LLDELF_LIB_DIR=" lld)
                (string-append "-DIGC_OPTION__LLD_BIN_DIR=" lld)
                (string-append "-DIGC_OPTION__LLDELF_H_DIR=" lld)
                ;; Avoids a file(GLOB_RECURSE) search for opencl-c.h next
                ;; to the clang-17 binary.
                (string-append "-DIGC_OPTION__OPENCL_HEADER_PATH="
                               clang "/lib/clang/17/include/opencl-c.h")
                "-DCMAKE_INSTALL_LIBDIR=lib"))
      #:phases
      #~(modify-phases %standard-phases
          ;; vc-intrinsics/SPIRV-Tools/SPIRV-Headers are built in-tree
          ;; with IGC regardless of LLVM_MODE: IGC's own
          ;; external/{SPIRV-Tools,vc-intrinsics} cmake wrappers expect
          ;; them as siblings of IGC's own source root.
          (add-after 'unpack 'add-vc-intrinsics
            (lambda _
              (copy-recursively #$(package-source vc-intrinsics)
                                "../vc-intrinsics")))
          (add-after 'unpack 'add-spirv-tools
            (lambda _
              (copy-recursively #$(package-source spirv-tools-for-igc)
                                "../SPIRV-Tools")))
          (add-after 'unpack 'add-spirv-headers
            (lambda _
              (copy-recursively #$(package-source spirv-headers-1.4.357.0)
                                "../SPIRV-Headers")))
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (mkdir-p "build")
              (chdir "build")
              (apply invoke "cmake" ".." configure-flags))))))
    (inputs (list libedit ncurses zlib zstd))
    (native-inputs (list bison
                         clang-17
                         flex
                         git-minimal
                         lld-17
                         llvm-17
                         opencl-clang
                         pkg-config
                         python
                         python-mako
                         python-pyyaml
                         spirv-headers-1.4.357.0
                         spirv-llvm-translator-17
                         spirv-tools-for-igc
                         vc-intrinsics))
    (home-page "https://github.com/intel/intel-graphics-compiler")
    (synopsis "Intel Graphics Compiler for OpenCL")
    (description "The Intel Graphics Compiler (IGC) is an LLVM-based compiler
for OpenCL targeting Intel graphics hardware.")
    (license license:expat)))

(define-public level-zero
  (package
    (name "level-zero")
    (version "1.34.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/oneapi-src/level-zero/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r5rx01d3rzg8hpn3qrl4c8vvk14iris1yb1jxk0jcapw49ryzv1"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list (string-append "-DVERSION_SHA=" #$version))))
    (native-inputs (list pkg-config python-minimal-wrapper))
    (home-page
     "https://oneapi-src.github.io/level-zero-spec/level-zero/latest/index.html")
    (synopsis "OneAPI Level Zero specification headers and loader")
    (description
     "Level Zero contains the following components of oneAPI:
@itemize
@item copies of the Level Zero Specification API C/C++ header files
@item loader
@item validation layer
@item tracing layer
@end itemize")
    (license
     (list license:expat                ;level-zero
           license:asl2.0))))           ;third-party/xla

;;; Updates and replaces oneapi-dnnl in (gnu packages machine-learning)
(define-public onednn
  ;; See doc/build/build.md.
  (package
    (name "onednn")
    (version "3.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/uxlfoundation/oneDNN/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0abrqhj33prdc17dw9r4mil1x9vg53kq615w0dncqm6vyvgx57ib"))
       (modules '((guix build utils)
                  (ice-9 rdelim)))
       ;; Copyright date used by code generation script
       (snippet
        '(for-each
          (lambda (file)
            (with-atomic-file-replacement
             file
             (lambda (in out)
               (let loop ((line (read-line in 'concat)))
                 (if (string-contains line "Copyright")
                     (display line out)
                     (loop (read-line in 'concat)))))))
          '("include/oneapi/dnnl/dnnl_debug.h"
            "src/common/dnnl_debug_autogenerated.cpp"
            "tests/benchdnn/dnnl_debug_autogenerated.cpp"
            "tests/benchdnn/dnnl_debug.hpp")))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags                 ;see doc/build/build_options.md
      #~(list
         #$@(if (target-riscv64?)
                (list "-DDNNL_CPU_RUNTIME=SEQ")
                ;; Default; also tbb and sycl.
                (list "-DDNNL_CPU_RUNTIME=OMP"))
         "-DDNNL_EXPERIMENTAL_UKERNEL=ON" ;Used in PyTorch
         "-DONEDNN_BUILD_GRAPH=OFF") ;See scripts/README.md
      #:phases
      #~(modify-phases %standard-phases
          ;; Benchdnn tests takes hours in a 100 cpu machine.
          (add-after 'unpack 'disable-benchdnn-testing
            (lambda _
              (substitute* "tests/CMakeLists.txt"
                (("add_subdirectory\\(benchdnn\\)") ""))))
          ;; See scripts/README.md.
          (add-after 'configure 'codegen
            (lambda _
              (with-directory-excursion "../source"
                ;; Generate types.xml.
                (invoke "castxml" "--castxml-cc-gnu-c" "clang"
                        "--castxml-output=1" "-Iinclude" "-I../build/include"
                        "include/oneapi/dnnl/dnnl_types.h" "-o" "types.xml")
                ;; Run generate_dnnl_debug.py.
                (invoke
                 "python3" "scripts/generate_dnnl_debug.py" "types.xml")
                ;; Generating format tags.
                ;; Modifies include/oneapi/dnnl/dnnl.hpp
                (invoke "python3" "scripts/generate_format_tags.py")))))))
    (native-inputs
     (list castxml clang googletest python-minimal-wrapper))
    (home-page "https://uxlfoundation.github.io/oneDNN/")
    (synopsis "Deep neural network library")
    (description
     "@acronym{OneDNN, OneAPI Deep Neural Network Library} is a cross-platform
performance library of basic building blocks for deep learning applications.")
    (supported-systems %64bit-supported-systems)
    (license license:asl2.0)))

(define-public oneapi-dnnl (deprecated-package "oneapi-dnnl" onednn))

;;; Updates and replaces tbb in (gnu packages tbb)
(define-public onetbb
  (package
    (name "onetbb")
    (version "2023.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/uxlfoundation/oneTBB/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15knh7rwdvwjylk4kcxlkhgdwla4m3k5zkxg4xvznnqh0zds2bpc"))
       (patches (search-patches "onetbb-other-arches.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list #$@(if (or (target-riscv64?)
                         (target-ppc32?))
                     '(list "-DTBB_TEST_LINK_FLAGS=-latomic")
                     '())
              #$@(if (or (target-arm32?)
                         (target-ppc32?))
                     '("-DTBB_TEST_COMPILE_FLAGS=-DTBB_TEST_LOW_WORKLOAD")
                     '())
              ;; Don't fail on warnings.
              "-DTBB_STRICT=OFF")
      #:phases
      #~(modify-phases %standard-phases
          #$@(cond
              ((target-arm32?)
               `((add-after 'unpack 'adjust-test-suite
                   (lambda _
                     (substitute* "test/CMakeLists.txt"
                       ;; Bus error, skipped on mips.
                       ((".*test_malloc_pools.*") ""))))))
              ((target-ppc32?)
               `((add-after 'unpack 'adjust-test-suite
                   (lambda _
                     (substitute* "test/CMakeLists.txt"
                       ;; These tests hang forever.
                       ((".*test_function_node.*") "")
                       ((".*test_multifunction_node.*") "")
                       ((".*test_async_node.*") ""))))))
              ((target-riscv64?)
               `((add-after 'unpack 'adjust-test-suite
                   (lambda _
                     (substitute* "test/CMakeLists.txt"
                       ;; This tests hangs forever.
                       ((".*test_task_group.*") ""))))))
              (else '())))))
    (home-page "https://uxlfoundation.github.io/oneTBB/")
    (synopsis "C++ library for parallel programming")
    (description
     "@acronym{OneTBB, OneAPI Threading Building Blocks} is a C++ runtime
library that abstracts the low-level threading details necessary for optimal
multi-core performance.  It uses common C++ templates and coding style to
eliminate tedious threading implementation work.  It provides parallel loop
constructs, asynchronous tasks, synchronization primitives, atomic operations,
and more.")
    (license license:asl2.0)))

(define-public onetbb-2022
  (package
    (inherit onetbb)
    (name "onetbb")
    (version "2022.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/uxlfoundation/oneTBB/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ibwr5cw0zw5dfcdhavr5zbmwqqf7vnsai0gh4mqw475l7lcb08w"))
       (patches (search-patches "onetbb-other-arches.patch"))))))

;; Built against LLVM 17 (Guix's prebuilt), matching IGC v2.40.13's
;; own dependency table -- see intel-graphics-compiler for why LLVM 17.
(define-public opencl-clang
  (package
    (name "opencl-clang")
    (version "17.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/opencl-clang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03z2nb9gndpxb60rx1bk6q1w6mkak7729z0rm66slzxfmhbfqira"))
       (patches (search-patches
                 "opencl-clang-separate-llvm-clang-prefix.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DPREFERRED_LLVM_VERSION="
                             #$(package-version llvm-17))
              (string-append "-DLLVM_DIR="
                             #$(this-package-input "llvm") "/lib/cmake/llvm")
              (string-append "-DClang_DIR="
                             #$(this-package-input "clang") "/lib/cmake/clang")
              "-DLLVMSPIRV_INCLUDED_IN_LLVM=OFF"
              (string-append "-DSPIRV_TRANSLATOR_DIR="
                             #$(this-package-input "spirv-llvm-translator"))
              ;; intel does not run tests against a plain llvm/clang;
              ;; substitutes needed, see below
              "-DLLVM_INCLUDE_TESTS=ON"
              (string-append "-DLLVM_EXTERNAL_LIT="
                             #$(this-package-native-input "python-lit")
                             "/bin/lit")
              ;; tests/lit.site.cfg.py.in assumes an in-tree build; point it
              ;; at clang-17 package instead
              (string-append "-DCLANG_TOOLS_DIR="
                             #$(this-package-input "clang") "/bin"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-occ-cli-tools-dir
            (lambda _
              ;; replace the tools dir of the non existent in-tree build
              ;; against our own one
              (substitute* "tests/lit.site.cfg.py.in"
                (((string-append "opencl_clang_tools_dir = "
                                "lit_config\\.substitute\\(path\\("
                                "r\"@LLVM_TOOLS_DIR@"))
                 (string-append "opencl_clang_tools_dir = "
                                "lit_config.substitute(path("
                                "r\"@CMAKE_CURRENT_BINARY_DIR@/occ-cli")))
              ;; cl-ext-option.cl exercises "-cl-std=CL3.1", which no
              ;; stock LLVM/Clang 17 release accepts
              (substitute* "tests/lit.cfg.py"
                (("\"Conformance\"\\]")
                 "\"Conformance\", \"cl-ext-option.cl\"]"))))
          ;; llvm does not register tests via CTest
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "cmake" "--build" "."
                        "--target" "check-opencl-clang")))))))
    (native-inputs (list pkg-config python-lit))
    (inputs (list llvm-17 clang-17 spirv-llvm-translator-17))
    (home-page "https://github.com/intel/opencl-clang")
    (synopsis "Clang-based compiler for OpenCL targeting SPIR-V, for LLVM 17")
    (description
     "A standalone build of Intel's opencl-clang (libopencl-clang)
against a prebuilt LLVM/Clang 17, as used by Intel Graphics Compiler in
Prebuilds mode.")
    (license license:asl2.0))) ;with LLVM exceptions

(define-public python-onetbb
  (package
    (inherit onetbb)
    (name "python-onetbb")
    (arguments
     (list
      #:configure-flags
      #~(list "-DTBB4PY_BUILD=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-python-install-directory
            (lambda _
              (substitute* "python/setup.py"
                (("extra_link_args=tbb_flag,")
                 (string-append
                  "extra_link_args=['-Wl,-rpath="
                  #$(this-package-input "onetbb") "/lib"
                  "', '-Wl,-rpath=" #$output "/lib'] + tbb_flag,")))))
          (replace 'build
            (lambda _
              (setenv "PYTHONHASHSEED" "0")
              (invoke "make" "python_build")))
          ;; The 'build phase already installs the modules
          (replace 'install
            (lambda _
              (with-directory-excursion "python/rml"
                (invoke "make" "install"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ctest" "-R" "python_test" "--output-on-failure")))))))
    (native-inputs (list swig-4.4 python-minimal python-setuptools))
    (inputs (list onetbb))
    (synopsis "Python bindings for the oneTBB parallel library")
    (description
     "@acronym{OneTBB, OneAPI Threading Building Blocks} is a C++ runtime
library that abstracts the low-level threading details necessary for optimal
multi-core performance.  It uses common C++ templates and coding style to
eliminate tedious threading implementation work.  It provides parallel loop
constructs, asynchronous tasks, synchronization primitives, atomic operations,
and more.  @code{python-onetbb} enables threading composability between two or
more thread-enabled Python libraries.")))

(define-public umf
  (package
    (name "umf")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/oneapi-src/unified-memory-framework/")
              (commit (string-append "v" version))))
       (sha256
        (base32
         "0g264s9zz4mcckd3cajmn2i1pz7y8cbrx2dmj8yqcwkbdysijbhw"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                    ;errors with "function not implemented"
      #:configure-flags
      #~(list "-DUMF_BUILD_CUDA_PROVIDER=OFF"
              "-DUMF_BUILD_EXAMPLES=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-ld-library-path
            (lambda _
              (for-each
               (lambda (f)
                 (delete-file (format #f "test/~a" f)))
               '("test_memspace_highest_capacity"
                 "test_memtarget"))
              (setenv "LD_LIBRARY_PATH"
                      (string-append #$(this-package-input "onetbb")
                                     "/lib"))))
          (add-after 'unpack 'fix-googletest
            (lambda _
              (substitute* "test/CMakeLists.txt"
                (("FetchContent_MakeAvailable\\(googletest\\)")
                 "find_package(GTest REQUIRED)")))))))
    (inputs
     (list `(,hwloc "lib") level-zero numactl onetbb))
    (native-inputs
     (list pkg-config python-minimal-wrapper googletest))
    (home-page "https://oneapi-src.github.io/unified-memory-framework/")
    (synopsis "OneAPI @acronym{UMG, Unified Memory Framework}")
    (description
     "The UMF is a library for constructing allocators and memory pools. It
also contains broadly useful abstractions and utilities for memory
management.  UMF allows users to manage multiple memory pools characterized by
different attributes, allowing certain allocation types to be isolated from
others and allocated using different hardware resources as required..")
    (license license:asl2.0)))

;; LLVM IR intrinsics consumed by intel-graphics-compiler's codegen
;; backend; version pinned to IGC v2.40.13's own dependency table.
(define-public vc-intrinsics
  (package
    (name "vc-intrinsics")
    (version "0.25.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/vc-intrinsics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "093yn5508xdpym3rrb1i0r27rhr4v7kvjix3qa3saikrfp1kadx3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list (string-append "-DLLVM_DIR=" #$(this-package-input "llvm")
                             "/lib/cmake/llvm"))))
    (native-inputs (list pkg-config))
    (inputs (list llvm-17 python-minimal))
    (home-page "https://github.com/intel/vc-intrinsics")
    (synopsis "LLVM intrinsics for Intel's GenX vector ISA")
    (description
     "vc-intrinsics provides LLVM IR intrinsics and an LLVM
pass for Intel's GenX vector compute instruction set, used by Intel
Graphics Compiler (IGC) and related tools to represent hardware-specific
vector operations in LLVM IR.")
    (license license:expat)))
