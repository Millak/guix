;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2023 Advanced Micro Devices, Inc.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages rocm-libs)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rocm)
  #:use-module (gnu packages rocm-tools)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (srfi srfi-1))

;; The components are tightly integrated and can only be upgraded as a unit. If
;; you want to upgrade ROCm, bump this version number and the version number in
;; - rocm.scm
;; - rocm-tools.scm
;; - gdb.scm
;; and update the hashes of the affected packages.

(define %rocm-version "7.1.1")
(define %rocm-libraries-url "https://github.com/ROCm/rocm-libraries")

;; Needed by some CMake scripts, should correspond to %rocm-version
(define %rocm-libraries-commit "f322e9ab6100cc638959e2caf636043faec9124e")

(define rocm-libraries-monorepo
  (origin
    (method git-fetch)
    (uri (git-reference
           (url %rocm-libraries-url)
           (commit (string-append "rocm-" %rocm-version))))
    (file-name (git-file-name "rocm-libraries" %rocm-version))
    (sha256 (base32 "0jyjhkg1xlvziip29wqsvh386qy72018y114cx0x3cgfd8qy1dh0"))))

(define* (rocm-library-source name
                              #:key (patches '())
                              (location #f))
  (computed-file
   (string-append name "-" %rocm-version)
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (copy-recursively
          (string-append #$rocm-libraries-monorepo
                         #$(if location
                               (string-append "/" location)
                               (string-append "/projects/" name)))
          #$output)
         (with-directory-excursion #$output
           (for-each
            (lambda (file)
              (invoke (string-append #+(delayed-object patch) "/bin/patch")
                      "--force" "--no-backup-if-mismatch"
                      "-p1" "-i" file))
            '#$(map (lambda (file)
                      (local-file (assume-valid-file-name file)))
                    patches)))))))


(define-public rocrand
  (package
    (name "rocrand")
    (version %rocm-version)
    (source (rocm-library-source "rocrand"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         (string-append "-DAMDGPU_TARGETS="
                        #$(current-amd-gpu-targets-string)))))
    (inputs (list rocm-hip-runtime))
    (native-inputs (list rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "RAND library for the HIP programming language")
    (description "This package contains implementations for
pseudorandom and quasirandom number generation in HIP.")
    (license license:expat)))

(define-public hiprand
  (package
    (name "hiprand")
    (version %rocm-version)
    (source (rocm-library-source "hiprand"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         (string-append "-DAMDGPU_TARGETS="
                        #$(current-amd-gpu-targets-string)))))
    (inputs (list rocm-hip-runtime rocrand))
    (native-inputs (list rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "RAND library with multiple supported backends")
    (description "This package contains a wrapper library for generating
random numbers on GPUs, in particular via rocRAND for AMD GPUs.")
    (license license:expat)))

(define-public hipblas-common
  (package
    (name "hipblas-common")
    (version %rocm-version)
    (source (rocm-library-source "hipblas-common"))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f)) ; requires GPU
    (native-inputs (list rocm-cmake))
    (home-page %rocm-libraries-url)
    (synopsis "Common files shared by hipBLAS and hipBLASLt")
    (description "hipBLAS-common is a header-only library with common
definitions for hipBLAS and hipBLASLt.")
    (license license:expat)))

(define-public mxdatagenerator
  (package
    (name "mxdatagenerator")
    (version %rocm-version)
    (source
     (rocm-library-source
      name
      #:location "shared/mxdatagenerator"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; tests use a lot of memory
      #:configure-flags #~'("-DMXDATAGENERATOR_BUILD_TESTING=ON")))
    (native-inputs (list googletest))
    (home-page "https://github.com/ROCm/hipBLASLt")
    (synopsis "Library for generating AMD GPU kernel assembly")
    (description "This package contains a library for generating and analyzing
AMD GPU assembly kernels.")
    (license (list license:expat))))

(define-public rocroller
  (package
    (name "rocroller")
    (version %rocm-version)
    (source
     (rocm-library-source
      name
      #:location "shared/rocroller"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:configure-flags
      #~(list
         "-DROCROLLER_ENABLE_CATCH=OFF"
         "-DROCROLLER_ENABLE_GEMM_CLIENT_TESTS=OFF"
         "-DROCROLLER_ENABLE_SLOW_TESTS=OFF"
         "-DROCROLLER_ENABLE_TEST_DISCOVERY=OFF"
         (string-append "-DCMAKE_MODULE_PATH="
                        #$(this-package-native-input "rocm-cmake")
                        "/share/rocmcmakebuildtools/cmake"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cmake
            (lambda _
              (substitute* "CMakeLists.txt"
                (("FetchContent_MakeAvailable\\(ROCmCMakeBuildTools\\)") "")))))))
    (inputs
     (list rocm-hip-runtime
           msgpack-cxx
           fmt-11
           spdlog-1.15))
    (native-inputs
     (list cli11
           googletest
           lapack
           libdivide
           mxdatagenerator
           openblas
           rocm-cmake
           rocm-toolchain
           yaml-cpp))
    (home-page %rocm-libraries-url)
    (synopsis "Library for generating AMD GPU kernel assembly")
    (description "This package contains a library for generating and analyzing
AMD GPU assembly kernels.")
    (license (list license:expat))))

(define-public origami
  (package
    (name "origami")
    (version %rocm-version)
    (source
     (rocm-library-source
      name
      #:location "shared/origami"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~'("-DROCM_FOUND=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cmake
            (lambda _
              (substitute* "cmake/dependencies.cmake"
                (("find_package\\(Git REQUIRED\\)") ""))
              (substitute* "cmake/origami-config.cmake.in"
                (("origami::origami") "roc::origami")))))))
    (inputs (list rocm-hip-runtime))
    (native-inputs
     (list boost
           googletest
           rocm-cmake
           rocm-toolchain))
    (home-page %rocm-libraries-url)
    (synopsis "Library for selecting GEMM configurations for AMD GPU kernels")
    (description "@code{origami} provides a method for selecting GEMM
configurations deterministically based on compute and memory latencies.")
    (license (list license:expat))))

(define-public tensile
  (package
    (name "tensile")
    (version %rocm-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://github.com/ROCm/tensile"))
              (commit (string-append "rocm-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wff80vd1x1vcg3n56qlc9hdabk9svz6qk0sznycjwzbsmpfb0mr"))
       (patches
        (search-patches "tensile-copy-if-not-exist.patch"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-rocm-directory
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("Tensile/Utilities/Toolchain.py")
                (("/opt/rocm/bin")
                 (dirname
                  (search-input-file inputs "/bin/clang++")))
                (("/opt/rocm/llvm/bin")
                 (dirname
                  (search-input-file inputs "/bin/clang++"))))))
          (add-after 'unpack 'fix-tests
            (lambda _
              (substitute* "Tensile/Tests/yaml_only/test_config.py"
                (("availableArchs = findAvailableArchs\\(\\)")
                 #$(string-append
                    "availableArchs = ['"
                    (string-join (current-amd-gpu-targets) "', '") "']")))
              (setenv "TENSILE_ROCM_ASSEMBLER_PATH"
                      (string-append (which "clang")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "Tensile/Tests" "-m" "unit"
                        "-k" "not test_prepAsm")))))))
    (native-inputs
     (list python-filelock
           python-pandas
           python-pytest
           rocminfo
           rocm-hip-runtime
           rocm-toolchain))
    (propagated-inputs
     (list msgpack-3
           python-msgpack
           python-pyyaml
           python-joblib
           python-rich))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "GEMM kernel generator for AMD GPUs")
    (description "Tensile is a tool for creating benchmark-driven
backend libraries for GEMMs, GEMM-like problems (such as batched
GEMM), and general N-dimensional tensor contractions on a GPU.  The
Tensile library is mainly used as backend library to rocBLAS.  Tensile
acts as the performance backbone for a wide variety of compute
applications running on AMD GPUs.")
    (license license:expat)))

(define hipblaslt-supported-targets
  (list "gfx1100"
        "gfx1101"
        "gfx1102"
        "gfx1103"
        "gfx1150"
        "gfx1151"
        "gfx1200"
        "gfx1201"
        "gfx908"
        "gfx90a"
        "gfx940"
        "gfx941"
        "gfx942"
        "gfx950"))

(define (hipblaslt-targets targets)
  (string-join
   (lset-intersection string=? hipblaslt-supported-targets targets)
   ";"))

(define-public hipblaslt
  (package
    (name "hipblaslt")
    (version %rocm-version)
    (source
     (rocm-library-source
      "hipblaslt"
      #:patches (search-patches "hipblaslt-python-nanobind.patch")))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:validate-runpath? #f ; Fails with GPU kernel files
      #:configure-flags
      #~(let ((targets
               #$(hipblaslt-targets (current-amd-gpu-targets))))
          (cons*
           (string-append "-DCMAKE_CXX_COMPILER=clang++")
           (string-append "-DCMAKE_C_COMPILER=clang")
           (string-append "-DGPU_TARGETS=" targets)
           ;; Unbundle rocroller and mxdatagenerator.
           "-DHIPBLASLT_ENABLE_THEROCK=ON"
           "-DHIPBLASLT_ENABLE_BLIS=OFF"
           (string-append "-DCMAKE_PREFIX_PATH="
                          #$(this-package-native-input "python-nanobind")
                          "/lib/python"
                          #$(version-major+minor
                             (package-version python))
                          "/site-packages/nanobind/cmake")
           "-DHIPBLASLT_BUILD_TESTING=OFF"
           "-DHIPBLASLT_ENABLE_ROCROLLER=OFF"
           (string-append "-DTENSILELITE_BUILD_PARALLEL_LEVEL="
                          (number->string (parallel-job-count)))
           (if (string-null? targets) '("-DHIPBLASLT_ENABLE_DEVICE=OFF")
               '())))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cmake
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (substitute* "tensilelite/Makefile"
                (("AS :=.*")
                 "AS := amdclang++\n")
                (("LDD :=.*")
                 "LDD := amdclang++\n"))
              (substitute* "cmake/dependencies.cmake"
                (("find_package\\(Git REQUIRED\\)") ""))
              (substitute* "CMakeLists.txt"
                (("add_subdirectory.*origami.*")
                 "find_package(origami REQUIRED)\n")
                ;; Do not enable default GPU targets if empty
                (("NOT GPU_TARGETS OR") ""))))
          (add-after 'fix-cmake 'setenv
            (lambda _
              (setenv "HIP_PATH" #$(this-package-input "rocm-hip-runtime")))))))
    (inputs
     (list blis
           hipblas-common
           lapack
           msgpack-cxx
           rocm-hip-runtime
           rocm-smi
           rocm-toolchain
           roctracer))
    (native-inputs
     (list gfortran
           mxdatagenerator
           origami
           procps
           python
           python-distro
           python-joblib
           python-msgpack
           python-nanobind
           python-orjson
           python-packaging
           python-pyyaml
           python-setuptools
           python-simplejson
           python-ujson
           rocm-cmake
           rocm-toolchain
           rocroller))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)
                  (max-silent-time . ,(* 6 3600))))
    (home-page %rocm-libraries-url)
    (synopsis "Flexible library for general matrix-matrix operations")
    (description "hipBLASLt is a library that provides general
matrix-matrix operations with a flexible API and extends
functionalities beyond a traditional BLAS library.")
    (license (list license:expat))))

(define-public rocblas
  (package
    (name "rocblas")
    (version %rocm-version)
    (source
     (rocm-library-source
      "rocblas"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:validate-runpath? #f
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         #$(string-append "-DAMDGPU_TARGETS=" (current-amd-gpu-targets-string))
         "-DBUILD_WITH_PIP=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cmake
            (lambda _
              (substitute* "library/CMakeLists.txt"
                (("find_package\\(Git REQUIRED\\)") "")))))))
    (inputs
     (list rocm-hip-runtime
           hipblas-common
           hipblaslt
           msgpack-cxx
           python
           roctracer
           tensile))
    (native-inputs (list rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "BLAS implementation for ROCm")
    (description "rocBLAS is the ROCm Basic Linear Algebra Subprograms
(BLAS) library.  It is implemented in the HIP programming language.")
    (license (list license:expat license:bsd-3))))

(define-public rocprim
  (package
    (name "rocprim")
    (version %rocm-version)
    (source
     (rocm-library-source
      "rocprim"
      ;; XXX: With libstdc++, the placement new operator is not defined for
      ;; device code.
      #:patches (search-patches "rocprim-placement-new-delete.patch")))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f ; requires GPU
      #:configure-flags
      #~(list "-DCMAKE_CXX_COMPILER=hipcc"
              #$(string-append "-DAMDGPU_TARGETS="
                               (current-amd-gpu-targets-string)))))
    (inputs (list rocm-hip-runtime))
    (native-inputs (list rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis
     "Header-only library providing HIP parallel primitives")
    (description
     "The rocPRIM is a header-only library providing HIP parallel primitives
for developing performant GPU-accelerated code on the AMD ROCm platform.")
    (license license:expat)))

(define-public rocsparse
  (package
    (name "rocsparse")
    (version %rocm-version)
    (source
     (rocm-library-source
      "rocsparse"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         #$(string-append "-DAMDGPU_TARGETS="
                          (current-amd-gpu-targets-string)))))
    (inputs (list rocm-hip-runtime rocprim))
    (native-inputs (list rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "Sparse linear algebra library for ROCm")
    (description "This package exposes a sparse BLAS interface for ROCm.  It
is implemented in the HIP programming language.")
    (license (list license:expat license:bsd-3))))

(define-public hipsparse
  (package
    (name "hipsparse")
    (version %rocm-version)
    (source (rocm-library-source "hipsparse"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         #$(string-append "-DGPU_TARGETS=" (current-amd-gpu-targets-string)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-example-fpie
            (lambda _
              (substitute* "clients/samples/CMakeLists.txt"
                (("add_executable.*EXAMPLE_TARGET.*" all)
                 (string-append
                  all "target_compile_options(${EXAMPLE_TARGET}"
                  " PRIVATE -fpie)\n"))))))))
    (inputs
     (list googletest
           gfortran
           rocm-hip-runtime
           rocsparse))
    (native-inputs (list rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "Sparse linear algebra library with multiple supported
backends")
    (description "This package contains a wrapper library for sparse linear
algebra on GPUs, in particular via rocSPARSE for AMD GPUs.")
    (license license:expat)))

(define-public rocsolver
  (package
    (name "rocsolver")
    (version %rocm-version)
    (source (rocm-library-source "rocsolver"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         #$(string-append "-DAMDGPU_TARGETS=" (current-amd-gpu-targets-string)))
      #:phases
      #~(modify-phases %standard-phases
          ;; No matching template is found without these fixes
          (add-after 'unpack 'fix-std-templates
            (lambda _
              (substitute* "library/src/auxiliary/rocauxiliary_bdsqr_hybrid.hpp"
                ((" pow\\(") " std::pow("))
              (substitute* "library/src/include/lapack_host_functions.hpp"
                (("\\(isnan") "(std::isnan"))
              (substitute* "library/src/lapack/roclapack_syevj_heevj.hpp"
                (("std::norm\\((.*)\\)" _ orig)
                 (string-append "std::norm(rocblas_complex_num(" orig "))"))))))))
    (inputs
     (list rocblas
           rocm-hip-runtime
           rocprim
           rocsparse))
    (native-inputs (list fmt rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "LAPACK implementation for ROCm")
    (description "rocSOLVER is a partial implementation of the LAPACK
functionality for ROCm.  It is written in the HIP programming language.")
    (license license:bsd-2)))

(define-public hipsolver
  (package
    (name "hipsolver")
    (version %rocm-version)
    (source (rocm-library-source "hipsolver"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         #$(string-append "-DAMDGPU_TARGETS=" (current-amd-gpu-targets-string)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'suitesparse-include
            (lambda _
              (substitute* "library/src/amd_detail/hipsolver_sparse.cpp"
                (("<suitesparse/cholmod\\.h>")
                 "<cholmod.h>")))))))
    (inputs (list
             rocblas
             rocm-hip-runtime
             rocsolver
             rocsparse
             suitesparse-config
             suitesparse-cholmod))
    (native-inputs (list gfortran pkg-config rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "LAPACK library with multiple supported backends")
    (description "This package contains a wrapper library for LAPACK on GPUs,
in particular via rocSOLVER for AMD GPUs.")
    (license license:expat)))

(define-public hipblas
  (package
    (name "hipblas")
    (version %rocm-version)
    (source (rocm-library-source "hipblas"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         #$(string-append "-DAMDGPU_TARGETS=" (current-amd-gpu-targets-string)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-cmake
            (lambda _
              (substitute* "library/CMakeLists.txt"
                (("find_package\\(Git.*") "")
                (("execute_process\\(COMMAND.*GIT_EXECUTABLE.*")
                 "set(dummy\n")
                (("(set\\(hipblas_VERSION_COMMIT_ID ).*" _ orig)
                 (string-append orig #$%rocm-libraries-commit ")\n"))))))))
    (inputs
     (list hipblas-common
           rocblas
           rocm-hip-runtime
           rocrand
           rocsolver
           rocsparse))
    (native-inputs (list gfortran rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "BLAS library with multiple supported backends")
    (description "This package contains a wrapper library for linear algebra
algorithms on GPUs, in particular via rocRAND, rocBLAS, rocSPARSE and
rocSOLVER for AMD GPUs.")
    (license license:expat)))

(define-public hipcub
  (package
    (name "hipcub")
    (version %rocm-version)
    (source (rocm-library-source "hipcub"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         #$(string-append "-DAMDGPU_TARGETS="
                          (current-amd-gpu-targets-string)))))
    (inputs (list rocm-hip-runtime rocprim))
    (native-inputs (list rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "Parallel primitives library with multiple supported backends")
    (description "This package contains a wrapper library with HIP parallel
primitives, in particular via rocPRIM for AMD GPUs.")
    (license license:bsd-3)))

(define-public rocthrust
  (package
    (name "rocthrust")
    (version %rocm-version)
    (source (rocm-library-source "rocthrust"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         #$(string-append "-DAMDGPU_TARGETS="
                          (current-amd-gpu-targets-string)))))
    (inputs (list rocm-hip-runtime rocprim))
    (native-inputs (list rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "Thrust port for HIP and ROCm")
    (description "@code{rocThrust} is a parallel algorithm library for ROCm,
based on @code{rocPRIM}.  It is a port of the CUDA Thrust library.")
    (license license:asl2.0)))

(define-public composable-kernel
  (package
    (name "composable-kernel")
    (version %rocm-version)
    (source
     (rocm-library-source
      "composablekernel"
      #:patches (search-patches "composable-kernel-conditional-kernels.patch")))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         "-DBUILD_TESTING=OFF"
         #$(string-append "-DGPU_TARGETS=" (current-amd-gpu-targets-string)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-cmake
            (lambda _
              (substitute* "CMakeLists.txt"
                (("find_package\\(Git.*") "")
                (("execute_process\\(COMMAND.*GIT_EXECUTABLE.*")
                 (string-append
                  "set(COMMIT_ID " #$%rocm-libraries-commit ")\n"))
                ;; Reduce memory consumption:
                ;; https://gitweb.gentoo.org/repo/gentoo.git/tree/sci-libs/composable-kernel/composable-kernel-7.1.0.ebuild?id=a772170f741d75565652cb319a4e37eeb7efe6e9#n86
                ((".*(-amdgpu-early-inline-all|-amdgpu-function-calls).*") ""))))
          (add-after 'unpack 'setenv
            (lambda _
              (setenv "HIP_PATH" #$(this-package-input "rocm-hip-runtime")))))))
    (inputs (list rocm-hip-runtime))
    (native-inputs (list python rocm-cmake rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "Library for writing performance-portable kernels")
    (description "Composable Kernel is a library which provides a tile-based
programming model for writing performance-portable kernels, in particular for
GPUs using the HIP programming language.")
    (license license:expat)))

(define-public half-rocm
  (package
    (name "half-rocm")
    (version %rocm-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ROCm/half")
              (commit (string-append "rocm-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vlsmrs3aiv30j1ifks9g9cl8b1xdjv7dd706w5npjhy27j4xzr1"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list rocm-cmake))
    (home-page "https://github.com/ROCm/half")
    (synopsis "Half-precision floating-point library")
    (description "This package provides a header-only C++ library for a
half-precision floating point type (IEEE 754 conformant) together with common
operations on this type.")
    (license license:expat)))

(define-public miopen
  (package
    (name "miopen")
    (version %rocm-version)
    (source
     (rocm-library-source
      "miopen"
      #:patches
      (search-patches
       "miopen-zstd.patch")))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DCMAKE_CXX_COMPILER=hipcc"
         "-DBoost_USE_STATIC_LIBS=OFF"
         #$(string-append "-DAMDGPU_TARGETS=" (current-amd-gpu-targets-string)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-tidy+tests
            (lambda _
              (substitute* "CMakeLists.txt"
                (("add_subdirectory.*test.*") "")
                (("set\\(MIOPEN_TIDY_ERRORS ALL\\)") ""))))
          (add-after 'unpack 'adjust-ck-checks
            (lambda _
              (substitute* (find-files "src" ".*mha.*\\.cpp$")
                (("MIOPEN_USE_COMPOSABLEKERNEL") "MIOPEN_USE_CK_MHA_OPS"))
              (substitute*
                  (cons*
                   "src/mlo_dir_conv.cpp"
                   "src/solver/conv_ck_igemm_fwd_bias_res_add_activ_fused.cpp"
                   "src/solver/conv_ck_igemm_fwd_bias_activ_fused.cpp"
                   (find-files "src" ".*xdl.*\\.cpp$"))
                (("MIOPEN_USE_COMPOSABLEKERNEL") "MIOPEN_USE_CK_XDL_OPS")))))))
    (inputs
     (list
      boost
      composable-kernel
      hipblas
      hipblas-common
      hipblaslt
      rocblas
      rocm-hip-runtime
      rocmlir
      rocrand
      roctracer
      sqlite
      `(,zstd "lib")))
    (native-inputs
     (list
      eigen
      frugally-deep
      functionalplus
      googletest
      half-rocm
      nlohmann-json
      pkg-config
      rocm-cmake
      rocm-toolchain))
    (properties `((amd-gpu-targets . ,%default-amd-gpu-targets)))
    (home-page %rocm-libraries-url)
    (synopsis "GPU library for machine learning primitives")
    (description "@code{MIOpen} is a library which exposes a C API to GPU
machine learning primitives.  This package contains the HIP version based on
ROCm.")
    (license license:expat)))
