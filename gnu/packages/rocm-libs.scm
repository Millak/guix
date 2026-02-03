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
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rocm)
  #:use-module (gnu packages serialization))

;; The components are tightly integrated and can only be upgraded as a unit. If
;; you want to upgrade ROCm, bump this version number and the version number in
;; - rocm.scm
;; - rocm-tools.scm
;; - gdb.scm
;; and update the hashes of the affected packages.

(define %rocm-version "7.1.1")
(define %rocm-libraries-url "https://github.com/ROCm/rocm-libraries")

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
