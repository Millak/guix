;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2022, 2023, 2025 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (gnu packages rocm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xdisorg))

;; The components are tightly integrated and can only be upgraded as a unit. If
;; you want to upgrade ROCm, bump this version number and update hashes below.
(define %rocm-version "6.4.2")

;; As of version 6.1.0 several of the ROCm projects are contained within their
;; forked LLVM repository.  This is the same as the source for llvm-for-rocm.
(define %rocm-llvm-origin
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://github.com/ROCm/llvm-project/")
           (commit (string-append "rocm-" %rocm-version))))
    (file-name (git-file-name "llvm-for-rocm" %rocm-version))
    (sha256
     (base32
      "1j2cr362k7snsh5c1z38ikyihmjvy0088rj0f0dhng6cjwgysryp"))))

(define-public rocm-cmake
  (package
    (name "rocm-cmake")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ROCm/rocm-cmake/")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1af9z59bm8pj577x43q614v3ff039wijvcdpgw6sdsq1c0ssj260"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; Tests try to use git commit
    (native-inputs (list git))
    (home-page "https://rocm.docs.amd.com/projects/ROCmCMakeBuildTools/")
    (synopsis "ROCm cmake modules")
    (description "ROCm cmake modules provides cmake modules for common build
tasks needed for the ROCM software stack.")
    (license license:ncsa)))

(define-public rocm-device-libs
  (package
    (name "rocm-device-libs")
    (version %rocm-version)
    (source %rocm-llvm-origin)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; Not sure how to run them.
      #:build-type "Release"
      #:configure-flags
      #~(list "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
              "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "amd/device-libs"))))))
    (inputs (list llvm-for-rocm))
    (home-page "https://github.com/ROCm/llvm-project/")
    (synopsis "ROCm Device libraries")
    (description "AMD-specific device-side language runtime libraries, namely
oclc, ocml, ockl, opencl, hip and hc.")
    (license license:ncsa)))

(define-public rocm-hip-cpu
  ;; There are no releases or tags.
  (let ((commit "e112c935057434897bb12d9ab3910380a8bd5f58")
        (release "0"))
    (package
      (name "rocm-hip-cpu")
      (version "0.1.4142")              ;from CMakeLists.txt
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/ROCm/HIP-CPU/")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rbih56kfry7scvww54dwx8ph11ddzc5bf4ww1vs1vmhi3r05gpa"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags #~(list "-DBUILD_EXAMPLES=ON")))
      (home-page "https://github.com/ROCm/HIP-CPU/")
      (synopsis "An implementation of HIP that works on CPUs")
      (description "The HIP CPU Runtime is a header-only library that allows
CPUs to execute unmodified HIP code.  It is generic and does not assume a
particular CPU vendor or architecture.")
      (license license:expat))))

(define-public rocm-comgr
  (package
    (name "rocm-comgr")
    (version %rocm-version)
    (source %rocm-llvm-origin)
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-to-build
            (lambda _
              (chdir "amd/comgr")
              (setenv "ROCM_PATH"
                      #$(this-package-input "rocm-device-libs")))))))
    (inputs (list rocm-device-libs))
    (native-inputs (list llvm-for-rocm python))
    (home-page "https://github.com/ROCm/ROCm-CompilerSupport")
    (synopsis "ROCm Code Object Manager")
    (description "The Comgr library provides APIs for compiling and inspecting
AMDGPU code objects.")
    (license license:ncsa)))

;; This package is deprecated; this is the last version released.
(define-public roct-thunk-interface
  (package
    (name "roct-thunk-interface")
    (version "6.2.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ROCm/ROCT-Thunk-Interface.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y3mn4860z7ca71cv4hhag7racpc208acy8rws8ldw5k8yjc68m0"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; Not sure how to run tests.
    (inputs (list libdrm numactl))
    (native-inputs (list `(,gcc "lib") pkg-config))
    (home-page "https://github.com/ROCm/ROCT-Thunk-Interface")
    (synopsis "Radeon Open Compute Thunk Interface")
    (description "User-mode API interfaces used to interact with the ROCk
driver.")
    (license license:ncsa)))

(define-public rocr-runtime
  (package
    (name "rocr-runtime")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ROCm/ROCR-Runtime.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01gqbzqm5m28dw9b2calrbp9a23w2cc2gwi3pay8yl8qvk4jgkff"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; No tests.
      #:build-type "Release"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'add-rocm-device-lib-path
            (lambda _
              (setenv "ROCM_PATH"
                      #$(this-package-input "rocm-device-libs")))))))
    (inputs
     (list libdrm
           libelf-shared
           llvm-for-rocm
           numactl
           rocm-device-libs ; For bitcode.
           roct-thunk-interface))
    (native-inputs (list pkg-config xxd))
    (home-page "https://github.com/ROCm/ROCR-Runtime")
    (synopsis "ROCm Platform Runtime")
    (description "User-mode API interfaces and libraries necessary for host
applications to launch compute kernels to available HSA ROCm kernel agents.")
    (license license:ncsa)))

(define-public rocm-opencl-runtime
  (package
    (name "rocm-opencl-runtime")
    (version %rocm-version)
    (home-page "https://github.com/ROCm/clr")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00zr1fw84nifn2b2zd4wxf00f1171hjmz1lypzzmydsk5yw01q0c"))
              (patches
               (search-patches
                "rocclr-5.6.0-enable-gfx800.patch"
                ;; Guix includes a program clinfo already.
                "rocm-opencl-runtime-4.3-noclinfo.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; Not sure how to run them.
      #:build-type "Release"
      #:configure-flags
      #~(list
         (string-append "-DAMD_OPENCL_PATH=" #$(package-source this-package))
         "-DCLR_BUILD_OCL=ON"
         (string-append "-DROCM_PATH=" #$output)
         ;; Don't build the ICD loader as we have the opencl-icd-loader
         ;; package already.
         "-DBUILD_ICD=OFF"
         ;; Don't duplicate the install in an "opencl" directory as well.
         "-DFILE_REORG_BACKWARD_COMPATIBILITY=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'no-os-release
            (lambda _
              (substitute* "opencl/packaging/CMakeLists.txt"
                (("\"/etc/os-release\"")
                 "\"/dev/null\""))))
          (add-after 'install 'create-icd
            ;; Manually install ICD, which simply consists of dumping
            ;; the path of the .so into the correct file.
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((vendors (string-append #$output "/etc/OpenCL/vendors"))
                     (sopath (string-append #$output "/lib/libamdocl64.so")))
                (mkdir-p vendors)
                (with-output-to-file (string-append vendors "/amdocl64.icd")
                  (lambda _ (display sopath)))))))))
    (inputs
     (list glew
           mesa
           numactl
           opencl-headers
           opencl-icd-loader
           rocm-comgr
           rocr-runtime))
    (synopsis "ROCm OpenCL Runtime")
    (description "OpenCL 2.0 compatible language runtime, supporting offline
and in-process/in-memory compilation.")
    (license license:ncsa)))

(define-public rocminfo
  (package
    (name "rocminfo")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ROCm/rocminfo.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15mzmxz011sg42jg0dbxz57f4fagmxzdjc6zhd0yab3cq7k1kiv2"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; No tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-binary-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "rocminfo.cc"
                (("lsmod")
                 (search-input-file inputs "bin/lsmod"))
                (("grep") (search-input-file inputs "bin/grep"))))))))
    (inputs
     (list rocr-runtime kmod))
    (home-page "https://github.com/ROCm/rocminfo")
    (synopsis "ROCm Application for Reporting System Info")
    (description "List @acronym{HSA,Heterogeneous System Architecture} Agents
available to ROCm and show their properties.")
    (license license:ncsa)))

(define-public rocm-bandwidth-test
  (package
    (name "rocm-bandwidth-test")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ROCm/rocm_bandwidth_test.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1afmyx0dkif7djdcm5rfhnsibbrkj4py6dvh0gw4x3v750ms69wv"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; No tests.
    (inputs (list rocr-runtime))
    (home-page "https://github.com/ROCm/rocm_bandwidth_test")
    (synopsis "Bandwidth test for ROCm")
    (description "RocBandwidthTest is designed to capture the performance
characteristics of buffer copying and kernel read/write operations.  The help
screen of the benchmark shows various options one can use in initiating
cop/read/writer operations.  In addition one can also query the topology of
the system in terms of memory pools and their agents.")
    (license license:ncsa)))

