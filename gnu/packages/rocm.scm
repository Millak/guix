;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
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
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xdisorg))

;; The components are tightly integrated and can only be upgraded as a unit. If
;; you want to upgrade ROCm, bump this version number and update hashes below.
(define %rocm-version "5.6.0")

(define-public rocm-cmake
  (package
    (name "rocm-cmake")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/rocm-cmake.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "183s2ksn142r7nl7l56qvyrgvvkdgqfdzmgkfpp4a6g9mjp88ady"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; Tests try to use git commit
    (native-inputs (list git))
    (home-page "https://github.com/RadeonOpenCompute/rocm-cmake")
    (synopsis "ROCm cmake modules")
    (description "ROCm cmake modules provides cmake modules for common build
tasks needed for the ROCM software stack.")
    (license license:ncsa)))

(define-public rocm-device-libs
  (package
    (name "rocm-device-libs")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/ROCm-Device-Libs.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jg96ycy99s9fis8sk1b7qx5p33anw16mqlm07zqbnhry2gqkcbh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
             "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE")))
    (inputs (list llvm-for-rocm))
    (home-page "https://github.com/RadeonOpenCompute/ROCm-Device-Libs")
    (synopsis "ROCm Device libraries")
    (description "AMD-specific device-side language runtime libraries, namely
oclc, ocml, ockl, opencl, hip and hc.")
    (license license:ncsa)))

(define-public rocm-comgr
  (package
    (name "rocm-comgr")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/ROCm-CompilerSupport.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15s2dx0pdvjv3xfccq5prkplcbwps8x9jas5qk93q7kv8wx57p3b"))
              (patches
               (search-patches "rocm-comgr-3.1.0-dependencies.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "lib/comgr"))))))
    (inputs (list llvm-for-rocm rocm-device-libs))
    (home-page "https://github.com/RadeonOpenCompute/ROCm-CompilerSupport")
    (synopsis "ROCm Code Object Manager")
    (description "The Comgr library provides APIs for compiling and inspecting
AMDGPU code objects.")
    (license license:ncsa)))

(define-public roct-thunk-interface
  (package
    (name "roct-thunk-interface")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/ROCT-Thunk-Interface.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0v8j4gkbb21gqqmz1b4nmampx5ywva99ipsx8lcjr5ckcg84fn9x"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; Not sure how to run tests.
    (inputs (list libdrm numactl))
    (native-inputs (list `(,gcc "lib") pkg-config))
    (home-page "https://github.com/RadeonOpenCompute/ROCT-Thunk-Interface")
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
                    (url "https://github.com/RadeonOpenCompute/ROCR-Runtime.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07wh7s1kgvpw8ydxmr2wvvn05fdqcmcc20qjbmnc3cbbhxviksyr"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; No tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'add-rocm-device-lib-path
            (lambda _
              (substitute* "src/image/blit_src/CMakeLists.txt"
                (("-O2")
                 (string-append
                  "-O2 --rocm-device-lib-path="
                  #$(this-package-input "rocm-device-libs")
                  "/amdgcn/bitcode/")))))
          (add-after 'add-rocm-device-lib-path 'chdir
            (lambda _
              (chdir "src"))))))
    (inputs
     (list libdrm
           libelf
           llvm-for-rocm
           numactl
           rocm-device-libs ; For bitcode.
           roct-thunk-interface))
    (native-inputs (list pkg-config xxd))
    (home-page "https://github.com/RadeonOpenCompute/ROCR-Runtime")
    (synopsis "ROCm Platform Runtime")
    (description "User-mode API interfaces and libraries necessary for host
applications to launch compute kernels to available HSA ROCm kernel agents.")
    (license license:ncsa)))

;; This is the source only for ROCclr as from v4.5 it should only be built as
;; part of a client.  A warning is output if attempting to build stand-alone
;; and there is no install.
(define rocclr-src
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ROCm-Developer-Tools/ROCclr.git")
          (commit (string-append "rocm-" %rocm-version))))
    (sha256
     (base32
      "1fzvnngxcvxscn718cqfglm4izccx88zjdr3g5ldfqw7hyd034sk"))
    (patches (search-patches "rocclr-5.6.0-enable-gfx800.patch"))))

(define-public rocm-opencl-runtime
  (package
    (name "rocm-opencl-runtime")
    (version %rocm-version)
    (home-page "https://github.com/RadeonOpenCompute/ROCm-OpenCL-Runtime")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1azfxf0ac3mnbyfgn30bz5glwlmaigzdz0cd29jzc4b05hks1yr3"))
              (patches
               (search-patches
                ;; Guix includes a program clinfo already.
                "rocm-opencl-runtime-4.3-noclinfo.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; Not sure how to run them.
      #:configure-flags
      #~(list
         (string-append "-DAMD_OPENCL_PATH=" #$(package-source this-package))
         ;; The ROCclr source is needed to build the runtime.
         (string-append "-DROCCLR_PATH=" #$rocclr-src)
         (string-append "-DROCM_PATH=" #$output)
         ;; Don't build the ICD loader as we have the opencl-icd-loader
         ;; package already.
         "-DBUILD_ICD=OFF"
         ;; Don't duplicate the install in an "opencl" directory as well.
         "-DFILE_REORG_BACKWARD_COMPATIBILITY=OFF")
      #:phases
      #~(modify-phases %standard-phases
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
                    (url "https://github.com/RadeonOpenCompute/rocminfo.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "150bvyxp9krq8f7jqd1g5b4l85rih4ch322y4sg1hnciqpabn6a6"))))
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
    (home-page "https://github.com/RadeonOpenCompute/rocminfo")
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
                    (url "https://github.com/RadeonOpenCompute/rocm_bandwidth_test.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ca6r8xijw3a3hrlgkqqsf3iqyia6sdmidgmjl12f5vypxzp5kmm"))
              (patches (search-patches "rocm-bandwidth-test-5.5.0-fix-includes.patch"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; No tests.
    (inputs (list rocr-runtime))
    (home-page "https://github.com/RadeonOpenCompute/rocm_bandwidth_test")
    (synopsis "Bandwidth test for ROCm")
    (description "RocBandwidthTest is designed to capture the performance
characteristics of buffer copying and kernel read/write operations. The help
screen of the benchmark shows various options one can use in initiating
cop/read/writer operations.  In addition one can also query the topology of the
system in terms of memory pools and their agents.")
    (license license:ncsa)))

