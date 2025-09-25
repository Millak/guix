;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2022, 2023, 2025 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2026 Jean-Baptiste Note <jean-baptiste.note@m4x.org>
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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages libffi)
  #:use-module (ice-9 match))

;; The components are tightly integrated and can only be upgraded as a unit. If
;; you want to upgrade ROCm, bump this version number and update hashes below.

(define %rocm-version "7.1.0")

;; ROCm-systems derived packages belong here.

(define %rocm-systems-url "https://github.com/ROCm/rocm-systems")
(define %rocm-systems-origin
  (origin
    (method git-fetch)
    (uri (git-reference
           (url (string-append %rocm-systems-url))
           (commit (string-append "rocm-" %rocm-version))))
    (file-name (git-file-name "rocm-systems" %rocm-version))
    (sha256
     (base32
      "16h88j5440csz69s7gpcbmiwn72bz4zjlkdm2c7wcyp73m2knnm2"))
    (patches
     (search-patches
      "rocclr-5.6.0-enable-gfx800.patch"
      "rocm-opencl-runtime-4.3-noclinfo.patch"))))

(define-public rocr-runtime
  (package
    (name "rocr-runtime")
    (version %rocm-version)
    (source %rocm-systems-origin)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; No tests.
      #:build-type "Release"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "projects/rocr-runtime")))
          (add-after 'unpack 'add-rocm-device-lib-path
            (lambda _
              (setenv "ROCM_PATH"
                      #$(this-package-input "rocm-device-libs")))))))
    (inputs
     (list libdrm
           libelf-shared
           clang-rocm
           lld-rocm
           numactl
           rocm-device-libs))           ; For bitcode.
    (native-inputs (list pkg-config xxd))
    (home-page %rocm-systems-url)
    (synopsis "ROCm Platform Runtime")
    (description "User-mode API interfaces and libraries necessary for host
applications to launch compute kernels to available HSA ROCm kernel agents.")
    (license license:ncsa)))

(define-public rocm-opencl-runtime
  (package
    (name "rocm-opencl-runtime")
    (version %rocm-version)
    (home-page %rocm-systems-url)
    (source %rocm-systems-origin)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; Not sure how to run them.
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
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "projects/clr")))
          (add-after 'chdir 'no-os-release
            (lambda _
              (substitute* "opencl/packaging/CMakeLists.txt"
                (("/etc/os-release") "/dev/null"))))
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
           libffi
           rocm-comgr
           rocr-runtime))
    (synopsis "ROCm OpenCL Runtime")
    (description "OpenCL 2.0 compatible language runtime, supporting offline
and in-process/in-memory compilation.")
    (license license:expat)))

;; this runtime includes the hipcc and hip-config present in rocm-hipcc
;; wrapping them in rocm-hipcc allows the copies that are made here to be
;; wrapped also.
(define-public rocm-hip-runtime
  (package
    (name "rocm-hip-runtime")
    (version %rocm-version)
    (home-page %rocm-systems-url)
    (source %rocm-systems-origin)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; Not sure how to run them.
      #:build-type "Release"
      #:configure-flags
      #~(list
         (string-append "-DAMD_OPENCL_PATH=" #$(package-source this-package))
         "-DCLR_BUILD_HIP=ON"
         "-DCLR_BUILD_OCL=OFF"
         (string-append "-DROCM_PATH=" #$output)
         "-DHIP_PLATFORM=amd"
         (string-append
          "-DHIPCC_BIN_DIR=" #$(this-package-native-input "rocm-hipcc") "/bin")
         (string-append
          "-DHIP_COMMON_DIR=" #$(package-source this-package) "/projects/hip")
         ;; for now
         "-DUSE_PROF_API=OFF"
         "-DHIP_ENABLE_ROCPROFILER_REGISTER=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "projects/clr")))
          (add-after 'chdir 'no-os-release
            (lambda _
              (substitute* '("hipamd/packaging/CMakeLists.txt"
                             "hipamd/CMakeLists.txt")
                (("/etc/os-release") "/dev/null"))))
          (add-after 'chdir 'less-cmake-warnings
            (lambda _
              (substitute* "rocclr/cmake/ROCclrLC.cmake"
                (("2\\.9") "3.0"))))
          (add-after 'chdir 'fix-clang-location-for-embed-pch
            (lambda _
              (substitute* "hipamd/src/CMakeLists.txt"
                ;; differentiate between CLANG and LLVM packages
                (("\\$\\{HIP_LLVM_ROOT\\}")
                 (string-append  "${HIP_LLVM_ROOT} "
                                 #$(this-package-input "clang-rocm"))))
              (substitute* "hipamd/src/hip_embed_pch.sh"
                (("\\$5") "$6")
                (("LLVM_DIR=\"\\$4\"")
                 "LLVM_DIR=\"$4\"; CLANG_DIR=\"$5\";")
                (("\\$LLVM_DIR/bin/clang")
                 (string-append "$CLANG_DIR/bin/clang"))))))))
    (inputs
     (list glew
           mesa
           numactl
           perl
           opencl-headers
           rocm-comgr
           rocr-runtime
           rocm-device-libs
           libffi
           clang-rocm))
    (native-inputs
     (list rocm-hipcc))
    (synopsis "ROCm HIP Runtime")
    (description "HIP language runtime, allowing execution of HIP kernels
on AMD harware, with library support for in-process/in-memory
compilation (hipclr) of HIP kernel code, and its execution on AMD hardware.")
    (license license:expat)))

(define-public rocminfo
  (package
    (name "rocminfo")
    (version %rocm-version)
    (source %rocm-systems-origin)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; No tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "projects/rocminfo")))
          (add-after 'chdir 'patch-binary-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "rocminfo.cc"
                (("lsmod")
                 (search-input-file inputs "bin/lsmod"))
                (("grep")
                 (search-input-file inputs "bin/grep"))))))))
    (inputs
     (list rocr-runtime kmod python))
    (home-page %rocm-systems-url)
    (synopsis "ROCm Application for Reporting System Info")
    (description "List @acronym{HSA,Heterogeneous System Architecture} Agents
available to ROCm and show their properties.")
    (license license:ncsa)))



;; ROCm base helpers.

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
                "19jyymisxiikphzmq6h8vy5cg0r5dz3lxr5wvdf44frb8wxr8vla"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; Tests try to use git commit
    (native-inputs (list git))
    (home-page "https://rocm.docs.amd.com/projects/ROCmCMakeBuildTools/")
    (synopsis "ROCm cmake modules")
    (description "ROCm cmake modules provides cmake modules for common build
tasks needed for the ROCM software stack.")
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
      (synopsis "Implementation of HIP that works on CPUs")
      (description "The HIP CPU Runtime is a header-only library that allows
CPUs to execute unmodified HIP code.  It is generic and does not assume a
particular CPU vendor or architecture.")
      (license license:expat))))

(define %default-amdgpu-targets
  '("gfx908" "gfx90a" "gfx942" "gfx1030" "gfx1100" "gfx1101" "gfx1200" "gfx1201"))

;; I guess the intent would be to have this overridable with a package
;; transform akin to --with-property=amdgpu-target="gfx1100;gfx1101"
(define %default-amdgpu-targets-property
  `(amdgpu-targets
    .
    ,(string-join %default-amdgpu-targets ";")))

(define-syntax-rule (%amdgpu-targets)
  (string-split
   (assoc-ref (package-properties this-package) 'amdgpu-targets)
   #\;))

(define-public rocm-bandwidth-test
  (package
    (name "rocm-bandwidth-test")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ROCm/rocm_bandwidth_test")
                     (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09ssn8lkjaypq8qy3v28z25qvxsyha4cv4dk6xmfxmgfy8252yqy"))
              (patches
               (search-patches
                "rocm-bandwidth-test-take-default-gpus-from-environment.patch"
                "rocm-bandwidth-test-fix-hsa-include-file-lookup.patch"
                "rocm-bandwidth-test-fix-external-packages-search.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:build-type "Release"
      #:configure-flags
      #~(list
         "-DAMD_APP_TREAT_WARNINGS_AS_ERRORS=FALSE"
         "-DAMD_APP_ROCM_BUILD_PACKAGE=ON"
         ;; The rocm build system is very opiniated about the rpath and
         ;; mangles it beyond recognition, stripping all inputs from it.
         ;; Avoid the breakage.
         "-DAMD_APP_ROCM_BUILD_TRY_RPATH=OFF"
         ;; Allows the plugin files to be correctly rpathed to $#output/lib
         ;; where shared libraries reside.
         "-DAMD_WORK_BENCH_PLUGIN_ADD_INSTALL_PREFIX_TO_RPATH=TRUE"
         ;; Unfortunately this also needs to be patched everywhere, as the
         ;; value is individually set to OFF for a lot of targets, and not
         ;; propagated from the top. So set it here, and use substitutions
         ;; below.
         "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
         (string-append "-DROCM_PATH=" #$output)
         "-DUSE_LOCAL_FMT_LIB=ON"
         "-DUSE_LOCAL_NLOHMANN_JSON=ON"
         "-DUSE_LOCAL_SPDLOG=ON"
         "-DUSE_LOCAL_BOOST=ON"
         "-DUSE_LOCAL_CLI11=ON"
         "-DUSE_LOCAL_CATCH2=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-out-bogus-git-requirement
            (lambda _
              (substitute* "CMakeLists.txt"
                (("find_package\\(Git QUIET\\)")
                 "set(GIT_FOUND ON)"))))
          ;; This converts the amdgpu-targets property to its proper entry
          ;; point in this package; this relies on the patch
          ;; rocm-bandwidth-test-take-default-gpus-from-environment.patch
          ;; above, as I can't seem to find a way to do multiline matches.
          ;; Note that compiling for all GPUs architectures is sequential, and
          ;; will run for a very long time.
          (add-after 'unpack 'inject-amdgpu-targets
            (lambda _
              (substitute* "plugins/tb/transferbench/CMakeLists.txt"
                (("set\\(DEFAULT_GPUS[^)]*\\)")
                 (string-append
                  "set(DEFAULT_GPUS " #$(string-join (%amdgpu-targets)) ")" )))))
          ;; See configure-flags above.
          (add-after 'unpack 'fix-use-link-path
            (lambda _
              (substitute* (find-files "." "CMakeLists\\.txt$|\\.cmake$")
                ;; NB: this will also catch instances of
                ;; CMAKE_INSTALL_RPATH_USE_LINK_PATH
                (("INSTALL_RPATH_USE_LINK_PATH FALSE")
                 "INSTALL_RPATH_USE_LINK_PATH TRUE"))))
          ;; For the same reason, just in case patchelf ends up in the inputs
          ;; (the build system may require it), we remove a hardcoded rpath in
          ;; a bash build script which triggers if patchelf is detected.
          (add-after 'unpack 'remove-hardcoded-patchelf
            (lambda _
              (substitute* "plugins/tb/transferbench/build_libamd_tb.sh"
                (("command -v patchelf") "false"))))
          ;; XXX The build system uses -fuse-ld=lld flag for c++ linking if it
          ;; finds ld.lld in any way.  Unfortunately, for some reason, ld.lld
          ;; does not correctly setup the rpath when compiling the
          ;; rocm_bandwidth_test binary (the only executable which shows the
          ;; problem).  We do need ld.lld from lld-rocm in the path for
          ;; ROCm-specific linking however.  Therefore, just disable ld.lld
          ;; lookup for standard binaries.
          ;; I guess there's a standard Guix way to have lld behave for rpath
          ;; -- which should be applied to lld-rocm, probably.
          (add-after 'unpack 'dont-use-lld-for-executables
            (lambda _
              (substitute* "cmake/build_utils.cmake"
                (("find_program\\(LD_LLD_PATH ld\\.lld\\)")
                 "#find_program(LD_LLD_PATH ld.lld)"))))
          ;; This is just cosmetic and removes superfluous rpath entries
          ;; involving a nonexistant relative llvm/lib path.
          ;; XXX: RUNPATH checks are okay with $ORIGIN, but it looks like
          ;; other packages are removing it altogether -- what to do?
          (add-after 'unpack 'remove-dubious-rpath
            (lambda _
              (substitute* (find-files "." "CMakeLists\\.txt$|\\.cmake$")
                ((":\\\\\\$ORIGIN(/\\.\\./lib)?/llvm/lib") "")))))))
    (inputs (list rocr-runtime
                  rocm-hip-runtime
                  rocm-cmake
                  rocm-device-libs
                  rocm-comgr
                  numactl
                  curl
                  fmt-11
                  nlohmann-json
                  spdlog-1.15
                  boost
                  cli11
                  catch2-3.8))
    (properties
     (list %default-amdgpu-targets-property))
    (native-inputs (list
                    rocm-hipcc
                    clang-rocm
                    lld-rocm))
    (home-page "https://github.com/ROCm/rocm_bandwidth_test")
    (synopsis "Bandwidth test for ROCm")
    (description "RocBandwidthTest is designed to capture the performance
characteristics of buffer copying and kernel read/write operations.  The help
screen of the benchmark shows various options one can use in initiating
cop/read/writer operations.  In addition one can also query the topology of
the system in terms of memory pools and their agents.")
    (license license:expat)))

;; e-smi looks hard to unbundle correctly from amd-smi
;; the required esmi version is hardcoded in CMakeLists.txt
(define (make-esmi-source version hash)
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://github.com/amd/esmi_ib_library.git")
           (commit version)))
    (file-name (git-file-name "esmi_ib_library" version))
    (sha256 hash)))

(define %e-smi-version-for-rocm "esmi_pkg_ver-4.1.2")
(define e-smi-for-amd-smi
  (make-esmi-source
   %e-smi-version-for-rocm
   (base32 "1lj35gsa5pgfpsv0bl5y3xpk3xhk8kgsi4nkl2kxj0gsiyny8gf2")))

(define-public amd-smi
  (package
    (name "amd-smi")
    (version "25.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ROCm/amdsmi")
                     (commit (string-append "rocm-" %rocm-version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cfsj91pwzvc3c306ivvkzp819g4mxr88h091r2hr4f6h4xvvvgl"))
              (patches
               (search-patches "amd-smi-python.patch"))))
    (build-system cmake-build-system)
    (arguments (list
                #:tests? #f ;; The test suite is empty and failing as of 25.5.1
                #:configure-flags
                #~(list
                   "-DBUILD_SHARED_LIBS=ON"
                   ;; this requires ctypeslib2 packaging
                   ;; "-DBUILD_WRAPPER=ON"
                   "-DENABLE_ESMI_LIB=ON"
                   "-DBUILD_CLI=ON")
                #:phases
                #~(modify-phases %standard-phases
                    (add-after 'unpack 'add-e-smi
                      (lambda* _
                        (copy-recursively
                         #$(this-package-input
                            (origin-file-name e-smi-for-amd-smi))
                         "esmi_ib_library")
                        ;; Fool cmake, which uses failing git calls above this.
                        (substitute* "CMakeLists.txt"
                          (("# Update to latest tags if not matched")
                           (format #f "set(latest_esmi_tag ~s)"
                                   #$%e-smi-version-for-rocm)))))
                    (add-after 'add-e-smi 'patch-dlopen
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* (find-files "src" "\\.cc$")
                          (("libdrm.so.2")
                           (search-input-file inputs "/lib/libdrm.so.2"))
                          (("libdrm_amdgpu.so")
                           (search-input-file inputs "/lib/libdrm_amdgpu.so")))))
                    (add-after 'add-e-smi 'patch-python
                      (lambda* _
                        (substitute* (find-files "py-interface" "\\.py$")
                          (("/opt/rocm") #$output)))))))
    (inputs (list libdrm
                  python
                  e-smi-for-amd-smi))
    (home-page "https://github.com/ROCm/amdsmi")
    (synopsis "ROCm library and application for managing AMD devices")
    (description "The AMD @acronym{SMI,System Management Interface} allows
managing and monitoring AMD devices, particularly in high-performance
computing environments.  It provides a user-space interface that allows
applications to control GPU operations, monitor performance, and retrieve
information about the system's drivers and GPUs.  It also provides a
command-line tool, @command{amd-smi}, which can be used to do the same.")
    (license (list license:expat license:ncsa))))
