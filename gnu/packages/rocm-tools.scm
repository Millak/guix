;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Advanced Micro Devices, Inc.
;;; Copyright © 2026 David Elsing <david.elsing@posteo.net>
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

(define-module (gnu packages rocm-tools)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages instrumentation)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rocm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

;; The components are tightly integrated and can only be upgraded as a unit. If
;; you want to upgrade ROCm, bump this version number and the version number in
;; - rocm.scm
;; - rocm-libs.scm
;; - gdb.scm
;; and update the hashes of the affected packages.

(define %rocm-version "7.1.1")

(define-public hipify
  (package
    (name "hipify")
    (version %rocm-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ROCm/hipify")
              (commit (string-append "rocm-" version))))
       (file-name (git-file-name name (string-append "rocm-" version)))
       (sha256
        (base32 "153nzm292sp9m52dkjss5bf3971p5nwi38dw49pnlc1g00f4bc9m"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f ; requires CUDA
      #:configure-flags
      #~'("-DCMAKE_CXX_COMPILER=clang++")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-generated
            (lambda _
              (delete-file "bin/hipify-perl")))
          (add-before 'configure 'prepare-cmake
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.CMAKE_CXX_COMPILER.*") "")
                (("set.CMAKE_C_COMPILER.*") ""))))
          (add-after 'build 'build-hipify-perl
            (lambda _
              (invoke "./hipify-clang" "--perl" "--o-hipify-perl-dir=../source/bin")))
          (add-after 'install 'chmod
            (lambda _
              (for-each (lambda (file)
                          (chmod file #o555))
                        (find-files (string-append #$output "/bin")))))
          (add-after 'install 'patch-shebang
            (lambda _
              (patch-shebang (string-append #$output "/bin/hipify-perl")))))))
    (inputs (list clang-rocm perl))
    (synopsis "HIPIFY: Convert CUDA to HIP code")
    (description
     "HIPIFY is a set of tools that you can use to automatically
translate CUDA source code into portable HIP C++.")
    (home-page "https://github.com/ROCm/HIPIFY")
    (license license:ncsa)))

;; XXX: replace with rocprofiler-sdk from rocm-systems
(define-public rocprofiler
  (package
    (name "rocprofiler")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ROCm/rocprofiler")
                     (commit (string-append "rocm-" version))
                     ;; XXX: unbundle perfetto
                     (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lh6jmnwkswsczgppvw50gymjvih2mahblniap6ny1g67zl3xqap"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; requires GPU
      #:build-type "Release"
      #:configure-flags
      #~(list
         (string-append "-DCMAKE_CXX_COMPILER=clang++")
         (string-append "-DCMAKE_C_COMPILER=clang")
         "-DROCPROFILER_LD_AQLPROFILE=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-tests
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*ROCPROFILER_BUILD_(TESTS|CI).*") ""))))
          (add-after 'unpack 'disable-git
            (lambda _
              (substitute* "plugin/perfetto/CMakeLists.txt"
                (("rocprofiler_checkout_git_submodule") "message"))))
          (add-after 'unpack 'fix-experimental
            (lambda _
              (substitute* (find-files "." "\\.(h|cpp)$")
                (("experimental(/|::)") ""))))
          (add-after 'unpack 'fix-script-path
            (lambda _
              (substitute* "bin/rocprofv2"
                (("ROCM_DIR=.*")
                 (string-append "ROCM_DIR=" #$output "\n"))
                (("LD_LIBRARY_PATH=.*")
                 (string-append
                  "LD_LIBRARY_PATH=" #$(this-package-input "aqlprofile") "/lib"
                  ":$LD_LIBRARY_PATH\n")))))
          (add-after 'install 'remove-rocprof
            (lambda _
              (delete-file (string-append #$output "/bin/rocprof")))))))
    (propagated-inputs (list roctracer))
    (inputs
     (list aqlprofile
           barectf
           elfutils
           libffi
           libpciaccess
           numactl
           rocm-hip-runtime))
    (native-inputs
     (list python
           python-cppheaderparser
           python-lxml
           python-pyyaml
           rocm-cmake
           rocm-toolchain))
    (home-page "https://github.com/ROCm/rocprofiler")
    (synopsis "ROC profiler library")
    (description "ROC profiler library.  Profiling with perf-counters
and derived metrics.")
    (license license:expat)))
