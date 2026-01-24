;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2024 Andy Tai <atai@atai.org>
;;; Copyright © 2025, 2026 Cayetano Santos <csantosb@inventati.org>
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

(define-module (gnu packages sycl)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rocm)
  #:use-module (gnu packages vulkan))

;; This file adds SYCL implementation related packages. Due to the fact that
;; SYCL devices like GPU are not available during build (store environment),
;; tests that require such devices are all disabled.
;; Check https://lists.gnu.org/archive/html/guix-devel/2018-04/msg00293.html

(define-public adaptivecpp
  (package
    (name "adaptivecpp")
    (version "25.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AdaptiveCpp/AdaptiveCpp/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sv5f366ybzpsdpka0fb578pb4q3537h6hkn825m0kbssh32wxk7"))))
    (build-system cmake-build-system)
    ;; Sync with llvm-for-rocm llvm release.
    (native-inputs (list clang-19 lld-19 python-minimal))
    (inputs
     (list boost
           libffi
           numactl
           rocm-opencl-runtime
           spirv-headers
           spirv-tools))
    (arguments
     (list
      #:configure-flags
      #~(list
         (string-append "-DCMAKE_EXE_LINKER_FLAGS=" "-Wl,-rpath="
                        #$output "/lib,-rpath="
                        #$output "/lib/hipSYCL/llvm-to-backend")
         (string-append
          "-DACPP_LLD_PATH=" (search-input-file %build-inputs "/bin/ld.lld")))
      #:tests? #f)) ; no tests
    (home-page "https://adaptivecpp.github.io/")
    (synopsis
     "Implementation of the SYCL programming language for accelerators")
    (description
     "AdaptiveCpp is aImplementation of SYCL and C++ standard parallelism
for CPUs and GPUs from all vendors, with independent, community-driven
compiler for C++-based heterogeneous programming models")
    (license license:bsd-2)))

(define-deprecated-package opensycl
  adaptivecpp)
