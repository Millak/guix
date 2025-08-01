;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2024 Andy Tai <atai@atai.org>
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
    (version "25.02.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AdaptiveCpp/AdaptiveCpp/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01wajw1vvbic1hiyz4rj7in09js3kl0xvaa2qpcg1pv7xkrz0xxx"))))
    (build-system cmake-build-system)
    ;; Sync with llvm-for-rocm llvm release.
    (native-inputs (list clang-19 llvm-19 python-minimal))
    (inputs
     (list boost
           libffi
           numactl
           rocm-opencl-runtime
           spirv-headers
           spirv-tools))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; FIXME: There is probably a much better way.
          ;; This file: bin/hipSYCL/llvm-to-backend/llvm-to-host-tool
          ;; requires libacpp-common.so and libllvm-to-{backend,host}.so
          ;; in RUNPATH
          (add-before 'validate-runpath 'create-symlinks
            (lambda _
              (symlink
               (string-append #$output "/lib/libacpp-common.so")
               (string-append
                #$output
                "/bin/hipSYCL/llvm-to-backend/libacpp-common.so"))
              (symlink
               (string-append
                #$output
                "/lib/hipSYCL/llvm-to-backend/libllvm-to-backend.so")
               (string-append
                #$output
                "/bin/hipSYCL/llvm-to-backend/libllvm-to-backend.so"))
              (symlink
               (string-append
                #$output
                "/lib/hipSYCL/llvm-to-backend/libllvm-to-host.so")
               (string-append
                #$output
                "/bin/hipSYCL/llvm-to-backend/libllvm-to-host.so")))))
      #:tests? #f)) ; no tests
    (home-page "https://adaptivecpp.github.io/")
    (synopsis
     "Implementation of the SYCL programming language for accelerators")
    (description
     "AdaptiveCpp is aImplementation of SYCL and C++ standard parallelism
for CPUs and GPUs from all vendors, with independent, community-driven
compiler for C++-based heterogeneous programming models")
    (license license:bsd-2)))

(define-public opensycl
  (deprecated-package "opensycl" adaptivecpp))
