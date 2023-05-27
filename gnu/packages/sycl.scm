;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Andy Tai <atai@atai.org>
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
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rocm)
  #:use-module (gnu packages vulkan))

;; This file adds SYCL implementation related packages. Due to the fact that
;; SYCL devices like GPU are not available during build (store environment),
;; tests that require such devices are all disabled.
;; Check https://lists.gnu.org/archive/html/guix-devel/2018-04/msg00293.html

(define-public opensycl
  (package
    (name "opensycl")
    (version "0.9.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenSYCL/OpenSYCL/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nynb93b68ghn1i1bsjvbbxpnh5b1yiw9jhhww1q1gi7w182x2g5"))))
    (build-system cmake-build-system)
    (native-inputs (list clang-15 llvm-15 python spirv-tools))
    (inputs (list boost rocm-opencl-runtime spirv-headers))
    (arguments `(#:tests? #f)) ; no tests
    (home-page "https://github.com/OpenSYCL/OpenSYCL")
    (synopsis "Implementation of the SYCL programming language for accelerators")
    (description
     "Open SYCL is a modern SYCL implementation targeting CPUs and GPUs from all
major vendors that supports many use cases and approaches for implementing SYCL")
    (license license:bsd-2)))
