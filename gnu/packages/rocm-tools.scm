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
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages rocm))

;; The components are tightly integrated and can only be upgraded as a unit. If
;; you want to upgrade ROCm, bump this version number and the version number in
;; - rocm.scm
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

