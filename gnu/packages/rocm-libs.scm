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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rocm))

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
