;;; Copyright © 2023 Advanced Micro Devices, Inc.
;;; Copyright © 2025-2026 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages rocm-apps)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages rocm)
  #:use-module (gnu packages rocm-libs)
  #:use-module (gnu packages version-control))

(define-public rochpl
  (package
    (name "rochpl")
    (version "7.0.2")
    (home-page "https://github.com/ROCm/rocHPL")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i3aqd2jnd4ivcpqli75kr9019fcraylxxv9laixmyx9gfxv20f0"))
       (patches (search-patches "rochpl-supported-distros.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DGPU_TARGETS="
                             #$(current-amd-gpu-targets-string))

              ;; "find_package(ROCM)" is deprecated since 6.4 so most likely
              ;; the reference to 'ROCM_FOUND' is incorrect.
              "-DROCM_FOUND=ON"

              (string-append "-DROCM_PATH="
                             #$(this-package-input "rocm-hip-runtime"))
              (string-append "-DHPL_MPI_DIR="
                             #$(this-package-input "openmpi-rocm")))
      #:tests? #f                                 ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'avoid-native-optimizations
            (lambda _
              (substitute* "CMakeLists.txt"
                (("-march=native")
                 ""))))
          (add-after 'unpack 'set-default-data-file-name
            (lambda _
              ;; Set the default 'HPL.dat' file name in 'run_rochpl' & co.
              (substitute* (find-files "scripts" "\\.in")
                (("filename=HPL\\.dat")
                 (string-append "filename=" #$output
                                "/share/rochpl/HPL.dat")))))
          (add-after 'install 'move-files-where-they-belong
            (lambda _
              ;; Move files from the top level to the relevant directories.
              (let ((datadir (string-append #$output "/share/rochpl")))
                (for-each (lambda (program)
                            (rename-file
                             (string-append #$output "/" program)
                             (string-append #$output "/bin/"
                                            program)))
                          '("mpirun_rochpl" "run_rochpl"))

                (mkdir-p datadir)
                (rename-file (string-append #$output "/HPL.dat")
                             (string-append datadir "/HPL.dat"))))))))
    (native-inputs
     (list rocm-cmake lld-rocm llvm-rocm git-minimal/pinned))
    (inputs
     (list rocm-device-libs
           rocm-hip-runtime
           rocr-runtime
           rocblas
           libomp-rocm
           openmpi-rocm))
    (synopsis "Linear algebra benchmark for AMD GPUs")
    (description
     "rocHPL is a benchmark based on the @acronym{HPL, high-performance
LINPACK} benchmark, a reference linear-algebra benchmark, implemented on top
of AMD's Radeon Open Compute (ROCm) platform.  rocHPL is created using the HIP
programming language and optimized for AMD's discrete GPUs.")
    (properties
     `((amd-gpu-targets . ,%default-amd-gpu-targets)
       (tunable? . #t)))
    (license (list license:bsd-4 license:bsd-3))))

