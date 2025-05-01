;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages opencl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

;; This file adds OpenCL implementation related packages. Due to the fact that
;; OpenCL devices like GPU are not available during build (store environment),
;; tests that require such devices are all disabled.
;; Check https://lists.gnu.org/archive/html/guix-devel/2018-04/msg00293.html

;; If you update either of opencl-headers, opencl-clhpp or opencl-icd-loader
;; note that they are released together (lockstep) and must be updated
;; together.
(define-public opencl-headers
  (package
    (name "opencl-headers")
    (version "2023.02.06")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/KhronosGroup/OpenCL-Headers")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1jxpx15gwxc6i7vp64xlzcxf57nl0qnaiip6jyr0j7iji47dm404"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; Not enabled during build.
    (synopsis "The Khronos OpenCL headers")
    (description
     "This package provides the C headers by Khronos for OpenCL programming.")
    (home-page "https://www.khronos.org/registry/OpenCL/")
    (license license:asl2.0)))

(define (make-opencl-headers major-version subversion)
  ;; The upstream OpenCL-Headers repository is no longer separating headers by
  ;; version; instead, you are supposed to #define CL_TARGET_OPENCL_VERSION.
  (deprecated-package (string-append "opencl-headers-"
                                     major-version "."
                                     subversion) opencl-headers))

(define-public opencl-headers-2.2
  (make-opencl-headers "2" "2"))
(define-public opencl-headers-2.1
  (make-opencl-headers "2" "1"))
(define-public opencl-headers-2.0
  (make-opencl-headers "2" "0"))
(define-public opencl-headers-1.2
  (make-opencl-headers "1" "2"))
(define-public opencl-headers-1.1
  (make-opencl-headers "1" "1"))
(define-public opencl-headers-1.0
  (make-opencl-headers "1" "0"))

(define-public opencl-clhpp
  (package
    (name "opencl-clhpp")
    (version "2023.02.06")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/OpenCL-CLHPP")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1m3v5apjv3qagym32xqg38pq6i8j5d8svz11clsx408nrlyngrj0"))
       (file-name (git-file-name name version))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (propagated-inputs
     (list opencl-headers))
    (arguments
     `(#:configure-flags (list "-DBUILD_EXAMPLES=OFF" "-DBUILD_TESTS=OFF"
                               "-DBUILD_TESTING=OFF") ;; CTest needs this to be turned off
       ;; The regression tests require a lot more dependencies.
       #:tests? #f))
    (build-system cmake-build-system)
    (home-page "https://github.khronos.org/OpenCL-CLHPP/")
    (synopsis "Khronos OpenCL-CLHPP")
    (description
     "This package provides the @dfn{host API} C++ headers for OpenCL.")
    (license license:expat)))

(define-public opencl-icd-loader
  (package
    (name "opencl-icd-loader")
    (version "2023.02.06")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KhronosGroup/OpenCL-ICD-Loader.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cmbcm6bz6kfvr0dy9hzf2vgfwcz8gbm8rxspqqpva6z74dz0qxr"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; Tests need stub loader setup.
    (native-search-paths
     (list (search-path-specification
            (variable "OCL_ICD_VENDORS")
            (files '("etc/OpenCL/vendors"))
            ;; Only supports a single directory.
            (separator #f))))
    (home-page "https://github.com/KhronosGroup/OpenCL-ICD-Loader")
    (inputs (list opencl-headers))
    (synopsis "OpenCL Installable Client Driver")
    (description
     "OpenCL defines an Installable Client Driver (ICD) mechanism to allow
developers to build applications against an Installable Client Driver loader
(ICD loader) rather than linking their applications against a specific OpenCL
implementation.  The ICD Loader is responsible for:

@itemize
@item Exporting OpenCL API entry points
@item Enumerating OpenCL implementations
@item Forwarding OpenCL API calls to the correct implementation
@end itemize

This package contains the Khronos official OpenCL ICD Loader.")
    (license license:asl2.0)))

(define-public clinfo
  (package
    (name "clinfo")
    (version "3.0.21.02.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Oblomov/clinfo")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sfxp6ai83i0vwdg7b05h0k07q6873q1z1avnyksj5zmzdnxya6j"))))
    (build-system gnu-build-system)
    (native-inputs
     (list opencl-headers))
    (inputs
     (list opencl-icd-loader))
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target))
              (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases (delete 'configure))
       #:tests? #f))
    (home-page "https://github.com/Oblomov/clinfo")
    (synopsis "Print information about OpenCL platforms and devices")
    ;; Only the implementation installed via Guix will be detected.
    (description
     "This package provides the @command{clinfo} command that enumerates all
possible (known) properties of the OpenCL platform and devices available on
the system.")
    (license license:cc0)))

(define-public ocl-icd
  (package
    (name "ocl-icd")
    (version "2.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OCL-dev/ocl-icd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y0lnxb6zlhfb5vxxib5n1vvxa4b23qc0j3lsih6yjz9j37mj7wz"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool ruby))
    (home-page "https://github.com/OCL-dev/ocl-icd")
    (synopsis "Generic OpenCL @acronym{ICD, Installable Client Driver} loader")
    (description
     "This package provides an OpenCL @acronym{ICD, Installable Client Driver}
loader.  It maintains a YAML database of all known and guessed function pointers
from vendor-specific drivers.  It also delivers a skeleton of bindings to
incorporate inside an OpenCL implementation to give it ICD functionalities.")
    (license license:bsd-2)))

(define-public beignet
  (package
    (name "beignet")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/beignet")
             (commit (string-append "Release_v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lpv3lvi2vrmzb8blflrpbd3jgin76zqmz6jcv17vn9mylqdrfnd"))
       (patches (search-patches "beignet-correct-file-names.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; There's a suspicious .isa binary file under kernels/.
        ;; Remove it.
        '(for-each delete-file (find-files "." "\\.isa$")))))
    (native-inputs (list pkg-config python))
    (inputs `(("clang@3.7" ,clang-3.7)
              ("clang-runtime@3.7" ,clang-runtime-3.7)
              ("glu" ,glu)
              ("llvm@3.7" ,llvm-3.7)
              ("libdrm" ,libdrm)
              ("libedit" ,libedit)
              ("libpthread-stubs" ,libpthread-stubs)
              ("libsm" ,libsm)
              ("libva" ,libva)
              ("libxfixes" ,libxfixes)
              ("libxext" ,libxext)
              ("mesa-utils" ,mesa-utils)
              ("ncurses" ,ncurses)
              ("ocl-icd" ,ocl-icd)
              ("opencl-icd-loader" ,opencl-icd-loader)
              ("opencl-headers" ,opencl-headers)
              ("xextproto" ,xextproto)
              ("zlib" ,zlib)))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCLANG_LIBRARY_DIR="
                            (assoc-ref %build-inputs "clang@3.7") "/lib")
             "-DENABLE_GL_SHARING=ON"
             "-DEXPERIMENTAL_DOUBLE=ON")

       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively
                (string-append out "/include"))
               #t)))
         (add-after 'remove-headers 'install-kernels
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (builddir (getcwd))
                    (source-dir (string-append
                                 builddir
                                 "/../beignet-Release_v1.3.2/kernels")))
               (copy-recursively source-dir
                                 (string-append out "/lib/beignet/kernels"))
               #t))))
       ;; Beignet tries to find GPU when running tests, which is not available
       ;; during build.
       #:tests? #f))
    (home-page "https://wiki.freedesktop.org/www/Software/Beignet/")
    (synopsis "OpenCL framework for Intel GPUs")
    (description
     "Beignet is an implementation of the OpenCL specification.  This code
base contains the code to run OpenCL programs on Intel GPUs---IvyBridge,
Haswell, Skylake, Apollolake, etc.  It defines and implements the OpenCL host
functions required to initialize the device, create the command queues, the
kernels and the programs, and run them on the GPU.  The code also contains a
back-end for the LLVM compiler framework.")
    ;; Beignet only supports Intel processors.
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:lgpl2.1+)))

(define-public pocl
  (package
    (name "pocl")
    (version "3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pocl/pocl")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1pw4q0hi5ynx34fvzryravz3jbh89f9cg60fkjj77sxh9xw8phdd"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
                 '(begin
                        ;; "kernel/test_printf_vectors" and
                        ;; "kernel/test_printf_vectors_ulongn"
                        ;; fail on aarch5 and likely other platforms
                        ;; as commented in CMakeLists.txt
                        ;; thus disable the block in CMakeList.txt adding
                        ;; these two tests
                       (substitute* "tests/kernel/CMakeLists.txt"
                         (("NOT ENABLE_POCL_FLOAT_CONVERSION") "false"))))))
    (build-system cmake-build-system)
    (native-inputs (list libltdl pkg-config python-3))
    (inputs (list clang-15 llvm-15
                  `(,hwloc-2 "lib") opencl-icd-loader))
    (arguments
     `(#:configure-flags (let* ((libdir (string-append (assoc-ref %outputs
                                                                  "out")
                                                       "/lib")))
                           (list "-DENABLE_ICD=ON"
                                 "-DENABLE_TESTSUITES=ON"
                                 ;; We are not developers, don't run conformance suite.
                                 "-DENABLE_CONFORMANCE=OFF"
                                 (string-append "-DEXTRA_HOST_LD_FLAGS=-L"
                                                (assoc-ref %build-inputs
                                                           "libc") "/lib")
                                 ;; We need both libdir and libdir/pocl in RUNPATH.
                                 (string-append "-DCMAKE_INSTALL_RPATH="
                                                libdir ";" libdir "/pocl")))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'set-HOME
                    (lambda _
                      (setenv "HOME" "/tmp")

                      ;; Since 2.9.0, hwloc fails when /sys is missing, so provide a
                      ;; fake topology.
                      (setenv "HWLOC_SYNTHETIC" "4"))))))
    (home-page "http://portablecl.org/")
    (synopsis "Portable Computing Language (pocl), an OpenCL implementation")
    (description
     "Pocl is a portable implementation of the OpenCL standard (1.2 with some
2.0 features supported).  This project seeks to improve performance
portability of OpenCL programs with the kernel compiler and the task run-time,
reducing the need for target-dependent manual optimizations.

pocl uses Clang as an OpenCL C frontend and LLVM for kernel compiler
implementation, and as a portability layer.  Thus, if your desired target has
an LLVM backend, it should be able to get OpenCL support easily by using
pocl.")
    (license license:expat)))

(define-public python-pytools
  (package
    (name "python-pytools")
    (version "2021.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytools" version))
       (sha256
        (base32 "1yyr4k6sqx859gjhc02633l2vxwdnj6m2f5blmf7dgq0gzzgcf05"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; Tests depend on packages not present in Guix.
    (propagated-inputs
     (list python-appdirs python-numpy))
    (home-page "https://pypi.org/project/pytools/")
    (synopsis "Assorted tools for Python")
    (description
     "Pytools is a bag of things that are ``missing'' from the Python standard
library:

@itemize
@item
small helper functions such as @code{len_iterable}, @code{argmin},
tuple generation, permutation generation, ASCII table pretty printing,
GvR's @code{monkeypatch_xxx} hack, the elusive @code{flatten}, and much more.
@item
Michele Simionato's decorator module
@item
A time-series logging module, @code{pytools.log}.
@item
Batch job submission, @code{pytools.batchjob}.
@item
A lexer, @code{pytools.lex}.
@end itemize\n")
    (license license:expat)))

(define-public python-pyopencl
  (package
    (name "python-pyopencl")
    (version "2025.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/inducer/pyopencl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02lvb286p8101sf3385lzv9ymz70vzjqnmfagmcy0fj912mx2svf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; FIXME: See <https://github.com/inducer/pyopencl/issues/784>, tests
      ;; require some special set up according to <.github/workflows/ci.yml>.
      #:tests? #f))
    (native-inputs
     (list cmake-minimal
           pybind11
           python-nanobind
           python-scikit-build-core))
    (inputs
     (list opencl-headers
           ocl-icd))
    (propagated-inputs
     (list python-mako
           python-numpy
           python-platformdirs
           python-pytools))
    (home-page "https://mathema.tician.de/software/pyopencl")
    (synopsis "Python wrapper for OpenCL")
    (description
     "PyOpenCL lets you access parallel computing devices such as GPUs from
Python @i{via} OpenCL.")
    (license license:expat)))
