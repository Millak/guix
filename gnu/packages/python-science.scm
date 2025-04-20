;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2020-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2022-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020-2025 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021, 2023 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Leo Famulari <leo@famulari.name>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2022, 2024 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023, 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 Marco Baggio <marco.baggio@mdc-berlin.de>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024 Rick Huijzer <ikbenrickhuyzer@gmail.com>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages python-science)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages chemistry)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-graphics)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages simulation)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages time)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject))

(define-public pyre
  (package
    (name "pyre")
    (version "1.12.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyre/pyre")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0crmssga481q2ggwcmj40nj5n9975wri14p609jdr9hwg4vdyvj2"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:imported-modules (append %cmake-build-system-modules
                                 %python-build-system-modules)
      #:modules '((guix build cmake-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:configure-flags
      #~(list (string-append "-DPYRE_VERSION=" #$version)
              (string-append "-DPYRE_DEST_PACKAGES="
                             (python:site-packages %build-inputs %outputs)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enable-bytecode-determinism
            (assoc-ref python:%standard-phases 'enable-bytecode-determinism))
          ;; Move the check phase after the Python 'pyre' module
          ;; is installed and made available.
          (delete 'check)
          (add-after 'install 'add-to-pythonpath
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (python:add-installed-pythonpath inputs outputs)))
          (add-after 'add-to-pythonpath 'wrap
            (assoc-ref python:%standard-phases 'wrap))
          (add-after 'add-to-pythonpath 'check
            (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
              (when tests?
                (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
                (let ((ignored-tests
                       (list
                        ;; The MPI tests are failing for unknown reasons (see:
                        ;; https://github.com/pyre/pyre/issues/126).
                        "tests.mpi"
                        ;; These tests have a cleanup phase that fails
                        ;; non-deterministically (see:
                        ;; https://github.com/pyre/pyre/issues/125).
                        "tests.pyre.lib.viz.flow"
                        ;; This test expects a TCP port 22 to be listening.
                        "tests.pyre.pkg.ipc.tcp.py"
                        ;; These postgres tests require a running postgresql
                        ;; daemon; they are also skipped in upstream CI.
                        "tests.postgres.ext"
                        ;; This test fails due to pre-1980 timestamps, not
                        ;; supported by ZIP.
                        "tests.pyre.pkg.filesystem.zip_open.py"
                        ;; This one trips on the patched python3 shebang.
                        "tests.pyre.pkg.filesystem.local_open.py")))
                  (invoke "ctest"
                          "-j" (if parallel-tests?
                                   (number->string (parallel-job-count))
                                   "1")
                          "-E" (string-join ignored-tests "|")))))))))
    (native-inputs (list openssh-sans-x python python-numpy pybind11 zip))
    (inputs (list gsl hdf5 openmpi postgresql))
    (propagated-inputs (list python-pyyaml)) ;for the Python bindings
    (home-page "http://pyre.orthologue.com/")
    (synopsis "Framework for building Scientific applications")
    (description
     "This package provides a framework for building scientific applications.
It aims to bring state of the art software design practices to scientific
computing, with the goal of providing a strong skeleton on which to build
scientific codes by steering the implementation towards usability and
maintainability.")
    (license license:bsd-3)))

(define-public python-algopy
  (package
    (name "python-algopy")
    (version "0.6.0") ; the higher versions requir NumPy 2+ stack
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "algopy" version))
       (sha256
        (base32 "1vjrzzxa3gvyh2zvm1vwg0s6a7dv23rihgdvgyj1vqniyymp91nq"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy
           python-scipy))
    (home-page "https://pythonhosted.org/algopy")
    (synopsis "Algorithmic Differentation in Python")
    (description
     "AlgoPy provides a functionality to differentiate functions implemented
as computer programs by using Algorithmic Differentiation (AD) techniques in
the forward and reverse mode.

The forward mode propagates univariate Taylor polynomials of arbitrary order.
Hence it is also possible to use AlgoPy to evaluate higher-order derivative
tensors.  The reverse mode is also known as backpropagation and can be found
in similar form in tools like PyTorch.  Speciality of AlgoPy is the
possibility to differentiate functions that contain matrix functions as
+,-,*,/, dot, solve, qr, eigh, cholesky.")
    (license license:bsd-3)))

(define-public python-cmocean
  (package
    (name "python-cmocean")
    (version "4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cmocean" version))
       (sha256
        (base32 "0z0d4ma6i228gwpgnkai8scs9bmzz41rirlnqpmb8hazzfcq71ip"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-packaging))
    (home-page "https://github.com/matplotlib/cmocean")
    (synopsis "Colormaps for Oceanography")
    (description
     "This package contains colormaps for commonly-used oceanographic
variables.  Most of the colormaps started from @code{matplotlib} colormaps,
but have now been adjusted using the viscm tool to be perceptually uniform.")
    (license license:expat)))

(define-public python-cvxpy
  (package
    (name "python-cvxpy")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cvxpy" version))
       (sha256
        (base32 "0lyri9j5gyg6m1bvfy1a4q2sqdy3w45lp0bxiq9as8srq347ic5i"))))
    (build-system pyproject-build-system)
    ;; It's odd but cvxpy appears to need pybind11 at runtime according to its
    ;; specification.  Moving pybind11 to native-inputs would break downstream
    ;; packages using cvxpy.
    (propagated-inputs (list pybind11
                             python-clarabel
			     python-ecos
                             python-numpy
                             python-osqp
                             python-scipy
                             python-scs))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/cvxpy/cvxpy")
    (synopsis "DSL for modeling convex optimization problems")
    (description
     "This package provides a domain-specific language for modeling convex
optimization problems in Python.")
    (license license:asl2.0)))

(define-public python-ecos
  (package
    (name "python-ecos")
    (version "2.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/embotech/ecos-python")
             (commit (string-append "v" version))
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16ljq8maflfkgbw16rldg6cy14vgz2pb3b2iga60i7yzkq2ikmyw"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-scipy))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/embotech/ecos")
    (synopsis "Embedded Cone Solver")
    (description
     "This is the Python package for ECOS: Embedded Cone Solver.  ECOS is
numerical software for solving convex second-order cone programs (SOCPs).")
    (license license:gpl3)))

(define-public python-formulaic
  (package
    (name "python-formulaic")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "formulaic" version))
       (sha256
        (base32 "18gvd3f2x358jj0df8vx5fhhnvzw047rsrs03vmvqnxaly97kpb4"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-astor
                             python-cached-property
                             python-interface-meta
                             python-numpy
                             python-pandas
                             python-scipy
                             python-typing-extensions
                             python-wrapt))
    (native-inputs (list python-hatchling python-hatch-vcs python-pytest))
    (home-page "https://github.com/matthewwardrop/formulaic")
    (synopsis "Implementation of Wilkinson formulas")
    (description "Formulaic is a high-performance implementation of Wilkinson
formulas for Python.")
    (license license:expat)))

(define-public python-imagehash
  (package
    (name "python-imagehash")
    (version "4.3.1")
    (source
     (origin
       (method git-fetch) ;no tests in PyPI
       (uri (git-reference
             ;; It's an effective and maintained fork of
             ;; <https://github.com/bunchesofdonald/photohash> project, which
             ;; has the latest release in 2016.
             (url "https://github.com/JohannesBuchner/imagehash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lw9lxzrzy9s5v3xc35vmh97hlyavnla087fp19k77va6v8vbjjf"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy
           python-pillow
           python-pywavelets
           python-scipy))
    (home-page "https://github.com/JohannesBuchner/imagehash")
    (synopsis "Perceptual Image Hashing library")
    (description
     "This package implements a functionality to tell whether two images look
nearly identical.  The image hash algorithms (average, perceptual, difference,
wavelet) analyse the image structure on luminance (without color information).
The color hash algorithm analyses the color distribution and black & gray
fractions (without position information).

Features:
@itemize
@item average hashing
@item perceptual hashing
@item difference hashing
@item wavelet hashing
@item HSV color hashing (colorhash)
@item crop-resistant hashing
@end itemize")
    (license license:bsd-2)))

(define-public python-numdifftools
  (package
    (name "python-numdifftools")
    (version "0.9.41")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "numdifftools" version))
       (sha256
        (base32 "1d49wd5jqnl0500jyws0vb7nv4dy4bb5ml4z9qx1n8867k6hbxsf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--pyargs" "numdifftools"
              "-k" (string-join
                    ;; Tests failing with error: TypeError: a must be an array
                    ;; of real numbers, see
                    ;; <https://github.com/pbrod/numdifftools/issues/72>.
                    (list "not test_high_order_derivative"
                          "test_low_order_derivative_on_example_functions"
                          "test_sinx_div_x"
                          "test_complex_hessian_issue_35"

                          "numdifftools.fornberg.Taylor"
                          "numdifftools.fornberg.derivative"
                          "numdifftools.fornberg.taylor")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.py"
                ;; We can run tests without deprecated pytest-runner.
                (("setup_requires.*") "")))))))
    (native-inputs
     (list python-algopy
           python-line-profiler
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-statsmodels
           python-wheel))
    (propagated-inputs
     (list python-numpy
           python-scipy))
    (home-page "https://github.com/pbrod/numdifftools")
    (synopsis "Solves automatic numerical differentiation problems")
    (description
     "This package implements a functionality to solve automatic numerical
differentiation problems in one or more variables.  Finite differences are
used in an adaptive manner, coupled with a Richardson extrapolation
methodology to provide a maximally accurate result.  The user can configure
many options like; changing the order of the method or the extrapolation, even
allowing the user to specify whether complex-step, central, forward or
backward differences are used.")
    (license license:bsd-3)))

(define-public python-osqp
  (package
    (name "python-osqp")
    (version "0.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/osqp/osqp-python")
             (commit (string-append "v" version))
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s1nbzkfsi2h4ji3v0k14pfcrvinakrwy4xdbz320lbaq3yb0b65"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Some of these test failures are explained by
      ;; https://github.com/osqp/osqp-python/issues/121.
      ;; These tests require the module "vec_emosqp", which we don't have.
      '(list "--ignore=src/osqp/tests/codegen_vectors_test.py"
             ;; These tests need "mat_emosqp".
             "--ignore=src/osqp/tests/codegen_matrices_test.py"
             ;; These fail with accuracy differences
             "--ignore=src/osqp/tests/update_matrices_test.py"
             "--ignore=src/osqp/tests/feasibility_test.py"
             "--ignore=src/osqp/tests/polishing_test.py"
             ;; This requires the nonfree MKL.
             "--ignore=src/osqp/tests/mkl_pardiso_test.py")
      #:phases
      #~(modify-phases %standard-phases
          ;; It looks like the upgrade to scipy 1.12.0 only broke the test
          ;; suite, not the features of this library.  See
          ;; https://github.com/osqp/osqp-python/issues/121.
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "requirements.txt"
                (("scipy.*1.12.0") "scipy <= 1.12.0"))))
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (propagated-inputs (list python-numpy python-qdldl python-scipy))
    ;; We need setuptools-scm only for the version number.  Without it the
    ;; version number will be "0.0.0" and downstream packages will complain.
    (native-inputs
     (list cmake-minimal
           python-pytest
           python-setuptools-scm
           python-setuptools
           python-wheel))
    (home-page "https://osqp.org/")
    (synopsis "OSQP: operator splitting QP solver")
    (description "The OSQP (Operator Splitting Quadratic Program) solver is a
numerical optimization package.")
    (license license:asl2.0)))

(define-public python-pint
  (package
    (name "python-pint")
    (version "0.24.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pint" version))
       (sha256
        (base32 "100vp5jg2sqj5wxaflj1rqjv2pk4fd55l2h2sdn7m0vlnlwm89rm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--ignore=pint/testsuite/benchmarks")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; PermissionError: [Errno 13] Permission denied:
              ;; '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-pytest-mpl
           python-pytest-subtests
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-flexcache
           python-flexparser
           python-platformdirs
           python-typing-extensions))
    (home-page "https://github.com/hgrecco/pint")
    (synopsis "Physical quantities module")
    (description
     "Pint is a Python package to define, operate and manipulate physical
quantities: the product of a numerical value and a unit of measurement.  It
allows arithmetic operations between them and conversions from and to
different units.")
    (license license:bsd-3)))

(define-public python-qdldl
  (package
    (name "python-qdldl")
    (version "0.1.7.post2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qdldl" version))
       (sha256
        (base32 "1lspam0k8gnw1yglqxvdv350fq00nkgdfmkizmx7bk0hxjjkj5ab"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list cmake-minimal
           pybind11
           python-setuptools
           python-wheel))
    (propagated-inputs (list python-numpy python-scipy))
    (home-page "https://github.com/oxfordcontrol/qdldl-python/")
    (synopsis "QDLDL LDL factorization routine")
    (description "This package provides a Python interface to the QDLDL LDL
factorization routine for quasi-definite linear system.")
    (license license:asl2.0)))

(define-public python-scipy
  (package
    (name "python-scipy")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scipy" version))
       (sha256
        (base32 "18rn15wg3lp58z204fbjjhy0h79c53yg3c4qqs9h3liniamspxab"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Step out of the source directory to avoid interference.
                (with-directory-excursion "/tmp"
                  (invoke "python" "-c"
                          (string-append
                           "import scipy; scipy.test('fast', parallel="
                           (number->string (parallel-job-count))
                           ", verbose=2)"))))))
          (add-after 'check 'install-doc
            (lambda* (#:key outputs #:allow-other-keys)
              ;; FIXME: Documentation cannot be built because it requires
              ;; a newer version of pydata-sphinx-theme, which currently
              ;; cannot build without internet access:
              ;; <https://github.com/pydata/pydata-sphinx-theme/issues/628>.
              ;; Keep the phase for easy testing.
              (let ((sphinx-build (false-if-exception
                                   (search-input-file input "bin/sphinx-build"))))
                (if sphinx-build
                    (let* ((doc (assoc-ref outputs "doc"))
                           (data (string-append doc "/share"))
                           (docdir (string-append
                                    data "/doc/"
                                    #$(package-name this-package) "-"
                                    #$(package-version this-package)))
                           (html (string-append docdir "/html")))
                      (with-directory-excursion "doc"
                        ;; Build doc.
                        (invoke "make" "html"
                                ;; Building the documentation takes a very long time.
                                ;; Parallelize it.
                                (string-append "SPHINXOPTS=-j"
                                               (number->string (parallel-job-count))))
                        ;; Install doc.
                        (mkdir-p html)
                        (copy-recursively "build/html" html)))
                    (format #t "sphinx-build not found, skipping~%"))))))))
    (propagated-inputs
     (append
       (if (supported-package? python-jupytext)  ; Depends on pandoc.
           (list python-jupytext)
           '())
       (list python-matplotlib
             python-mpmath
             python-mypy
             python-numpy
             python-numpydoc
             python-pydata-sphinx-theme
             python-pydevtool
             python-pythran
             python-rich-click
             python-sphinx
             python-threadpoolctl
             python-typing-extensions)))
    (inputs (list openblas pybind11-2.10))
    (native-inputs
     (list gfortran
           ;; XXX: Adding gfortran shadows GCC headers, causing a compilation
           ;; failure.  Somehow also providing GCC works around it ...
           gcc
           meson-python
           pkg-config
           python-click
           python-cython-0.29.35
           python-doit
           python-hypothesis
           python-pooch
           python-pycodestyle
           python-pydevtool
           python-pytest
           python-pytest-cov
           python-pytest-timeout
           python-pytest-xdist))
    (home-page "https://scipy.org/")
    (synopsis "The Scipy library provides efficient numerical routines")
    (description "The SciPy library is one of the core packages that make up
the SciPy stack.  It provides many user-friendly and efficient numerical
routines such as routines for numerical integration and optimization.")
    (license license:bsd-3)))

(define-public python-scikit-allel
  (package
    (name "python-scikit-allel")
    (version "1.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit_allel" version))
       (sha256
        (base32 "0d9yadzhsjjqkh6rz273f53iwczk0c7pv9dajzcrmfnk036b8f4s"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             (string-append
              ;; AttributeError: 'Dataset' object has no attribute 'asstr'
              "not test_vcf_to_hdf5"
              " and not test_vcf_to_hdf5_exclude"
              " and not test_vcf_to_hdf5_rename"
              " and not test_vcf_to_hdf5_group"
              " and not test_vcf_to_hdf5_ann"
              ;; Does not work with recent hmmlearn
              " and not test_roh_mhmm_0pct"
              " and not test_roh_mhmm_100pct"))
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'build-ext
           (lambda _
             (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-dask python-numpy))
    (native-inputs
     (list htslib
           python-cython
           python-h5py
           python-hmmlearn
           python-numexpr
           python-numpy
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel
           python-zarr))
    (home-page "https://github.com/cggh/scikit-allel")
    (synopsis "Explore and analyze genetic variation data")
    (description
     "This package provides utilities for exploratory analysis of large scale
genetic variation data.")
    (license license:expat)))

(define-public python-scikit-build-core
  (package
    (name "python-scikit-build-core")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit_build_core" version))
       (sha256
        (base32 "0baaava7jvc69r5j803vjxvf2cnx0f3gjhqalipp7l4d1cgwg3vp"))))
    (build-system pyproject-build-system)
    ;; Tests are aborted with the admonition: "setup.py install is
    ;; deprecated. Use build and pip and other standards-based tools."
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-exceptiongroup
                             python-importlib-metadata
                             python-importlib-resources
                             python-packaging
                             python-pathspec
                             python-tomli
                             python-typing-extensions))
    (native-inputs (list pybind11
                         python-cattrs
                         python-fastjsonschema
                         python-hatch-fancy-pypi-readme
                         python-hatch-vcs
                         python-hatchling
                         python-numpy
                         python-pip
                         python-pypa-build
                         python-pytest
                         python-pytest-subprocess
                         python-rich
                         python-setuptools
                         python-setuptools-scm
                         python-virtualenv
                         python-wheel))
    (home-page "https://github.com/scikit-build/scikit-build-core")
    (synopsis "Build backend for CMake based projects")
    (description "Scikit-build-core is a build backend for Python that uses
CMake to build extension modules.  It has a simple yet powerful static
configuration system in pyproject.toml, and supports almost unlimited
flexibility via CMake.  It was initially developed to support the demanding
needs of scientific users, but can build any sort of package that uses
CMake.")
    (license license:asl2.0)))

(define-public python-scikit-fem
  (package
    (name "python-scikit-fem")
    (version "10.0.2")
    (source
     (origin
       (method git-fetch)        ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/kinnala/scikit-fem")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10kvzm4fmazsrddd83m0903wan67fkj13vdp6w1iw6wm6a0b5h28"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list
                      ;; Tests require Jax.
                      "--ignore=tests/test_autodiff.py"
                      "--ignore=tests/test_examples.py")))
    (native-inputs
     (list python-autograd
           python-pyamg
           ;; python-jax ; not packed yet
           python-pytest
           python-shapely
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-meshio
           python-numpy
           python-scipy))
    (home-page "https://scikit-fem.readthedocs.io/en/latest/")
    (synopsis "Library for performing finite element assembly")
    (description
     "@code{scikit-fem} is a library for performing finite element assembly.
Its main purpose is the transformation of bilinear forms into sparse matrices
and linear forms into vectors.")
    (license license:bsd-3)))

(define-public python-scikit-fuzzy
  (package
    (name "python-scikit-fuzzy")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit_fuzzy" version))
       (sha256
        (base32 "0zsfyd8cpd2l82fwh3smxbwhb3bkqwlq17cbav53axma4c2k9r9f"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--pyargs" "skfuzzy")))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-networkx python-numpy python-scipy))
    (home-page "https://github.com/scikit-fuzzy/scikit-fuzzy")
    (synopsis "Fuzzy logic toolkit for SciPy")
    (description
     "This package implements many useful tools for projects involving fuzzy
logic, also known as grey logic.")
    (license license:bsd-3)))

(define-public python-scikit-image
  (package
    (name "python-scikit-image")
    (version "0.23.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scikit-image/scikit-image")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bc8i57sjk44vd9k1ilr6fpvfq1zbq9yfi22lz22k26mzrlisym3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Disable flaky test
      #:test-flags #~(list "-k" "not test_ellipse_parameter_stability")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'change-home-dir
            (lambda _
              ;; Change from /homeless-shelter to /tmp for write permission.
              (setenv "HOME" "/tmp")))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion "/tmp"
                  (apply invoke "pytest" "-v" "--doctest-modules"
                         (append test-flags (list #$output))))))))))
    ;; See requirements/ for the list of build and run time requirements.
    ;; NOTE: scikit-image has an optional dependency on python-pooch, however
    ;; propagating it would enable many more tests that require online data.
    (propagated-inputs
     (list python-cloudpickle
           python-dask
           python-imageio
           python-lazy-loader
           python-matplotlib
           python-networkx
           python-numpy
           python-pillow
           python-pythran
           python-pywavelets
           python-scipy
           python-tifffile))
    (native-inputs
     (list meson-python
           python-cython
           python-numpydoc
           python-packaging
           python-pytest
           python-pytest-localserver
           python-wheel))
    (home-page "https://scikit-image.org/")
    (synopsis "Image processing in Python")
    (description
     "Scikit-image is a collection of algorithms for image processing.")
    (license license:bsd-3)))

(define-public python-scikit-misc
  (package
    (name "python-scikit-misc")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit_misc" version))
       (sha256
        (base32 "18sj7qa3kk4pqh3rzg2c64lf03nciv9cf985yh1h2kpqqndgdhf5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "--pyargs" "skmisc")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-version
            (lambda _
              (call-with-output-file "skmisc/_version.py"
                (lambda (port)
                  (display (string-append "__version__ = \"" #$version "\"")
                           port)))
              (substitute* "meson.build"
                (("^  version: run_command.*")
                 (string-append "  version: '" #$version "',\n")))
              (substitute* "pyproject.toml"
                (("dynamic = \\['version'\\]")
                 (string-append "version = \"" #$version "\""))))))))
    (propagated-inputs (list meson-python
                             python-numpy
                             python-numpydoc
                             python-spin
                             python-twine))
    (native-inputs (list gfortran
                         pkg-config
                         python-cython-3
                         python-meson-python
                         python-numpy
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-wheel))
    (home-page "https://has2k1.github.io/scikit-misc/stable")
    (synopsis "Miscellaneous tools for scientific computing.")
    (description "This package provides miscellaneous tools for data analysis
and scientific computing.")
    (license license:bsd-3)))

(define-public python-boost-histogram
  (package
    (name "python-boost-histogram")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "boost_histogram" version))
       (sha256
        (base32 "0p2f90p5jwlwrjz3hq2fzaifkmny33g2mpi89nnhi3w41f1jxr2i"))))
    (build-system pyproject-build-system)
    ;; This package bundles files from Boost::Histogram and doesn't provide
    ;; a way to use a system library.
    (propagated-inputs (list python-numpy))
    (native-inputs (list cmake-minimal
                         pybind11
                         python-pytest
                         python-pytest-benchmark
                         python-scikit-build-core
                         python-setuptools-scm))
    (home-page "https://boost-histogram.readthedocs.io/en/latest/")
    (synopsis "Python bindings for the Boost::Histogram library")
    (description
     "This package provides Python bindings for the Boost::Histogram library,
one of the fastest libraries for histogramming.")
    (license license:bsd-3)))

(define-public python-scikit-opt
  (package
    (name "python-scikit-opt")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit-opt" version))
       (sha256
        (base32 "0ycqizgsj7q57asc1bphzhf1fx9zqn0vx5rli7q541bas64hfqiy"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-pytorch python-scipy))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/guofei9987/scikit-opt")
    (synopsis "Swarm intelligence algorithms in Python")
    (description
     "Scikit-opt (or sko) is a Python module implementing @dfn{swarm
intelligence} algorithms: genetic algorithm, particle swarm optimization,
simulated annealing, ant colony algorithm, immune algorithm, artificial fish
swarm algorithm.")
    (license license:expat)))

(define-public python-scikit-optimize
  (package
    (name "python-scikit-optimize")
    (version "0.10.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/holgern/scikit-optimize")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pc6avzxz8l32km5jvv3maih0a5x2akxybvxl2hdg04qz2l0kz8b"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-joblib
           python-matplotlib
           python-numpy
           python-pyaml
           python-scikit-learn
           python-scipy))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://scikit-optimize.github.io/")
    (synopsis "Sequential model-based optimization toolbox")
    (description "Scikit-Optimize, or @code{skopt}, is a simple and efficient
library to minimize (very) expensive and noisy black-box functions.  It
implements several methods for sequential model-based optimization.
@code{skopt} aims to be accessible and easy to use in many contexts.")
    (license license:bsd-3)))

(define-public python-scikit-surprise
  (package
    (name "python-scikit-surprise")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NicolasHug/Surprise")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15ckx2i41vs21sa3yqyj12zr0h4zrcdf3lrwcy2c1cq2bjq7mnvz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              ;; Change from /homeless-shelter to /tmp for write
              ;; permission.
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-cython-3
           python-pandas
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-joblib
           python-numpy
           python-scikit-learn))
    (home-page "https://surpriselib.com/")
    (synopsis "Recommender system library for Scikit-learn")
    (description
     "This package provides a Python library for building and analyzing
recommender systems that deal with explicit rating data.  It was designed with
the following purposes in mind:
@itemize
@item Provide tools to handle downloaded or user-provided datasets.
@item Provide ready-to-use prediction algorithms and similarity measures.
@item Provide a base for creating custom algorithms.
@item Provide tools to evaluate, analyse and compare algorithm performance.
@item Provide documentation with precise details regarding library algorithms.
@end itemize")
    (license license:bsd-3)))

(define-public python-scikit-survival
  (let ((revision "1")
        ;; We need a later commit for support of a more recent sklearn and
        ;; numpy 2.
        (commit "bceb53ebb8306f959c70fae2be9d552f33dd3f21"))
    (package
      (name "python-scikit-survival")
      (version (git-version "0.22.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sebp/scikit-survival")
               (commit commit)
               ;; This package contains a copy of Eigen.  It would be good to
               ;; figure out how to use our own Eigen package.
               (recursive? #true)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1m3z64nv4sgay0mdrrw4q4z5ylx63a9w2x43w1r4g8kpg7z9rdfc"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-version
              (lambda _
                (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                        #$(version-major+minor version)))))))
      (propagated-inputs
       (list python-ecos
             python-importlib-resources
             python-joblib
             python-numexpr
             python-numpy
             python-osqp
             python-pandas
             python-scikit-learn
             python-scipy))
      (native-inputs
       (list python-black
             python-pypa-build
             python-coverage
             python-cython-3
             python-packaging
             python-pytest
             python-setuptools
             python-setuptools-scm
             python-tomli
             python-tox))
      (home-page "https://github.com/sebp/scikit-survival")
      (synopsis "Survival analysis built on top of scikit-learn")
      (description "Scikit-survival is a Python module for survival analysis
built on top of scikit-learn.  It allows doing survival analysis while
utilizing the power of scikit-learn, e.g., for pre-processing or doing
cross-validation.")
      (license license:gpl3+))))

(define-public python-snakemake-interface-common
  (package
    (name "python-snakemake-interface-common")
    (version "1.17.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snakemake/snakemake-interface-common")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19fyqs048zdvrmq5sdayzch850kwsyv2x6xn57cjjzcm4zpjrh9w"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "python3" "tests/tests.py")))))))
    (native-inputs (list python-poetry-core python-pytest))
    (propagated-inputs (list python-argparse-dataclass python-configargparse))
    (home-page "https://github.com/snakemake/snakemake-interface-common")
    (synopsis "Common functions and classes for Snakemake and its plugins")
    (description "This package provides common functions and classes
for Snakemake and its plugins.")
    (license license:expat)))

(define-public python-snakemake-interface-executor-plugins
  (package
    (name "python-snakemake-interface-executor-plugins")
    (version "9.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://github.com/snakemake/"
                                 "snakemake-interface-executor-plugins"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kjjcgkk1rbavb687x5ayw35ayhsnhpg9262k317x911wqpsj2fm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python3" "tests/tests.py")))))))
    (propagated-inputs (list python-argparse-dataclass
                             python-snakemake-interface-common
                             python-throttler))
    (native-inputs (list python-poetry-core python-pytest))
    (home-page (string-append "https://github.com/snakemake/"
                              "python-snakemake-interface-executor-plugins"))
    (synopsis "Interface for Snakemake executor plugins")
    (description
     "This package provides a stable interface for interactions between Snakemake and
its executor plugins.")
    (license license:expat)))

(define-public python-snakemake-interface-report-plugins
  (package
    (name "python-snakemake-interface-report-plugins")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://github.com/snakemake/"
                                 "snakemake-interface-report-plugins"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i6z9vk6nv2m3jsym0glrb7h9isdlfza2yq14vbqcslybdi9ykfa"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;circular dependency on snakemake
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python3" "tests/tests.py")))))))
    (propagated-inputs (list python-snakemake-interface-common python-pytest))
    (native-inputs (list python-poetry-core))
    (home-page (string-append "https://github.com/snakemake/"
                              "python-snakemake-interface-report-plugins"))
    (synopsis "Interface for Snakemake report plugins")
    (description "This package provides a stable interface for interactions
between Snakemake and its report plugins.")
    (license license:expat)))

(define-public python-snakemake-interface-software-deployment-plugins
  (package
    (name "python-snakemake-interface-software-deployment-plugins")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://github.com/snakemake/"
                   "snakemake-interface-software-deployment-plugins"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b4kkznfyfck9f92pkimhyl13ljisfn67rsilm1a5inq2ywpmxba"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python3" "tests/tests.py")))))))
    (propagated-inputs (list python-argparse-dataclass
                             python-snakemake-interface-common))
    (native-inputs (list python-poetry-core))
    (home-page (string-append "https://github.com/snakemake/"
                "snakemake-interface-software-deployment-plugins"))
    (synopsis "Interface for Snakemake software deployment plugins")
    (description
     "This package provides a stable interface for interactions between Snakemake and
its software deployment plugins.")
    (license license:expat)))

(define-public python-snakemake-interface-storage-plugins
  (package
    (name "python-snakemake-interface-storage-plugins")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://github.com/snakemake/"
                                 "snakemake-interface-storage-plugins"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05n5xgwagb01nyzi8xfvp0nvdfl24lxidgksm7k86p68n1rijd5a"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;circular dependency on snakemake
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python3" "tests/tests.py")))))))
    (propagated-inputs (list python-reretry python-snakemake-interface-common
                             python-throttler python-wrapt))
    (native-inputs (list python-poetry-core python-pytest))
    (home-page (string-append "https://github.com/snakemake/"
                              "snakemake-interface-storage-plugins"))
    (synopsis "Interface for Snakemake storage plugins")
    (description
     "This package provides a stable interface for interactions between
Snakemake and its storage plugins.")
    (license license:expat)))

(define-public python-uhi
  (package
    (name "python-uhi")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "uhi" version))
       (sha256
        (base32 "0753b7yw0zi06g4azafnk3w8i3q6js9i6wwg3pya464gygrbnncm"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-boost-histogram
                         python-fastjsonschema
                         python-hatch-vcs
                         python-hatchling
                         python-pytest))
    (home-page "https://github.com/scikit-hep/uhi")
    (synopsis "Universal Histogram Interface")
    (description "This is a package meant primarily for documenting histogram
indexing and the PlottableHistogram Protocol and any future cross-library
standards.  It also contains the code for the PlottableHistogram Protocol, to
be used in type checking libraries wanting to conform to the protocol.  It is
not usually a runtime dependency, but only a type checking, testing, and/or
docs dependency in support of other libraries.")
    (license license:bsd-3)))

(define-public python-tdda
  (package
    (name "python-tdda")
    (version "2.2.17")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tdda" version))
       (sha256
        (base32 "1l2ph60m20ii4ljgd81wccpp5p8p2m81irr97k7850s2l1qnikcw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         ;; One test fails with error: AssertionError: False is not true : 5
         ;; lines are different, starting at line 1
         "--deselect=tdda/test_tdda.py::TestOne::test_ddiff_values_output")
      #:phases
      #~(modify-phases %standard-phases
          ;; "datetime.UTC" is not availalbe in Python 3.10 but in
          ;; 3.11 it's present
          ;; <https://docs.python.org/3/library/datetime.html#datetime.UTC>.
          (add-after 'unpack 'fix-Python3.11-datetime.UTC
            (lambda _
            (substitute* (find-files "." "\\.py")
              (("datetime.UTC")
               "datetime.timezone.utc")))))))
    (native-inputs
     (list python-numpy
           python-chardet
           python-pandas
           python-pyarrow
           python-pytest
           python-rich
           python-setuptools
           python-wheel))
    (home-page "https://www.stochasticsolutions.com")
    (synopsis "Test-driven data analysis library for Python")
    (description
     "The TDDA Python module provides command-line and Python API support
for the overall process of data analysis, through tools that perform
reference testing, constraint discovery for data, automatic inference
of regular expressions from text data and automatic test generation.")
    (license license:expat))) ; MIT License

(define-public python-trimesh
  (package
    (name "python-trimesh")
    (version "4.5.3")
    (source
     (origin
       (method git-fetch) ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/mikedh/trimesh")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17fyapp8nffnnf95bmcvllvg41fjlpvlv6qndbm048hnyayixxld"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; XXX: When more optional modules are available review
      ;; disabled tests once again.
      ;;
      ;; Disable tests requiring optional, not packed modules.
      #~(list "-k" (string-join
                    (list "not test_bezier_example"
                          "test_discrete"
                          "test_dxf"
                          "test_ply_path_bezier"
                          "test_ply_path_line"
                          "test_ply_path_multi"
                          "test_revolve"
                          "test_screw"
                          "test_simple_closed"
                          "test_simple_extrude"
                          "test_simple_open"
                          "test_slice_onplane"
                          "test_spline_3D"
                          "test_svg"
                          ;; Following tests require network:
                          "test_fuze"
                          "test_remote")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: It struggles to load and fails with error: AttributeError:
          ;; module 'trimesh' has no attribute '__main__'.
          (delete 'sanity-check))))
    (native-inputs
     (list python-coveralls
           python-pyinstrument
           python-pytest))
    (propagated-inputs
     (list ;; python-cascadio       ; not packed yet, optional
           python-chardet
           python-colorlog
           python-httpx
           python-jsonschema
           python-lxml
           ;; python-mapbox-earcut  ; not packed yet, optional
           ;; python-manifold3d     ; not packed yet, optional
           python-meshio
           python-networkx
           python-numpy
           ;; python-openctm        ; not packed yet, optional
           python-pillow
           python-psutil
           python-pycollada
           python-pyglet
           python-requests
           python-rtree
           python-scikit-image
           python-scipy
           python-setuptools
           python-shapely
           ;; python-svg-path       ; not packed yet, optional
           python-sympy
           ;; python-vhacdx         ; not packed yet, optional
           ;; python-xatlas         ; not packed yet, optional
           python-xxhash))
    (home-page "https://github.com/mikedh/trimesh")
    (synopsis "Python library for loading and using triangular meshes")
    (description
     "Trimesh is a pure Python library for loading and using triangular meshes
with an emphasis on watertight surfaces.  The goal of the library is to
provide a full featured and well tested Trimesh object which allows for easy
manipulation and analysis, in the style of the Polygon object in the Shapely
library.")
    (license license:expat)))

(define-public python-meshzoo
  (package
    (name "python-meshzoo")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/diego-hayashi/meshzoo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "107byfppbq16fqyp2hw7ydcvvahspzq0hzvlvzqg2zxi1aigbr68"))))
    (build-system pyproject-build-system)
    (propagated-inputs
      (list python-numpy))
    (native-inputs (list python-flit-core python-matplotlib python-pytest))
    (home-page "https://github.com/diego-hayashi/meshzoo")
    (synopsis "Mesh generator for simple geometries")
    (description
      "@code{meshzoo} is a mesh generator for finite element or finite
volume computations for simple domains like regular polygons, disks,
spheres, cubes, etc.")
    (license license:gpl3+)))

(define-public python-mpl-scatter-density
  (package
    (name "python-mpl-scatter-density")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mpl_scatter_density" version))
       (sha256
        (base32 "0cynk1rk6k2xklgv69difphrz6id77x3xb58kbs4mc4q7z6bvfid"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; See <https://github.com/astrofrog/mpl-scatter-density/issues/42>.
      #:test-flags #~(list "-k" "not test_default_dpi")))
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-pytest-mpl
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-fast-histogram
           python-matplotlib
           python-numpy))
    (home-page "https://github.com/astrofrog/mpl-scatter-density")
    (synopsis "Matplotlib helpers to make density scatter plots")
    (description
     "This package provides functionality to make it easy to make scatter
density maps, both for interactive and non-interactive use.")
    (license license:bsd-2)))

(define-public python-mpsplines
  ;; No release on PyPI no git tag, use the latest commit.
  (let ((commit "4967655fca8f4d0fc0685486c8ec2f1fe2f199d2")
        (revision "0"))
    (package
      (name "python-mpsplines")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jararias/mpsplines")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wqfvjp1d6xzb07qnq72h76f0mx7za9pasgw3qp3ciaycmlkvdr3"))))
      (build-system pyproject-build-system)
      (arguments
       (list #:tests? #f)) ; no tests provided
      (native-inputs
       (list python-setuptools
             python-wheel))
      (propagated-inputs
       (list python-scipy
             python-numpy
             python-loguru))
      (home-page "https://github.com/jararias/mpsplines")
      (synopsis "Mean preserving interpolation with splines")
      (description
       "Thi package implements a functionality for mean-preserving
interpolation of 1D data (for example, time series) with splines.")
      (license license:bsd-3))))

(define-public python-pyamg
  (package
    (name "python-pyamg")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyamg" version))
              (modules '((guix build utils)))
              (snippet
               ;; Delete autogenerated files, regenerate in a phase.
               #~(begin
                   (for-each
                    (lambda (file)
                      (delete-file (string-append "pyamg/amg_core/" file)))
                    '("air_bind.cpp"
                      "evolution_strength_bind.cpp"
                      "graph_bind.cpp"
                      "krylov_bind.cpp"
                      "linalg_bind.cpp"
                      "relaxation_bind.cpp"
                      "ruge_stuben_bind.cpp"
                      "smoothed_aggregation_bind.cpp"
                      "tests/bind_examples_bind.cpp"))))
              (sha256
               (base32
                "0l3dliwynxyjvbgpmi2k8jqvkkw6fc00c8w69h6swhrkfh0ql12z"))))
    (arguments
     (list
      #:test-flags
      ;; Test installed package in order to find C++ modules.
      #~(list "--pyargs" "pyamg.tests")
      #:phases
      #~(modify-phases %standard-phases
          ;; Regenerate the autogenerated files.
          (add-after 'unpack 'amg-core-bind-them
            (lambda _
              ;; bindthem.py heavily depends on location to produce *_bind.cpp
              ;; file, make it available in tests as well.
              (copy-file "pyamg/amg_core/bindthem.py"
                         "pyamg/amg_core/tests/bindthem.py")
              (with-directory-excursion "pyamg/amg_core"
                (substitute* "bindthem.py"
                  (("/usr/bin/env python3") (which "python3")))
                (invoke "sh" "generate.sh"))
              (with-directory-excursion "pyamg/amg_core/tests"
                (invoke "python" "bindthem.py" "bind_examples.h")))))))
    (build-system pyproject-build-system)
    (native-inputs
     (list pybind11
           python-cppheaderparser
           python-pytest
           python-pyyaml
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs (list python-numpy python-scipy))
    (home-page "https://github.com/pyamg/pyamg")
    (synopsis "Algebraic Multigrid Solvers in Python")
    (description "PyAMG is a Python library of Algebraic Multigrid
(AMG) solvers. It features implementations of:
@itemize
@item Ruge-Stuben (RS) or Classical AMG
@item AMG based on Smoothed Aggregation (SA)
@item Adaptive Smoothed Aggregation (αSA)
@item Compatible Relaxation (CR)
@item Krylov methods such as CG, GMRES, FGMRES, BiCGStab, MINRES, etc.
@end itemize")
    (license license:expat)))

(define-public python-pyet
  (package
    (name "python-pyet")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyet" version))
       (sha256
        (base32 "1dblsx0bv1g453hcx5vwij1zgankwgwvhwllqkn47k578h038xvy"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-mock
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-pandas
           python-xarray))
    (home-page "https://github.com/pyet-org/pyet")
    (synopsis "Python package for evapotranspiration calculation")
    (description
     "This package provides a Python library for calculating
Evapotranspiration using various standard methods.")
    (license license:expat)))

(define-public python-nibabel
  (package
    (name "python-nibabel")
    (version "5.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nibabel" version))
       (sha256
        (base32 "16snprwgp7qzp51j3mx0zdn7brvx6r14ankldi24ny0w7d8adp0b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; AttributeError: 'dict' object has no attribute 'add'.
      #:test-flags #~(list "-k" "not test_first and not test_second")))
    (propagated-inputs (list python-importlib-resources python-numpy
                             python-packaging python-typing-extensions))
    (native-inputs (list python-hatch-vcs
                         python-hatchling
                         python-pytest
                         python-pytest-httpserver
                         python-pytest-xdist))
    (home-page "https://nipy.org/nibabel/")
    (synopsis "Read and write access to common neuroimaging file formats")
    (description
     "@code{nibabel} is a library that provides read and write access to
common neuroimaging file formats, including: ANALYZE (plain, SPM99, SPM2
and later), GIFTI, NIfTI1, NIfTI2, CIFTI-2, MINC1, MINC2, AFNI BRIK/HEAD,
ECAT and Philips PAR/REC.  In addition, NiBabel also supports FreeSurfer’s
MGH, geometry, annotation and morphometry files, and provides some limited
support for DICOM.")
    (license license:expat))) ; and other non-copyleft licenses

(define-public python-tspex
  (package
    (name "python-tspex")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tspex" version))
              (sha256
               (base32
                "0x64ki1nzhms2nb8xpng92bzh5chs850dvapr93pkg05rk22m6mv"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-matplotlib python-numpy python-pandas python-xlrd))
    (home-page "https://apcamargo.github.io/tspex/")
    (synopsis "Calculate tissue-specificity metrics for gene expression")
    (description
     "This package provides a Python package for calculating
tissue-specificity metrics for gene expression.")
    (license license:gpl3+)))

(define-public python-ndindex
  (package
    (name "python-ndindex")
    (version "1.7")                     ;newer versions require a newer numpy
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ndindex" version))
       (sha256
        (base32 "1lpgsagmgxzsas7g8yiv6wmyss8q57w92h70fn11rnpadsvx16xz"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-flags #~(list "-c" "/dev/null"))) ;avoid coverage
    (native-inputs
     (list python-cython
           python-numpy
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://quansight-labs.github.io/ndindex/")
    (synopsis "Python library for manipulating indices of ndarrays")
    (description "This package provides a Python library for manipulating
indices of @code{ndarrays}.")
    (license license:expat)))

(define-public python-pandas-1
  (package
    (name "python-pandas")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas" version))
       (sha256
        (base32 "1cdhngylzh352wx5s3sjyznn7a6kmjqcfg97hgqm5h3yb9zgv8vl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "--pyargs" "pandas"
             "-n" (number->string (parallel-job-count))
             "-m" "not slow and not network and not db"
             "-k"
             (string-append
              ;; TODO: Missing input
              "not TestS3"
              " and not s3"
              ;; No module named 'pandas.io.sas._sas'
              " and not test_read_expands_user_home_dir"
              " and not test_read_non_existent"
              ;; Unknown failures
              " and not test_switch_options"
              ;; These fail with: td64 doesn't return NotImplemented, see numpy#17017
              " and not test_nat_comparisons"
              ;; Crashes
              " and not test_bytes_exceed_2gb"
              ;; get_subplotspec() returns None; possibly related to
              ;; https://github.com/pandas-dev/pandas/issues/54577
              " and not test_plain_axes"
              ;; This test fails when run with pytest-xdist
              ;; (see https://github.com/pandas-dev/pandas/issues/39096).
              " and not test_memory_usage"))
      #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'patch-build-system
             (lambda _
               (substitute* "pyproject.toml"
                 ;; Not all data files are distributed with the tarball.
                 (("--strict-data-files ") "")
                 ;; Unknown property "asyncio_mode"
                 (("asyncio_mode = \"strict\"") ""))))
           (add-after 'unpack 'patch-which
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "pandas/io/clipboard/__init__.py"
                 (("^WHICH_CMD = .*")
                  (string-append "WHICH_CMD = \""
                                 (search-input-file inputs "/bin/which")
                                 "\"\n")))))
           (add-before 'check 'prepare-x
             (lambda _
               (system "Xvfb &")
               (setenv "DISPLAY" ":0")
               ;; xsel needs to write a log file.
               (setenv "HOME" "/tmp")))
           ;; The compiled libraries are only in the output at this point,
           ;; but they are needed to run tests.
           ;; FIXME: This should be handled by the pyargs pytest argument,
           ;; but is not for some reason.
           (add-before 'check 'pre-check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (copy-recursively
                (string-append (site-packages inputs outputs)
                               "/pandas/_libs")
                "pandas/_libs"))))))
    (propagated-inputs
     (list python-jinja2
           python-matplotlib
           python-numpy
           python-openpyxl
           python-pytz
           python-dateutil
           python-xlrd
           python-xlsxwriter))
    (inputs
     (list which xclip xsel))
    (native-inputs
     (list python-cython-0.29.35
           python-beautifulsoup4
           python-lxml
           python-html5lib
           python-pytest
           python-pytest-mock
           python-pytest-xdist
           python-setuptools
           python-wheel
           ;; Needed to test clipboard support.
           xorg-server-for-tests))
    (home-page "https://pandas.pydata.org")
    (synopsis "Data structures for data analysis, time series, and statistics")
    (description
     "Pandas is a Python package providing fast, flexible, and expressive data
structures designed to make working with structured (tabular,
multidimensional, potentially heterogeneous) and time series data both easy
and intuitive.  It aims to be the fundamental high-level building block for
doing practical, real world data analysis in Python.")
    (license license:bsd-3)))

(define-public python-pandas-2
  (package
    (name "python-pandas")
    (version "2.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pandas-dev/pandas")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00f6jnplwg7iffnxdm4hpfls0ncbarc23933xq1rm5nk5g8dcldx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--pyargs" "pandas"
              ;; "--exitfirst"
              ;; XXX The tests won't even start on my 16 core laptop, but they
              ;; start with 4 processes.
              "--numprocesses" (number->string (min 4 (parallel-job-count)))
              "-m" "not slow and not network and not db"
              ;; All tests errored.
              "--ignore=pandas/tests/io/test_clipboard.py"
              "-k" (string-join
                    (list
                     "not test_git_version"
                     "test_show_versions_console"
                     ;; Not testing ~ expansion.
                     "test_expand_user"
                     "test_get_handle_with_path"
                     ;; These test access the internet (see:
                     ;; https://github.com/pandas-dev/pandas/issues/45085).:
                     ;; pandas/tests/io/xml/test_xml.py::test_wrong_url[lxml]
                     ;; pandas/tests/io/xml/test_xml.py::test_wrong_url[etree]
                     "test_wrong_url"
                     ;; TODO: Missing input
                     "TestS3"
                     "s3"
                     ;; This test fails when run with pytest-xdist
                     ;; (see: https://github.com/pandas-dev/pandas/issues/39096).
                     "test_memory_usage"
                     "test_parsing_tzlocal_deprecated"
                     ;; PyArrow is optional.
                     "test_style_bar_with_pyarrow_NA_values"
                     "test_very_negative_exponent"
                     "test_usecols_no_header_pyarrow"
                     "test_scientific_no_exponent[pyarrow-None]"
                     "test_inspect_getmembers"
                     ;; SciPy introduces cycle, optional.
                     "test_savefig"
                     ;; It requires a fresh python-tzdata, including new
                     ;; timezones.
                     "test_repr"
                     ;; These tests should be skipped on 32bit systems:
                     ;; Cannot cast array data from dtype('int64') to dtype('int32')
                     #$@(if (not (target-64bit?))
                            #~("test_inf_bound_infinite_recursion"
                               "test_reindex_behavior_with_interval_index"
                               "test_repeating_interval_index_with_infs")
                            #~()))
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'version-set-by-guix
            (lambda _
              (with-output-to-file "_version.py"
                (lambda _
                  (display
                   (string-append "__version__ = \""
                                  #$(package-version this-package)
                                  "\""))))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" ".")
              ;; Skip tests that require lots of resources.
              (setenv "PANDAS_CI" "1")))
          (add-after 'unpack 'patch-which
            (lambda _
              (substitute* "pandas/io/clipboard/__init__.py"
                (("^WHICH_CMD = .*")
                 (string-append "WHICH_CMD = \""
                                #$(this-package-input "which")
                                "/bin/which\"\n")))))
          ;; The compiled libraries are only in the output at this point,
          ;; but they are needed to run tests.
          ;; FIXME: This should be handled by the pyargs pytest argument,
          ;; but is not for some reason.
          (add-before 'check 'pre-check
            (lambda _
              (copy-recursively
               (string-append #$output
                              "/lib/python3.11/site-packages/pandas/_libs")
               "pandas/_libs"))))))
    (propagated-inputs
     (list python-dateutil
           python-jinja2
           python-matplotlib
           python-numpy
           python-openpyxl
           python-pytz
           python-tzdata
           python-xlrd
           python-xlsxwriter))
    (inputs
     (list which xclip xsel))
    (native-inputs
     (list meson-python
           python-beautifulsoup4
           python-cython-3
           python-html5lib
           python-lxml
           python-matplotlib
           python-openpyxl
           python-pytest-asyncio
           python-pytest
           python-pytest-localserver
           python-pytest-mock
           python-pytest-xdist
           python-versioneer))
    (home-page "https://pandas.pydata.org")
    (synopsis "Data structures for data analysis, time series, and statistics")
    (description
     "Pandas is a Python package providing fast, flexible, and expressive data
structures designed to make working with structured (tabular,
multidimensional, potentially heterogeneous) and time series data both easy
and intuitive.  It aims to be the fundamental high-level building block for
doing practical, real world data analysis in Python.")
    (license license:bsd-3)))

(define-public python-pandas python-pandas-2)

(define-public python-pandas-stubs
  (package
    (name "python-pandas-stubs")
    ;; The versioning follows that of Pandas and uses the date of the
    ;; python-pandas-stubs release.
    (version "2.2.3.241126")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pandas-dev/pandas-stubs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xbvin2l7h8vq9g24n4n2l49pdxbi15qghq7zkhh567p3pbmvsyb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--ignore=tests/test_io.py" ;requires python-calamine
                           "-k"
                           (string-append
                            ;; The python-pyarrow package in Guix is built
                            ;; with ORC integration, but these tests fail with
                            ;; an abort in ORC because a timezone file is not
                            ;; in the expected location:
                            ;; https://github.com/apache/arrow/issues/40633
                            "not test_orc"
                            " and not test_orc_path"
                            " and not test_orc_buffer"
                            " and not test_orc_columns"
                            " and not test_orc_bytes"
                            " and not test_all_read_without_lxml_dtype_backend"

                            ;; Apparently "numpy.bool_" is not the same as the
                            ;; expected "bool".
                            " and not test_timedelta_cmp"
                            " and not test_timedelta_cmp_rhs"
                            " and not test_timestamp_cmp"
                            " and not test_timestamp_eq_ne_rhs"))
      #:phases
      '(modify-phases %standard-phases
         ;; We cannot yet upgrade numpy to 1.26 because numba needs numpy
         ;; >1.24.
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "pyproject.toml"
               (("numpy = \\{ version = \">=1.26.0\", python = \"<3.13\" \\}")
                "numpy = { version = \">=1.23.0\", python = \"<3.13\" }"))))
         (add-before 'check 'prepare-x
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0")
             ;; xsel needs to write a log file.
             (setenv "HOME" (getcwd)))))))
    (propagated-inputs (list python-types-pytz))
    ;; Add python-fastparquet to native inputs once it has been packaged. Its
    ;; tests will be skipped for now.
    (native-inputs (list python-lxml
                         python-matplotlib
                         python-odfpy
                         python-pandas
                         python-poetry-core
                         python-pyarrow
                         python-pyreadstat
                         python-pytest
                         python-scipy
                         python-sqlalchemy-2
                         python-tables
                         python-tabulate
                         python-xarray
                         ;; Needed to test clipboard support.
                         which
                         xclip
                         xorg-server-for-tests
                         xsel))
    (home-page "https://pandas.pydata.org")
    (synopsis "Type annotations for pandas")
    (description
     "This package contains public type stubs for @code{python-pandas},
following the convention of providing stubs in a separate package, as
specified in @acronym{PEP, Python Enhancement Proposal} 561.  The stubs cover
the most typical use cases of @code{python-pandas}.  In general, these stubs
are narrower than what is possibly allowed by @code{python-pandas}, but follow
a convention of suggesting best recommended practices for using
@code{python-pandas}.")
    (license license:bsd-3)))

(define-public python-pandarallel
  (package
    (name "python-pandarallel")
    (version "1.6.5")
    (source
     (origin
       (method git-fetch)        ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/nalepae/pandarallel/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r2wlxlwp4wia0vm15k4cp421mwa20k4k5g2ml01inprj8bl1p0p"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-n" (number->string (parallel-job-count)))))
    (propagated-inputs
     (list python-dill
           python-pandas
           python-psutil))
    (native-inputs
     (list python-mkdocs-material
           python-numpy
           python-pytest
           python-pytest-cov
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (home-page "https://nalepae.github.io/pandarallel/")
    (synopsis "Tool to parallelize Pandas operations across CPUs")
    (description
     "@code{pandarallel} allows any Pandas user to take advantage of their
multi-core computer, while Pandas uses only one core.  @code{pandarallel} also
offers nice progress bars (available on Notebook and terminal) to get an rough
idea of the remaining amount of computation to be done.")
    (license license:bsd-3)))

(define-public python-pandera
  (package
    (name "python-pandera")
    ;; FIXME: The latest version requires hypothesis >= 6.92.7, which can't be
    ;; picked from python-hypothesis-next for some reason.
    (version "0.18.0")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unionai-oss/pandera")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14b5aij5zjkwvsimg0v00qvp59mhhq7ljim4qghcn432vkg9gh47"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              "--ignore=tests/pyspark"
              "-k" (string-join
                    ;; Failed: DID NOT RAISE <class 'pandera.errors.SchemaError'>
                    (list "not test_from_records_validates_the_schema"
                          "test_init_pandas_dataframe_errors"
                          "test_schema_dtype_crs_without_coerce"
                          "test_schema_from_dataframe"
                          "test_schema_model"
                          "test_validate_coerce_on_init"
                          ;; multimethod.DispatchError: ('str_length: 0
                          ;; methods found', (<class
                          ;; 'pandas.core.series.Series'>, <class 'NoneType'>,
                          ;; <class 'int'>), [])
                          "test_succeeding"
                          "test_failing"
                          "test_failing_with_none"
                          ;; pandera.errors.SchemaError: Error while executing
                          ;; check function: KeyError("foo")
                          "test_check_groups"
                          ;; [pandas_series.py-plugin_mypy.ini-expected_errors13]
                          ;; - assert 1 == 2
                          "test_pandas_stubs_false_positives"
                          ;; TypeError: type 'Series' is not subscriptable
                          "test_pandas_modules_importable")
                    " and not "))))
    ;; Pandera comes with a lot of extras. We test as many as possible, but do
    ;; not include all of them in the propagated-inputs. Currently, we have to
    ;; skip the pyspark and io tests due to missing packages python-pyspark
    ;; and python-frictionless.
    (propagated-inputs (list python-hypothesis-next ;strategies extra
                             python-modin
                             python-multimethod
                             python-numpy
                             python-packaging
                             python-pandas
                             python-pandas-stubs ;mypy extra
                             python-pydantic-2
                             python-scipy ;hypotheses extra
                             python-typeguard
                             python-typing-inspect
                             python-wrapt))
    (native-inputs (list python-dask ;dask extra
                         python-fastapi ;fastapi extra
                         python-geopandas ;geopandas extra
                         python-pyarrow ;needed to run fastapi tests
                         python-pytest
                         python-pytest-asyncio
                         python-pytest-xdist
                         python-setuptools
                         python-sphinx
                         python-uvicorn ;needed to run fastapi tests
                         python-wheel))
    (home-page "https://github.com/unionai-oss/pandera")
    (synopsis "Perform data validation on dataframe-like objects")
    (description
     "@code{python-pandera} provides a flexible and expressive API for
performing data validation on dataframe-like objects to make data processing
pipelines more readable and robust.  Dataframes contain information that
@code{python-pandera} explicitly validates at runtime.  This is useful in
production-critical data pipelines or reproducible research settings.  With
@code{python-pandera}, you can:

@itemize
@item Define a schema once and use it to validate different dataframe types.
@item Check the types and properties of columns.
@item Perform more complex statistical validation like hypothesis testing.
@item Seamlessly integrate with existing data pipelines via function decorators.
@item Define dataframe models with the class-based API with pydantic-style syntax.
@item Synthesize data from schema objects for property-based testing.
@item Lazily validate dataframes so that all validation rules are executed.
@item Integrate with a rich ecosystem of tools like @code{python-pydantic},
@code{python-fastapi} and @code{python-mypy}.
@end itemize")
    (license license:expat)))

(define-public python-pyjanitor
  (package
    (name "python-pyjanitor")
    (version "0.27.0")
    (source
     (origin
       ;; The build requires the mkdocs directory for the description in
       ;; setup.py. This is not included in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyjanitor-devs/pyjanitor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04vsg189msv8frx4zhpcg61djl7wsfvibmz0zmiih4rvkgly2cdr"))))
    (build-system pyproject-build-system)
    ;; Pyjanitor has an extensive test suite. For quick debugging, the tests
    ;; marked turtle can be skipped using "-m" "not turtle".
    (arguments
     (list
      #:test-flags '(list
                     "-n" (number->string (parallel-job-count))
                     ;; Tries to connect to the internet.
                     "-k" (string-append "not test_is_connected"
                                         ;; Test files are not included
                                         " and not test_read_commandline_bad_cmd"
                                         ;; This fails due to differences in accuracy
                                         " and not test_jitter_results")
                     ;; Test files are not included
                     "--ignore=tests/io/test_read_csvs.py"
                     ;; PySpark has not been packaged yet.
                     "--ignore=tests/spark/functions/test_clean_names_spark.py"
                     "--ignore=tests/spark/functions/test_update_where_spark.py")
      #:phases
      #~(modify-phases %standard-phases
          ;; Pandas 2.1.1 does not offer the BME frequency.
          (add-after 'unpack 'pandas-compat
            (lambda _
              (substitute* '("tests/functions/test_select_rows.py"
                             "tests/functions/test_select_columns.py")
                (("freq=\"BME\"") "freq=\"BM\""))))
          (add-before 'check 'set-env-ci
            (lambda _
              ;; Some tests are skipped if the JANITOR_CI_MACHINE
              ;; variable is not set.
              (setenv "JANITOR_CI_MACHINE" "1"))))))
    (propagated-inputs (list python-multipledispatch
                             python-natsort
                             python-pandas-flavor
                             python-scipy
                             ;; Optional imports.
                             python-biopython ;biology submodule
                             python-unyt)) ;engineering submodule
    (native-inputs (list python-pytest
                         python-pytest-xdist
                         ;; Optional imports. We do not propagate them due to
                         ;; their size.
                         python-numba ;speedup of joins
                         python-setuptools
                         python-wheel
                         rdkit)) ;chemistry submodule
    (home-page "https://github.com/pyjanitor-devs/pyjanitor")
    (synopsis "Tools for cleaning and transforming pandas DataFrames")
    (description
     "@code{pyjanitor} provides a set of data cleaning routines for
@code{pandas} DataFrames.  These routines extend the method chaining API
defined by @code{pandas} for a subset of its methods.  Originally, this
package was a port of the R package by the same name and it is inspired by the
ease-of-use and expressiveness of the @code{dplyr} package.")
    (license license:expat)))

(define-public python-pymcubes
  (package
    (name "python-pymcubes")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch) ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/pmneila/PyMCubes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v2qhc4pwanx6a8k843mbh45yk77n3w63sy5lzk5c3q4pkvfj1b9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'prepare-test-environment
            (lambda _
              ;; FileNotFoundError: [Errno 2] No such file or directory:
              ;; 'output/test.obj'
              (mkdir "output")
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-pycollada
           python-numpy
           python-scipy))
    (home-page "https://github.com/pmneila/PyMCubes")
    (synopsis "Marching cubes for Python")
    (description
     "@code{PyMCubes} is an implementation of the marching cubes algorithm to
extract iso-surfaces from volumetric data.  The volumetric data can be given
as a three-dimensional @code{NumPy} array or as a Python function @code{f(x,
y, z)}.")
    (license license:bsd-3)))

(define-public python-pythran
  (package
    (name "python-pythran")
    (version "0.17.0")
    (home-page "https://github.com/serge-sans-paille/pythran")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1rm9lfbz5qvah1m0rr5gaaahkf1gzwlw1ysvym2l2yh0clglav94"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list (string-append "--numprocesses=" (number->string
                                               (parallel-job-count)))
             ;; XXX There are lots of tests of the format
             ;; pythran/tests/test_*.py, but they cannot easily be selected.
             "pythran/tests/test_typing.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Remove compiler flag that trips newer GCC:
                ;; https://github.com/serge-sans-paille/pythran/issues/908
                (substitute* "pythran/tests/__init__.py"
                  (("'-Wno-absolute-value',") ""))
                (setenv "HOME" (getcwd))
                ;; This setup is modelled after the upstream CI system.
                (call-with-output-file ".pythranrc"
                  (lambda (port)
                    (format port "[compiler]\nblas=openblas~%")))))))))
    (native-inputs
     ;; For tests.
     (list openblas
           python-pytest
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list boost xsimd                  ;headers need to be available
           python-beniget python-gast python-numpy python-ply))
    (synopsis "Ahead of Time compiler for numeric kernels")
    (description
     "Pythran is an ahead of time compiler for a subset of the Python
language, with a focus on scientific computing.  It takes a Python module
annotated with a few interface descriptions and turns it into a native
Python module with the same interface, but (hopefully) faster.")
    (license license:bsd-3)))

(define-public python-pyts
  (package
    (name "python-pyts")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyts" version))
              (sha256
               (base32
                "00pdzfkl0b4vhfdm8zas7b904jm2hhivdwv3wcmpik7l2p1yr85c"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-joblib python-numba python-numpy
           python-scikit-learn
           python-scipy))
    (native-inputs
     (list python-pytest python-pytest-cov python-setuptools
           python-wheel))
    (home-page "https://github.com/johannfaouzi/pyts")
    (synopsis "Python package for time series classification")
    (description
     "This package provides a Python package for time series classification.")
    (license license:bsd-3)))

(define-public python-bottleneck
  (package
    (name "python-bottleneck")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bottleneck" version))
       (sha256
        (base32 "1x29yj4yr12v646si63gkxj9b6lx1xk65536wqy4i9fyk4bqx3ps"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'rebuild-ext
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs (list python-pytest
                         python-setuptools
                         python-wheel))
    (propagated-inputs (list python-numpy))
    (home-page "https://github.com/pydata/bottleneck")
    (synopsis "Fast NumPy array functions written in C")
    (description
     "Bottleneck is a collection of fast, NaN-aware NumPy array functions
written in C.")
    (license license:bsd-2)))

(define-public python-numpoly
  (package
    (name "python-numpoly")
    (version "1.2.11")
    (source (origin
              (method git-fetch) ;; PyPI is missing some Pytest fixtures
              (uri (git-reference
                    (url "https://github.com/jonathf/numpoly")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01g21v91f4d66xd0bvap0n6d6485w2fnq1636gx6h2s42550rlbd"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-importlib-metadata python-numpy))
    (native-inputs (list python-pytest python-setuptools python-sympy
                         python-wheel))
    (home-page "https://numpoly.readthedocs.io/en/master/")
    (synopsis "Polynomials as a numpy datatype")
    (description "Numpoly is a generic library for creating, manipulating and
evaluating arrays of polynomials based on @code{numpy.ndarray objects}.")
    ;; Tests fail with dtype mismatches on 32-bit architectures, suggesting
    ;; that numpoly only supports 64 bit platforms.
    (supported-systems '("x86_64-linux" "aarch64-linux" "powerpc64le-linux"))
    (license license:bsd-2)))

(define-public python-spin
  (package
  (name "python-spin")
  (version "0.8")
  (source
   (origin
     (method url-fetch)
     (uri (pypi-uri "spin" version))
     (sha256
      (base32 "0ff48nagfaai3j26g1db4zq2bwdv6kj5l7xhcs2l9kzg7qzrmhr7"))))
  (build-system pyproject-build-system)
  (propagated-inputs (list python-click python-colorama python-tomli))
  (native-inputs (list python-pytest python-setuptools python-wheel))
  (home-page "https://github.com/scientific-python/spin")
  (synopsis "Developer tool for scientific Python libraries")
  (description "@code{spin} is a simple interface for common development
tasks.  It comes with a few common build commands out the box, but can
easily be customized per project.

The impetus behind developing the tool was the mass migration of scientific
Python libraries (SciPy, scikit-image, and NumPy, etc.) to Meson, after
distutils was deprecated.  When many of the build and installation commands
changed, it made sense to abstract away the nuisance of having to re-learn
them.")
  (license license:bsd-3)))

(define-public python-baycomp
  (package
    (name "python-baycomp")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "baycomp" version))
       (sha256
        (base32 "1c1354a7b3g8slychjgyjxqdm8z40z9kviyl9n4g9kfpdg0p4d64"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-matplotlib python-numpy python-scipy))
    (home-page "https://github.com/janezd/baycomp")
    (synopsis "Library for comparison of Bayesian classifiers")
    (description
     "Baycomp is a library for Bayesian comparison of classifiers.  Functions
in the library compare two classifiers on one or on multiple data sets.  They
compute three probabilities: the probability that the first classifier has
higher scores than the second, the probability that differences are within the
region of practical equivalence (rope), or that the second classifier has
higher scores.")
    (license license:expat)))

(define-public python-fast-histogram
  (package
    (name "python-fast-histogram")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fast_histogram" version))
       (sha256
        (base32 "1sk9xa85cgm4sylzblwv3qr2dmm0ic06zkwxqa2xlazjiawp629r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-hypothesis
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/astrofrog/fast-histogram")
    (synopsis "Fast simple 1D and 2D histograms")
    (description
     "The fast-histogram mini-package aims to provide simple and fast
histogram functions for regular bins that don't compromise on performance.  It
doesn't do anything complicated - it just implements a simple histogram
algorithm in C and keeps it simple.  The aim is to have functions that are
fast but also robust and reliable.  The result is a 1D histogram function here
that is 7-15x faster than @code{numpy.histogram}, and a 2D histogram function
that is 20-25x faster than @code{numpy.histogram2d}.")
    (license license:bsd-3)))

(define-public python-fastcluster
  (package
    (name "python-fastcluster")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fastcluster" version))
       (sha256
        (base32 "19labbgnq85p4r4jbli2p045lgh57larhi2g2anagfxnlzpqdf5a"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-scipy python-setuptools python-wheel))
    (home-page "https://danifold.net/fastcluster.html")
    (synopsis "Fast hierarchical clustering routines for R and Python")
    (description "The fastcluster package implements seven common hierarchical
clustering schemes efficiently.  The package is made with two interfaces to
standard software: R and Python.")
    (license license:bsd-2)))

(define-public python-fbpca
  (package
    (name "python-fbpca")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "fbpca" version))
              (sha256
               (base32
                "1lbjqhqsdmqk86lb86q3ywf7561zmdny1dfvgwqkyrkr4ij7f1hm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy python-scipy))
    (home-page "https://fbpca.readthedocs.io/")
    (synopsis "Functions for principal component analysis and accuracy checks")
    (description
     "This package provides fast computations for @dfn{principal component
analysis} (PCA), SVD, and eigendecompositions via randomized methods")
    (license license:bsd-3)))

(define-public python-geosketch
  (package
    (name "python-geosketch")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "geosketch" version))
              (sha256
               (base32
                "0knch5h0p8xpm8bi3b5mxyaf1ywwimrsdmbnc1xr5icidcv9gzmv"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ;there are none
    (propagated-inputs (list python-fbpca python-numpy python-scikit-learn))
    (home-page "https://github.com/brianhie/geosketch")
    (synopsis "Geometry-preserving random sampling")
    (description "geosketch is a Python package that implements the geometric
sketching algorithm described by Brian Hie, Hyunghoon Cho, Benjamin DeMeo,
Bryan Bryson, and Bonnie Berger in \"Geometric sketching compactly summarizes
the single-cell transcriptomic landscape\", Cell Systems (2019).  This package
provides an example implementation of the algorithm as well as scripts
necessary for reproducing the experiments in the paper.")
    (license license:expat)))

(define-public python-einops
  (package
    (name "python-einops")
    (version "0.6.1")
    (source (origin
              (method git-fetch) ;PyPI misses .ipynb files required for tests
              (uri (git-reference
                    (url "https://github.com/arogozhnikov/einops")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h8p39kd7ylg99mh620xr20hg7v78x1jnj6vxwk31rlw2dmv2dpr"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'set-backend
                          (lambda _
                            ;; Einops supports different backends, but we test
                            ;; only NumPy for availability and simplicity.
                            (setenv "EINOPS_TEST_BACKENDS" "numpy"))))))
    (native-inputs (list jupyter
                         python-hatchling
                         python-nbconvert
                         python-nbformat
                         python-parameterized
                         python-pytest))
    (propagated-inputs (list python-numpy))
    (home-page "https://einops.rocks/")
    (synopsis "Tensor operations for different backends")
    (description "Einops provides a set of tensor operations for NumPy and
multiple deep learning frameworks.")
    (license license:expat)))

(define-public python-xarray
  (package
    (name "python-xarray")
    (version "2023.12.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "xarray" version))
              (sha256
               (base32
                "0cyldwchcrmbm1y7l1ry70kk8zdh7frxci3c6iwf4iyyj34dnra5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This needs a more recent version of python-hypothesis
      '(list "--ignore=xarray/tests/test_strategies.py"
             ;; These are known to fail with Pandas 2
             "-k"
             (string-append "not test_datetime_conversion_warning"
                            " and not test_timedelta_conversion_warning"
                            ;; These expect deprecation warnings that are not
                            ;; emitted in our case.
                            " and not test_drop_index_labels"
                            " and not test_rename_multiindex"))))
    (native-inputs
     (list python-setuptools python-setuptools-scm python-pytest python-wheel))
    (propagated-inputs
     (list python-numpy python-packaging python-pandas))
    (home-page "https://github.com/pydata/xarray")
    (synopsis "N-D labeled arrays and datasets")
    (description "Xarray (formerly xray) makes working with labelled
multi-dimensional arrays simple, efficient, and fun!

Xarray introduces labels in the form of dimensions, coordinates and attributes
on top of raw NumPy-like arrays, which allows for a more intuitive, more
concise, and less error-prone developer experience.  The package includes a
large and growing library of domain-agnostic functions for advanced analytics
and visualization with these data structures.")
    (license license:asl2.0)))

(define-public python-xarray-dataclasses
  (package
    (name "python-xarray-dataclasses")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astropenguin/xarray-dataclasses/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "043lc1hadr5y0y16g682viiafy0hfsa7q18lqmndpyvnmcgm893z"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-typing-extensions
                             python-xarray))
    (native-inputs (list python-pytest python-poetry-core))
    (home-page "https://github.com/astropenguin/xarray-dataclasses/")
    (synopsis "Data creation made easy by dataclass")
    (description "@code{xarray-dataclasses} is a Python package that makes it
easy to create @code{xarray}'s @code{DataArray} and @code{Datase} objects that
are \"typed\" (i.e. fixed dimensions, data type, coordinates, attributes, and
name) using the Python's @code{dataclass}.")
    (license license:expat)))

;; Note that this package will be folded into xarray eventually.  See
;; https://github.com/pydata/xarray/issues/8572 for details.
(define-public python-xarray-datatree
  (package
    (name "python-xarray-datatree")
    (version "0.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xarray-datatree" version))
       (sha256
        (base32 "1x1s25s6dp1f2hck9qw8vl8hgkyy23rcwag2a9vd3w0dbgrrl5i6"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-packaging python-xarray))
    ;; We need setuptools-scm to correctly record the version string.
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel
           python-zarr))
    (home-page "https://github.com/xarray-contrib/datatree")
    (synopsis "Hierarchical tree-like data structures for xarray")
    (description "Datatree is a prototype implementation of a tree-like
hierarchical data structure for @code{xarray}.  Datatree is in the process of
being merged upstream into @code{xarray}.")
    (license license:asl2.0)))

(define-public python-xarray-einstats
  (package
    (name "python-xarray-einstats")
    (version "0.7.0")
    (source (origin
              (method git-fetch) ; no tests in PyPI
              (uri (git-reference
                    (url "https://github.com/arviz-devs/xarray-einstats")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14c424swpdginaz4pm3nmkizxy34x19q6xq3d4spx9s9031f6n3a"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-einops python-flit-core python-numba
                         python-pytest))
    (propagated-inputs (list python-numpy python-scipy python-xarray))
    (home-page "https://einstats.python.arviz.org/en/latest/")
    (synopsis "Stats, linear algebra and einops for xarray")
    (description
     "@code{xarray_einstats} provides wrappers around some NumPy and SciPy
functions and around einops with an API and features adapted to xarray.")
    (license license:asl2.0)))

(define-public python-xarray-schema
  (package
    (name "python-xarray-schema")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xarray-schema" version))
       (sha256
        (base32 "08194629696z98dkc74i6c9zmy1jicvd2ajb75q0lsf0i427cv4w"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-xarray))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/carbonplan/xarray-schema")
    (synopsis "Schema validation for Xarray objects")
    (description "This package implements schema validation for Xarray
objects.")
    (license license:expat)))

(define-public python-pytensor
  (package
    (name "python-pytensor")
    (version "2.28.3") ; the minimal version supporting SciPy 1.12.0
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pymc-devs/pytensor")
                    (commit (string-append "rel-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yz1yslms6kdmy4sgnvbnghhclcpkc80z3vaw9c2y3b3j1fs9b4v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; XXX: Full test suite takes about 20-30min to complete in single
      ;; thread, attempt to run tests in parallel with pytest-xdist fails even
      ;; so upstream provides a support for that, try to figure out how to
      ;; improve it.
      ;;
      ;; Upstream implements a script, showing slow tests which may be used to
      ;; exclude even more hanging/slow ones, see
      ;; <scripts/slowest_tests/extract-slow-tests.py>.
      ;;
      ;; Skip computationally intensive tests.
      #~(list "--ignore" "tests/scan/"
              "--ignore" "tests/tensor/"
              "--ignore" "tests/sandbox/"
              "--ignore" "tests/sparse/sandbox/"
              ;; Tests hang while running from these files.
              "--ignore" "tests/compile/test_compilelock.py"
              "--ignore" "tests/link/jax/test_tensor_basic.py"
              ;; XXX: Tests finish with error in these files, check why.
              "--ignore" "tests/compile/function/test_types.py"
              "--ignore" "tests/link/numba/test_basic.py"
              "--ignore" "tests/link/numba/test_blockwise.py"
              "--ignore" "tests/link/numba/test_elemwise.py"
              "-k" (string-join
                    ;; Skip benchmark tests.
                    (list "not test_elemwise_speed"
                          "test_logsumexp_benchmark"
                          "test_fused_elemwise_benchmark"
                          "test_scan_multiple_output"
                          "test_vector_taps_benchmark"
                          "test_cython_performance"
                          ;; Assertion fails in tests.
                          "test_choose_signature"
                          "test_fgraph_to_python_names")
                    " and not ")
              ;; Tests collection selects pytensor, which does not contain
              ;; tests and fails to pass; manually provide a test directory
              ;; instead.
              "tests")
      #:phases
      #~(modify-phases %standard-phases
          ;; Replace version manually because pytensor uses
          ;; versioneer, which requires git metadata.
          (add-after 'unpack 'versioneer
            (lambda _
              (invoke "versioneer" "install")
              (substitute* "setup.py"
                (("version=versioneer.get_version\\(),")
                 (format #f "version=~s," #$version)))))
          (add-before 'check 'pre-check
            (lambda _
              ;; It is required for most tests.
              (setenv "HOME" "/tmp")
              ;; Cython extensions have to be built before running the tests.
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs (list python-cython
                         python-pytest
                         python-pytest-mock
                         python-versioneer
                         python-setuptools
                         python-wheel))
    (propagated-inputs (list python-cons
                             python-etuples
                             python-filelock
                             python-logical-unification
                             python-minikanren
                             python-numba
                             python-numpy
                             python-scipy
                             python-typing-extensions))
    (home-page "https://pytensor.readthedocs.io/en/latest/")
    (synopsis
     "Library for mathematical expressions in multi-dimensional arrays")
    (description
     "PyTensor is a Python library that allows one to define, optimize, and
efficiently evaluate mathematical expressions involving multi-dimensional
arrays.  It is a fork of the Aesara library.")
    (license license:bsd-3)))

(define-public python-msgpack-numpy
  (package
    (name "python-msgpack-numpy")
    (version "0.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "msgpack-numpy" version))
       (sha256
        (base32
         "0sbfanbkfs6c77np4vz0ayrwnv99bpn5xgj5fnf2yhhk0lcd6ry6"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-msgpack python-numpy))
    (home-page "https://github.com/lebedov/msgpack-numpy")
    (synopsis
     "Numpy data serialization using msgpack")
    (description
     "This package provides encoding and decoding routines that enable the
serialization and deserialization of numerical and array data types provided
by numpy using the highly efficient @code{msgpack} format.  Serialization of
Python's native complex data types is also supported.")
    (license license:bsd-3)))

(define-public python-ruffus
  (package
    (name "python-ruffus")
    (version "2.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruffus" version))
       (sha256
        (base32
         "1ai673k1s94s8b6pyxai8mk17p6zvvyi87rl236fs6ls8mpdklvc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "ruffus/test"
                 (invoke "bash" "run_all_unit_tests3.cmd"))))))))
    (native-inputs
     (list python-pytest))
    (home-page "http://www.ruffus.org.uk")
    (synopsis "Light-weight computational pipeline management")
    (description
     "Ruffus is designed to allow scientific and other analyses to be
automated with the minimum of fuss and the least effort.")
    (license license:expat)))

(define-public python-scs
  (package
    (name "python-scs")
    (version "3.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bodono/scs-python")
             (commit "3.2.4")
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06wd8m3ri0gaddl7qq6243g25zjlnh3da915b73jnrfh7sg1nqsj"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list pkg-config
           python-pytest
           meson-python))
    (inputs
     (list openblas))
    (propagated-inputs
     (list python-numpy
           python-scipy))
    (home-page "https://github.com/bodono/scs-python")
    (synopsis "Splitting conic solver")
    (description "This package provides a Python interface for the
SCS (Splitting conic solver) library.")
    (license license:expat)))

(define-public python-statannot
  (package
    (name "python-statannot")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statannot" version))
       (sha256
        (base32
         "1f8c2sylzr7lpjbyqxsqlp9xi8rj3d8c9hfh98x4jbb83zxc4026"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy python-seaborn python-matplotlib python-pandas
           python-scipy))
    (home-page
     "https://github.com/webermarcolivier/statannot")
    (synopsis "Add annotations to existing plots generated by seaborn")
    (description
     "This is a Python package to compute statistical test and add statistical
annotations on an existing boxplots and barplots generated by seaborn.")
    (license license:expat)))

(define-public python-unyt
  (package
    (name "python-unyt")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "unyt" version))
       (sha256
        (base32 "0jrq2vhan2h280h6cw1sm5hys2nzmf19w4py64k3nrkc320z9mni"))))
    (build-system pyproject-build-system)
    (arguments
     ;; This is a Numpy DeprecationWarning, remove it on next update.
     (list #:test-flags ''("-k" "not test_h5_io")))
    ;; Pint is optional, but we do not propagate it due to its size.
    (native-inputs
     (list python-pint
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    ;; Astropy is an optional import, but we do not include it as it creates a
    ;; module cycle: astronomy->python-science->astronomy.
    (propagated-inputs
     (list python-h5py        ; optional import
           python-matplotlib  ; optional import
           python-numpy
           python-sympy))
    (home-page "https://unyt.readthedocs.io")
    (synopsis "Library for working with data that has physical units")
    (description
     "@code{unyt} is a Python library working with data that has physical
units.  It defines the @code{unyt.array.unyt_array} and
@code{unyt.array.unyt_quantity} classes (subclasses of NumPy’s ndarray class)
for handling arrays and scalars with units,respectively")
    (license license:bsd-3)))

(define-public python-upsetplot
  (package
    (name "python-upsetplot")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "UpSetPlot" version))
       (sha256
        (base32
         "14l5gcj88cclkj1mf74bcy1pxq1hgsiy27fa3vxrsk32ik1nmdwm"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-matplotlib python-pandas))
    (native-inputs
     (list python-pytest-runner python-pytest-cov
           python-setuptools python-wheel))
    (home-page "https://upsetplot.readthedocs.io")
    (synopsis "Draw UpSet plots with Pandas and Matplotlib")
    (description
     "This is a Python implementation of UpSet plots by Lex et al.
UpSet plots are used to visualize set overlaps; like Venn diagrams but more
readable.")
    (license license:bsd-3)))

(define-public python-vedo
  (package
    (name "python-vedo")
    (version "2025.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marcomusy/vedo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hrqyvcxxbc1wz0cnafc8rvsi5mj19kck4b6pmddh25rlhdcr5qb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: The whole test suite depends on the data from
      ;; <https://vedo.embl.es/examples> providing samples which need to be
      ;; downloaded during tests, find the way how to enable it.
      #:tests? #f
      #:phases
       #~(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           ;; vtk does not provide Python metadata.
           ;;
           ;; ...checking requirements: ERROR: vedo==2025.5.3
           ;; DistributionNotFound(Requirement.parse('vtk'), {'vedo'})
           (lambda _
             (substitute* "pyproject.toml"
               (("\"vtk\",") "")))))))
    (native-inputs
     (list pkg-config
           python-pkgconfig
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-deprecated
           python-matplotlib
           python-numpy
           python-pygments
           vtk))
    (home-page "https://github.com/marcomusy/vedo")
    (synopsis
     "Analysis and visualization of 3D objects and point clouds")
    (description
     "@code{vedo} is a fast and lightweight python module for
scientific analysis and visualization.  The package provides a wide
range of functionalities for working with three-dimensional meshes and
point clouds.  It can also be used to generate high quality
two-dimensional renderings such as scatter plots and histograms.
@code{vedo} is based on @code{vtk} and @code{numpy}.")
    ;; vedo is released under the Expat license.  Included fonts are
    ;; covered by the OFL license and textures by the CC0 license.
    ;; The earth images are in the public domain.
    (license (list license:expat
                   license:silofl1.1
                   license:cc0
                   license:public-domain))))

(define-public python-pandas-flavor
  (package
    (name "python-pandas-flavor")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas_flavor" version))
       (sha256
        (base32
         "0473lkbdnsag3w5x65sxwjlyq0i7z938ssxqwn2cpcml282vksx1"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-lazy-loader python-packaging python-pandas python-xarray))
    (home-page "https://github.com/pyjanitor-devs/pandas_flavor")
    (synopsis "Write your own flavor of Pandas")
    (description "Pandas 0.23 added a simple API for registering accessors
with Pandas objects.  Pandas-flavor extends Pandas' extension API by

@itemize
@item adding support for registering methods as well
@item making each of these functions backwards compatible with older versions
of Pandas
@end itemize")
    (license license:expat)))

(define-public python-pingouin
  (package
    (name "python-pingouin")
    (version "0.5.4")
    (source
     ;; The PyPI tarball does not contain the tests.
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/raphaelvallat/pingouin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1j3qkgvyc31604ddl952h4hwza7schg8kwkycmxvpvx7xjj7nn68"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This one fails due to minor differences in accuracy
      '(list "-k" "not test_logistic_regression")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'loosen-requirements
           (lambda _
             (substitute* '("requirements.txt" "setup.py")
               ;; Remove sklearn pinning since it works fine with 1.1.2:
               ;; https://github.com/raphaelvallat/pingouin/pull/300
               (("scikit-learn<1\\.1\\.0")
                "scikit-learn"))))
         ;; On loading, Pingouin uses the outdated package to check if a newer
         ;; version is available on PyPI. This check adds an extra dependency
         ;; and is irrelevant to Guix users. So, disable it.
         (add-after 'unpack 'remove-outdated-check
           (lambda _
             (substitute* "setup.py"
               (("\"outdated\",") ""))
             (substitute* "pingouin/__init__.py"
               (("^from outdated[^\n]*") "")
               (("^warn_if_outdated[^\n]*") ""))))
         (add-after 'unpack 'sklearn-compatibility
           (lambda _
             (substitute* "pingouin/regression.py"
               (("kwargs\\[\"penalty\"\\] = \"none\"")
                "kwargs[\"penalty\"] = None")))))))
    (native-inputs
     (list python-pytest python-pytest-cov))
    (propagated-inputs
     (list python-matplotlib
           python-mpmath
           python-numpy
           python-pandas
           python-pandas-flavor
           python-scikit-learn
           python-scipy
           python-seaborn
           python-statsmodels
           python-tabulate))
    (home-page "https://pingouin-stats.org/")
    (synopsis "Statistical package for Python")
    (description "Pingouin is a statistical package written in Python 3 and
based mostly on Pandas and NumPy.  Its features include

@itemize
@item ANOVAs: N-ways, repeated measures, mixed, ancova
@item Pairwise post-hocs tests (parametric and non-parametric) and pairwise
correlations
@item Robust, partial, distance and repeated measures correlations
@item Linear/logistic regression and mediation analysis
@item Bayes Factors
@item Multivariate tests
@item Reliability and consistency
@item Effect sizes and power analysis
@item Parametric/bootstrapped confidence intervals around an effect size or a
correlation coefficient
@item Circular statistics
@item Chi-squared tests
@item Plotting: Bland-Altman plot, Q-Q plot, paired plot, robust correlation,
and more
@end itemize")
    (license license:gpl3)))

(define-public python-dask-expr
  (package
    (name "python-dask-expr")
    (version "1.0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dask/dask-expr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c2q8w8wl5d2hycbjp9vavkl5f36kaz390wxlis2d8d43jnqhf0d"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false ;need python-distributed, which needs dask-expr.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'versioneer
            (lambda _
              ;; Our version of versioneer needs setup.cfg.  This is adapted
              ;; from pyproject.toml.
              (with-output-to-file "setup.cfg"
                (lambda ()
                  (display "\
[versioneer]
VCS = git
style = pep440
versionfile_source = dask_expr/_version.py
versionfile_build = dask_expr/_version.py
tag_prefix =
parentdir_prefix = dask_expr-
")))
              (invoke "versioneer" "install")
              (substitute* "setup.py"
                (("versioneer.get_version\\(\\)")
                 (string-append "\"" #$version "\""))))))))
    (propagated-inputs (list python-pandas python-pyarrow))
    (native-inputs
     ;; We use python-dask/bootstrap so that python-dask can propagate this
     ;; package without creating a mutually recursive dependency.
     (list python-dask/bootstrap
           python-pytest
           python-setuptools
           python-versioneer
           python-wheel))
    (home-page "https://github.com/dask/dask-expr")
    (synopsis "Dask DataFrames with query optimization")
    (description "This is a rewrite of Dask DataFrame that includes query
optimization and generally improved organization.")
    (license license:bsd-3)))

(define-public python-distributed
  (package
    (name "python-distributed")
    (version "2024.4.2")
    (source
     (origin
       ;; The test files are not included in the archive on pypi
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dask/distributed")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0sy9mqa8qlxsagbz8xn304csrlxhxj4b6k84yrjxdcmkp9pkx166"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-m"
             (string-append "not slow"
                            " and not flaky"
                            " and not gpu"
                            " and not ipython"
                            " and not avoid_ci")
             "-k"
             (string-append
              ;; These fail because they require network access,
              ;; specifically access to 8.8.8.8.
              "not "
              (string-join
               (list
                "TestClientSecurityLoader.test_security_loader"
                "test_BatchedSend"
                "test_allowed_failures_config"
                "test_async_context_manager"
                "test_async_with"
                "test_client_repr_closed_sync"
                "test_client_is_quiet_cluster_close"
                "test_close_closed"
                "test_close_fast_without_active_handlers"
                "test_close_grace_period_for_handlers"
                "test_close_loop_sync"
                "test_close_properly"
                "test_close_twice"
                "test_compression"
                "test_connection_pool"
                "test_connection_pool_close_while_connecting"
                "test_connection_pool_detects_remote_close"
                "test_connection_pool_outside_cancellation"
                "test_connection_pool_remove"
                "test_connection_pool_respects_limit"
                "test_connection_pool_tls"
                "test_counters"
                "test_dashboard_host"
                "test_dashboard_link_cluster"
                "test_dashboard_link_inproc"
                "test_deserialize_error"
                "test_dont_override_default_get"
                "test_ensure_no_new_clients"
                "test_errors"
                "test_fail_to_pickle_target_2"
                "test_failure_doesnt_crash"
                "test_file_descriptors_dont_leak"
                "test_finished"
                "test_freeze_batched_send"
                "test_get_client_functions_spawn_clusters"
                "test_host_uses_scheduler_protocol"
                "test_identity_inproc"
                "test_identity_tcp"
                "test_large_packets_inproc"
                "test_locked_comm_drop_in_replacement"
                "test_locked_comm_intercept_read"
                "test_locked_comm_intercept_write"
                "test_messages_are_ordered_bsend"
                "test_messages_are_ordered_raw"
                "test_mixing_clients_different_scheduler"
                "test_multiple_listeners"
                "test_no_dangling_asyncio_tasks"
                "test_plugin_exception"
                "test_plugin_internal_exception"
                "test_plugin_multiple_exceptions"
                "test_ports"
                "test_preload_import_time"
                "test_preload_manager_sequence"
                "test_queue_in_task"
                "test_quiet_client_close"
                "test_rebalance_sync"
                "test_repr_localcluster"
                "test_require_encryption"
                "test_rpc_default"
                "test_rpc_inproc"
                "test_rpc_message_lifetime_default"
                "test_rpc_message_lifetime_inproc"
                "test_rpc_message_lifetime_tcp"
                "test_rpc_serialization"
                "test_rpc_tcp"
                "test_rpc_tls"
                "test_rpc_with_many_connections_inproc"
                "test_rpc_with_many_connections_tcp"
                "test_scheduler_file"
                "test_security_dict_input_no_security"
                "test_security_loader"
                "test_security_loader_ignored_if_explicit_security_provided"
                "test_security_loader_ignored_if_returns_none"
                "test_send_after_stream_start"
                "test_send_before_close"
                "test_send_before_start"
                "test_send_recv_args"
                "test_send_recv_cancelled"
                "test_sending_traffic_jam"
                "test_serializers"
                "test_server"
                "test_server_comms_mark_active_handlers"
                "test_shutdown"
                "test_shutdown_localcluster"
                "test_teardown_failure_doesnt_crash_scheduler"
                "test_tell_workers_when_peers_have_left"
                "test_threadpoolworkers_pick_correct_ioloop"
                "test_tls_listen_connect"
                "test_tls_temporary_credentials_functional"
                "test_variable_in_task"
                "test_worker_preload_text"
                "test_worker_uses_same_host_as_nanny"
                "test_nanny_timeout")   ; access to 127.0.0.1
               " and not ")

              ;; This seems to want to use 64GB of memory.
              " and not test_computation_object_code_dask_compute"

              ;; These fail because it doesn't find dask[distributed]
              " and not test_quiet_close_process"

              ;; There is no distributed.__git_revision__ property.
              " and not test_git_revision"

              ;; The system monitor did not return a dictionary containing
              ;; "host_disk_io.read_bps".
              " and not test_disk_config"

              ;; These fail because the exception text format
              ;; appears to have changed.
              " and not test_exception_text"
              " and not test_worker_bad_args"

              ;; These time out
              " and not test_nanny_timeout"

              ;; These tests are rather flaky
              " and not test_quiet_quit_when_cluster_leaves"
              " and not multiple_clients_restart"
              " and not test_steal_twice"
              " and not test_task_groups_update_start_stop"
              " and not test_web_preload"
              " and not test_web_preload_worker"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'versioneer
            (lambda _
              ;; Our version of versioneer needs setup.cfg.  This is adapted
              ;; from pyproject.toml.
              (with-output-to-file "setup.cfg"
                (lambda ()
                  (display "\
[versioneer]
VCS = git
style = pep440
versionfile_source = distributed/_version.py
versionfile_build = distributed/_version.py
tag_prefix =
parentdir_prefix = distributed-
")))
              (invoke "versioneer" "install")
              (substitute* "setup.py"
                (("versioneer.get_version\\(\\)")
                 (string-append "\"" #$version "\"")))))
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              ;; This option is not supported by our version of pytest.
              (substitute* "pyproject.toml"
                (("--cov-config=pyproject.toml.*") ""))))
          (add-after 'unpack 'fix-references
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* '("distributed/comm/tests/test_ucx_config.py"
                             "distributed/tests/test_client.py"
                             "distributed/tests/test_queues.py"
                             "distributed/tests/test_variable.py"
                             "distributed/cli/tests/test_tls_cli.py"
                             "distributed/cli/tests/test_dask_spec.py"
                             "distributed/cli/tests/test_dask_worker.py"
                             "distributed/cli/tests/test_dask_scheduler.py")
                (("\"dask-scheduler\"")
                 (format #false "\"~a/bin/dask-scheduler\"" #$output))
                (("\"dask-worker\"")
                 (format #false "\"~a/bin/dask-worker\"" #$output)))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "DISABLE_IPV6" "1")
              ;; Disable job queueing
              (setenv "DASK_DISTRIBUTED__SCHEDULER__WORKER_SATURATION" "inf")
              ;; Do not use dask-expr
              (setenv "DASK_DATAFRAME__QUERY_PLANNING" "False")
              ;; The integration tests are all problematic to some
              ;; degree.  They either require network access or some
              ;; other setup.  We only run the tests in
              ;; distributed/tests.
              (for-each (lambda (dir)
                          (delete-file-recursively
                           (string-append "distributed/" dir "/tests")))
                        (list "cli" "comm" "dashboard" "deploy" "diagnostics"
                              "http" "http/scheduler" "http/worker"
                              "protocol" "shuffle"))))
          ;; We need to use "." here.
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "python" "-m" "pytest" "." "-vv" test-flags)))))))
    (propagated-inputs
     (list python-click
           python-cloudpickle
           python-cryptography
           python-dask
           python-dask-expr
           python-msgpack
           python-psutil
           python-pyyaml
           python-setuptools
           python-sortedcontainers
           python-tblib
           python-toolz
           python-tornado-6
           python-urllib3
           python-zict))
    (native-inputs
     (list python-flaky
           python-importlib-metadata
           python-pytest
           python-pytest-timeout
           python-versioneer
           python-wheel))
    (home-page "https://distributed.dask.org")
    (synopsis "Distributed scheduler for Dask")
    (description "Dask.distributed is a lightweight library for distributed
computing in Python.  It extends both the @code{concurrent.futures} and
@code{dask} APIs to moderate sized clusters.")
    (license license:bsd-3)))

(define-public python-modin
  (package
    (name "python-modin")
    (version "0.32.0")
    (source
     (origin
       ;; The archive on pypi does not include all required files.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/modin-project/modin")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1vb3iffgspryb6nvwiwdnypb922vkn2yvyzc1y0wwxcb0c0fl78d"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; These four tests fail because an expected error is not raised.
              "-k" "not test_binary_bad_broadcast")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'loosen-requirements
           (lambda _
             (substitute* "setup.py"
               ;; Don't depend on a specific version of Pandas.
               (("pandas==") "pandas>="))))
         (replace 'check
           (lambda* (#:key tests? test-flags #:allow-other-keys)
             (when tests?
               (setenv "MODIN_ENGINE" "dask")
               (apply invoke "python" "-m" "pytest"
                      "modin/tests/numpy" test-flags)
               (setenv "MODIN_ENGINE" "python")
               (apply invoke "python" "-m" "pytest"
                      "modin/tests/numpy" test-flags)))))))
    (propagated-inputs
     (list python-cloudpickle
           python-dask
           python-distributed
           python-numpy
           python-packaging
           python-pandas
           python-s3fs))
    (native-inputs
     (list python-boto3
           python-jinja2
           python-lxml
           python-matplotlib
           python-msgpack
           python-openpyxl
           python-psutil
           python-pyarrow
           python-pytest
           python-pytest-benchmark
           python-pytest-cov
           python-pytest-xdist
           python-scipy
           python-sqlalchemy
           python-tables
           python-tqdm
           python-xarray
           python-xlrd
           python-wheel))
    (home-page "https://github.com/modin-project/modin")
    (synopsis "Make your pandas code run faster")
    (description
     "Modin uses Ray or Dask to provide an effortless way to speed up your
pandas notebooks, scripts, and libraries.  Unlike other distributed DataFrame
libraries, Modin provides seamless integration and compatibility with existing
pandas code.")
    (license license:asl2.0)))

(define-public python-numpy-groupies
  (package
    (name "python-numpy-groupies")
    (version "0.9.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "numpy_groupies" version))
       (sha256
        (base32 "000qz0z78rs3l6y0dd2vzvd2lx3mczm2762whwsdnhz6c35axdq1"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-runner
           python-numba
           python-numpy))
    (home-page "https://github.com/ml31415/numpy-groupies")
    (synopsis "Tools for group-indexing operations: aggregated sum and more")
    (description
     "This package provides optimized tools for group-indexing operations:
aggregated sum and more.")
    (license license:bsd-3)))

(define-public python-plotnine
  (package
    (name "python-plotnine")
    (version "0.14.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/has2k1/plotnine")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02ph0h312qn5a9ivh2qhv0x9sybccgbidzvb8im1hikwcqp8v2fw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; XXX: Check for any new failing tests during next update cycle.
      ;; These all fail because the images are considered to be too different,
      ;; though they really do look fine.
      ;; See https://github.com/has2k1/plotnine/issues/627
      `(list ,@(map (lambda (file) (string-append "--ignore=" file))
                    (list "tests/test_aes.py"
                          "tests/test_annotation_logticks.py"
                          "tests/test_coords.py"
                          "tests/test_facet_labelling.py"
                          "tests/test_facets.py"
                          "tests/test_geom_bar_col_histogram.py"
                          "tests/test_geom_bin_2d.py"
                          "tests/test_geom_boxplot.py"
                          "tests/test_geom_count.py"
                          "tests/test_geom_density_2d.py"
                          "tests/test_geom_density.py"
                          "tests/test_geom_dotplot.py"
                          "tests/test_geom_freqpoly.py"
                          "tests/test_geom_map.py"
                          "tests/test_geom_path_line_step.py"
                          "tests/test_geom_point.py"
                          "tests/test_geom_raster.py"
                          "tests/test_geom_rect_tile.py"
                          "tests/test_geom_ribbon_area.py"
                          "tests/test_geom_sina.py"
                          "tests/test_geom_smooth.py"
                          "tests/test_geom_text_label.py"
                          "tests/test_geom_violin.py"
                          "tests/test_layout.py"
                          "tests/test_position.py"
                          "tests/test_qplot.py"
                          "tests/test_scale_internals.py"
                          "tests/test_scale_labelling.py"
                          "tests/test_stat_ecdf.py"
                          "tests/test_stat_function.py"
                          "tests/test_stat_summary.py"
                          "tests/test_theme.py"))
             "-k"
             (string-append "not "
                            (string-join
                             (list
                              ;; This triggers an unexpected but harmless
                              ;; warning.
                              "test_save_method"
                              ;; This test fails to set the locale.
                              "test_no_after_scale_warning"
                              ;; Missing optional modules
                              "test_non_linear_smooth"
                              "test_non_linear_smooth_no_ci")
                             " and not "
                             'infix)))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pretend-version
            ;; The version string is usually derived via setuptools-scm, but
            ;; without the git metadata available, the version string is set to
            ;; '999'.
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'check 'pre-check
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; The data files are referenced by the tests but they are not
              ;; installed.
              (copy-recursively "plotnine/data"
                                (string-append (site-packages inputs
                                                              outputs)
                                               "/plotnine/data"))
              (setenv "CI" "1")      ;skip tests that are known to fail on CI.
              ;; Matplotlib needs to be able to write its configuration file
              ;; somewhere.
              (setenv "MPLCONFIGDIR" "/tmp")
              (setenv "TZ" "UTC")
              (setenv "TZDIR"
                      (search-input-directory inputs "share/zoneinfo")))))))
    (propagated-inputs (list python-adjusttext
                             python-matplotlib
                             python-mizani
                             python-numpy
                             python-patsy
                             python-scipy
                             python-statsmodels))
    (native-inputs (list python-geopandas
                         python-mock
                         python-pandas
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-setuptools-scm
                         python-wheel
                         tzdata-for-tests))
    (home-page "https://github.com/has2k1/plotnine")
    (synopsis "Grammar of Graphics for Python")
    (description
     "Plotnine is a Python implementation of the Grammar of Graphics.
It is a powerful graphics concept for creating plots and visualizations in a
structured and declarative manner.  It is inspired by the R package ggplot2
and aims to provide a similar API and functionality in Python.")
    (license license:expat)))

(define-public python-pyvista
  (package
    (name "python-pyvista")
    (version "0.44.2")
    (source
     ;; The PyPI tarball does not contain the tests.
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyvista/pyvista")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lnh4cvf6wld7hm293015d80ny0vnsk96ckfvc2crzd1b79ch1v5"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-imageio
           python-matplotlib
           python-meshio
           python-numpy
           python-pillow
           python-pooch
           python-scooby
           vtk))
    ;; packages needed for testing
    (native-inputs (list python-pytest
                         python-scipy
                         python-ipython
                         python-trimesh
                         python-tqdm))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; vtk check fails in sanity-check, comment out
         (add-after 'unpack 'patch-pyproject
           (lambda _
             (substitute* "pyproject.toml"
               (("'vtk<9\\.4\\.0'," all) (string-append "#" all)))))
         (add-after 'unpack 'fix-failing-tests
           (lambda _
             (rename-file "tests/plotting/test_charts.py" "tests/plotting/_test_charts.py")
             (rename-file "tests/examples/test_download_files.py" "tests/examples/_test_download_files.py")
             (rename-file "tests/examples/test_downloads.py" "tests/examples/_test_downloads.py")
             (rename-file "tests/plotting/test_texture.py" "tests/plotting/_test_texture.py")
             (substitute* "tests/plotting/test_plotting.py"
               (("\"\"\"Determine if using mesa.\"\"\"" all)
                (string-append all "\n    return False")))
             (substitute* "tests/test_meshio.py"
               (("cow = .*$" all) (string-append "#" all "\n"))
               ((", cow") ""))
             (substitute* "tests/core/test_dataset.py"
               (("test_partition") "_test_partition"))
             (substitute* "tests/core/test_composite.py"
               (("test_ensight_multi_block_io") "_test_ensight_multi_block_io"))
             (substitute* "tests/core/test_dataset_filters.py"
               (("test_connectivity_.*$" all) (string-append "_" all))
               (("test_compute_boundary_mesh_quality") "_test_compute_boundary_mesh_quality"))
             (substitute* "tests/core/test_polydata_filters.py"
               (("test_protein_ribbon") "_test_protein_ribbon"))
             (substitute* "tests/core/test_validation.py"
               (("test_cast_to_numpy_raises") "_test_cast_to_numpy_raises"))
             (substitute* "tests/plotting/test_actor.py"
               (("test_actor_texture") "_test_actor_texture"))
             (substitute* "tests/plotting/test_lookup_table.py"
               (("test_init_cmap") "_test_init_cmap")
               (("test_repr") "_test_repr"))
             (substitute* "tests/plotting/test_plotter.py"
               (("test_add_multiple") "_test_add_multiple")
               (("test_plot_return_img_without_cpos") "_test_plot_return_img_without_cpos")
               (("test_plot_return_img_with_cpos") "_test_plot_return_img_with_cpos")
               (("test_only_screenshots_flag") "_test_only_screenshots_flag"))
             (substitute* "tests/plotting/test_plotting_utilities.py"
               (("test_gif_reader") "_test_gif_reader"))
             (substitute* "tests/plotting/test_render_window_interactor.py"
               (("test_timer") "_test_timer")
               (("test_add_timer_event") "_test_add_timer_event")
               (("test_interpolate") "_test_interpolate"))
             (substitute* "tests/plotting/test_renderer.py"
               (("test_legend_.*$" all) (string-append "_" all)))
             (substitute* "tests/plotting/test_theme.py"
               (("test_box_axes") "_test_box_axes")
               (("test_load_theme") "_test_load_theme")
               (("test_save_before_close_callback") "_test_save_before_close_callback")
               (("test_user_logo") "_test_user_logo"))
             (substitute* "tests/core/test_geometric_sources.py"
               (("test_translate_direction_collinear") "_test_translate_direction_collinear"))
             (substitute* "tests/examples/test_dataset_loader.py"
               (("test_dataset_loader_one_file") "_test_dataset_loader_one_file")
               (("test_dataset_loader_two_files_one_loadable") "_test_dataset_loader_two_files_one_loadable")
               (("test_dataset_loader_two_files_both_loadable") "_test_dataset_loader_two_files_both_loadable")
               (("test_dataset_loader_cubemap") "_test_dataset_loader_cubemap")
               (("test_dataset_loader_dicom") "_test_dataset_loader_dicom")
               (("test_dataset_loader_from_nested_files_and_directory") "_test_dataset_loader_from_nested_files_and_directory")
               (("test_dataset_loader_from_nested_multiblock") "_test_dataset_loader_from_nested_multiblock")
               (("test_load_dataset_no_reader") "_test_load_dataset_no_reader"))
             (substitute* "tests/core/test_reader.py"
               (("test_ensightreader_arrays") "_test_ensightreader_arrays")
               (("test_ensightreader_timepoints") "_test_ensightreader_timepoints")
               (("test_ensightreader_time_sets") "_test_ensightreader_time_sets")
               (("test_dcmreader") "_test_dcmreader")
               (("test_objreader") "_test_objreader")
               (("test_stlreader") "_test_stlreader")
               (("test_tecplotreader") "_test_tecplotreader")
               (("test_byureader") "_test_byureader")
               (("test_facetreader") "_test_facetreader")
               (("test_plot3dmetareader") "_test_plot3dmetareader")
               (("test_multiblockplot3dreader") "_test_multiblockplot3dreader")
               (("test_binarymarchingcubesreader") "_test_binarymarchingcubesreader")
               (("test_pvdreader") "_test_pvdreader")
               (("test_pvdreader_no_time_group") "_test_pvdreader_no_time_group")
               (("test_pvdreader_no_part_group") "_test_pvdreader_no_part_group")
               (("test_openfoamreader_arrays_time") "_test_openfoamreader_arrays_time")
               (("test_openfoamreader_active_time") "_test_openfoamreader_active_time")
               (("test_openfoamreader_read_data_time_value") "_test_openfoamreader_read_data_time_value")
               (("test_openfoamreader_read_data_time_point") "_test_openfoamreader_read_data_time_point")
               (("test_openfoam_skip_zero_time") "_test_openfoam_skip_zero_time")
               (("test_openfoam_cell_to_point_default") "_test_openfoam_cell_to_point_default")
               (("test_openfoam_patch_arrays") "_test_openfoam_patch_arrays")
               (("test_openfoam_case_type") "_test_openfoam_case_type")
               (("test_read_cgns") "_test_read_cgns")
               (("test_bmpreader") "_test_bmpreader")
               (("test_demreader") "_test_demreader")
               (("test_jpegreader") "_test_jpegreader")
               (("test_meta_image_reader") "_test_meta_image_reader")
               (("test_nifti_reader") "_test_nifti_reader")
               (("test_nrrd_reader") "_test_nrrd_reader")
               (("test_png_reader") "_test_png_reader")
               (("test_pnm_reader") "_test_pnm_reader")
               (("test_slc_reader") "_test_slc_reader")
               (("test_tiff_reader") "_test_tiff_reader")
               (("test_hdr_reader") "_test_hdr_reader")
               (("test_avsucd_reader") "_test_avsucd_reader")
               (("test_hdf_reader") "_test_hdf_reader")
               (("test_xdmf_reader") "_test_xdmf_reader")
               (("test_fluentcffreader") "_test_fluentcffreader")
               (("test_gambitreader") "_test_gambitreader")
               (("test_gaussian_cubes_reader") "_test_gaussian_cubes_reader")
               (("test_gesignareader") "_test_gesignareader")
               (("test_pdbreader") "_test_pdbreader")
               (("test_particle_reader") "_test_particle_reader")
               (("test_prostar_reader") "_test_prostar_reader"))))
         ;; test phase writes files to $HOME
         (add-before 'check 'redirect-HOME
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (home-page "https://docs.pyvista.org/")
    (synopsis "3D plotting and mesh analysis through VTK")
    (description
     "PyVista is...

@itemize
@item @emph{Pythonic VTK}: a high-level API to the Visualization
Toolkit (VTK);
@item mesh data structures and filtering methods for spatial datasets;
@item 3D plotting made simple and built for large/complex data geometries.
@end itemize

This package provides a Pythonic, well-documented interface exposing VTK's
powerful visualization backend to facilitate rapid prototyping, analysis, and
visual integration of spatially referenced datasets.")
    (license license:expat)))

(define-public python-simplespectral
  (package
    (name "python-simplespectral")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "SimpleSpectral" version))
       (sha256
        (base32 "0qh3xwdv9cwcqdamvglrhm586p4yaq1hd291py1fvykhk2a2d4w6"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy python-scipy))
    (home-page "https://github.com/xmikos/simplespectral")
    (synopsis "FFT module for Python")
    (description
     "This package provides a simplified @code{scipy.signal.spectral} module
to do spectral analysis in Python.")
    (license license:expat)))

(define-public python-traittypes
  (package
    (name "python-traittypes")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "traittypes" version))
       (sha256
        (base32 "1mlv93irdrgxrhnhq3ksi9585d55bpi4mv9dha4p8gkkjiia4vxy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; This one test fails because it doesn't raise an expected exception.
      #:test-flags '(list "-k" "not test_bad_values")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'numpy-compatibility
           (lambda _
             (substitute* "traittypes/tests/test_traittypes.py"
               (("np\\.int") "int")))))))
    (propagated-inputs (list python-traitlets))
    (native-inputs
     (list python-numpy
           python-pandas
           python-nose
           python-pytest
           python-setuptools
           python-xarray
           python-wheel))
    (home-page "https://github.com/jupyter-widgets/traittypes")
    (synopsis "Trait types for NumPy, SciPy and friends")
    (description "The goal of this package is to provide a reference
implementation of trait types for common data structures used in the scipy
stack such as numpy arrays or pandas and xarray data structures.  These are
out of the scope of the main traitlets project but are a common requirement to
build applications with traitlets in combination with the scipy stack.")
    (license license:bsd-3)))

(define-public python-aplus
  (package
    (name "python-aplus")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aplus" version))
       (sha256
        (base32 "1rznc26nlp641rn8gpdngfp79a3fji38yavqakxi35mx2da04msg"))))
    (build-system python-build-system)
    (home-page "https://github.com/xogeny/aplus")
    (synopsis "Promises/A+ for Python")
    (description "This package is an implementation of the Promises/A+
specification and test suite in Python.")
    (license license:expat)))

(define-public python-clarabel
  (package
    (name "python-clarabel")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "clarabel" version))
       (sha256
        (base32 "15k32ynvh45n9q905bxwamh5w5cia9bxzmwz69wbribmyhsv22m3"))
       (patches
        (search-patches "python-clarabel-blas.patch"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules
                           ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system) #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'build-python-module
            (assoc-ref py:%standard-phases 'build))
          (add-after 'build-python-module 'install-python-module
            (assoc-ref py:%standard-phases 'install)))
      #:cargo-inputs
      `(("rust-amd" ,rust-amd-0.2)
        ("rust-blas" ,rust-blas-0.22)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-derive-builder" ,rust-derive-builder-0.11)
        ("rust-enum-dispatch" ,rust-enum-dispatch-0.3) ;0.3.8
        ("rust-itertools" ,rust-itertools-0.11)
        ("rust-lapack" ,rust-lapack-0.19)
        ("rust-lazy-static" ,rust-lazy-static-1) ;1.4
        ("rust-libc" ,rust-libc-0.2)
        ("rust-num-derive" ,rust-num-derive-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-pyo3" ,rust-pyo3-0.20)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-thiserror" ,rust-thiserror-1))
      #:features '(list "python")
      #:install-source? #false))
    (inputs
     (list maturin))
    (native-inputs
     (list python-wrapper))
    (propagated-inputs (list python-numpy python-scipy))
    (home-page "https://github.com/oxfordcontrol/Clarabel.rs")
    (synopsis "Interior-point solver for convex conic optimisation problems")
    (description "Clarabel.rs is a Rust implementation of an interior point
numerical solver for convex optimization problems using a novel homogeneous
embedding.")
    (license license:asl2.0)))

(define-public python-climin
  (package
    (name "python-climin")
    (version "0.1a1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "climin" version))
              (sha256
               (base32
                "1wpjisd5zzi5yvjff02hnxn84822k8sdxvvd33lil2x79wdb36rv"))))
    (build-system python-build-system)
    (native-inputs (list python-nose))
    (propagated-inputs (list python-numpydoc python-numpy python-scipy))
    (home-page "https://github.com/BRML/climin")
    (synopsis "Optimization for machine learning")
    (description
     "@command{climin} is a Python package for optimization,
heavily biased to machine learning scenarios.  It works on top of
@command{numpy} and (partially) @command{gnumpy}.")
    (license license:bsd-3)))

(define-public python-corner
  (package
    (name "python-corner")
    (version "2.2.2")
    (source
     (origin
       (method git-fetch) ;no tests in PyPi archive
       (uri (git-reference
             (url "https://github.com/dfm/corner.py")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i4dk4jxh0saysya2cnsfwlxwpldbdl174i9pwi4qj82av9jr2ii"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         ;; XXX: Disable tests which failed with mismatched images, check why.
         "-k" (string-append "not test_labels[png]"
                             " and not test_title_quantiles[png]"
                             " and not test_title_quantiles_default[png]"
                             " and not test_title_quantiles_raises[png]"
                             " and not test_bins[png]"
                             " and not test_bins_log[png]"
                             " and not test_titles1[png]"
                             " and not test_titles2[png]"
                             " and not test_pandas[png]"
                             " and not test_tight[png]"
                             " and not test_extended_overplotting[png]"
                             " and not test_reverse_overplotting[png]"
                             " and not test_arviz[png]"
                             " and not test_range_fig_arg[png]"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pretend-version
            ;; XXX: Make sure you're either building from a fully intact git
            ;; repository or PyPI tarballs. Most other sources (such as GitHub's
            ;; tarballs, a git checkout without the .git folder) don't contain
            ;; the necessary metadata and will not work.
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (propagated-inputs
     (list python-matplotlib))
    (native-inputs
     (list python-arviz python-pytest python-scipy python-setuptools-scm))
    (home-page "http://corner.readthedocs.io/")
    (synopsis "Make some beautiful corner plots")
    (description
     "This Python module uses @code{matplotlib} to visualize multidimensional
samples using a scatterplot matrix. In these visualizations, each one- and
two-dimensional projection of the sample is plotted to reveal covariances.
corner was originally conceived to display the results of Markov Chain Monte
Carlo simulations and the defaults are chosen with this application in mind but
it can be used for displaying many qualitatively different samples.")
    (license license:bsd-2)))

(define-public python-paramz
  (package
    (name "python-paramz")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sods/paramz")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ywc2jzj40m6wmq227j3snxvp4434s0m1xk1abg6v6mr87pv2sa9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k"
              ;; Two tests fail with error: TypeError: arrays to stack must be
              ;; passed as a "sequence" type such as list or tuple.
              (string-append "not test_raveled_index"
                             " and not test_regular_expression_misc")
              "paramz/tests/array_core_tests.py"
              "paramz/tests/cacher_tests.py"
              "paramz/tests/examples_tests.py"
              "paramz/tests/index_operations_tests.py"
              "paramz/tests/init_tests.py"
              "paramz/tests/lists_and_dicts_tests.py"
              "paramz/tests/model_tests.py"
              "paramz/tests/observable_tests.py"
              "paramz/tests/parameterized_tests.py"
              "paramz/tests/pickle_tests.py"
              "paramz/tests/verbose_optimize_tests.py")))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-decorator
           python-numpy
           python-scipy
           python-six))
    (home-page "https://github.com/sods/paramz")
    (synopsis "The Parameterization Framework")
    (description
     "@command{paramz} is a lightweight parameterization framework
for parameterized model creation and handling.  Its features include:

@itemize
 @item Easy model creation with parameters.
 @item Fast optimized access of parameters for optimization routines.
 @item Memory efficient storage of parameters (only one copy in memory).
 @item Renaming of parameters.
 @item Intuitive printing of models and parameters.
 @item Gradient saving directly inside parameters.
 @item Gradient checking of parameters.
 @item Optimization of parameters.
 @item Jupyter notebook integration.
 @item Efficient storage of models, for reloading.
 @item Efficient caching.
@end itemize")
    (license license:bsd-3)))

(define-public python-pods
  (package
    (name "python-pods")
    (version "0.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pods" version))
       (sha256
        (base32 "1z57jdwml2jzr2dq20p7pzx3ayhajgfd2d0xqjvgzx576hp2z1ac"))))
    (build-system pyproject-build-system)
    ;; Tests depend on Nose framework and try to download test data from
    ;; <https://github.com/SheffieldML/GPmat>.
    (arguments (list #:tests? #f))
    (native-inputs
     (list python-poetry-core))
    (propagated-inputs
     (list python-pandas
           python-pyyaml
           python-scipy
           python-tables))
    (home-page "https://github.com/lawrennd/ods")
    (synopsis "Python software for Open Data Science")
    (description "This package provides utilities and tools for open data
science including tools for accessing data sets in Python.")
    (license license:bsd-3)))

(define-public python-pyfma
  (package
    (name "python-pyfma")
    (version "0.1.6")
    (source (origin
              (method git-fetch)   ;for tests
              (uri (git-reference
                    (url "https://github.com/nschloe/pyfma")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12i68jj9n1qj9phjnj6f0kmfhlsd3fqjlk9p6d4gs008azw5m8yn"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy))
    (native-inputs (list pybind11 python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/nschloe/pyfma")
    (synopsis "Fused multiply-add for Python")
    (description "@code{pyfma} provides an implementation of fused
multiply-add which computes @code{(x*y) + z} with a single rounding.
This is useful for dot products, matrix multiplications, polynomial
evaluations (e.g., with Horner's rule), Newton's method for evaluating
functions, convolutions, artificial neural networks etc.")
    (license license:expat)))

(define-public python-pydicom
  (package
    (name "python-pydicom")
    (version "2.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pydicom/pydicom")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ksyyc1hbhyqy289a2frn84ss29fb7czirx3dkxx56f4ia33b4c8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Skip tests that require networking.
      #~(list "-k" (string-append
                    "not test_jpeg_ls_pixel_data.py"
                    " and not test_gdcm_pixel_data.py"
                    " and not test_pillow_pixel_data.py"
                    " and not test_rle_pixel_data.py"
                    " and not Test_JPEG_LS_Lossless_transfer_syntax"
                    " and not test_numpy_pixel_data.py"
                    " and not test_data_manager.py"
                    " and not test_handler_util.py"
                    " and not test_overlay_np.py"
                    " and not test_encoders_pydicom.py"
                    " and not test_encaps.py"
                    " and not test_reading_ds_with_known_tags_with_UN_VR"
                    " and not TestDatasetOverlayArray"
                    " and not TestReader"
                    " and not test_filewriter.py"))))
    (native-inputs (list python-pytest python-flit-core))
    (inputs (list gdcm libjpeg-turbo))
    (propagated-inputs (list python-numpy python-pillow))
    (home-page "https://github.com/pydicom/pydicom")
    (synopsis "Python library for reading and writing DICOM data")
    (description "@code{python-pydicom} is a Python library for reading and
writing DICOM medical imaging data.  It can read, modify and write DICOM
data.")
    (license license:expat)))

(define-public python-deepdish
  ;; XXX: The project may no longer be compatible with the version of NumPy
  ;; packed in Guix (now 1.24.4), use the latest commit containing fixes.
  ;; See: <https://github.com/uchicago-cs/deepdish/issues/50>.
  ;; However, there is a maintained fork that appears to be a good
  ;; replacement: https://github.com/portugueslab/flammkuchen.
  (let ((commit "3f2dff7a03f1b31f6924b665ad5b8c299329c1cd")
        (revision "0"))
    (package
      (name "python-deepdish")
      (version (git-version "0.3.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/uchicago-cs/deepdish")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1n3r6z5zd18kdmzyg1gkm9lqi573szlxbls1ck5wjn4a14ar9fw3"))))
      (arguments
       ;; Disable few failing tests to pass the build.
       (list
        #:test-flags
        #~(list "-k" (string-append "not test_pad"
                                    " and not test_pad_repeat_border"
                                    " and not test_pad_repeat_border_corner"
                                    " and not test_pad_to_size"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'dont-vendor-six
              (lambda _
                (delete-file "deepdish/six.py")
                (substitute* "deepdish/io/hdf5io.py"
                  (("from deepdish import six") "import six"))
                (substitute* "deepdish/io/ls.py"
                  (("from deepdish import io, six, __version__")
                   "from deepdish import io, __version__
import six
")))))))
      (build-system pyproject-build-system)
      (native-inputs
       (list python-pytest
             python-pandas
             python-setuptools
             python-wheel))
      (propagated-inputs
       (list python-numpy
             python-scipy
             python-six
             python-tables))
      (home-page "https://github.com/uchicago-cs/deepdish")
      (synopsis "Python library for HDF5 file saving and loading")
      (description
       "Deepdish is a Python library to load and save HDF5 files.
The primary feature of deepdish is its ability to save and load all kinds of
data as HDF5.  It can save any Python data structure, offering the same ease
of use as pickling or @code{numpy.save}, but with the language
interoperability offered by HDF5.")
      (license license:bsd-3))))

(define-public python-simple-pid
  (package
    (name "python-simple-pid")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "simple-pid" version))
              (sha256
               (base32
                "094mz6rmfq1h0gpns5vlxb7xf9297hlkhndw7g9k95ziqfkv7mk0"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "unittest" "discover" "tests/")))))))
    (home-page "https://github.com/m-lundberg/simple-pid")
    (synopsis "Easy to use PID controller")
    (description "This package provides a simple and easy-to-use @acronym{PID,
proportional-integral-derivative} controller.")
    (license license:expat)))

(define-public python-supersmoother
  (package
    (name "python-supersmoother")
    (version "0.4")
    (source
     (origin
       (method git-fetch)        ; no package in PyPI
       (uri (git-reference
             (url "https://github.com/jakevdp/supersmoother")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lkj8l2mpki6x2pxcwlrplx63lhi8h9v2rzxgjfb0cppsfr8m1wp"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-scipy
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "http://github.com/jakevdp/supersmoother")
    (synopsis "Python implementation of Friedman's Supersmoother")
    (description
     "This package provides an efficient implementation of
@url{https://www.slac.stanford.edu/pubs/slacpubs/3250/slac-pub-3477.pdf,
Friedman's SuperSmoother} based in Python.  It makes use of numpy for fast
numerical computation.")
    (license license:bsd-2)))

(define-public python-opt-einsum
  (package
    (name "python-opt-einsum")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "opt_einsum" version))
              (sha256
               (base32
                "0jb5lia0q742d1713jk33vlj41y61sf52j6pgk7pvhxvfxglgxjr"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy))
    (native-inputs
     (list python-pytest python-pytest-cov python-setuptools python-wheel))
    (home-page "https://github.com/dgasmith/opt_einsum")
    (synopsis "Optimizing numpys einsum function")
    (description
     "Optimized einsum can significantly reduce the overall execution time of
einsum-like expressions by optimizing the expression's contraction order and
dispatching many operations to canonical BLAS, cuBLAS, or other specialized
routines.  Optimized einsum is agnostic to the backend and can handle NumPy,
Dask, PyTorch, Tensorflow, CuPy, Sparse, Theano, JAX, and Autograd arrays as
well as potentially any library which conforms to a standard API. See the
documentation for more information.")
    (license license:expat)))

(define-public python-vaex-core
  (package
    (name "python-vaex-core")
    (version "4.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://www.github.com/maartenbreddels/vaex")
             (commit (string-append "core-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sp096msbzgjlwi8c1ink2bp4pjff9pvikqz1y1li8d3in4gpgdr"))
       (patches
        (search-patches "python-vaex-core-fix-tsl-use.patch"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet
        #~(begin
            ;; Delete everything except for vaex-core itself:
            (define (delete-except exception)
              (lambda (file)
                (unless (member file `("." ".." ,exception))
                  (delete-file-recursively file))))
            (for-each (delete-except "packages") (scandir "."))
            (with-directory-excursion "packages"
              (for-each (delete-except "vaex-core") (scandir ".")))
            (for-each (lambda (file)
                        (unless (member file '("." ".."))
                          (rename-file
                           (string-append "packages/vaex-core/" file)
                           file)))
                      (scandir "packages/vaex-core"))
            (delete-file-recursively "packages")
            (delete-file-recursively "vendor")))))
    (build-system pyproject-build-system)
    (arguments
     ;; require vaex.server and others, which require vaex-core.
     (list #:tests? #false))
    (inputs
     (list boost pcre pybind11 string-view-lite tsl-hopscotch-map))
    (propagated-inputs
     (list python-aplus
           python-blake3
           python-click ;XXX for dask
           python-cloudpickle
           python-dask
           python-filelock
           python-frozendict
           python-future
           python-nest-asyncio
           python-numpy
           python-pandas
           python-progressbar2
           python-pyarrow
           python-pydantic-2
           python-pydantic-settings
           python-pyyaml
           python-requests
           python-rich
           python-six
           python-tabulate))
    (native-inputs
     (list python-pytest python-cython-3 python-setuptools python-wheel))
    (home-page "https://www.github.com/maartenbreddels/vaex")
    (synopsis "Core of Vaex library for exploring tabular datasets")
    (description "Vaex is a high performance Python library for lazy
Out-of-Core DataFrames (similar to Pandas), to visualize and explore big
tabular datasets.  This package provides the core modules of Vaex.")
    (license license:expat)))

(define-public python-salib
  (package
    (name "python-salib")
    (version "1.4.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SALib/SALib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18xfyzircsx2q2lmfc9lxb6xvkxicnc83qzghd7df1jsprr5ymch"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-matplotlib
                             python-multiprocess
                             python-numpy
                             python-pandas
                             python-scipy))
    (native-inputs (list python-hatchling python-pytest python-pytest-cov))
    (home-page "https://salib.readthedocs.io/en/latest/")
    (synopsis "Tools for global sensitivity analysis")
    (description "SALib provides tools for global sensitivity analysis.  It
contains Sobol', Morris, FAST, DGSM, PAWN, HDMR, Moment Independent and
fractional factorial methods.")
    (license license:expat)))

(define-public python-pylems
  (package
    (name "python-pylems")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PyLEMS" version))
              (sha256
               (base32
                "074azbyivjbwi61fs5p8z9n6d8nk8xw6fmln1www13z1dccb3740"))))
    (build-system python-build-system)
    (propagated-inputs (list python-lxml))
    (home-page "https://github.com/LEMS/pylems")
    (synopsis
     "Python support for the Low Entropy Model Specification language (LEMS)")
    (description
     "A LEMS simulator written in Python which can be used to run
NeuroML2 models.")
    (license license:lgpl3)))

(define-public python-pynetdicom
  (package
    (name "python-pynetdicom")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pynetdicom" version))
              (sha256
               (base32
                "1smzrnc93nmv8jz4np9knas74a46b1nhb3hjpf8n9vfpxypgnwcn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Tests takes about 10-15min to complete.
      ;; Skip tests that require networking.
      #~(list "-k" (string-append
                    " not TestFindSCP"
                    " and not TestQRGetServiceClass"
                    " and not TestQRMoveServiceClass"
                    " and not TestStoreSCP"
                    " and not test_ae.py"
                    " and not test_echoscp.py"
                    " and not test_qrscp_echo.py"
                    " and not test_storescp.py"
                    " and not test_pr_level_patient"
                    " and not test_pr_level_series"
                    " and not test_scp_cancelled"))))
    (native-inputs (list python-codespell
                         python-coverage
                         python-poetry-core
                         python-pytest
                         python-pytest-cov
                         python-pytest-xdist
                         python-sphinx
                         python-sphinx-rtd-theme))
    (propagated-inputs (list python-mypy
                             python-numpydoc
                             python-pydicom
                             python-pyfakefs
                             python-sqlalchemy))
    (home-page "https://github.com/pydicom/pynetdicom")
    (synopsis "Python implementation of the DICOM networking protocol")
    (description
     "@code{pynetdicom} is a Python package that implements the DICOM
networking protocol.  It allows the easy creation of DICOM
@acronym{SCUs,Service Class Users} and @acronym{SCPs,Service Class
Providers}.")
    (license license:expat)))

(define-public python-pynrrd
  (package
    (name "python-pynrrd")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mhe/pynrrd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09gdyi4kbi3512ydgqxkgr4j7b9a95qh83fk2n9s41bns4id9xj7"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-nptyping python-numpy python-typing-extensions))
    (home-page "https://github.com/mhe/pynrrd")
    (synopsis "Python module for reading and writing NRRD files")
    (description
     "@code{pynrrd} is a Python module for reading and writing @acronym{NRRD,
Nearly Raw Raster Data} files (format designed to support scientific
visualization and image processing involving N-dimensional raster data) into
and from numpy arrays.")
    (license license:expat)))

(define-public python-pynsee
  (package
    (name "python-pynsee")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pynsee" version))
       (sha256
        (base32 "1w084ynwdd9f4wpcnakqc0nxcbj9gr8vppv4rd258i3dp1qq4sw5"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))  ; XXX: Tests require network access.
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-appdirs
           python-openpyxl
           python-pandas
           python-platformdirs
           python-pyarrow
           python-requests
           python-shapely
           python-tqdm
           python-unidecode
           python-urllib3
           python-xlrd))
    (home-page "https://pynsee.readthedocs.io")
    (synopsis
     "Tools to Easily Search and Download French Data From INSEE and IGN APIs")
    (description
     "This package provides tools to easily search and download French data
from INSEE and IGN APIs.  This data includes more than 150 000 macroeconomic
series, a dozen datasets of local french data, numerous sources available on
@url{insee.fr}, geographical limits of administrative areas taken from IGN as
well as key metadata and SIRENE database containing data on all French
compagnies.")
    (license license:expat)))

(define-public python-pyqtgraph
  (package
    (name "python-pyqtgraph")
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyqtgraph" version))
       (sha256
        (base32 "1kiazyc8mqyx0479qdcvdclzq0g1hpp93dyq8444w1f72628s42q"))))
    (build-system pyproject-build-system)
    (arguments
     ;; This test fails.  It suggests to disable assert rewriting in Pytest,
     ;; but it still doesn't pass.
     (list #:test-flags #~'("-k" "not test_reload")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-qpa
                 (lambda _
                   (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (inputs
     (list qtbase-5))
    (propagated-inputs
     (list python-h5py
           python-numpy
           python-pyopengl
           python-scipy
           python-pyqt))
    (home-page "https://www.pyqtgraph.org")
    (synopsis "Scientific graphics and GUI library for Python")
    (description
     "PyQtGraph is a Pure-python graphics library for PyQt5, PyQt6, PySide2
and PySide6.  It is intended for use in mathematics, scientific or engineering
applications.")
    (license license:expat)))

(define-public python-libneuroml
  (package
    (name "python-libneuroml")
    (version "0.6.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NeuralEnsemble/libNeuroML.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04cfff9phm19x87p86xrkhd6wlpxvdwk3rf1c3qgyncfchws0sjh"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-numpy
           python-setuptools
           python-tables
           python-wheel))
    (propagated-inputs
     (list python-lxml
           python-natsort
           python-networkx))
    (home-page "https://libneuroml.readthedocs.org/en/latest/")
    (synopsis
     "Python library for working with NeuroML descriptions of neuronal models")
    (description
     "This package provides a Python library for working with NeuroML descriptions of
neuronal models")
    (license license:bsd-3)))

(define-public snakemake
  (package
    (name "snakemake")
    (version "8.29.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "snakemake" version))
       (sha256
        (base32 "1ilpmrjmnc529p4gw2x23ik1d8b5pm6k1dhq08dknvfjsf3vgyjr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         ;; XXX: Unclear why these tests fail.
         "--ignore=tests/test_report_href/test_script.py"
         "--ignore=tests/test_script_py/scripts/test_explicit_import.py"
         "--ignore=tests/test_output_index.py"
         ;; We don't care about testing old python@3.7 on Guix.
         "--ignore=tests/test_conda_python_3_7_script/test_script.py"
         ;; Those require additional snakemake plugins.
         "--ignore=tests/test_api.py"
         "--ignore=tests/test_executor_test_suite.py"
         ;; We don't care about lints.
         "--ignore=tests/test_linting.py"
         ;; These tests attempt to change S3 buckets on AWS and fail
         ;; because there are no AWS credentials.
         "--ignore=tests/test_tibanna"
         ;; It's a similar story with this test, which requires access
         ;; to the Google Storage service.
         "--ignore=tests/test_google_lifesciences")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'avoid-assets-download
            (lambda _
              (substitute* "setup.py"
                (("^from assets import Assets") "")
                (("^Assets\\.deploy\\(\\)") ""))))
          ;; For cluster execution Snakemake will call Python.  Since there is
          ;; no suitable GUIX_PYTHONPATH set, cluster execution will fail.  We
          ;; fix this by calling the snakemake wrapper instead.
          (add-after 'unpack 'call-wrapper-not-wrapped-snakemake
            (lambda _
              (substitute* "snakemake/executors/__init__.py"
                (("self\\.get_python_executable\\(\\),")
                 "")
                (("\"-m snakemake\"")
                 (string-append "\"" #$output
                                "/bin/snakemake" "\""))
                ;; The snakemake command produced by format_job_exec contains
                ;; references to /gnu/store.  Prior to patching above that's
                ;; just a reference to Python; after patching it's a reference
                ;; to the snakemake executable.
                ;;
                ;; In Tibanna execution mode Snakemake arranges for a certain
                ;; Docker image to be deployed to AWS.  It then passes its own
                ;; command line to Tibanna.  This is misguided because it only
                ;; ever works if the local Snakemake command was run inside
                ;; the same Docker image.  In the case of using Guix this is
                ;; never correct, so we need to replace the store reference.
                (("tibanna_args.command = command")
                 (string-append
                  "tibanna_args.command = command.replace('"
                  #$output "/bin/snakemake', 'python3 -m snakemake')")))))
          (add-after 'unpack 'patch-version
            (lambda _
              (substitute* "setup.py"
                (("version=versioneer.get_version\\(\\)")
                 (format #f "version=~s" #$version)))
              (substitute* '("snakemake/_version.py"
                             "versioneer.py")
                (("0\\+unknown") #$version))))
          (add-before 'check 'pre-check
            (lambda* (#:key tests?  #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")))))))
    (propagated-inputs
     (list python-appdirs
           python-conda-inject
           python-configargparse
           python-connection-pool
           python-dpath
           python-gitpython
           python-humanfriendly
           python-immutables
           python-jinja2
           python-jsonschema
           python-nbformat
           python-packaging
           python-psutil
           python-pulp
           python-pyyaml
           python-requests
           python-reretry
           python-smart-open
           python-snakemake-interface-common
           python-snakemake-interface-executor-plugins
           python-snakemake-interface-report-plugins
           python-snakemake-interface-storage-plugins
           python-tabulate
           python-throttler
           python-wrapt
           python-yte))
    (native-inputs
     (list python-docutils
           python-numpy
           python-pandas
           python-setuptools
           python-tomli
           python-wheel))
    (home-page "https://snakemake.readthedocs.io")
    (synopsis "Python-based execution environment for make-like workflows")
    (description
     "Snakemake aims to reduce the complexity of creating workflows by
providing a clean and modern domain specific specification language (DSL) in
Python style, together with a fast and comfortable execution environment.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
