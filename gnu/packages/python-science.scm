;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2018, 2020-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016,2024 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2021-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2022 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019-2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2019 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2020-2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020-2025 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2021, 2023 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2022 kiasoc5 <kiasoc5@tutanota.com>
;;; Copyright © 2022, 2024 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2022 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2023, 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 Marco Baggio <marco.baggio@mdc-berlin.de>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024 Rick Huijzer <ikbenrickhuyzer@gmail.com>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Mark Walker <mark.damon.walker@gmail.com>
;;; Copyright © 2025 Nguyễn Gia Phong <cnx@loang.net>
;;; Copyright © 2025 Jake Forster <jakecameron.forster@gmail.com>
;;; Copyright © 2025 Ghislain Vaillant <ghislain.vaillant@inria.fr>
;;; Copyright © 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages duckdb)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
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
  #:use-module (guix deprecation)
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

(define-public python-adjusttext
  (package
    (name "python-adjusttext")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "adjusttext" version))
       (sha256
        (base32 "18dw5kqxan4m8kvw3w1lm0p69gj95i7rcgmcfs485x1s8pa5rdsa"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #false)) ;there are none
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-scipy))
    (home-page "https://github.com/Phlya/adjustText")
    (synopsis "Adjust text position in matplotlib plots to minimize overlaps")
    (description
     "Often when we want to label multiple points on a graph the text will
start heavily overlapping with both other labels and data points.  This can be
a major problem requiring manual solution.  However this can be largely
automated by smart placing of the labels (difficult) or iterative adjustment
of their positions to minimize overlaps (relatively easy).  This library
implements the latter option to help with matplotlib graphs.")
    (license license:expat)))

(define-public python-algopy
  (package
    (name "python-algopy")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "algopy" version))
       (sha256
        (base32 "0l9d4pkbal6m6q8v6w5zr9wlij4sfycc8i2w7irk0i4n8hyvm0ja"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools))
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

(define-public python-anndata
  (package
    (name "python-anndata")
    (version "0.12.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/theislab/anndata")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1404x0z37z7xfdzkkafh62amchikrz0ssyc83rn6pv3dd4nn8nid"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 4167 passed, 1208 skipped, 12 xfailed, 2225 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; XXX: AttributeError: module 'pyarrow.lib' has no attribute
              ;; 'PyExtensionType'
              "--ignore=tests/test_awkward.py"
              ;; These tests require CUDA backed package:
              ;; <https://github.com/cupy/cupy>.
              "--deselect=sts/test_views.py::test_view_of_view"
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "tests/"
                                                    test))
                      (list "test_concatenate.py::test_concatenate_layers"
                            "test_concatenate.py::test_concat_different_types_dask"
                            "test_concatenate.py::test_concat_on_var_outer_join"
                            "test_concatenate.py::test_concatenate_layers_misaligned"
                            "test_concatenate.py::test_concatenate_layers_outer"
                            "test_concatenate.py::test_concatenate_roundtrip"
                            "test_concatenate.py::test_error_on_mixed_device"
                            "test_concatenate.py::test_nan_merge"
                            "test_concatenate.py::test_pairwise_concat"
                            "test_concatenate.py::test_transposed_concat"
                            "test_dask.py::test_dask_to_disk_view"
                            "test_dask.py::test_dask_to_memory_unbacked"
                            "test_gpu.py::test_adata_raw_gpu"
                            "test_gpu.py::test_gpu"
                            "test_gpu.py::test_raw_gpu"
                            "test_helpers.py::test_as_cupy_dask"
                            "test_helpers.py::test_as_dask_functions"
                            "test_io_elementwise.py::test_io_spec_cupy"
                            "test_obsmvarm.py::test_1d_declaration"
                            "test_obsmvarm.py::test_1d_set"
                            "test_views.py::test_ellipsis_index"
                            "test_views.py::test_modify_view_component"
                            "test_views.py::test_set_scalar_subset_X"
                            "test_views.py::test_view_different_type_indices"
                            "test_views.py::test_view_of_view"))
              "-k" (string-join
                    ;; TypeError: _fix_co_filename() argument 2 must be str,
                    ;; not PosixPath
                    (list "not test_hints"
                          ;; ValueError: Cannot convert. CSC format must be
                          ;; 2D. Got 1D
                          "test_backed_modification_sparse[csc_matrix]"
                          ;; Failed: DID NOT WARN. No warnings of type (<class
                          ;; 'FutureWarning'>,) were emitted.
                          "test_old_format_warning_thrown")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          ;; Doctests require scanpy from (gnu packages bioinformatics)
          (add-after 'unpack 'disable-doctests
            (lambda _
              (substitute* "pyproject.toml"
                (("--doctest-modules") ""))))
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          ;; Numba needs a writable dir to cache functions.
          (add-before 'check 'set-numba-cache-dir
            (lambda _
              (setenv "NUMBA_CACHE_DIR" "/tmp"))))))
    (native-inputs
     (list hdf5                ;for h5diff, tests/test_io_backwards_compat.py
           python-awkward
           python-boltons
           python-dask
           python-distributed
           python-filelock
           python-hatch-vcs
           python-hatchling
           python-joblib
           python-loompy
           python-matplotlib
           python-openpyxl
           python-pyarrow
           python-pytest
           python-pytest-mock
           python-pytest-randomly
           python-pytest-xdist
           python-scikit-learn
           python-setuptools-scm))
    (propagated-inputs
     (list python-array-api-compat
           python-h5py
           python-legacy-api-wrap
           python-natsort
           python-numpy
           python-packaging
           python-pandas
           python-scipy
           python-zarr))
    (home-page "https://github.com/theislab/anndata")
    (synopsis "Annotated data for data analysis pipelines")
    (description "Anndata is a package for simple (functional) high-level APIs
for data analysis pipelines.  In this context, it provides an efficient,
scalable way of keeping track of data together with learned annotations and
reduces the code overhead typically encountered when using a mostly
object-oriented library such as @code{scikit-learn}.")
    (license license:bsd-3)))

(define-public python-anndata-0.11
  (package
    (inherit python-anndata)
    (name "python-anndata")
    (version "0.11.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/theislab/anndata")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05i805k5kvmwp5k0qw9vxvpgjwys284nq529mfn7vwlryz9d247m"))))))

;; A bare minimal package, mainly to use in tests and reduce closure
;; size. Tests are left out in the main package to slim down native-inputs.
(define-public python-anndata-minimal
  (package/inherit python-anndata
    (name "python-anndata-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments python-anndata)
       ((#:tests? _ #t) #f)))
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-setuptools-scm))))

;; XXX: See: <https://codeberg.org/guix/guix/issues/3093>.
(define-public python-aplus
  ;; PyPI release lacks the latest version, Git has no tags.
  (let ((commit "1ab8ebec987fb7213766784aad02cbf4410d9036")
        (revision "0"))
    (package
      (name "python-aplus")
      (version (git-version "0.11.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/xogeny/aplus")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02jcfj7dywvs0sd60c85pxwh0mwsj9p1q27445pba6j489x3dffj"))))
      (build-system pyproject-build-system)
      (arguments
       (list #:tests? #f))      ;they depend on Nose test runner
      (native-inputs
       (list python-setuptools))
      (home-page "https://github.com/xogeny/aplus")
      (synopsis "Promises/A+ for Python")
      (description
       "This package is an implementation of the Promises/A+ specification and
test suite in Python.")
      (license license:expat))))

(define-public python-apted
  ;; PyPI release lacks tests and there is no Git tag.
  (let ((commit "828b3e3f4c053f7d35f0b55b0d5597e8041719ac")
        (revision "0"))
    (package
      (name "python-apted")
      (version (git-version "1.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/JoaoFelipe/apted")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1vw1sbn41cysmhr4ib58cw3hzs1xjxwb1d8r1yhrqgjk5q6ckjw7"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        #~(list "--deselect=apted/tests/test_chained.py::test_factory"
                "--deselect=apted/tests/test_correctness.py::test_factory"
                "--deselect=apted/tests/test_per_edit_operation_correctness.py::test_factory")))
      (native-inputs
       (list python-pytest
             python-setuptools
             python-wheel))
      (home-page "https://github.com/JoaoFelipe/apted")
      (synopsis "Algorithm for the tree edit distance")
      (description
       "This is a Python implementation of the APTED algorithm,which
supersedes the RTED algorithm for computing the tree edit distance.")
      (license license:expat))))

(define-public python-asap3
  (package
    (name "python-asap3")
    (version "3.13.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asap3" version))
       (sha256
        (base32 "1qpy9nnwv692hd3cg70n2zwjank25mlsfvvirklijwis3m4lfqg4"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           which)) ;for build
    (propagated-inputs
     (list python-ase
           python-numpy))
    (home-page "https://wiki.fysik.dtu.dk/asap")
    (synopsis "ASAP - classical potentials for Molecular Dynamics with ASE.")
    (description "This package provides accelerated simulations and potentials
of solids.")
    (license license:lgpl3)))

(define-public python-ase
  (package
    (name "python-ase")
    (version "3.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ase" version))
       (sha256
        (base32 "0xahqqyxkxrjh1g23icydngrvc8iv3lnd4iys9i802jvfxas6wd0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 2 failed, 2999 passed, 566 skipped, 5 xfailed, 47 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; DeprecationWarning.
              "--deselect"
              "ase/test/fio/test_espresso.py::test_pw_input_write_nested_flat"
              ;; UserWarning.
              "--deselect"
              "ase/test/fio/test_espresso.py::TestConstraints::test_fix_scaled")))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-setuptools))
    (inputs (list spglib))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-scipy))
    (home-page "https://wiki.fysik.dtu.dk/ase/")
    (synopsis "Atomic Simulation Environment")
    (description "This package provides a set of tools and Python modules for
setting up, manipulating, running, visualizing and analyzing atomistic
simulations.")
    (license license:lgpl2.1+)))

(define-public python-baycomp
  (package
    (name "python-baycomp")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "baycomp" version))
       (sha256
        (base32 "1v6s4mfr6xzjbv9a2v89hywm6fbv5nii0qczvcfjanvdn7bmmcij"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
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
      (with-extensions (list (pyproject-guile-json))
      #~(modify-phases %standard-phases
          (add-after 'build 'build-python-module
            (assoc-ref py:%standard-phases 'build))
          (add-after 'build-python-module 'install-python-module
            (assoc-ref py:%standard-phases 'install))))
      #:features '(list "python")
      #:install-source? #false))
    (inputs
     (cons maturin (cargo-inputs 'python-clarabel)))
    (native-inputs
     (list python-wrapper))
    (propagated-inputs (list python-numpy python-scipy))
    (home-page "https://github.com/oxfordcontrol/Clarabel.rs")
    (synopsis "Interior-point solver for convex conic optimisation problems")
    (description "Clarabel.rs is a Rust implementation of an interior point
numerical solver for convex optimization problems using a novel homogeneous
embedding.")
    (license license:asl2.0)))

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

(define-public python-cvxpy
  (package
    (name "python-cvxpy")
    (version "1.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cvxpy" version))
       (sha256
        (base32 "0p9zp2ci1zw56hy2q0w18s3kb1h3j0j4m4gw2sg6a9qw00c24lab"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1344 passed, 391 skipped, 4 deselected, 237 warnings
      #:test-flags
      #~(list #$@(map (lambda (test) (string-append "--deselect="
                                                    "cvxpy/tests/"
                                                    "test_cone2cone.py::"
                                                    test))
                      ;; cvxpy.error.SolverError: Either candidate conic
                      ;; solvers (['GLPK_MI', 'SCIPY']) do not support the
                      ;; cones output by the problem (SOC, NonNeg, Zero), or
                      ;; there are not enough constraints in the problem.
                      (list "TestSlacks::test_mi_socp_2"
                            ;; cvxpy.error.SolverError: Solver 'CVXOPT'
                            ;; failed. Try another solver, or solve with
                            ;; verbose=True for more information.
                            "TestOpRelConeQuad::test_oprelcone_1_m1_k3_complex"
                            "TestOpRelConeQuad::test_oprelcone_1_m3_k1_complex"
                            ;; cvxpy.error.SolverError: Solver 'CVXOPT'
                            ;; failed. Try another solver, or solve with
                            ;; verbose=True for more information.
                            "TestOpRelConeQuad::test_oprelcone_2")))))
    (native-inputs
     (list pybind11
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-clarabel
           python-numpy
           python-osqp
           python-scipy
           python-scs
           ;; [optional]
           ;; python-cuopt-cu12
           python-cvxopt
           ;; python-cylp ;FIXME: <https://codeberg.org/guix/guix/issues/2912>
           ;; python-daqp
           ;; python-diffcp
           python-ecos
           ;; python-gurobipy
           ;; python-highspy
           ;; python-mosek
           ;; python-nvidia-cuda-runtime-cu12
           ;; python-ortools
           ;; python-piqp
           ;; python-proxsuite
           ;; python-pyscipopt
           ;; python-qoco
           #;python-xpress))
    (home-page "https://github.com/cvxpy/cvxpy")
    (synopsis "DSL for modeling convex optimization problems")
    (description
     "This package provides a domain-specific language for modeling convex
optimization problems in Python.")
    (license license:asl2.0)))

;; Note: Remember to update python-distributed when updating dask.
(define-public python-dask
  (package
    (name "python-dask")
    (version "2025.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dask/dask/")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12m4p4zfm96fjsm45wppdrylsi71vjr0ywplz6q7fhw9vbhk0kki"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 15277 passed, 751 skipped, 261 xfailed, 280 xpassed, 90 warnings
      #:test-flags
      #~(list "-m" "not gpu and not slow and not network"
              "--pyargs" "dask"
              "--numprocesses" (number->string (min 4 (parallel-job-count)))
              "--reruns=3"
              "-k" (string-join
                    ;; This one cannot be interrupted.
                    (list "not test_interrupt"
                          ;; AttributeError: 'Array' object has no attribute
                          ;; 'expr'
                          "test_blockwise"
                          "test_is_dask_collection_doesnt_materialize")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                ((".*--cov-config=pyproject.toml.*") ""))))
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'check 'remove-local-source
            (lambda _
              (delete-file-recursively "dask"))))))
    (native-inputs
     (list python-pytest
           python-pytest-asyncio
           python-pytest-mock
           python-pytest-rerunfailures
           python-pytest-timeout
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm-next
           tzdata-for-tests))
    (propagated-inputs
     (list python-click
           python-cloudpickle
           python-fsspec
           python-importlib-metadata
           python-packaging
           python-partd
           python-pyyaml
           python-toolz
           ;; [optional]
           python-lz4
           python-numpy
           python-pandas
           python-pyarrow))
    (home-page "https://github.com/dask/dask/")
    (synopsis "Parallel computing with task scheduling")
    (description
     "Dask is a flexible parallel computing library for analytics.  It
consists of two components: dynamic task scheduling optimized for computation,
and large data collections like parallel arrays, dataframes, and lists that
extend common interfaces like NumPy, Pandas, or Python iterators to
larger-than-memory or distributed environments.  These parallel collections
run on top of the dynamic task schedulers.")
    (license license:bsd-3)))

(define-public python-dask-image
  (package
    (name "python-dask-image")
    (version "2025.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dask_image" version))
       (sha256
        (base32 "1cx07dh09yqq6swyziiy9mb2f27ywja18gylfly1874a7af1mks5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 2171 passed, 177 skipped, 3405 warnings
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                (("--flake8") ""))))
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-pytest
           python-pytest-timeout
           python-setuptools
           python-setuptools-scm-next))
    (propagated-inputs
     (list python-dask
           python-numpy
           python-pandas
           python-pims
           python-scipy
           python-tifffile))
    (home-page "https://github.com/dask/dask-image")
    (synopsis "Distributed image processing")
    (description "This is a package for image processing with Dask arrays.
Features:

@itemize
@item Provides support for loading image files.
@item Implements commonly used N-D filters.
@item Includes a few N-D Fourier filters.
@item Provides some functions for working with N-D label images.
@item Supports a few N-D morphological operators.
@end itemize")
    (license license:bsd-3)))

(define-public python-decaylanguage
  (package
    (name "python-decaylanguage")
    (version "0.18.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "decaylanguage" version))
       (sha256
        (base32 "0kc9i9k51kg2zv8dwywpigiipxzmyxpzb101imjsvv1licip7b8v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; This file fails to be collected with "DeprecationWarning: setDaemon()
      ;; is deprecated, set the daemon attribute instead".
      #:test-flags #~(list "--ignore" "tests/test_convert.py")))
    (propagated-inputs (list python-attrs
                             python-graphviz
                             python-hepunits
                             python-lark
                             python-numpy
                             python-pandas
                             python-particle
                             python-plumbum))
    (native-inputs (list python-hatch-vcs
                         python-hatchling
                         python-pytest))
    (home-page "https://decaylanguage.readthedocs.io/en/latest/")
    (synopsis "Language to describe, manipulate and convert particle decays")
    (description "DecayLanguage implements a language to describe and convert
particle decays between digital representations, effectively making it
possible to interoperate several fitting programs.  Particular interest is
given to programs dedicated to amplitude analyses.")
    (license license:bsd-3)))

(define-public python-deepdish
  ;; XXX: The project may no longer be compatible with the version of NumPy
  ;; packed in Guix (now 1.26.4, and 2.3.1), use the latest commit containing
  ;; fixes.  See: <https://github.com/uchicago-cs/deepdish/issues/50>.
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
             python-setuptools))
      (propagated-inputs
       (list python-numpy-1
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

(define-public python-distributed
  (package
    (name "python-distributed")
    (version "2025.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dask/distributed")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ljzgbbs61si8b3qvxf7fb74wdhnj2mfg9a5nfngqv662fnibabg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; TODO: Test suite requires networking for most of the tests or special
      ;; care, find a way to enable some unit tests, see:
      ;; <.github/workflows/tests.yaml>.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                (("--cov-config.*") ""))))
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm-next))
    (propagated-inputs
     (list python-click
           python-cloudpickle
           python-dask
           python-jinja2
           python-locket
           python-msgpack
           python-packaging
           python-psutil
           python-pyyaml
           python-sortedcontainers
           python-tblib
           python-toolz
           python-tornado-6
           python-urllib3
           python-zict))
    (home-page "https://distributed.dask.org")
    (synopsis "Distributed scheduler for Dask")
    (description "Dask.distributed is a lightweight library for distributed
computing in Python.  It extends both the @code{concurrent.futures} and
@code{dask} APIs to moderate sized clusters.")
    (license license:bsd-3)))

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

(define-public python-efficient-apriori
  (package
    (name "python-efficient-apriori")
    (version "2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "efficient_apriori" version))
       (sha256
        (base32 "0vmdp8qkir7jrmwgpzajssyxh6q78m0q16pr1v657vla9x5wxn2s"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--doctest-modules" "-vv" "efficient_apriori")))
    (native-inputs (list python-setuptools python-pytest))
    (home-page "https://github.com/tommyod/Efficient-Apriori")
    (synopsis "An efficient Python implementation of the Apriori algorithm.")
    (description "An efficient Python implementation of the Apriori algorithm,
which uncovers hidden structures in categorical data")
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fastcluster" version))
       (sha256
        (base32 "00nzjrk8cp3kwm0qax5xxg61dxq9b8s3jspsqx4skyn3lpmkl8ym"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-scipy
           python-setuptools))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://danifold.net/fastcluster.html")
    (synopsis "Fast hierarchical clustering routines for R and Python")
    (description "The fastcluster package implements seven common hierarchical
clustering schemes efficiently.  The package is made with two interfaces to
standard software: R and Python.")
    (license license:bsd-2)))

(define-public python-fgivenx
  (package
    (name "python-fgivenx")
    ;; See: https://github.com/handley-lab/fgivenx/issues/30
    (properties '((commit . "cf51dbf8b7efdd8d84f055740a069a5b882dcf77")
                  (revision . "0")))
    (version (git-version "2.4.2"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/handley-lab/fgivenx")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z9lwrjvz977gk6z7zqj8d21yi0xcz67mj1737yzzdc8c03nzg9w"))))
    (build-system pyproject-build-system)
    ;; tests: 25 passed, 1 warning
    (native-inputs
     (list python-pytest
           python-pytest-mpl
           python-setuptools))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-scipy
           ;; [optional]
           python-getdist
           python-joblib
           python-tqdm))
    (home-page "https://github.com/handley-lab/fgivenx")
    (synopsis "Functional Posterior Plotter")
    (description
     "@code{fgivenx} is a Python package for plotting posteriors of functions.
It is currently used in astronomy, but will be of use to any scientists
performing Bayesian analyses which have predictive posteriors that are
functions.

This package allows one to plot a predictive posterior of a function,
dependent on sampled parameters.  It assumes one has a Bayesian posterior
@code{Post(theta|D,M)} described by a set of posterior samples
@code{{theta_i}~Post}. If there is a function parameterised by theta
@code{y=f(x;theta)}, then this script will produce a contour plot of the
conditional posterior @code{P(y|x,D,M)} in the @code{(x,y)} plane.")
    (license license:expat)))

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

(define-public python-geosketch
  (package
    (name "python-geosketch")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brianhie/geosketch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lnynk4r87zqck5mmj33axmly34hh7lrlmfy1qidrw7xihy28g5a"))))
    (build-system pyproject-build-system)
    ;; XXX: Avoid circular dependency on python-scanorama.
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools))
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

(define-public python-hepunits
  (package
    (name "python-hepunits")
    (version "2.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hepunits" version))
       (sha256
        (base32 "1n1nf2rz2d86qzjmcwykbc16jzsqb45vs8lyksg98b3jd8nwsd4l"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatch-vcs python-hatchling python-pytest))
    (home-page "https://github.com/scikit-hep/hepunits")
    (synopsis "Units and constants in the HEP system of units")
    (description "@code{hepunits} collects the most commonly used units and
constants in the HEP System of Units, as derived from the basic units
originally defined by the CLHEP project.")
    (license license:bsd-3)))

(define-public python-hist
  (package
    (name "python-hist")
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hist" version))
       (sha256
        (base32 "17cd46c0ixq18fr2kgzam09w1sr4qkd9l6nsjdbl4vggw80ck9vx"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-boost-histogram
                             python-histoprint
                             python-numpy
                             python-typing-extensions))
    (native-inputs (list python-hatch-vcs
                         python-hatchling
                         python-pytest
                         python-pytest-mpl))
    (home-page "https://hist.readthedocs.io/en/latest/")
    (synopsis "Hist classes and utilities")
    (description
     "Hist is an analyst-friendly front-end for @code{boost-histogram}.")
    (license license:bsd-3)))

(define-public python-histoprint
  (package
    (name "python-histoprint")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "histoprint" version))
       (sha256
        (base32 "07d2lk64gwhjvw4wccvwks3j4ig7g99q627jjxz4ans5a29p5pz1"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-click python-numpy python-uhi))
    (native-inputs (list python-awkward
                         python-boost-histogram
                         python-hatch-vcs
                         python-hatchling
                         python-pytest
                         python-rich))
    (home-page "https://github.com/scikit-hep/histoprint")
    (synopsis "Pretty print histograms to the console")
    (description "Histoprint uses a mix of terminal color codes and Unicode
trickery (i.e. combining characters) to plot overlaying histograms.")
    (license license:expat)))

(define-public python-imagehash
  (package
    (name "python-imagehash")
    (version "4.3.2")
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
        (base32 "1rz1fpwhcx0cbln189bcs61wlwgngcjcn77jvm0yji5s7lshhipy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Upstream plans to remove Python 2 compatibility:
          ;; https://github.com/JohannesBuchner/imagehash/pull/223
          (add-after 'unpack 'remove-six
            (lambda _
              (substitute* (find-files "." ".py$")
                          (("import six") "")
                          (("six\\.assertRaisesRegex\\(self, ")
                           "self.assertRaisesRegex(")
                          (("six\\.exec_") "")))))))
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

(define-public python-iminuit
  (package
    (name "python-iminuit")
    (version "2.31.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iminuit" version))
       (sha256
        (base32 "066y0khk5bv56jv0p5bkkmya3rwijskz9yq9ch3jlgfqzzqh9q6m"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list "-k" "not test_interactive_pyside6")))
    (native-inputs
     (list cmake-minimal
           pybind11
           python-annotated-types
           python-boost-histogram
           python-ipykernel
           python-jacobi
           python-joblib
           python-pydantic
           python-pytest
           python-resample
           python-scikit-build-core
           python-tabulate))
    ;; All inputs besides python-numpy are optional but greatly improve
    ;; the package.
    ;; FIXME: Numba segfaults Python in some tests.
    (propagated-inputs
     (list python-ipywidgets
           python-matplotlib
           ;; python-numba
           ;; python-numba-stats
           python-numpy
           python-scipy
           python-unicodeitplus))
    (home-page "https://github.com/scikit-hep/iminuit")
    (synopsis "Python interface for MINUIT2")
    (description
     "@code{iminuit} is a Jupyter-friendly Python interface for the @code{Minuit2}
C++ library maintained by CERN's ROOT team.

Minuit was designed to optimize statistical cost functions, for
maximum-likelihood and least-squares fits.  It provides the best-fit
parameters and error estimates from likelihood profile analysis.

Optionally, Iminuit supports SciPy minimizers as alternatives to Minuit's
MIGRAD algorithm and Numba accelerated functions.")
    ;; Python interface under MIT Expat, Iminuit C++ library under LGPL v2.1+.
    (license (list license:expat license:lgpl2.1+))))

(define-public python-jacobi
  (package
    (name "python-jacobi")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jacobi" version))
       (sha256
        (base32 "0a08680q6rnl6b1azq0lzd8r08pgnjd9ynwivb1g2vi4ccb4h7y1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                ;; AttributeError: module 'numpy' has no attribute
                ;; 'VisibleDeprecationWarning'
                ((".*error::numpy.VisibleDeprecationWarning.*") "")))))))
    (native-inputs
     (list python-pytest
           python-pytest-benchmark
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/hdembinski/jacobi")
    (synopsis "Compute numerical derivatives")
    (description
     "This package provides fast numerical derivatives for analytic
functions with arbitrary round-off error and error propagation.")
    (license license:expat)))

(define-public python-legendkit
  (package
    (name "python-legendkit")
    (version "0.3.6")
    (source
     (origin
       (method git-fetch)       ;no tests in PyPI archive
       (uri (git-reference
              (url "https://github.com/Marsilea-viz/legendkit")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03xdhh95w4zydwa4ahp3gyfa6i60c9s5arfcj366knckb1bnpnn9"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-matplotlib))
    (home-page "https://github.com/Marsilea-viz/legendki")
    (synopsis "Legend creation and manipulation for matplotlib")
    (description
     "This package implements a functionality to create and manipulate plot
legends for @code{matplotlib}.")
    (license license:expat)))

(define-public python-libneuroml
  (package
    (name "python-libneuroml")
    (version "0.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NeuralEnsemble/libNeuroML.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x5sgm0250zdfnw16cbmgl45hz2kfmww5lghpyhlcadgnbzyq3dx"))))
    (build-system pyproject-build-system)
    ;; tests: 116 passed, 5 xfailed, 5 warnings
    (native-inputs
     (list python-pytest
           python-numpy
           python-setuptools
           python-tables))
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

(define-public python-marsilea
  (package
    (name "python-marsilea")
    (version "0.5.4")
    (source
     (origin
       (method git-fetch)       ;no tests in PyPI archive
       (uri (git-reference
              (url "https://github.com/Marsilea-viz/marsilea")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09pyfx0gn46ypsp991d3n4a4xx6zlbpss078lw6yywnhl834v2i0"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-legendkit
           python-matplotlib
           python-numpy
           python-pandas
           python-platformdirs
           python-scipy
           python-seaborn))
    (home-page "https://github.com/Marsilea-viz/marsilea")
    (synopsis "Declarative creation of composable visualizations")
    (description
     "Marsilea is a Python library for creating composable visualizations in a
declarative way.  It is built on top of Matplotlib and provides a high-level
API for you to puzzle different visualizations together like logo.")
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

(define-public python-modin
  (package
    (name "python-modin")
    (version "0.37.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/modin-project/modin")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kqdx3b7sb3895ynypb6swf2jly26xvdghqqrm9ahqps16cn9dx9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests:
      ;; - api: 5 passed 
      ;; - dask: 269 passed, 2 skipped, 93 xfailed, 191 warnings
      ;; - python: 269 passed, 2 skipped, 93 xfailed, 197 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; AssertionError: assert 'array <...>
              "-k" (string-join
                    (list "not test_repr[100]"
                          "test_repr[size1]"
                          "test_repr[size2]"
                          "test_repr[size3]"
                          "test_repr[size4]"
                          "test_repr[size5]"
                          "test_repr[size6]"
                          "test_repr[size7]"
                          "test_repr[size8]"
                          "test_repr[size9]")
                    " and not ")
              "modin/tests/numpy")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'versioneer
            (lambda _
              (invoke "versioneer" "install")
              (substitute* "setup.py"
                (("version=versioneer.get_version\\(),")
                 (string-append "version='" #$version "',")))))
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "setup.cfg"
                ((" --cov-.*--cov-report=") ""))))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                ;; API basic tests
                (invoke "pytest" "-vv"
                        "modin/tests/test_executions_api.py"
                        "modin/tests/test_headers.py"
                        "modin/tests/core/test_dispatcher.py::test_add_option")
                ;; More complex engine tests, the complete set up requires
                ;; database access and AWS credentials, see:
                ;; <.github/workflows/ci.yml >.
                (setenv "MODIN_ENGINE" "dask")
                (apply invoke  "pytest" "-vv"  test-flags)
                (setenv "MODIN_ENGINE" "python")
                (apply invoke "pytest" "-vv"  test-flags)))))))
    (native-inputs
     (list python-boto3
           python-pytest
           python-pytest-xdist
           python-s3fs
           python-setuptools
           python-versioneer))
    (propagated-inputs
     (list python-fsspec
           python-numpy
           python-packaging
           python-pandas
           python-psutil
           python-typing-extensions
           ;; [optinoal]
           python-dask
           python-distributed))
    (home-page "https://github.com/modin-project/modin")
    (synopsis "Make your pandas code run faster")
    (description
     "Modin uses Ray or Dask to provide an effortless way to speed up your
pandas notebooks, scripts, and libraries.  Unlike other distributed DataFrame
libraries, Modin provides seamless integration and compatibility with existing
pandas code.")
    (license license:asl2.0)))

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
    (build-system pyproject-build-system)
    (arguments
     (list #:test-backend #~'unittest))
    (native-inputs
     (list python-setuptools))
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

(define-public python-multiscale-spatial-image
  (package
    (name "python-multiscale-spatial-image")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "multiscale_spatial_image" version))
       (sha256
        (base32 "1avcc7hrgyza793i02vjfvyn2bg0ag50h1pagapbpj65hl3v4whb"))))
    (build-system pyproject-build-system)
    ;; All interesting tests require file downloads over IPFS.
    (arguments (list #:tests? #false))
    (propagated-inputs
     (list python-dask
           python-numpy
           python-dateutil
           python-spatial-image
           python-xarray
           python-xarray-dataclass
           python-zarr))
    (native-inputs
     (list python-hatchling))
    (home-page "https://github.com/spatial-image/multiscale-spatial-image")
    (synopsis "Multi-dimensional spatial image data structure")
    (description
     "This package lets you generate a multiscale, chunked, multi-dimensional
spatial image data structure that can serialized to OME-NGFF.  Each scale is a
scientific Python Xarray spatial-image Dataset, organized into nodes of an
Xarray Datatree.")
    (license license:asl2.0)))

(define-public python-narwhals
  (package
    (name "python-narwhals")
    (version "2.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "narwhals" version))
       (sha256
        (base32 "0rd9z4pmp7cm1l11iahwxwkgl48ki3fiynj4d110i5cxp5smjn59"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 2846 passed, 215 skipped, 59 xfailed
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; Run a minimal portion of tests, the complete test suite
              ;; requires Polars, PySpark and SqlFrame packages.
              "--constructors=pandas"
              "-k" (string-join
                    ;; XXX: ValueError: Minimum version of modin supported by Narwhals is
                    ;; (0, 8, 2), found: (0,)
                    (list "not test_allow_series"
                          "test_cross_join_non_pandas"
                          "test_eager_only_eager"
                          "test_from_native_roundtrip_identity"
                          "test_namespace_series_from_iterable"
                          "test_series_only"
                          "test_to_native_namespace")
                    " and not "))))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-pytest-env
           python-hatchling))
    (propagated-inputs
     ;;    [core]
     (list python-duckdb
           python-pandas
           ;; python-polars ;https://codeberg.org/guix/guix/pulls/2570
           python-pyarrow
           ;; python-sqlframe
           ;; [optional]
           ;; python-cudf
           python-dask
           ;; python-ibis-framework
           python-modin
           python-packaging
           ;; python-pyarrow-hotfix
           ;; python-pyspark
           python-rich))
    (home-page "https://narwhals-dev.github.io/narwhals/")
    (synopsis "Compatibility layer between dataframe libraries")
    (description
     "This package provides an extremely lightweight compatibility layer
between dataframe libraries.
@itemize
@item full API support: cuDF, Modin, pandas, Polars, PyArrow
@item lazy-only support: Dask, DuckDB, Ibis, PySpark, SQLFrame
@end itemize")
    (license license:expat)))

;; A bare minimal package, mainly to use in tests and reduce closure
;; size. Tests are left out in the main package to slim down native-inputs.
(define-public python-narwhals-minimal
  (package/inherit python-narwhals
    (name "python-narwhals-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments python-anndata)
       ((#:tests? _ #t) #f)))
    (native-inputs
     (list python-hatchling))
    ;; All Narwals dependencies are optional.
    (propagated-inputs '())))

(define-public python-ndindex
  (package
    (name "python-ndindex")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ndindex" version))
       (sha256
        (base32 "048gc4pwvsyxkz7brph1fwmlgjd35alimvnb3248y91iy30i6q8g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 121 passed, 1 deselected, 3 warnings
      #:test-flags
      #~(list "--pyargs" "ndindex"
              "-c" "/dev/null" ;avoid coverage
              "-k" "not test_iter_indices_matmul") ; flaky
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-local-source
            (lambda _
              (delete-file-recursively "ndindex"))))))
    (native-inputs
     (list python-cython
           python-numpy
           python-pytest
           python-setuptools
           python-sympy))
    (home-page "https://quansight-labs.github.io/ndindex/")
    (synopsis "Python library for manipulating indices of ndarrays")
    (description "This package provides a Python library for manipulating
indices of @code{ndarrays}.")
    (license license:expat)))

(define-public python-nestcheck
  (package
    (name "python-nestcheck")
    ;; 0.2.1 was placed in 2019, there are a lot of changes providing
    ;; comparability with Python 3.11, use the latest commit from master's
    ;; HEAD.
    (properties '((commit . "513ef962ef7b0d66377686f9fe0a9e354dad48b3")
                  (revision . "0")))
    (version (git-version "0.2.1"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ejhigson/nestcheck")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zzms2jkiapawnjyr5i7c61m7pmg6yd3nmpv23bdx51glz2fmglc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 58 passed, 6 deselected, 10 warnings
      #:test-flags
      ;; TypeError: MultiIndex.set_levels() got an unexpected keyword argument
      ;; 'inplace'
      #~(list "-k" (string-append "not test_run_list_error_summary"
                                  ;; AttributeError: 'Series' object has no
                                  ;; attribute 'iteritems'
                                  " and not test_kde_plot_df"
                                  ;; Test is not determenistic and fails with
                                  ;; assertion not equal for DF array.
                                  " and not test_summary_df"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-pytest
            (lambda _
              (substitute* "tests/test_core.py"
                (("'nose'") "'pytest'")))))))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-fgivenx
           python-matplotlib
           python-numpy
           python-pandas
           python-scipy
           python-tqdm))
    (home-page "https://github.com/ejhigson/nestcheck")
    (synopsis "Nested sampling calculations utilities")
    (description
     "This package implements a functionality to work with
@url{https://en.wikipedia.org/wiki/Nested_sampling_algorithm, Nested
sampling}, a popular numerical method for Bayesian computation, which
simultaneously generates samples from the posterior distribution and an
estimate of the Bayesian evidence for a given likelihood and prior.
@code{nestcheck} provides Python utilities for analysing samples produced by
nested sampling, and estimating uncertainties on nested sampling
calculations (which have different statistical properties to calculations
using other numerical methods).")
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

(define-public python-numba-stats
  (package
    (name "python-numba-stats")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "numba_stats" version))
       (sha256
        (base32 "1m5gwrc3liqydpix5ckyc7s5ysvgvinbznnkcvk2xjz13lwfy2s8"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numba
           python-numpy
           python-scipy))
    (home-page "https://github.com/scikit-hep/numba-stats")
    (synopsis "Accelerated implementations of SciPy probability distributions")
    (description
     "This package provides Numba-accelerated implementations of common SciPy
probability distributions and others used in particle physics.

The supported distributions are:

@itemize
@item Uniform
@item (Truncated) Normal
@item Log-normal
@item Poisson
@item Binomial
@item (Truncated) Exponential
@item Student's t
@item Voigtian
@item Crystal Ball
@item Generalised double-sided Crystal Ball
@item Tsallis-Hagedorn, a model for the minimum bias pT distribution
@item Q-Gaussian
@item Bernstein density (not normalized to unity)
@item Cruijff density (not normalized to unity)
@item CMS-Shape
@item Generalized Argus
@end itemize")
    (license license:expat)))

(define-public python-numdifftools
  (package
    (name "python-numdifftools")
    (version "0.9.42")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "numdifftools" version))
       (sha256
        (base32 "1hgv3jhf4y9qrizkwfryj2b56zd0i2dvzig1y7r4ng193wbparl6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 177 passed, 3 skipped, 2 deselected, 971 warnings
      #:test-flags
      #~(list "-m" "not benchmark and not slow")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                ((" --cov=numdifftools --cov-report=xml") "")))))))
    (native-inputs
     (list python-algopy
           python-line-profiler
           python-pdm-backend
           python-pytest
           python-statsmodels))
    (propagated-inputs
     (list python-numpy
           python-scipy
           ;; [optional]
           python-matplotlib))
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

(define-public python-numpoly
  (package
    (name "python-numpoly")
    (version "1.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jonathf/numpoly")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19cld52ddvlbza1l8g1irfj603m6f5yifqy4pm397ffrxipfbbq6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 219 passed, 2 deselected, 4 warnings
      #:test-flags
      ;; AssertionError: invalid results type: 3 != <class 'int'>
      #~(list "--deselect=test/test_array_function.py::test_count_nonzero")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-local-source
            (lambda _
              (delete-file-recursively "numpoly"))))))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-sympy))
    (propagated-inputs
     (list python-importlib-metadata
           python-numpy))
    (home-page "https://numpoly.readthedocs.io/en/master/")
    (synopsis "Polynomials as a numpy datatype")
    (description "Numpoly is a generic library for creating, manipulating and
evaluating arrays of polynomials based on @code{numpy.ndarray objects}.")
    ;; Tests fail with dtype mismatches on 32-bit architectures, suggesting
    ;; that numpoly only supports 64 bit platforms.
    (supported-systems '("x86_64-linux" "aarch64-linux" "powerpc64le-linux"))
    (license license:bsd-2)))

(define-public python-numpy-groupies
  (package
    (name "python-numpy-groupies")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "numpy_groupies" version))
       (sha256
        (base32 "1q13gi0018maifhn6dkwi0pprr3p7ikv9r3zffg6p1ayspdazm5f"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-numba
           python-pandas
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/ml31415/numpy-groupies")
    (synopsis "Tools for group-indexing operations: aggregated sum and more")
    (description
     "This package provides optimized tools for group-indexing operations:
aggregated sum and more.")
    (license license:bsd-3)))

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

(define-public python-osfclient
  (package
    (name "python-osfclient")
    (version "0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/osfclient/osfclient")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ympjh028xgwkzvhwqa31rack1h8nni7zzn2alp1819m4pm8hysn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; 2 tests fail with assertion not equal:
      ;; AssertionError: assert 16 == (4 + (4 * 2))
      ;; See: <https://github.com/osfclient/osfclient/issues/214>.
      #~(list "-k"
              (string-append "not test_recursive_upload"
                             " and not nottest_recursive_upload_with_subdir"))))
    (native-inputs
     (list python-pytest
           python-mock
           python-setuptools))
    (propagated-inputs
     (list python-requests
           python-six
           python-tqdm))
    (home-page "https://github.com/osfclient/osfclient")
    (synopsis "Python library and command-line client for file storage on OSF")
    (description
     "The @code{osfclient} is a python library and a command-line client for
up- and downloading files to and from @url{https://osf.io/, Open Science
Framework} projects.  The @acronym{OSF, Open Science Framework} is an open
source project which facilitates the open collaboration of researchers on the
web, by sharing data and other research outputs.")
    (license license:bsd-3)))

(define-public python-osqp
  (package
    (name "python-osqp")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/osqp/osqp-python")
             (commit (string-append "v" version))
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b5j0hv6dlbs3dm9xvs2ijnjr2r8xnchs4jyk3dx16qhcp85wklb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 47 passed, 3 skipped, 1027 warnings
      #:test-flags
      ;; These tests require the module "vec_emosqp", which we don't have.
      #~(list "--ignore=src/osqp/tests/codegen_vectors_test.py"
             ;; These tests need "mat_emosqp".
             "--ignore=src/osqp/tests/codegen_matrices_test.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-osqp-source-location
            ;; XXX: Maybe implement it as CMake configure flags, otherwise
            ;; each package depending on osqp needs to have this phase?
            (lambda _
              (copy-recursively #$(package-source
                                   (this-package-native-input "osqp"))
                                "osqp")
              (substitute* "osqp/algebra/_common/lin_sys/qdldl/qdldl.cmake"
                (("Fetching/configuring QDLDL solver")
                 (format #f "Adding/configuring QDLDL solver from: ~a"
                         #$(package-source
                            (this-package-native-input "qdldl"))))
                (("GIT_REPOSITORY https://github.com/osqp/qdldl\\.git")
                 (format #f "SOURCE_DIR ~a"
                         #$(package-source
                            (this-package-native-input "qdldl"))))
                (("GIT_TAG v0.1.8")
                 ""))
              (substitute* "CMakeLists.txt"
                (("Fetching/configuring OSQP")
                 (format #f "Adding/configuring OSQP: ~a"
                         (string-append (getcwd) "/osqp")))
                (("GIT_REPOSITORY https://github.com/osqp/osqp\\.git")
                 (format #f "SOURCE_DIR ~a"
                         (string-append (getcwd) "/osqp")))
                (("GIT_TAG v1.0.0")
                 ""))))
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list cmake-minimal
           osqp
           pybind11
           python-pytest
           python-pytorch
           python-scikit-build-core
           python-setuptools-scm
           qdldl))
    (propagated-inputs
     (list python-jinja2
           python-joblib
           python-numpy
           python-scipy
           python-setuptools))
    (home-page "https://osqp.org/")
    (synopsis "OSQP: operator splitting QP solver")
    (description "The OSQP (Operator Splitting Quadratic Program) solver is a
numerical optimization package.")
    (license license:asl2.0)))

(define-public python-particle
  (package
    (name "python-particle")
    (version "0.25.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "particle" version))
       (sha256
        (base32 "0as50k5hinxszsm6lnghnmx2cyjy77c0i2gvzf2q64g2x5b7xkvq"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-attrs
                             python-hepunits
                             python-typing-extensions))
    (native-inputs (list python-hatch-vcs
                         python-hatchling
                         python-pandas
                         python-pytest
                         python-pytest-benchmark
                         python-tabulate))
    (home-page "https://github.com/scikit-hep/particle")
    (synopsis "Extended PDG particle data and MC identification codes")
    (description
     "@code{Particle} provides a pythonic interface to the Particle Data Group
(PDG) particle data tables and particle identification codes, with extended
particle information and extra goodies.")
    (license license:bsd-3)))

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

(define-public python-pyamg
  (package
    (name "python-pyamg")
    (version "5.3.0")
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
                "02w6xy3i0qcpsfc64zw8k15mi6qykq65h3d98vi9p6fdlkqx08sk"))))
    (arguments
     (list
      ;; tests: 40 passed, 1 skipped
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
                (invoke "python" "bindthem.py" "bind_examples.h"))))
          (add-before 'check 'pre-check
            (lambda _
              (copy-recursively "pyamg/tests" "tests")
              (delete-file-recursively "pyamg"))))))
    (build-system pyproject-build-system)
    (native-inputs
     (list pybind11
           python-cppheaderparser
           python-pytest
           python-pyyaml
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy
           python-scipy))
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

(define-public python-pykdtree
  (package
    (name "python-pykdtree")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pykdtree" version))
       (sha256
        (base32 "1xb5xdp32s5ffcbbb6vlrj4i70hdknajvr9yhzx0wld52rx9caxx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; FIXME: Tests are unable to import properly, but it seems to work in
      ;; real conditions.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'fix-site-packages
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (with-directory-excursion (site-packages inputs outputs)
                (for-each delete-file (find-files "." "test*"))))))))
    (native-inputs
     (list python-cython python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/storpipfugl/pykdtree")
    (synopsis "Fast kd-tree implementation with OpenMP-enabled queries")
    (description
     "@code{pykdtree} is a kd-tree implementation for fast nearest neighbour
search in Python.")
    (license license:lgpl3+)))

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
      ;; Tests takes about 10-15min to complete, and not thread save.
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
    (native-inputs (list python-numpydoc
                         python-poetry-core
                         python-pytest))
    (propagated-inputs (list python-pydicom
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

(define-public python-pyzx
  (package
    (name "python-pyzx")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)        ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/zxcalc/pyzx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
         (base32 "1bnmz08w1bmipir1wnn1k3fw64rply7891xns22qfj6yh0j1n6rj"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-flags
                     #~(list
                        ;; Ignore long running tests
                        "--ignore=tests/long_test.py"
                        "--ignore=tests/long_scalar_test.py")
                     #:phases
                     #~(modify-phases %standard-phases
                         (add-before 'check 'pre-check
                           (lambda* (#:key inputs outputs #:allow-other-keys)
                             (setenv "HOME" "/tmp")
                             ;; Matplotlib needs to be able to write its
                             ;; configuration file somewhere.
                             (setenv "MPLCONFIGDIR" "/tmp"))))))
    (native-inputs (list python-pytest
                         python-setuptools
                         python-wheel))
    (propagated-inputs (list python-ipywidgets
                             python-lark
                             python-numpy
                             python-pyperclip
                             python-tqdm))
    (home-page "https://github.com/zxcalc/pyzx")
    (synopsis "Quantum circuit rewriting and optimisation using the ZX-calculus")
    (description
     "PyZX is a Python tool implementing the theory of ZX-calculus for the
creation, visualisation, and automated rewriting of large-scale quantum
circuits.  PyZX currently allows you to:

@itemize
@item Read in quantum circuits in the file format of QASM, Quipper or Quantomatic;
@item Rewrite circuits into a pseudo-normal form using the ZX-calculus;
@item Extract new simplified circuits from these reduced graphs;
@item Visualise the ZX-graphs and rewrites using either Matplotlib, Quantomatic
or as a TikZ file for use in LaTeX documents;
@item Output the optimised circuits in QASM, QC or QUIPPER format.
@end itemize")
    (license license:asl2.0)))

(define-public python-qdldl
  (package
    (name "python-qdldl")
    (version "0.1.7.post5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qdldl" version))
       (sha256
        (base32 "0vi8dgrw32qj03z2dd3zqd0d625pibq3xmlgmidfsnwvqkhrj4qb"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list cmake-minimal
           pybind11
           python-pytest
           python-setuptools))
    (propagated-inputs (list python-numpy python-scipy))
    (home-page "https://github.com/oxfordcontrol/qdldl-python/")
    (synopsis "QDLDL LDL factorization routine")
    (description "This package provides a Python interface to the QDLDL LDL
factorization routine for quasi-definite linear system.")
    (license license:asl2.0)))

(define-public python-qutip
  (package
    (name "python-qutip")
    (version "5.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qutip" version))
       (sha256
        (base32 "0rl4piaj13g7g5i9wgdqc60q59dhk4lr34hw8v7xgnw6wkhiflb2"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (use-modules (ice-9 string-fun))
            ;; Delete cythonized files.  Not all cpp files are generated
            ;; by Cython, delete only those with accompanying Cython
            ;; file extensions (.pyx, .pxd).
            (for-each (lambda (file)
                        (when (or-map
                               (lambda (cython-ext)
                                 (file-exists? (string-replace-substring
                                                file ".cpp" cython-ext)))
                               (list ".pyx" ".pxd"))
                          (delete-file file)))
                      (find-files "." ".cpp"))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 15987 passed, 167 skipped, 95 deselected, 5 xfailed, 71 warnings
      #:test-flags
      #~(list "-m" "not flaky and not slow"      ;ignore flaky and slow tests
              "--ignore=tests/solver/")          ;depends on loky
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-local-source
            (lambda _
              (copy-recursively "qutip/tests" "tests")
              (delete-file-recursively "qutip"))))))
    (native-inputs
     (list python-cython
           python-pytest
           python-pytest-rerunfailures
           python-setuptools))
    (propagated-inputs
     (list python-numpy
           python-packaging
           python-scipy
           ;; [optional]
           python-cvxopt
           python-cvxpy
           ;; python-loky       ;not packaged yet in Guix
           python-mpi4py
           python-mpmath
           python-tqdm))
    (home-page "https://qutip.org")
    (synopsis "Quantum Toolbox in Python")
    (description
     "QuTiP is a library for simulating the dynamics of closed and open quantum
systems.  It aims to provide numerical simulations of a wide variety of quantum
mechanical problems, including those with Hamiltonians and/or collapse operators
with arbitrary time-dependence, commonly found in a wide range of physics
applications.")
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
        (base32 "1ai673k1s94s8b6pyxai8mk17p6zvvyi87rl236fs6ls8mpdklvc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "ruffus/test"
                  (invoke "bash" "run_all_unit_tests3.cmd"))))))))
    (native-inputs (list python-pytest python-setuptools))
    (home-page "http://www.ruffus.org.uk")
    (synopsis "Light-weight computational pipeline management")
    (description
     "Ruffus is designed to allow scientific and other analyses to be
automated with the minimum of fuss and the least effort.")
    (license license:expat)))

(define-public python-salib
  (package
    (name "python-salib")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/SALib/SALib")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v7l6qbxgclz644fq1vmakfasxcdhg1g019b5w47hlxqw8fx0ipl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 190 passed, 1 xfailed, 28 warnings
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-matplotlib
           python-multiprocess
           python-numpy
           python-pandas
           python-scipy))
    (home-page "https://salib.readthedocs.io/en/latest/")
    (synopsis "Tools for global sensitivity analysis")
    (description "SALib provides tools for global sensitivity analysis.  It
contains Sobol', Morris, FAST, DGSM, PAWN, HDMR, Moment Independent and
fractional factorial methods.")
    (license license:expat)))

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
    (version "11.0.0")
    (source
     (origin
       (method git-fetch)        ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/kinnala/scikit-fem")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13zh57raz2qcdfhsvpdlyiba5q0s0lh5b3gmsmh4cfrncrkdh6mh"))))
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
           python-setuptools))
    (propagated-inputs
     (list python-meshio
           python-numpy
           python-matplotlib
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
    (version "0.25.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/scikit-image/scikit-image")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cr3ki47z9g8kylnff1nrmv5fr3lrgmibl41q0v98pldghnslxdv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 8271 passed, 160 skipped, 1 deselected, 89 xfailed
      #:test-flags
      #~(list "--ignore=benchmarks/"
              "--pyargs" "skimage"
              ;; RuntimeWarning: divide by zero encountered in scalar divide
              "-k" "not test_ellipse_parameter_stability")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-local-source
            (lambda _
              (delete-file-recursively "skimage"))))))
    ;; See pyproject.toml for the list of build and run time requirements.
    ;; NOTE: scikit-image has an optional dependency on python-pooch, however
    ;; propagating it would enable many more tests that require online data.
    (propagated-inputs
     (list python-imageio
           python-lazy-loader
           python-networkx
           python-numpy
           python-packaging
           python-pillow
           python-scipy
           python-tifffile
           ;; [optional]
           ;; python-astropy
           ;; python-cloudpickle
           ;; python-dask
           ;; python-matplotlib
           ;; python-pooch
           ;; python-pyamg
           ;; python-pywavelets
           ;; python-scikit-learn
           #;python-simpleitk))
    (native-inputs
     (list meson-python
           python-cython
           python-pytest
           python-pytest-localserver
           python-pythran))
    (home-page "https://scikit-image.org/")
    (synopsis "Image processing in Python")
    (description
     "Scikit-image is a collection of algorithms for image processing.")
    (license license:bsd-3)))

;; XXX: Deprecated on <2025-12-12>.
(define-deprecated/public-alias python-scikit-image-next python-scikit-image)

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
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              ;; Drop test coverage requirements.
              (substitute* "pyproject.toml"
                (("--cov(-[^ ]*)?=[^ ]*") ""))))
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
                             python-spin))
    (native-inputs (list gfortran
                         pkg-config
                         python-cython
                         python-meson-python
                         python-numpy
                         python-numpydoc
                         python-pytest
                         python-setuptools
                         python-wheel))
    (home-page "https://has2k1.github.io/scikit-misc/stable")
    (synopsis "Miscellaneous tools for scientific computing.")
    (description "This package provides miscellaneous tools for data analysis
and scientific computing.")
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
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-numpy
           python-pytorch
           python-scipy))
    (home-page "https://github.com/guofei9987/scikit-opt")
    (synopsis "Swarm intelligence algorithms in Python")
    (description
     "Scikit-opt (or sko) is a Python module implementing @dfn{swarm
intelligence} algorithms: genetic algorithm, particle swarm optimization,
simulated annealing, ant colony algorithm, immune algorithm, artificial fish
swarm algorithm.")
    (license license:expat)))

(define-public python-scikit-optimize
  ;; XXX: The project might be not maintained, see
  ;; <https://github.com/holgern/scikit-optimize/issues/6>.
  (package
    (name "python-scikit-optimize")
    (version "0.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/holgern/scikit-optimize")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pc6avzxz8l32km5jvv3maih0a5x2akxybvxl2hdg04qz2l0kz8b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 398 passed, 1 skipped, 179 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "-k" (string-join
                    ;; XXX: As the project is not actively maintained, review
                    ;; failing test when a fresh release is available.
                    (list "not test_acq_optimizer"
                          "test_acq_optimizer_with_time_api"
                          "test_categorical_init_vals"
                          "test_consistent_x_iter_dimensions"
                          "test_early_stopping_delta_x"
                          "test_early_stopping_delta_x_empty_result_object"
                          "test_early_stopping_delta_y"
                          "test_early_stopping_delta_y_with_x0"
                          "test_exhaust_initial_calls"
                          "test_fixed_random_states"
                          "test_init_points_and_models"
                          "test_init_vals"
                          "test_init_vals_and_models"
                          "test_minimizer_api"
                          "test_minimizer_api_random_only"
                          "test_minimizer_space_constraint"
                          "test_minimizer_with_space"
                          "test_mixed_spaces"
                          "test_optimizer_base_estimator_string_smoke"
                          "test_optimizer_base_estimator_string_smoke_njobs"
                          "test_per_second_api"
                          "test_repeated_x"
                          "test_tree_based_minimize")
                    " and not "))))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-setuptools))
    (propagated-inputs
     (list python-joblib
           python-matplotlib
           python-numpy
           python-pyaml
           python-scikit-learn
           python-scipy))
    (home-page "https://scikit-optimize.github.io/")
    (synopsis "Sequential model-based optimization toolbox")
    (description
     "Scikit-Optimize, or @code{skopt}, is a simple and efficient library to
minimize (very) expensive and noisy black-box functions.  It implements
several methods for sequential model-based optimization.  @code{skopt} aims to
be accessible and easy to use in many contexts.")
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
      ;; tests: 82 passed
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              ;; Change from /homeless-shelter to /tmp for write
              ;; permission.
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-cython
           python-pandas
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-joblib
           python-numpy-1
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
  (package
    (name "python-scikit-survival")
    (version "0.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sebp/scikit-survival")
              (commit (string-append "v" version))
              ;; TODO: This package contains a copy of Eigen in
              ;; sksurv/linear_model/src.  It would be good to figure out
              ;; how to use our own Eigen package.
              (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r0xi9saz247ph6bgrc3iknq0m60d8rs4sl4mdv4s1zja80w1cp8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1013 passed, 105 warnings
      #:test-flags
      #~(list "--durations=10" ;to help in spotting long running tests
              "--numprocesses" (number->string (min 4 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                      #$(version-major+minor version)))))))
    (propagated-inputs
     (list python-ecos
           python-joblib
           python-numexpr
           python-numpy
           python-osqp
           python-pandas
           python-scikit-learn
           python-scipy))
    (native-inputs
     (list python-cython
           python-packaging
           python-pytest
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (home-page "https://github.com/sebp/scikit-survival")
    (synopsis "Survival analysis built on top of scikit-learn")
    (description "Scikit-survival is a Python module for survival analysis
built on top of scikit-learn.  It allows doing survival analysis while
utilizing the power of scikit-learn, e.g., for pre-processing or doing
cross-validation.")
    (license license:gpl3+)))

(define-public python-scipy
  (package
    (name "python-scipy")
    (version "1.16.3")
    ;; TODO: PyPI archive bundles extra in subprojects:
    ;; - https://github.com/boostorg/math
    ;; - https://github.com/scipy/HiGHS
    ;; - https://github.com/scipy/xsf
    ;; - qhull
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scipy" version))
       (sha256
        (base32 "1jxf6mjr3whbh23p8bnlcyiss5rsamq37qgys8xz8qi781cpds01"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 78689 passed, 4982 skipped, 319 xfailed, 19 xpassed, 2660 warnings
      #:configure-flags
      ''(("setup-args" . #("-Duse-system-libraries=all")))
      #:test-flags
      #~(list "--durations=10"
              "--numprocesses" (number->string (min 4 (parallel-job-count)))
              "--pyargs" "scipy"
              "-k" (string-join
                    ;; Network access is requied.
                    (list "not test_ascent"
                          "test_electrocardiogram"
                          "test_existence_all"
                          "test_face"
                          ;; pycparser.ply.yacc.YaccError: Unable to build parser
                          "test_callbacks"
                          "test_bad_callbacks"
                          ;; AssertionError: Items are not equal: ACTUAL:
                          ;; np.complex128(inf+nanj) DESIRED: (inf+0j)
                          "test_expm1_complex"
                          ;; AssertionError: Not equal to tolerance rtol=1e-07, atol=0
                          "test_log1p_complex"
                          ;; AssertionError: Not equal to tolerance rtol=5e-09, atol=0
                          "test_nctdtr_accuracy[3.0-5.0--2.0-1.5645373999149622e-09-5e-09]"
                          ;; Bad results (X out of Y) for the following points
                          ;; (in output 0):
                          "test_spherical_in_complex"
                          "test_spherical_jn_complex"
                          "test_spherical_kn"
                          "test_spherical_yn_complex"
                          ;; Not equal to tolerance rtol=1e-07, atol=0
                          "test_negative_real_gh14582[spherical_in-False]"
                          "test_negative_real_gh14582[spherical_in-True]"
                          "test_negative_real_gh14582[spherical_jn-False]"
                          "test_negative_real_gh14582[spherical_jn-True]"
                          "test_negative_real_gh14582[spherical_yn-False]"
                          "test_negative_real_gh14582[spherical_yn-True]"
                          ;; Failed: DID NOT WARN. No warnings of type (<class
                          ;; 'RuntimeWarning'>,) were emitted.
                          "test_boost_eval_issue_14606")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "meson.build"
                ;; boost
                (("1.88.0") "1.89.0"))))
          (add-after 'set-paths 'hide-gfortran
            ;; See: <https://issues.guix.gnu.org/73439#45>.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gfortran #$(this-package-native-input "gfortran")))
                (setenv "CPLUS_INCLUDE_PATH"
                        (string-join
                         (delete (string-append gfortran "/include/c++")
                                 (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                         ":")))))
          (replace 'check
            (lambda* (#:key test-flags tests? #:allow-other-keys)
              (when tests?
                ;; Step out of the source directory to avoid interference.
                ;; See: <.github/workflows/linux.yml> for any other posible
                ;; tests setup.
                (with-directory-excursion "/tmp"
                  (setenv "HOME" "/tmp")
                  (setenv "PYTHONOPTIMIZE" "2")
                  (apply invoke "pytest" "-vv" test-flags))))))))
    (native-inputs
     (list gfortran
           meson-python
           pkg-config
           python-click
           python-cython
           python-doit
           python-hypothesis
           python-mpmath
           python-numpydoc
           python-pooch
           python-pycodestyle
           python-pydevtool
           python-pytest
           python-pytest-timeout
           python-pytest-xdist
           python-pythran
           python-rich-click
           python-threadpoolctl
           python-typing-extensions))
    (inputs
     (list boost
           openblas
           pybind11
           qhull
           xsimd))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://scipy.org/")
    (synopsis "The Scipy library provides efficient numerical routines")
    (description "The SciPy library is one of the core packages that make up
the SciPy stack.  It provides many user-friendly and efficient numerical
routines such as routines for numerical integration and optimization.")
    (license license:bsd-3)))

(define-public python-simple-pid
  (package
    (name "python-simple-pid")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "simple_pid" version))
              (sha256
               (base32
                "17p9bgka5yv5lbnbk374yjccrlizm572wv3xb479072lahf7cwap"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/m-lundberg/simple-pid")
    (synopsis "Easy to use PID controller")
    (description "This package provides a simple and easy-to-use @acronym{PID,
proportional-integral-derivative} controller.")
    (license license:expat)))

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

(define-public python-snakemake-executor-plugin-slurm-jobstep
  (package
    (name "python-snakemake-executor-plugin-slurm-jobstep")
    (version "0.3.0")
    (home-page "https://github.com/snakemake/snakemake-executor-plugin-slurm-jobstep")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ly15ywmbfcm5z7jy7dxiidpw3immsdd2k80vrm4pza721irxcar"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "python3" "tests/tests.py")))))))
    (native-inputs (list python-poetry-core
                         snakemake))
    (synopsis "Snakemake executor plugin: slurm-jobstep")
    (description "A Snakemake executor plugin for running srun jobs inside of
SLURM jobs (meant for internal use by python-snakemake-executor-plugin-slurm).")
    (license license:expat)))

(define-public python-snakemake-executor-plugin-slurm
  (package
    (name "python-snakemake-executor-plugin-slurm")
    (version "1.7.0")
    (home-page "https://github.com/snakemake/snakemake-executor-plugin-slurm/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x7ghrkvmxqbcjl69hxp5axa1av3s0mdc0i9xjg8qjnd3hgd82r3"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "python3" "tests/tests.py")))))))
    (native-inputs (list python-pandas
                         python-poetry-core
                         python-pytest
                         python-snakemake-executor-plugin-slurm-jobstep
                         snakemake))
    (synopsis "Snakemake executor plugin: slurm")
    (description "A Snakemake executor plugin for running SLURM jobs.")
    (license license:expat)))

(define-public python-sparse
  (package
    (name "python-sparse")
    (version "0.15.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sparse" version))
       (sha256
        (base32
         "0rp29gp82qwwkq210pzh2qmlqhi2007nb7p7nwqmrkgmjq6cwxjc"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-numba python-numpy python-scipy))
    (native-inputs
     (list python-dask
           python-pytest
           python-pytest-cov
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/pydata/sparse/")
    (synopsis "Library for multi-dimensional sparse arrays")
    (description
     "This package implements sparse arrays of arbitrary dimension on top of
@code{numpy} and @code{scipy.sparse}.  Sparse array is a matrix in which most
of the elements are zero.  @code{python-sparse} generalizes the
@code{scipy.sparse.coo_matrix} and @code{scipy.sparse.dok_matrix} layouts, but
extends beyond just rows and columns to an arbitrary number of dimensions.
Additionally, this project maintains compatibility with the
@code{numpy.ndarray} interface rather than the @code{numpy.matrix} interface
used in @code{scipy.sparse}.  These differences make this project useful in
certain situations where @code{scipy.sparse} matrices are not well suited, but
it should not be considered a full replacement.  It lacks layouts that are not
easily generalized like @dfn{compressed sparse row/column}(CSR/CSC) and
depends on @code{scipy.sparse} for some computations.")
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
      ;; tests: 276 passed, 14 skipped, 2 deselected
      #:test-flags
      #~(list
         ;; One test fails with error: AssertionError: False is not true : 5
         ;; lines are different, starting at line 1
         "--deselect=tdda/test_tdda.py::TestOne::test_ddiff_values_output"
         ;; TypeError: 'property' object is not iterable 
         "--deselect=tdda/test_tdda.py::TestPandasDataFrames::test_types_match")
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
           python-setuptools))
    (home-page "https://www.stochasticsolutions.com")
    (synopsis "Test-driven data analysis library for Python")
    (description
     "The TDDA Python module provides command-line and Python API support
for the overall process of data analysis, through tools that perform
reference testing, constraint discovery for data, automatic inference
of regular expressions from text data and automatic test generation.")
    (license license:expat))) ; MIT License

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
           python-pynose
           python-pytest
           python-setuptools
           python-xarray))
    (home-page "https://github.com/jupyter-widgets/traittypes")
    (synopsis "Trait types for NumPy, SciPy and friends")
    (description "The goal of this package is to provide a reference
implementation of trait types for common data structures used in the scipy
stack such as numpy arrays or pandas and xarray data structures.  These are
out of the scope of the main traitlets project but are a common requirement to
build applications with traitlets in combination with the scipy stack.")
    (license license:bsd-3)))

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

(define-public python-tspex
  (package
    (name "python-tspex")
    ;; 0.6.3 is not tagged; must be this one, as it is the latest, from the
    ;; day of the release, and the commit message is "Bump to 0.6.3".
    (properties '((commit . "d393ff497b7c14d673e792bd6c84ddd734be1239")
                  (revision . "0")))
    (version (git-version "0.6.3"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/apcamargo/tspex")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0khhnahhn0jp9y14q6wgq0xqadqszwn1iq3y562bhfmv09f4j1ik"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-coverage
            (lambda _
              (substitute* "pytest.ini"
                (("--cov.*") ""))))
          ;; The seaborn styles have different names.
          (add-after 'unpack 'fix-seaborn
            (lambda _
              (substitute* (find-files "." ".py$")
                (("seaborn-") "seaborn-v0_8-")))))))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-pandas
           python-seaborn
           python-xlrd))
    (home-page "https://apcamargo.github.io/tspex/")
    (synopsis "Calculate tissue-specificity metrics for gene expression")
    (description
     "This package provides a Python package for calculating
tissue-specificity metrics for gene expression.")
    (license license:gpl3+)))

(define-public python-pandas
  (package
    (name "python-pandas")
    (version "2.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pandas-dev/pandas")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qf4frgj31kd9i544n8v03a0bv9mgml3f7n9n1rik187q3r8ygfg"))
       (patches (search-patches "python-pandas-2-no-pytz_datetime.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 174115 passed, 24224 skipped, 990 xfailed, 77 xpassed, 110 warnings
      #:test-flags
      #~(list "-m" (string-join
                    (list "not db" "network" "single_cpu" "slow" "slow_arm")
                    " and not ")
              "--numprocesses" (number->string (min 4 (parallel-job-count)))
              "-k" (string-join
                    (list "not test_git_version"
                          "test_parsing_tzlocal_deprecated"
                          "test_show_versions_console")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'version-set-by-guix
            (lambda _
              (with-output-to-file "_version.py"
                (lambda _
                  (display
                   (string-append "__version__ = \"" #$version "\""))))))
          (replace 'check
            (lambda* (#:key inputs outputs test-flags tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (with-directory-excursion
                    (string-append (string-append (site-packages inputs outputs)
                                                  "/pandas"))
                  (apply invoke "pytest" "-vv" test-flags))))))))
    (propagated-inputs
     (list python-numpy
           python-dateutil
           python-pytz
           python-tzdata
           ;; XXX: Pandas lists a lot of optional dependencies which are not
           ;; hard requirements, leave them listed here and commented out for
           ;; the reference purpose. Try to keep closure as bare minimal as
           ;; possible.
           ;;
           ;; [optional]
           ;; python-adbc-driver-postgresql
           ;; python-adbc-driver-sqlite
           ;; python-beautifulsoup4
           ;; python-bottleneck
           ;; python-fastparquet
           ;; python-fsspec
           ;; python-gcsfs
           ;; python-html5lib
           ;; python-hypothesis
           ;; python-jinja2
           ;; python-lxml
           ;; python-matplotlib
           ;; python-numba
           ;; python-numexpr
           ;; python-odfpy
           ;; python-openpyxl
           ;; python-psycopg2
           ;; python-pyarrow
           ;; python-pyiceberg
           ;; python-pymysql
           ;; python-pyqt5
           ;; python-pyreadstat
           ;; python-python-calamine
           ;; python-pytz
           ;; python-pyxlsb
           ;; python-qtpy
           ;; python-s3fs
           ;; python-scipy
           ;; python-sqlalchemy
           ;; python-tables
           ;; python-tabulate
           ;; python-xarray
           ;; python-xlrd
           ;; python-xlsxwriter
           #;python-zstandard))
    (inputs
     (list xclip xsel))
    (native-inputs
     (list meson
           meson-python
           python-lxml
           python-pytest
           python-pytest-asyncio
           python-pytest-xdist
           python-versioneer
           tzdata-for-tests))
    (home-page "https://pandas.pydata.org")
    (synopsis "Data structures for data analysis, time series, and statistics")
    (description
     "Pandas is a Python package providing fast, flexible, and expressive data
structures designed to make working with structured (tabular,
multidimensional, potentially heterogeneous) and time series data both easy
and intuitive.  It aims to be the fundamental high-level building block for
doing practical, real world data analysis in Python.")
    (license license:bsd-3)))

(define-deprecated/public-alias python-pandas-2 python-pandas)

(define-public python-pandas-stubs
  (package
    (name "python-pandas-stubs")
    ;; The versioning follows that of Pandas and uses the date of the
    ;; python-pandas-stubs release.
    (version "2.3.3.251219")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pandas-dev/pandas-stubs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xh753wv5dbc59qp7fas323181mlblvhqmd9a4g7zzhaa2mxmzqs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1619 passed, 12 skipped, 7 deselected
      #:test-flags
      #~(list
         ;; ImportError: Missing optional dependency 'python-calamine'.
         "--deselect=tests/test_io.py::test_read_excel"
         "--deselect=tests/test_io.py::test_excel_reader")))
    (native-inputs
     (list python-beautifulsoup4
           python-fsspec
           python-html5lib
           python-jinja2
           python-loguru
           python-lxml
           python-matplotlib
           python-mypy
           python-numexpr
           python-odfpy
           python-openpyxl
           python-pandas
           python-poetry-core
           python-pyarrow
           python-pyreadstat
           python-pytest
           python-scipy
           python-sqlalchemy-2
           python-tabulate
           python-typing-extensions
           python-xarray
           python-xlrd
           python-xlsxwriter
           ;; Not packaged yet
           ;; 
           ;; python-calamine
           ;; python-poethepoet
           ;; python-pyarrow-stubs
           ;; python-pyrefly
           ;; python-python-calamine
           ;; python-pyxlsb
           ;; python-scipy-stubs
           ;; python-ty
           ;; python-types-python-dateutil
           tzdata-for-tests))
    (propagated-inputs
     (list python-numpy
           python-types-pytz))
    (home-page "https://pandas.pydata.org")
    (synopsis "Type annotations for pandas")
    (description
     "This package contains public type stubs for @code{python-pandas}, following
the convention of providing stubs in a separate package, as specified in
@acronym{PEP, Python Enhancement Proposal} 561.  The stubs cover the most
typical use cases of @code{python-pandas}.  In general, these stubs are
narrower than what is possibly allowed by @code{python-pandas}, but follow a
convention of suggesting best recommended practices for using
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
    (version "0.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandera" version))
       (sha256
        (base32 "1x5vjp1ra252ncyfsfrc7vck5snx807mpwzd0hvv0vpi9v096934"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 3033 passed, 51 skipped, 11 xfailed, 8385 warnings
      #:test-flags
      ;; With higher threads count tests randomly fail during collection.
      #~(list "--numprocesses" (number->string (min 4 (parallel-job-count)))
              ;; TODO: Ignore tests for not packaged python-ibis-framework,
              ;; python-polars, and python-pyspark.
              "--ignore=tests/ibis"
              "--ignore=tests/polars"
              "--ignore=tests/pyspark"
              ;; E ModuleNotFoundError: No module named 'sphinx'
              "--ignore=tests/pandas/test_docs_setting_column_widths.py"
              ;; Nework access is required.
              "--ignore=tests/fastapi/test_app.py"
              ;; TypeError: __class__ assignment: 'GeoDataFrame' object layout
              ;; differs from 'DataFrame'
              "-k" (string-join
                    (list "not test_schema_model[data0-True]"
                          "test_schema_from_dataframe[data1-True]"
                          "test_schema_no_geometry"
                          ;; The most of the tests from this goup XFAIL or fail.
                          "test_pandas_stubs_false_positives")
                    " and not "))))
    (native-inputs
     (list python-joblib
           python-pytest
           python-pytest-asyncio
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm
           python-uvicorn))
    (inputs
     ;; [optional]
     ;; Pandera comes with a lot of extras. We test as many as possible, but do
     ;; not include all of them in the propagated-inputs. Currently, we have to
     ;; skip the pyspark and io tests due to missing packages python-pyspark
     ;; and python-frictionless.
     (list python-dask
           python-distributed
           python-geopandas
           python-hypothesis
           ;; python-ibis-framework ;missing from Guix
           python-modin
           python-numpy
           python-pandas
           ;; python-polars         ;missing from Guix
           ;; python-pyspark        ;missing from Guix
           ;; python-ray            ;missing from Guix
           python-scipy
           python-shapely))
    (propagated-inputs
     (list python-packaging
           python-pydantic
           python-typeguard
           python-typing-extensions
           python-typing-inspect))
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
    (version "0.31.0")
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
        (base32 "06y6fvydrsjqdpbd20icd194693x127qhb19fgw248jfjyg5ga44"))))
    (build-system pyproject-build-system)
    ;; Pyjanitor has an extensive test suite. For quick debugging, the tests
    ;; marked turtle can be skipped using "-m" "not turtle".
    (arguments
     (list
      ;; tests: 1042 passed, 2 skipped, 2 deselected, 45 xfailed, 6 xpassed,
      ;;        735 warnings
      #:test-flags
      ;; The tests take quite long, so consider adding the "-n" line and
      ;; adding python-pytest-xdist to the native-inputs when testing.
      ;; However, the tests are not deterministic when ran with -n, so
      ;; disable again before committing.
      #~(list ;; "-n" (number->string (parallel-job-count))
              ;; Test files are not included.
              "--ignore=tests/io/test_read_csvs.py"
              ;; Polars has not been packaged yet.
              "--ignore=tests/polars"
              ;; PySpark has not been packaged yet.
              "--ignore=tests/spark/functions/test_clean_names_spark.py"
              "--ignore=tests/spark/functions/test_update_where_spark.py"
              ;; Tries to connect to the internet.
              "-k" (string-append "not test_is_connected"
                                  ;; Test files are not included.
                                  " and not test_read_commandline_bad_cmd"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-env-ci
            (lambda _
              ;; Some tests are skipped if the JANITOR_CI_MACHINE
              ;; variable is not set.
              (setenv "JANITOR_CI_MACHINE" "1"))))))
    ;; TODO: Remove python-requests and inject its target data to make the
    ;; package behaviour reproducible.
    (propagated-inputs (list python-multipledispatch
                             python-natsort
                             python-pandas-flavor
                             python-requests
                             python-scipy
                             ;; Optional imports.
                             python-biopython ;biology submodule
                             python-unyt)) ;engineering submodule
    (native-inputs (list python-pytest
                         ;;python-pytest-xdist ;only for -n when testing
                         python-setuptools
                         ;; Optional imports. We do not propagate them due to
                         ;; their size.
                         python-numba ;speedup of joins
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/serge-sans-paille/pythran")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rm9lfbz5qvah1m0rr5gaaahkf1gzwlw1ysvym2l2yh0clglav94"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; FIXME: find more reliable tests file(s), all tests from
      ;; test_typing.py fail with error: ModuleNotFoundError: No module named
      ;; 'distutils.msvccompiler'.
      #:tests? #f))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list boost                 ;headers need to be available
           xsimd
           python-beniget
           python-gast
           python-numpy
           python-ply))
    (home-page "https://github.com/serge-sans-paille/pythran")
    (synopsis "Ahead of Time compiler for numeric kernels")
    (description
     "Pythran is an ahead of time compiler for a subset of the Python
language, with a focus on scientific computing.  It takes a Python module
annotated with a few interface descriptions and turns it into a native
Python module with the same interface, but (hopefully) faster.")
    (license license:bsd-3)))

(define-public python-pyts
  (let ((commit "4f3d97bcb1016d33dbfaef68c0931756a4552410")
        (revision "0"))
    (package
      (name "python-pyts")
      (version (git-version "0.13.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/johannfaouzi/pyts")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16hlxwajdz44qs8vi7bhiania2b3201fv3pqiwsx79rb554bvl66"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        #~(list "--pyargs" "pyts"
                ;; Most likely a flaky test.
                "--deselect=preprocessing/transformer.py::\
pyts.preprocessing.transformer.QuantileTransformer")))
      (propagated-inputs
       (list python-joblib
             python-numba
             python-numpy
             python-scikit-learn
             python-scipy))
      (native-inputs
       (list python-pytest python-pytest-cov python-setuptools))
      (home-page "https://github.com/johannfaouzi/pyts")
      (synopsis "Python package for time series classification")
      (description
       "pyts is a Python package for time series classification.  It aims to
make time series classification easily accessible by providing preprocessing
and utility tools, and implementations of state-of-the-art algorithms.  Most
of these algorithms transform time series, thus pyts provides several tools to
perform these transformations.")
      (license license:bsd-3))))

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

;; XXX: Not maintained since 2019. The project was archived by the owner on
;; Nov 2, 2020. It is now read-only.
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
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; TypeError: 'dia_matrix' object is not subscriptable
      #~(list "--deselect=fbpca.py::TestPCA::test_sparse"
              "fbpca.py")))
    (native-inputs (list python-pytest python-setuptools))
    (propagated-inputs
     (list python-numpy python-scipy))
    (home-page "https://fbpca.readthedocs.io/")
    (synopsis "Functions for principal component analysis and accuracy checks")
    (description
     "This package provides fast computations for @dfn{principal component
analysis} (PCA), SVD, and eigendecompositions via randomized methods")
    (license license:bsd-3)))

(define-public python-einops
  (package
    (name "python-einops")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch) ;PyPI misses .ipynb files required for tests
       (uri (git-reference
             (url "https://github.com/arogozhnikov/einops")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07xd5a4sya3mr003f17hxykcbq3zf3mnr51qagv7fy55qcnbkn97"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Skip optional dependency on Jupyter during tests.
      #~(list "--ignore=scripts/test_notebooks.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-backend
            (lambda _
              ;; Einops supports different backends, but we test
              ;; only NumPy for availability and simplicity.
              (setenv "EINOPS_TEST_BACKENDS" "numpy"))))))
    (native-inputs
     (list python-hatchling
           python-nbconvert
           python-nbformat
           python-parameterized
           python-pytest))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://einops.rocks/")
    (synopsis "Tensor operations for different backends")
    (description
     "Einops provides a set of tensor operations for NumPy and multiple deep
learning frameworks.")
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

(define-public python-unyt
  (package
    (name "python-unyt")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "unyt" version))
       (sha256
        (base32 "04qjjv5zga7dh355ygsvkckfqi86nf03w6ckw5zm0120xw9p1shp"))))
    (build-system pyproject-build-system)
    ;; tests: 647 passed, 56 skipped, 2 xfailed
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy
           python-packaging
           python-sympy))
    (home-page "https://unyt.readthedocs.io")
    (synopsis "Library for working with data that has physical units")
    (description
     "@code{unyt} is a Python library working with data that has physical
units.  It defines the @code{unyt.array.unyt_array} and
@code{unyt.array.unyt_quantity} classes (subclasses of NumPy’s ndarray class)
for handling arrays and scalars with units,respectively")
    (license license:bsd-3)))

(define-public python-uproot
  (package
    (name "python-uproot")
    (version "5.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "uproot" version))
       (sha256
        (base32 "024k5kjwcd2nw5hfxhpl0x9p5aq0qrg0nlh9v24vr39rcqadh52a"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         ;; conftest.py is not required and it imports modules we do not use.
         "--noconftest"
         ;; There is no easy way to skip tests that require the network, so
         ;; just run a handful of tests that pass.
         "tests/test_0351_write_TList.py"
         "tests/test_0352_write_THashList.py"
         "tests/test_0439_check_awkward_before_numpy.py"
         "tests/test_0976_path_object_split.py"
         "tests/test_1198_coalesce.py"
         "tests/test_1264_write_NumPy_array_of_strings.py"
         "tests/test_1318_dont_compare_big_endian_in_awkward.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              ;; Version is determined dynamically from .git.
              (substitute* "pyproject.toml"
                (("\\[project\\]")
                 (string-append "[project]" "\n" "version = \""
                                #$version "\""))
                (("\"version\"") "")))))))
    (native-inputs
     (list python-hatch-vcs
           python-pytest
           python-pytest-timeout
           python-setuptools))
    (propagated-inputs
     (list python-awkward
           python-cramjam
           python-fsspec
           python-numpy
           python-packaging
           python-xxhash))
    (home-page "https://uproot.readthedocs.io")
    (synopsis "ROOT I/O in Python using NumPy")
    (description
     "Uproot is a Python library for reading and writing ROOT files.  It uses
NumPy and does not depend on C++ ROOT.")
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

(define-public python-vaex-core
  (package
    (name "python-vaex-core")
    (version "4.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://www.github.com/maartenbreddels/vaex")
              (commit (string-append "core-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m6h6m0vm8vdx2nk26nvlbyfvlj0g9ph8cdh38258gn18fd2db0l"))
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
     (list
      ;; TODO: Require vaex.server and others, which require vaex-core;
      ;; implement bootsrapping.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.py"
                ;; "dask!=2022.4.0,<2024.9"; there is a note "fingerprinting
                ;; in no longer deterministic as of 2024.9.0" which may be
                ;; resolved in 2024.12.1.
                ((",<2024.9") ""))))
          (add-before 'build 'patch-missing-include
            (lambda _
              ;; See: <https://github.com/vaexio/vaex/issues/2382>.
              ;; TODO: Update to the latest version including the fix.
              (substitute* "src/string_utils.hpp"
                (("#include <nonstd/string_view.hpp>")
                 "#include <cstdint>\n#include <nonstd/string_view.hpp>")))))))
    (native-inputs
     (list pybind11
           python-cython
           python-setuptools))
    (inputs
     (list boost
           pcre
           string-view-lite
           tsl-hopscotch-map))
    (propagated-inputs
     (list python-aplus
           python-blake3
           python-cloudpickle
           python-dask
           python-filelock
           python-frozendict
           python-future
           python-nest-asyncio
           python-numpy
           python-pandas
           python-pyarrow
           python-pydantic
           python-pyyaml
           python-rich
           python-six            ;hard dependency
           python-tabulate
           ;; [optional]
           python-diskcache
           python-fsspec
           ;; python-gcsfs       ;not packaged yet in Guix
           python-graphviz
           python-h5py
           python-httpx
           ;; python-ipyvolume  ;not packaged yet in Guix
           python-psutil
           python-s3fs))
    (home-page "https://www.github.com/maartenbreddels/vaex")
    (synopsis "Core of Vaex library for exploring tabular datasets")
    (description "Vaex is a high performance Python library for lazy
Out-of-Core DataFrames (similar to Pandas), to visualize and explore big
tabular datasets.  This package provides the core modules of Vaex.")
    (license license:expat)))

(define-public python-vector
  (package
    (name "python-vector")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vector" version))
       (sha256
        (base32 "0hnnld7yqxb1i6miqfac7nmd0fn7xbvq1w9akp278b1jaclyl1cb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 813 passed, 3 skipped, 1 deselected
      #:test-flags
      ;; assert array([2.]) == array([-2.])
      #~(list "--deselect=tests/test_issues.py::test_issue_443" )))
    (native-inputs
     (list ;; python-dask-awkward   ;not packaged yet in Guix
           python-hatch-vcs
           python-hatchling
           ;; python-jax            ;not packaged yet in Guix
           python-notebook
           python-optree
           python-papermill
           python-pytest
           #;python-spark-parse))   ;not packaged yet in Guix
    (propagated-inputs
     (list python-numpy
           python-packaging
           ;; [optional]
           python-awkward
           python-numba
           python-sympy))
    (home-page "https://github.com/scikit-hep/vector")
    (synopsis "Arrays of 2D, 3D, and Lorentz vectors")
    (description "Vector is a Python library for 2D and 3D spatial vectors, as
well as 4D space-time vectors.  It is especially intended for performing
geometric calculations on arrays of vectors, rather than one vector at a time
in a Python @code{for} loop.")
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

(define-public python-xarray
  (package
    (name "python-xarray")
    (version "2025.12.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "xarray" version))
              (sha256
               (base32
                "1vczqm5daz79n7w3ycd0m1wf0bf78wd84w6xbgac8sfcvkxadxkk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 6677 passed, 9632 skipped, 14 xfailed, 4 xpassed, 53 warnings 
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                ((".*--mypy-.*") "")))))))
    (native-inputs
     (list python-pytest
           python-pytest-asyncio
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy
           python-packaging
           python-pandas))
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

(define-public python-xarray-dataclass
  (package
    (name "python-xarray-dataclass")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/xarray-contrib/xarray-dataclass/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q3y9gbzrp1mh48y7gggqgggwnarxdn32h907mfax1hi9ap6ywil"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-hatchling))
    (propagated-inputs
     (list python-numpy
           python-typing-extensions
           python-xarray))
    (home-page "https://github.com/xarray-contrib/xarray-dataclass/")
    (synopsis "Xarray data creation by data classes")
    (description
     "xarray-dataclass is a Python package that makes it easy to create
@url{https://xarray.pydata.org/en/stable/index.html, xarray}'s DataArray and
Dataset objects that are \"typed\" (i.e. fixed dimensions, data type,
coordinates, attributes, and name) using
@url{https://docs.python.org/3/library/dataclasses.html, the Python's
dataclass}.  It's a successor of not maintained
https://github.com/astropenguin/xarray-dataclasses.")
    (license license:expat)))

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

(define-public python-xarray-einstats
  (package
    (name "python-xarray-einstats")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/arviz-devs/xarray-einstats")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11crz1l6swabwzmwbrxypfw8gbbp81higzgi5nsxzfigcrrqq30a"))))
    (build-system pyproject-build-system)
    ;; tests: 317 passed, 48 skipped 
    (native-inputs
     (list python-flit-core
           python-pytest))
    (propagated-inputs
     (list python-numpy
           python-scipy
           python-xarray
           ;; [optinoal]
           python-einops
           python-numba))
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
    (version "2.36.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pymc-devs/pytensor")
                    (commit (string-append "rel-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m9nmwnn0ixrk9ml0yc6qjdcw0vmw7swfcw8vahyfmp70pirhimn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1906 passed, 2581 skipped, 34 deselected, 12 xfailed, 274 warnings
      #:test-flags
      ;; XXX: Full test suite takes about 20-30min to complete in single
      ;; thread, attempt to run tests in parallel with pytest-xdist fails even
      ;; so upstream provides a support for that, try to figure out how to
      ;; improve it.
      ;;
      ;; Upstream implements a script, showing slow tests which may be used to
      ;; exclude even more hanging/slow ones, see:
      ;; <scripts/slowest_tests/extract-slow-tests.py>.
      ;;
      #~(list "--benchmark-disable"
              "--benchmark-skip"
              "--durations=50"
              ;; Skip computationally intensive tests.
              "--ignore=tests/scan/"
              "--ignore=tests/tensor/"
              "--ignore=tests/sandbox/"
              "--ignore=tests/sparse/sandbox/"
              ;; A mixture of assertions are not equal in these tests.
              "--deselect=tests/link/numba/test_nlinalg.py::test_Eigh[x0-L-None]"
              #$@(map (lambda (test)
                        (string-append "--deselect=tests/graph/rewriting/"
                                       "test_basic.py::"
                                       test))
                      ;; Tests fail with similar errors: AssertionError:
                      ;; assert 'FunctionGraph(Op1(Op2(x, y), z))' ==
                      ;; 'FunctionGraph(Op4(z, y))'
                      (list "TestPatternNodeRewriter::test_replace_output"
                            "TestPatternNodeRewriter::test_nested_out_pattern"
                            "TestPatternNodeRewriter::test_unification_1"
                            "TestPatternNodeRewriter::test_replace_subgraph"
                            "TestPatternNodeRewriter::test_no_recurse"
                            "TestPatternNodeRewriter::test_multiple"
                            "TestPatternNodeRewriter::test_nested_even"
                            "TestPatternNodeRewriter::test_nested_odd"
                            "TestPatternNodeRewriter::test_expand"
                            "TestPatternNodeRewriter::test_ambiguous"
                            "TestPatternNodeRewriter::test_constant"
                            "TestPatternNodeRewriter::test_constraints"
                            "TestPatternNodeRewriter::test_match_same"
                            "TestPatternNodeRewriter::test_eq"
                            "TestEquilibrium::test_1"
                            "TestEquilibrium::test_2"
                            "TestEquilibrium::test_low_use_ratio"
                            ;; A mixture of assertions are not equal:
                            "TestPatternNodeRewriter::test_allow_multiple_clients"
                            "TestPatternNodeRewriter::test_op_pattern"
                            "test_patternsub_values_eq_approx[out_pattern0-True]"
                            "test_patternsub_values_eq_approx[out_pattern0-False]"
                            "test_patternsub_values_eq_approx[x-True]"
                            "test_patternsub_values_eq_approx[x-False]"
                            "test_patternsub_multi_output_nodes"))
              #$@(map (lambda (test)
                        (string-append "--deselect=tests/graph/" test))
                      (list "rewriting/test_kanren.py::test_kanren_basic"
                            "rewriting/test_kanren.py::test_KanrenRelationSub_filters"
                            "rewriting/test_kanren.py::test_KanrenRelationSub_dot"
                            "rewriting/test_unify.py::test_unify_Variable"
                            "rewriting/test_unify.py::test_ConstrainedVar"
                            "rewriting/test_unify.py::test_unify_OpPattern"
                            "test_destroyhandler.py::test_misc")))
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
              ;; This would otherwise interfere with finding the installed
              ;; pytensor when running tests.
              (delete-file-recursively "pytensor"))))))
    (native-inputs (list python-cython
                         python-pytest
                         python-pytest-benchmark
                         python-pytest-mock
                         python-pytest-xdist
                         python-versioneer
                         python-setuptools))
    (propagated-inputs (list python-cons
                             python-etuples
                             python-filelock
                             python-logical-unification
                             python-minikanren
                             python-numba
                             python-numpy
                             python-scipy
                             ;; [optinal]
                             ;; python-jaxlib
                             ;; python-jax
                             #;python-llvmlite))
    (home-page "https://pytensor.readthedocs.io/en/latest/")
    (synopsis
     "Library for mathematical expressions in multi-dimensional arrays")
    (description
     "PyTensor is a Python library that allows one to define, optimize, and
efficiently evaluate mathematical expressions involving multi-dimensional
arrays.  It is a fork of the Aesara library.")
    (license license:bsd-3)))

(define-public python-scs
  (package
    (name "python-scs")
    (version "3.2.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bodono/scs-python")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wg1g6das5hs53z3sjn2m8646023d14q9shmwfb38n23baqi468g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 26 passed
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'ensure-no-mtimes-pre-1980 'substitute-git-submodules
            (lambda _
              (rmdir "scs_source")
              (symlink #$(package-source
                          (this-package-native-input "scs"))
                       "scs_source")
              (rmdir "scs/pythoncapi-compat")
              (symlink #$(package-source
                          (this-package-native-input "pythoncapi-compat"))
                       "scs/pythoncapi-compat"))))))
    (native-inputs
     (list meson-python
           pkg-config
           python-pytest
           pythoncapi-compat
           scs))
    (inputs
     (list lapack
           openblas))
    (propagated-inputs
     (list python-numpy
           python-scipy))
    (home-page "https://github.com/bodono/scs-python")
    (synopsis "Splitting conic solver")
    (description "This package provides a Python interface for the
SCS (Splitting conic solver) library.")
    (license license:expat)))

(define-public python-pandas-flavor
  (package
    (name "python-pandas-flavor")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas_flavor" version))
       (sha256
        (base32
         "0rn3pnracv8013j3f737qal3isf1brbc3mpxqhr03vik322sapr5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                (("addopts = .*") "")))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-pandas
           python-xarray))
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
    (version "0.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/raphaelvallat/pingouin")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0i3yzdlj08di3mzi69ci57jm5myl123hp8c5vn1g35k77m4zpgvd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 85 passed, 3 deselected, 43 warnings
      #:test-flags
      ;; AssertionError: Arrays are not equal
      #~(list "--deselect=tests/test_pairwise.py::TestPairwise::test_pairwise_tests"
              ;; AssertionError: assert False
              "--deselect=tests/test_power.py::TestPower::test_power_ttest")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                ((" --cov") "")))))))
    (native-inputs
     (list python-pytest
           python-setuptools))
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

(define-public python-plotly
  (package
    (name "python-plotly")
    (version "5.24.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/plotly/plotly.py")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pnsj23bxj7c39hzdz49v72flwbc8knc7dy831lvc0hrbssm4j60"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 2715 passed, 18 skipped, 41 deselected, 606 warnings
      #:test-flags
      ;; XXX: Combination of missing packages and assertion errors.
      #~(list "--ignore=plotly/tests/test_optional/test_kaleido/test_kaleido.py"
              "-k" (string-join
                    (list "not test_build_df_from_vaex_and_polars"
                          "test_build_df_with_hover_data_from_vaex_and_polars"
                          "test_bytesio"
                          "test_colorscale_and_levels_same_length"
                          "test_correct_order_param"
                          "test_dependencies_not_imported"
                          "test_ensure_orca_ping_and_proc"
                          "test_external_server_url"
                          "test_fips_values_same_length"
                          "test_full_choropleth"
                          "test_invalid_figure_json"
                          "test_latex_fig_to_image[eps]"
                          "test_lazy_imports"
                          "test_legend_dots"
                          "test_linestyle"
                          "test_mimetype_combination"
                          "test_orca_executable_path"
                          "test_orca_version_number"
                          "test_pdf_renderer_show_override"
                          "test_png_renderer_mimetype"
                          "test_problematic_environment_variables[eps]"
                          "test_sanitize_json[auto]"
                          "test_sanitize_json[json]"
                          "test_sanitize_json[orjson]"
                          "test_scope_is_not_list"
                          "test_scraper"
                          "test_server_timeout_shutdown"
                          "test_simple_to_image[eps]"
                          "test_svg_renderer_show"
                          "test_to_image_default[eps]"
                          "test_topojson_fig_to_image[eps]"
                          "test_validate_orca"
                          "test_write_image_string[eps]"
                          "test_write_image_string_bad_extension_failure"
                          "test_write_image_string_bad_extension_override"
                          "test_write_image_string_format_inference[eps]"
                          "test_write_image_writeable[eps]")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'skip-npm
            ;; npm is not packaged so build without it
            (lambda _
              (setenv "SKIP_NPM" "T")))
          (add-after 'unpack 'fix-version
            ;; TODO: Versioneer in Guix gets its release version from the
            ;; parent directory, but the plotly package is located inside a
            ;; depth 3 subdirectory.  Try to use versioneer if possible.
            (lambda _
              (substitute* "packages/python/plotly/setup.py"
                (("version=versioneer.get_version\\(),")
                 (format #f "version=~s," #$version)))
              (substitute* "packages/python/plotly/plotly/version.py"
                (("__version__ = get_versions\\(\\)\\[\"version\"\\]")
                 (format #f "__version__ = ~s" #$version)))))
          (add-after 'fix-version 'chdir
            (lambda _
              (chdir "packages/python/plotly"))))))
    ;; XXX: Plotly requires a long list of test only packages, do not
    ;; propagate them, see: <packages/python/plotly/test_requirements>.
    (native-inputs
     (list python-geopandas
           python-ipykernel
           python-ipython-minimal
           python-ipywidgets
           python-matplotlib
           python-nbformat
           python-numpy-1
           python-orjson
           python-pandas
           python-pillow
           python-psutil
           python-pyshp
           python-pytest
           python-pytz
           python-requests
           python-retrying
           python-scikit-image
           python-scipy
           python-setuptools
           python-shapely
           python-statsmodels
           python-tenacity
           python-vaex-core
           python-xarray))
    (propagated-inputs
     (list python-packaging
           python-tenacity))
    (home-page "https://plotly.com/python/")
    (synopsis "Interactive plotting library for Python")
    (description
     "Plotly's Python graphing library makes interactive,publication-quality
graphs online.  Examples of how to make line plots, scatter plots, area
charts, bar charts, error bars, box plots, histograms, heatmaps, subplots,
multiple-axes, polar charts, and bubble charts.")
    (license license:expat)))

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
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pyvista/pyvista")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lnh4cvf6wld7hm293015d80ny0vnsk96ckfvc2crzd1b79ch1v5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 5891 passed, 623 skipped, 355 deselected, 3 xfailed, 167 warnings
      #:test-flags
      ;; TODO: Find out what's going on with skipped tests.
      #~(list "--ignore=tests/plotting/test_charts.py"
              "--ignore=tests/examples/test_download_files.py"
              "--ignore=tests/examples/test_downloads.py"
              "--ignore=tests/plotting/test_texture.py"
              "-k" (string-join
                    (list "not test_actor_texture"
                          "test_add_multiple"
                          "test_add_timer_event"
                          "test_avsucd_reader"
                          "test_binarymarchingcubesreader"
                          "test_bmpreader"
                          "test_box_axes"
                          "test_byureader"
                          "test_cast_to_numpy_raises"
                          "test_compute_boundary_mesh_quality"
                          "test_connectivity_"
                          "test_dataset_loader_cubemap"
                          "test_dataset_loader_dicom"
                          "test_dataset_loader_from_nested_files_and_directory"
                          "test_dataset_loader_from_nested_multiblock"
                          "test_dataset_loader_one_file"
                          "test_dataset_loader_two_files_both_loadable"
                          "test_dataset_loader_two_files_one_loadable"
                          "test_dcmreader"
                          "test_demreader"
                          "test_ensight_multi_block_io"
                          "test_ensightreader_arrays"
                          "test_ensightreader_time_sets"
                          "test_ensightreader_timepoints"
                          "test_facetreader"
                          "test_fluentcffreader"
                          "test_gambitreader"
                          "test_gaussian_cubes_reader"
                          "test_gesignareader"
                          "test_gif_reader"
                          "test_hdf_reader"
                          "test_hdr_reader"
                          "test_init_cmap"
                          "test_interpolate"
                          "test_jpegreader"
                          "test_legend_"
                          "test_load_dataset_no_reader"
                          "test_load_theme"
                          "test_meta_image_reader"
                          "test_multiblockplot3dreader"
                          "test_nifti_reader"
                          "test_nrrd_reader"
                          "test_objreader"
                          "test_only_screenshots_flag"
                          "test_openfoam_case_type"
                          "test_openfoam_cell_to_point_default"
                          "test_openfoam_patch_arrays"
                          "test_openfoam_skip_zero_time"
                          "test_openfoamreader_active_time"
                          "test_openfoamreader_arrays_time"
                          "test_openfoamreader_read_data_time_point"
                          "test_openfoamreader_read_data_time_value"
                          "test_particle_reader"
                          "test_partition"
                          "test_pdbreader"
                          "test_plot3dmetareader"
                          "test_plot_return_img_with_cpos"
                          "test_plot_return_img_without_cpos"
                          "test_png_reader"
                          "test_pnm_reader"
                          "test_prostar_reader"
                          "test_protein_ribbon"
                          "test_pvdreader"
                          "test_pvdreader_no_part_group"
                          "test_pvdreader_no_time_group"
                          "test_read_cgns"
                          "test_repr"
                          "test_save_before_close_callback"
                          "test_slc_reader"
                          "test_stlreader"
                          "test_tecplotreader"
                          "test_tiff_reader"
                          "test_timer"
                          "test_translate_direction_collinear"
                          "test_user_logo"
                          "test_xdmf_reader"
                          ;; XXX: incompatible with Numpy@2
                          ;; Drop when updating along with vtk.
                          "test_check_subdtype_changes_type")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          ;; vtk check fails in sanity-check, comment out
          (add-after 'unpack 'patch-pyproject
            (lambda _
              (substitute* "pyproject.toml"
                (("'vtk<9\\.4\\.0'," all) (string-append "#" all)))))
          (add-after 'unpack 'fix-failing-tests
            (lambda _
              (substitute* "tests/plotting/test_plotting.py"
                (("\"\"\"Determine if using mesa.\"\"\"" all)
                 (string-append all "\n    return False")))
              (substitute* "tests/test_meshio.py"
                (("cow = .*$" all) (string-append "#" all "\n"))
                ((", cow") ""))))
          ;; test phase writes files to $HOME
          (add-before 'check 'redirect-HOME
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-ipython
           python-pytest
           python-scipy
           python-tqdm
           python-trimesh))
    (propagated-inputs
     (list python-imageio
           python-matplotlib
           python-meshio
           python-numpy
           python-pillow
           python-pooch
           python-scooby
           vtk))
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

(define-public python-pyvistaqt
  (package
    (name "python-pyvistaqt")
    (version "0.11.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyvista/pyvistaqt")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04f9cd98k463pdrpi8jby411x9mc0ih62gl0nv0h9w3r7pwl61yl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest
            (lambda _
              (substitute* "tests/conftest.py"
                (("pytest.skip")
                 "pytest.mark.skipif"))))
          (add-before 'check 'before-check
            (lambda _
              ;; Testing requires write access.
              (setenv "HOME" "/tmp")
              ;; Testing requires a running xorg server.
              (system "Xvfb :99 -screen 0 1024x768x24 &")
              (setenv "DISPLAY" ":99.0"))))))
    (propagated-inputs (list python-pyvista python-qtpy))
    (native-inputs (list python-ipython
                         python-matplotlib
                         python-numpy
                         python-pyqt-6
                         python-pytest
                         python-pytest-cov
                         python-pytest-qt
                         python-setuptools
                         python-sphinx-gallery
                         xorg-server))
    (home-page "https://github.com/pyvista/pyvistaqt")
    (synopsis "Qt support for PyVista")
    (description
     "@code{pyvistaqt} is a helper module for @code{pyvista} to enable you to
plot using Qt by placing a vtk-widget into a background renderer.  This can be
quite useful when you desire to update your plot in real-time.")
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
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))                ;No tests, also not in git repository.
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-numpy python-scipy))
    (home-page "https://github.com/xmikos/simplespectral")
    (synopsis "FFT module for Python")
    (description
     "This package provides a simplified @code{scipy.signal.spectral} module
to do spectral analysis in Python.")
    (license license:expat)))

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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nschloe/pyfma")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12i68jj9n1qj9phjnj6f0kmfhlsd3fqjlk9p6d4gs008azw5m8yn"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy-1))
    (native-inputs (list pybind11 python-pytest python-setuptools))
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

(define-public python-supersmoother
  ;; 0.4 was release in 2017, there a lot of changes on master branch
  ;; providing tests fixtures.
  (let ((commit "0a81544ac6bb33bdb08deeba69e97a4ceebcebcf")
        (revision "0"))
    (package
      (name "python-supersmoother")
      (version (git-version "0.4" revision commit))
      (source
       (origin
         (method git-fetch)        ; no package in PyPI
         (uri (git-reference
                (url "https://github.com/jakevdp/supersmoother")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1r79nssw4a44zizvqg8y685nv3asdfj440s227phfww6kz33s3la"))))
      (build-system pyproject-build-system)
      (native-inputs
       (list python-pytest
             python-scipy
             python-setuptools))
      (propagated-inputs
       (list python-numpy))
      (home-page "http://github.com/jakevdp/supersmoother")
      (synopsis "Python implementation of Friedman's Supersmoother")
      (description
       "This package provides an efficient implementation of
@url{https://www.slac.stanford.edu/pubs/slacpubs/3250/slac-pub-3477.pdf,
Friedman's SuperSmoother} based in Python.  It makes use of numpy for fast
numerical computation.")
      (license license:bsd-2))))

(define-public python-pylems
  (package
    (name "python-pylems")
    (version "0.6.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LEMS/pylems")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gimdx89cdla1b6zzkdrmj979nn2zy2475qvpwxxas0iv27ql0vj"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Disable tests that require networking
     (list #:test-flags #~(list "./lems/test" "-k" "not test_load_write_xml")))
    (native-inputs (list python-setuptools python-pytest))
    (propagated-inputs (list python-lxml python-matplotlib))
    (home-page "https://github.com/LEMS/pylems")
    (synopsis
     "Python support for the Low Entropy Model Specification language (LEMS)")
    (description "A @acronym{LEMS, Low Entropy Model Specification} simulator
written in Python which can be used to run NeuroML2 models.")
    (license license:lgpl3)))

(define-public python-pynrrd
  (package
    (name "python-pynrrd")
    (version "1.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mhe/pynrrd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l0hjbqzf5i1bmpxpblpyyqkhci3mb5n07x6hqf2a91hggfyrvda"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-setuptools))
    (propagated-inputs
     (list python-numpy))
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

(define-public python-dvc-objects
  (package
    (name "python-dvc-objects")
    (version "5.1.1")
    (home-page "https://github.com/iterative/dvc-objects")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dvc_objects" version))
       (sha256
        (base32 "1amx5z8k2v2hbsajg0dcd5dxmmlv9bnbchpas95s8sj86cm8yc4y"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-fsspec
                             python-funcy-1.14))
    (native-inputs
     (list python-mypy
           python-pytest
           python-pytest-asyncio
           python-pytest-benchmark
           python-pytest-cov
           python-pytest-mock
           python-pytest-sugar
           python-reflink
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (synopsis "Filesystem and object-db level abstractions for DVC")
    (description "Dvc objects provides a filesystem and object-db level
abstractions to use in dvc and dvc-data.")
    (license license:asl2.0)))

(define-public python-dvc-data
  (package
    (name "python-dvc-data")
    (version "3.16.12")
    (home-page "https://github.com/iterative/dvc-data")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dvc_data" version))
              (sha256
               (base32
                "156iwdn7v5jhwbpwz92n28qiasgcbmcqv9vxg8xbvdfxzlzw0b7r"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-attrs
           python-dictdiffer
           python-diskcache
           python-dvc-objects
           python-fsspec
           python-funcy-1.14
           python-orjson
           python-pygtrie
           python-sqltrie
           python-tqdm))
    (native-inputs
     (list python-click
           python-pytest
           python-pytest-benchmark
           python-pytest-cov
           python-pytest-mock
           ;; python-pytest-servers is not packaged in Guix yet
           python-setuptools
           python-setuptools-scm
           python-typer
           python-wheel))
    (arguments
     (list
      #:test-flags
      ;; TODO: package python-pytest-server with its transitive dependencies
      #~(list "--ignore=tests/hashfile/test_db.py"
              "--ignore=tests/hashfile/test_db_index.py"
              "--ignore=tests/hashfile/test_obj.py"
              "--ignore=tests/index/test_build.py"
              "--ignore=tests/index/test_checkout.py"
              "--ignore=tests/index/test_fs.py"
              "--ignore=tests/index/test_index.py"
              "--ignore=tests/index/test_storage.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-pyproject
            (lambda _
              ;; setuptools cannot handle both license and license-files
              (substitute* "pyproject.toml"
                (("^license = .*") "license = {text = \"Apache-2.0\"}\n")
                (("^license-files = .*") "")))))))
    (synopsis "DVC's data management subsystem")
    (description "Dvc data is DVC's data management subsystem.")
    (license license:asl2.0)))

(define-public python-pyqtgraph
  (package
    (name "python-pyqtgraph")
    (version "0.13.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyqtgraph" version))
       (sha256
        (base32 "1qyr461hcvhgy02slfkgrbip2xwa8zz6dvmi1476v6f66lclzy34"))))
    (build-system pyproject-build-system)
    (arguments
     ;; tests: 949 passed, 1356 skipped, 2 deselected, 8 xfailed, 130 warnings
     (list #:test-flags
           ;; Failed: CALL ERROR: Exceptions caught in Qt event loop.
           #~(list "--deselect=tests/exporters/test_svg.py::test_plotscene"
                   ;; The test_reload test fails.  It suggests to disable
                   ;; assert rewriting in Pytest, but it still doesn't pass.
                   "-k" "not test_reload"
                   ;; Run unit tets only.
                   "tests")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-qpa
                 (lambda _
                   (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs
     (list python-pytest
           ;; Do not propagate Qt5/Qt6 let the user of the package to select
           ;; any supported one, see
           ;; <https://pyqtgraph.readthedocs.io/en/pyqtgraph-0.13.7>
           ;; </getting_started/how_to_use.html#pyqt-and-pyside>.
           python-pyqt-6
           python-pytest-qt
           python-setuptools))
    (propagated-inputs
     (list python-h5py
           python-numpy
           python-pyopengl
           python-scipy))
    (home-page "https://www.pyqtgraph.org")
    (synopsis "Scientific graphics and GUI library for Python")
    (description
     "PyQtGraph is a Pure-python graphics library for PyQt5, PyQt6, PySide2
and PySide6.  It is intended for use in mathematics, scientific or engineering
applications.")
    (license license:expat)))

(define-public pyzo
  (package
    (name "pyzo")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyzo/pyzo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a82k7hjmv20lrwiwsdrvczrm21wq16m4snwsirwhj0jh5k1x9iw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--ignore-glob=pyzo/yoton/tests/*"     ; XXX: yoton is outdated.
              "--ignore=pyzo/codeeditor/_test.py"))) ; XXX: cannot import qt.
    (native-inputs
     (list python-flit-core
           python-pytest
           python-setuptools))
    (inputs (list python-pyside-6))
    (home-page "https://pyzo.org")
    (synopsis "Python IDE for scientific computing")
    (description
     "Pyzo is a Python IDE focused on interactivity and introspection,which
makes it very suitable for scientific computing.  Its practical design is
aimed at simplicity and efficiency.

It consists of two main components, the editor and the shell, and uses a set
of pluggable tools to help the programmer in various ways.  Some example tools
are source structure, project manager, interactive help, workspace...")
    (license license:bsd-2)))

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

(define-public snakemake-5
  (package
    (name "snakemake")
    (version "5.32.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snakemake/snakemake")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nxp4z81vykv07kv2b6zrwk7ns8s10zqsb7vcignp8695yq3nlcm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list
        ;; We have no TES support.
        "--ignore=tests/test_tes.py"
        ;; This test attempts to change S3 buckets on AWS and fails
        ;; because there are no AWS credentials.
        "--ignore=tests/test_tibanna.py"
        ;; It's a similar story with this test, which requires access
        ;; to the Google Storage service.
        "--ignore=tests/test_google_lifesciences.py"
        ;; Unclear failure.
        "-k" "not test_lint[long_run-positive]")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'tabulate-compatibility
            (lambda _
              (substitute* "snakemake/dag.py"
                (("\"job\": rule,")
                 "\"job\": rule.name,"))))
          (add-after 'unpack 'patch-version
            (lambda _
              (substitute* "setup.py"
                (("version=versioneer.get_version\\(\\)")
                 (format #f "version=~s" #$version)))
              (substitute* '("snakemake/_version.py"
                             "versioneer.py")
                (("0\\+unknown") #$version))))
          ;; For cluster execution Snakemake will call Python.  Since there is
          ;; no suitable PYTHONPATH set, cluster execution will fail.  We fix
          ;; this by calling the snakemake wrapper instead.
          (add-after 'unpack 'call-wrapper-not-wrapped-snakemake
            (lambda _
              (substitute* "snakemake/executors/__init__.py"
                (("\\{sys.executable\\} -m snakemake")
                 (string-append #$output "/bin/snakemake")))))
          (add-before 'check 'pre-check
            (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-appdirs
           python-configargparse
           python-datrie
           python-docutils
           python-gitpython
           python-jinja2
           python-jsonschema
           python-nbformat
           python-networkx
           python-psutil
           python-pulp
           python-pyyaml
           python-ratelimiter
           python-requests
           python-toposort
           python-wrapt))
    (native-inputs
     (list git-minimal
           python-wrapper
           python-pytest
           python-pandas
           python-requests-mock
           python-setuptools
           python-wheel))
    (home-page "https://snakemake.readthedocs.io")
    (synopsis "Python-based execution environment for make-like workflows")
    (description
      "Snakemake aims to reduce the complexity of creating workflows by
providing a clean and modern domain specific specification language (DSL) in
Python style, together with a fast and comfortable execution environment.")
    (license license:expat)))

(define-public snakemake-6
  (package
    (inherit snakemake-5)
    (name "snakemake")
    (version "6.15.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snakemake/snakemake")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09yrpi9f86r9yvcm2dfjs5zy87c4j31bxama77kfd6y8yfrrjlai"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list
        ;; This test attempts to change S3 buckets on AWS and fails
        ;; because there are no AWS credentials.
        "--ignore=tests/test_tibanna.py"
        ;; E   ModuleNotFoundError: No module named 'google'
        "--ignore=tests/test_google_lifesciences.py"
        ;; Unclear failure.
        "-k" "not test_lint[long_run-positive]")
      #:phases
      #~(modify-phases %standard-phases
          ;; For cluster execution Snakemake will call Python.  Since there is
          ;; no suitable GUIX_PYTHONPATH set, cluster execution will fail.  We
          ;; fix this by calling the snakemake wrapper instead.

          ;; XXX: There is another instance of sys.executable on line 692, but
          ;; it is not clear how to patch it.
          (add-after 'unpack 'call-wrapper-not-wrapped-snakemake
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "snakemake/executors/__init__.py"
                (("\\{sys.executable\\} -m snakemake")
                 (string-append #$output "/bin/snakemake")))))
          (add-after 'unpack 'tabulate-compatibility
            (lambda _
              (substitute* "snakemake/dag.py"
                (("\"job\": rule,")
                 "\"job\": rule.name,"))))
          (add-after 'unpack 'patch-version
            (lambda _
              (substitute* "setup.py"
                (("version=versioneer.get_version\\(\\)")
                 (format #f "version=~s" #$version)))
              (substitute* '("snakemake/_version.py"
                             "versioneer.py")
                (("0\\+unknown") #$version))))
          (add-before 'check 'pre-check
            (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-appdirs
           python-configargparse
           python-connection-pool
           python-datrie
           python-docutils
           python-filelock
           python-gitpython
           python-jinja2
           python-jsonschema
           python-nbformat
           python-networkx
           python-psutil
           python-pulp
           python-pyyaml
           python-py-tes
           python-ratelimiter
           python-requests
           python-smart-open
           python-stopit
           python-tabulate
           python-toposort
           python-wrapt))
    (native-inputs
     (list git-minimal
           python-wrapper
           python-pytest
           python-pandas
           python-requests-mock
           python-setuptools
           python-wheel))))

(define-public snakemake-7
  (package
    (inherit snakemake-6)
    (name "snakemake")
    (version "7.32.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snakemake/snakemake")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d5hizai89k1glfqfkvf1ghj0l7wm8il6gl5pfwk2gkza87yka6d"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This test attempts to change S3 buckets on AWS and fails
      ;; because there are no AWS credentials.
      '(list "--ignore=tests/test_tibanna.py"
             ;; It's a similar story with this test, which requires access to
             ;; the Google Storage service.
             "--ignore=tests/test_google_lifesciences.py"
             "--ignore-glob=tests/test_conda_python_3_7_script/*"
             ;; We don't have a slurm installation in the build environment
             "--ignore=tests/test_slurm.py")
      #:phases
      #~(modify-phases %standard-phases
          ;; For cluster execution Snakemake will call Python.  Since there is
          ;; no suitable GUIX_PYTHONPATH set, cluster execution will fail.  We
          ;; fix this by calling the snakemake wrapper instead.
          (add-after 'unpack 'call-wrapper-not-wrapped-snakemake
            (lambda* (#:key outputs #:allow-other-keys)
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
            (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-appdirs
           python-configargparse
           python-connection-pool
           python-datrie
           python-docutils
           python-filelock
           python-gitpython
           python-humanfriendly
           python-jinja2
           python-jsonschema
           python-nbformat
           python-networkx
           python-psutil
           python-pulp
           python-pyyaml
           python-py-tes
           python-requests
           python-retry
           python-reretry
           python-smart-open
           python-stopit
           python-tabulate
           python-throttler
           python-toposort
           python-wrapt
           python-yte))
    (native-inputs
     (list git-minimal
           python-wrapper
           python-pytest
           python-pandas
           python-requests-mock
           python-setuptools
           python-wheel))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
