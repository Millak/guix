;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2020, 2021, 2022, 2023, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020, 2021, 2022, 2023, 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021, 2023 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023, 2024 Troy Figiel <troy@troyfigiel.com>
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
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages simulation)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject))

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
     (list python-jupytext
           python-matplotlib
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
           python-typing-extensions))
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
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit-allel" version))
       (sha256
        (base32 "1vg88ng6gd175gzk39iz1drxig5l91dyx398w2kbw3w8036zv8gj"))))
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
     (list python-dask
           python-numpy))
    (native-inputs
     (list python-cython
           ;; The following are all needed for the tests
           htslib
           python-h5py
           python-hmmlearn
           python-numexpr
           python-pytest
           python-scipy
           python-setuptools-scm
           python-zarr))
    (home-page "https://github.com/cggh/scikit-allel")
    (synopsis "Explore and analyze genetic variation data")
    (description
     "This package provides utilities for exploratory analysis of large scale
genetic variation data.")
    (license license:expat)))

(define-public python-scikit-fem
  (package
    (name "python-scikit-fem")
    (version "9.0.1")
    (source (origin
              (method git-fetch)        ; no tests in PyPI
              (uri (git-reference
                    (url "https://github.com/kinnala/scikit-fem")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r1c88rbaa7vjfnljbzx8paf36yzpy33bragl99ykn6i2srmjrd4"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-meshio python-numpy python-scipy))
    (native-inputs
     (list python-autograd
           python-pyamg
           python-pytest
           python-shapely))
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
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit-fuzzy" version))
       (sha256
        (base32 "0bp1n771fj44kdp7a00bcvfwirvv2rc803b7g6yf3va7v0j29c8s"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))   ;XXX: not compatible with newer numpy.testing
    (native-inputs
     (list python-nose))
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
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scikit-image/scikit-image")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10fzyq2w1ldvfkmj374l375yrx33xrlw39xc9kmk8fxfi77jpykd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
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

(define-public python-scikit-optimize
  (package
    (name "python-scikit-optimize")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/scikit-optimize/scikit-optimize")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hsq6pmryimxc275yrcy4bv217bx7ma6rz0q6m4138bv4zgq18d1"))
              (patches
               ;; These are for compatibility with more recent versions of
               ;; numpy and scikit-learn.
               (search-patches "python-scikit-optimize-1148.patch"
                               "python-scikit-optimize-1150.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; Since scikit-learn 1.3 max_features no longer supports
               ;; 'auto', which is identical to 'sqrt'
               '(substitute* '("skopt/learning/forest.py"
                               "skopt/learning/tests/test_forest.py")
                  (("max_features=['\"]auto['\"]")
                   "max_features='sqrt'")))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-joblib
           python-matplotlib
           python-numpy
           python-pyaml
           python-scikit-learn
           python-scipy))
    (native-inputs
     (list python-pytest))
    (home-page "https://scikit-optimize.github.io/")
    (synopsis "Sequential model-based optimization toolbox")
    (description "Scikit-Optimize, or @code{skopt}, is a simple and efficient
library to minimize (very) expensive and noisy black-box functions.  It
implements several methods for sequential model-based optimization.
@code{skopt} aims to be accessible and easy to use in many contexts.")
    (license license:bsd-3)))

(define-public python-tdda
  (package
    (name "python-tdda")
    (version "2.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tdda" version))
       (sha256
        (base32 "1xs91s8b7cshjcqw88qsrjh10xly799k5rf2ycawqfz2mw8sy3br"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "tdda" "test")))))))
    (native-inputs (list python-numpy python-pandas))
    (home-page "https://www.stochasticsolutions.com")
    (synopsis "Test-driven data analysis library for Python")
    (description
     "The TDDA Python module provides command-line and Python API support
for the overall process of data analysis, through tools that peform
reference testing, constraint discovery for data, automatic inference
of regular expressions from text data and automatic test generation.")
    (license license:expat))) ; MIT License

(define-public python-trimesh
  (package
    (name "python-trimesh")
    (version "3.23.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trimesh" version))
       (sha256
        (base32 "08967axlnmfv98n05dhrkynyrmcc814hl8184gzzmcy4rjg6dzdx"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy))
    (native-inputs
     (list python-coveralls
           python-pyinstrument
           python-pytest
           python-pytest-cov))
    (arguments
     `(;; TODO: Get tests to work.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build
           (lambda _
             (substitute* "trimesh/resources/templates/blender_boolean.py.tmpl"
               (("\\$MESH_PRE")
                "'$MESH_PRE'")))))))
    (home-page "https://github.com/mikedh/trimesh")
    (synopsis "Python library for loading and using triangular meshes")
    (description
     "Trimesh is a pure Python library for loading and using triangular meshes
with an emphasis on watertight surfaces.  The goal of the library is to provide
a full featured and well tested Trimesh object which allows for easy
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

(define-public python-pyamg
  (package
    (name "python-pyamg")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyamg" version))
              (sha256
               (base32
                "0l3dliwynxyjvbgpmi2k8jqvkkw6fc00c8w69h6swhrkfh0ql12z"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Test installed package in order to find C++ modules.
      #~(list "--pyargs" "pyamg.tests")))
    (native-inputs (list pybind11 python-pytest python-setuptools-scm))
    (propagated-inputs (list python-numpy python-scipy))
    (home-page "https://github.com/pyamg/pyamg")
    (synopsis "Algebraic Multigrid Solvers in Python")
    (description "PyAMG is a Python library of Algebraic Multigrid
(AMG) solvers.

PyAMG features implementations of:
@itemize
@item Ruge-Stuben (RS) or Classical AMG
@item AMG based on Smoothed Aggregation (SA)
@item Adaptive Smoothed Aggregation (αSA)
@item Compatible Relaxation (CR)
@item Krylov methods such as CG, GMRES, FGMRES, BiCGStab, MINRES, etc.
@end itemize
PyAMG is primarily written in Python with supporting C++ code for
performance critical operations.")
    (license license:expat)))

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

(define-public python-pandas
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

(define-public python-pandas-stubs
  (package
    (name "python-pandas-stubs")
    ;; The versioning follows that of Pandas and uses the date of the
    ;; python-pandas-stubs release. This is the latest version of
    ;; python-pandas-stubs for python-pandas 1.5.3.
    (version "1.5.3.230321")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pandas-dev/pandas-stubs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1blwlq5053pxnmx721zdd6v8njiybz4azribx2ygq33jcpmknda6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-k"
                           (string-append
                            ;; The python-pyarrow package in Guix is not built
                            ;; with ORC integration, causing these tests to
                            ;; fail.
                            "not test_orc"
                            " and not test_orc_path"
                            " and not test_orc_buffer"
                            " and not test_orc_columns"
                            " and not test_orc_bytes"))
      #:phases '(modify-phases %standard-phases
                  (add-before 'check 'prepare-x
                    (lambda _
                      (system "Xvfb &")
                      (setenv "DISPLAY" ":0")
                      ;; xsel needs to write a log file.
                      (setenv "HOME"
                              (getcwd)))))))
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
                         python-sqlalchemy
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

(define-public python-pandera
  (package
    (name "python-pandera")
    (version "0.17.2")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unionai-oss/pandera")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mnqk583z90k1n0z3lfa4rd0ng40v7hqfk7phz5gjmxlzfjbxa1x"))
       (modules '((guix build utils)))
       ;; These tests require PySpark and Modin. We need to remove the entire
       ;; directory, since the conftest.py in these directories contain
       ;; imports.  (See: https://github.com/pytest-dev/pytest/issues/7452)
       (snippet '(begin
                   (delete-file-recursively "tests/pyspark")
                   (delete-file-recursively "tests/modin")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "-k"
                          (string-append
                           ;; Mypy functionality is experimental and relying
                           ;; on pandas-stubs can lead to false
                           ;; positives. These tests currently fail.
                           "not test_python_std_list_dict_generics"
                           " and not test_python_std_list_dict_empty_and_none"
                           " and not test_pandas_modules_importable"))))
    ;; Pandera comes with a lot of extras. We test as many as possible, but do
    ;; not include all of them in the propagated-inputs. Currently, we have to
    ;; skip the pyspark and io tests due to missing packages python-pyspark
    ;; and python-frictionless.
    (propagated-inputs (list python-hypothesis ;strategies extra
                             python-multimethod
                             python-numpy
                             python-packaging
                             python-pandas
                             python-pandas-stubs ;mypy extra
                             python-pydantic
                             python-scipy ;hypotheses extra
                             python-typeguard-4
                             python-typing-inspect
                             python-wrapt))
    (native-inputs (list python-dask ;dask extra
                         python-fastapi ;fastapi extra
                         python-geopandas ;geopandas extra
                         python-pyarrow ;needed to run fastapi tests
                         python-pytest
                         python-pytest-asyncio
                         python-sphinx
                         python-uvicorn)) ;needed to run fastapi tests
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

(define-public python-pythran
  (package
    (name "python-pythran")
    (version "0.11.0")
    (home-page "https://github.com/serge-sans-paille/pythran")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0cm7wfcyvkp1wmq7n1lyf2d3sj6158jf63bagjpjmfnjwij19n0p"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled Boost and xsimd.
                  (delete-file-recursively "third_party")))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'do-not-install-third-parties
                 (lambda _
                   (substitute* "setup.py"
                     (("third_parties = .*")
                      "third_parties = []\n"))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; Remove compiler flag that trips newer GCC:
                     ;; https://github.com/serge-sans-paille/pythran/issues/908
                     (substitute* "pythran/tests/__init__.py"
                       (("'-Wno-absolute-value',")
                        ""))
                     (setenv "HOME" (getcwd))
                     ;; This setup is modelled after the upstream CI system.
                     (call-with-output-file ".pythranrc"
                       (lambda (port)
                         (format port "[compiler]\nblas=openblas~%")))
                     (invoke "pytest" "-vv"
                             (string-append "--numprocesses="
                                            (number->string
                                             (parallel-job-count)))
                             "pythran/tests/test_cases.py")))))))
    (native-inputs
     ;; For tests.
     (list openblas python-pytest python-pytest-xdist))
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
     (list python-pytest python-pytest-cov))
    (home-page "https://github.com/johannfaouzi/pyts")
    (synopsis "Python package for time series classification")
    (description
     "This package provides a Python package for time series classification.")
    (license license:bsd-3)))

(define-public python-bottleneck
  (package
    (name "python-bottleneck")
    (version "1.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Bottleneck" version))
       (sha256
        (base32 "1y410r3scfhs6s1j1jpxig01qlyn2hr2izyh1qsdlsfl78vpwip1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "setup.py" "pytest")))))))
    (native-inputs
     (list python-hypothesis python-pytest python-pytest-runner))
    (propagated-inputs
     (list python-numpy))
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
    (native-inputs (list python-pytest python-sympy))
    (home-page "https://numpoly.readthedocs.io/en/master/")
    (synopsis "Polynomials as a numpy datatype")
    (description "Numpoly is a generic library for creating, manipulating and
evaluating arrays of polynomials based on @code{numpy.ndarray objects}.")
    ;; Tests fail with dtype mismatches on 32-bit architectures, suggesting
    ;; that numpoly only supports 64 bit platforms.
    (supported-systems '("x86_64-linux" "aarch64-linux" "powerpc64le-linux"))
    (license license:bsd-2)))

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
     ;; This needs a more recent version of python-hypothesis
     (list #:test-flags '(list "--ignore=xarray/tests/test_strategies.py")))
    (native-inputs
     (list python-setuptools-scm python-pytest))
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

(define-public python-pytensor
  (package
    (name "python-pytensor")
    (version "2.18.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pymc-devs/pytensor")
                    (commit (string-append "rel-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qa0y13xfm6w7ry7gp0lv84c8blyg34a9ns7ynwqyhf9majq08s5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Replace version manually because pytensor uses
          ;; versioneer, which requires git metadata.
          (add-after 'unpack 'versioneer
            (lambda _
              (with-output-to-file "setup.cfg"
                (lambda ()
                  (display "\
[versioneer]
VCS = git
style = pep440
versionfile_source = pytensor/_version.py
versionfile_build = pytensor/_version.py
tag_prefix =
parentdir_prefix = pytensor-
")))
              (invoke "versioneer" "install")
              (substitute* "setup.py"
                (("versioneer.get_version\\(\\)")
                 (string-append "\"" #$version "\"")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp") ; required for most tests
                ;; Test discovery fails, have to call pytest by hand.
                ;; test_tensor_basic.py file requires JAX.
                (invoke "python" "-m" "pytest" "-vv"
                        "--ignore" "tests/link/jax/test_tensor_basic.py"
                        ;; Skip benchmark tests.
                        "-k" (string-append
                              "not test_elemwise_speed"
                              " and not test_logsumexp_benchmark"
                              " and not test_fused_elemwise_benchmark"
                              " and not test_scan_multiple_output"
                              " and not test_vector_taps_benchmark"
                              " and not test_cython_performance")
                        ;; Skip computationally intensive tests.
                        "--ignore" "tests/scan/"
                        "--ignore" "tests/tensor/"
                        "--ignore" "tests/sandbox/"
                        "--ignore" "tests/sparse/sandbox/")))))))
    (native-inputs (list python-cython
                         python-pytest
                         python-pytest-mock
                         python-versioneer))
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

(define-public python-upsetplot
  (package
    (name "python-upsetplot")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "UpSetPlot" version))
       (sha256
        (base32
         "11zrykwnb00w5spx4mnsnm0f9gwrphdczainpmwkyyi50vipaa2l"))
       (modules '((guix build utils)))
       (snippet
        ;; Patch for compatibility with newer setuptools:
        ;; https://github.com/jnothman/UpSetPlot/pull/178
        '(substitute* "upsetplot/data.py"
           (("import distutils")
            "from distutils.version import LooseVersion")
           (("if distutils\\.version\\.LooseVersion")
            "if LooseVersion")))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-v" "--doctest-modules")))))))
    (propagated-inputs
     (list python-matplotlib python-pandas))
    (native-inputs
     (list python-pytest-runner python-pytest-cov))
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
    (version "2022.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marcomusy/vedo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hhv4xc4bphhd1zrnf7r6fpf65xvkdqmb1lh51qg1xpv91h2az0h"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           ;; These tests require online data.
           (lambda _
             (substitute* "tests/common/test_actors.py"
               (("^st = .*") "")
               (("^assert isinstance\\(st\\.GetTexture\\(\\), .*") ""))
             (delete-file "tests/common/test_pyplot.py")))
         (add-after 'build 'mpi-setup
           ,%openmpi-setup)
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (setenv "HOME" (getcwd))
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "tests"
                 (for-each (lambda (dir)
                             (with-directory-excursion dir
                               (invoke "./run_all.sh")))
                           '("common" "dolfin"))))))
         ;; Disable the sanity check, which fails with the following error:
         ;;
         ;;   ...checking requirements: ERROR: vedo==2022.2.0 DistributionNotFound(Requirement.parse('vtk<9.1.0'), {'vedo'})
         (delete 'sanity-check))))
    (native-inputs
     (list pkg-config
           python-pkgconfig))
    (propagated-inputs
     (list fenics
           python-deprecated
           python-matplotlib
           python-numpy
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
    (version "0.5.2")
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
         "0czy7cpn6xx9fs6wbz6rq2lpkb1a89bzxj1anf2f9in1m5qyrh83"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
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

(define-public python-pyglm
  (package
    (name "python-pyglm")
    (version "2.5.7")
    (source
     (origin
       ;; Test files are not included in the archive in pypi.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Zuzu-Typ/PyGLM")
             (commit version)
             ;; Checkout the bundled `glm` submodule.  PyGLM uses the
             ;; currently unreleased GLM_EXT_matrix_integer feature.  Can
             ;; maybe unbundle once glm@0.9.9.9 is released.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08v0cgkwsf8rxscx5g9c5p1dy38rvak2fy3q6hg985if1nj6d9ks"))))
    (build-system python-build-system)
    (home-page "https://github.com/Zuzu-Typ/PyGLM")
    (synopsis "OpenGL Mathematics library for Python")
    (description "PyGLM is a Python extension library which brings the OpenGL
Mathematics (GLM) library to Python.")
    (license license:zlib)))

(define-public python-distributed
  (package
    (name "python-distributed")
    (version "2023.7.0")
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
         "0b93fpwz7kw31pkzfyihpkw8mzbqshzd6rw5vcwld7n3z2aaaxxb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-x" "-m"
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
                "test_mixing_clients_different_scheduler"
                "test_multiple_listeners"
                "test_no_dangling_asyncio_tasks"
                "test_plugin_exception"
                "test_plugin_internal_exception"
                "test_plugin_multiple_exceptions"
                "test_ports"
                "test_preload_import_time"
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
                "test_nanny_timeout") ; access to 127.0.0.1
               " and not ")

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
              " and not test_steal_twice"))
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
     (list python-importlib-metadata
           python-pytest
           python-pytest-timeout
           python-flaky
           python-versioneer))
    (home-page "https://distributed.dask.org")
    (synopsis "Distributed scheduler for Dask")
    (description "Dask.distributed is a lightweight library for distributed
computing in Python.  It extends both the @code{concurrent.futures} and
@code{dask} APIs to moderate sized clusters.")
    (license license:bsd-3)))

(define-public python-modin
  (package
    (name "python-modin")
    (version "0.15.1")
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
         "0nf2pdqna2vn7vq7q7b51f3cfbrxfn77pyif3clibjsxzvfm9k03"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))))
         (add-after 'unpack 'loosen-requirements
           (lambda _
             (substitute* "setup.py"
               ;; Don't depend on a specific version of Pandas.
               (("pandas==")
                "pandas>="))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "MODIN_ENGINE" "dask")
               (invoke "python" "-m" "pytest"
                       "modin/pandas/test/test_concat.py")
               (setenv "MODIN_ENGINE" "python")
               (invoke "python" "-m" "pytest"
                       "modin/pandas/test/test_concat.py")))))))
    (propagated-inputs
     (list python-cloudpickle
           python-dask
           python-distributed
           python-numpy
           python-packaging
           python-pandas))
    (native-inputs
     (list python-coverage
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
           python-xlrd))
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
    (version "0.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/has2k1/plotnine")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lg53wcm00lj8zbb4q9yj4a0n0fqaqq7c7vj18bda0k56gg0fpwl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; XXX: Check for any new failing tests during next update cycle.
      ;; These all fail because the images are considered to be too different,
      ;; though they really do look fine.
      '(list "-k"
             (string-append "not TestThemes"
                            (string-join (list
                                          ;; Image tests
                                          "test_adjust_text"
                                          "test_annotation_logticks_coord_flip_discrete"
                                          "test_annotation_logticks_faceting"
                                          "test_arrow"
                                          "test_aslabeller_dict_0tag"
                                          "test_caption_simple"
                                          "test_continuous_x"
                                          "test_continuous_x_fullrange"
                                          "test_coord_trans_backtransforms"
                                          "test_coord_trans_se_false"
                                          "test_custom_shape"
                                          "test_datetime_scale_limits"
                                          "test_dir_v_ncol"
                                          "test_discrete_x"
                                          "test_discrete_x_fullrange"
                                          "test_facet_grid_drop_false"
                                          "test_facet_grid_expression"
                                          "test_facet_grid_space_ratios"
                                          "test_facet_wrap"
                                          "test_facet_wrap_expression"
                                          "test_facet_wrap_label_both"
                                          "test_label_context_wrap2vars"
                                          "test_labeller_cols_both_grid"
                                          "test_labeller_cols_both_wrap"
                                          "test_labeller_towords"
                                          "test_missing_data_discrete_scale"
                                          "test_ribbon_facetting"
                                          "test_stack_non_linear_scale"
                                          "test_uneven_num_of_lines"

                                          ;; Missing optional modules
                                          "test_non_linear_smooth"
                                          "test_non_linear_smooth_no_ci")
                                         " and not "
                                         'prefix)))
      #:phases '(modify-phases %standard-phases
                  (add-before 'check 'pre-check
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; The data files are referenced by the tests but they are not
                      ;; installed.
                      (copy-recursively "plotnine/data"
                                        (string-append (site-packages inputs
                                                                      outputs)
                                                       "/plotnine/data"))
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
    (version "0.42.3")
    (source
     ;; The PyPI tarball does not contain the tests.
     ;; (However, we don't yet actually run the tests.)
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyvista/pyvista")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qxq0y0hc72hb60w3qq48fma8l6ffz7bdm75ymn1020bvfqrm1s4"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-imageio
           python-matplotlib
           python-meshio
           python-numpy
           python-pillow
           python-pooch
           python-scooby
           vtk))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Disable tests for now because they require several modules
         ;; currently unpackaged in Guix.
         (delete 'check)
         ;; Disable the sanity check, which fails with the following error:
         ;;
         ;;   ...checking requirements: ERROR: pyvista==0.42.3 DistributionNotFound(Requirement.parse('vtk'), {'pyvista'})
         (delete 'sanity-check))))
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
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; This one test fails because it doesn't raise an expected
               ;; exception.
               (invoke "pytest" "-vv" "-k" "not test_bad_values")))))))
    (propagated-inputs (list python-traitlets))
    (native-inputs
     (list python-numpy
           python-pandas
           python-nose
           python-pytest
           python-xarray))
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

(define-public python-paramz
  (package
    (name "python-paramz")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "paramz" version))
              (sha256
               (base32
                "16hbh97kj6b1c2gw22rqnr3w3nqkszh9gj8vgx738gq81wf225q9"))))
    (build-system python-build-system)
    (propagated-inputs (list python-decorator python-numpy python-scipy
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

(define-public python-gpy
  (package
    (name "python-gpy")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "GPy" version))
              (sha256
               (base32
                "1yx65ajrmqp02ykclhlb0n8s3bx5r0xj075swwwigiqaippr7dx2"))
             (snippet
              #~(begin (use-modules (guix build utils))
                       (substitute* "GPy/models/state_space_main.py"
                         (("collections\\.Iterable") "collections.abc.Iterable"))))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'remove-plotting-tests
                    ;; These fail
                    (lambda _
                      (delete-file "GPy/testing/plotting_tests.py"))))))
    (native-inputs (list python-cython python-nose python-climin))
    (propagated-inputs (list python-numpy python-paramz python-scipy
                             python-six))
    (home-page "https://sheffieldml.github.io/GPy/")
    (synopsis "The Gaussian Process Toolbox")
    (description
     "@command{GPy} is a Gaussian Process (GP) framework written in
Python, from the Sheffield machine learning group.  GPy implements a range of
machine learning algorithms based on GPs.")
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
    (native-inputs (list pybind11 python-pytest))
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
    (native-inputs (list python-pytest))
    (inputs (list gdcm libjpeg-turbo))
    (propagated-inputs (list python-numpy python-pillow))
    (home-page "https://github.com/pydicom/pydicom")
    (synopsis "Python library for reading and writing DICOM data")
    (description "@code{python-pydicom} is a Python library for reading and
writing DICOM medical imaging data.  It can read, modify and write DICOM
data.")
    (license license:expat)))

(define-public python-deepdish
  (package
    (name "python-deepdish")
    (version "0.3.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "deepdish" version))
              (sha256
               (base32
                "1wqzwh3y0mjdyba5kfbvlamn561d3afz50zi712c7klkysz3mzva"))))
    (arguments
     ;; XXX: The project may no longer be compatible with the version of
     ;; numpy packed in Guix.
     ;; See: https://github.com/uchicago-cs/deepdish/issues/50.
     ;;
     ;; However, there is a maintained fork that appears to be a good
     ;; replacement: https://github.com/portugueslab/flammkuchen.
     ;;
     ;; Disable few failing tests to pass the build.
     (list #:test-flags
           #~(list "-k" (string-append "not test_pad"
                                       " and not test_pad_repeat_border"
                                       " and not test_pad_repeat_border_corner"
                                       " and not test_pad_to_size"))
           #:phases #~(modify-phases %standard-phases
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
    (native-inputs (list python-pandas))
    (propagated-inputs (list python-numpy python-scipy python-six
                             python-tables))
    (home-page "https://github.com/uchicago-cs/deepdish")
    (synopsis "Python library for HDF5 file saving and loading")
    (description
     "Deepdish is a Python library to load and save HDF5 files.
The primary feature of deepdish is its ability to save and load all kinds of
data as HDF5.  It can save any Python data structure, offering the same ease
of use as pickling or @code{numpy.save}, but with the language
interoperability offered by HDF5.")
    (license license:bsd-3)))

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
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv")))))))
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-pytest python-pytest-cov python-pytest-pep8))
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
    (version "4.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vaex-core" version))
       (sha256
        (base32 "0ni862x5njhfsldjy49xmasd34plrs7yrmkyss6z1b6sgkbw9fsb"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries
        '(for-each delete-file-recursively
                   (list "vendor/boost"
                         "vendor/pcre"
                         "vendor/pybind11")))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false ;require vaex.server and others, which require vaex-core.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" )))))))
    (inputs
     (list boost pcre pybind11-2.3))
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
           python-pydantic
           python-pyyaml
           python-requests
           python-rich
           python-six
           python-tabulate))
    (native-inputs
     (list python-pytest python-cython))
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
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pynetdicom" version))
              (sha256
               (base32
                "0farmgviaarb3f4xn751card3v0lza57vwgl5azxxq65p7li44i3"))))
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
    (native-inputs (list python-pyfakefs python-pytest))
    (propagated-inputs (list python-pydicom python-sqlalchemy))
    (home-page "https://github.com/pydicom/pynetdicom")
    (synopsis "Python implementation of the DICOM networking protocol")
    (description
     "@code{pynetdicom} is a Python package that implements the DICOM
networking protocol.  It allows the easy creation of DICOM
@acronym{SCUs,Service Class Users} and @acronym{SCPs,Service Class
Providers}.")
    (license license:expat)))

(define-public python-libneuroml
  (package
    (name "python-libneuroml")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NeuralEnsemble/libNeuroML.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mrm4rd6x1sm6hkvhk20mkqp9q53sl3lbvq6hqzyymkw1iqq6bhy"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-lxml python-six))
    (native-inputs (list python-pytest python-numpy python-tables))
    (home-page "https://libneuroml.readthedocs.org/en/latest/")
    (synopsis
     "Python library for working with NeuroML descriptions of neuronal models")
    (description
     "This package provides a Python library for working with NeuroML descriptions of
neuronal models")
    (license license:bsd-3)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
