;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020, 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2022 Eric Bavier <bavier@posteo.net>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
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
  #:use-module (guix build-system python))

(define-public python-scipy
  (package
    (name "python-scipy")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scipy" version))
       (sha256
        (base32 "1gghkwn93niyasm36333xbqrnn3yiadq9d97wnc9mg14nzbg5m1i"))))
    (outputs '("out" "doc"))
    (build-system python-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build python-build-system)
                  (ice-9 format))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-pythran
            (lambda _
              (setenv "SCIPY_USE_PYTHRAN" "0")))
          (add-before 'build 'change-home-dir
            (lambda _
              ;; Change from /homeless-shelter to /tmp for write permission.
              (setenv "HOME" "/tmp")))
          (add-before 'build 'configure-openblas
            (lambda _
              (call-with-output-file "site.cfg"
                (lambda (port)
                  (format port
                          "\
[blas]
libraries = openblas
library_dirs = ~a/lib
include_dirs = ~:*~a/include

[atlas]
library_dirs = ~:*~a/lib
atlas_libs = openblas~%"  #$(this-package-input "openblas"))))))
          (add-before 'build 'parallelize-build
            (lambda _
              (setenv "NPY_NUM_BUILD_JOBS"
                      (number->string (parallel-job-count)))))
          (add-before 'check 'install-doc
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                     (doc (string-append data "/doc/" #$name "-" #$version))
                     (html (string-append doc "/html")))
                (with-directory-excursion "doc"
                  ;; Build doc.
                  (invoke "make" "html"
                          ;; Building the documentation takes a very long time.
                          ;; Parallelize it.
                          (string-append "SPHINXOPTS=-j"
                                         (number->string (parallel-job-count))))
                  ;; Install doc.
                  (mkdir-p html)
                  (copy-recursively "build/html" html)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./runtests.py" "-vv" "--no-build" "--mode=fast"
                        "-j" (number->string (parallel-job-count)))))))))
    (propagated-inputs (list python-numpy python-matplotlib python-pyparsing))
    (inputs (list openblas pybind11))
    (native-inputs
     (list gfortran
           perl
           python-cython
           python-numpydoc
           python-pydata-sphinx-theme
           python-pytest
           python-pytest-xdist
           python-sphinx
           python-sphinx-panels
           python-threadpoolctl
           which))
    (home-page "https://scipy.org/")
    (synopsis "The Scipy library provides efficient numerical routines")
    (description "The SciPy library is one of the core packages that make up
the SciPy stack.  It provides many user-friendly and efficient numerical
routines such as routines for numerical integration and optimization.")
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
    (version "0.19.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit-image" version))
       (sha256
        (base32 "0l645smf7w1kail70z8d9r3xmvz7qh6g7n3d2bpacbbnw5ykdd94"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'change-home-dir
           (lambda _
             ;; Change from /homeless-shelter to /tmp for write permission.
             (setenv "HOME" "/tmp")
             #t))
         (replace 'build
           (lambda _
             (invoke "make")))
         (replace 'check
           (lambda _
             ;; The following tests require online data.
             (invoke "python" "-m" "pytest" "skimage" "--doctest-modules" "-k"
                     (string-append "not test_ndim"
                                    " and not test_skin")))))))
    ;; See requirements/ for the list of build and run time requirements.
    ;; NOTE: scikit-image has an optional dependency on python-pooch, however
    ;; propagating it would enable many more tests that require online data.
    (propagated-inputs
     (list python-cloudpickle
           python-dask
           python-imageio
           python-matplotlib
           python-networkx
           python-numpy
           python-pillow
           python-pythran
           python-pywavelets
           python-scipy
           python-tifffile))
    (native-inputs
     (list python-cython
           python-pytest
           python-pytest-localserver))
    (home-page "https://scikit-image.org/")
    (synopsis "Image processing in Python")
    (description
     "Scikit-image is a collection of algorithms for image processing.")
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
    (build-system python-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "python" "setup.py" "build_ext" "--inplace")
                 (invoke "python" "-m" "pytest" "-v" "allel"
                         ;; AttributeError: 'Dataset' object has no attribute 'asstr'
                         "-k" (string-append
                                "not test_vcf_to_hdf5"
                                " and not test_vcf_to_hdf5_exclude"
                                " and not test_vcf_to_hdf5_rename"
                                " and not test_vcf_to_hdf5_group"
                                " and not test_vcf_to_hdf5_ann"))))))))
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

(define-public python-sgp4
  (package
    (name "python-sgp4")
    (version "2.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sgp4" version))
       (sha256
        (base32 "0dncp9i5b6afkg7f8mj9j0qzsp008b8v73yc0qkmizhpns7mvwvx"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/brandon-rhodes/python-sgp4")
    (synopsis "Track earth satellite TLE orbits using SGP4")
    (description
     "This package provides a Python implementation of the most recent version
of the SGP4 satellite tracking algorithm.")
    (license license:expat)))

(define-public python-trimesh
  (package
    (name "python-trimesh")
    (version "3.10.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trimesh" version))
       (sha256
        (base32 "0bw55cwxlxds0j54naijh64sdb0rkscx4i1fy0ql94h96kw2p2ir"))))
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
             (substitute* "trimesh/resources/templates/blender_boolean.py"
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

(define-public python-pandas
  (package
    (name "python-pandas")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas" version))
       (sha256
        (base32 "04lsak3j5hq2hk0vfjf532rdxdqmg2akamdl4yl3qipihp2izg4j"))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build python-build-system)
                  (ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enable-parallel-build
           (lambda _
             (substitute* "setup.py"
               (("\"-j\", type=int, default=1")
                (format #f "\"-j\", type=int, default=~a"
                        (parallel-job-count))))))
         (add-after 'unpack 'patch-which
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((which (assoc-ref inputs "which")))
               (substitute* "pandas/io/clipboard/__init__.py"
                 (("^WHICH_CMD = .*")
                  (string-append "WHICH_CMD = \"" which "\"\n"))))))
         (add-before 'check 'prepare-x
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0")
             ;; xsel needs to write a log file.
             (setenv "HOME" "/tmp")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (let ((build-directory
                    (string-append
                     (getcwd) "/build/"
                     (first (scandir "build"
                                     (cut string-prefix? "lib." <>))))))
               (substitute* "pyproject.toml"
                 ;; Not all data files are distributed with the tarball.
                 (("--strict-data-files ") ""))
               (with-directory-excursion build-directory
                 (when tests?
                   (invoke "pytest" "-vv" "pandas" "--skip-slow"
                           "--skip-network"
                           "-n" (number->string (parallel-job-count))
                           "-k"
                           (string-append
                            ;; These test access the internet (see:
                            ;; https://github.com/pandas-dev/pandas/issues/45085).:
                            ;; pandas/tests/io/xml/test_xml.py::test_wrong_url[lxml]
                            ;; pandas/tests/io/xml/test_xml.py::test_wrong_url[etree]
                            "not test_wrong_url"
                            ;; TODO: Missing input
                            " and not TestS3"
                            " and not s3"
                            ;; This test fails when run with pytest-xdist
                            ;; (see:
                            ;; https://github.com/pandas-dev/pandas/issues/39096).
                            " and not test_memory_usage"))))))))))
    (propagated-inputs
     (list python-jinja2
           python-numpy
           python-openpyxl
           python-pytz
           python-dateutil
           python-xlrd
           python-xlsxwriter))
    (inputs
     (list which xclip xsel))
    (native-inputs
     (list python-cython
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

(define-public python-bottleneck
  (package
    (name "python-bottleneck")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Bottleneck" version))
       (sha256
        (base32 "0wz5320jx3n4q2nsvwvc7cpi66b46qbals9v53m955rmcq5ry5r0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "setup.py" "pytest"))))))
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

(define-public python-xarray
  (package
    (name "python-xarray")
    (version "0.15.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "xarray" version))
              (sha256
               (base32
                "1yx8j66b7rn10m2l6gmn8yr9cn38pi5cj0x0wwpy4hdnhy6i7qv4"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools-scm python-pytest))
    (propagated-inputs
     (list python-numpy python-pandas))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest"))))))
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

(define-public python-msgpack-numpy
  (package
    (name "python-msgpack-numpy")
    (version "0.4.6.post0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "msgpack-numpy" version))
       (sha256
        (base32
         "0syzy645mwcy7lfjwz6pc8f9p2vv1qk4limc8iina3l5nnf0rjyz"))))
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
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas_flavor" version))
       (sha256
        (base32
         "12g4av8gpl6l83yza3h97j3f2jblqv69frlidrvdq8ny2rc6awbq"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pandas python-xarray))
    (home-page "https://github.com/Zsailer/pandas_flavor")
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
    (version "2022.05.2")
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
         "009jrlk7kmazrd3nkl217cl3x5ddg7kw9mqdgq1z9knv5h1rm8qv"))
       ;; Delete bundled copy of python-versioneer.
       (snippet '(delete-file "versioneer.py"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'versioneer
           (lambda _
             (invoke "versioneer" "install")))
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
                (format #false "\"~a/bin/dask-scheduler\""
                        (assoc-ref outputs "out")))
               (("\"dask-worker\"")
                (format #false "\"~a/bin/dask-worker\""
                        (assoc-ref outputs "out"))))))
         ;; ERROR: distributed==2022.5.2
         ;; ContextualVersionConflict (locket 0.2.0
         ;; (/gnu/store/...-python-locket-0.2.0/lib/python3.9/site-packages),
         ;; Requirement.parse('locket>=1.0.0'), {'distributed'})
         (delete 'sanity-check)
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
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
                               "protocol" "shuffle"))
               (invoke "python" "-m" "pytest" "-vv" "distributed"
                       "-m"
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
                          "test_errors"
                          "test_fail_to_pickle_target_2"
                          "test_file_descriptors_dont_leak"
                          "test_finished"
                          "test_get_client_functions_spawn_clusters"
                          "test_host_uses_scheduler_protocol"
                          "test_identity_inproc"
                          "test_identity_tcp"
                          "test_large_packets_inproc"
                          "test_locked_comm_drop_in_replacement"
                          "test_locked_comm_intercept_read"
                          "test_locked_comm_intercept_write"
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
                          "test_threadpoolworkers_pick_correct_ioloop"
                          "test_tls_listen_connect"
                          "test_tls_temporary_credentials_functional"
                          "test_variable_in_task"
                          "test_worker_preload_text"
                          "test_worker_uses_same_host_as_nanny")
                         " and not ")

                        ;; These fail because it doesn't find dask[distributed]
                        " and not test_quiet_close_process"

                        ;; This one fails because of a silly assert failure:
                        ;; '2022.05.2' == '2022.5.2'
                        " and not test_version"
                        " and not test_git_revision"

                        ;; Recursion stack failure.  No idea what they
                        ;; expected to happen.
                        " and not test_stack_overflow"

                        ;; These tests are rather flaky
                        " and not test_quiet_quit_when_cluster_leaves"
                        " and not multiple_clients_restart"))))))))
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
     (list python-pytest
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

(define-public python-pyvista
  (package
    (name "python-pyvista")
    (version "0.36.1")
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
        (base32 "1kjilcrz2cyh67n79r8dpxrans99mlviz2whc6g7j8hgn7v14z2n"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-appdirs
           python-imageio
           python-matplotlib
           python-meshio
           python-numpy
           python-pillow
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
         ;;   ...checking requirements: ERROR: pyvista==0.34.0 DistributionNotFound(Requirement.parse('vtk'), {'pyvista'})
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
                "1yx65ajrmqp02ykclhlb0n8s3bx5r0xj075swwwigiqaippr7dx2"))))
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
     (list #:phases #~(modify-phases %standard-phases
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
    (build-system python-build-system)
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

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
