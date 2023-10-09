;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017-2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018, 2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021, 2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2020 Alexander Krotov <krotov@iitp.ru>
;;; Copyright © 2020 Pierre Langlois <pierre.langlos@gmx.com>
;;; Copyright © 2021, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Alexandre Hannud Abdo <abdo@member.fsf.org>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 David Elsing <david.elsing@posteo.net>
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

(define-module (gnu packages graph)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public plfit
  (package
    (name "plfit")
    (version "0.9.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ntamas/plfit")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "042b60cnsz5wy27sz026xs0mnn9p58j46crgs78skncgkvzqyyc6"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON")))
    (home-page "https://github.com/ntamas/plfit")
    (synopsis "Tool for fitting power-law distributions to empirical data")
    (description "The @command{plfit} command fits power-law distributions to
empirical (discrete or continuous) data, according to the method of Clauset,
Shalizi and Newman (@cite{Clauset A, Shalizi CR and Newman MEJ: Power-law
distributions in empirical data.  SIAM Review 51, 661-703 (2009)}).")
    (license license:gpl2+)))

(define-public igraph
  (package
    (name "igraph")
    (version "0.10.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/igraph/igraph")
             (commit version)))
       (file-name (git-file-name name version))
       (patches (search-patches "igraph-fix-varargs-integer-size.patch"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet '(begin

                   (delete-file-recursively "vendor")
                   (delete-file-recursively "src/isomorphism/bliss")
                   (substitute* '("src/CMakeLists.txt"
                                  "etc/cmake/benchmark_helpers.cmake")
                     ;; Remove extraneous bundling related variables.
                     ((".*_IS_VENDORED.*") "")
                     ((".*add_sub.*isomorphism/bliss.*") "")
                     (("(.*TARGETS.*)bliss(.*)cxsparse_vendored(.*)pcg(.*)"
                       _ part1 part2 part3 part4)
                      (string-append part1 part2 part3 part4))
                     (("cxsparse_vendored") "cxsparse")
                     ((" pcg ") " pcg_random "))
                   (substitute* "CMakeLists.txt"
                     (("add_sub.*vendor.*") ""))))
       (sha256
        (base32
         "025f9c2jsawniqkig4l5z3v9aw3ipazmnlsf80b653mns5bvj1yn"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DBUILD_SHARED_LIBS=ON"
              ;; Use the same integer width as suitesparse-cxsparse, which
              ;; uses int64_t in SuiteSparse v6.0.0 and later.
              "-DIGRAPH_INTEGER_SIZE=64")
      #:test-target "check"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'version-file
            (lambda _
              (let ((port (open-file "IGRAPH_VERSION" "w")))
                (display #$version port)
                (close port))))
          (add-after 'unpack 'patch-suitesparse
            (lambda _
              (substitute* '("src/core/sparsemat.c"
                             "include/igraph_sparsemat.h")
                (("<cs/cs\\.h>") "<cs.h>")
                (("cs_igraph") "cs_dl")
                (("__BEGIN_DECLS.*" all)
                 (string-append all "\n#define CS_LONG\n")))))
          (add-after 'unpack 'patch-pcg
            (lambda _
              (substitute* '("src/random/rng_pcg32.c"
                             "src/random/rng_pcg64.c")
                (("#include \"pcg/(.*)\"" _ name)
                 (string-append "#include <" name ">")))))
          (add-after 'unpack 'patch-bliss
            (lambda _
              (substitute* "src/isomorphism/bliss.cc"
                (("#include \"bliss.*")
                 (string-append
                  "#include <bliss/graph.hh>\n"
                  "#include <bliss/digraph.hh>\n")))))
          (add-after 'build 'build-doc
            (lambda _
              (invoke "cmake" "--build" "." "--target" "html")))
          (add-after 'install 'install-doc
            (lambda _
              (copy-recursively
               "doc/html"
               (string-append #$output "/share/doc/"
                              #$name "-" #$version "/html")))))))
    (native-inputs
     (list bison
           docbook-xml-4.3
           docbook-xsl
           flex
           pcg-c
           pkg-config
           ;; For the HTML documentation.
           python
           source-highlight
           xmlto))
    (inputs
     (list arpack-ng
           bliss
           glpk
           ;lapack
           openblas
           plfit
           suitesparse-cxsparse))
    ;; libxml2 is in the 'Requires.private' of igraph.pc.
    (propagated-inputs (list libxml2))
    (home-page "https://igraph.org")
    (synopsis "Network analysis and visualization")
    (description
     "This package provides a library for the analysis of networks and graphs.
It can handle large graphs very well and provides functions for generating
random and regular graphs, graph visualization, centrality methods and much
more.")
    (license license:gpl2+)))

(define-public python-igraph
  ;; Temporarily use a precise commit, as there was a mistake in the last
  ;; release that was fixed by it (see:
  ;; https://github.com/igraph/python-igraph/issues/632).
  (let ((revision "0")
        (commit "b6ebd8eb277fc1d0e33340a6624629a10c638992"))
    (package
      (inherit igraph)
      (name "python-igraph")
      (version (git-version "0.10.4" revision commit))
      (source (origin
                (method git-fetch)
                ;; The PyPI archive lacks tests.
                (uri (git-reference
                      (url "https://github.com/igraph/python-igraph")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0dhrz5a6pi6vs94fm8q4nmkh6v1nmpw1sk482xls213zcbbh67hd"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'specify-libigraph-location
              (lambda _
                (let ((igraph #$(this-package-input "igraph")))
                  (substitute* "setup.py"
                    (("(LIBIGRAPH_FALLBACK_INCLUDE_DIRS = ).*" _ var)
                     (string-append
                      var (format #f "[~s]~%"
                                  (string-append igraph "/include/igraph"))))
                    (("(LIBIGRAPH_FALLBACK_LIBRARY_DIRS = ).*" _ var)
                     (string-append
                      var (format #f "[~s]~%"
                                  (string-append igraph "/lib")))))))))))
      (inputs (list igraph))
      (propagated-inputs (list python-texttable))
      (native-inputs (list python-pytest))
      (home-page "https://igraph.org/python/")
      (synopsis "Python bindings for the igraph network analysis library"))))

(define-public r-rbiofabric
  (let ((commit "666c2ae8b0a537c006592d067fac6285f71890ac")
        (revision "1"))
    (package
      (name "r-rbiofabric")
      (version (string-append "0.3-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wjrl/RBioFabric")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1yahqrcrqpbcywv73y9rlmyz8apdnp08afialibrr93ch0p06f8z"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-igraph))
      (home-page "http://www.biofabric.org/")
      (synopsis "BioFabric network visualization")
      (description "This package provides an implementation of the function
@code{bioFabric} for creating scalable network digrams where nodes are
represented by horizontal lines, and edges are represented by vertical
lines.")
      (license license:expat))))

(define-public python-plotly
  (package
    (name "python-plotly")
    (version "5.20.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plotly/plotly.py")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i22sv8p3kl84nkldbv1253kld85rbwp2pdxivxn64wwflfpqvx6"))))
    (build-system python-build-system)
    (arguments
     (list
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
              (chdir "packages/python/plotly")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-x" "plotly/tests/test_core")
                (invoke "pytest" "-x" "plotly/tests/test_io")
                ;; FIXME: Add optional dependencies and enable their tests.
                ;; (invoke "pytest" "-x" "plotly/tests/test_optional")
                (invoke "pytest" "_plotly_utils/tests")))))))
    (native-inputs
     (list python-ipywidgets python-pytest python-xarray))
    (propagated-inputs
     (list python-ipython
           python-pandas
           python-pillow
           python-requests
           python-retrying
           python-six
           python-tenacity
           python-statsmodels))
    (home-page "https://plotly.com/python/")
    (synopsis "Interactive plotting library for Python")
    (description "Plotly's Python graphing library makes interactive,
publication-quality graphs online.  Examples of how to make line plots, scatter
plots, area charts, bar charts, error bars, box plots, histograms, heatmaps,
subplots, multiple-axes, polar charts, and bubble charts.")
    (license license:expat)))

(define-public python-plotly-2.4.1
  (package (inherit python-plotly)
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "plotly" version))
        (sha256
         (base32
          "0s9gk2fl53x8wwncs3fwii1vzfngr0sskv15v3mpshqmrqfrk27m"))))
   (native-inputs '())
   (propagated-inputs
    (list python-decorator
          python-nbformat
          python-pandas
          python-pytz
          python-requests
          python-six))
   (arguments
    (list
     #:tests? #false ;The tests are not distributed in the release
     #:phases
     '(modify-phases %standard-phases
        (add-after 'unpack 'python-compatibility
          (lambda _
            (substitute* "plotly/grid_objs/grid_objs.py"
              (("from collections import MutableSequence")
               "from collections.abc import MutableSequence")))))))))

(define-public python-louvain
  (package
    (name "python-louvain")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-louvain" version))
       (patches (search-patches "python-louvain-fix-test.patch"))
       (sha256
        (base32 "0sx53l555rwq0z7if8agirjgw4ddp8r9b949wwz8vlig03sjvfmp"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools-57))       ;for use_2to3 support
    (propagated-inputs
     (list python-networkx python-numpy))
    (home-page "https://github.com/taynaud/python-louvain")
    (synopsis "Louvain algorithm for community detection")
    (description
     "This package provides a pure Python implementation of the Louvain
algorithm for community detection in large networks.")
    (license license:bsd-3)))

(define-public python-vtraag-louvain
  (package
    (name "python-vtraag-louvain")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "louvain" version))
              (sha256
               (base32
                "16l2zi4jwc3vpvpnz32jv7xy0g5087dp9y57wxplj1xa9r312x0i"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-bundled-igraph
           (lambda _
             (substitute* "setup.py"
               (("self.external = False")
                "self.external = True")
               (("self.use_pkgconfig = False")
                "self.use_pkgconfig = True")))))))
    (inputs (list igraph))
    (propagated-inputs (list python-igraph))
    (native-inputs
     (list pkg-config
           python-ddt
           python-setuptools-scm))
    (home-page "https://github.com/vtraag/louvain")
    (synopsis "Community detection in large networks")
    (description
     "Louvain is a general algorithm for methods of community detection in
large networks.")
    (license license:gpl3+)))

(define-public python-graphtools
  (package
    (name "python-graphtools")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KrishnaswamyLab/graphtools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aaxhij4y5z2vvc34qnb5py6nw3ciz35a3z4lfr223f9kvfpqgak"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             ;; The warning message has changed in numpy.
             (substitute* "test/test_data.py"
               (("\"A sparse matrix was passed, but.*array.\",")
                "\"Sparse data was passed, but dense data is required. Use '.toarray()' to convert to a dense numpy array.\",")
               ;; anndata prints a warning that causes the test to fail.
               (("import warnings" m)
                (string-append m "\nwarnings.filterwarnings(\"ignore\")")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Incompatibility with sklearn.
               ;; 'kNNLandmarkGraph' object has no attribute '_landmark_op'
               (delete-file "test/test_landmark.py")
               (setenv "LOKY_MAX_CPU_COUNT" "1")
               (invoke "nose2" "-v")))))))
    (propagated-inputs
     (list python-deprecated
           python-future
           python-numpy
           python-pygsp
           python-scikit-learn
           python-scipy
           python-tasklogger))
    (native-inputs
     (list util-linux ;for lscpu
           python-anndata
           python-black
           python-coverage
           python-coveralls
           python-nose
           python-nose2
           python-pandas
           python-parameterized
           python-igraph))
    (home-page "https://github.com/KrishnaswamyLab/graphtools")
    (synopsis "Tools for building and manipulating graphs in Python")
    (description "This package provides tools for building and manipulating
graphs in Python.")
    (license license:gpl3)))

(define-public python-louvain-igraph
  (package
    (name "python-louvain-igraph")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vtraag/louvain-igraph")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1j2ybihvvzggwjb9zvm829aqb5b94q10h8bw6v0h42xd9w75z9sv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'find-igraph
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)
              (substitute* "setup.py"
                (("/usr/include/igraph")
                 (string-append #$(this-package-input "igraph")
                                "/include/igraph"))))))))
    (propagated-inputs (list python-igraph))
    (inputs (list igraph))
    (native-inputs
     (list python-ddt
           python-setuptools-scm
           pkg-config))
    (home-page "https://github.com/vtraag/louvain-igraph")
    (synopsis "Implementation of the Louvain algorithm")
    (description "This package implements the Louvain algorithm for community
detection in C++ and exposes it to Python.  Besides the relative flexibility
of the implementation, it also scales well, and can be run on graphs of
millions of nodes (as long as they can fit in memory).  The core function is
@code{find_partition} which finds the optimal partition using the louvain
algorithm for a number of different methods.")
    (license license:gpl3+)))

(define-public python-pygsp
  (package
    (name "python-pygsp")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyGSP" version))
       (sha256
        (base32 "002q4z3p3ka81rzhgi66qqmz1ccrg9hwch4bax7jsqixg64asx28"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These all fail due to a type error in scipy.
      '(list "-k" (string-append "not test_bunny"
                                 " and not test_lowstretchtree"
                                 " and not test_nngraph"
                                 " and not test_plot_graphs"
                                 " and not test_randomregular"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'disable-doctests
           (lambda _
             (substitute* "pygsp/tests/test_all.py"
               (("def test_docstrings.*") "def _disabled_test_docstrings():\n")
               (("return doctest.DocFileSuite.*") "return False\n")
               (("suites.append\\(test_docstrings.*")
                "")))))))
    (propagated-inputs (list python-numpy python-scikit-image python-scipy))
    (native-inputs
     (list python-coverage python-coveralls python-flake8
           python-pytest))
    (home-page "https://github.com/epfl-lts2/pygsp")
    (synopsis "Graph Signal Processing in Python")
    (description "The PyGSP is a Python package to ease signal processing on
graphs.")
    (license license:bsd-3)))

(define-public faiss
  (package
    (name "faiss")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/facebookresearch/faiss")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pk15jfa775cy2pqmzq62nhd6zfjxmpvz5h731197c28aq3zw39w"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "utils.cpp"
                    (("#include <immintrin.h>")
                     "#ifdef __SSE__\n#include <immintrin.h>\n#endif"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DBUILD_WITH_GPU=OFF"     ; thanks, but no thanks, CUDA.
             "-DBUILD_TUTORIAL=OFF")    ; we don't need those
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-build
           (lambda _
             (let ((features (list ,@(let ((system (or (%current-target-system)
                                                       (%current-system))))
                                       (cond
                                        ((string-prefix? "x86_64" system)
                                         '("-mavx" "-msse2" "-mpopcnt"))
                                        ((string-prefix? "i686" system)
                                         '("-msse2" "-mpopcnt"))
                                        (else
                                         '()))))))
               (substitute* "CMakeLists.txt"
                 (("-m64") "")
                 (("-mpopcnt") "")      ; only some architectures
                 (("-msse4")
                  (string-append
                   (string-join features)
                   " -I" (getcwd)))
                 ;; Build also the shared library
                 (("ARCHIVE DESTINATION lib")
                  "LIBRARY DESTINATION lib")
                 (("add_library.*" m)
                  "\
add_library(objlib OBJECT ${faiss_cpu_headers} ${faiss_cpu_cpp})
set_property(TARGET objlib PROPERTY POSITION_INDEPENDENT_CODE 1)
add_library(${faiss_lib}_static STATIC $<TARGET_OBJECTS:objlib>)
add_library(${faiss_lib} SHARED $<TARGET_OBJECTS:objlib>)
install(TARGETS ${faiss_lib}_static ARCHIVE DESTINATION lib)
\n")))

             ;; See https://github.com/facebookresearch/faiss/issues/520
             (substitute* "IndexScalarQuantizer.cpp"
               (("#define USE_AVX") ""))

             ;; Make header files available for compiling tests.
             (mkdir-p "faiss")
             (for-each (lambda (file)
                         (mkdir-p (string-append "faiss/" (dirname file)))
                         (copy-file file (string-append "faiss/" file)))
                       (find-files "." "\\.h$"))
             #t))
         (replace 'check
           (lambda _
             (invoke "make" "-C" "tests"
                     (format #f "-j~a" (parallel-job-count)))))
         (add-after 'install 'remove-tests
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively
              (string-append (assoc-ref outputs "out")
                             "/test"))
             #t)))))
    (inputs
     (list openblas))
    (native-inputs
     (list googletest))
    (home-page "https://github.com/facebookresearch/faiss")
    (synopsis "Efficient similarity search and clustering of dense vectors")
    (description "Faiss is a library for efficient similarity search and
clustering of dense vectors.  It contains algorithms that search in sets of
vectors of any size, up to ones that possibly do not fit in RAM.  It also
contains supporting code for evaluation and parameter tuning.")
    (license license:bsd-3)))

(define-public python-faiss
  (package (inherit faiss)
    (name "python-faiss")
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "python") #t))
         (add-after 'chdir 'build-swig
           (lambda* (#:key inputs #:allow-other-keys)
             (with-output-to-file "../makefile.inc"
               (lambda ()
                 (let ((python-version ,(version-major+minor (package-version python))))
                   (format #t "\
PYTHONCFLAGS =-I~a/include/python~a/ -I~a/lib/python~a/site-packages/numpy/core/include
LIBS = -lpython~a -lfaiss
SHAREDFLAGS = -shared -fopenmp
CXXFLAGS = -fpermissive -fopenmp -fPIC
CPUFLAGS = ~{~a ~}~%"
                           (assoc-ref inputs "python*") python-version
                           (assoc-ref inputs "python-numpy") python-version
                           python-version
                           (list ,@(let ((system (or (%current-target-system)
                                                     (%current-system))))
                                     (cond
                                       ((string-prefix? "x86_64" system)
                                        '("-mavx" "-msse2" "-mpopcnt"))
                                       ((string-prefix? "i686" system)
                                        '("-msse2" "-mpopcnt"))
                                       (else
                                         '()))))))))
             (substitute* "Makefile"
               (("../libfaiss.a") ""))
             (invoke "make" "cpu"))))))
    (inputs
     `(("faiss" ,faiss)
       ("openblas" ,openblas)
       ("python*" ,python)
       ("swig" ,swig)))
    (propagated-inputs
     (list python-matplotlib python-numpy))
    (description "Faiss is a library for efficient similarity search and
clustering of dense vectors.  This package provides Python bindings to the
Faiss library.")))

(define-public python-leidenalg
  (package
    (name "python-leidenalg")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "leidenalg" version))
       (sha256
        (base32
         "1wvmi6ca9kf8pbxg6b18n64h82wr9a6wcdazyn82pww0dwxzwp3y"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                      ;tests are not included
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-requirements
                    (lambda _
                      (substitute* "setup.py"
                        (("self.external = False")
                         "self.external = True")
                        (("self.use_pkgconfig = False")
                         "self.use_pkgconfig = True")
                        (("python-igraph >=")
                         "igraph >=")))))))
    (native-inputs
     (list pkg-config python-setuptools-scm))
    (inputs
     (list igraph))
    (propagated-inputs
     (list python-igraph))
    (home-page "https://github.com/vtraag/leidenalg")
    (synopsis "Community detection in large networks")
    (description
     "Leiden is a general algorithm for methods of community detection in
large networks.  This package implements the Leiden algorithm in C++ and
exposes it to Python.  Besides the relative flexibility of the implementation,
it also scales well, and can be run on graphs of millions of nodes (as long as
they can fit in memory).  The core function is @code{find_partition} which
finds the optimal partition using the Leiden algorithm, which is an extension
of the Louvain algorithm, for a number of different methods.")
    (license license:gpl3+)))

(define-public edge-addition-planarity-suite
  (package
    (name "edge-addition-planarity-suite")
    (version "3.0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://github.com/graph-algorithms/"
                                  name))
              (commit (string-append "Version_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1c7bnxgiz28mqsq3a3msznmjq629w0qqjynm2rqnnjn2qpc22h3i"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (synopsis "Embedding of planar graphs")
    (description "The package provides a reference implementation of the
linear time edge addition algorithm for embedding planar graphs and
isolating planarity obstructions.")
    (license license:bsd-3)
    (home-page
      "https://github.com/graph-algorithms/edge-addition-planarity-suite")))

(define-public rw
  (package
    (name "rw")
    ;; There is a version 0.8, but the tarball is broken with symlinks
    ;; to /usr/share.
    (version "0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/rankwidth/"
                                  "rw-" version ".tar.gz"))
              (sha256
               (base32
                "0hdlxxmlccb6fp7g58zv0rdzpbyjn9bgqlf052sgrk95zq33bq61"))
              (patches (search-patches "rw-igraph-0.10.patch"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list igraph))
    (home-page "https://sourceforge.net/projects/rankwidth/")
    (synopsis "Rank-width and rank-decomposition of graphs")
    (description "rw computes rank-width and rank-decompositions
of graphs.")
    (license license:gpl2+)))

(define-public mscgen
  (package
    (name "mscgen")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.mcternan.me.uk/mscgen/software/mscgen-src-"
                           version ".tar.gz"))
       (sha256
        (base32
         "08yw3maxhn5fl1lff81gmcrpa4j9aas4mmby1g9w5qcr0np82d1w"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list gd))
    (home-page "https://www.mcternan.me.uk/mscgen/")
    (synopsis "Message Sequence Chart Generator")
    (description "Mscgen is a small program that parses Message Sequence Chart
descriptions and produces PNG, SVG, EPS or server side image maps (ismaps) as
the output.  Message Sequence Charts (MSCs) are a way of representing entities
and interactions over some time period and are often used in combination with
SDL.  MSCs are popular in Telecoms to specify how protocols operate although
MSCs need not be complicated to create or use.  Mscgen aims to provide a simple
text language that is clear to create, edit and understand, which can also be
transformed into common image formats for display or printing.")
    (license license:gpl2+)))

(define-public python-graph-tool
  (package
    (name "python-graph-tool")
    (version "2.59")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://downloads.skewed.de/graph-tool/graph-tool-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1bmck5fcihj9lr5kd8x624bdi9xhfc13pl4mwzv74jr5lz07kr6d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules (,@%default-gnu-imported-modules
                           (guix build python-build-system))
       #:modules (,@%default-gnu-modules
                  ((guix build python-build-system) #:select (site-packages)))
       ;; The build process peaks around 4GB/RAM per core.
       #:parallel-build? #f
       #:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost"))
             (string-append "--with-python-module-path="
                            (site-packages %build-inputs %outputs)))))
    (native-inputs
     (list ncurses pkg-config))
    (inputs
     (list boost
           cairomm-1.14
           cgal
           expat
           gmp
           gtk+
           python-wrapper
           sparsehash))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-pycairo
           python-scipy
           python-zstandard))
    (synopsis "Manipulate and analyze graphs with Python efficiently")
    (description "Graph-tool is an efficient Python module for manipulation
and statistical analysis of graphs (a.k.a. networks).  Contrary to most other
Python modules with similar functionality, the core data structures and
algorithms are implemented in C++, making extensive use of template
metaprogramming, based heavily on the Boost Graph Library.  This confers it a
level of performance that is comparable (both in memory usage and computation
time) to that of a pure C/C++ library.")
    (home-page "https://graph-tool.skewed.de/")
    (license license:lgpl3+)))
