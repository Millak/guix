;;; Copyright © 2026 Danny Milosavljevic <dannym@friendly-machines.com>
;;; Copyright © 2025 Nigko Yerden <nigko.yerden@gmail.com>
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

(define-module (gnu packages physics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix build-system python) #:select (pypi-uri))
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages noweb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-graphics)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public alps
  (package
    (name "alps")
    (version "2.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ALPSim/ALPS")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dlmfl73dgw0ry7bcvdlr0p5fz0h62xn6c00vrpnjk60kj8cxsnr"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:parallel-tests? #f
      #:configure-flags
      #~(list "-DALPS_ENABLE_OPENMP=ON"
              "-DALPS_BUILD_PYTHON=ON"
              "-DALPS_BUILD_FORTRAN=ON")
      #:phases
      #~(modify-phases %standard-phases
          ;; Below is a workaround borrowed from the hdf5 package
          ;; for the problem with including <fenv.h> gfortran header
          (add-after 'set-paths 'hide-gfortran
            (lambda _
              (let ((gfortran #$(this-package-input "gfortran")))
                (setenv "CPLUS_INCLUDE_PATH"
                        (string-join
                         (delete (string-append gfortran "/include/c++")
                                 (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                         ":")))))
          (add-after 'unpack 'rewrite-find-boost-cmake
            (lambda _
              (let ((boost-components '("chrono" "timer" "date_time"
                                        "filesystem" "iostreams"
                                        "program_options" "regex"
                                        "serialization" "thread"
                                        "python" "mpi" "numpy")))
                (call-with-output-file "config/FindBoostForALPS.cmake"
                  (lambda (port)
                    (display "find_package(Boost REQUIRED COMPONENTS" port)
                    (for-each (lambda (x)
                                (format port "\n~a" x)) boost-components)
                    (display ")\n" port)
                    (display "set(ALPS_HAVE_BOOST_NUMPY ON)\n" port))))))
          (add-after 'rewrite-find-boost-cmake 'fix-cmake-cxx-flags
            (lambda _
              (substitute* "CMakeLists.txt"
                (("^set\\(CMAKE_CXX_FLAGS .* -fpermissive.*\\)")
                 (string-append "set(CMAKE_CXX_FLAGS \""
                                "${CMAKE_CXX_FLAGS}"
                                " -fpermissive"
                                " -DBOOST_NO_AUTO_PTR"
                                " -DBOOST_FILESYSTEM_NO_CXX20_ATOMIC_REF"
                                " -DBOOST_AC_USE_PTHREADS"
                                ;; For boost-1.89 the flag below leads
                                ;; to weird errors in tests:
                                ;; " -DBOOST_SP_USE_PTHREADS"
                                " -DBOOST_TIMER_ENABLE_DEPRECATED"
                                "\")")))))
          (add-after 'fix-cmake-cxx-flags 'reconcile-with-boost-1.89
            (lambda _
              (substitute*
                  '("src/alps/numeric/matrix/matrix_element_iterator.hpp"
                    "src/alps/numeric/matrix/strided_iterator.hpp")
                (("^#include <boost/iterator/iterator_facade.hpp>" include)
                 (string-append "#include <boost/type_traits/is_same.hpp>\n"
                                "#include <boost/type_traits/add_const.hpp>\n"
                                include)))))
          (add-after 'reconcile-with-boost-1.89 'fix-fortran-comment
            (lambda _
              (substitute* "src/alps/fortran/alps_fortran.h"
                (("^\\*") "!")))))))
    (native-inputs
     (list python-scipy   ; only used in tests
           python-numpy))
    (inputs
     (list expat
           gfortran
           hdf5
           python
           openblas))
    (propagated-inputs
     ;; User defined applications should use the boost and openmpi libs the
     ;; alps package compiled with.
     (list boost-mpi-numpy
           openmpi))
    (home-page "https://alps.comp-phys.org/")
    (synopsis "Algorithms and Libraries for Physics Simulations")
    (description
     "The ALPS (Algorithms and Libraries for Physics Simulations) package
provides a set of components for numerical simulations of condensed matter
systems, including bosonic, fermionic, and spin systems with exact
diagonalization, classical and quantum Monte Carlo, dynamical mean field
theory, and density matrix renormalization group methods.  It contains both
ready to use applications and C++ library for simplifying the development of
such components.")
    (license license:expat)))

(define-public python-brille
  (package
    (name "python-brille")
    (version "0.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brille/brille")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vyxa7k6yrpxizbmljrv7bnsf7dzxsfbs4id36x09jjxwh7dysjj"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
              ;; Boost.System is header-only since 1.69, but FindBoost looks for
              ;; libboost_system.so which doesn't exist.
              "-DHIGHFIVE_USE_BOOST=OFF"
              ;; Pretend we're doing a scikit-build build to skip Conan.
              "-DSKBUILD=ON"
              (string-append "-DSKBUILD_PROJECT_NAME=brille")
              (string-append "-DSKBUILD_PROJECT_VERSION=" #$version))
      #:imported-modules `(,@%cmake-build-system-modules
                           ,@%pyproject-build-system-modules)
      #:modules '((guix build cmake-build-system)
                  ((guix build python-build-system) #:select (site-packages))
                  (guix build utils))
      #:phases
      (with-extensions (list (pyproject-guile-json))
        #~(modify-phases %standard-phases
            (add-after 'unpack 'create-pkg-info
              (lambda _
                ;; Create PKG-INFO so DynamicVersion.cmake finds version without git.
                (call-with-output-file "PKG-INFO"
                  (lambda (port)
                    (format port "Metadata-Version: 2.1
Name: brille
Version: ~a
" #$version)))))
            (add-after 'install 'install-python
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((site-packages (site-packages inputs outputs)))
                  (mkdir-p (string-append site-packages "/brille"))
                  ;; Install Python source files and compiled extension module.
                  (for-each (lambda (file)
                              (install-file file
                                            (string-append site-packages "/brille")))
                            (append
                             (find-files "../source/brille" "\\.py$")
                             (find-files "." "^_brille\\..*\\.so$"))))))))))
    (native-inputs
     (list catch2-3
           cmake-minimal
           highfive
           pybind11-2
           python-wrapper
           python-setuptools
           python-setuptools-scm))
    (inputs
     (list hdf5))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/brille/brille")
    (synopsis "Symmetry operations and interpolation in Brillouin zones")
    (description
     "Brille is a C++ library for symmetry operations and linear interpolation
within an irreducible part of the first Brillouin zone.  It provides Python
bindings via pybind11 for use in phonon calculations and inelastic neutron
scattering simulations.")
    (license license:agpl3+)))

(define-public python-gofit
  (package
    (name "python-gofit")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ralna/gofit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x7kk97k4v1mzgs29z9i2yidjx4hmdwhng77178l564hn29k1c2b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-requirements
            (lambda _
              ;; pybind11[global] is a build-time dependency, not runtime.
              ;; <https://github.com/pybind/python_example/issues/45>
              (substitute* "setup.py"
                (("install_requires=")
                 "setup_requires="))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; test_multistart_ls.py needs cubic.txt in cwd
                (copy-file "tests/cubic.txt" "cubic.txt")
                (for-each (lambda (test)
                            (invoke "python" test))
                          (find-files "tests" "^test_.*\\.py$"))))))))
    (native-inputs (list pybind11-2 python-setuptools cmake python-pytest
                         python-numpy))
    (inputs (list eigen))
    (home-page "https://github.com/ralna/gofit")
    (synopsis "GOFit: Global Optimization for Fitting problems")
    (description "GOFit: Global Optimization for Fitting problems.")
    (license license:bsd-3)))

(define-public python-mslice
  (package
    (name "python-mslice")
    (version "2.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mantidproject/mslice.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bbg9hyl6jxyk79hshvqvcbwbx48x6va5nyhavj5kjg6ybd0n8fd"))
       (patches
        (search-patches "python-mslice-matplotlib-3.6-compatibility.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f                    ;require mantid
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-compatibility
                 (lambda _
                   ;; dedent_interpd was an alias for interpd; the alias was
                   ;; removed in
                   ;; <https://github.com/matplotlib/matplotlib/pull/28826>.
                   (substitute* "src/mslice/plotting/pyplot.py"
                     (("@_docstring\\.dedent_interpd")
                      "@_docstring.interpd"))
                   ;; self.execute("cls") fails; use widget's clear() method.
                   ;; <https://github.com/mantidproject/mslice/issues/1152>
                   (substitute* "src/mslice/widgets/ipythonconsole/ipython_widget.py"
                     (("self\\.execute\\(\"cls\"\\)")
                      "self.clear()"))))
               (delete 'sanity-check))))  ;would require mantid
    (propagated-inputs (list python-matplotlib python-pyqt python-qtpy
                             python-qtawesome))
    (native-inputs (list python-pytest python-setuptools))
    (home-page "https://github.com/mantidproject/mslice")
    (synopsis "Performs slices and cuts of multi-dimensional data produced by Mantid")
    (description "This package provides a tool for performing slices and cuts
of multi-dimensional data produced by Mantid.")
    (license license:gpl3+)))

(define-public python-mvesuvio
  (package
    (name "python-mvesuvio")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mantidproject/vesuvio")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1drpz41dfqlfbx9jpsgig6hv52ylxs79ghzk2a7m6109vbawksvw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f  ; tests require mantid
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-version
            (lambda _
              ;; versioningit needs git tags; set version via environment
              ;; variable and create _version.py directly.
              (mkdir-p "src/mvesuvio")
              (call-with-output-file "src/mvesuvio/_version.py"
                (lambda (port)
                  (format port "__version__ = \"~a\"~%" #$version))))))))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-dill
           python-iminuit
           python-jacobi))
    (home-page "https://github.com/mantidproject/vesuvio")
    (synopsis "Analysis library for VESUVIO neutron spectrometer data")
    (description
     "MVesuvio provides optimized analysis procedures for neutron scattering
data from the VESUVIO spectrometer.  It is a script library meant to be
imported in Mantid Workbench's script editor (@code{import mvesuvio as mv}),
not a GUI application.")
    (license license:gpl3)))

(define-public python-pycifrw
  (package
    (name "python-pycifrw")
    (version "4.4.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jamesrhester/pycifrw.git")
             (commit version)))
       (sha256
        (base32 "0xda4vgm6dz6fhhrfv8y6igsc5kznlnv0j3yrwkbcd3qqv16ic6r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'generate-sources
            (lambda _
              (invoke "make" "-C" "src" "PYTHON=python3" "package")
              (invoke "make" "-C" "src/drel" "PYTHON=python3" "package")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python3" "TestPyCIFRW.py")))))))
    (propagated-inputs
     (list python-numpy python-ply))
    (native-inputs
     (list latex2html noweb python-setuptools))
    (home-page "https://github.com/jamesrhester/pycifrw")
    (synopsis "CIF file reader and writer")
    (description
     "PyCifRW provides support for reading and writing CIF (Crystallographic
Information File) format files.  CIF is the standard format for
crystallographic data exchange endorsed by the International Union of
Crystallography.")
    (license license:psfl)))

(define-public python-pyoncat
  (package
    (name "python-pyoncat")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyoncat" version))
       (sha256
        (base32 "16lkpbkn7wyx8ag42sjmqm0c5acfs5dbglf3ahnfwpvcgcxj6jql"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f  ; no tests in sdist
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-build-system
                 (lambda _
                   ;; Upstream uses the deprecated poetry.masonry.api build
                   ;; backend which requires the full poetry package.  Guix
                   ;; only has poetry-core, so switch to poetry.core.masonry.api.
                   (substitute* "pyproject.toml"
                     (("requires = \\[\"poetry\"\\]")
                      "requires = [\"poetry-core\"]")
                     (("build-backend = \"poetry\\.masonry\\.api\"")
                      "build-backend = \"poetry.core.masonry.api\"")))))))
    (native-inputs
     (list python-poetry-core))
    (propagated-inputs
     (list python-oauthlib
           python-requests
           python-requests-oauthlib))
    (home-page "https://oncat.ornl.gov")
    (synopsis "Python client for ONCat (ORNL Neutron Catalog)")
    (description
     "This package provides a Python client for ONCat, the Oak Ridge National
Laboratory Neutron Catalog.  ONCat is a data catalog service for neutron
scattering facilities.")
    (license license:expat)))

(define-public python-pyoncatqt
  (package
    (name "python-pyoncatqt")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neutrons/pyoncatqt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wdzff1jn2jv742qm7g728yzp7axgf7nrizm5hms2w796a1zdqwv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f  ; tests require mantid
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-version
            (lambda _
              ;; versioningit needs git tags; patch pyproject.toml to use
              ;; static version and create _version.py directly.
              (substitute* "pyproject.toml"
                (("dynamic = \\[\"version\"\\]")
                 (string-append "version = \"" #$version "\""))
                (("source = \"versioningit\"") "")
                (("\\[tool\\.hatch\\.build\\.hooks\\.versioningit-onbuild\\]")
                 "[tool.hatch.build.hooks.versioningit-onbuild]
enable-by-default = false"))
              (mkdir-p "src/pyoncatqt")
              (call-with-output-file "src/pyoncatqt/_version.py"
                (lambda (port)
                  (format port "__version__ = \"~a\"~%" #$version))))))))
    (native-inputs
     (list python-hatchling))
    (propagated-inputs
     (list python-oauthlib
           python-pyoncat
           python-qtpy))
    (home-page "https://github.com/neutrons/pyoncatqt")
    (synopsis "Qt GUI elements for ONCat authentication")
    (description
     "This package provides common Qt GUI elements for authenticating with
ONCat (ORNL Neutron Catalog), including login dialogs and session management
widgets.")
    (license license:gpl3)))

(define-public python-pystog
  (package
    (name "python-pystog")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neutrons/pystog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dwljmyp083v5a189xzdxxsdkazh5bmbm2f2k79jp7lds0y8h9lg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-version
            (lambda _
              ;; versioningit needs git tags; patch pyproject.toml to use
              ;; static version and create _version.py directly.
              (substitute* "pyproject.toml"
                (("dynamic = \\[\"version\"\\]")
                 (string-append "version = \"" #$version "\""))
                (("source = \"versioningit\"") "")
                (("\\[tool\\.hatch\\.build\\.hooks\\.versioningit-onbuild\\]")
                 "[tool.hatch.build.hooks.versioningit-onbuild]
enable-by-default = false"))
              (mkdir-p "src/pystog")
              (call-with-output-file "src/pystog/_version.py"
                (lambda (port)
                  (format port "__version__ = \"~a\"~%" #$version))))))))
    (propagated-inputs
     (list python-h5py
           python-numpy))
    (native-inputs
     (list python-hatchling
           python-pytest))
    (home-page "https://github.com/neutrons/pystog")
    (synopsis "Total scattering function manipulator")
    (description
     "PyStoG is a Python package for converting between different total
scattering functions used in crystalline and amorphous materials research.
It handles reciprocal-space structure factors and real-space pair distribution
functions, performing Fourier transforms between them and applying filters to
remove spurious artifacts in the data.")
    (license license:gpl3+)))

(define-public python-quasielasticbayes
  (package
    (name "python-quasielasticbayes")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mantidproject/quasielasticbayes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05va9qygw4a9app61spw6hqmbn9cq09w0dik9g6xvzpwcmfb7yx4"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:imported-modules `((guix build python-build-system)
                           ,@%meson-build-system-modules)
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'check
            (lambda* (#:key tests? inputs outputs #:allow-other-keys)
              (when tests?
                (py:add-installed-pythonpath inputs outputs)
                (invoke "pytest" "../source/src/quasielasticbayes/test")))))))
    (native-inputs
     (list gfortran
           python
           python-numpy
           python-pytest))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/mantidproject/quasielasticbayes")
    (synopsis "Bayesian analysis for quasi-elastic neutron scattering")
    (description
     "This package provides Python wrappers for Fortran routines used to
perform Bayesian analysis on quasi-elastic neutron-scattering data.  The
original Fortran code was written by Dr. Devinder Sivia in the 1980s.")
    (license license:bsd-3)))

(define-public python-quickbayes
  (package
    (name "python-quickbayes")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "quickbayes" version))
       (sha256
        (base32 "1w3w612cz92bkxjk2wyfcjf502vgp45ajpz92llk1d0z8q1pdn49"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Upstream runs two test configurations (see .github/workflows/run_tests.yml):
      ;; - without gofit: test/default and test/shared
      ;; - with gofit: test/gofit and test/shared
      ;; Since we have gofit, run the gofit configuration.
      #~(list "test/gofit" "test/shared")))
    (propagated-inputs
     (list python-joblib python-numpy python-scipy))
    (native-inputs
     (list python-hatchling python-pytest python-gofit))
    (home-page "https://quickbayes.readthedocs.io/")
    (synopsis "Bayesian analysis tools for neutron scattering")
    (description
     "QuickBayes provides Bayesian analysis tools for analyzing neutron
scattering data.  It is designed for use with the Mantid framework for
neutron and muon data analysis.")
    (license license:bsd-3)))

(define-public python-shiver
  (package
    (name "python-shiver")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neutrons/Shiver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05bxn1wqqylixzkk4is9nqkcsxfiix46s8m18db4c1zj7kkjawwv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f  ; tests require mantid
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-version
            (lambda _
              ;; versioningit needs git tags; patch pyproject.toml to use
              ;; static version and create _version.py directly.
              (substitute* "pyproject.toml"
                (("dynamic = \\[\"version\"\\]")
                 (string-append "version = \"" #$version "\""))
                (("source = \"versioningit\"") "")
                (("\\[tool\\.hatch\\.build\\.hooks\\.versioningit-onbuild\\]")
                 "[tool.hatch.build.hooks.versioningit-onbuild]
enable-by-default = false"))
              (mkdir-p "src/shiver")
              (call-with-output-file "src/shiver/_version.py"
                (lambda (port)
                  (format port "__version__ = \"~a\"~%" #$version))))))))
    (native-inputs
     (list python-hatchling))
    (propagated-inputs
     (list python-configupdater
           python-pyoncatqt
           python-qtpy))
    (home-page "https://github.com/neutrons/Shiver")
    (synopsis "Spectroscopy histogram visualizer for neutron event reduction")
    (description
     "Shiver (Spectroscopy Histogram Visualizer for Event Reduction) is a
desktop application for examining Time of Flight inelastic neutron data from
single crystal, direct geometry experiments.  It integrates with Mantid
Workbench and appears in the Interfaces menu when both packages are
installed.")
    (license license:gpl3)))

(define-public python-spglib
  (package
    (name "python-spglib")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "spglib" version))
       (sha256
        (base32 "1sq8niay87n7hmby6hs628zzpc4apx6kp77cjvyi87hal0mxlvnn"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-numpy
           python-typing-extensions))
    (native-inputs
     (list cmake-minimal
           python-pytest
           python-pyyaml
           python-scikit-build-core
           python-setuptools-scm))
    (home-page "https://spglib.readthedocs.io/")
    (synopsis "Python bindings for spglib crystal symmetry library")
    (description
     "Spglib is a library for finding and handling crystal symmetries written
in C.  This package provides Python bindings for spglib, allowing Python
programs to find symmetry operations, identify space groups, and perform
other symmetry-related operations on crystal structures.")
    (license license:bsd-3)))

(define-public python-seekpath
  (package
    (name "python-seekpath")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "seekpath" version))
       (sha256
        (base32 "1i2jhjc4ikd31v8wkxzfrvhwlv0dlzpkysf3lkafcql2c9wwbkii"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "tests")))))))
    (propagated-inputs
     (list python-numpy
           python-scipy
           python-spglib))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (home-page "https://github.com/giovannipizzi/seekpath")
    (synopsis "K-path finder for band structure calculations")
    (description
     "SeeK-path is a Python module to obtain band paths in the Brillouin zone
of crystal structures.  It automatically detects Bravais lattice types and
generates k-point labels and band paths following crystallographic
conventions.")
    (license license:expat)))

(define-public python-euphonic
  (package
    (name "python-euphonic")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pace-neutrons/Euphonic.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18l5chzk6qhggxsgkqqidxx2nr4piziabvirw05v43kqm9awjfww"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-numpy-include
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((numpy (assoc-ref inputs "python-numpy"))
                     (site (site-packages inputs `(("out" . ,numpy)))))
                (substitute* "meson.build"
                  (("np_inc = include_directories\\(py\\.get_path\\('platlib'\\) / 'numpy/core/include'\\)")
                   (string-append "np_inc = include_directories('"
                                  numpy site "/numpy/core/include')"))))))
          (add-before 'check 'delete-source
            (lambda _
              (delete-file-recursively "euphonic"))))))
    (propagated-inputs
     (list python-brille ; optional
           python-h5py ; optional, for phonopy-reader
           python-matplotlib ; optional
           python-pyyaml ; optional, for phonopy-reader
           python-numpy
           python-scipy
           python-pint
           python-seekpath
           python-spglib
           python-threadpoolctl
           python-toolz))
    (native-inputs
     ;; Note: build-backend is mesonpy.
     (list ninja python-meson python-numpy python-packaging python-pytest
           python-pytest-lazy-fixtures python-pytest-mock
           pkg-config))
    (home-page "https://github.com/pace-neutrons/Euphonic")
    (synopsis "Phonon calculations for inelastic neutron scattering")
    (description
     "Euphonic is a Python package for calculating phonon bandstructures and
simulating inelastic neutron scattering (INS) from force constants.  It can
read output from CASTEP and Phonopy and calculate phonon frequencies,
eigenvectors, and structure factors.")
    (license license:gpl3+)))

(define-public mantid
  (package
    (name "mantid")
    (version "6.14.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mantidproject/mantid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d41wcwv8kpka086gqz3ywdbrrbkxpaxfq7hhgqf3w8f2n6jilw5"))
       (patches
        (search-patches "mantid-openmp-cleanup.patch"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:configure-flags
      #~(list "-DENABLE_MANTIDQT=ON"
              "-DENABLE_WORKBENCH=ON"
              "-DENABLE_DOCS=OFF"
              "-DENABLE_PRECOMMIT=OFF"
              "-DCMAKE_CXX_STANDARD=20"
              "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
              "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
              "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
              (string-append "-DCMAKE_PREFIX_PATH="
                             #$(this-package-input "qtbase"))
              (string-append "-DPYQT5_SIP_DIR="
                             #$(this-package-input "python-pyqt")
                             "/lib/python3.11/site-packages/PyQt5/bindings"))
      #:modules '((guix build qt-build-system)
                  (guix build utils)
                  (ice-9 textual-ports))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-boost-system
            (lambda _
              ;; Enable core dumps.
              (setrlimit 'core #f #f)
              ;; boost_system is now header-only in modern boost (>= 1.69)
              ;; and not a separate cmake target. Remove from COMPONENTS.
              ;; Also disable HDF4 support as Guix doesn't have an hdf4 package
              ;; anymore.
              (substitute* "buildconfig/CMake/CommonSetup.cmake"
                (("date_time regex serialization filesystem system")
                 "date_time regex serialization filesystem")
                (("find_package\\(HDF4 REQUIRED\\)")
                 "# HDF4 disabled - no hdf4 package in Guix"))
              (substitute* "Framework/LegacyNexus/CMakeLists.txt"
                (("\\$\\{HDF4_DF_LIBRARY\\} \\$\\{HDF4_MFHDF_LIBRARY\\} ")
                 ""))
              (substitute* "Framework/LegacyNexus/inc/MantidLegacyNexus/napiconfig.h"
                (("#define WITH_HDF4")
                 "/* #define WITH_HDF4 - disabled, no hdf4 in Guix */"))))
          (add-after 'unpack 'fix-version-detection
            (lambda _
              ;; The cmake build uses versioningit with git to determine the
              ;; version. Since git is not available during build, we create
              ;; a simple cmake file that sets the version directly.
              (call-with-output-file "buildconfig/CMake/VersionNumber.cmake"
                (lambda (port)
                  (format port "# Version information - patched for Guix build
  set(VERSION_MAJOR ~a)
  set(VERSION_MINOR ~a)
  set(VERSION_PATCH ~a)
  set(VERSION_TWEAK \"\")
  set(REVISION_FULL \"v~a\")
  set(REVISION_SHORT \"v~a\")
  set(REVISION_DATE \"Thu, 06 Nov 2025\")
  message(STATUS \"Version: ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}${VERSION_TWEAK}\")
  "
                          #$(version-major version)
                          #$(cadr (string-split version #\.))
                          #$(string-drop version (+ 1 (string-length (version-major+minor version))))
                          #$version
                          #$version)))))
          (add-after 'unpack 'use-system-googletest
            (lambda _
              ;; The cmake build tries to fetch googletest from git.
              ;; We use the system googletest instead.
              ;; Also include CMakePackageConfigHelpers and GNUInstallDirs
              ;; which are needed by Framework/CMakeLists.txt.
              (with-output-to-file "buildconfig/CMake/GoogleTest.cmake"
                (lambda ()
                  (display "# Use system googletest
  include(CMakePackageConfigHelpers)
  include(GNUInstallDirs)

  find_package(GTest REQUIRED)
  # Create aliases that mantid expects
  if(NOT TARGET gmock)
    add_library(gmock ALIAS GTest::gmock)
  endif()
  if(NOT TARGET gtest)
    add_library(gtest ALIAS GTest::gtest)
  endif()
  if(NOT TARGET gmock_main)
    add_library(gmock_main ALIAS GTest::gmock_main)
  endif()
  if(NOT TARGET gtest_main)
    add_library(gtest_main ALIAS GTest::gtest_main)
  endif()
  ")))))
          (add-after 'unpack 'fix-sip-include-dirs
            (lambda* (#:key inputs #:allow-other-keys)
              ;; sip-build needs to find PyQt5's .sip files for %Import directives.
              ;; Add sip-include-dirs to the pyproject.toml template.
              (let ((pyqt-sip-dir (string-append (assoc-ref inputs "python-pyqt")
                                                  "/lib/python3.11/site-packages/PyQt5/bindings")))
                (substitute* "buildconfig/CMake/sip-templates/pyproject.toml.in"
                  (("\\[tool\\.sip\\.project\\]")
                   (string-append "[tool.sip.project]\n"
                                  "sip-include-dirs = [\"" pyqt-sip-dir "\"]"))))))
          (add-after 'unpack 'fix-pip-no-network
            (lambda _
              ;; The build-phase editable install is only for developer convenience
              ;; (creates egg-links so devs can import from build dir).
              ;; Skip it entirely - they will just touch the stamp file.
              ;;
              ;; The install-phase pip install still runs and does the real work.
              (substitute* "buildconfig/CMake/PythonPackageTargetFunctions.cmake"
                (("\\$\\{Python_EXECUTABLE\\} -m pip install --editable \\.")
                 "${CMAKE_COMMAND} -E true")
                (("pip install \\$\\{CMAKE_CURRENT_SOURCE_DIR\\}")
                 "pip install --no-build-isolation --prefix ${CMAKE_INSTALL_PREFIX} ${CMAKE_CURRENT_SOURCE_DIR}"))))
          ;; Remove dependencies on UnitTestData (requires network to download).
          ;; Tests that need external data will be excluded from ctest.
          (add-after 'unpack 'remove-external-data-dependencies
            (lambda _
              (substitute* (find-files "." "CMakeLists\\.txt$")
                (("add_dependencies\\([A-Za-z]+Test UnitTestData\\)") ""))))
          (add-after 'unpack 'fix-rpath-for-guix
            (lambda _
              ;; Mantid sets INSTALL_RPATH explicitly per-target, which overrides
              ;; CMAKE_INSTALL_RPATH_USE_LINK_PATH.  Rename the property to a
              ;; non-existent name so CMake ignores it and uses the global setting.
              ;;
              ;; The pattern "PROPERTIES INSTALL_RPATH" only matches the property
              ;; name in set_target_properties() calls, not:
              ;; - CMAKE_INSTALL_RPATH (variable name)
              ;; - CMAKE_INSTALL_RPATH_USE_LINK_PATH (variable name)
              ;; - LINUX_INSTALL_RPATH (function keyword parameter)
              ;; - PARSED_LINUX_INSTALL_RPATH (parsed argument variable)
              (substitute* (append (filter (lambda (f)
                                              (not (string-contains f "PythonInterface")))
                                            (find-files "." "CMakeLists\\.txt$"))
                                   (find-files "buildconfig/CMake" "\\.cmake$"))
                (("PROPERTIES INSTALL_RPATH")
                 "PROPERTIES DISABLED_INSTALL_RPATH"))
              ;; Fix EXT_INSTALL_RPATH for Python modules - the relative path
              ;; is totally invalid.
              (substitute* "Framework/PythonInterface/CMakeLists.txt"
                (("set\\(EXT_INSTALL_RPATH [^)]*\\)")
                 (string-append "set(EXT_INSTALL_RPATH \"" #$output "/lib;" #$output "/plugins\")")))))
          (add-after 'unpack 'remove-disable-new-dtags
            (lambda _
              ;; Remove the --disable-new-dtags flag--so it uses modern RUNPATH
              ;; instead of RPATH.
              (substitute* "buildconfig/CMake/GNUSetup.cmake"
                (("-Wl,--disable-new-dtags")
                 ""))
              (substitute* "qt/python/mantidqt/mantidqt/widgets/codeeditor/test/test_codeeditor.py"
                (("exc\\.__name__") "type(exc).__name__"))
              ;; np.all on tuple doesn't compare - it just checks truthiness.
              (substitute* "scripts/test/Engineering/texture/test_TextureUtils.py"
                (("self\\.assertTrue\\(np\\.all\\(\\(_get_run_and_prefix_from_ws_log\\(mock_ws, ws_name\\), \\(run_number, prefix\\)\\)\\)\\)")
                 "self.assertEqual(_get_run_and_prefix_from_ws_log(mock_ws, ws_name), (run_number, prefix))"))
              (substitute* "scripts/test/Engineering/common/test_texture_sample_viewer.py"
                (("self\\.assertTrue\\(np\\.all\\(\\(mesh\\.shape, \\(12, 3, 3\\)\\)\\)\\)")
                 "self.assertEqual(mesh.shape, (12, 3, 3))")
                (("self\\.assertTrue\\(np\\.all\\(\\(np\\.unique\\(mesh\\), \\(-1, 1\\)\\)\\)\\)")
                 "self.assertTrue(np.array_equal(np.unique(mesh), np.array([-1, 1])))")
                (("self\\.assertTrue\\(np\\.all\\(\\(mesh, self\\.get_cube_mesh\\(2\\)\\)\\)\\)")
                 "self.assertTrue(np.array_equal(mesh, self.get_cube_mesh(2)))"))
              (substitute*
"qt/python/mantidqtinterfaces/mantidqtinterfaces/Engineering/gui/engineering_diffraction/tabs/texture/test/test_texture_presenter.py"
                (("self\\.assertTrue\\(np\\.all\\(\\(self\\.presenter\\.ws_names, \\[\"existing_ws\", \"new_ws\"\\]\\)\\)\\)")
                 "self.assertEqual(self.presenter.ws_names, [\"existing_ws\", \"new_ws\"])")
                (("self\\.assertTrue\\(np\\.all\\(\\(self\\.presenter\\.ws_names, \\[\"ws2\"\\]\\)\\)\\)")
                 "self.assertEqual(self.presenter.ws_names, [\"ws2\"])")
                (("self\\.assertTrue\\(np\\.all\\(\\(self\\.presenter\\.get_assigned_params\\(\\), \\[\"param2\"\\]\\)\\)\\)")
                 "self.assertEqual(self.presenter.get_assigned_params(), [\"param2\"])"))
              ;; np.xrange never existed; use range() for index iteration.
              (substitute* "scripts/SCD_Reduction/BVGFitTools.py"
                (("np\\.xrange") "range"))
              ;; set_draggable fails with offscreen backend (KeyError: 'pick_event').
              (substitute* "Framework/PythonInterface/mantid/plots/utility.py"
                (("getattr\\(legend, SET_DRAGGABLE_METHOD\\)\\(state, use_blit, update\\)")
                 "try:\n        getattr(legend, SET_DRAGGABLE_METHOD)(state, use_blit, update)\n    except KeyError:\n        pass  # Offscreen backend doesn't support draggable legends"))
              ;; legendTest.py imports matplotlib.pyplot without setting Agg backend,
              ;; causing Qt5Agg to fail display_is_valid() check.
              (substitute* "Framework/PythonInterface/test/python/mantid/plots/legendTest.py"
                (("import matplotlib\\.pyplot as plt")
                 "import matplotlib
matplotlib.use(\"AGG\")
import matplotlib.pyplot as plt"))
              ;; tostring_rgb() was removed in matplotlib 3.9; use buffer_rgba().
              ;; buffer_rgba() returns RGBA (4 channels) instead of RGB (3).
              (substitute* "Framework/PythonInterface/test/python/mantid/plots/modest_image/test_modest_image.py"
                (("\\.tostring_rgb\\(\\)") ".buffer_rgba()")
                (("np\\.fromstring\\(") "np.frombuffer(")
                (("np\\.frombuffer\\(([^)]*), sep=\"\"\\)" all args)
                 (string-append "np.frombuffer(" args ")"))
                (("get_width_height\\(\\)\\[::-1\\] \\+ \\(3,\\)")
                 "get_width_height()[::-1] + (4,)"))
              ;; matplotlib 3.10 changed container tracking; container may
              ;; already be removed when mantid tries to remove it.
              (substitute* "Framework/PythonInterface/mantid/plots/mantidaxes.py"
                (("self\\.containers\\.remove\\(artist\\)")
                 "self.containers.remove(artist) if artist in self.containers else None"))
              (substitute* "qt/python/mantidqt/mantidqt/widgets/plotconfigdialog/curvestabwidget/__init__.py"
                (("ax\\.containers\\.remove\\(curve\\)")
                 "ax.containers.remove(curve) if curve in ax.containers else None"))))
          (add-after 'qt-wrap 'unwrap-non-executables
            (lambda _
              ;; qt-wrap wrapped these files thinking they were executables.
              ;; Restore them so mantid can read them as config/data files.
              (for-each
               (lambda (file)
                 (let ((wrapped (string-append #$output "/bin/" file))
                       (real (string-append #$output "/bin/." file "-real")))
                   (when (file-exists? real)
                     (delete-file wrapped)
                     (rename-file real wrapped))))
               '("Mantid.properties" "mantid-scripts.pth"))))
          (add-after 'unwrap-non-executables 'wrap-python
            (lambda _
              (let* ((python-version #$(version-major+minor (package-version python)))
                     (bin-dir (string-append #$output "/bin"))
                     (site-packages (string-append #$output "/lib/python"
                                                   python-version "/site-packages")))
                (for-each
                 (lambda (prog)
                   (wrap-program (string-append bin-dir "/" prog)
                     `("GUIX_PYTHONPATH" prefix
                       (,site-packages ,(getenv "GUIX_PYTHONPATH")))
                     `("PYTHONPATH" prefix (,bin-dir))
                     `("FORCE_QT_API" = ("PyQt5"))))
                 '("launch_mantidworkbench" "workbench" "instrumentview")))))
          ;; Build test executables (they are EXCLUDE_FROM_ALL in CMake).
          (add-before 'check 'build-tests
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (let ((job-count (if parallel-build?
                                   (number->string (parallel-job-count))
                                   "1")))
                (invoke "cmake" "--build" "." "--target" "AllTests"
                        "-j" job-count))))
          ;; Fix PYTHONPATH in test configurations.  PyUnitTest.cmake bakes
          ;; $ENV{PYTHONPATH} at configure time (empty), so tests can't find
          ;; modules.  Also, we disable mantid's sitecustomize.py (which would
          ;; shadow Guix's), so we need to add all paths it would have added.
          (add-before 'check 'fix-test-pythonpath
            (lambda _
              (let* ((source-dir (string-append (dirname (getcwd)) "/source"))
                     (build-dir (getcwd))
                     ;; Core mantid module path (would have been in mantid.pth
                     ;; if pip --editable was used, but we disabled that).
                     (mantid-path (string-append source-dir "/Framework/PythonInterface"))
                     ;; Read paths from .pth files generated by mantid's build
                     (pth-files '("bin/mantid-scripts.pth"
                                  "bin/testhelpers.pth"))
                     (pth-paths
                      (apply append
                             (map (lambda (pth-file)
                                    (if (file-exists? pth-file)
                                        (string-split
                                         (string-trim-right
                                          (call-with-input-file pth-file
                                            get-string-all))
                                         #\newline)
                                        '()))
                                  pth-files)))
                     ;; Also add Qt Python packages (not in .pth files)
                     (qt-paths (list (string-append source-dir "/qt/python/mantidqt")
                                     (string-append source-dir "/qt/python/mantidqtinterfaces")
                                     (string-append source-dir "/qt/python/instrumentview")))
                     ;; Add workbench module path for tests that import from workbench
                     (workbench-path (string-append source-dir "/qt/applications/workbench"))
                     (all-paths (string-join (cons mantid-path (append pth-paths qt-paths (list workbench-path))) ":")))
                (substitute* (find-files "." "CTestTestfile\\.cmake$")
                  ;; Prepend paths to tests that already have PYTHONPATH.
                  (("PYTHONPATH=")
                   (string-append "PYTHONPATH=" all-paths ":"))
                  ;; Add PYTHONPATH to C++ tests that don't have ENVIRONMENT.
                  ;; FindCxxTest.cmake only sets ENVIRONMENT on WIN32.
                  ;; Match "PROPERTIES  LABELS" (no ENVIRONMENT) to avoid
                  ;; duplicating on "PROPERTIES  ENVIRONMENT" lines.
                  (("PROPERTIES +LABELS")
                   (string-append "PROPERTIES ENVIRONMENT \"PYTHONPATH="
                                  all-paths "\" LABELS"))))
              ;; Disable mantid's sitecustomize.py - it shadows Guix's and
              ;; prevents GUIX_PYTHONPATH from being processed.
              (rename-file "bin/sitecustomize.py" "bin/sitecustomize.py.bak")))
          ;; Set HOME for tests - mantid's ConfigService needs to create
          ;; files in ~/.mantid during initialization.  Create the directory
          ;; upfront to avoid race conditions when tests run in parallel.
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp")
              (mkdir-p "/tmp/.mantid")))
          ;; Start Xvfb for tests that need a display.  Matplotlib's
          ;; Qt5Agg backend requires a valid X11 or Wayland display
          ;; connection because matplotlib checks XOpenDisplay() or
          ;; wl_display_connect() before allowing Qt backends, even
          ;; though Qt itself works fine with QT_QPA_PLATFORM=offscreen.
          ;; This is a matplotlib design decision to prevent Qt from
          ;; crashing when DISPLAY is set but invalid - but it also
          ;; blocks legitimate headless use.
          ;;
          ;; We also force Qt5Agg backend via MPLBACKEND because several
          ;; tests import matplotlib.pyplot at module level before
          ;; @start_qapplication creates a QApplication.  When pyplot is
          ;; imported without a running Qt, it auto-selects TkAgg (the
          ;; default in Guix's matplotlib).  Later when Qt starts and the
          ;; test calls plt.subplots(), matplotlib tries to use TkAgg but
          ;; fails with "Cannot load backend 'TkAgg' which requires the
          ;; 'tk' interactive framework, as 'qt' is currently running".
          ;; Affected tests:
          ;;   - workbench/plotting/test/test_figuremanager.py
          ;;   - mantidqt/widgets/colorbar/test/test_colorbar.py
          ;;   - instrumentview/test/test_view.py (via FullInstrumentViewWindow)
          (add-before 'check 'prepare-x
            (lambda _
              (system "Xvfb :99 -screen 0 1024x768x24 &")
              (setenv "DISPLAY" ":99")
              (setenv "MPLBACKEND" "Qt5Agg")))
          ;; Exclude tests that require external data files (not available
          ;; in the build sandbox without network access).  We exclude
          ;; specific tests that use FileFinder/Load to access external
          ;; data rather than whole modules.
          (replace 'check
            (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
              (when tests?
                (setenv "OMP_NUM_THREADS" "10")
                (setenv "OPENBLAS_NUM_THREADS" "10")
                ;; They forgot to omp_pause_resource(omp_pause_soft, 0)
                ;; at the end of the test suite--and now random tests
                ;; were caught in a death race.
                (setenv "MALLOC_CONF" "tcache:false") ; doesn't work
                ;(setenv "OMP_WAIT_POLICY" "passive")
                ;; Exclude unit tests that need external data or have
                ;; other issues. System tests are excluded via -L UnitTest.
                (let ((exclude-pattern
                       (string-join
                        '(;;; C++ Kernel tests needing external data or
                          ;;; system info not available in sandbox.
                          "KernelTest_FileDescriptorTest" ; needs external data
                          "KernelTest_LegacyNexusDescriptorTest" ; needs external data
                          "KernelTest_AttenuationProfileTest" ; needs external data
                          "KernelTest_ConfigServiceTest" ; empty username and OS version in sandbox
                          ;;; C++ API tests needing external data.
                          "APITest_ScriptBuilderTest" ; needs external data
                          "APITest_ExperimentInfoTest" ; needs external data
                          "APITest_FileBackedExperimentInfoTest" ; needs external data
                          "APITest_FileFinderTest" ; needs external data
                          "APITest_FileLoaderRegistryTest" ; needs external data
                          "APITest_FilePropertyTest" ; needs external data
                          "APITest_GroupingLoaderTest" ; needs external data
                          "APITest_WorkspaceHistoryIOTest" ; needs external data
                          ;;; C++ Algorithms tests needing external data.
                          "AlgorithmsTest_AppendSpectraTest" ; needs external data
                          "AlgorithmsTest_ApplyCalibrationTest" ; needs external data
                          "AlgorithmsTest_ApplyTransmissionCorrectionTest" ; needs external data
                          "AlgorithmsTest_CalculateTransmissionTest" ; needs external data
                          "AlgorithmsTest_ChangeBinOffsetTest" ; needs external data
                          "AlgorithmsTest_CloneWorkspaceTest" ; needs external data
                          "AlgorithmsTest_ConjoinWorkspacesTest" ; needs external data
                          "AlgorithmsTest_ConvertSpectrumAxisTest" ; needs external data
                          "AlgorithmsTest_CorrectKiKfTest" ; needs external data
                          "AlgorithmsTest_CorrectToFileTest" ; needs external data
                          "AlgorithmsTest_CropToComponentTest" ; needs external data
                          "AlgorithmsTest_DiffractionFocussing2Test" ; needs external data
                          "AlgorithmsTest_DiffractionFocussingTest" ; needs external data
                          "AlgorithmsTest_ExtractFFTSpectrumTest" ; needs external data
                          "AlgorithmsTest_FFTSmooth2Test" ; needs external data
                          "AlgorithmsTest_FilterBadPulsesTest" ; needs external data
                          "AlgorithmsTest_FindCenterOfMassPosition2Test" ; needs external data
                          "AlgorithmsTest_FindPeaksConvolveTest" ; needs external data
                          "AlgorithmsTest_FindPeaksTest" ; needs external data
                          "AlgorithmsTest_FitPeaksTest" ; needs external data
                          "AlgorithmsTest_GetEiTest" ; needs external data
                          "AlgorithmsTest_IdentifyNoisyDetectorsTest" ; needs external data
                          "AlgorithmsTest_MultiplyRangeTest" ; needs external data
                          "AlgorithmsTest_Q1D2Test" ; needs external data
                          "AlgorithmsTest_Q1DWeightedTest" ; needs external data
                          "AlgorithmsTest_QxyTest" ; needs external data
                          "AlgorithmsTest_RebinRagged2Test" ; needs external data
                          "AlgorithmsTest_RemoveSpectraTest" ; needs external data
                          "AlgorithmsTest_SofQWCentreTest" ; needs external data (via SofQWTest.h)
                          "AlgorithmsTest_SofQWCutTest" ; needs external data
                          "AlgorithmsTest_SofQWNormalisedPolygonTest" ; needs external data (via SofQWTest.h)
                          "AlgorithmsTest_SofQWPolygonTest" ; needs external data (via SofQWTest.h)
                          "AlgorithmsTest_SofQWTest" ; needs external data
                          "AlgorithmsTest_StripVanadiumPeaks2Test" ; needs external data
                          "AlgorithmsTest_StripVanadiumPeaksTest" ; needs external data
                          "AlgorithmsTest_SumEventsByLogValueTest" ; needs external data
                          "AlgorithmsTest_SumOverlappingTubesTest" ; needs external data
                          "AlgorithmsTest_TransposeTest" ; needs external data
                          "AlgorithmsTest_WeightedMeanTest" ; needs external data
                          "AlgorithmsTest_CreateGroupingWorkspaceTest" ; needs external data
                          "AlgorithmsTest_EstimateResolutionDiffractionTest" ; needs external data
                          "AlgorithmsTest_MaskDetectorsIfTest" ; excluded: unknown reason
                          "AlgorithmsTest_VesuvioL1ThetaResolutionTest" ; excluded: unknown reason
                          ;;; C++ Crystal tests needing external data.
                          "CrystalTest_CentroidPeaksTest" ; needs external data
                          "CrystalTest_FindUBUsingFFTTest" ; needs external data
                          "CrystalTest_FindUBUsingIndexedPeaksTest" ; needs external data
                          "CrystalTest_FindUBUsingLatticeParametersTest" ; needs external data
                          "CrystalTest_GoniometerAnglesFromPhiRotationTest" ; needs external data
                          "CrystalTest_IndexSXPeaksTest" ; needs external data
                          "CrystalTest_LoadIsawPeaksTest" ; needs external data
                          "CrystalTest_LoadIsawUBTest" ; needs external data
                          "CrystalTest_OptimizeCrystalPlacementTest" ; needs external data
                          "CrystalTest_OptimizeLatticeForCellTypeTest" ; needs external data
                          "CrystalTest_PeakHKLErrorsTest" ; needs external data
                          "CrystalTest_PeakIntegrationTest" ; needs external data
                          "CrystalTest_PredictFractionalPeaksTest" ; needs external data
                          "CrystalTest_PredictSatellitePeaksTest" ; needs external data
                          "CrystalTest_SCDCalibratePanels2ObjFuncTest" ; needs external data
                          "CrystalTest_SCDCalibratePanels2Test" ; needs external data
                          "CrystalTest_SCDCalibratePanelsTest" ; needs external data
                          "CrystalTest_SaveIsawPeaksTest" ; needs external data
                          "CrystalTest_SaveIsawUBTest" ; needs external data
                          "CrystalTest_SaveLauenormTest" ; needs external data
                          "CrystalTest_SelectCellOfTypeTest" ; needs external data
                          "CrystalTest_SelectCellWithFormTest" ; needs external data
                          "CrystalTest_SetCrystalLocationTest" ; needs external data
                          "CrystalTest_ShowPeakHKLOffsetsTest" ; needs external data
                          "CrystalTest_ShowPossibleCellsTest" ; needs external data
                          "CrystalTest_TransformHKLTest" ; needs external data
                          "CrystalTest_LoadIsawSpectrumTest" ; needs external data
                          ;;; C++ CurveFitting tests needing external data.
                          "CurveFittingTest_ConvolutionFitSequentialTest" ; needs external data
                          "CurveFittingTest_FitPowderDiffPeaksTest" ; needs external data
                          "CurveFittingTest_FunctionQDependsTest" ; needs external data
                          "CurveFittingTest_PlotPeakByLogValueHelperTest" ; needs external data
                          "CurveFittingTest_ProfileChiSquared1DTest" ; needs external data
                          "CurveFittingTest_TabulatedFunctionTest" ; needs external data
                          ;;; C++ DataHandling tests.
                          ;;; Most need external data files not available in
                          ;;; the build sandbox.  Additionally, ALL tests share
                          ;;; one binary (DataHandlingTest), and Mantid's
                          ;;; Strings::randomString() (Strings.cpp) calls
                          ;;; rand() without srand(), so every parallel ctest
                          ;;; process generates identical temp filenames.
                          ;;; Under ctest -j N, parallel DataHandlingTest
                          ;;; processes race on these files (ENOENT from
                          ;;; prepareFile() deleting another process's file,
                          ;;; EAGAIN from HDF5 fcntl locks).
                          "DataHandlingTest_AlignAndFocusPowderSlimTest" ; needs external data
                          "DataHandlingTest_DetermineChunkingTest" ; needs external data
                          "DataHandlingTest_GroupDetectors2Test" ; needs external data
                          "DataHandlingTest_LoadBBY2Test" ; needs external data
                          "DataHandlingTest_LoadBBYTest" ; needs external data
                          "DataHandlingTest_LoadCSNSNexusTest" ; needs external data
                          "DataHandlingTest_LoadCalFileTest" ; needs external data
                          "DataHandlingTest_LoadCanSAS1dTest" ; needs external data
                          "DataHandlingTest_LoadDNSEventTest" ; needs external data
                          "DataHandlingTest_LoadDaveGrpTest" ; needs external data
                          "DataHandlingTest_LoadDetectorInfoTest" ; needs external data
                          "DataHandlingTest_LoadDetectorsGroupingFileTest" ; needs external data
                          "DataHandlingTest_LoadEMUauTest" ; needs external data
                          "DataHandlingTest_LoadErrorEventsNexusTest" ; needs external data
                          "DataHandlingTest_LoadEventNexusTest" ; needs external data
                          "DataHandlingTest_LoadEventPreNexus2Test" ; needs external data
                          "DataHandlingTest_LoadFITSTest" ; needs external data
                          "DataHandlingTest_LoadGSSTest" ; needs external data
                          "DataHandlingTest_LoadHFIRSANSTest" ; needs external data
                          "DataHandlingTest_LoadIDFFromNexusTest" ; needs external data
                          "DataHandlingTest_LoadILLDiffractionTest" ; needs external data
                          "DataHandlingTest_LoadILLIndirect2Test" ; needs external data
                          "DataHandlingTest_LoadILLLagrangeTest" ; needs external data
                          "DataHandlingTest_LoadILLPolarizationFactorsTest" ; needs external data
                          "DataHandlingTest_LoadILLPolarizedDiffractionTest" ; needs external data
                          "DataHandlingTest_LoadILLReflectometryTest" ; needs external data
                          "DataHandlingTest_LoadILLSALSATest" ; needs external data
                          "DataHandlingTest_LoadILLSANSTest" ; needs external data
                          "DataHandlingTest_LoadILLTOF2Test" ; needs external data
                          "DataHandlingTest_LoadILLTOF3Test" ; needs external data
                          "DataHandlingTest_LoadILLTest" ; needs external data
                          "DataHandlingTest_LoadISISNexusTest" ; needs external data
                          "DataHandlingTest_LoadInstrumentFromRawTest" ; needs external data
                          "DataHandlingTest_LoadIsawDetCalTest" ; needs external data
                          "DataHandlingTest_LoadLogTest" ; needs external data
                          "DataHandlingTest_LoadMLZTest" ; needs external data
                          "DataHandlingTest_LoadMappingTableTest" ; needs external data
                          "DataHandlingTest_LoadMaskTest" ; needs external data
                          "DataHandlingTest_LoadMcStasNexusTest" ; needs external data
                          "DataHandlingTest_LoadMcStasTest" ; needs external data
                          "DataHandlingTest_LoadMuonNexusV2Test" ; needs external data
                          "DataHandlingTest_LoadNGEMTest" ; needs external data
                          "DataHandlingTest_LoadNXSPETest" ; needs external data
                          "DataHandlingTest_LoadNXcanSASTest" ; needs external data
                          "DataHandlingTest_LoadNexusLogsTest" ; needs external data
                          "DataHandlingTest_LoadNexusMonitorsTest" ; needs external data
                          "DataHandlingTest_LoadNexusProcessedTest" ; needs external data
                          "DataHandlingTest_LoadNexusTest" ; needs external data
                          "DataHandlingTest_LoadPDFgetNFileTest" ; needs external data
                          "DataHandlingTest_LoadPLNTest" ; needs external data
                          "DataHandlingTest_LoadPreNexusMonitorsTest" ; needs external data
                          "DataHandlingTest_LoadPreNexusTest" ; needs external data
                          "DataHandlingTest_LoadQKKTest" ; needs external data
                          "DataHandlingTest_LoadRKHTest" ; needs external data
                          "DataHandlingTest_LoadRawBin0Test" ; needs external data
                          "DataHandlingTest_LoadRawSaveNxsLoadNxsTest" ; needs external data
                          "DataHandlingTest_LoadRawSpectrum0Test" ; needs external data
                          "DataHandlingTest_LoadSINQFocusTest" ; needs external data
                          "DataHandlingTest_LoadSPETest" ; needs external data
                          "DataHandlingTest_LoadSampleDetailsFromRawTest" ; needs external data
                          "DataHandlingTest_LoadSampleShapeTest" ; needs external data
                          "DataHandlingTest_LoadSassenaTest" ; needs external data
                          "DataHandlingTest_LoadSpecTest" ; needs external data
                          "DataHandlingTest_LoadSpice2dTest" ; needs external data
                          "DataHandlingTest_LoadSpiceAsciiTest" ; needs external data
                          "DataHandlingTest_LoadSpiceXML2DDetTest" ; needs external data
                          "DataHandlingTest_LoadSwansTest" ; needs external data
                          "DataHandlingTest_LoadTOFRawNexusTest" ; needs external data
                          "DataHandlingTest_LoadTest" ; needs external data
                          "DataHandlingTest_ModifyDetectorDotDatFileTest" ; needs external data
                          "DataHandlingTest_PDLoadCharacterizationsTest" ; needs external data
                          "DataHandlingTest_RawFileInfoTest" ; needs external data
                          "DataHandlingTest_SNSAppendGeometryToNexusTest" ; needs external data
                          "DataHandlingTest_SaveBankScatteringAnglesTest" ; needs external data
                          "DataHandlingTest_SaveCanSAS1dTest" ; needs external data
                          "DataHandlingTest_SaveCanSAS1dTest2" ; needs external data
                          "DataHandlingTest_SaveDaveGrpTest" ; needs external data
                          "DataHandlingTest_SaveDetectorsGroupingTest" ; needs external data
                          "DataHandlingTest_SaveGSASInstrumentFileTest" ; needs external data
                          "DataHandlingTest_SaveMaskTest" ; needs external data
                          "DataHandlingTest_SaveNISTDATTest" ; needs external data
                          "DataHandlingTest_SaveNexusTest" ; needs external data
                          "DataHandlingTest_SaveOpenGenieAsciiTest" ; needs external data
                          "DataHandlingTest_SavePDFGuiTest" ; needs external data
                          "DataHandlingTest_SaveRMCProfileTest" ; needs external data
                          "DataHandlingTest_CreateSimulationWorkspaceTest" ; needs external data
                          "DataHandlingTest_GenerateGroupingPowder2Test" ; needs external data
                          "DataHandlingTest_GenerateGroupingPowderTest" ; needs external data
                          "DataHandlingTest_LoadAsciiStlTest" ; needs external data
                          "DataHandlingTest_LoadBinaryStlTest" ; needs external data
                          "DataHandlingTest_LoadEmptyInstrumentTest" ; needs external data
                          "DataHandlingTest_LoadEventAsWorkspace2DTest" ; needs external data
                          "DataHandlingTest_LoadNexusProcessed2Test" ; needs external data
                          "DataHandlingTest_LoadPSIMuonBinTest" ; needs external data
                          "DataHandlingTest_LoadRaw3Test" ; needs external data
                          "DataHandlingTest_LoadSESANSTest" ; needs external data
                          "DataHandlingTest_LoadSampleEnvironmentTest" ; needs external data
                          "DataHandlingTest_MeshFileIOTest" ; needs external data
                          "DataHandlingTest_SampleEnvironmentSpecParserTest" ; needs external data
                          "DataHandlingTest_SaveGSSTest" ; needs external data
                          "DataHandlingTest_SaveNexusESSTest" ; needs external data
                          "DataHandlingTest_SaveNexusProcessedTest" ; needs external data
                          "DataHandlingTest_SaveReflectometryAsciiTest" ; needs external data
                          "DataHandlingTest_SaveStlTest" ; needs external data
                          "DataHandlingTest_SetScalingPSDTest" ; needs external data
                          "DataHandlingTest_StartAndEndTimeFromNexusFileExtractorTest" ; needs external data
                          "DataHandlingTest_UpdateInstrumentFromFileTest" ; needs external data
                          "DataHandlingTest_XMLInstrumentParameterTest" ; needs external data
                          "DataHandlingTest_ProcessBankSplitFullTimeTaskTest" ; needs external data
                          "DataHandlingTest_LoadNXcanSASPerformanceTest2D" ; rand() temp file race
                          "DataHandlingTest_SaveNXcanSASTest" ; rand() temp file race
                          "DataHandlingTest_SavePolarizedNXcanSASTest" ; rand() temp file race
                          "LegacyNexusTest_LegacyNeXusFileLeakTest" ; needs external data
                          "LegacyNexusTest_LegacyNeXusFileNapiTest" ; needs external data
                          "LegacyNexusTest_LegacyNeXusFileReadTest" ; needs external data
                          "LegacyNexusTest_LegacyNeXusFileTest" ; needs external data
                          "LiveDataTest_FileEventDataListenerTest" ; needs external data
                          "MDAlgorithmsTest_ConvertCWPDMDToSpectraTest" ; needs external data
                          "MDAlgorithmsTest_ConvertCWSDExpToMomentumTest" ; needs external data
                          "MDAlgorithmsTest_ConvertHFIRSCDtoMDETest" ; needs external data
                          "MDAlgorithmsTest_ConvertSpiceDataToRealSpaceTest" ; needs external data
                          "MDAlgorithmsTest_GetSpiceDataRawCountsFromMDTest" ; needs external data
                          "MDAlgorithmsTest_IntegrateEllipsoidsTest" ; needs external data
                          "MDAlgorithmsTest_LoadDNSSCDTest" ; needs external data
                          "MDAlgorithmsTest_LoadGaussCubeTest" ; needs external data
                          "MDAlgorithmsTest_LoadMDTest" ; needs external data
                          "MDAlgorithmsTest_LoadSQW2Test" ; needs external data
                          "MDAlgorithmsTest_LoadSQWTest" ; needs external data
                          "MantidQtInterfacesDirectTestQt5_ALFInstrumentModelTest" ; needs external data
                          "MantidQtInterfacesIndirectTestQt5_ISISEnergyTransferModelTest" ; needs external data
                          "MantidQtInterfacesIndirectTestQt5_ISISEnergyTransferModelUtilsTest" ; needs external data
                          "MantidQtInterfacesInelasticTestQt5_ElwinModelTest" ; needs external data
                          "MantidQtInterfacesMuonTestQt5_ALCDataLoadingPresenterTest" ; needs external data
                          "MantidQtWidgetsCommonTestQt5_FindFilesWorkerTest" ; needs external data
                          "MantidScientificInterfacesISISReflectometryTestQt5_DecoderTest" ; needs external data
                          "MuonTest_AlphaCalcTest" ; needs external data
                          "MuonTest_ApplyDeadTimeCorrTest" ; needs external data
                          "MuonTest_AsymmetryCalcTest" ; needs external data
                          "MuonTest_CalMuonDeadTimeTest" ; needs external data
                          "MuonTest_LoadInstrumentFromNexusTest" ; needs external data
                          "MuonTest_LoadMuonLogTest" ; needs external data
                          "MuonTest_LoadMuonNexus1Test" ; needs external data
                          "MuonTest_LoadMuonNexus2Test" ; needs external data
                          "MuonTest_LoadMuonNexus3Test" ; needs external data
                          "MuonTest_PhaseQuadMuonTest" ; needs external data
                          "MuonTest_PlotAsymmetryByLogValueTest" ; needs external data
                          "NexusTest_NexusClassesTest" ; needs external data
                          "NexusTest_NexusDescriptorLazyTest" ; needs external data
                          "NexusTest_NexusDescriptorTest" ; needs external data
                          "NexusTest_NexusFileTest" ; needs external data
                          "NexusTest_NexusIOHelperTest" ; needs external data
                          "PSISINQTest_LoadFlexiNexusTest" ; needs external data
                          "ReflectometryTest_CreateTransmissionWorkspaceAuto2Test" ; needs external data
                          "ReflectometryTest_ReflectometryReductionOneAuto2Test" ; needs external data
                          "ReflectometryTest_ReflectometryReductionOneAuto3Test" ; needs external data
                          "WorkflowAlgorithmsTest_AlignAndFocusPowderTest" ; needs external data
                          "WorkflowAlgorithmsTest_ExtractQENSMembersTest" ; needs external data
                          "WorkflowAlgorithmsTest_LoadEventAndCompressTest" ; needs external data
                          "WorkflowAlgorithmsTest_MuonProcessTest" ; needs external data
                          "WorkflowAlgorithmsTest_SANSSolidAngleCorrectionTest" ; needs external data
                          ;;; Python tests needing external data files.
                          "python.algorithms.Abins2DBasicTest.Abins2DBasicTest" ; needs external data
                          "python.algorithms.SaveReflectionsTest.SaveReflectionsTest" ; needs external data
                          "python.api.WorkspaceHistoryTest.WorkspaceHistoryTest"
                          "mantidqt_qt5.test_samplelogs_model.test_samplelogs_model" ; needs external data
                          "workbench.test_workspacewidget.test_workspacewidget"
                          "python.Calibration.test_tube.test_tube"
                          "python.Calibration.test_tube_calib.test_tube_calib"
                          "python.Calibration.tofpd.test_diagnostics.test_diagnostics" ; needs external data
                          "python.DNSReductionQt5.dns_file_test.dns_file_test" ; needs external data
                          "python.Engineering.test_pawley_utils.test_pawley_utils" ; needs external data
                          "python.EngineeringDiffraction.test_gsas2_model.test_gsas2_model" ; needs external data
                          "python.IsisPowder.ISISPowderAbstractInstrumentTest.ISISPowderAbstractInstrumentTest" ; needs external data
                          "python.IsisPowder.ISISPowderCommonTest.ISISPowderCommonTest"
                          "python.MuonQt5.corrections_model_test.corrections_model_test" ; needs external data
                          "python.MuonQt5.dead_time_corrections_model_test.dead_time_corrections_model_test" ; needs external data
                          "python.MuonQt5.fft_presenter_context_interaction_test.fft_presenter_context_interaction_test" ; needs external data
                          "python.MuonQt5.home_runinfo_presenter_test.home_runinfo_presenter_test" ; needs external data
                          "python.MuonQt5.load_utils_test.load_utils_test" ; needs external data
                          "python.MuonQt5.loadfile_model_test.loadfile_model_test" ; needs external data
                          "python.MuonQt5.loadwidget_presenter_multiple_file_test.loadwidget_presenter_multiple_file_test" ; needs external data
                          "python.MuonQt5.loadwidget_presenter_test.loadwidget_presenter_test" ; needs external data
                          "python.MuonQt5.max_ent_presenter_load_interaction_test.max_ent_presenter_load_interaction_test" ; needs external data
                          "python.MuonQt5.muon_context_test.muon_context_test" ; needs external data
                          "python.MuonQt5.muon_context_with_frequency_test.muon_context_with_frequency_test" ; needs external data
                          "python.MuonQt5.muon_data_context_test.muon_data_context_test" ; needs external data
                          "python.MuonQt5.results_tab_model_test.results_tab_model_test" ; needs external data
                          "python.MuonQt5.workspace_naming_test.workspace_naming_test" ; needs external data
                          "python.SANS.SANSadd2Test.SANSadd2Test"
                          "python.SANS.algorithm_detail.calculate_sans_transmission_test.calculate_sans_transmission_test"
                          "python.SANS.algorithm_detail.calculate_transmission_helper_test.calculate_transmission_helper_test"
                          "python.SANS.algorithm_detail.create_sans_adjustment_workspaces_test.create_sans_adjustment_workspaces_test"
                          "python.SANS.algorithm_detail.crop_helper_test.crop_helper_test"
                          "python.SANS.algorithm_detail.mask_sans_workspace_test.mask_sans_workspace_test"
                          "python.SANS.command_interface.command_interface_state_director_test.command_interface_state_director_test" ; needs external data
                          "python.SimpleAPILoadTest.SimpleAPILoadTest" ; needs external data
                          "python.WorkflowAlgorithms.ApplyPaalmanPingsCorrectionTest.ApplyPaalmanPingsCorrectionTest" ; needs external data
                          "python.WorkflowAlgorithms.BayesQuasi2Test.BayesQuasi2Test" ; needs external data
                          "python.WorkflowAlgorithms.BayesQuasiTest.BayesQuasiTest" ; needs external data
                          "python.WorkflowAlgorithms.BayesStretch2Test.BayesStretch2Test" ; needs external data
                          "python.WorkflowAlgorithms.BayesStretchTest.BayesStretchTest" ; needs external data
                          "python.WorkflowAlgorithms.CalculateMonteCarloAbsorptionTest.CalculateMonteCarloAbsorptionTest" ; needs external data
                          "python.WorkflowAlgorithms.D4ILLReductionTest.D4ILLReductionTest" ; needs external data
                          "python.WorkflowAlgorithms.D7AbsoluteCrossSectionsTest.D7AbsoluteCrossSectionsTest" ; needs external data
                          "python.WorkflowAlgorithms.D7YIGPositionCalibrationTest.D7YIGPositionCalibrationTest" ; needs external data
                          "python.WorkflowAlgorithms.DirectILLAutoProcessTest.DirectILLAutoProcessTest"
                          "python.WorkflowAlgorithms.DirectILLCollectDataTest.DirectILLCollectDataTest" ; needs external data
                          "python.WorkflowAlgorithms.EnergyWindowScanTest.EnergyWindowScanTest" ; needs external data
                          "python.WorkflowAlgorithms.HFIRPowderReductionTest.HFIRPowderReductionTest" ; needs external data
                          "python.WorkflowAlgorithms.ISISIndirectDiffractionReductionTest.ISISIndirectDiffractionReductionTest" ; needs external data
                          "python.WorkflowAlgorithms.ISISIndirectEnergyTransferTest.ISISIndirectEnergyTransferTest" ; needs external data
                          "python.WorkflowAlgorithms.IndirectAnnulusAbsorption2Test.IndirectAnnulusAbsorption2Test" ; needs external data
                          "python.WorkflowAlgorithms.IndirectAnnulusAbsorptionTest.IndirectAnnulusAbsorptionTest" ; needs external data
                          "python.WorkflowAlgorithms.IndirectCalibrationTest.IndirectCalibrationTest" ; needs external data
                          "python.WorkflowAlgorithms.IndirectCylinderAbsorption2Test.IndirectCylinderAbsorption2Test" ; needs external data
                          "python.WorkflowAlgorithms.IndirectCylinderAbsorptionTest.IndirectCylinderAbsorptionTest" ; needs external data
                          "python.WorkflowAlgorithms.IndirectDiffScanTest.IndirectDiffScanTest" ; needs external data
                          "python.WorkflowAlgorithms.IndirectFlatPlateAbsorption2Test.IndirectFlatPlateAbsorption2Test" ; needs external data
                          "python.WorkflowAlgorithms.IndirectFlatPlateAbsorptionTest.IndirectFlatPlateAbsorptionTest" ; needs external data
                          "python.WorkflowAlgorithms.IndirectILLEnergyTransferTest.IndirectILLEnergyTransferTest"
                          "python.WorkflowAlgorithms.IndirectILLReductionDIFFTest.IndirectILLReductionDIFFTest"
                          "python.WorkflowAlgorithms.IndirectILLReductionFWSTest.IndirectILLReductionFWSTest"
                          "python.WorkflowAlgorithms.IndirectILLReductionQENSTest.IndirectILLReductionQENSTest"
                          "python.WorkflowAlgorithms.IndirectReplaceFitResultTest.IndirectReplaceFitResultTest" ; needs external data
                          "python.WorkflowAlgorithms.IndirectResolutionTest.IndirectResolutionTest" ; needs external data
                          "python.WorkflowAlgorithms.IndirectSampleChangerTest.IndirectSampleChangerTest"
                          "python.WorkflowAlgorithms.IndirectTransmissionMonitorTest.IndirectTransmissionMonitorTest" ; needs external data
                          "python.WorkflowAlgorithms.IndirectTwoPeakFitTest.IndirectTwoPeakFitTest"
                          "python.WorkflowAlgorithms.IqtFitMultipleTest.IqtFitMultipleTest" ; needs external data
                          "python.WorkflowAlgorithms.IqtFitSequentialTest.IqtFitSequentialTest" ; needs external data
                          "python.WorkflowAlgorithms.LagrangeILLReductionTest.LagrangeILLReductionTest" ; needs external data
                          "python.WorkflowAlgorithms.LoadWANDTest.LoadWANDTest" ; needs external data
                          "python.WorkflowAlgorithms.MDNormSCDPreprocessIncoherentTest.MDNormSCDPreprocessIncoherentTest"
                          "python.WorkflowAlgorithms.OSIRISDiffractionReductionTest.OSIRISDiffractionReductionTest" ; needs external data
                          "python.WorkflowAlgorithms.PaalmanPingsMonteCarloAbsorptionTest.PaalmanPingsMonteCarloAbsorptionTest" ; needs external data
                          "python.WorkflowAlgorithms.PolDiffILLReductionTest.PolDiffILLReductionTest"
                          "python.WorkflowAlgorithms.PowderILLDetectorScanTest.PowderILLDetectorScanTest"
                          "python.WorkflowAlgorithms.PowderILLParameterScanTest.PowderILLParameterScanTest"
                          "python.WorkflowAlgorithms.QuickBayesHelperTest.QuickBayesHelperTest" ; needs external data
                          "python.WorkflowAlgorithms.ReflectometryILLAutoProcessTest.ReflectometryILLAutoProcessTest"
                          "python.WorkflowAlgorithms.ReflectometryILLConvertToQTest.ReflectometryILLConvertToQTest" ; needs external data
                          "python.WorkflowAlgorithms.ReflectometryILLPolarizationCorTest.ReflectometryILLPolarizationCorTest" ; needs external data
                          "python.WorkflowAlgorithms.ReflectometryILLPreprocessTest.ReflectometryILLPreprocessTest" ; needs external data
                          "python.WorkflowAlgorithms.ReflectometryILLSumForegroundTest.ReflectometryILLSumForegroundTest" ; needs external data
                          "python.WorkflowAlgorithms.ReflectometryISISCalibrationTest.ReflectometryISISCalibrationTest" ; needs external data
                          "python.WorkflowAlgorithms.ReflectometryISISCreateTransmissionTest.ReflectometryISISCreateTransmissionTest"
                          "python.WorkflowAlgorithms.ReflectometryISISLoadAndProcessTest.ReflectometryISISLoadAndProcessTest" ; needs external data
                          "python.WorkflowAlgorithms.ReflectometryISISPreprocessTest.ReflectometryISISPreprocessTest" ; needs external data
                          "python.WorkflowAlgorithms.ResNorm2Test.ResNorm2Test" ; needs external data
                          "python.WorkflowAlgorithms.SANSDarkRunBackgroundCorrectionTest.SANSDarkRunBackgroundCorrectionTest" ; needs external data
                          "python.WorkflowAlgorithms.SANSILLAutoProcessTest.SANSILLAutoProcessTest"
                          "python.WorkflowAlgorithms.SANSILLIntegrationTest.SANSILLIntegrationTest"
                          "python.WorkflowAlgorithms.SANSILLMultiProcessTest.SANSILLMultiProcessTest"
                          "python.WorkflowAlgorithms.SANSILLParameterScanTest.SANSILLParameterScanTest" ; needs external data
                          "python.WorkflowAlgorithms.SANSILLReduction2Test.SANSILLReduction2Test"
                          "python.WorkflowAlgorithms.SANSILLReductionTest.SANSILLReductionTest"
                          "python.WorkflowAlgorithms.SimpleShapeDiscusInelasticTest.SimpleShapeDiscusInelasticTest" ; needs external data
                          "python.WorkflowAlgorithms.SimpleShapeMonteCarloAbsorptionTest.SimpleShapeMonteCarloAbsorptionTest" ; needs external data
                          "python.WorkflowAlgorithms.SimulatedDensityOfStatesTest.SimulatedDensityOfStatesTest" ; needs external data
                          "python.WorkflowAlgorithms.SofQWMomentsScanTest.SofQWMomentsScanTest"
                          "python.WorkflowAlgorithms.SofQWMomentsTest.SofQWMomentsTest"
                          "python.WorkflowAlgorithms.SwapWidthsTest.SwapWidthsTest" ; needs external data
                          "python.WorkflowAlgorithms.TOSCABankCorrectionTest.TOSCABankCorrectionTest" ; needs external data
                          "python.WorkflowAlgorithms.TimeSliceTest.TimeSliceTest" ; needs external data
                          "python.WorkflowAlgorithms.TransformToIqtTest.TransformToIqtTest"
                          "python.WorkflowAlgorithms.VesuvioDiffractionReductionTest.VesuvioDiffractionReductionTest" ; needs external data
                          "python.algorithms.Abins2BasicTest.Abins2BasicTest" ; needs external data
                          "python.algorithms.AbinsAdvancedParametersTest.AbinsAdvancedParametersTest"
                          "python.algorithms.AbinsBasicTest.AbinsBasicTest" ; needs external data
                          "python.algorithms.AngularAutoCorrelationsSingleAxisTest.AngularAutoCorrelationsSingleAxisTest" ; needs external data
                          "python.algorithms.AngularAutoCorrelationsTwoAxesTest.AngularAutoCorrelationsTwoAxesTest" ; needs external data
                          "python.algorithms.AnstoCommonSearchTest.AnstoCommonSearchTest" ; needs external data
                          "python.algorithms.ApplyDetectorScanEffCorrTest.ApplyDetectorScanEffCorrTest" ; needs external data
                          "python.algorithms.CalculateEfficiencyCorrectionTest.CalculateEfficiencyCorrectionTest" ; needs external data
                          "python.algorithms.CheckForSampleLogsTest.CheckForSampleLogsTest" ; needs external data
                          "python.algorithms.ConvertQtoHKLMDHistoTest.ConvertQtoHKLMDHistoTest" ; needs external data
                          "python.algorithms.CorrectLogTimesTest.CorrectLogTimesTest" ; needs external data
                          "python.algorithms.CreateLeBailFitInputTest.CreateLeBailFitInputTest" ; needs external data
                          "python.algorithms.DakotaChiSquaredTest.DakotaChiSquaredTest" ; needs external data
                          "python.algorithms.EnggCalibrateFullTest.EnggCalibrateFullTest" ; needs external data
                          "python.algorithms.EnggCalibrateTest.EnggCalibrateTest" ; needs external data
                          "python.algorithms.EnggFocusTest.EnggFocusTest" ; needs external data
                          "python.algorithms.EnggVanadiumCorrectionsTest.EnggVanadiumCorrectionsTest" ; needs external data
                          "python.algorithms.FilterLogByTimeTest.FilterLogByTimeTest" ; needs external data
                          "python.algorithms.FindGoniometerFromUBTest.FindGoniometerFromUBTest"
                          "python.algorithms.GetEiT0atSNSTest.GetEiT0atSNSTest" ; needs external data
                          "python.algorithms.GroupBySampleChangerPositionTest.GroupBySampleChangerPositionTest" ; needs external data
                          "python.algorithms.HB2AReduceTest.HB2AReduceTest" ; needs external data
                          "python.algorithms.HB3AAdjustSampleNormTest.HB3AAdjustSampleNormTest" ; needs external data
                          "python.algorithms.HB3AFindPeaksTest.HB3AFindPeaksTest" ; needs external data
                          "python.algorithms.HB3AIntegrateDetectorPeaksTest.HB3AIntegrateDetectorPeaksTest" ; needs external data
                          "python.algorithms.HB3AIntegratePeaksTest.HB3AIntegratePeaksTest" ; needs external data
                          "python.algorithms.HB3APredictPeaksTest.HB3APredictPeaksTest" ; needs external data
                          "python.algorithms.IndexSatellitePeaksTest.IndexSatellitePeaksTest" ; needs external data
                          "python.algorithms.LRDirectBeamSortTest.LRDirectBeamSortTest" ; needs external data
                          "python.algorithms.LoadAndMergeTest.LoadAndMergeTest"
                          "python.algorithms.LoadDNSLegacyTest.LoadDNSLegacyTest" ; needs external data
                          "python.algorithms.LoadEmptyVesuvioTest.LoadEmptyVesuvioTest" ; needs external data
                          "python.algorithms.LoadGudrunOutputTest.LoadGudrunOutputTest"
                          "python.algorithms.LoadLampTest.LoadLampTest" ; needs external data
                          "python.algorithms.LoadLogPropertyTableTest.LoadLogPropertyTableTest" ; needs external data
                          "python.algorithms.LoadNMoldyn3AsciiTest.LoadNMoldyn3AsciiTest" ; needs external data
                          "python.algorithms.LoadNMoldyn4Ascii1DTest.LoadNMoldyn4Ascii1DTest"
                          "python.algorithms.LoadNMoldyn4AsciiTest.LoadNMoldyn4AsciiTest"
                          "python.algorithms.LoadSANS1MLZTest.LoadSANS1MLZTest" ; needs external data
                          "python.algorithms.LoadWANDSCDTest.LoadWANDSCDTest" ; needs external data
                          "python.algorithms.NOMADMedianDetectorTestTest.NOMADMedianDetectorTestTest" ; needs external data
                          "python.algorithms.OptimizeCrystalPlacementByRunTest.OptimizeCrystalPlacementByRunTest" ; needs external data
                          "python.algorithms.PEARLTransfitTest.PEARLTransfitTest" ; needs external data
                          "python.algorithms.PoldiAutoCorrelation6Test.PoldiAutoCorrelation6Test" ; needs external data
                          "python.algorithms.RebinRaggedTest.RebinRaggedTest" ; needs external data
                          "python.algorithms.RefineSingleCrystalGoniometerTest.RefineSingleCrystalGoniometerTest" ; needs external data
                          "python.algorithms.RetrieveRunInfoTest.RetrieveRunInfoTest"
                          "python.algorithms.SNSPowderReductionTest.SNSPowderReductionTest"
                          "python.algorithms.SaveGEMMAUDParamFileTest.SaveGEMMAUDParamFileTest" ; needs external data
                          "python.algorithms.SelectNexusFilesByMetadataTest.SelectNexusFilesByMetadataTest" ; needs external data
                          "python.algorithms.SetDetScaleTest.SetDetScaleTest" ; needs external data
                          "python.algorithms.SetSampleFromLogsTest.SetSampleFromLogsTest" ; needs external data
                          "python.algorithms.SortByQVectorsTest.SortByQVectorsTest" ; needs external data
                          "python.algorithms.TOFTOFCropWorkspaceTest.TOFTOFCropWorkspaceTest" ; needs external data
                          "python.algorithms.TOFTOFMergeRunsTest.TOFTOFMergeRunsTest" ; needs external data
                          "python.algorithms.VelocityAutoCorrelationsTest.VelocityAutoCorrelationsTest" ; needs external data
                          "python.algorithms.VelocityCrossCorrelationsTest.VelocityCrossCorrelationsTest" ; needs external data
                          "python.api.AlgorithmTest.AlgorithmTest" ; needs external data
                          "python.api.FileFinderTest.FileFinderTest" ; needs external data
                          "python.api.ITableWorkspaceTest.ITableWorkspaceTest" ; needs external data
                          "python.api.MultipleFilePropertyTest.MultipleFilePropertyTest" ; needs external data
                          "python.corelli.calibration.test_bank.test_bank"
                          "python.corelli.calibration.test_database.test_database"
                          "python.corelli.calibration.test_utils.test_utils" ; needs external data
                          "python.dataobjects.SpecialWorkspace2DTest.SpecialWorkspace2DTest"
                          "python.dataobjects.Workspace2DPickleTest.Workspace2DPickleTest" ; needs external data
                          "python.directtools.DirectToolsTest.DirectToolsTest" ; needs external data
                          "python.plots.datafunctionsTest.datafunctionsTest" ; needs external data
                          "python.scripts.ConvertToWavelengthTest.ConvertToWavelengthTest"
                          "python.scripts.DirectEnergyConversionTest.DirectEnergyConversionTest" ; needs external data
                          "python.scripts.DirectPropertyManagerTest.DirectPropertyManagerTest" ; needs external data
                          "python.scripts.IndirectReductionCommonTest.IndirectReductionCommonTest" ; needs external data
                          "python.scripts.ReductionWrapperTest.ReductionWrapperTest"
                          "python.scripts.ReflectometryQuickAuxiliaryTest.ReflectometryQuickAuxiliaryTest"
                          "python.scripts.RunDescriptorTest.RunDescriptorTest" ; needs external data
                          "python.scripts.SANSDarkRunCorrectionTest.SANSDarkRunCorrectionTest" ; needs external data
                          "python.scripts.SANSUtilityTest.SANSUtilityTest"
                          "python.utils.absorptioncorrutilsTest.absorptioncorrutilsTest" ; needs external data
                          "python.scripts.AbinsAlgorithmTest.AbinsAlgorithmTest" ; needs external data
                          "python.scripts.AbinsCalculateSPowderTest.AbinsCalculateSPowderTest"
                          "python.scripts.AbinsDataTest.AbinsDataTest" ; needs external data
                          "python.scripts.AbinsLoadCASTEPTest.AbinsLoadCASTEPTest" ; needs external data
                          "python.scripts.AbinsLoadCRYSTALTest.AbinsLoadCRYSTALTest"
                          "python.scripts.AbinsLoadDMOL3Test.AbinsLoadDMOL3Test"
                          "python.scripts.AbinsLoadGAUSSIANTest.AbinsLoadGAUSSIANTest"
                          "python.scripts.AbinsLoadJSONTest.AbinsLoadJSONTest"
                          "python.scripts.AbinsLoadMoldenTest.AbinsLoadMoldenTest"
                          "python.scripts.AbinsLoadPhonopyTest.AbinsLoadPhonopyTest"
                          "python.scripts.AbinsLoadVASPTest.AbinsLoadVASPTest" ; needs external data
                          "python.scripts.AbinsPowderCalculatorTest.AbinsPowderCalculatorTest"
                          "python.IsisPowder.ISISPowderGemOutputTest.ISISPowderGemOutputTest" ; needs external data
                          "python.IsisPowder.ISISPowderRunDetailsTest.ISISPowderRunDetailsTest" ; needs external data
                          "python.SANS.SANSReducerTest.SANSReducerTest" ; needs external data
                          "python.SANS.SansIsisGuiSettings.SansIsisGuiSettings" ; needs external data
                          "python.SANS.command_interface.batch_csv_file_parser_test.batch_csv_file_parser_test" ; needs external data
                          "python.algorithms.GenerateLogbookTest.GenerateLogbookTest"
                          "python.algorithms.LoadMultipleGSSTest.LoadMultipleGSSTest"
                          "python.WorkflowAlgorithms.MolDynTest.MolDynTest" ; needs external data
                          "python.WorkflowAlgorithms.ReflectometryILL_commonTest.ReflectometryILL_commonTest" ; needs external data
                          "python.DNSReductionQt5.tof_powder_plot_presenter_test.tof_powder_plot_presenter_test"
                          "python.MuonQt5.add_raw_plots_test.add_raw_plots_test"
                          "python.EngineeringDiffraction.engineering_diffraction_io_test.engineering_diffraction_io_test" ; needs external data
                          "PythonSANS.file_information_test.file_information_test"
                          "PythonSANS.xml_parsing_test.xml_parsing_test"
                          "PythonSANSQt5.create_state_test.create_state_test" ; needs external data
                          "PythonSANSQt5.diagnostics_page_model_test.diagnostics_page_model_test"
                          "PythonSANSQt5.gui_state_director_test.gui_state_director_test"
                          "PythonSANSQt5.test_run_tab_presenter.test_run_tab_presenter"
                          "PythonTOFTOFReductionQt5.TOFTOFScriptElementTest.TOFTOFScriptElementTest" ; needs external data
                          "mantidqt_qt5.test_projectparser_mantidplot.test_projectparser_mantidplot" ; needs external data
                          "mantidqt_qt5.test_sample_shape.test_sample_shape" ; needs external data
                          "mantidqt_qt5.test_tableworkspacedisplay_io.test_tableworkspacedisplay_io" ; needs external data
                          "workbench.test_toolbar.test_toolbar" ; needs external data
                          ;; "Key combination is not set by Scintilla" for Ctrl+A.
                          "mantidqt_qt5.test_interpreter.test_interpreter"
                          ;; Test expects ValueError but gets Exception.
                          ;; <https://github.com/mantidproject/mantid/issues/21251>
                          "mantidqt_qt5.test_codeeditor.test_codeeditor"
                          ;; Expects numpy 2.0 function signatures (device, copy).
                          ;; Title offset (-0.05,1.05) vs expected (-0.07,1.07).
                          ;; test_custom_eliding_delegate_respects_padding:
                          ;; The delegate calls QFontMetrics.elidedText to
                          ;; truncate "3.141592653589793" to fit a 93 px rect.
                          ;; The test asserts startswith("3.141592") but never
                          ;; sets a specific font, so the truncation point
                          ;; depends on the system default font metrics.
                          "mantidqt_qt5.test_matrixworkspacedisplay_delegate.test_matrixworkspacedisplay_delegate"
                          ;;; Those have segfault on cleanup after successful tests.
                          "mantidqt_qt5.test_apply_all_properties.test_apply_all_properties"
                          "python.MuonQt5.MultiPlotWidget_test.MultiPlotWidget_test"
                          "python.MuonQt5.muon_analysis_plot_widget_test.muon_analysis_plot_widget_test"
                          "python.DNSReductionQt5.dns_modus_test.dns_modus_test"
                          "python.MuonQt5.frequency_analysis_plot_widget_test.frequency_analysis_plot_widget_test"
                          "mantidqt_qt5.test_samplelogs_view.test_samplelogs_view"
                          "mantidqt_qt5.test_sliceviewer_imageinfowidget.test_sliceviewer_imageinfowidget")
                        "|")))
                  (apply invoke "ctest" "--output-on-failure"
                         "-j" (if parallel-tests?
                                  (number->string (parallel-job-count))
                                  "1")
                         ;; Only run unit tests, not system tests.
                         ;; Upstream CI does the same (see "ctest -L UnitTest"
                         ;; in buildconfig/Jenkins/Conda/run-tests).
                         ;; System tests are run separately via the
                         ;; "systemtest" script and require external data
                         ;; files that "cannot be placed in the shared
                         ;; repository" (dev-docs/source/SystemTests.rst).
                         "-L" "UnitTest"
                         "-E" exclude-pattern
                         '()))))))))
    (native-inputs
     (list cmake googletest pkg-config python-pytz python-setuptools python-wrapper
           xorg-server-for-tests))
    (inputs
     (list boost
           eigen
           glu
           gsl
           hdf5
           jemalloc
           jsoncpp
           libjpeg-turbo
           librdkafka
           mesa
           muparser
           opencascade-occt
           openssl
           poco
           python
           python-pyqt
           python-qtpy
           python-pyqt5-sip
           python-pyqtwebengine
           qscintilla
           qtbase-5
           qtsvg-5
           qttools-5
           qtwayland-5
           qtwebengine-5
           tbb
           zlib))
    (propagated-inputs
     (list python-annotated-types
           python-dateutil
           python-euphonic
           python-h5py
           python-ipython-pygments-lexers
           python-joblib
           python-lz4
           python-matplotlib
           python-mslice
           python-mvesuvio           ; optional: script library, no menu item
           python-numpy
           python-shiver             ; optional: Interfaces menu extension
           python-orsopy
           python-psutil
           python-pycifrw
           python-pydantic
           python-pydantic-core
           python-pystog
           python-pystack
           python-pyvista
           python-pyvistaqt
           python-pyyaml
           python-qtconsole
           python-qtpy
           python-quasielasticbayes
           python-quickbayes
           python-scooby
           python-scipy
           python-seekpath
           python-superqt
           python-toml
           vtk))
    (home-page "https://www.mantidproject.org/")
    (synopsis "Data analysis framework for neutron and muon experiments")
    (description
     "Mantid is a framework for the reduction and analysis of neutron and muon
scattering data.  It provides a collection of algorithms for data processing,
visualization tools, and interfaces for instrument scientists and users at
neutron and muon facilities worldwide.

Optional GUI extensions such as @code{python-mslice} and @code{python-shiver}
appear in the Workbench Interfaces menu when installed alongside Mantid.
Script libraries like @code{python-mvesuvio} can be imported in Workbench's
script editor.")
    (license license:gpl3+)))
