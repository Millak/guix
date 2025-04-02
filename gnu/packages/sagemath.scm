;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2025 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2025 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages sagemath)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz))

(define-public brial
  (package
    (name "brial")
    (version "1.2.8")
    (source
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/BRiAl/BRiAl/")
             (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0qhgckd4fvbs40jw14mvw89rccv94d3df27kipd27hxd4cx7y80y"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list boost libpng m4ri))
    (arguments
    ;; We are missing the boost unit test framework.
     `(#:tests? #f
       #:configure-flags (list "--without-boost-unit-test-framework")))
    (synopsis "Arithmetic of polynomials over boolean rings")
    (description "BRiAl is the successor to  PolyBoRi maintained by the
Sage community.  Its core is a C++ library, which provides high-level data
types for Boolean polynomials and monomials, exponent vectors, as well as
for the underlying polynomial rings and subsets of the powerset of the
Boolean variables.  As a unique approach, binary decision diagrams are
used as internal storage type for polynomial structures.")
    (license license:gpl2+)
    (home-page "https://github.com/BRiAl/BRiAl/")))

(define-public lcalc
  (package
    (name "lcalc")
    (version "2.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/sagemath/lcalc")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rwyx292y3jbsp88wagn9nhl9z7wsnl2yrs5imxkbxq87pnrj5a7"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags '(list "--with-pari")))
    (inputs (list pari-gp))
    (native-inputs (list autoconf automake libtool pkg-config gengetopt))
    (home-page "https://gitlab.com/sagemath/lcalc")
    (synopsis "C++ library for computing with L-functions")
    (description
     "Lcalc computes L-functions, in particular the Riemann zeta function,
Dirichlet L-functions and L-functions attached to elliptic curves and
modular forms.")
    (license license:gpl2+)))

(define-public cliquer
  (package
    (name "cliquer")
    (version "1.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dimpase/autocliquer")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00gcmrhi2fjn8b246w5a3b0pl7p6haxy5wjvd9kcqib1xanz59z4"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool))
    (synopsis "C routines for finding cliques in weighted graphs")
    (description "Cliquer is a set of reentrant C routines for finding
cliques in a weighted or unweighted graph.  It uses an exact
branch-and-bound algorithm.  It can search for maximum or maximum-weight
cliques or cliques with size or weight within a given range, restrict the
search to maximal cliques, store cliques in memory and call a user-defined
function for every found clique.")
    (license license:gpl2+)
    (home-page "https://github.com/dimpase/autocliquer")))

(define-public libbraiding
  (package
    (name "libbraiding")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://github.com/miguelmarco/"
                                  name))
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d9vrn507phpzwwmmrxiig8s484rdi3gjghj75d4sdzbiqyyr9yp"))
       (patches (search-patches "libbraiding-no-s.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (synopsis "Computations with braid groups")
    (description "libbraiding performs computations with braid groups,
in particular it computes normal forms of group elements.")
    (license license:gpl2+)
    (home-page "https://github.com/miguelmarco/libbraiding")))

(define-public libhomfly
  (package
    (name "libhomfly")
    (version "1.02r6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://github.com/miguelmarco/"
                                  name))
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0sv3cwrf9v9sb5a8wbhjmarxvya13ma3j8y8592f9ymxlk5y0ldk"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list libgc))
    (synopsis "Computation of homfly polynomials of links")
    (description "libhomfly computes homfly polynomials of links,
represented as strings.")
    (license license:public-domain)
    (home-page "https://github.com/miguelmarco/libhomfly")))

(define-public python-cypari2
  (package
    (name "python-cypari2")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cypari2" version))
       (sha256
        (base32
         "1f43bx3pryc1mg7i9gijl2c6aavijlbcxx9wyw11jn40lak1g85a"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-cython-3 python-setuptools python-wheel))
    (propagated-inputs
     (list python-cysignals))
    (inputs
     (list gmp pari-gp))
    (home-page "https://cypari2.readthedocs.io/")
    (synopsis
     "Python interface to the number theory library libpari")
    (description
     "Cypari2 provides a Python interface to the number theory library
PARI/GP.  It has been spun off from the SageMath mathematics software system,
but it can be used independently.")
    (license license:gpl2+)))

(define-public python-gmpy2
  (package
    (name "python-gmpy2")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gmpy2" version))
       (sha256
        (base32
         "1lc29g3s4z5f1qbsc2x9i9sf6wrpni9pwiwmb1wwx3hjr85i8xfs"))))
    (build-system python-build-system)
    (native-inputs
     (list unzip))
    (inputs
     (list gmp mpfr mpc))
    (home-page "https://github.com/aleaxit/gmpy")
    (synopsis
     "GMP/MPIR, MPFR, and MPC interface to Python 2.6+ and 3.x")
    (description
     "This package provides a Python interface to the GNU multiprecision
libraries GMO, MPFR and MPC.")
    (license license:lgpl3+)))

(define-public python-memory-allocator
  (package
    (name "python-memory-allocator")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "memory_allocator" version))
       (sha256
        (base32 "1r7g175ddbpn5kjgs6f09s7mfachzw94p02snki6f6830dmj22fn"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-cython python-setuptools python-wheel))
    (home-page "https://github.com/sagemath/memory_allocator")
    (synopsis "Extension class to allocate memory easily with Cython")
    (description "This package provides a single extension class
 @code{MemoryAllocator} with @{cdef} methods

@itemize
@item @code{malloc}
@item @code{calloc}
@item @code{allocarray}
@item @code{realloc}
@item @code{reallocarray}
@item @code{aligned_malloc}
@item @code{aligned_malloc}
@item @code{aligned_calloc}
@item @code{aligned_allocarray}")
    (license license:gpl3+)))

(define-public python-pplpy
  (package
    (name "python-pplpy")
    (version "0.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pplpy" version))
       (sha256
        (base32 "1zggfj09zkfcabcsasq27vwbhdmkig4yn380gi6wykcih9n22anl"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-cython-3
           python-pytest
           python-setuptools
           python-wheel))
    (inputs (list gmp mpc mpfr pari-gp ppl))
    (propagated-inputs (list python-cysignals python-gmpy2))
    (home-page "https://github.com/sagemath/pplpy")
    (synopsis "Python PPL wrapper")
    (description "This Python package provides a wrapper to the C++ Parma
Polyhedra Library (PPL).")
    (license license:gpl3+)))

(define conway-polynomials
  (package
    (name "conway-polynomials")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "conway_polynomials" version))
       (sha256
        (base32 "1wz03a08kwlswx1pcr5d99ppmhpfzs2a5i969rnb2ghsz1j9yqag"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/sagemath/conway-polynomials")
    (synopsis "Python interface to Frank Lübeck's Conway polynomial database")
    (description "This package provides a Python interface to Frank Lübeck's
Conway polynomial database.  These are used in several computer algebra
systems such as GAP and SageMath to provide quick access to those Conway
polynomials.  The aim of this package is to make them available through a
generic Python interface.")
    (license license:gpl3+)))

(define polytopes-db
  (package
    (name "polytopes-db")
    (version "20170220")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://mirrors.mit.edu/sage/spkg/upstream/polytopes_db"
                    "/polytopes_db-" version ".tar.bz2"))
              (sha256
               (base32
                "1q0cd811ilhax4dsj9y5p7z8prlalqr7k9mzq178c03frbgqny6b"))))
    (build-system copy-build-system)
    (arguments '(#:install-plan '(("." "share/reflexive_polytopes"))))
    (home-page "https://doc.sagemath.org/html/en/reference/spkg/polytopes_db.html")
    (synopsis "Lists of 2- and 3-dimensional reflexive polytopes")
    (description
     "This package contains data for 2- and 3-dimensional reflexive polytopes
to be used by SageMath.")
    ;; Debian says GPLv2+.
    (license license:gpl2+)))

(define graphs
  (package
    (name "graphs")
    (version "20210214")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://mirrors.mit.edu/sage/spkg/upstream/"
                    "graphs/graphs-" version ".tar.bz2"))
              (sha256
               (base32
                "0h9p5wrxips51x6vpfiiaqzp9j004nwppzc9qc2iaqakk06pq8q7"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("." "share/graphs"))))
    (home-page "https://www.sagemath.org")
    (synopsis "Database of graphs")
    (description
     "This package contains databases of graphs.  It also includes the
@acronym{ISGCI, Information System on Graph Classes and their Inclusions}
database.")
    ;; Debian says GPLv2+.
    (license license:gpl2+)))

(define-public sage
  (package
    (name "sage")
    (version "10.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sagemath/sage")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m2f6k6nwgyzfhf45r0kp798aimjxhpfnmsp1k03jpj9d6mhadk4"))))
    (build-system pyproject-build-system)
    (native-inputs
      (list autoconf automake m4 pkg-config ; for ./bootstrap
            python-cython-3
            python-cysignals
            python-memory-allocator
            python-pkgconfig
            python-jinja2
            python-setuptools
            python-wheel))
    (propagated-inputs
      (list ;; required to make the sage script start
            python-ipython
            python-ipywidgets
            python-traitlets
            ;; required for the Jupyter notebook
            python-jupyter-client
            python-notebook))
    (inputs
      (list boost
            brial
            cliquer
            conway-polynomials
            coreutils
            ecl
            eclib
            edge-addition-planarity-suite
            fflas-ffpack
            flint
            fontconfig
            freetype
            gap
            gd
            giac
            givaro
            glpk
            gmp
            gmp-ecm
            graphs
            gsl
            ijg-libjpeg
            iml
            lcalc
            libbraiding
            libhomfly
            linbox
            libpng
            m4ri
            m4rie
            maxima-ecl
            mpc
            mpfi
            mpfr
            ntl
            openblas
            pari-gp
            polytopes-db
            python
            python-cypari2
            python-fpylll
            python-gmpy2
            python-numpy
            python-pplpy
            rw
            sed
            singular
            symmetrica))
    (arguments
      (list
        #:tests? #f ; Tests can be run using "make ptestlong", but it is
                    ; expected that some of them fail.
        #:phases
        #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda _
              (let ((ecl #$(this-package-input "ecl"))
                    (ecm #$(this-package-input "gmp-ecm"))
                    (maxima #$(this-package-input "maxima-ecl"))
                    (maxima-version #$(package-version
                                        (this-package-input "maxima-ecl")))
                    (singular #$(this-package-input "singular"))
                    (gap #$(this-package-input "gap"))
                    (pari-gp #$(this-package-input "pari-gp"))
                    (python #$(this-package-input "python"))
                    (coreutils #$(this-package-input "coreutils"))
                    (sed #$(this-package-input "sed"))
                    (graphs #$(this-package-input "graphs"))
                    (polytopes-db #$(this-package-input "polytopes-db")))
                (substitute* (find-files "build/bin")
                  (("sage-bootstrap-python") "python"))
                (substitute* "src/sage/env.py"
                  (("\"ecl-config\"")
                   (string-append "\"" ecl "/bin/ecl-config\""))
                  (("\"ecm\"")
                   (string-append "\"" ecm "/bin/ecm\""))
                  (("\"maxima\"")
                   (string-append "\"" maxima "/bin/maxima\""))
                  (("\"MAXIMA_FAS\"")
                   (string-append "\"MAXIMA_FAS\", \"" maxima
                                  "/lib/maxima/" maxima-version
                                  "/binary-ecl/maxima.fas\""))
                  (("\"Singular\"")
                   (string-append "\"" singular "/bin/Singular\""))
                  (("var\\('SAGE_GAP_COMMAND', None\\)")
                   (string-append "var('SAGE_GAP_COMMAND', \""
                                  gap "/bin/gap\")"))
                  (("join\\(SAGE_LOCAL, \"lib\", \"gap\"\\)")
                   (string-append "\"" gap "/lib/gap\""))
                  (("join\\(SAGE_LOCAL, \"share\", \"gap\"\\)")
                   (string-append "\"" gap "/share/gap\""))
                  ;; definition of SAGE_VENV, which ends up in kernel.json
                  (("os.path.abspath\\(sys.prefix\\)")
                   (string-append "\"" #$output "\""))
                  ;; paths of the databases
                  (("GRAPHS_DATA_DIR\"")
                   (string-append
                     "GRAPHS_DATA_DIR\", "
                     "\"" graphs "/share/graphs\""))
                  (("POLYTOPE_DATA_DIR\"")
                   (string-append
                     "POLYTOPE_DATA_DIR\", "
                     "\"" polytopes-db "/share/reflexive_polytopes\"")))
                (substitute* "src/sage/interfaces/gp.py"
                  (("command=f\"gp")
                   (string-append "command=f\"" pari-gp "/bin/gp")))
                (substitute* "src/bin/sage"
                  (("exec python3")
                   (string-append "exec " python "/bin/python3"))
                  (("mkdir")
                   (string-append coreutils "/bin/mkdir"))
                  ((" sed ")
                   (string-append " " sed "/bin/sed "))))))
          (add-before 'build 'setup
            (lambda _
              (setenv "SAGE_NUM_THREADS"
                      (number->string (parallel-job-count)))
              (invoke "./bootstrap")
              (chdir "src")))
          (delete 'sanity-check)))) ; does not reflect reality
    (home-page "https://www.sagemath.org/")
    (synopsis "SageMath computer algebra system")
    (description
     "SageMath is a mathematics software built on top of many existing
packages such as NumPy, SciPy, Matplotlib, Sympy, Maxima, GAP, FLINT,
R and others. Their combined power may be accessed through a common,
Python-based language or directly via interfaces or wrappers.")
    ;; Documentation under cc-by-sa3.0, see COPYING.txt in the distribution.
    (license license:gpl3)))
