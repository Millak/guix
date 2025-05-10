;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022–2024 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2023 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024-2025 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages potassco)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages re2c)
  #:use-module (gnu packages sphinx))

(define-public libpotassco
  ;; No public release, update together with clasp
  (let ((revision "3")
        (commit "2eecf5f066fe1f77c9122547f7e07ba1e8c1dcdf"))
    (package
      (name "libpotassco")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/potassco/libpotassco")
                      (commit commit)))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
                     (delete-file "tests/catch.hpp")
                     (substitute* (find-files "tests" "\\.cpp")
                       (("\"catch.hpp\"") "<catch/catch.hpp>"))))
                (sha256
                 (base32
                  "0l502qcpwnnpa84hg7vynicpqp3mvivw150jlywqdp1g1irwnh7j"))))
      (arguments
       `(#:configure-flags '("-DLIB_POTASSCO_BUILD_TESTS=on"
                             "-DLIB_POTASSCO_INSTALL_LIB=on"
                             "-DBUILD_SHARED_LIBS=on")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-cmake
             (lambda _
               (substitute* "CMakeLists.txt"
                 ;; clasp expects lowercase potassco and include directory is
                 ;; lowercase as well, so let's use that
                 (("\"cmake/Potassco\"") "\"cmake/potassco\"")
                 (("PotasscoConfig\\.cmake") "potassco-config.cmake")
                 (("PotasscoConfigVersion\\.cmake")
                  "potassco-config-version.cmake"))
               (rename-file "cmake/PotasscoConfig.cmake.in"
                            "cmake/potassco-config.cmake.in"))))))
      (build-system cmake-build-system)
      (native-inputs (list catch2-1))
      (home-page "https://potassco.org/")
      (synopsis "Utility library for Potassco's projects")
      (description "@code{libpotassco} is a utility library providing functions
and datatypes for
@itemize
@item parsing, writing, and converting logic programs in aspif and smodels
format,
@item passing information between a grounder and a solver,
@item and defining and parsing command-line options and for creating
command-line applications.
@end itemize
Furthermore, it comes with the tool @command{lpconvert} that converts either
between aspif and smodels format or to a human-readable text format.")
      (license license:expat))))

(define-public clasp
  (package
    (name "clasp")
    (version "3.3.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clasp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches (search-patches "clasp-hide-event-ids.patch"))
              (sha256
               (base32
                "0qap7rar8a5mkqz28n2hnvr4cfv5x0rh4zs3wdp919dw4d034chr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DCLASP_BUILD_TESTS=on"
                           "-DCLASP_INSTALL_LIB=on"
                           "-DCLASP_USE_LOCAL_LIB_POTASSCO=off"
                           "-DBUILD_SHARED_LIBS=on")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-cmake
           (lambda _
             (substitute* "CMakeLists.txt"
               ;; Use lowercase to be consistent with libpotassco
               (("\"cmake/Clasp\"") "\"cmake/clasp\"")
               (("ClaspConfig\\.cmake") "clasp-config.cmake")
               (("ClaspConfigVersion\\.cmake")
                "clasp-config-version.cmake"))
             (substitute* "cmake/ClaspConfig.cmake.in"
               (("find_package\\(Potassco") "find_package(potassco"))
             (rename-file "cmake/ClaspConfig.cmake.in"
                          "cmake/clasp-config.cmake.in"))))))
    (inputs
     (list libpotassco))
    (home-page "https://potassco.org/")
    (synopsis "Answer set solver")
    (description "clasp is an answer set solver for (extended) normal and
disjunctive logic programs.  The primary algorithm of clasp relies on
conflict-driven nogood learning, a technique that proved very successful for
satisfiability checking (SAT).")
    (license license:expat)))

(define-public clingo
  (package
    (name "clingo")
    (version "5.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clingo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (delete-file-recursively "clasp")
                   (delete-file-recursively "libgringo/gen")
                   (delete-file-recursively "third_party")
                   (delete-file "libpyclingo/_clingo.c")))
              (sha256
               (base32
                "1mxl3gwx55sf2ifcb92mfy989c50yqpnq0d0r2mxdqr0riy40hjb"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~`("-DCLINGO_BUILD_TESTS=on"
                            "-DCLINGO_INSTALL_LIB=on"
                            "-DCLINGO_BUILD_STATIC=off"
                            "-DCLINGO_BUILD_SHARED=on"
                            "-DCLINGO_USE_LOCAL_CLASP=off"
                            "-DCLINGO_USE_LOCAL_CATCH=off")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-cmake
            (lambda _
              (substitute* "CMakeLists.txt"
                (("add_subdirectory\\(clasp\\)")
                 "find_package(clasp REQUIRED)")
                (("add_subdirectory\\(third_party\\)")
                 (string-append
                  "find_package(tsl-hopscotch-map)\n"
                  "find_package(tl-optional)\n"
                  "find_package(mpark_variant)\n"
                  "find_package(tsl-sparse-map)\n"
                  "find_package(tsl-ordered-map)\n"
                  "find_package(wide-integer)\n"
                  "find_package(Catch2 3 REQUIRED)")))
              (substitute* "libclingo/CMakeLists.txt"
                (("\"cmake/Clingo\"") "\"cmake/clingo\"")
                (("ClingoConfig\\.cmake") "clingo-config.cmake")
                (("ClingoConfigVersion\\.cmake")
                 "clingo-config-version.cmake"))
              (substitute* "libgringo/CMakeLists.txt"
                (("mpark::variant") "mpark_variant")
                (("math::wide_integer") "wide-integer::wide-integer"))
              (substitute* "cmake/ClingoConfig.cmake.in"
                (("find_package\\(Clasp") "find_package(clasp"))
              (rename-file "cmake/ClingoConfig.cmake.in"
                           "cmake/clingo-config.cmake.in"))))))
    (inputs (list catch2-3 clasp libpotassco))
    (native-inputs (list bison re2c
                         mpark-variant
                         pkg-config
                         tl-optional
                         tsl-hopscotch-map
                         tsl-ordered-map
                         tsl-sparse-map
                         wide-integer))
    (home-page "https://potassco.org/")
    (synopsis "Grounder and solver for logic programs")
    (description "Clingo computes answer sets for a given logic program.")
    (license license:expat)))

(define-public clingo-dl
  (package
    (name "clingo-dl")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clingo-dl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0vf51pgwgiac801gr6w5pnxb6wa0kacz09ncrcn25w5siz17g4si"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f        ; no tests
                     #:configure-flags #~`("-DPYCLINGODL_ENABLE=off")))
    (inputs (list clingo))
    (home-page "https://github.com/potassco/clingo-dl")
    (synopsis "Solver for answer set programs modulo difference constraints")
    (description "Clingo-DL is an extension to Clingo that models constraints
over difference logic.")
    (license license:expat)))

(define-public clingo-lpx
  (package
    (name "clingo-lpx")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clingo-lpx")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (delete-file-recursively "third_party")))
              (sha256
               (base32
                "1i184gy18k0mpqywbgziwl5wzkwwcdks81axndk4x6hjy878vhww"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DCLINGOLPX_BUILD_TESTS=on"
                                     "-DPYCLINGOLPX_ENABLE=off")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-cmake
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("add_subdirectory\\(third_party\\)")
                      "find_package(Catch2 3 REQUIRED)")))))))
    (home-page "https://github.com/potassco/clingo-lpx")
    (inputs (list clingo flint))
    (native-inputs (list catch2-3))
    (synopsis "Simplex solver")
    (description "Clingo-LPX is an extension to Clingo that models constraints
and goals over linear (in)equations.")
    (license license:expat)))

(define-public clingcon
  (package
    (name "clingcon")
    (version "5.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clingcon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (delete-file-recursively "third_party")))
              (sha256
               (base32
                "0050qp5gpznigpm743br8yhjg62gl739xmzkfr70hlqm1xrj0sa7"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-cmake
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("add_subdirectory\\(third_party\\)")
                      "find_package(Catch2 3 REQUIRED)"))
                   ;; We use libwide-integer as a header-only library, so there
                   ;; is no library to link
                   (substitute* "libclingcon/CMakeLists.txt"
                     (("target_link_libraries\\(.* libwide-integer\\)" all)
                      (string-append "#" all))))))))
    (home-page "https://potassco.org/clingcon")
    (inputs (list clingo wide-integer))
    (native-inputs (list catch2-3))
    (synopsis "Constraint answer set solver")
    (description "Clingcon is an answer set solver for constraint logic
It extends Clingo with constraint solving capacities for constraints over
finite domain integer variables, using techniques from the area of SMT,
like conflict-driven learning and theory propagation.")
    (license license:expat)))

(define-public plasp
  (package
    (name "plasp")
    (version "3.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/plasp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "123v1bjzh7yjwgcc5k55rkfz0lfl8ish5p3z8x3pn8k1svd50xal"))
              (patches (search-patches
                        "plasp-fix-normalization.patch"
                        "plasp-include-iostream.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f        ; No ‘test’ target
           #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (copy-recursively "bin"
                                     (string-append (assoc-ref outputs "out")
                                                    "/bin")))))))
    (inputs (list cxxopts mapbox-variant))
    (home-page "https://potassco.org/")
    (synopsis "ASP planning tools for PDDL")
    (description "@command{plasp} is a tool collection for planning in
answer set programming.  It supports a subset of PDDL 3.1 and SAS 3.")
    (license license:expat)))

(define-public emacs-pasp-mode
  (let ((commit "59385eb0e8ebcfc8c11dd811fb145d4b0fa3cc92")
        (revision "1"))
    (package
     (name "emacs-pasp-mode")
     (version (git-version "0.1.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/santifa/pasp-mode")
                    (commit commit)))
              (file-name (git-file-name name version))
              (patches
               (search-patches "emacs-pasp-mode-quote-file-names.patch"))
              (sha256
               (base32
                "1ar4vws3izzmir7m870mccci620ns3c5j26dcmwaxavhgw45wcmf"))))
     (build-system emacs-build-system)
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'defconst-version
             (lambda _
               (emacs-batch-edit-file "pasp-mode.el"
                 '(progn
                   (search-forward-regexp "(defcustom pasp-mode-version")
                   (forward-sexp)
                   (kill-sexp)
                   (backward-sexp)
                   (beginning-of-line)
                   (kill-sexp)
                   (insert (format "(defconst emacs-pasp-version \"%s\" %s)"
                                   #$version (cadr kill-ring)))
                   (basic-save-buffer)))))
           (add-after 'unpack 'hardcode-clingo
             (lambda* (#:key inputs #:allow-other-keys)
               (emacs-substitute-variables "pasp-mode.el"
                 ("pasp-clingo-path"
                  (search-input-file inputs "/bin/clingo"))))))))
     (inputs (list clingo))
     (home-page "https://github.com/santifa/pasp-mode")
     (synopsis "Major mode for editing answer set programs")
     (description
      "This package provides a major mode for editing answer set programs,
in particular ones that can be solved by @command{clingo}.")
     (license license:gpl3+))))

(define-public (make-lua-clingo name lua)
  (package
    (inherit clingo)
    (name name)
    (version (package-version clingo)) ; for #$version in arguments
    (arguments
     (substitute-keyword-arguments (package-arguments clingo)
       ((#:configure-flags flags #~'())
        #~(cons* "-DCLINGO_BUILD_WITH_LUA=yes"
                 (string-append "-DLUACLINGO_INSTALL_DIR="
                                #$output "/lib/lua/"
                                #$(package-version lua))
                 "-DCLINGO_USE_LIB=yes"
                 #$flags))))
    (inputs (list clingo lua))
    (synopsis "Lua bindings for clingo")
    (description "This package provides Lua bindings to the clingo package,
making it so that you can write @acronym{ASPs, Answer Set Programs} through
Lua code.")))

(define-public lua5.1-clingo (make-lua-clingo "lua5.1-clingo" lua-5.1))
(define-public lua5.2-clingo (make-lua-clingo "lua5.2-clingo" lua-5.2))

(define-public python-clingo
  (package
    (inherit clingo)
    (name "python-clingo")
    (version (package-version clingo)) ; for #$version in arguments
    (arguments
     (substitute-keyword-arguments (package-arguments clingo)
       ((#:configure-flags flags #~'())
        #~(cons* "-DCLINGO_BUILD_WITH_PYTHON=pip"
                 (string-append "-DCMAKE_MODULE_PATH="
                                #$(this-package-native-input "python-scikit-build")
                                "/lib/cmake/modules")
                 "-DCLINGO_USE_LIB=yes"
                 #$flags))
       ((#:imported-modules _ '())
        `(,@%cmake-build-system-modules
          (guix build python-build-system)))
       ((#:modules _ '())
        '((guix build cmake-build-system)
          ((guix build python-build-system) #:prefix python:)
          (guix build utils)))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'generate-sources
              (lambda _
                (with-directory-excursion "libpyclingo"
                  (invoke "python" "compile.py" "c"))))
            (add-after 'install 'install-distinfo
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (with-directory-excursion (python:site-packages inputs outputs)
                   (let ((dir (string-append "clingo-" #$version ".dist-info")))
                     (mkdir-p dir)
                     (call-with-output-file (string-append dir "/METADATA")
                       (lambda (port)
                         (format port "Metadata-Version: 1.1~%")
                         (format port "Name: clingo~%")
                         (format port "Version: ~a~%" #$version)))))))))))
    (inputs (list clingo python-wrapper))
    (propagated-inputs (list python-cffi))
    (native-inputs (modify-inputs (package-native-inputs clingo)
                     (prepend python-scikit-build)))
    (synopsis "Python bindings for clingo")
    (description "This package provides Python bindings to the clingo package,
making it so that you can write @acronym{ASPs, Answer Set Programs} through
Python code.")))

(define-public python-clingo-dl
  (package
    (inherit clingo-dl)
    (name "python-clingo-dl")
    (version (package-version clingo-dl))
    (arguments
     (list
      #:configure-flags
      #~(list "-DPYCLINGODL_ENABLE=pip"
              (string-append "-DCMAKE_MODULE_PATH="
                             #$(this-package-native-input "python-scikit-build")
                             "/lib/cmake/modules"))
      #:tests? #f
      #:imported-modules  `(,@%cmake-build-system-modules
                            (guix build python-build-system))
      #:modules '((guix build cmake-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-distinfo
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (with-directory-excursion (python:site-packages inputs outputs)
                (let ((dir (string-append "clingodl-" #$version ".dist-info")))
                  (mkdir-p dir)
                  (call-with-output-file (string-append dir "/METADATA")
                    (lambda (port)
                      (format port "Metadata-Version: 1.1~%")
                      (format port "Name: clingodl~%")
                      (format port "Version: ~a~%" #$version))))))))))
    (inputs (modify-inputs (package-inputs clingo-dl)
              (prepend python-wrapper)))
    (propagated-inputs (list python-clingo python-cffi))
    (native-inputs (modify-inputs (package-native-inputs clingo-dl)
                     (prepend python-scikit-build)))
    (synopsis "Python bindings for clingo-dl")
    (description "This package allows users to add the clingo-dl propagator
as a theory to clingo from Python code.  It also supports running clingo-dl
directly from the python command line.")))

(define-public python-clingox
  (package
    (name "python-clingox")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/potassco/python-clingox")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ji0sdqlv0byxmdipwk60afsb82r0rr1j73r7j2508hsfk94m2i8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; fixture 's' not found
      #~(list "--deselect=clingox/tests/test_ast.py::test_rename"
              "--deselect=clingox/tests/test_ast.py::test_reify"
              "--deselect=clingox/tests/test_ast.py::test_ast_dict")))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs (list python-clingo))
    (home-page "https://potassco.org/clingo")
    (synopsis "Auxiliary functions for Clingo")
    (description
     "This package provides additional functions to go along with the Python
bindings for Clingo.")
    (license license:expat)))

(define-public python-asprin
  (let ((revision "1")
        (commit "bc5a0cf7d9ba346cf91cba66282b5946dbf1331c"))
    (package
     (name "python-asprin")
     (version (git-version "3.1.1" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/asprin")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rf8yjdlpkvzp9917fvhfrrzag47vvfm7j2k5g44w1ggqyrz8fps"))))
     (build-system pyproject-build-system)
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'fix-source
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "asprin/src/solver/metasp/reify.py"
                 (("\"clingo\"")
                  (string-append "\""
                                 (search-input-file inputs "bin/clingo")
                                 "\""))))))))
     (native-inputs
      (list python-setuptools
            python-wheel))
     (inputs
      (list clingo))
     (propagated-inputs
      (list python-clingo))
     (home-page "https://potassco.org/")
     (synopsis "Optimization in Answer Set Programming")
     (description "@command{asprin} is a general framework for optimization in
@acronym{ASP, Answer Set Programming}, that allows for computing optimal
stable models of logic programs by means of preferences.  Some preference types
are already predefined, but more can be added as logic programs.")
     (license license:expat))))

(define-public python-clorm
  (package
    (name "python-clorm")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clorm")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wbxniq60ph7bdaypcaahym7jxmlnm2zhrfmrgrk441i1iaida24"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'fix-breaking-tests
                 (lambda _
                   ;; noclingo tests rely on this being set
                   (setenv "CLORM_NOCLINGO" "1")
                   (delete-file "tests/test_mypy_query.py"))))))
    (propagated-inputs (list python-clingo))
    (native-inputs (list python-typing-extensions python-setuptools python-wheel))
    (home-page "https://potassco.org")
    (synopsis "Object relational mapping to clingo")
    (description "@acronym{Clorm, Clingo ORM} provides an @acronym{ORM,
Object Relational Mapping} interface to the @acronym{ASP, answer set
programming} solver clingo.  Its goal is to make integration of clingo
into Python programs easier.")
    (license license:expat)))

(define-public python-plingo
  (package
    (name "python-plingo")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/plingo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bdz755c6isp29layvzsw9c4kr12x7b5d8ip37ay3cl4dlq4bid3"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-script
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((script (string-append (assoc-ref outputs "out")
                                                "/bin/plingo")))
                     (mkdir-p (dirname script))
                     (call-with-output-file script
                       (lambda (port)
                         (display "#!/usr/bin/env python\n" port)
                         (display "from plingo import main\n" port)
                         (display "main()\n" port)))
                     (chmod script #o755)
                     ;; XXX: Does this cross-compile?
                     (patch-shebang script)))))))
    (propagated-inputs (list python-clingo))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://potassco.org/")
    (synopsis "Solve probabilistic logic programs")
    (description "This package provides a system to solve probabilistic
logic programs with clingo.  It can solve the reasoning tasks of finding
the most probable model as well as finding all models and their probabilities.")
    (license license:expat)))

(define-public python-telingo
  (package
    (name "python-telingo")
    (version "2.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/telingo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q6hlh4b5hsa4n5agvmfa9rhsxfd2g6kpl4b9kfccwbmf6dh51k6"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-clingo))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://potassco.org/")
    (synopsis "Solve dynamic temporal logic programs")
    (description "This package provides a system to solve dynamic temporal
logic programs based on clingo.")
    (license license:expat)))

(define-public python-clingraph
  (package
    (name "python-clingraph")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clingraph")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16q54rkwr84byzy27795rl9z08kcyxsg7lfk017yr8p5axh9a9rr"))))
    (build-system pyproject-build-system)
    (inputs (list dot2tex graphviz))
    (propagated-inputs (list python-clingo
                             python-clorm
                             python-graphviz
                             python-imageio
                             python-jinja2
                             python-jsonschema
                             python-networkx
                             python-setuptools))
    (native-inputs (list dot2tex graphviz python-pylint python-pytest
                         python-wheel))
    (home-page "https://github.com/potassco/clingraph")
    (synopsis "Visualizer for graphs defined as logic programs")
    (description
     "This package provides a clingo-based visualizer for graphs defined
as logic programs.")
    (license license:expat)))

(define-public python-clinguin
  (package
   (name "python-clinguin")
   (version "2.1.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/potassco/clinguin")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0wfgrs8h5i5mmd5sbzca2xw57f3d3ni75775wjkaq6sg0zm9sqjs"))
            (modules '((guix build utils)))
            (snippet
             #~(begin
                 (substitute* "setup.cfg"
                   ;; Fun fact of the day
                   ;; some typo squatter hosted
                   ;; a package named tk
                   (("tk") "")
                   ;; XXX: python-clingo-dl installs clingodl instead…
                   (("clingo-dl") "clingodl"))))))
   (build-system pyproject-build-system)
   (native-inputs
    (list python-setuptools
          python-wheel))
   (propagated-inputs
    (list python-clingo
          python-clingo-dl
          python-clorm
          python-clingexplaid
          python-clingraph
          python-fastapi
          python-httpx
          python-nbconvert
          python-nbformat
          python-nbsphinx
          python-networkx
          python-pillow
          python-pydantic
          `(,python "tk")
          python-sphinx-rtd-theme
          python-traitlets
          python-uvicorn))
   (home-page "https://github.com/potassco/clinguin")
   (synopsis "Clingo-based interactive UI")
   (description "Clinguin is a graphical user interface toolkit for clingo,
which allows user interfaces to be specified entirely as a logic program.")
   (license license:expat)))

(define-public python-clintest
  (package
    (name "python-clintest")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clintest")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k8y3pm3w81n2appfl98drv1hpgjjqi2hxb1aa52y2m831lir38s"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-clingo))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://potassco.org/clintest/")
    (synopsis "Test framework for clingo programs")
    (description "Clintest is a framework for unit testing clingo programs.
It provides various components to assemble the most commonly used tests quickly,
but also works fine along custom-built test.  Clintest monitors the test
outcome while solving to abort the search for solutions once the outcome is
certain.")
    (license license:expat)))

(define-public python-clingexplaid
  (package
    (name "python-clingexplaid")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clingo-explaid")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1s80cs3clvz26r7cvjprlk6zip7yqswwhzzwmmrv5mf5p89ymrgm"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list "-k" "not test_main")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-pyproject-toml
                          (lambda _
                            (substitute* "pyproject.toml"
                              (("dynamic = .*" all)
                               (string-append "version = \""
                                              #$version
                                              "\"\n"))
                              (("\"autoflake\",") "")))))))
    (propagated-inputs (list python-clingo))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/potassco/clingo-explaid")
    (synopsis "Develop explanation systems with Clingo")
    (description "This package provides tools to develop explanation systems
with clingo.  It allows extracting minimal unsatisfiable subsets and
unsatisfiable constraints.")
    (license license:expat)))
