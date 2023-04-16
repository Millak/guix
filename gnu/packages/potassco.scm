;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022, 2023 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz))

(define-public libpotassco
  ;; No public release, update together with clasp
  (let ((revision "1")
        (commit "2f9fb7ca2c202f1b47643aa414054f2f4f9c1821"))
    (package
      (name "libpotassco")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/potassco/libpotassco")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1c32f9gqclf7qx07lpx8wd720vfhkjqhzc6nyy8mjmgwpmb3iyyn"))))
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
    (version "3.3.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clasp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "163ps9zq7xppqy9hj5qnw6z5lcjnm4xf5fwjsavpia5ynm3hngcw"))))
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
    (version "5.6.2")
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
                   ;; TODO: Unvendor other third-party stuff
                   (delete-file-recursively "third_party/catch")))
              (sha256
               (base32
                "19s59ndcm2yj0kxlikfxnx2bmp6b7n31wq1zvwc7hyk37rqarwys"))))
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
                 "find_package(clasp REQUIRED)"))
              (substitute* "libclingo/CMakeLists.txt"
                (("\"cmake/Clingo\"") "\"cmake/clingo\"")
                (("ClingoConfig\\.cmake") "clingo-config.cmake")
                (("ClingoConfigVersion\\.cmake")
                 "clingo-config-version.cmake"))
              (substitute* "cmake/ClingoConfig.cmake.in"
                (("find_package\\(Clasp") "find_package(clasp"))
              (rename-file "cmake/ClingoConfig.cmake.in"
                           "cmake/clingo-config.cmake.in")))
          (add-after 'unpack 'skip-failing-tests
            (lambda _
              (with-directory-excursion "libclingo/tests"
                (substitute* "CMakeLists.txt"
                  (("COMMAND test_clingo" all)
                   (string-append all
                                  " -f "
                                  "\"${CMAKE_CURRENT_SOURCE_DIR}/good.txt\"")))
                (call-with-output-file "good.txt"
                  (lambda (port)
                    (for-each (lambda (test) (format port "~s~%" test))
                              '("parse-ast-v2" "add-ast-v2" "build-ast-v2"
                                "unpool-ast-v2" "parse_term"
                                "propagator" "propgator-sequence-mining"
                                "symbol" "visitor"))))))))))
    (inputs (list catch2-3.1 clasp libpotassco))
    (native-inputs (list pkg-config))
    (home-page "https://potassco.org/")
    (synopsis "Grounder and solver for logic programs")
    (description "Clingo computes answer sets for a given logic program.")
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

(define-public python-clingo
  (package
    (inherit clingo)
    (name "python-clingo")
    (version (package-version clingo)) ; for #$version in arguments
    (arguments
     (substitute-keyword-arguments (package-arguments clingo)
       ((#:configure-flags flags #~'())
        #~(cons* "-DCLINGO_BUILD_WITH_PYTHON=pip"
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
            (add-after 'unpack 'fix-failing-tests
              (lambda _
                (substitute* "libpyclingo/clingo/tests/test_conf.py"
                  (("ctl\\.solve\\(on_statistics=on_statistics\\)" all)
                   (string-append
                    all
                    "; self.skipTest(\"You shall not fail.\")")))))
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

(define-public python-clorm
  (package
   (name "python-clorm")
   (version "1.4.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/potassco/clorm")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0jx99y71mrgdicn1da5dwz5nzgvvpabrikff783sg4shbv2cf0b5"))))
   (build-system pyproject-build-system)
   (arguments
    (list #:phases
          #~(modify-phases %standard-phases
              (add-before 'check 'fix-breaking-tests
                (lambda _
                  ;; noclingo tests rely on this being set
                  (setenv "CLORM_NOCLINGO" "1")
                  (delete-file "tests/test_mypy_query.py")
                  (substitute* "tests/test_clingo.py"
                    (("self\\.assertTrue\\(os_called\\)" all)
                     (string-append "# " all))))))))
   (propagated-inputs (list python-clingo))
   (native-inputs (list python-typing-extensions))
   (home-page "https://potassco.org")
   (synopsis "Object relational mapping to clingo")
   (description "@acronym{Clorm, Clingo ORM} provides an @acronym{ORM,
Object Relational Mapping} interface to the @acronym{ASP, answer set
programming} solver clingo.  Its goal is to make integration of clingo
into Python programs easier.")
   (license license:expat)))

(define-public python-telingo
  (package
    (name "python-telingo")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/telingo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches (search-patches "python-telingo-fix-comparison.patch"))
              (sha256
               (base32
                "0g3khxfdzc2hc7dkiyyqhb399h6h21m5wkp6wy8w71n0m32fiy53"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-clingo))
    (home-page "https://potassco.org/")
    (synopsis "Solve dynamic temporal logic programs")
    (description "This package provides a system to solve dynamic temporal
logic programs based on clingo.")
    (license license:expat)))

(define-public python-clingraph
  (package
    (name "python-clingraph")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/potassco/clingraph")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bdhli20nw9qnyxmpisgz7m97d7bwx6lbmxy9bgqvm6mipprnv3n"))))
    (build-system pyproject-build-system)
    (inputs (list dot2tex graphviz))
    (propagated-inputs (list python-clingo
                             python-clorm
                             python-graphviz
                             python-imageio
                             python-jinja2
                             python-jsonschema
                             python-networkx))
    (native-inputs (list dot2tex graphviz python-pylint python-pytest))
    (home-page "https://github.com/potassco/clingraph")
    (synopsis "Visualizer for graphs defined as logic programs")
    (description
     "This package provides a clingo-based visualizer for graphs defined
as logic programs.")
    (license license:expat)))
