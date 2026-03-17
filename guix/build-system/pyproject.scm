;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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

(define-module (guix build-system pyproject)
  #:use-module ((gnu packages) #:select (search-auxiliary-file))
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (%pyproject-build-system-modules
            default-python
            default-pytest-guix-plugin
            default-sanity-check.py
            pyproject-build
            pyproject-build-system
            pyproject-guile-json))

;; Commentary:
;;
;; Standard build procedure for Python packages using 'pyproject.toml'.
;; This is implemented as an extension of 'python-build-system'.
;;
;; Code:

(define %pyproject-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build pyproject-build-system)
    (guix build toml)
    ,@%default-gnu-imported-modules))

(define (default-python)
  "Return the default Python package, resolved lazily."
  ;; We are using python-sans-pip-wrapper, because it does not contain
  ;; setuptools. This allows us to skip the dependency on setuptools for
  ;; packages which don’t need it. And it allows us to more easily swap
  ;; out setuptools if a different version is required.
  (@* (gnu packages python) python-sans-pip-wrapper))

(define (pyproject-guile-json)
  "Return the default guile-json package, resolved lazily."
  (@* (gnu packages guile) guile-json-4))

;; Maybe try to upstream it at some point, it's currently flavored for guix
;; but the idea itself is more general.
(define (default-pytest-guix-plugin python)
  (let* ((effective (version-major+minor (package-version python)))
         (site (string-append "lib/python" effective "/site-packages/")))
    (package
      (name "python-pytest-guix")
      (version "0.0.1")
      (source (local-file (search-auxiliary-file "python/pytest_guix.py")))
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let* ((site (string-append #$output "/" #$site))
                   (dist (string-append site "pytest_guix-" #$version
                                        ".dist.info")))
              (mkdir-p dist)
              (copy-file #$source (string-append site "/pytest_guix.py"))
              (call-with-output-file (string-append dist "/entry_points.txt")
                (lambda (port)
                  (format port "[pytest11]~%guix=pytest_guix~%")))))))
      (home-page "https://guix.gnu.org/")
      (synopsis "Ignore selected pytest options")
      (description
       "This package provides the script to cleanly ignore pytest options at the
build-system level.")
      (license license:gpl3+))))

(define %default-pytest-guix-options
  #~'(("cov"
       "--cov"
       "--cov-reset"
       "--cov-report"
       "--cov-config"
       "--no-cov-on-fail"
       "--no-cov"
       "--cov-fail-under"
       "--cov-append"
       "--cov-branch"
       "--cov-context")
      ("html"
       "--html" "--self-contained-html" "--css")
      ("mypy"
       "--mypy" "--mypy-config-file" "--mypy-ignore-missing-imports")
      ("isort" "isort")
      ("flake8" "flake8")
      ("black" "black")
      ("flakes" "flakes")
      ("pep8" "pep8")))

;; TODO: On the next iteration of python-team, migrate the sanity-check to
;; importlib_metadata instead of setuptools.
(define (default-sanity-check.py)
  (local-file (search-auxiliary-file "python/sanity-check.py")))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                test-backend
                (python (default-python))
                (python-pytest-guix (default-pytest-guix-plugin python))
                (sanity-check.py (default-sanity-check.py))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:python #:inputs #:native-inputs
      #:python-pytest-guix #:sanity-check.py))
  (define native-inputs-labels (map car native-inputs))
  (define has-pytest?
    (or (member "python-pytest-bootstrap" native-inputs-labels)
        (member "python-pytest" native-inputs-labels)))


  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs
          `(("python" ,python)
            ("sanity-check.py" ,sanity-check.py)
            ,@(if (and has-pytest?
                       (match test-backend
                         ((or 'pytest-with-guix-plugin #f) #t)
                         (_ #f)))
                  `(("python-pytest-guix" ,python-pytest-guix))
                  `())
            ,@native-inputs))
         (outputs (append outputs '(wheel)))
         (build pyproject-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (pyproject-build name inputs
                          #:key source
                          (tests? #t)
                          (configure-flags ''())
                          (backend-path #f)
                          (build-backend #f)
                          (test-backend #f)
                          (test-flags ''())
                          (pytest-guix-options %default-pytest-guix-options)
                          (phases '%standard-phases)
                          (outputs '("out" "wheel"))
                          (search-paths '())
                          (system (%current-system))
                          (guile #f)
                          (guile-json (pyproject-guile-json))
                          (imported-modules %pyproject-build-system-modules)
                          (modules '((guix build pyproject-build-system)
                                     (guix build utils)))
                          allowed-references
                          disallowed-references)
  "Build SOURCE using PYTHON, and with INPUTS."
  (define build
    (with-extensions (list guile-json)
      (with-imported-modules imported-modules
        #~(begin
            (use-modules #$@(sexp->gexp modules))

            #$(with-build-variables inputs outputs
                #~(pyproject-build
                   #:name #$name
                   #:source #+source
                   #:configure-flags #$configure-flags
                   #:system #$system
                   #:backend-path #$backend-path
                   #:build-backend #$build-backend
                   #:test-backend #$test-backend
                   #:test-flags #$test-flags
                   #:tests? #$tests?
                   #$@(let ((labels (map car inputs)))
                        (if (or (member "python-pytest-bootstrap" labels)
                                (member "python-pytest" labels))
                            #~(#:pytest-guix-options #$pytest-guix-options)
                            #~()))
                   #:phases #$(if (pair? phases)
                                  (sexp->gexp phases)
                                  phases)
                   #:outputs %outputs
                   #:search-paths '#$(sexp->gexp
                                      (map search-path-specification->sexp
                                           search-paths))
                   #:inputs %build-inputs))))))


  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name build
                      #:system system
                      #:graft? #f                 ;consistent with 'gnu-build'
                      #:target #f
                      #:guile-for-build guile
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references)))

(define pyproject-build-system
  (build-system
    (name 'pyproject)
    (description "The PEP517-compliant Python build system")
    (lower lower)))

;;; pyproject.scm ends here
