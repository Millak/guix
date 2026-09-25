;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (guix build typst-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((guix build copy-build-system) #:prefix copy:)
  #:use-module (guix build toml)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (%standard-phases
            typst-package-dir
            call-with-typst-package-path
            typst-build))

;; Base error type.
(define-condition-type &typst-build-error &error typst-build-error?)

;; Raised when 'install cannot find the built source to install
;; and nothing has been installed yet.
(define-condition-type &nothing-to-install &typst-build-error
  nothing-to-install?)

;; Raised when 'check cannot find a valid test system in the inputs.
(define-condition-type &test-system-not-found &typst-build-error
  test-system-not-found?)

(define (typst-output outputs)
  "Return the path of the typst output, if there is one, or fall-back to out."
  (or (assoc-ref outputs "typst") (assoc-ref outputs "out")))

(define (typst-package-dir input)
  (string-append input "/share/typst/packages"))

(define (call-with-typst-package-path path thunk)
  "Set $TYPST_PACKAGE_PATH to PATH while evaluating THUNK.

Note that TYPST_PACKAGE_PATH only supports a single directory, and some build
scripts assume it to be writable."
  (let ((restore (getenv "TYPST_PACKAGE_PATH")))
    (dynamic-wind
      (lambda () (setenv "TYPST_PACKAGE_PATH" path))
      thunk
      (lambda () (setenv "TYPST_PACKAGE_PATH" restore)))))

(define* (add-install-to-package-path #:key outputs #:allow-other-keys)
  "Prepend the typst output to GUIX_TYPST_PACKAGE_PATH."
  (let ((package-path (getenv "GUIX_TYPST_PACKAGE_PATH")))
    (setenv "GUIX_TYPST_PACKAGE_PATH"
            (string-append
             (typst-output outputs) "/share/typst/packages"
             (or (and package-path ":") "") (or package-path "")))))

(define* (install/just #:key outputs (namespace "preview") #:allow-other-keys)
  "Use \"just\" to build @NAMESPACE/PACKAGE.

Optionally install the namespace, if the namespace directory is produced
locally.  Raise an error if the namespace directory can not be found."
  (let ((destination (typst-package-dir (typst-output outputs))))
    (invoke "just" "package" namespace)
    (cond
     ((file-exists? destination) #t)
     ((file-exists? namespace)
      (copy-recursively namespace (string-append destination "/" namespace)))
     (else (raise (condition (&nothing-to-install)))))))

(define* (install/toml #:key (namespace "preview") outputs #:allow-other-keys)
  "Install the package under NAMESPACE according to TOML metadata."
  (let* ((toml (parse-toml-file "typst.toml"))
         (name (recursive-assoc-ref toml '("package" "name")))
         (version (recursive-assoc-ref toml '("package" "version")))
         (entry-point (recursive-assoc-ref toml '("package" "entrypoint")))
         (entry-dir (dirname entry-point))
         (exclude (false-if-exception (recursive-assoc-ref toml '("package" "exclude")))))
    (when exclude
      (display "WARNING: exclude globs are not yet supported\n"))

    ((assoc-ref copy:%standard-phases 'install)
     #:install-plan
     (list
      (list
       "."
       (string-append "share/typst/packages/" namespace "/" name "/" version)
       #:include (if (string=? entry-dir ".") '() (list entry-dir))
       #:include-regexp `("^README" "^LICENSE" "typst\\.toml$"
                          ,@(if (string=? entry-dir ".") '("\\.typ$") '()))
       #:exclude exclude
       #:output (if (member "typst" outputs) "typst" "out")))
     #:outputs outputs)))

(define* (install #:key (namespace "preview") build-backend outputs
                  #:allow-other-keys #:rest args)
  "Use BUILD_BACKEND (or guess) to install the package under NAMESPACE."
  (case build-backend
    ((just) (apply install/just args))
    ((toml) (apply install/toml args))
    (else
     (if (and (any file-exists? (list "Justfile" "justfile"))
              (which "just"))
         (apply install/just args)
         (apply install/toml args)))))

(define* (check #:key tests? test-backend (test-flags '()) #:allow-other-keys)
  "Use TEST_BACKEND to run tests for the provided typst package."
  (if tests?
      (case (or test-backend
                (and (any file-exists? (list "Justfile" "justfile"))
                     'just)
                (and (which "tt") 'tt))
        ((just) (apply invoke "just" "test" test-flags))
        ((tt) (apply invoke "tt" "run" test-flags))
        (else (raise (condition (&test-system-not-found)))))
      (display "test suite not run\n")))

(define* (sanity-check #:key outputs (namespace "preview") #:allow-other-keys)
  "Perform a sanity check to prove that installed packages can be loaded."
  (for-each
   (lambda (file)
     (let* ((toml (parse-toml-file file))
            (name (recursive-assoc-ref toml '("package" "name")))
            (version (recursive-assoc-ref toml '("package" "version"))))
       (invoke "typst" "eval"
               (format #f "import \"@~a/~a:~a\" as m; m"
                       namespace name version))))
   (find-files (string-append (typst-output outputs) "/share/typst/packages")
               "typst\\.toml$")))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'build)
    (replace 'install install)
    (add-after 'install 'add-install-to-package-path
      add-install-to-package-path)
    (add-after 'add-install-to-package-path 'sanity-check sanity-check)
    (replace 'check check)))

(define* (typst-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Typst package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
