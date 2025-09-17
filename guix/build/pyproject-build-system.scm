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

(define-module (guix build pyproject-build-system)
  #:use-module ((guix build python-build-system) #:prefix python:)
  #:use-module ((guix build utils) #:hide (delete))
  #:use-module (guix build json)
  #:use-module (guix build toml)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (%standard-phases
            add-installed-pythonpath
            site-packages
            python-version
            pyproject-build))

;;; Commentary:
;;;
;;; PEP 517-compatible build system for Python packages.
;;;
;;; PEP 517 mandates the use of a TOML file called pyproject.toml at the
;;; project root, describing build and runtime dependencies, as well as the
;;; build system, which can be different from setuptools. This module uses
;;; that file to extract the build system used and call its wheel-building
;;; entry point build_wheel (see 'build). setuptools’ wheel builder is
;;; used as a fallback if either no pyproject.toml exists or it does not
;;; declare a build-system. It supports config_settings through the
;;; standard #:configure-flags argument.
;;;
;;; This wheel, which is just a ZIP file with a file structure defined
;;; by PEP 427 (https://www.python.org/dev/peps/pep-0427/), is then unpacked
;;; and its contents are moved to the appropriate locations in 'install.
;;;
;;; Then entry points, as defined by the PyPa Entry Point Specification
;;; (https://packaging.python.org/specifications/entry-points/) are read
;;; from a file called entry_points.txt in the package’s site-packages
;;; subdirectory and scripts are written to bin/. These are not part of a
;;; wheel and expected to be created by the installing utility.
;;; TODO: Add support for PEP-621 entry points.
;;;
;;; This module also supports in-tree build backends, which can be
;;; overridden by #:backend-path.
;;;
;;; Code:
;;;

;; Re-export these variables from python-build-system as many packages
;; rely on these.
(define python-version python:python-version)
(define site-packages python:site-packages)
(define add-installed-pythonpath python:add-installed-pythonpath)

;; Base error type.
(define-condition-type &python-build-error &error python-build-error?)

;; Raised when 'check cannot find a valid test system in the inputs.
(define-condition-type &test-system-not-found &python-build-error
  test-system-not-found?)

;; Raised when multiple wheels are created by 'build.
(define-condition-type &cannot-extract-multiple-wheels &python-build-error
  cannot-extract-multiple-wheels?)

;; Raised, when no wheel has been built by the build system.
(define-condition-type &no-wheels-built &python-build-error no-wheels-built?)

;; Raised, when no installation candidate wheel has been found.
(define-condition-type &no-wheels-found &python-build-error no-wheels-found?)

(define* (build #:key outputs build-backend backend-path configure-flags #:allow-other-keys)
  "Build a given Python package."

  (let* ((wheel-output (assoc-ref outputs "wheel"))
         (wheel-dir (if wheel-output wheel-output "dist"))
         (pyproject.toml (if (file-exists? "pyproject.toml")
                             (parse-toml-file "pyproject.toml")
                             '()))
         ;; backend-path is prepended to sys.path, so in-tree backends can be
         ;; found. We assume toml is json-compatible and do not encode the resulting
         ;; JSON list expression.
         (auto-backend-path (recursive-assoc-ref
                             pyproject.toml
                             '("build-system" "backend-path")))
         (use-backend-path (call-with-output-string
                             (cut write-json
                                  (or backend-path auto-backend-path '()) <>)))
         ;; There is no easy way to get data from Guile into Python via
         ;; s-expressions, but we have JSON serialization already, which Python
         ;; also supports out-of-the-box.
         (config-settings (call-with-output-string
                            (cut write-json configure-flags <>)))
         ;; python-setuptools’ default backend supports setup.py *and*
         ;; pyproject.toml. Allow overriding this automatic detection via
         ;; build-backend.
         (auto-build-backend (recursive-assoc-ref
                              pyproject.toml
                              '("build-system" "build-backend")))
         ;; Use build system detection here and not in importer, because a) we
         ;; have alot of legacy packages and b) the importer cannot update arbitrary
         ;; fields in case a package switches its build system.
         (use-build-backend (or build-backend
                                auto-build-backend
                                "setuptools.build_meta")))
    (format #t
            (string-append
             "Using '~a' to build wheels, auto-detected '~a', override '~a'.~%"
             "Prepending '~a' to sys.path, auto-detected '~a', override '~a'.~%")
            use-build-backend auto-build-backend build-backend
            use-backend-path auto-backend-path backend-path)
    (mkdir-p wheel-dir)
    ;; Call the PEP 517 build function, which drops a .whl into wheel-dir.
    (invoke "python" "-c"
            "import sys, importlib, json
backend_path = json.loads (sys.argv[1]) or []
backend_path.extend (sys.path)
sys.path = backend_path
config_settings = json.loads (sys.argv[4])
builder = importlib.import_module(sys.argv[2])
builder.build_wheel(sys.argv[3], config_settings=config_settings)"
            use-backend-path
            use-build-backend
            wheel-dir
            config-settings)))

(define* (check #:key tests? test-backend test-flags #:allow-other-keys)
  "Run the test suite of a given Python package."
  (if tests?
      ;; Unfortunately with PEP 517 there is no common method to specify test
      ;; systems.  Guess test system based on inputs instead.
      (let* ((pytest (which "pytest"))
             (nosetests (which "nosetests"))
             (nose2 (which "nose2"))
             (stestr (which "stestr"))
             (have-setup-py (file-exists? "setup.py"))
             ;; unittest default pattern
             ;; See https://docs.python.org/3/library/unittest.html\
             ;; #cmdoption-unittest-discover-p
             (tests-found (find-files "." "test.*\\.py$"))
             (use-test-backend
              (or test-backend
                  ;; Prefer pytest
                  (if pytest 'pytest #f)
                  (if stestr 'stestr #f)
                  (if nosetests 'nose #f)
                  (if nose2 'nose2 #f)
                  ;; Fall back to setup.py. The command is deprecated, but is
                  ;; a superset of unittest, so should work for most packages.
                  ;; Keep it until setuptools removes `setup.py test'.
                  ;; See https://setuptools.pypa.io/en/latest/deprecated/\
                  ;; commands.html#test-build-package-and-run-a-unittest-suite
                  (if have-setup-py 'setup.py #f)
                  (if tests-found 'unittest #f))))
        (format #t "Using ~a~%" use-test-backend)
        (match use-test-backend
          ('pytest
           (apply invoke pytest "-vv" test-flags))
          ('nose
           (apply invoke nosetests "-v" test-flags))
          ('nose2
           (apply invoke nose2 "-v" "--pretty-assert" test-flags))
          ('setup.py
           (apply invoke "python" "setup.py"
                  (if (null? test-flags)
                      '("test" "-v")
                      test-flags)))
          ('stestr
           (apply invoke stestr "run" test-flags))
          ('unittest
           (apply invoke "python" "-m" "unittest" test-flags))
          ('custom
           (apply invoke "python" test-flags))
          ;; The developer should explicitly disable tests in this case.
          (else (raise (condition (&test-system-not-found))))))
      (format #t "test suite not run~%")))

(define* (install #:key inputs outputs #:allow-other-keys)
  "Install a wheel file according to PEP 427."
  ;; See <https://packaging.python.org/en/latest/specifications/\
  ;; binary-distribution-format/#binary-distribution-format>.
  (let ((site-dir (site-packages inputs outputs))
        (python (assoc-ref inputs "python"))
        (out (assoc-ref outputs "out")))
    (define (extract file)
      "Extract wheel (ZIP file) into site-packages directory"
      ;; Use Python’s zipfile to avoid extra dependency
      (invoke "python" "-m" "zipfile" "-e" file site-dir))

    (define (expand-data-directory directory)
      "Move files from all .data subdirectories to their respective\ndestinations."
      ;; Python’s distutils.command.install defines this mapping from source to
      ;; destination mapping.
      (let ((source (string-append directory "/scripts"))
            (destination (string-append out "/bin")))
        (when (file-exists? source)
          (copy-recursively source destination)
          (delete-file-recursively source)
          (for-each
           (lambda (file)
             (chmod file #o755)
             ;; PEP 427 recommends that installers rewrite
             ;; this odd shebang, but avoid the binary case.
             (unless (elf-file? file)
               (substitute* file
                 (("#!python")
                  (string-append "#!" python "/bin/python")))))
           (find-files destination))))
      ;; Data can be contained in arbitrary directory structures.  Most
      ;; commonly it is used for share/.
      (let ((source (string-append directory "/data"))
            (destination out))
        (when (file-exists? source)
          (copy-recursively source destination)
          (delete-file-recursively source)))
      (let* ((distribution (car (string-split (basename directory) #\-)))
             (source (string-append directory "/headers"))
             (destination (string-append out "/include/python"
                                         (python-version python)
                                         "/" distribution)))
        (when (file-exists? source)
          (copy-recursively source destination)
          (delete-file-recursively source))))

    (define (list-directories base predicate)
      ;; Cannot use find-files here, because it’s recursive.
      (scandir base
               (lambda (name)
                 (let ((stat (lstat (string-append base "/" name))))
                   (and (not (member name '("." "..")))
                        (eq? (stat:type stat) 'directory)
                        (predicate name stat))))))

    (let* ((wheel-output (assoc-ref outputs "wheel"))
           (wheel-dir (if wheel-output wheel-output "dist"))
           (wheels-found (or (scandir wheel-dir
                                      (cut string-suffix? ".whl" <>))
                             '()))
           (wheels (map (cut string-append wheel-dir "/" <>) wheels-found)))
      (cond
       ;; This can happen if the 'build phase has been changed or when using
       ;; the install phase using an alternative build-system.
       ((null? wheels-found)
        (raise (condition (&no-wheels-found))))
       ((> (length wheels) 1)
        ;; This code does not support multiple wheels yet, because their
        ;; outputs would have to be merged properly.
        (raise (condition (&cannot-extract-multiple-wheels))))
       ((= (length wheels) 0)
        (raise (condition (&no-wheels-built)))))
      (for-each extract wheels))
    (let ((datadirs (map (cut string-append site-dir "/" <>)
                         (list-directories site-dir
                                           (file-name-predicate "\\.data$")))))
      (for-each (lambda (directory)
                  (expand-data-directory directory)
                  (rmdir directory)) datadirs))))

(define* (compile-bytecode #:key inputs outputs #:allow-other-keys)
  "Compile installed byte-code in site-packages."
  (let* ((site-dir (site-packages inputs outputs))
         (python (assoc-ref inputs "python"))
         (major-minor (map string->number
                           (take (string-split (python-version python) #\.) 2)))
         (<3.7? (match major-minor
                  ((major minor)
                   (or (< major 3)
                       (and (= major 3)
                            (< minor 7)))))))
    (if <3.7?
        ;; These versions don’t have the hash invalidation modes and do
        ;; not produce reproducible bytecode files.
        (format #t "Skipping bytecode compilation for Python version ~a < 3.7~%"
                (python-version python))
        (invoke "python" "-m" "compileall"
                "--invalidation-mode=unchecked-hash" site-dir))))

(define* (create-entrypoints #:key inputs outputs #:allow-other-keys)
  "Implement Entry Points Specification
(https://packaging.python.org/specifications/entry-points/) by PyPa,
which creates runnable scripts in bin/ from entry point specification
file entry_points.txt.  This is necessary, because wheels do not contain
these binaries and installers are expected to create them."

  (define (entry-points.txt->entry-points file)
    "Specialized parser for Python configfile-like files, in particular
entry_points.txt.  Returns a list of console_script and gui_scripts
entry points."
    (call-with-input-file file
      (lambda (in)
        (let loop ((line (read-line in))
                   (inside #f)
                   (result '()))
          (if (eof-object? line)
              result
              (let* ((group-match (string-match "^\\[(.+)\\]$" line))
                     (group-name (if group-match
                                     (match:substring group-match 1)
                                     #f))
                     (next-inside (if (not group-name)
                                      inside
                                      (or (string=? group-name
                                                    "console_scripts")
                                          (string=? group-name "gui_scripts"))))
                     (item-match (string-match
                                  "^([^ =]+)\\s*=\\s*([^:]+):(.+)$" line)))
                (if (and inside item-match)
                    (loop (read-line in)
                          next-inside
                          (cons (list (match:substring item-match 1)
                                      (match:substring item-match 2)
                                      (match:substring item-match 3))
                                result))
                    (loop (read-line in) next-inside result))))))))

  (define (create-script path name module function)
    "Create a Python script from an entry point’s NAME, MODULE and FUNCTION
and return write it to PATH/NAME."
    (let ((interpreter (which "python"))
          (file-path (string-append path "/" name)))
      (format #t "Creating entry point for '~a.~a' at '~a'.~%"
              module function file-path)
      (call-with-output-file file-path
        (lambda (port)
          ;; Technically the script could also include search-paths,
          ;; but having a generic 'wrap phases also handles manually
          ;; written entry point scripts.
          (format port "#!~a -sP
# Auto-generated entry point script.
import sys
from ~a import ~a
if __name__ == '__main__':
    sys.exit(~a())~%" interpreter module function function)))
      (chmod file-path #o755)))

  (let* ((site-dir (site-packages inputs outputs))
         (out (assoc-ref outputs "out"))
         (bin-dir (string-append out "/bin"))
         (entry-point-files (find-files site-dir "^entry_points.txt$")))
    (mkdir-p bin-dir)
    (for-each (lambda (f)
                (for-each (lambda (ep)
                            (apply create-script
                                   (cons bin-dir ep)))
                          (entry-points.txt->entry-points f)))
              entry-point-files)))

(define* (set-SOURCE-DATE-EPOCH* #:rest _)
  "Set the 'SOURCE_DATE_EPOCH' environment variable.  This is used by tools
that incorporate timestamps as a way to tell them to use a fixed timestamp.
See https://reproducible-builds.org/specs/source-date-epoch/."
  ;; Use a post-1980 timestamp because the Zip format used in wheels do
  ;; not support timestamps before 1980.
  (setenv "SOURCE_DATE_EPOCH" "315619200"))

(define %standard-phases
  ;; The build phase only builds C extensions and copies the Python sources,
  ;; while the install phase copies then byte-compiles the sources to the
  ;; prefix directory.  The check phase is moved after the installation phase
  ;; to ease testing the built package.
  (modify-phases python:%standard-phases
    (replace 'set-SOURCE-DATE-EPOCH set-SOURCE-DATE-EPOCH*)
    (replace 'build build)
    (replace 'install install)
    (delete 'check)
    ;; Must be before tests, so they can use installed packages’ entry points.
    (add-before 'wrap 'create-entrypoints create-entrypoints)
    (add-after 'wrap 'check check)
    (add-before 'check 'compile-bytecode compile-bytecode)))

(define* (pyproject-build #:key inputs (phases %standard-phases)
                          #:allow-other-keys #:rest args)
  "Build the given Python package, applying all of PHASES in order."
  (apply python:python-build #:inputs inputs #:phases phases args))

;;; pyproject-build-system.scm ends here
