;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2022 Vivien Kraus <vivien@planete-kraus.eu>
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

(define-module (test-pypi)
  #:use-module (guix import pypi)
  #:use-module (guix base32)
  #:use-module (guix memoization)
  #:use-module (guix utils)
  #:use-module ((guix base16) #:select (base16-string->bytevector))
  #:use-module (guix upstream)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module (guix tests http)
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix build-system python)
  #:use-module ((guix build utils)
                #:select (delete-file-recursively
                          which mkdir-p dump-port
                          with-directory-excursion))
  #:use-module ((guix diagnostics) #:select (guix-warning-port))
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module (json)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs))

(define default-sha256
  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(define default-sha256/base32
  (bytevector->nix-base32-string
   (base16-string->bytevector default-sha256)))

(define* (foo-json #:key (name "foo") (name-in-url #f))
  "Create a JSON description of an example pypi package, named @var{name},
optionally using a different @var{name in its URL}."
  (scm->json-string
   `((info
      . ((version . "1.0.0")
         (name . ,name)
         (license . "GNU LGPL")
         (summary . "summary")
         (home_page . "http://example.com")
         (classifiers . #())
         (download_url . "")))
     (urls . #())
     (releases
      . ((1.0.0
          . #(((url . ,(format #f "~a/~a-1.0.0.egg"
                               (%local-url #:path "")
                               (or name-in-url name)))
               (packagetype . "bdist_egg"))
              ((url . ,(format #f "~a/~a-1.0.0.tar.gz"
                               (%local-url #:path "")
                               (or name-in-url name)))
               (packagetype . "sdist")
               (digests . (("sha256" . ,default-sha256))))
              ((url . ,(format #f "~a/~a-1.0.0-py2.py3-none-any.whl"
                               (%local-url #:path "")
                               (or name-in-url name)))
               (packagetype . "bdist_wheel")))))))))

(define test-specifications
  '("Fizzy [foo, bar]"
    "PickyThing<1.6,>1.9,!=1.9.6,<2.0a0,==2.4c1"
    "SomethingWithMarker[foo]>1.0;python_version<\"2.7\""
    "requests [security,tests] >= 2.8.1, == 2.8.* ; python_version < \"2.7\""
    "pip @ https://github.com/pypa/pip/archive/1.3.1.zip#\
sha1=da9234ee9982d4bbb3c72346a6de940a148ea686"))

(define test-requires.txt "\
# A comment
 # A comment after a space
foo ~= 3
bar != 2

[test]
pytest (>=2.5.0)
")

;; Beaker contains only optional dependencies.
(define test-requires.txt-beaker "\
[crypto]
pycryptopp>=0.5.12

[cryptography]
cryptography

[testsuite]
Mock
coverage
")

(define test-metadata "\
Classifier: Programming Language :: Python :: 3.7
Requires-Dist: baz ~= 3
Requires-Dist: bar != 2
Provides-Extra: test
Requires-Dist: pytest (>=2.5.0) ; extra == 'test'
")

(define test-metadata-with-extras "
Classifier: Programming Language :: Python :: 3.7
Requires-Python: >=2.7, !=3.0.*, !=3.1.*, !=3.2.*, !=3.3.*
Requires-Dist: wrapt (<2,>=1)
Requires-Dist: bar

Provides-Extra: dev
Requires-Dist: tox ; extra == 'dev'
Requires-Dist: bumpversion (<1) ; extra == 'dev'
")

;;; Provides-Extra can appear before Requires-Dist.
(define test-metadata-with-extras-jedi "\
Requires-Python: >=2.7, !=3.0.*, !=3.1.*, !=3.2.*, !=3.3.*
Provides-Extra: testing
Requires-Dist: parso (>=0.3.0)
Provides-Extra: testing
Requires-Dist: pytest (>=3.1.0); extra == 'testing'
")

(define sample-directory
  ;; Directory containing tarballs and .whl files for this test.
  (let ((template (string-append (or (getenv "TMPDIR") "/tmp")
                                 "/guix-pypi-test-XXXXXX")))
    (mkdtemp! template)))

(define (pypi-tarball name specs)
  "Return a PyPI tarball called NAME suffixed with '.tar.gz' and containing
the files specified in SPECS.  Return its file name."
  (let ((directory (in-vicinity sample-directory name))
        (tarball (in-vicinity sample-directory (string-append name ".tar.gz"))))
    (false-if-exception (delete-file tarball))
    (mkdir-p directory)
    (for-each (match-lambda
                ((file content)
                 (mkdir-p (in-vicinity directory (dirname file)))
                 (call-with-output-file (in-vicinity directory file)
                   (lambda (port)
                     (display content port)))))
              specs)
    (parameterize ((current-output-port (%make-void-port "w0")))
      (system* "tar" "-C" sample-directory "-czvf" tarball
               (basename directory)))
    (delete-file-recursively directory)
    tarball))

(define (wheel-file name specs)
  "Return a Wheel file called NAME suffixed with '.whl' and containing the
files specified by SPECS.  Return its file name."
  (let* ((directory (in-vicinity sample-directory
                                 (string-append name ".dist-info")))
         (zip-file (in-vicinity sample-directory
                                (string-append name ".zip")))
         (whl-file (in-vicinity sample-directory
                                (string-append name ".whl"))))
    (false-if-exception (delete-file whl-file))
    (mkdir-p directory)
    (for-each (match-lambda
                ((file content)
                 (mkdir-p (in-vicinity directory (dirname file)))
                 (call-with-output-file (in-vicinity directory file)
                   (lambda (port)
                     (display content port)))))
              specs)
    ;; zip always adds a "zip" extension to the file it creates,
    ;; so we need to rename it.
    (with-directory-excursion (dirname directory)
      (system* "zip" "-qr" zip-file (basename directory)))
    (rename-file zip-file whl-file)
    (delete-file-recursively directory)
    whl-file))

(define (file-dump file)
  "Return a procedure that dumps FILE to the given port."
  (lambda (output)
    (call-with-input-file file
      (lambda (input)
        (dump-port input output)))))

(define-syntax-rule (with-pypi responses body ...)
  (with-http-server responses
    (parameterize ((%pypi-base-url (%local-url #:path "/")))
      body ...)))


(test-begin "pypi")

(test-equal "guix-package->pypi-name, old URL style"
  "psutil"
  (guix-package->pypi-name
   (dummy-package "foo"
                  (source (dummy-origin
                           (uri
                            "https://pypi.org/packages/source/p/psutil/psutil-4.3.0.tar.gz"))))))

(test-equal "guix-package->pypi-name, new URL style"
  "certbot"
  (guix-package->pypi-name
   (dummy-package "foo"
                  (source (dummy-origin
                           (uri
                            "https://pypi.org/packages/a2/3b/4756e6a0ceb14e084042a2a65c615d68d25621c6fd446d0fc10d14c4ce7d/certbot-0.8.1.tar.gz"))))))

(test-equal "guix-package->pypi-name, several URLs"
  "cram"
  (guix-package->pypi-name
   (dummy-package "foo"
                  (source
                   (dummy-origin
                    (uri (list "https://bitheap.org/cram/cram-0.7.tar.gz"
                               (pypi-uri "cram" "0.7"))))))))

(test-equal "guix-package->pypi-name, honor 'upstream-name'"
  "bar-3"
  (guix-package->pypi-name
   (dummy-package "foo"
                  (properties
                   '((upstream-name . "bar-3"))))))

(test-equal "specification->requirement-name"
  '("Fizzy" "PickyThing" "SomethingWithMarker" "requests" "pip")
  (map specification->requirement-name test-specifications))

(test-equal "parse-requires.txt"
  (list '("foo" "bar") '("pytest"))
  (mock ((ice-9 ports) call-with-input-file
         call-with-input-string)
        (parse-requires.txt test-requires.txt)))

(test-equal "parse-requires.txt - Beaker"
  (list '() '("Mock" "coverage"))
  (mock ((ice-9 ports) call-with-input-file
         call-with-input-string)
        (parse-requires.txt test-requires.txt-beaker)))

(test-equal "parse-wheel-metadata, with extras"
  (list '("wrapt" "bar") '("tox" "bumpversion"))
  (mock ((ice-9 ports) call-with-input-file
         call-with-input-string)
        (parse-wheel-metadata test-metadata-with-extras)))

(test-equal "parse-wheel-metadata, with extras - Jedi"
  (list '("parso") '("pytest"))
  (mock ((ice-9 ports) call-with-input-file
         call-with-input-string)
        (parse-wheel-metadata test-metadata-with-extras-jedi)))

(test-equal "find-project-url, with numpy"
  "numpy"
  (find-project-url
   "numpy"
   "https://files.pythonhosted.org/packages/0a/c8/a62767a6b374a0dfb02d2a0456e5f56a372cdd1689dbc6ffb6bf1ddedbc0/numpy-1.22.1.zip"))

(test-equal "find-project-url, uWSGI"
  "uwsgi"
  (find-project-url
   "uWSGI"
   "https://files.pythonhosted.org/packages/24/fd/93851e4a076719199868d4c918cc93a52742e68370188c1c570a6e42a54f/uwsgi-2.0.20.tar.gz"))

(test-equal "find-project-url, flake8-array-spacing"
  "flake8_array_spacing"
  (find-project-url
   "flake8-array-spacing"
   "https://files.pythonhosted.org/packages/a4/21/ff29b901128b681b7de7a2787b3aeb3e1f3cba4a8c0cffa9712cbff016bc/flake8_array_spacing-0.2.0.tar.gz"))

(test-equal "find-project-url, foo/goo"
  "foo"
  (find-project-url
   "foo"
   "https://files.pythonhosted.org/packages/f0/f00/goo-0.0.0.tar.gz"))

(test-assert "pypi->guix-package, no wheel"
  (let ((tarball (pypi-tarball
                  "foo-1.0.0"
                  `(("src/bizarre.egg-info/requires.txt"
                     ,test-requires.txt))))
        (twice (lambda (lst) (append lst lst))))
    (with-pypi (twice `(("/foo-1.0.0.tar.gz" 200 ,(file-dump tarball))
                        ("/foo-1.0.0-py2.py3-none-any.whl" 404 "")
                        ("/foo/json" 200 ,(lambda (port)
                                            (display (foo-json) port)))))
      (match (pypi->guix-package "foo")
        (`(package
            (name "python-foo")
            (version "1.0.0")
            (source (origin
                      (method url-fetch)
                      (uri (pypi-uri "foo" version))
                      (sha256
                       (base32 ,(? string? hash)))))
            (build-system pyproject-build-system)
            (propagated-inputs (list python-bar python-foo))
            (native-inputs (list python-pytest))
            (home-page "http://example.com")
            (synopsis "summary")
            (description "summary.")
            (license license:lgpl2.0))
         (and (string=? default-sha256/base32 hash)
              (equal? (pypi->guix-package "foo" #:version "1.0.0")
                      (pypi->guix-package "foo"))
              (guard (c ((error? c) #t))
                (pypi->guix-package "foo" #:version "42"))))
        (x
         (pk 'fail x #f))))))

(test-skip (if (which "zip") 0 1))
(test-assert "pypi->guix-package, wheels"
  (let ((tarball (pypi-tarball
                  "foo-1.0.0"
                  '(("foo-1.0.0/foo.egg-info/requires.txt"
                     "wrong data \
to make sure we're testing wheels"))))
        (wheel (wheel-file "foo-1.0.0"
                           `(("METADATA" ,test-metadata)))))
    (with-pypi `(("/foo-1.0.0.tar.gz" 200 ,(file-dump tarball))
                 ("/foo-1.0.0-py2.py3-none-any.whl"
                  200 ,(file-dump wheel))
                 ("/foo/json" 200 ,(lambda (port)
                                     (display (foo-json) port))))
      ;; Not clearing the memoization cache here would mean returning the value
      ;; computed in the previous test.
      (invalidate-memoization! pypi->guix-package)
      (match (pypi->guix-package "foo")
        (`(package
            (name "python-foo")
            (version "1.0.0")
            (source (origin
                      (method url-fetch)
                      (uri (pypi-uri "foo" version))
                      (sha256
                       (base32 ,(? string? hash)))))
            (build-system pyproject-build-system)
            (propagated-inputs (list python-bar python-baz))
            (native-inputs (list python-pytest))
            (home-page "http://example.com")
            (synopsis "summary")
            (description "summary.")
            (license license:lgpl2.0))
         (string=? default-sha256/base32 hash))
        (x
         (pk 'fail x #f))))))

(test-assert "pypi->guix-package, no usable requirement file."
  (let ((tarball (pypi-tarball "foo-1.0.0"
                               '(("foo.egg-info/.empty" "")))))
    (with-pypi `(("/foo-1.0.0.tar.gz" 200 ,(file-dump tarball))
                 ("/foo-1.0.0-py2.py3-none-any.whl" 404 "")
                 ("/foo/json" 200 ,(lambda (port)
                                     (display (foo-json) port))))
      ;; Not clearing the memoization cache here would mean returning the
      ;; value computed in the previous test.
      (invalidate-memoization! pypi->guix-package)
      (match (pypi->guix-package "foo")
        (`(package
            (name "python-foo")
            (version "1.0.0")
            (source (origin
                       (method url-fetch)
                       (uri (pypi-uri "foo" version))
                       (sha256
                        (base32 ,(? string? hash)))))
            (build-system pyproject-build-system)
            (home-page "http://example.com")
            (synopsis "summary")
            (description "summary.")
            (license license:lgpl2.0))
         (string=? default-sha256/base32 hash))
        (x
         (pk 'fail x #f))))))

(test-assert "pypi->guix-package, package name contains \"-\" followed by digits"
  (let ((tarball (pypi-tarball "foo-99-1.0.0"
                               `(("src/bizarre.egg-info/requires.txt"
                                  ,test-requires.txt)))))
    (with-pypi `(("/foo-99-1.0.0.tar.gz" 200 ,(file-dump tarball))
                 ("/foo-99-1.0.0-py2.py3-none-any.whl" 404 "")
                 ("/foo-99/json" 200 ,(lambda (port)
                                        (display (foo-json #:name "foo-99")
                                                 port))))
      (match (pypi->guix-package "foo-99")
        (`(package
            (name "python-foo-99")
            (version "1.0.0")
            (source (origin
                      (method url-fetch)
                      (uri (pypi-uri "foo-99" version))
                      (sha256
                       (base32 ,(? string? hash)))))
            (properties (quote (("upstream-name" . "foo-99"))))
            (build-system pyproject-build-system)
            (propagated-inputs (list python-bar python-foo))
            (native-inputs (list python-pytest))
            (home-page "http://example.com")
            (synopsis "summary")
            (description "summary.")
            (license license:lgpl2.0))
         (string=? default-sha256/base32 hash))
        (x
         (pk 'fail x #f))))))

(test-equal "package-latest-release"
  (list '("foo-1.0.0.tar.gz")
        '("foo-1.0.0.tar.gz.asc")
        (list (upstream-input
               (name "bar")
               (downstream-name "python-bar")
               (type 'propagated))
              (upstream-input
               (name "foo")
               (downstream-name "python-foo")
               (type 'propagated))
              (upstream-input
               (name "pytest")
               (downstream-name "python-pytest")
               (type 'native))))
  (let ((tarball (pypi-tarball
                  "foo-1.0.0"
                  `(("src/bizarre.egg-info/requires.txt"
                     ,test-requires.txt)))))
    (with-pypi `(("/foo-1.0.0.tar.gz" 200 ,(file-dump tarball))
                 ("/foo-1.0.0-py2.py3-none-any.whl" 404 "")
                 ("/foo/json" 200 ,(lambda (port)
                                     (display (foo-json) port))))
      (define source
        (package-latest-release
         (dummy-package "python-foo"
                        (version "0.1.2")
                        (source (dummy-origin
                                 (method url-fetch)
                                 (uri (pypi-uri "foo" version))))
                        (build-system python-build-system))
         (list %pypi-updater)))

      (list (map basename (upstream-source-urls source))
            (map basename (upstream-source-signature-urls source))
            (upstream-source-inputs source)))))

(test-end "pypi")
(delete-file-recursively sample-directory)

;; Local Variables:
;; eval: (put 'with-pypi 'scheme-indent-function 1)
;; End:
