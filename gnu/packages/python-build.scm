;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2018, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Garek Dyszel <garekdyszel@disroot.org>
;;; Copyright © 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Antero Mejr <mail@antr.me>
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

(define-module (gnu packages python-build)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;;; Commentary:
;;;
;;; Python packages to build... Python packages.  Since they are bound to be
;;; relied on by many, their dependencies should be kept minimal, and this
;;; module should not depend on other modules containing Python packages.
;;;
;;; Code:


;;; These are dependencies used by the build systems contained herein; they
;;; feel a bit out of place but are kept here to prevent circular module
;;; dependencies.
(define-public python-pathspec
  (package
    (name "python-pathspec")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pathspec" version))
       (sha256
        (base32
         "11qnlcanr1mqcpqpq1hmnwrs26csbsa2bafc7biq09x91y0dx617"))))
    (build-system python-build-system)
    (home-page "https://github.com/cpburnz/python-pathspec")
    (synopsis "Utility library for gitignore style pattern matching of file paths")
    (description
     "This package provides a utility library for gitignore style pattern
matching of file paths.")
    (license license:mpl2.0)))

(define-public python-pluggy
  (package
    (name "python-pluggy")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pluggy" version))
       (sha256
        (base32 "1w8c3mpliqm9biqw75ci8cfj1x5pb6g5zwblqp27ijgxjj7aizrc"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://pypi.org/project/pluggy/")
    (synopsis "Plugin and hook calling mechanism for Python")
    (description
     "Pluggy is an extraction of the plugin manager as used by Pytest but
stripped of Pytest specific details.")
    (license license:expat)))

(define-public python-toml
  (package
    (name "python-toml")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "toml" version))
       (sha256
        (base32 "13z6rff86bzdpl094x0vmfvls779931xj90dlbs9kpfm138s3gdk"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                     ;no tests suite in release
    (home-page "https://github.com/uiri/toml")
    (synopsis "Library for TOML")
    (description
     "@code{toml} is a library for parsing and creating Tom's Obvious, Minimal
Language (TOML) configuration files.")
    (license license:expat)))

(define-public python-tomli-w
  (package
    (name "python-tomli-w")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tomli_w" version))
       (sha256
        (base32 "1fg13bfq5qy1ym4x77815nhxh1xpfs0drhn9r9464cz00m1l6qzl"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;to avoid extra dependencies
    (native-inputs (list python-pypa-build python-flit-core))
    (home-page "https://github.com/hukkin/tomli-w")
    (synopsis "Minimal TOML writer")
    (description "Tomli-W is a Python library for writing TOML.  It is a
write-only counterpart to Tomli, which is a read-only TOML parser.")
    (license license:expat)))

(define-public python-pytoml
  (package
    (name "python-pytoml")
    (version "0.1.21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytoml" version))
       (sha256
        (base32
         "1rv1byiw82k7mj6aprcrqi2vdabs801y97xhfnrz7kxds34ggv4f"))))
    (build-system python-build-system)
    (home-page "https://github.com/avakar/pytoml")
    (synopsis "Parser for TOML")
    (description "This package provides a Python parser for TOML-0.4.0.")
    (license license:expat)))

(define-public python-six-bootstrap
  (package
    (name "python-six-bootstrap")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "six" version))
       (sha256
        (base32
         "09n9qih9rpj95q3r4a40li7hk6swma11syvgwdc68qm1fxsc6q8y"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))          ;to avoid pytest dependency
    (home-page "https://pypi.org/project/six/")
    (synopsis "Python 2 and 3 compatibility utilities")
    (description
     "Six is a Python 2 and 3 compatibility library.  It provides utility
functions for smoothing over the differences between the Python versions with
the goal of writing Python code that is compatible on both Python versions.
Six supports every Python version since 2.5.  It is contained in only one
Python file, so it can be easily copied into your project.")
    (license license:x11)))

(define-public python-tomli
  (package
    (name "python-tomli")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tomli" version))
       (sha256
        (base32 "1f4a9nvy8g82bl0k1wdxz9y1j843ai4l4glp0iyy357c5ap6qr1z"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                      ;disabled to avoid extra dependencies
    (native-inputs (list python-flit-core-bootstrap python-six-bootstrap))
    (home-page "https://github.com/hukkin/tomli")
    (synopsis "Small and fast TOML parser")
    (description "Tomli is a minimal TOML parser that is fully compatible with
@url{https://toml.io/en/v1.0.0,TOML v1.0.0}.  It is about 2.4 times as fast as
@code{python-toml}.")
    (license license:expat)))

(define-public python-trove-classifiers
  (package
    (name "python-trove-classifiers")
    (version "2024.10.21.16")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trove_classifiers" version))
              (sha256
               (base32
                "1wzmij9b84pixms7nk2fawhvryj355rsi4rjwsfrspkxsrax1jqp"))))
    (build-system pyproject-build-system)
    (arguments (list #:build-backend "setuptools.build_meta"
                     #:tests? #f))      ;keep dependencies to a minimum
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/pypa/trove-classifiers")
    (synopsis "Canonical source for classifiers on PyPI")
    (description "This package is the canonical source for classifiers use on
PyPI (pypi.org).")
    (license license:asl2.0)))

(define-public python-typing-extensions
  (package
    (name "python-typing-extensions")
    (version "4.12.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "typing_extensions" version))
              (sha256
               (base32
                "1f7z47hmz48kgixzb3ffw6zml8j1iflf6ml8xr6xsng5qxasszhs"))))
    (build-system pyproject-build-system)
    ;; Disable the test suite to keep the dependencies to a minimum.  Also,
    ;; the test suite requires Python's test module, not available in Guix.
    (arguments (list #:tests? #f))
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/python/typing_extensions")
    (synopsis "Experimental type hints for Python")
    (description
     "The typing_extensions module contains additional @code{typing} hints not
yet present in the of the @code{typing} standard library.
Included are implementations of:
@enumerate
@item ClassVar
@item ContextManager
@item Counter
@item DefaultDict
@item Deque
@item NewType
@item NoReturn
@item overload
@item Protocol
@item runtime
@item Text
@item Type
@item TYPE_CHECKING
@item AsyncGenerator
@end enumerate\n")
    (license license:psfl)))


;;;
;;; Python builder packages.
;;;
(define-public python-pip
  (package
    (name "python-pip")
    (version "23.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pip" version))
       (sha256
        (base32
         "0jnk639v9h7ghslm4jnlic6rj3v29nygflx1hgxxndg5gs4kk1a0"))
       (snippet
        #~(begin
            (delete-file "src/pip/_vendor/certifi/cacert.pem")
            (delete-file "src/pip/_vendor/certifi/core.py")
            (with-output-to-file "src/pip/_vendor/certifi/core.py"
              (lambda _
                (display "\"\"\"
certifi.py
~~~~~~~~~~
This file is a Guix-specific version of core.py.

This module returns the installation location of SSL_CERT_FILE or
/etc/ssl/certs/ca-certificates.crt, or its contents.
\"\"\"
import os

_CA_CERTS = None

try:
    _CA_CERTS = os.environ [\"SSL_CERT_FILE\"]
except:
    _CA_CERTS = os.path.join(\"/etc\", \"ssl\", \"certs\", \"ca-certificates.crt\")

def where() -> str:
    return _CA_CERTS

def contents() -> str:
    with open(where(), \"r\", encoding=\"ascii\") as data:
        return data.read()")))))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))          ; there are no tests in the pypi archive.
    (home-page "https://pip.pypa.io/")
    (synopsis "Package manager for Python software")
    (description
     "Pip is a package manager for Python software, that finds packages on the
Python Package Index (PyPI).")
    (license license:expat)))

(define-public python-setuptools
  (package
    (name "python-setuptools")
    (version "67.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools" version))
       (sha256
        (base32
         "16myxkpa89r045il88zcygdy1zbi2mvvpz5b4a70p9jhklmfjz95"))
       (modules '((guix build utils)))
       (snippet
        ;; TODO: setuptools now bundles the following libraries:
        ;; packaging, pyparsing, six and appdirs.  How to unbundle?
        ;; Remove included binaries which are used to build self-extracting
        ;; installers for Windows.
        '(for-each delete-file (find-files "setuptools"
                                           "^(cli|gui).*\\.exe$")))))
    (build-system python-build-system)
    ;; FIXME: Tests require pytest, which itself relies on setuptools.
    ;; One could bootstrap with an internal untested setuptools.
    (arguments (list #:tests? #f))
    (home-page "https://pypi.org/project/setuptools/")
    (synopsis "Library designed to facilitate packaging Python projects")
    (description "Setuptools is a fully-featured, stable library designed to
facilitate packaging Python projects, where packaging includes:
@itemize
@item Python package and module definitions
@item distribution package metadata
@item test hooks
@item project installation
@item platform-specific details.
@end itemize")
    (license (list license:psfl         ;setuptools itself
                   license:expat        ;six, appdirs, pyparsing
                   license:asl2.0       ;packaging is dual ASL2/BSD-2
                   license:bsd-2))))

;; This is the last version with use_2to3 support.
(define-public python-setuptools-57
  (package
    (inherit python-setuptools)
    (version "57.5.0")
    (source (origin
              (inherit (package-source python-setuptools))
              (uri (pypi-uri "setuptools" version))
              (sha256
               (base32
                "091sp8lrin7qllrhhx7y0iiv5gdb1d3l8a1ip5knk77ma1njdlyr"))))))

(define-public python-wheel
  (package
    (name "python-wheel")
    (version "0.40.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "wheel" version))
        (sha256
         (base32
          "0ww8fgkvwv35ypj4cnngczdwp6agr4qifvk2inb32azfzbrrc4fd"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: The test suite runs "python setup.py bdist_wheel", which in turn
     ;; fails to find the newly-built bdist_wheel library, even though it is
     ;; available on PYTHONPATH.  What search path is consulted by setup.py?
     '(#:tests? #f))
    (home-page "https://github.com/pypa/wheel")
    (synopsis "Format for built Python packages")
    (description
     "A wheel is a ZIP-format archive with a specially formatted filename and
the @code{.whl} extension.  It is designed to contain all the files for a PEP
376 compatible install in a way that is very close to the on-disk format.  Many
packages will be properly installed with only the @code{Unpack} step and the
unpacked archive preserves enough information to @code{Spread} (copy data and
scripts to their final locations) at any later time.  Wheel files can be
installed with a newer @code{pip} or with wheel's own command line utility.")
    (license license:expat)))

;;; TODO: Deprecate with https://github.com/pypa/pyproject-hooks.
(define-public python-pep517-bootstrap
  (hidden-package
   (package
     (name "python-pep517-bootstrap")
     (version "0.9.1")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pep517" version))
        (sha256
         (base32
          "0zqidxah03qpnp6zkg3zd1kmd5f79hhdsfmlc0cldaniy80qddxf"))))
     (build-system python-build-system)
     (arguments
      `(#:tests? #f))                     ;to avoid circular dependencies
     (propagated-inputs
      (list python-toml python-wheel))
     (home-page "https://github.com/pypa/pep517")
     (synopsis "Wrappers to build Python packages using PEP 517 hooks")
     (description
      "Wrappers to build Python packages using PEP 517 hooks.")
     (license license:expat))))

(define-public python-pyparsing
  (package
    (name "python-pyparsing")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyparsing" version))
       (sha256
        (base32 "02jz5rv3vx46xvjszda467l269jz9narlrwd0a83mpk6zma0i631"))))
    (build-system pyproject-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f                      ;no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((doc (string-append (assoc-ref outputs "doc")
                                        "/share/doc/" ,name "-" ,version))
                    (html-doc (string-append doc "/html"))
                    (examples (string-append doc "/examples")))
               (mkdir-p html-doc)
               (mkdir-p examples)
               (for-each
                (lambda (dir tgt)
                  (map (lambda (file)
                         (install-file file tgt))
                       (find-files dir ".*")))
                (list "docs" "htmldoc" "examples")
                (list doc html-doc examples))))))))
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/pyparsing/pyparsing")
    (synopsis "Python parsing class library")
    (description
     "The pyparsing module is an alternative approach to creating and
executing simple grammars, vs. the traditional lex/yacc approach, or the use
of regular expressions.  The pyparsing module provides a library of classes
that client code uses to construct the grammar directly in Python code.")
    (license license:expat)))

;;; This is the last release compatible with Python 2.
(define-public python-pyparsing-2.4.7
  (package
    (inherit python-pyparsing)
    (version "2.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyparsing" version))
       (sha256
        (base32 "1hgc8qrbq1ymxbwfbjghv01fm3fbpjwpjwi0bcailxxzhf3yq0y2"))))
    (native-inputs (list python-setuptools python-wheel))))

(define-public python-packaging-bootstrap
  (package
    (name "python-packaging-bootstrap")
    (version "24.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "packaging" version))
       (sha256
        (base32
         "0zxrq3nn0lmmqp3p2d92v0yqbs29kl87k4vkqmmk8bckbvfaca62"))))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))         ;disabled to avoid extra dependencies
    (propagated-inputs
     (list python-pyparsing python-six-bootstrap))
    (native-inputs
     (list python-flit-core))
    (home-page "https://github.com/pypa/packaging")
    (synopsis "Core utilities for Python packages")
    (description "Packaging is a Python module for dealing with Python packages.
It offers an interface for working with package versions, names, and dependency
information.")
    ;; From 'LICENSE': This software is made available under the terms of
    ;; *either* of the licenses found in LICENSE.APACHE or LICENSE.BSD.
    ;; Contributions to this software is made under the terms of *both* these
    ;; licenses.
    (license (list license:asl2.0 license:bsd-2))))

;;; The name 'python-pypa-build' is chosen rather than 'python-build' to avoid
;;; a name clash with python-build from (guix build-system python).
(define-public python-pypa-build
  (package
    (name "python-pypa-build")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "build" version))
              (sha256
               (base32
                "17xqija27x4my1yrnk6q2vwln60r39g2dhby9zg2l99qjgbdrahs"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ;to tests in the PyPI release
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'use-toml-instead-of-tomli
                    ;; Using toml instead of tomli eases bootstrapping.
                    (lambda _
                      (substitute* "setup.cfg"
                        (("tomli>=.*")
                         "toml\n")))))))
    (propagated-inputs
     `(("python-packaging" ,python-packaging-bootstrap)
       ("python-pep517", python-pep517-bootstrap)
       ("python-toml" ,python-toml)))
    (home-page "https://pypa-build.readthedocs.io/en/latest/")
    (synopsis "Simple Python PEP 517 package builder")
    (description "The @command{build} command invokes the PEP 517 hooks to
build a distribution package.  It is a simple build tool and does not perform
any dependency management.  It aims to keep dependencies to a minimum, in
order to make bootstrapping easier.")
    (license license:expat)))

;; There are quite a few amount of Python installers in the wild we need the
;; one from PyPa team.
(define-public python-pypa-installer
  (package
    (name "python-pypa-installer")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "installer" version))
       (sha256
        (base32 "0cdnqh3a3amw8k4s1pzfjh0hpvzw4pczgl702s1b16r82qqkwvd2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ; Depends on pytest, which we cannot import into this module.
    (native-inputs
     (list python-flit-core))
    (home-page "https://installer.readthedocs.io/")
    (synopsis "low-level library for installing from a Python wheel distribution")
    (description
     "This package provides a library for installing Python wheels.")
    (license license:expat)))

(define-public python-poetry-core
  (package
    (name "python-poetry-core")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "poetry_core" version))
       (sha256
        (base32 "1f31gwhnfyrdymlm0ym6k6c6r0x98zcr2s4xz4blz2zm9chljbbs"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                      ;disabled to avoid extra dependencies
    (home-page "https://github.com/python-poetry/poetry-core")
    (synopsis "Poetry PEP 517 build back-end")
    (description
     "The @code{poetry-core} module provides a PEP 517 build back-end
implementation developed for Poetry.  This project is intended to be
a light weight, fully compliant, self-contained package allowing PEP 517
compatible build front-ends to build Poetry managed projects.")
    (license license:expat)))

;;; This package exists to bootstrap python-tomli.
(define-public python-flit-core-bootstrap
  (package
    (name "python-flit-core-bootstrap")
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flit" version))
       (sha256
        (base32 "0dz9sp2zlhkmk6sm5gapbbb30f7xq3n3jn5zxx5pkp25ppsaiwnh"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-toml))
    (arguments
     ;; flit-core has a test suite, but it requires Pytest.  Disable it so
     ;; as to not pull pytest as an input.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           ;; flit-core requires itself to build.  Luckily, a
           ;; bootstrapping script exists, which does so using just
           ;; the checkout sources and Python.
           (lambda _
             (invoke "python" "flit_core/build_dists.py")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (whl (car (find-files "." "\\.whl$"))))
               (invoke "pip" "--no-cache-dir" "--no-input"
                       "install" "--no-deps" "--prefix" out whl))))
         ;; The sanity-check phase fails because flit depends on tomli at
         ;; run-time, but this core variant avoids it to avoid a cycle.
         (delete 'sanity-check))))
    (home-page "https://github.com/pypa/flit")
    (synopsis "Core package of the Flit Python build system")
    (description "This package provides @code{flit-core}, a PEP 517 build
backend for packages using Flit.  The only public interface is the API
specified by PEP 517, @code{flit_core.buildapi}.")
    (license license:bsd-3)))

(define-public python-flit-core
  (package/inherit python-flit-core-bootstrap
    (name "python-flit-core")
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-flit-core-bootstrap)
       (replace "python-toml" python-tomli)))))

(define-public python-flit-scm
  (package
    (name "python-flit-scm")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "flit_scm" version))
              (sha256
               (base32
                "1ckbkykfr7f7wzjzgh0gm7h6v3pqzx2l28rw6dsvl6zk4kxxc6wn"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f        ;to avoid extra dependencies
                     ;; flit-scm wants to use flit-core, which it renames to
                     ;; 'buildapi', but that isn't found even when adding the
                     ;; current directory to PYTHONPATH.  Use setuptools'
                     ;; builder instead.
                     #:build-backend "setuptools.build_meta"))
    (propagated-inputs (list python-flit-core python-setuptools-scm python-tomli))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://gitlab.com/WillDaSilva/flit_scm")
    (synopsis "PEP 518 build backend combining flit_core and setuptools_scm")
    (description "This package provides a PEP 518 build backend that uses
@code{setuptools_scm} to generate a version file from your version control
system, then @code{flit_core} to build the package.")
    (license license:expat)))

(define-public python-setuptools-scm
  (package
    (name "python-setuptools-scm")
    (version "7.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools_scm" version))
              (sha256
               (base32 "09wg4zg30ir1c2cvwqipaz3hwaxz503fgw5zdvaxgakilx2q6l3c"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))    ;avoid extra dependencies such as pytest
    (propagated-inputs (list python-packaging-bootstrap python-tomli
                             python-typing-extensions))
    (home-page "https://github.com/pypa/setuptools_scm/")
    (synopsis "Manage Python package versions in SCM metadata")
    (description
     "Setuptools_scm handles managing your Python package versions in
@dfn{software configuration management} (SCM) metadata instead of declaring
them as the version argument or in a SCM managed file.")
    (license license:expat)))

(define-public python-setuptools-scm-next
  (package
    (inherit python-setuptools-scm)
    (name "python-setuptools-scm")
    (version "8.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools_scm" version))
              (sha256
               (base32 "19y84rzqwb2rd88bjrlafrhfail2bnk6apaig8xskjviayva3pj2"))))
    (build-system pyproject-build-system)
    (arguments (list
                ;; pyproject-build-system will error handle forms such as
                ;; "module:object", so we set it.
                #:build-backend "setuptools.build_meta"
                #:phases
                #~(modify-phases %standard-phases
                    (add-before 'build 'setenv
                      (lambda _
                        ;; pyproject-build-system ignore backend-path,
                        ;; and __import__ ignore GUIX_PYTHONPATH, so set
                        ;; PYTHONPATH.
                        (setenv "PYTHONPATH"
                                (string-append
                                 (getcwd)
                                 ":"
                                 (getcwd) "/src:"
                                 (getenv "GUIX_PYTHONPATH"))))))
                #:tests? #f))    ;avoid extra dependencies such as pytest
    (native-inputs (list python-setuptools python-wheel))))

(define-public python-editables
  (package
    (name "python-editables")
    (version "0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pfmoore/editables")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bp959fz987jvrnkilhyr41fw4g00g9jfyiwmfvy96hv1yl68w8b"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))    ;avoid extra dependencies such as pytest
    (native-inputs
     (list python-flit-core))
    (home-page "https://github.com/pfmoore/editables")
    (synopsis "Editable installations")
    (description "This library supports the building of wheels which, when
installed, will expose packages in a local directory on @code{sys.path} in
``editable mode''.  In other words, changes to the package source will be
reflected in the package visible to Python, without needing a reinstall.")
    (license license:expat)))

;; This package may be removed when we have Python 3.11 on board.
(define-public python-exceptiongroup
  (package
    (name "python-exceptiongroup")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/agronholm/exceptiongroup")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wcvzwgjs0xmggs6dh92jxdqi988gafzh10hrzvw10kasy0xakfj"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ;TODO: Circular dependency on pytest
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: PEP 517 manual build/install procedures copied from
          ;; python-isort.
          (replace 'build
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)
              ;; ZIP does not support timestamps before 1980.
              (setenv "SOURCE_DATE_EPOCH" "315532800")
              (invoke "python" "-m" "build" "--wheel" "--no-isolation" ".")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((whl (car (find-files "dist" "\\.whl$"))))
                (invoke "pip" "--no-cache-dir" "--no-input"
                        "install" "--no-deps" "--prefix" #$output whl))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "tests")))))))
    (native-inputs (list python-flit-scm python-pypa-build))
    (home-page "https://github.com/agronholm/exceptiongroup")
    (synopsis "PEP 654 backport from Python 3.11")
    (description "This is a backport of the @code{BaseExceptionGroup} and
@code{ExceptionGroup} classes from Python 3.11.")
    (license license:expat)))

(define-public python-hatchling
  (package
    (name "python-hatchling")
    (version "1.26.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hatchling" version))
              (sha256
               (base32
                "1s9lq2x5g5f24j34r7q3zp39wyqwjixfkq8vb4gxzmf5ws96b5cd"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))                  ;to keep dependencies to a minimum
    (propagated-inputs (list python-editables
                             python-packaging-bootstrap
                             python-pathspec
                             python-pluggy
                             python-tomli
                             python-trove-classifiers))
    (home-page "https://hatch.pypa.io/latest/")
    (synopsis "Modern, extensible Python build backend")
    (description "Hatch is a modern, extensible Python project manager.  It
has features such as:
@itemize
@item Standardized build system with reproducible builds by default
@item Robust environment management with support for custom scripts
@item Easy publishing to PyPI or other indexes
@item Version management
@item Configurable project generation with sane defaults
@item Responsive CLI, ~2-3x faster than equivalent tools.
@end itemize")
    (license license:expat)))

(define-public python-hatch-fancy-pypi-readme
  (package
    (name "python-hatch-fancy-pypi-readme")
    (version "22.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hatch_fancy_pypi_readme" version))
              (sha256
               (base32
                "0sn2wsfbpsbf2mqhjvw62h1cfy5mz3d7iqyqvs5c20cnl0n2i4fs"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;avoid extra test dependencies
    (propagated-inputs (list python-hatchling python-tomli
                             python-typing-extensions))
    (home-page "https://github.com/hynek/hatch-fancy-pypi-readme")
    (synopsis "Fancy PyPI READMEs with Hatch")
    (description "This hatch plugin allows defining a project description in
terms of concatenated fragments that are based on static strings, files and
parts of files defined using cut-off points or regular expressions.")
    (license license:expat)))

(define-public python-hatch-vcs
  (package
    (name "python-hatch-vcs")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hatch_vcs" version))
              (sha256
               (base32
                "1viz2mdfxfqpsd5f30410q6smj90qfxihvy9idzwd0p4ziy11iff"))))
    (arguments (list #:tests? #f))      ;avoid extra test dependencies
    (build-system pyproject-build-system)
    (propagated-inputs (list python-hatchling python-setuptools-scm))
    (home-page "https://github.com/ofek/hatch-vcs")
    (synopsis "Hatch plugin for versioning with your preferred VCS")
    (description "This package is a plugin for Hatch that uses your preferred
version control system (like Git) to determine project versions.")
    (license license:expat)))

(define-public python-installer
  (package
    (name "python-installer")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "installer" version))
       (sha256
        (base32 "0cdnqh3a3amw8k4s1pzfjh0hpvzw4pczgl702s1b16r82qqkwvd2"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;avoid extra test dependencies
    (native-inputs
     (list python-flit-core))
    (home-page "https://installer.rtfd.io/")
    (synopsis "Installer library for Python wheels")
    (description
     "This package provides a low-level library for installing a Python
package from a wheel distribution.  It provides basic functionality and
abstractions for handling wheels and installing packages from wheels.")
    (license license:expat)))

(define-public python-pdm-backend
  (package
    (name "python-pdm-backend")
    (version "2.4.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pdm_backend" version))
              (sha256
               (base32
                "0a0741c1g5vxhrizyxh40mjxdmbsc4xid5vy4aji23f1g9x09nfv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ; Depends on pytest, which we cannot import into this module.
    (home-page "https://pdm-backend.fming.dev/")
    (synopsis
     "PEP 517 build backend for PDM")
    (description
     "PDM-Backend is a build backend that supports the latest packaging
standards, which includes PEP 517, PEP 621 and PEP 660.")
    (license license:expat)))

