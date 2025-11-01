;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>
;;; Copyright © 2017, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2021 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018, 2021-2023, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Garek Dyszel <garekdyszel@disroot.org>
;;; Copyright © 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2024 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Antero Mejr <mail@antr.me>
;;; Copyright © 2024, 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Nguyễn Gia Phong <cnx@loang.net>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix deprecation)
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
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pathspec" version))
       (sha256
        (base32 "04jpkzic8f58z6paq7f3f7fdnlv9l89khv3sqsqk7ax10caxb0m4"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-backend #~'unittest))
    (native-inputs
     (list python-flit-core
           python-setuptools))
    (home-page "https://github.com/cpburnz/python-pathspec")
    (synopsis "Utility library for gitignore style pattern matching of file paths")
    (description
     "This package provides a utility library for gitignore style pattern
matching of file paths.")
    (license license:mpl2.0)))

(define-public python-pluggy
  (package
    (name "python-pluggy")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pluggy" version))
       (sha256
        (base32 "1wr2vnbb7gy9wlz01yvb7rn4iqzd3mwmidk11ywk7395fq5i7k3x"))))
    (build-system pyproject-build-system)
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
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                     ;no tests suite in release
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/uiri/toml")
    (synopsis "Library for TOML")
    (description
     "@code{toml} is a library for parsing and creating Tom's Obvious, Minimal
Language (TOML) configuration files.")
    (license license:expat)))

(define-public python-tomli-w
  (package
    (name "python-tomli-w")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tomli_w" version))
       (sha256
        (base32 "08ahw9db7qycd4fzh4j51cgzp1rdl7snm5scrplpphj7ban4zl9d"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;to avoid extra dependencies
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/hukkin/tomli-w")
    (synopsis "Minimal TOML writer")
    (description "Tomli-W is a Python library for writing TOML.  It is a
write-only counterpart to Tomli, which is a read-only TOML parser.")
    (license license:expat)))

(define-public python-pygments
  (package
    (name "python-pygments")
    (version "2.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pygments" version))
       (sha256
        (base32
         "07qm8mx3y5r8ri6zpn0hp9zx5g02bydhi7pkv54hdp3nhlm6vhb1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; 4568 passed, 16 skipped, 597 deselected
      ;;
      ;; Ignore tests requiring "wcag_contrast_ratio"
      #~(list "--ignore=tests/contrast/test_contrasts.py"
              ;; Tests fail with not matched diff.
              "--ignore=tests/examplefiles/awk/test.awk"
              "--ignore=tests/examplefiles/bash/example.sh"
              "--ignore=tests/examplefiles/make/Makefile"
              "--ignore=tests/examplefiles/phix/example.exw"
              "--ignore=tests/examplefiles/sed/all_sorts_of_syntax.sed"
              "--ignore=tests/examplefiles/sed/count_words.sed"
              "--ignore=tests/examplefiles/sed/increment_number.sed"
              "--ignore=tests/examplefiles/sed/reverse.sed"
              "--ignore=tests/examplefiles/slurm/example.sl"
              ;; Assertion error to find example file by following symlink:
              ;; assert p.is_file(), f"Example file {p} not found"
              "--deselect=tests/test_basic_api.py::test_lexer_classes")))
    (native-inputs
     (list python-hatchling python-pytest-bootstrap))
    (home-page "https://pygments.org/")
    (synopsis "Syntax highlighting")
    (description
     "Pygments is a syntax highlighting package written in Python.")
    (license license:bsd-2)))

(define-public python-pygments-bootstrap
   (package/inherit python-pygments
     (name "python-pygments-bootstrap")
     (native-inputs
      (list python-hatchling))
     (arguments `(#:tests? #f))))

(define-public python-pytest-bootstrap
  (package
    (name "python-pytest-bootstrap")
    (version "9.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest" version))
       (sha256
        (base32 "04fz1vbhb2l6k8lmrk8wqhkxhprlnkq21z6rs48rdn1bm58nc63m"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs
     (list python-setuptools-bootstrap
           python-setuptools-scm-bootstrap))
    (propagated-inputs
     (list python-iniconfig
           python-packaging-bootstrap
           python-pluggy
           python-pygments-bootstrap))  ;it is in installation dependencies
    (home-page "https://docs.pytest.org/en/latest/")
    (synopsis "Python testing library")
    (description
     "Pytest is a testing tool that provides auto-discovery of test modules
and functions, detailed info on failing assert statements, modular fixtures,
and many external plugins.")
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
        (base32 "1rv1byiw82k7mj6aprcrqi2vdabs801y97xhfnrz7kxds34ggv4f"))))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))          ;to avoid pytest dependency
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/avakar/pytoml")
    (synopsis "Parser for TOML")
    (description "This package provides a Python parser for TOML-0.4.0.")
    (license license:expat)))

(define-public python-tomli
  (package
    (name "python-tomli")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tomli" version))
       (sha256
        (base32 "1zq58p2bplyf0xpi9fnyn4w6vc1fkw8whkj0yxhcwdf8g7ff2ifd"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                      ;disabled to avoid extra dependencies
    (native-inputs (list python-flit-core-bootstrap))
    (home-page "https://github.com/hukkin/tomli")
    (synopsis "Small and fast TOML parser")
    (description "Tomli is a minimal TOML parser that is fully compatible with
@url{https://toml.io/en/v1.0.0,TOML v1.0.0}.  It is about 2.4 times as fast as
@code{python-toml}.")
    (license license:expat)))

(define-public python-trove-classifiers
  (package
    (name "python-trove-classifiers")
    (version "2025.5.9.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trove_classifiers" version))
       (sha256
        (base32 "1dawkrcc353rz1h4mazw25vkbhics5lpqrwc8qad6b3flykwi9vw"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:build-backend "setuptools.build_meta"
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
    (version "4.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "typing_extensions" version))
       (sha256
        (base32 "0rhlhs28jndgp9fghdhidn6g7xiwx8vvihxbxhlgl4ncfg8lishc"))))
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

(define-deprecated/public-alias python-typing-extensions-next
  python-typing-extensions)            ;may be removed after 2025-12-01


;;;
;;; Python builder packages.
;;;
(define-public python-pbr
  (package
    (name "python-pbr")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pbr" version))
       (sha256
        (base32 "0mvy1z1dyl28w0brns1vdhc98hzbn5b3fsw1xj41amdqs88wpjry"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f)) ;; Most tests seem to use the Internet.
    ;; Message from upstream:
    ;;
    ;; DO NOT add any other dependencies as PBR is meant to be minimalist to
    ;; avoid problems with bootstrapping build environments.
    ;;
    ;; See: <https://opendev.org/openstack/pbr/src/tag/7.0.1/requirements.txt>.
    (propagated-inputs
     (list python-setuptools))
    (home-page "https://docs.openstack.org/pbr/latest/")
    (synopsis "Enhance the default behavior of Python’s setuptools")
    (description
     "Python Build Reasonableness (PBR) is a library that injects some useful
and sensible default behaviors into your setuptools run.  It will set
versions, process requirements files and generate AUTHORS and ChangeLog file
from git information.")
    (license license:asl2.0)))

;; It may be removed after 2025-12-22.
(define-deprecated/public-alias python-pbr-next python-pbr)

(define-public python-pip
  (package
    (name "python-pip")
    (version "25.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pip" version))
       (sha256
        (base32
         "0xwhrng91a48zb5qmb6vagbjr6kzvbc8b08nq9a5139h3m0mvr1x"))
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
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))          ; there are no tests in the pypi archive.
    (native-inputs
     (list python-setuptools))
    (home-page "https://pip.pypa.io/")
    (synopsis "Package manager for Python software")
    (description
     "Pip is a package manager for Python software, that finds packages on the
Python Package Index (PyPI).")
    (license license:expat)))

(define-public python-setuptools
  (package
    (name "python-setuptools")
    (version "80.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools" version))
       (sha256
        (base32 "175iixi2h2jz8y2bpwziak360hvv43jfhipwzbdniryd5r04fszk"))
       (modules '((guix build utils)))
       (snippet
        ;; TODO: setuptools now bundles the following libraries:
        ;; packaging, pyparsing, six and appdirs.  How to unbundle?
        ;; Remove included binaries which are used to build self-extracting
        ;; installers for Windows.
        '(for-each delete-file (find-files "setuptools"
                                           "^(cli|gui).*\\.exe$")))))
    (build-system pyproject-build-system)
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

(define-public python-setuptools-bootstrap
  (package/inherit python-setuptools
    (name "python-setuptools-bootstrap")
    ;; version and source are purposefully not inherited, to allow
    ;; updating python-setuptools in the python-team scope, whereas
    ;; python-setuptools-bootstrap is in the core-packages-team scope.
    (version "80.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools" version))
       (sha256
        (base32 "175iixi2h2jz8y2bpwziak360hvv43jfhipwzbdniryd5r04fszk"))
       (modules '((guix build utils)))
       (snippet
        ;; TODO: setuptools now bundles the following libraries:
        ;; packaging, pyparsing, six and appdirs.  How to unbundle?
        ;; Remove included binaries which are used to build self-extracting
        ;; installers for Windows.
        '(for-each delete-file (find-files "setuptools"
                                           "^(cli|gui).*\\.exe$")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;disabled to avoid extra dependencies
      ;; Essentially a lighter copy of the former python-build-system.
      ;; Using it rather than pyproject-build-system allows to edit the latter
      ;; without a world rebuild (for the meson package in particular).
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (delete 'configure)
          (replace 'build
            (lambda _
              (invoke "python" "./setup.py" "build")))
          (replace 'install
            (lambda _
              (invoke "python" "./setup.py" "install"
                      (string-append "--prefix=" #$output) "--no-compile")
              (invoke "python" "-m" "compileall"
                      "--invalidation-mode=unchecked-hash" #$output)))
          ;; XXX: Despite using the same setup.py, it seems that the
          ;; bootstrap version is not able to install the distutils hack.
          (add-after 'install 'fix-installation
            (lambda* (#:key outputs #:allow-other-keys)
              (with-directory-excursion
                  (car (find-files #$output "site-packages"
                                   #:directories? #t))
                (call-with-output-file "distutils-precedence.pth"
                  (lambda (port)
                    (display
                     (string-join
                      '("import os"
                        "var = 'SETUPTOOLS_USE_DISTUTILS'"
                        "enabled = os.environ.get(var, 'local') == 'local'"
                        "enabled and __import__('_distutils_hack').add_shim();")
                      "; ")
                     port)))))))))
    (native-inputs (list))
    ;; Avoid introducing an additional module-dependency.
    (inputs (list (module-ref (resolve-interface '(gnu packages python))
                              'python-wrapper)))
    (propagated-inputs (list))))

(define-public python-setuptools-67
  (package
    (inherit python-setuptools)
    (version "67.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools" version))
       (sha256
        (base32 "16myxkpa89r045il88zcygdy1zbi2mvvpz5b4a70p9jhklmfjz95"))
       (modules '((guix build utils)))
       (snippet
        '(for-each delete-file (find-files "setuptools" "^(cli|gui).*\\.exe$")))))
    (native-inputs
     (list python-wheel-0.40))))

(define-public python-setuptools-79
  (package
    (inherit python-setuptools)
    (name "python-setuptools")
    (version "79.0.1")
    (source
     (origin
       (inherit (package-source python-setuptools))
       (method url-fetch)
       (uri (pypi-uri "setuptools" version))
       (sha256
        (base32 "127svm8cdpvmq37gcrbvdr9fhrhs0nscnzh63gypjc1wyfwfg30j"))))))

(define-deprecated/public-alias python-setuptools-next
  python-setuptools)                   ;may be removed after 2025-12-01

(define-public python-wheel
  (package
    (name "python-wheel")
    (version "0.46.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "wheel" version))
        (sha256
         (base32 "0f3abrpkf3spv0lk5mhv8m2ch0j074a3rivn7hfxzxx0bpxpwizx"))))
    (build-system pyproject-build-system)
    (arguments
     ;; FIXME: The test suite runs "python setup.py bdist_wheel", which in turn
     ;; fails to find the newly-built bdist_wheel library, even though it is
     ;; available on PYTHONPATH.  What search path is consulted by setup.py?
     '(#:tests? #f))
    (native-inputs
     (list python-flit-core))
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

(define-public python-wheel-0.40
  (package
    (inherit python-wheel)
    (version "0.40.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "wheel" version))
        (sha256
         (base32 "0ww8fgkvwv35ypj4cnngczdwp6agr4qifvk2inb32azfzbrrc4fd"))))))

;;; TODO: Deprecate with https://github.com/pypa/pyproject-hooks.
;;;
;;; From PyPI web page: The core of this package has been renamed to
;;; pyproject-hooks (https://pyproject-hooks.readthedocs.io). Please use that
;;; package or build (https://pypa-build.readthedocs.io/en/stable/) in place
;;; of pep517.
(define-public python-pep517-bootstrap
  (hidden-package
   (package
     (name "python-pep517-bootstrap")
     (version "0.13.1")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pep517" version))
        (sha256
         (base32 "05xk0x7b5n7zmcqrznm4lnbakgdjpin19mp5zyzb92wksgzs4bqv"))))
     (build-system pyproject-build-system)
     (arguments
      `(#:tests? #f))                     ;to avoid circular dependencies
      (native-inputs (list python-flit-core))
     (home-page "https://github.com/pypa/pep517")
     (synopsis "Wrappers to build Python packages using PEP 517 hooks")
     (description
      "Wrappers to build Python packages using PEP 517 hooks.")
     (license license:expat))))

(define-public python-pyparsing
  (package
    (name "python-pyparsing")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyparsing" version))
       (sha256
        (base32 "1giqgjknzbbh34qf8ij5rsrqlxfysjx39xi85vvl5ddkp0d3zhdr"))))
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

(define-public python-packaging-bootstrap
  (package
    (name "python-packaging-bootstrap")
    (version "25.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "packaging" version))
       (sha256
        (base32 "0kzwn2ar4ndm90qrvgyjcbkqz3klrg0ziwm1yrhbyxynk0n8fhyl"))))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))         ;disabled to avoid extra dependencies
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

(define-public python-pretend
  (package
    (name "python-pretend")
    (version "1.0.9")
    (source
     (origin
       (method git-fetch)       ;no tests in PyPI archive
       (uri (git-reference
              (url "https://github.com/alex/pretend")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "156l685r9mg7i4xyrk9ql3sxk088irxlg8x7md5i0d05hdw1z8rs"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-packaging-bootstrap
           python-pytest-bootstrap
           python-setuptools-bootstrap))
    (home-page "https://github.com/alex/pretend")
    (synopsis "Library for stubbing in Python")
    (description
     "Pretend is a library to make stubbing with Python easier.  Stubbing is a
technique for writing tests.  You may hear the term mixed up with mocks,fakes,
or doubles.  Basically, a stub is an object that returns pre-canned responses,
rather than doing any computation.")
    (license license:bsd-3)))

;;; The name 'python-pypa-build' is chosen rather than 'python-build' to avoid
;;; a name clash with python-build from (guix build-system python).
(define-public python-pypa-build
  (package
    (name "python-pypa-build")
    ;; Newer version needs more inputs, consider to move to python-xyz.
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "build" version))
       (sha256
        (base32 "0g5w28ban6k9qywqwdqiqms3crg75rsvfphl4f4qkg8wi57741qs"))))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))         ;disabled to avoid extra dependencies
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-packaging-bootstrap
           python-pep517-bootstrap
           python-toml))
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
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "poetry_core" version))
       (sha256
        (base32 "194nwaig8zkbj2ig4zwf1k7cl4vqlmvs8mmdkb425mky8was08h5"))))
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

(define-deprecated/public-alias python-poetry-core-next
  python-poetry-core)                  ;may be removed after 2025-12-01

;;; This package exists to bootstrap python-tomli.
(define-public python-flit-core-bootstrap
  (package
    (name "python-flit-core-bootstrap")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flit" version))
       (sha256
        (base32 "0h1pxi2hgr95321bgl45l86693zl14l3shj0idsyg4k9v56z700w"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; flit-core has a test suite, but it requires Pytest.  Disable it so
      ;; as to not pull pytest as an input.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-license
            ;; flit_core bundles the 'tomli' TOML parser, to avoid a
            ;; bootstrapping problem. See
            ;; <https://github.com/pypa/packaging-problems/issues/342>.
            (lambda _
              (delete-file-recursively "flit_core/flit_core/vendor")
              (substitute* "flit_core/pyproject.toml"
                (("license-files.*") "license-files = [\"LICENSE*\"]\n"))))
          (replace 'build
            ;; flit-core requires itself to build.  Luckily, a
            ;; bootstrapping script exists, which does so using just
            ;; the checkout sources and Python.
            (lambda _
              (chdir "flit_core")
              (invoke "python" "build_dists.py")))
          (delete 'sanity-check))))
    (propagated-inputs
     (list python-toml))
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
       (delete "python-toml")
       (prepend python-tomli)))))

(define-deprecated/public-alias python-flit-core-next
  python-flit-core)                    ;may be removed after 2025-12-01

(define-public python-flit-scm
  (package
    (name "python-flit-scm")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flit_scm" version))
       (sha256
        (base32 "1ckbkykfr7f7wzjzgh0gm7h6v3pqzx2l28rw6dsvl6zk4kxxc6wn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f        ;no tests in PyPI archive or Git checkout
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-backend
            ;; flit_scm imports flit_core's buildapi and tries to make it
            ;; available as "flit_scm:buildapi", see comment in
            ;; <flit_scm/__init__.py>; but it fails during build phase with
            ;; error: ModuleNotFoundError: No module named
            ;; 'flit_scm:buildapi'.
            ;;
            ;; Use flit_core.buildapi directly to build flit_scm.
            (lambda _
              (substitute* "pyproject.toml"
                (("flit_scm:buildapi") "flit_core.buildapi")))))))
    (propagated-inputs
     (list python-flit-core
           python-setuptools-scm
           python-tomli))
    (home-page "https://gitlab.com/WillDaSilva/flit_scm")
    (synopsis "PEP 518 build backend combining flit_core and setuptools_scm")
    (description "This package provides a PEP 518 build backend that uses
@code{setuptools_scm} to generate a version file from your version control
system, then @code{flit_core} to build the package.")
    (license license:expat)))

(define-public python-setuptools-scm
  (package
    (name "python-setuptools-scm")
    (version "8.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools_scm" version))
       (sha256
        (base32 "0qra4jysbdwlrwsb5iz8kai1xxbsz6adzbrbscvx1b2xny95wm9x"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f    ;avoid extra dependencies such as pytest
      ;; pyproject-build-system will error handle forms such as
      ;; "module:object", so we set it.
      #:build-backend "setuptools.build_meta"))
    (propagated-inputs
     (list python-packaging-bootstrap
           python-setuptools))
    (home-page "https://github.com/pypa/setuptools_scm/")
    (synopsis "Manage Python package versions in SCM metadata")
    (description
     "Setuptools_scm handles managing your Python package versions in
@dfn{software configuration management} (SCM) metadata instead of declaring
them as the version argument or in a SCM managed file.")
    (license license:expat)))

(define-public python-setuptools-scm-bootstrap
  (package/inherit python-setuptools-scm
    (name "python-setuptools-scm-bootstrap")
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f    ;avoid extra dependencies such as pytest
      ;; pyproject-build-system will error handle forms such as
      ;; "module:object", so we set it.
      #:build-backend "setuptools.build_meta"))
    (native-inputs
     (list python-packaging-bootstrap))
    (propagated-inputs
     (list python-setuptools-bootstrap))))

(define-public python-setuptools-scm-next
  (package
    (inherit python-setuptools-scm)
    (name "python-setuptools-scm")
    (version "9.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools_scm" version))
       (sha256
        (base32 "0mzgyyg1cgigkmlfm0iy44f2092zn8xc093ygn4a11jncss4lrqw"))))))

(define-public python-six
  (package
    (name "python-six")
    (version "1.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "six" version))
       (sha256
        (base32 "109ajcsfhrz33lbwbb337w34crc3lb9rjnxrcpnbczlf8rfk6w7z"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools))
    (home-page "https://pypi.org/project/six/")
    (synopsis "Python 2 and 3 compatibility utilities")
    (description
     "Six is a Python 2 and 3 compatibility library.  It provides utility
functions for smoothing over the differences between the Python versions with
the goal of writing Python code that is compatible on both Python versions.
Six supports every Python version since 2.5.  It is contained in only one
Python file, so it can be easily copied into your project.")
    (license license:x11)))

(define-public python-six-bootstrap python-six)

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
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/agronholm/exceptiongroup")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kygngc6j7hm68w8q327jvym2z4gpyh93g2af6g419qaqqv7axkg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f       ;to keep dependencies to a minimum
      #:build-backend "setuptools.build_meta"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-flit-scm))
    (propagated-inputs
     (list python-typing-extensions))
    (home-page "https://github.com/agronholm/exceptiongroup")
    (synopsis "PEP 654 backport from Python 3.11")
    (description "This is a backport of the @code{BaseExceptionGroup} and
@code{ExceptionGroup} classes from Python 3.11.")
    (license license:expat)))

(define-public python-hatchling
  (package
    (name "python-hatchling")
    (version "1.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hatchling" version))
       (sha256
        (base32 "1mhzjhg7ky8npcrnbwwq30w8s73mm73m5z0j260v7aqrk1njj74p"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))                  ;to keep dependencies to a minimum
    (propagated-inputs
     (list python-packaging-bootstrap
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

(define-public python-hatchling-bootstrap
  (package/inherit python-hatchling
    (name "python-hatchling-bootstrap")
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-hatchling)
       (replace "python-packaging" python-packaging-bootstrap)))))

(define-public python-hatchling-for-hatch
  ;; For hatch@1.9.7, remove when no longer required.
  (hidden-package
   (package
     (inherit python-hatchling)
     (version "1.21.1")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "hatchling" version))
        (sha256
         (base32 "1g40g68vzfakddd9f0psp9jkfqy3v3la4zs5g127ski2792l195v"))))
    (propagated-inputs
     (list python-editables
           python-packaging-bootstrap
           python-pathspec
           python-pluggy
           python-trove-classifiers)))))

(define-public python-hatch-docstring-description
  (package
    (name "python-hatch-docstring-description")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hatch_docstring_description" version))
       (sha256
        (base32 "1qwa8m4yswn0bkc5xzq30xsaca578axl5ig2r6mkcdxsfg196pdi"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;avoid extra test dependencies
    (native-inputs
     (list python-hatch-vcs
           python-hatchling))
    (propagated-inputs
     (list python-hatchling))
    (home-page "https://github.com/flying-sheep/hatch-docstring-description")
    (synopsis "Derive PyPI package description from Python package docstring ")
    (description
     "This package provides a Hatchling plugin to read the description from the
package docstring.")
    (license license:gpl3+)))

(define-public python-hatch-fancy-pypi-readme
  (package
    (name "python-hatch-fancy-pypi-readme")
    (version "25.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hatch_fancy_pypi_readme" version))
       (sha256
        (base32 "0i803kq80qx0k1lj3z69zw40ynqxml4p1qsc851izmchzwyysn4w"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;avoid extra test dependencies
    (propagated-inputs (list python-hatchling python-tomli))
    (home-page "https://github.com/hynek/hatch-fancy-pypi-readme")
    (synopsis "Fancy PyPI READMEs with Hatch")
    (description "This hatch plugin allows defining a project description in
terms of concatenated fragments that are based on static strings, files and
parts of files defined using cut-off points or regular expressions.")
    (license license:expat)))

(define-public python-hatch-requirements-txt
  (package
    (name "python-hatch-requirements-txt")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hatch_requirements_txt" version))
       (sha256
        (base32 "083xakilrmy0ymh34s9wm8x8s7s8vn7ij33xz9avn1gxb1bnws1c"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;avoid extra test dependencies
    (propagated-inputs (list python-hatchling python-packaging-bootstrap))
    (home-page "https://github.com/repo-helper/hatch-requirements-txt")
    (synopsis "Hatchling plugin to read requirements.txt")
    (description
     "This package implements a functionality to read project dependencies
from requirements.txt.")
    (license license:expat)))

(define-public python-hatch-vcs
  (package
    (name "python-hatch-vcs")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hatch_vcs" version))
       (sha256
        (base32 "1yczh2aqrf9p3gs1dayswz5pp9z2yhmlld0c14ah4d20d49gm583"))))
    (arguments (list #:tests? #f))      ;avoid extra test dependencies
    (build-system pyproject-build-system)
    (propagated-inputs (list python-hatchling python-setuptools-scm))
    (home-page "https://github.com/ofek/hatch-vcs")
    (synopsis "Hatch plugin for versioning with your preferred VCS")
    (description "This package is a plugin for Hatch that uses your preferred
version control system (like Git) to determine project versions.")
    (license license:expat)))

(define-public python-hatch-vcs-bootstrap
  (package/inherit python-hatch-vcs
    (name "python-hatch-vcs-bootstrap")
    (native-inputs
     (list python-packaging-bootstrap))
    (propagated-inputs
     (list python-hatchling-bootstrap
           python-setuptools-scm-bootstrap))))

(define-public python-iniconfig
  (package
    (name "python-iniconfig")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iniconfig" version))
       (sha256
        (base32 "1iz1fg3n6pv4q8jzv1q0izl5001diwqggizrg3p3ywrn1gix5frs"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;no tests in PyPI, tests introduce cycle with pytest
    (native-inputs
     (list python-hatch-vcs-bootstrap
           python-hatchling-bootstrap))
    (home-page "https://github.com/RonnyPfannschmidt/iniconfig")
    (synopsis "Simple INI-file parser")
    (description "The @code{iniconfig} package provides a small and simple
     INI-file parser module having a unique set of features ; @code{iniconfig}
     @itemize
     @item maintains the order of sections and entries              ;
     @item supports multi-line values with or without line-continuations ;
     @item supports \"#\" comments everywhere                            ;
     @item raises errors with proper line-numbers                        ;
     @item raises an error when two sections have the same name.
     @end itemize")
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
    (version "2.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pdm_backend" version))
       (sha256
        (base32 "0nzyfa4jb0cpfhh0jfqsk4xz8rbyfyxqrh2ps3axm2ih8321kh2n"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ; Depends on pytest, which we cannot import into this module.
    (home-page "https://pdm-backend.fming.dev/")
    (synopsis "PEP 517 build backend for PDM")
    (description
     "PDM-Backend is a build backend that supports the latest packaging
standards, which includes PEP 517, PEP 621 and PEP 660.")
    (license license:expat)))

