;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016 宋文武 <iyzsong@envs.net>
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>
;;; Copyright © 2017 Thomas Danckaert <thomas.danckaert@gmail.com>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018-2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019-2024 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2019, 2021, 2025 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2022 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020, 2021, 2025 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Sebastian Gibb <mail@sebastiangibb.de>
;;; Copyright © 2022 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Tomasz Jeneralczyk <tj@schwi.pl>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2024 Danny Milosavljevic <dannym@friendly-machines.com>
;;; Copyright © 2024-2025 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Navid Afkhami <navid.afkhami@mdc-berlin.de>
;;; Copyright © 2024, 2025 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2024 Markku Korkeala <markku.korkeala@iki.fi>
;;; Copyright © 2024 Vincent Prat <vprat@deeplinks.com>
;;; Copyright © 2025 Evgeny Pisemsky <mail@pisemsky.site>
;;; Copyright © 2025 Florent Pruvost <florent.pruvost@inria.fr>
;;; Copyright © 2025 Matthew Elwin <elwin@northwestern.edu>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
;;; Copyright © 2025 Zheng Junjie <z572@z572.online> 
;;; Copyright © 2026 Nguyễn Gia Phong <cnx@loang.net>
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

(define-module (gnu packages python-check)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public python-aioresponses
  (package
    (name "python-aioresponses")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pnuckowski/aioresponses")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fcm1rl1h91c2ca446kl5r2q229a8cfad2xn9gmsmdvn29wm35kc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         "tests" "-k"
         (string-append
          ;; These tests require network access.
          "not test_address_as_instance_of_url_combined_with_pass_through "
          "and not test_pass_through_with_origin_params"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-pbr-version
            (lambda _
              (setenv "PBR_VERSION" #$version))))))
    (native-inputs (list python-pbr python-ddt python-pytest python-setuptools))
    (propagated-inputs (list python-aiohttp))
    (home-page "https://github.com/pnuckowski/aioresponses")
    (synopsis "Mock out requests made by ClientSession from aiohttp package")
    (description
     "Aioresponses is a helper to mock/fake web requests in python aiohttp
package.  For requests module there are a lot of packages that help us with
testing (eg. httpretty, responses, requests-mock).  When it comes to testing
asynchronous HTTP requests it is a bit harder (at least at the beginning).
The purpose of this package is to provide an easy way to test asynchronous
HTTP requests.")
    (license license:expat)))

(define-public python-allpairspy
  (package
    (name "python-allpairspy")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "allpairspy" version))
       (sha256
        (base32 "1zh983y9k5idna677vsjlfxlzm1dfff3w1n63sfi3vkfnnik37gn"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools))
    (home-page "https://github.com/thombashi/allpairspy")
    (synopsis "Pairwise test combinations generator")
    (description
     "This is a Python library for test combinations generator.  The generator
allows one to create a set of tests using @emph{pairwise combinations} method,
reducing a number of combinations of variables into a lesser set that covers
most situations.")
    (license license:expat)))

(define-public python-approval-utilities
  (package
    (name "python-approval-utilities")
    (version "15.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/approvals/ApprovalTests.Python")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mkvrx252xc140n4rrvgbqc1hrlw1gq7nx5hr1z4rxkaxvr8prkh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f  ; Tests are run in the python-approvaltests package.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'setup
            (lambda _
              (rename-file "setup/setup.approval_utilities.py"
                           "setup.py")
              (substitute* "setup.py"
                (("from setup_utils import get_version")
                 "")
                (("version=get_version\\(\\),")
                 (format #f "version=~s," #$version))))))))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/approvals/ApprovalTests.Python")
    (synopsis "Utilities for @code{python-approvaltests}")
    (description
     "This package provides utilities for production code that work well with
@code{python-approvaltests}.")
    (license license:asl2.0)))

(define-public python-approvaltests
  (package/inherit python-approval-utilities
    (name "python-approvaltests")
    (version (package-version python-approval-utilities))
    (arguments
     (list
      #:tests? #f  ; Tests are run in the python-approvaltests package.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (substitute* "setup/setup_utils.py"
                (("version=get_version\\(\\),")
                 (format #f "version=~s," #$version))
                (("\\(get_parent_directory\\(\\)\\.parent / ")
                 "Path("))
              (rename-file "setup/setup.py"
                           "setup.py")
              ;; Assume the chdir.
              (substitute* "setup.py"
                (("\\.\\.")
                 "."))
              (rename-file "setup/setup_utils.py"
                           "setup_utils.py"))))))
    (native-inputs (list python-setuptools))
    (propagated-inputs
     (list python-allpairspy
           python-approval-utilities
           python-beautifulsoup4
           python-empty-files
           python-mock
           python-pyperclip
           python-pytest
           python-testfixtures
           python-typing-extensions))
    (home-page "https://github.com/approvals/ApprovalTests.Python")
    (synopsis "Assertion/verification library to aid testing")
    (description
     "This package provides tools verify objects that require more than a
simple assert including long strings, large arrays, and complex hash
structures and objects, i.e. when you need a more granular look at the test
failure.")
    (license license:asl2.0)))

(define-public python-assay
  ;; No release yet.
  (let ((commit "74617d70e77afa09f58b3169cf496679ac5d5621")
        (revision "0"))
    (package
      (name "python-assay")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/brandon-rhodes/assay")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1klxmamj88mn0q348r08zksccgsbch8sp0m4b04s3myrqnslp2nd"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags #~(list "-m" "assay.tests")
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? test-flags #:allow-other-keys)
                (when tests?
                  (apply invoke "python" test-flags)))))))
      (native-inputs
       (list python-setuptools python-wheel))
      (home-page "https://github.com/brandon-rhodes/assay")
      (synopsis "Python testing framework")
      (description
       "This package provides opiniotated Python test framework prototype.")
      (license license:expat))))

(define-public python-assertpy
  (package
    (name "python-assertpy")
    (version "1.1")
    (source
     (origin
       (method git-fetch) ;no tests in PyPI distribution
       (uri (git-reference
              (url "https://github.com/assertpy/assertpy")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hnfh45cmqyp7zasrllwf8gbq3mazqlhhk0sq1iqlh6fig0yfq2f"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools))
    (home-page "https://github.com/assertpy/assertpy")
    (synopsis "Simple assertion library for unit testing")
    (description
     "This package provides a simple assertion library for unit testing in
Python with a fluent API.")
    (license license:bsd-3)))

(define-public python-atpublic
  (package
    (name "python-atpublic")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "atpublic" version))
        (sha256
         (base32
          "060v2b5jfn7p99j09amxlb6w9ynwbq7fix31kl0caz0hs09fx61h"))))
    (build-system pyproject-build-system)
    (arguments (list #:build-backend "pdm.backend"))
    (native-inputs
     (list python-pytest python-pdm-backend python-sybil python-pytest-cov))
    (home-page "https://public.readthedocs.io/")
    (synopsis "@code{@@public} decorator for populating @code{__all__}")
    (description
     "This Python module adds a @code{@@public} decorator and function which
populates a module's @code{__all__} and optionally the module globals.  With
it, the declaration of a name's public export semantics are not separated from
the implementation of that name.")
    (license (list license:asl2.0
                   license:lgpl3))))    ; only for setup_helpers.py

(define-public python-autopep8
  (package
    (name "python-autopep8")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "autopep8" version))
       (sha256
        (base32 "0n0pjdk39n6vlddjqvbpkxd4a7q33dkf0k2yk6dbd5wijr7hli49"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-pycodestyle python-tomli))
    (native-inputs
     (list python-setuptools))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'prepare-check
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (home-page "https://github.com/hhatto/autopep8")
    (synopsis "Format Python code according to the PEP 8 style guide")
    (description
     "@code{autopep8} automatically formats Python code to conform to
the PEP 8 style guide.  It uses the pycodestyle utility to determine
what parts of the code needs to be formatted.  @code{autopep8} is
capable of fixing most of the formatting issues that can be reported
by pycodestyle.")
    (license (license:non-copyleft
              "https://github.com/hhatto/autopep8/blob/master/LICENSE"))))

(define-public python-avocado-framework
  (package
    (name "python-avocado-framework")
    (version "96.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "avocado-framework" version))
       (sha256
        (base32 "0zhz6423p0b5gqx2mvg7dmq8m9gbsay7wqjdwzirlwcg2v3rcz0m"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; The test suite hangs, due to a serious bug in Python/Avocado (see:
      ;; https://github.com/avocado-framework/avocado/issues/4935).
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              ;; These are runtime dependencies (inputs).
              (substitute* "avocado/plugins/spawners/podman.py"
                (("default='/usr/bin/podman'")
                 "default='podman'"))
              (substitute* "avocado/utils/podman.py"
                (("\"/usr/bin/env\", \"python3\"")
                 (format #f "~s" (search-input-file inputs "bin/python"))))
              (substitute* "avocado/utils/memory.py"
                (("\"sync\"")
                 (format #f "~s" (search-input-file inputs "bin/sync")))
                (("/bin/sh")
                 (search-input-file inputs "bin/sh")))
              ;; Batch process the tests modules with less care; if something
              ;; is wrong, the test suite will fail.  These are tests
              ;; dependencies (native inputs).
              (substitute* (find-files "selftests" "\\.py$")
                (("#!/usr/bin/env")
                 (string-append "#!" (search-input-file (or native-inputs inputs)
                                                        "bin/env")))
                (("/bin/(false|true|sh|sleep|sudo)" _ name)
                 (search-input-file (or native-inputs inputs)
                                    (string-append "bin/" name))))))
          (add-after 'unpack 'remove-broken-entrypoints
            ;; The avocado-external-runner entry point fails to load, the
            ;; 'scripts' top level package not being found (see:
            ;; https://github.com/avocado-framework/avocado/issues/5370).
            (lambda _
              (substitute* "setup.py"
                (("'avocado-external-runner = scripts.external_runner:main'.*")
                 ""))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (setenv "PYTHONPATH" (getcwd))
                (invoke "./selftests/check.py" "--skip" "static-checks")))))))
    (native-inputs (list bash-minimal coreutils-minimal perl sudo
                         python-setuptools))
    (inputs (list bash-minimal coreutils-minimal))
    (home-page "https://avocado-framework.github.io/")
    (synopsis "Tools and libraries to help with automated testing")
    (description "Avocado is a set of tools and libraries to help with
automated testing, i.e. a test framework.  Native tests are written in Python
and they follow the unittest pattern, but any executable can serve as a
test.  The following output formats are supported:
@table @asis
@item xUnit
an XML format that contains test results in a structured form, and are used by
other test automation projects, such as Jenkins.
@item JSON
a widely used data exchange format.  The JSON Avocado plugin outputs job
information, similarly to the xunit output plugin.
@item TAP
Provides the basic TAP (Test Anything Protocol) results.  Unlike most existing
Avocado machine readable outputs this one is streamlined (per test results).
@end table")
    (license license:gpl2)))            ;some files are under GPLv2 only

(define-public python-bandit
  (package
    (name "python-bandit")
    (version "1.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bandit" version))
       (sha256
        (base32 "0sz5lkg9anqz6ir157xr8ng9ymgj37ymbplkhl3w4qb9zhjrrznv"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))         ;tests require complex setup and networking
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-pyyaml
           python-rich
           python-stevedore))
    (home-page "https://github.com/PyCQA/bandit")
    (synopsis "Security oriented static analyser for python code")
    (description
     "Bandit is a tool designed to find common security issues in Python code.
To do this Bandit processes each file, builds an AST from it, and runs
appropriate plugins against the AST nodes.  Once Bandit has finished scanning
all the files it generates a report.")
    (license license:asl2.0)))

(define-public python-beartype
  (package
    (name "python-beartype")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "beartype" version))
       (sha256
        (base32 "0i1j1mkcw0fgms9qg7j30nlgcr0b0ndx2l98sb162wp8bj7hg9gr"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling python-pytest))
    (home-page "https://github.com/beartype/beartype")
    (synopsis "Fast runtime type checking for Python")
    (description "Beartype aims to be a very fast runtime type checking tool
written in pure Python.")
    (license license:expat)))

(define-public python-blockbuster
  (package
    (name "python-blockbuster")
    (version "1.5.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cbornet/blockbuster")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16ycwblivp6d7s78sxq97f9xb4vxcjfr5srxhpkpssv770lp37fs"))))
    (build-system pyproject-build-system)
    ;; test_ssl_socket wants to connects to the Internet.
    (arguments (list #:test-flags #~'("-k" "not test_ssl_socket")))
    (native-inputs (list python-hatchling
                         python-pytest
                         python-pytest-asyncio
                         python-requests))
    (propagated-inputs (list python-forbiddenfruit))
    (home-page "https://github.com/cbornet/blockbuster")
    (synopsis "Utility to detect blocking calls in Python async event loop")
    (description
     "Blockbuster is a Python package designed to detect and prevent
blocking calls within an asynchronous event loop.  It is particularly useful
when executing tests to ensure that your asynchronous code
does not inadvertently call blocking operations,
which can lead to performance bottlenecks and unpredictable behavior.

It does this by wrapping common blocking functions
and raising an exception when they are called within an asynchronous context.
Note that Blockbuster currently only detects @code{asyncio} event loops
and is tested only with CPython.")
    (license license:asl2.0)))

(define-public python-codacy-coverage
  (package
    (name "python-codacy-coverage")
    (version "1.3.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/codacy/python-codacy-coverage")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cxq2c6wyzynqjvc5szyhwvzdz4g3a4dv6bx80w4k4d9p40699hv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Pytest is unable to find tests, even with common tricks.
      ;; TODO: Run tox.ini unittests after next python-team merge.
      #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-check-manifest python-requests))
    (home-page "https://github.com/codacy/python-codacy-coverage")
    (synopsis "Codacy coverage reporter for Python")
    (description
     "This package analyses Python test suites and reports how much of the
code is covered by them.  This tool is part of the Codacy suite for analysing
code quality.")
    (license license:expat)))

(define-public python-covdefaults
  (package
    (name "python-covdefaults")
    (version "2.3.0")
    (source
     (origin
       ;; The PyPI tarball does not include tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asottile/covdefaults")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b34zkn7g66iavjxdy8hg25ab56bafgsqizf6l1anszncayal6px"))))
    (build-system pyproject-build-system)
    (arguments
     ;; 88 passed, 1 deselected
     ;; AttributeError: type object 'Plugins' has no attribute 'load_plugins'
     (list #:test-flags #~(list "-k" "not test_coverage_init")))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-coverage))
    (home-page "https://github.com/asottile/covdefaults")
    (synopsis "Coverage plugin to provide opinionated default settings")
    (description
     "Covdefaults is a coverage plugin to provide opinionated default
 settings.")
    (license license:expat)))

(define-public python-coveralls
  (package
    (name "python-coveralls")
    (version "4.0.1")
    (source
     (origin
       (method git-fetch)       ;no tests in PyPI archive
       (uri (git-reference
              (url "https://github.com/coveralls-clients/coveralls-python")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1411h7rwxgp9ag26bmlpy7g7sdh39f56dc1mrd1n74bjsgvwzj6l"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags '(list
                          ;; XXX: Avoid git dependency.
                          "--ignore=tests/git_test.py"
                          ;; XXX: Unable to find coverage package.
                          "--ignore=tests/api/reporter_test.py"
                          "--ignore=tests/integration_test.py")))
    (native-inputs
     (list python-poetry-core
           python-mock
           python-pytest
           python-responses))
    (propagated-inputs
     (list python-coverage
           python-docopt
           python-pyyaml
           python-requests))
    (home-page "https://github.com/coveralls-clients/coveralls-python")
    (synopsis "Show coverage stats online via coveralls.io")
    (description
     "Coveralls.io is a service for publishing code coverage statistics online.
This package provides seamless integration with coverage.py (and thus pytest,
nosetests, etc...) in Python projects.")
    (license license:expat)))

(define-public python-cram
  (package
    (name "python-cram")
    (version "0.7")
    (home-page "https://bitheap.org/cram/")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append home-page "cram-" version ".tar.gz")
                  (pypi-uri "cram" version)))
       (sha256
        (base32 "0bvz6fwdi55rkrz3f50zsy35gvvwhlppki2yml5bj5ffy9d499vx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (find-files "cram" ".*\\.py$")
                ;; Replace default shell path.
                (("/bin/sh")
                 (search-input-file inputs "bin/sh")))
              (substitute* (find-files "tests" ".*\\.t$")
                (("md5")
                 "md5sum")
                (("/bin/(sh|bash)")
                 (search-input-file inputs "bin/sh")))
              (substitute* "cram/_test.py"
                ;; This hack works around a bug triggered by substituting
                ;; the /bin/sh paths. "tests/usage.t" compares the output of
                ;; "cram -h", which breaks the output at 80 characters. This
                ;; causes the line showing the default shell to break into two
                ;; lines, but the test expects a single line...
                (("env\\['COLUMNS'\\] = '80'")
                 "env['COLUMNS'] = '160'"))))
          (replace 'check
            ;; The test phase uses the built library and executable.
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (begin
                    (setenv "PATH" (string-append (getenv "PATH")
                                                  ":" #$output "/bin"))
                    (invoke "make" "quicktest"))
                  (format #t "test suite not run.~%")))))))
    (native-inputs (list python-setuptools python-wheel))
    (synopsis "Simple testing framework for command line applications")
    (description
     "Cram is a functional testing framework for command line applications.
Cram tests look like snippets of interactive shell sessions.  Cram runs each
command and compares the command output in the test with the command’s actual
output.")
    (license license:gpl2+)))

(define-public python-crosshair
  (package
    (name "python-crosshair")
    (version "0.0.101")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "crosshair_tool" version))
       (sha256
        (base32 "10vrfrwmxgvfxcqz284xf40cpr3an788bbwsvi3lnd0v467b2vn3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 12229 passed, 12 skipped, 3 xfailed, 84 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; check_examples_test.py contains failing tests that
              ;; show what happens if a counterexample is found.
              "--ignore=crosshair/examples/check_examples_test.py"
              ;; AttributeError: 'ArithRef' object has no attribute 'as_long'
              "--deselect=crosshair/statespace_test.py::test_model_value_to_python_ArithRef"
              ;; AssertionError: Got MessageType.CANNOT_CONFIRM instead of
              ;; MessageType.CONFIRMED (use `pytest -v` to show trace)
              "--deselect=crosshair/register_contract_test.py::test_register_numpy_randint")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.py"
                ;; For some reason Python version is not set properly or can't
                ;; be identified.
                ((".*z3-solver.*") ""))))
          (add-before 'check 'set-test-env
            (lambda _
              (setenv "PYTHONHASHSEED" "0")))))) ;tests rely on this value
    (native-inputs
     (list python-icontract ;optional
           python-importlib-metadata
           python-mypy
           python-numpy
           python-pytest
           python-pytest-xdist
           python-setuptools))
    (propagated-inputs
     (list python-importlib-metadata
           python-packaging
           python-pygls
           python-typeshed-client
           python-typing-inspect
           python-typing-extensions
           z3))
    (home-page "https://crosshair.readthedocs.io")
    (synopsis "Analysis tool for Python using symbolic execution")
    (description
     "@code{crosshair} is an analysis tool for Python that works by repeatedly
calling your functions with symbolic inputs.  It uses an @acronym{SMT,
Satisfiability modulo theories} solver explore viable execution paths and find
counterexamples for you.")
    (license (list license:asl2.0 license:expat license:psfl))))

(define-public python-cucumber-expressions
  (package
    (name "python-cucumber-expressions")
    (version "18.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cucumber/cucumber-expressions")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1syxa142v9sajy7n2az7d0jc6lsjg93kw659pxfs3g6ddrngpdri"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Project's repository contains go, java, javascript, perl, python
          ;; and ruby implementations.
          (add-after 'unpack 'chdir-python
            (lambda _
              (chdir "python"))))))
    (native-inputs
     (list python-poetry-core python-pytest python-pyyaml))
    (home-page "https://github.com/cucumber/cucumber-expressions")
    (synopsis "A simpler alternative to Regular Expressions")
    (description
     "This package provides an alternative to Regular Expressions with a more
intuitive syntax.")
    (license license:expat)))

(define-public python-cucumber-tag-expressions
  (package
    (name "python-cucumber-tag-expressions")
    (version "6.1.1")
    (source
     (origin
       (method git-fetch)               ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/cucumber/tag-expressions")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hanh7hzxmx0f6fp2ykabsg32snmp8y9pd7s5xix15r1gnn7lvp9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Project's repository contains go, java, javascript, perl, python
          ;; and ruby implementations.
          (add-after 'unpack 'chdir-python
            (lambda _
              (chdir "python"))))))
    (native-inputs
     (list python-path
           python-pytest
           python-pytest-html
           python-pyyaml
           python-setuptools
           python-setuptools-scm))
    (home-page "https://github.com/cucumber/tag-expressions")
    (synopsis "Tag-expression parser for cucumber/behave")
    (description
     "This package provides a tag-expression parser for Cucumber and
@command{behave}.")
    (license license:expat)))

(define-public python-deal
  (package
    (name "python-deal")
    (version "4.24.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "deal" version))
       (sha256
        (base32 "078a46agz00k6chf9pjnfcnnc298in4d6rp06mspd11frfxx9x0w"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 992 passed, 13 skipped, 28 deselected, 2 warnings
      #:test-flags
      ;; Network access is required
      #~(list "--deselect=tests/test_imports.py::test_smoke_has"
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "tests/test_runtime/"
                                                    "test_offline.py::"
                                                    test))
                      (list "test_raises_exception"
                            "test_raises_specified_exception"
                            "test_allow_network"
                            "test_decorating_async_function"
                            "test_decorating_generator"))
              ;; TypeError: MaxRetryError.__init__() missing 2 required
              ;; positional arguments: 'pool' and 'url'
              "--deselect=tests/test_runtime/test_pure.py::test_pure_offline"
              ;; TypeError: MaxRetryError.__init__() missing 2 required
              ;; positional arguments: 'pool' and 'url'
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "tests/test_runtime/"
                                                    "test_raises.py::"
                                                    test))
                      (list "test_raises_doesnt_override_another_contract"
                            "test_raises_doesnt_override_another_contract_async"
                            "test_raises_generator"))
              ;; AttributeError: module 'typeguard' has no attribute 'CallMemo'
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "tests/test_testing.py::"
                                                    test))
                      (list "test_concat"
                            "test_decorator_div1_smoke"
                            "test_decorator_div2_smoke"
                            "test_decorator_rejects_bad"
                            "test_disable_type_checks"
                            "test_div1_short"
                            "test_div2_short"
                            "test_example"
                            "test_explicit_kwargs"
                            "test_explicit_strategy"
                            "test_params_ok_with_excs"
                            "test_return_type"
                            "test_return_type_checks"
                            "test_run_ok"
                            "test_type_var"
                            "test_typecheck_explicit_kwargs"))
              ;; Assertions are not equal.
              "--deselect=tests/test_cli/test_test.py::test_no_violations"
              "--deselect=tests/test_doctest.py::test_doctest[test5]")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                ((".*--cov.*") "")))))))
    (native-inputs
     (list python-flit-core
           python-docstring-parser
           python-pytest
           python-urllib3))
    (propagated-inputs
     (list python-astroid
           python-deal-solver
           python-pygments
           python-typeguard))
    (home-page "https://github.com/life4/deal")
    (synopsis "Design by contract for Python")
    (description
     "This package provides a Python library for
@url{https://en.wikipedia.org/wiki/Design_by_contract, design by contract}
(DbC) and checking values, exceptions, and side-effects. In a nutshell, deal
implements functionality to write bug-free code.  By adding a few decorators
to the code, providing free tests, static analysis, formal verification, and
much more.")
    (license license:expat)))

(define-public python-deal-solver
  (package
    (name "python-deal-solver")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/life4/deal-solver")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16kxxip5czjsy1b4xds4zpjz3rgpsrp4k2bbvw1r3z2in509w0qc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 6893 passed, 40 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; AssertionError: assert <Conclusion.SKIP: 'skipped'> is
              ;; <Conclusion.OK: 'proved!'>
              (string-append "--deselect=tests/test_types/test_set.py"
                             "::test_expr_asserts_ok[len({4, 5, 5, 6}) >= 3]"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                ((".*--cov.*") "")))))))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-flit-core))
    (propagated-inputs
     (list python-astroid
           z3))
    (home-page "https://github.com/life4/deal-solver")
    (synopsis "z3-powered solver (theorem prover) for Deal")
    (description
     "This package provides a @url{https://github.com/Z3Prover/z3, z3}-powered
solver (theorem prover) for Deal.")
    (license license:expat)))

(define-public python-ddt
  (package
    (name "python-ddt")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ddt" version))
       (sha256
        (base32 "0jz0lglz5z5clsbralbpmd1hxs4ndb6ls7lvl7216c4nhfqdc5fj"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-aiounittest
           python-pytest
           python-setuptools
           python-six
           python-wheel))
    (propagated-inputs
     (list python-pyyaml))
    (home-page "https://github.com/datadriventests/ddt")
    (synopsis "Data-Driven Tests")
    (description
     "Data-Driven Tests (@dfn{DDT}) allow you to multiply one test case by
running it with different test data, and make it appear as multiple test
cases.")
    (license license:expat)))

(define-public python-doc8
  (package
    (name "python-doc8")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "doc8" version))
       (sha256
        (base32 "162b6lff5pcd4sp6sfm5fds8kllnx7ipzbyvi3irgk718h0z698j"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-docutils
           python-pygments
           python-restructuredtext-lint
           python-stevedore
           python-tomli))
    (home-page "https://launchpad.net/doc8")
    (synopsis "Style checker for Sphinx (or other) RST documentation")
    (description
     "Doc8 is an opinionated style checker for reStructured Text and plain
text styles of documentation.")
    (license license:asl2.0)))

(define-public python-dpcontracts
  (package
    (name "python-dpcontracts")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dpcontracts" version))
       (sha256
        (base32 "0ji38afb5kb52rrjhcqklqvabxxb1lbl32vr7d94iamy2qgxzybc"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ;no tests
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/deadpixi/contracts")
    (synopsis "Implementation of contracts for Python")
    (description
     "This package provides a simple implementation of contracts for Python.
Contracts are a debugging and verification tool.  They are declarative
statements about what states a program must be in to be considered \"correct\"
at runtime.  They are similar to assertions, and are verified automatically at
various well-defined points in the program.  Contracts can be specified on
functions and on classes.")
    (license license:lgpl3+)))

(define-public python-empty-files
  (package
    (name "python-empty-files")
    (version "0.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/approvals/EmptyFiles.Python")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w3pkhymkh9wl4g8x5pssbsxr78bzb6qix95mxqqfwbx0g42iz1z"))))
    (build-system pyproject-build-system)
    ;; XXX: Circular dependency on python-approvaltests for tests.
    (arguments (list #:tests? #f))
    (propagated-inputs (list python-requests))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/approvals/EmptyFiles.Python")
    (synopsis "Serve empty files of many types")
    (description "This project will create an empty file of a type
requested.  If possible, that file will be the smallest valid file for that
type.  For example, an empty jpg will be a 1x1 pixel jpg.")
    (license license:asl2.0)))

(define-public python-eradicate
  (package
    (name "python-eradicate")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/myint/eradicate")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "135xywygriid1wvqkra13iccaayh5r6a233jyfrj6kizhflksy2p"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/myint/eradicate")
    (synopsis "Remove commented-out code from Python sources")
    (description
     "The @command{eradicate} command removes commented-out code from Python
files.  It does this by detecting block comments that contain valid Python
syntax that are likely to be commented out code.")
    (license license:expat)))

(define-public python-expecttest
    (package
      (name "python-expecttest")
      (version "0.3.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pytorch/expecttest")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1diz07lbbdlypbmcikj646vr5fav4prgs68nmnlqi0fr7m01l4zw"))))
      (build-system pyproject-build-system)
      (native-inputs
       (list python-hypothesis
             python-poetry-core
             ))
      (home-page "https://github.com/pytorch/expecttest")
      (synopsis "Python module for expect tests")
      (description "@code{expecttest} is a Python module for expect tests, where
the initial expected value of a test can be automatically set by running the
test itself.")
      (license license:expat)))

(define-public python-flake8-builtins
  (package
    (name "python-flake8-builtins")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flake8_builtins" version))
       (sha256
        (base32 "19psav7pnqy3m5g4z1zah4ksbnk9bzx1jbbibs631xg44gc3vamx"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list "run_tests.py")))
    (native-inputs
     (list python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-flake8))
    (home-page "https://github.com/gforcada/flake8-builtins")
    (synopsis "Check for python builtins being used as variables or parameters")
    (description
     "This package implements a functionality to check for python builtins
being used as variables or parameters.")
    (license license:gpl2)))

(define-public python-flake8-class-newline
  (package
    (name "python-flake8-class-newline")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/AlexanderVanEck/flake8-class-newline")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15fw0iw2c3a3n2aarfgq7147406489xd8nk0kkj9k2x98fkwwnyh"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-backend #~'unittest))
    (propagated-inputs (list python-flake8))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/AlexanderVanEck/flake8-class-newline")
    (synopsis "Flake8 lint for newline after class definitions")
    (description
     "This package provides a flake8 extension to lint for newline after class
definitions.")
    (license license:expat)))

(define-public python-flake8-comprehensions
  (package
    (name "python-flake8-comprehensions")
    (version "3.16.0")
    (source
     (origin
       (method git-fetch) ;; no tests in the PyPI tarball
       (uri (git-reference
             (url "https://github.com/adamchainz/flake8-comprehensions")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w49k9hv77s5kq60j8j3pbvq7d79rfldqmghlqvn1xxkdkra1v7q"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-flake8-path
           python-pytest-randomly
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-flake8))
    (home-page "https://github.com/adamchainz/flake8-comprehensions")
    (synopsis "List, set and dict comprehensions")
    (description
     "This package provides a flake8 plugin to help you write better
list/set/dict comprehensions.")
    (license license:expat)))

(define-public python-flake8-deprecated
  (package
    (name "python-flake8-deprecated")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flake8_deprecated" version))
       (sha256
        (base32 "18iazzh2l42fcjmkqdwncsl1h2s6sbi26vz4m0gmd80w3l0cm5pf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "run_tests.py")))
    (native-inputs
     (list python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-flake8))
    (home-page "https://github.com/gforcada/flake8-deprecated")
    (synopsis "Warns about deprecated method calls in Python")
    (description
     "This flake8 plugin helps you keep up with method deprecations by
providing hints about what deprecated methods should be replaced with.")
    (license license:gpl2)))

(define-public python-flake8-docstrings
  (package
    (name "python-flake8-docstrings")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pycqa/flake8-docstrings")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a9cx11rz9asb4xkz7dg65kx8mpa74xqh5qp3lsiy74y4idwp9qi"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f)) ;there are no tests
    (propagated-inputs (list python-flake8 python-pydocstyle))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/pycqa/flake8-docstrings")
    (synopsis "Extension for flake8 which uses pydocstyle to check docstrings")
    (description
     "This package provides a extension for flake8 which uses pydocstyle to
check docstrings.")
    (license license:expat)))

(define-public python-flake8-import-order
  (package
    (name "python-flake8-import-order")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flake8_import_order" version))
       (sha256
        (base32 "1cmhpiaj9bgh64mg4y93hcbsifvqa2lriz3la0iy8cbn95akqfqk"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools
           python-pytest
           python-pylama))
    (propagated-inputs
     (list python-pycodestyle))
    (home-page "https://github.com/PyCQA/flake8-import-order")
    (synopsis
     "Flake8 and pylama plugin that checks the ordering of import statements")
    (description
     "This package provieds a flake8 and pylama plugin that checks the ordering
of import statements.")
    (license license:lgpl3)))

(define-public python-flexmock
  (package
    (name "python-flexmock")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flexmock" version))
       (sha256
        (base32 "18dcr7mpldf3cxsqi9rak75n4z7x3j544l4ixdspairm7cf6cp23"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-poetry-core
           python-pytest))
    (home-page "https://flexmock.readthedocs.io/")
    (synopsis "Testing library for Python")
    (description
     "flexmock is a testing library for Python that makes it easy to create
mocks, stubs and fakes.")
    (license license:bsd-3)))

(define-public python-gcovr
  (package
    (name "python-gcovr")
    (version "8.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gcovr" version))
       (sha256
        (base32
          "0ych0kqbxp5giksdkh6i32vh97zrsh3m2rfs02c8rxx7qkwp38zs"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling
           python-hatch-fancy-pypi-readme
           python-hatch-vcs
           python-pytest
           python-pytest-timeout))
    (propagated-inputs
     (list python-lxml
           python-jinja2
           python-colorlog
           python-pygments
           python-tomli))
    (home-page "https://gcovr.com/")
    (synopsis "Utility for generating code coverage results")
    (description
      "Gcovr provides a utility for managing the use of the GNU gcov
utility and generating summarized code coverage results.  It is inspired
by the Python coverage.py package, which provides a similar utility for
Python.")
    (license license:bsd-3)))

(define-public python-green
  (package
    (name "python-green")
    (version "4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "green" version))
       (sha256
        (base32 "1cd62nbn5dvlpnsyplp6cb24wd230san8dpm6pnl99n2kwzpq1m4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-tvvv"
              "green.test.test_version"
              "green.test.test_cmdline")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags  #:allow-other-keys)
              (when tests?
                (setenv "PATH" (string-append #$output "/bin:" (getenv "PATH")))
                (apply invoke "green" test-flags)))))))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-colorama
           python-coverage
           python-lxml
           python-unidecode))
    (home-page "https://github.com/CleanCut/green")
    (synopsis "Clean, colorful, fast Python test runner")
    (description
     "@code{green} is a Python test runner that describes itself as:
@table @emph
@item Clean
Low redundancy in output.  Result statistics for each test is vertically aligned.
@item Colorful
Terminal output makes good use of color when the terminal supports it.
@item Fast
Tests run in independent processes (one per processor by default).
@item Powerful
Multi-target and auto-discovery support.
@item Traditional
It uses the normal @code{unittest} classes and methods.
@item Descriptive
Multiple verbosity levels, from just dots to full docstring output.
@item Convenient
Bash-completion and ZSH-completion of options and test targets.
@item Thorough
Built-in integration with @url{http://nedbatchelder.com/code/coverage/, coverage}.
@end table")
    (license license:expat)))

(define-public python-hiro
  (package
    (name "python-hiro")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alisaifee/hiro")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j7z54nd72qfc065jgljqx53dhfkfz0922fk8qqczg7swmqf6cqv"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-pytest-cov python-setuptools))
    (home-page "https://hiro.readthedocs.io")
    (synopsis "Time manipulation utilities for testing in Python")
    (description
     "Hiro provides context managers and utilities to either freeze,
accelerate or decelerate and jump between different points in time.  Functions
exposed by the standard library’s @code{time}, @code{datetime} and @code{date}
modules are patched within the contexts exposed.")
    (license license:expat)))

(define-public python-httmock
  (package
    (name "python-httmock")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/patrys/httmock")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dy7pjq4gz476jcnbbpzk8w8qxr9l8wwgw9x2c7lf6fzsgnf404q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'custom
      #:test-flags #~(list "tests.py")))
    (native-inputs (list python-setuptools))
    (propagated-inputs (list python-requests))
    (home-page "https://github.com/patrys/httmock")
    (synopsis "Mocking library for requests")
    (description "This package provides a library for replying fake data to
Python software under test, when they make an HTTP query.")
    (license license:asl2.0)))

(define-public python-hypothesmith
  (package
    (name "python-hypothesmith")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hypothesmith" version))
       (sha256
        (base32 "05hpr3iqqsrfvkzdn7wjxp92hjxmin3pch96fn4mvs68sq14ihcn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "-k" (string-append
                    ;; XXX: hypothesis.errors.Unsatisfiable
                    "not test_source_code_from_libcst_node_type[MatchSingleton]"
                    ;; XXX: Python/Black versions not as expected.
                    " and not test_black_autoformatter_from_grammar"))))
    (native-inputs
     (list python-black         ;hard requirements to run tests
           python-parso
           python-pytest
           python-pytest-cov
           python-pytest-xdist
           python-setuptools))
    (propagated-inputs
     (list python-hypothesis
           python-lark
           python-libcst-minimal))
    (home-page "https://github.com/Zac-HD/hypothesmith")
    (synopsis "Strategies for generating Python programs")
    (description
     "This package contains hypothesis strategies for generating Python
programs, something like CSmith, a random generator of C programs.")
    (license license:mpl2.0)))

(define-public python-icontract
  (package
    (name "python-icontract")
    (version "2.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Parquery/icontract")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fix7wx899kn8vp9aa5m6q71la48gx3qqx4qd74535m61pb50r7f"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.py"
                (("asttokens>=2,<3") "asttokens"))))
          (add-before 'check 'set-icontract-slow
            (lambda _
              ;; Setting ICONTRACT_SLOW, does not enable a slow test suite.
              ;; It only causes a single test to run, that checks the value of
              ;; icontract.SLOW is set correctly.
              (setenv "ICONTRACT_SLOW" "1"))))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-asyncstdlib
           python-astor
           python-deal
           python-dpcontracts
           python-mypy
           python-numpy
           python-pytest
           python-setuptools
           python-typeguard))
    (propagated-inputs
     (list python-asttokens
           python-typing-extensions))
    (home-page "https://icontract.readthedocs.io")
    (synopsis "Design-by-contract programming for Python")
    (description
     "@code{icontract} brings design-by-contract to Python with informative
violation messages and inheritance.  @code{icontract} provides two function,
@code{require} and @code{ensure} for preconditions and postconditions
respectively.  Additionally, it provides a class decorator, @code{invariant},
to establish class invariants.")
    (license license:expat)))

(define-public python-inline-snapshot
  (package
    (name "python-inline-snapshot")
    (version "0.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "inline_snapshot" version))
       (sha256
        (base32 "19x5j97i96p3xr9xyjvwh0mmpcnypf8g5hf2jjm6g82ghsv3rrqp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 518 passed, 88 skipped, 1 xfailed, 903 subtests passed
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; To prevent adding mypy and pyright.
              "--ignore=tests/test_typing.py")))
    (native-inputs
     (list python-black         ;XXX: used in tests/conftest.py to self lint
           python-dirty-equals
           python-freezegun
           python-hatchling
           python-hypothesis
           python-pydantic
           python-pytest-bootstrap
           python-pytest-freezer
           python-pytest-mock
           python-pytest-subtests
           python-pytest-xdist))
    (propagated-inputs
     (list python-asttokens
           python-executing
           python-rich))
    (home-page "https://github.com/15r10nk/inline-snapshot/")
    (synopsis "Golden master/snapshot/approval testing library")
    (description
     "This package can be used for different things:

@enumerate
@item golden master/approval/snapshot testing.  The idea is that you have a
  function with a currently unknown result and you want to write a tests, which
  ensures that the result does not change during refactoring.
@item Compare things which are complex like lists with lot of numbers or
  complex data structures.
@item Things which might change during the development like error messages.
@end enumerate

@code{inline-snapshot} automates the process of recording, storing and
updating the value you want to compare with.  The value is converted with
@code{repr()} and stored in the source file as argument of the
@code{snapshot()} function.")
    (license license:expat)))

(define-public python-junit-xml
  ;; XXX: There are no tags or PyPI releases, so take the latest commit
  ;; and use the version defined in setup.py.
  (let ((version "1.9")
        (commit "4bd08a272f059998cedf9b7779f944d49eba13a6")
        (revision "0"))
    (package
      (name "python-junit-xml")
      (version (git-version version revision commit))
      (home-page "https://github.com/kyrus/python-junit-xml")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0b8kbjhk3j10rk0vcniy695m3h43yip6y93h1bd6jjh0cp7s09c7"))))
      (build-system pyproject-build-system)
      (native-inputs (list python-pytest python-setuptools))
      (propagated-inputs (list python-six))
      (synopsis "Create JUnit XML test results")
      (description
       "This package provides a Python module for creating JUnit XML test
result documents that can be read by tools such as Jenkins or Bamboo.")
      (license license:expat))))

(define-public python-line-profiler
  (package
    (name "python-line-profiler")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "line_profiler" version))
       (sha256
        (base32 "15hs8pmv7pcilnhhp0l5pamjihmh7zlnvvpsnf046lbnz0jhzq89"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; XXX: Test can't compare the versions: AssertionError: Version
      ;; Mismatch: kernprof and line_profiler must be in
      ;; sync. kernprof.line_profiler = . kernprof.__version__ = 4.2.0.
      #~(list "--deselect=tests/test_cli.py::test_version_agreement")))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-ubelt
           python-wheel
           python-xdoctest))
    (home-page "https://github.com/pyutils/line_profiler")
    (synopsis "Line-by-line profiler for Python")
    (description
     "This package provides @code{line_profiler} - a Python module for doing
line-by-line profiling of functions.  @code{kernprof} is a convenient script
for running either @code{line_profiler} or the Python standard library's
cProfile or profile modules, depending on what is available.  It's a
successor of @url{https://github.com/rkern/line_profiler}.")
    (license license:bsd-3)))

(define-public python-mamba
  (package
    (name "python-mamba")
    (version "0.11.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/nestorsalceda/mamba")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kyqd6q216srmylklm9ryjkrxrby3b4bs1v9a5wg4aanxmalh6j7"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f)) ;No test
    (propagated-inputs (list python-clint python-coverage))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://nestorsalceda.com/mamba/")
    (synopsis "Test runner for Python")
    (description
     "Mamba is a Behaviour-Driven Development tool for Python developers.
     Is heavily influenced from RSpec, Mocha, Jasmine or Ginkgo.")
    (license license:expat)))

(define-public python-memory-profiler
  (package
    (name "python-memory-profiler")
    (version "0.61")
    (source
     (origin
       ;; PyPi tarball lacks tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pythonprofilers/memory_profiler")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n6g47qqmnn7abh3v25535hd8bmfvhf9bnp72m7bkd89f715m7xh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: @profile is not loaded in some test files and there are 3
          ;; tests fail, disable them for now.
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (with-directory-excursion "test"
                (for-each delete-file
                          '("test_as.py"
                            "test_func.py"
                            "test_gen.py"
                            "test_loop.py"
                            "test_loop_decorated.py"
                            "test_mprofile.py"
                            "test_nested.py"
                            "test_precision_command_line.py"
                            "test_unicode.py")))
              (substitute* "test/test_attributes.py"
                (("def test_with_profile") "def __off_test_with_profile"))
              (substitute* "test/test_stream_unicode.py"
                (("def test_unicode") "def __off_test_unicode"))
              (substitute* "test/test_tracemalloc.py"
                (("def test_memory_profiler")
                 "def __off_test_memory_profiler")))))))
    (native-inputs
     (list python-pytest
           python-pytest-fixture-config
           python-safety
           python-setuptools
           python-wheel))
    (propagated-inputs (list python-psutil))
    (home-page "https://github.com/pythonprofilers/memory_profiler")
    (synopsis "Memory profiler for Python")
    (description
     "This package provides a module for monitoring the memory usage of a
Python program.")
    (license license:bsd-3)))

(define-public python-mockito
  (package
    (name "python-mockito")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/kaste/mockito-python")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fg8jflcf4c929gd4zbcrk73d08waaqjfjmdjrgnv54mzl35pjxl"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-numpy python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/kaste/mockito-python")
    (synopsis "Mocking library for Python")
    (description "This package provides a Python implementation of the Java
library of the same name.  It eases monkey patching, for example to stub out
side effects when unit testing.")
    (license license:expat)))

(define-public python-mypy
  (package
    (name "python-mypy")
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mypy" version))
       (sha256
        (base32
         "1avv8cj0qfhpw4s36bjhg994rml35fs4ndz78xg1r14l4050ml3b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              ;; It tries to download hatchling and install aditional test
              ;; dependencies.
              "--ignore=mypy/test/testpep561.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              ;; The directory '/homeless-shelter/.cache/pip' or its parent
              ;; directory is not owned or is not writable by the current
              ;; user.
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-lxml
           python-psutil
           python-pytest
           python-pytest-xdist
           python-setuptools
           python-types-setuptools
           python-wheel))
    (propagated-inputs
     (list python-mypy-extensions
           python-pathspec
           python-tomli
           python-typing-extensions))
    (home-page "https://www.mypy-lang.org/")
    (synopsis "Static type checker for Python")
    (description "Mypy is an optional static type checker for Python that aims
to combine the benefits of dynamic typing and static typing.  Mypy combines
the expressive power and convenience of Python with a powerful type system and
compile-time type checking.  Mypy type checks standard Python programs; run
them using any Python VM with basically no runtime overhead.")
    ;; Most of the code is under MIT license; Some files are under Python Software
    ;; Foundation License version 2: stdlib-samples/*, mypyc/lib-rt/pythonsupport.h and
    ;; mypyc/lib-rt/getargs.c
    (license (list license:expat license:psfl))))

(define-public python-mypy-extensions
  (package
    (name "python-mypy-extensions")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)       ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/python/mypy_extensions")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12a3qs6rckxljlgw8ylkgcgpwllz96rw82lrgmhlzdgqcnqhbl0w"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-backend #~'unittest
                     #:test-flags #~(list "discover" "tests")))
    (native-inputs
     (list python-flit-core))
    (home-page "https://github.com/python/mypy_extensions")
    (synopsis "Experimental extensions for MyPy")
    (description
     "The @code{python-mypy-extensions} module defines experimental extensions
to the standard @code{typing} module that are supported by the MyPy
typechecker.")
    (license license:expat)))

;;; This variant exists to break a cycle between python-pylama and python-isort.
(define-public python-mypy-minimal
  (hidden-package
   (package
     (inherit python-mypy)
     (name "python-mypy-minimal")
     (arguments
      `(#:tests? #f))
     (native-inputs
      (list python-setuptools
            python-wheel)))))

(define-public python-nbmake
  (package
    (name "python-nbmake")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/treebeardtech/nbmake")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06syl819kwqhmjwp34lri31f0pypwnxs9j03s5lbk12w42mihzdi"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-ipykernel python-nbclient python-nbformat python-pygments))
    (native-inputs
     (list python-poetry-core
           python-pytest
           python-pytest-xdist
           python-pyyaml))
    (home-page "https://github.com/treebeardtech/nbmake")
    (synopsis "Pytest plugin for testing notebooks")
    (description "This package provides a Pytest plugin for testing Jupyter
notebooks.")
    (license license:asl2.0)))

(define-public python-nbval
  (package
    (name "python-nbval")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbval" version))
       (sha256
        (base32 "154h6xpf9h6spgg3ax6k79fd40j197ipwnfjmf5rc2kvc2bmgjbp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This test fails because of a mismatch in the output of LaTeX
      ;; equation environments.  Seems OK to skip.
      #~(list "--ignore=tests/test_nbdime_reporter.py"
              ;; assert <ExitCode.USAGE_ERROR: 4> == 0
              "--deselect=tests/test_coverage.py::test_coverage")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'fix-test
            (lambda _
              ;; This test fails because of a mismatch in the output of LaTeX
              ;; equation environments.  Seems OK to skip.
              (delete-file
               "tests/ipynb-test-samples/test-latex-pass-correctouput.ipynb")
              ;; Prevent adding python-sympy.
              (delete-file
               "tests/ipynb-test-samples/test-latex-pass-failsbutignoreoutput.ipynb"))))))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-coverage
           python-ipykernel
           python-jupyter-client
           python-nbformat))
    (home-page "https://github.com/computationalmodelling/nbval")
    (synopsis "Pytest plugin to validate Jupyter notebooks")
    (description
     "This plugin adds functionality to Pytest to recognise and collect Jupyter
notebooks.  The intended purpose of the tests is to determine whether execution
of the stored inputs match the stored outputs of the @file{.ipynb} file.  Whilst
also ensuring that the notebooks are running without errors.")
    (license license:bsd-3)))

(define-public python-nox
  (package
    (name "python-nox")
    (version "2025.11.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/wntrblm/nox")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19cpsdi158ngn5jskfk50zbjn745nhlijx1w73rcmmqph0rl518r"))))
    (build-system pyproject-build-system)
    ;; tests: 631 passed, 40 skipped
    (arguments
     (list
      #:test-flags
      #~(list "-k"
              (string-join
               ;; XXX: Tests fails to find uv.
               (list "not test_download_python_always_preexisting_interpreter[uv]"
                     "test_download_python_auto_missing_interpreter[uv]"
                     "test_download_python_failed_install[always-uv]"
                     "test_download_python_failed_install[auto-uv]"
                     "test_noxfile_script_mode"
                     "test_noxfile_script_mode_exec"
                     "test_noxfile_script_mode_url_req")
               " and not "))))
    (native-inputs
     (list python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-argcomplete
           python-attrs
           python-colorlog
           python-dependency-groups
           python-humanize
           python-virtualenv))
    (home-page "https://nox.thea.codes/")
    (synopsis "Flexible test automation")
    (description
     "@code{nox} is a command-line tool that automates testing in multiple
Python environments, similar to @code{tox}.  Unlike tox, Nox uses a standard
Python file for configuration.")
    (license license:asl2.0)))

(define-public python-nptyping
  (package
    (name "python-nptyping")
    (version "2.5.0")
    (source (origin
              (method git-fetch)        ;pypi only contains a binary wheel
              (uri (git-reference
                    (url "https://github.com/ramonhagenaars/nptyping")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m6iq98qi9pl5hcc5k99bvy5w293vrlsdnimxl020i60rfnihgl7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         ;; This one started failing with the last update of Numpy.
         "--ignore=tests/test_beartype.py"
         ;; Multiple failures due to undefined names (typing package must be
         ;; too outdated, or perhaps they use a newer pandas).
         "--ignore=tests/test_mypy.py"
         "--ignore=tests/pandas_/test_mypy_dataframe.py"
         "--ignore=tests/pandas_/test_fork_sync.py" ;requires connectivity
         ;; This test requires 'python-pyright', not packaged.
         "--ignore=tests/test_pyright.py"
         ;; This one fails with "Unexpected argument of type <class 'tuple'>".
         "--ignore=tests/test_typeguard.py"
         ;; This one runs pip and fails.
         "--ignore=tests/test_wheel.py")))
    (native-inputs
     (list python-beartype
           python-feedparser
           python-pandas
           python-pytest
           python-setuptools
           python-typeguard))
    (propagated-inputs
     (list python-numpy-1
           python-typing-extensions
           python-pandas-stubs))
    (home-page "https://github.com/ramonhagenaars/nptyping")
    (synopsis "Type hints for Numpy")
    (description "This package provides extensive dynamic type checks for
dtypes and shapes of arrays for NumPy, extending @code{numpy.typing}.")
    (license license:expat)))

(define-public python-pandas-vet
  (package
    (name "python-pandas-vet")
    (version "2023.8.2")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deppen8/pandas-vet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vkc9sa8x6vfmnd24pxp3gjlmbwx926h4y5alkdbbpb9x5h5ml3j"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-version
            (lambda _
              (substitute* "pyproject.toml"
                (("^source = \"regex_commit\"") "")
                (("^tag_sign.*") "")
                (("\\[tool.hatch.version\\]") "")
                (("dynamic = \\[\"version\"\\]")
                 (string-append "version = \"" #$version "\"")))
              (with-output-to-file "src/pandas_vet/__about__.py"
                (let* ((version #$(package-version this-package) )
                       (version-tuple (string-join (string-split version #\.) ", ")))
                  (lambda ()
                    (format #t
                            "__version__ = version = '~a'
__version_tuple__ = version_tuple = (~a)~%" version version-tuple)))))))))
    (propagated-inputs (list python-attrs python-flake8))
    (native-inputs (list python-hatchling python-pytest python-pytest-cov))
    (home-page "https://github.com/deppen8/pandas-vet")
    (synopsis "Opionated @code{flake8} plugin for @code{pandas} code")
    (description
     "This package provides a @code{flake8} plugin to lint @code{pandas} code
in an opinionated way.")
    (license license:expat)))

(define-public python-pep8-naming
  (package
    (name "python-pep8-naming")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pep8_naming" version))
       (sha256
        (base32 "0acxcg4z43kkddlvjbcbbh1jp8rx5z0cq9hz7jlyvpm2mfcs9x7n"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python" "run_tests.py")))))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-flake8))
    (home-page "https://github.com/PyCQA/pep8-naming")
    (synopsis "Check PEP-8 naming conventions")
    (description
     "This package provides the @code{pep8-naming} Python module, a plugin for
flake8 to check PEP-8 naming conventions.")
    (license license:expat)))

(define-public python-pyannotate
  (package
    (name "python-pyannotate")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dropbox/pyannotate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gmhl4ldan0p774dhrs9a7bmjjphlsy3hhfqq84gak15rdjs59ga"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-backend #~'unittest))
    (native-inputs (list python-setuptools))
    (propagated-inputs (list python-mypy-extensions python-six))
    (home-page "https://github.com/dropbox/pyannotate")
    (synopsis "Auto-generate PEP-484 annotations")
    (description
     "This package, PyAnnotate, is used to auto-generate PEP-484 annotations.")
    (license license:asl2.0)))

(define-public python-pycotap
  (package
    (name "python-pycotap")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/remko/pycotap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xw3mrrsw7wc8yas9p2hnzj2m3mw7p1qxkj6l942gidngqphyhar"))))
    (build-system pyproject-build-system)
    (arguments
     ;; FIXME Unclear why tests fail.
     (list #:tests? #f))
    (native-inputs (list python-pytest python-setuptools))
    (home-page "https://github.com/remko/pycotap")
    (synopsis "Tiny Python TAP test runner")
    (description
     "This package provides a simple Python test runner for unittest that
outputs Test Anything Protocol (TAP) results to standard output.  Contrary to
other TAP runners for Python, pycotap...
@itemize
@item
prints TAP (and only TAP) to standard output instead of to a separate file,
allowing you to pipe it directly to TAP pretty printers and processors;
@item only contains a TAP reporter, so no parsers, no frameworks, no
dependencies, etc;
@item
is configurable: you can choose how you want the test output and test result
diagnostics to end up in your TAP output (as TAP diagnostics, YAML blocks, or
attachments).
@end itemize")
    (license license:expat)))

(define-public python-pyflakes
  (package
    (name "python-pyflakes")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyflakes" version))
       (sha256
        (base32 "0gxgz0kg008pgmjk1dn8z3g00dfa9pc3f80pm6r1yqjly4zn0q8w"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/PyCQA/pyflakes")
    (synopsis "Passive checker of Python programs")
    (description
     "Pyflakes statically checks Python source code for common errors.")
    (license license:expat)))

(define-public python-pyinstrument
  (package
    (name "python-pyinstrument")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyinstrument" version))
       (sha256
        (base32 "1a5shhhqy45bqjdahy1lnxikrpq5sv5p610fivz1qg0bk7d1qh5w"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         #$@(map (lambda (test) (string-append "--deselect="
                                               "test/test_cmdline.py::"
                                               "TestCommandLine::"
                                               test))
                 ;; subprocess.CalledProcessError: Command '['pyinstrument',
                 ;; '-m', 'busy_wait_module']' returned non-zero exit status
                 ;; 1.
                 (list "test_module_running[pyinstrument_invocation0]"
                       "test_single_file_module_running[pyinstrument_invocation0]"
                       "test_running_yourself_as_module[pyinstrument_invocation0]"
                       "test_module_execution_details[pyinstrument_invocation0]")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              (delete-file-recursively "pyinstrument"))))))
    (native-inputs
     (list python-flaky
           python-greenlet
           python-pytest
           python-pytest-asyncio
           python-pytest-trio
           python-setuptools))
    (home-page "https://github.com/joerick/pyinstrument")
    (synopsis "Call stack profiler for Python")
    (description
     "Pyinstrument is a Python profiler to help you optimize your code.")
    (license license:bsd-3)))

(define-public python-pylama
  (package
    (name "python-pylama")
    (version "8.4.1")
    (source
     (origin
       (method git-fetch)               ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/klen/pylama")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x9cnyfnd574mj8ckd5hbfg2wy128zg0k2cd3zc7vdbnimksvqaq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Cycles with pylint: python-pylama -> python-isort -> python-pylint ->
      ;; python-pylama
      #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-mypy-minimal
           python-mccabe
           python-pycodestyle
           python-pydocstyle
           python-pyflakes))
    (home-page "https://github.com/klen/pylama")
    (synopsis "Code audit tool for python")
    (description
     "Pylama is a code audit tool for Python and JavaScript to check for
style, syntax and other code health metrics.  It is essentially a convenient
wrapper above tools such as Pyflakes, pydocstyle, pycodestyle and McCabe,
among others.")
    (license license:lgpl3+)))

(define-public python-pynose
  (package
    (name "python-pynose")
    (version "1.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pynose" version))
       (sha256
        (base32 "0jbzmxnxmgf60158gpvfsp8j2cid6psfwj3j94vxv61z8wk4xnl1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            ;; A simple regression tests, taken from project's GitHub Actions,
            ;; see: <.github/workflows/python-package.yml>.
            (lambda* (#:key inputs outputs tests? #:allow-other-keys)
              (when tests?
                (with-output-to-file "nothing.py"
                  (const (display "def test_1(): pass\n")))
                (invoke "pynose" "nothing.py" "--co" "-v")
                (invoke "pynose" "nothing.py" "-v")
                (invoke "nosetests" "nothing.py" "--co" "-v")
                (invoke "nosetests" "nothing.py" "-v")
                (delete-file "nothing.py")))))))
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/mdmintz/pynose")
    (synopsis "Unittest framework for Python")
    (description
     "@code{pynose} is a maintained successor of deprecated @code{nose} unittest
runner.
Changes over @code{nose}:
@itemize
@item fixes @code{AttributeError: module 'collections' has no attribute 'Callable'}
@item fixes @code{AttributeError: module 'inspect' has no attribute 'getargspec'}
@item fixes @code{ImportError: cannot import name '_TextTestResult' from 'unittest'}
@item fixes @code{RuntimeWarning: TestResult has no addDuration method}
@item fixes @code{DeprecationWarning: pkg_resources is deprecated as an API}
@item fixes all @code{flake8} issues from the original nose
@item replaces the imp module with the newer importlib module
@item the default logging level now hides @code{INFO} logs for less noise
@item adds @code{--capture-logs} for hiding output from all logging levels
@item adds @code{--logging-init} to use @code{logging.basicConfig(level)}
@item the @code{-s} option is always active to see the output of @code{print()}
@item adds @code{--capture-output} for hiding the output of @code{print()}
@item adds @code{--co} as a shortcut to using @code{--collect-only}
@end itemize")
    (license license:lgpl2.0)))

(define-public python-pytest-aiohttp
  (package
    (name "python-pytest-aiohttp")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/aio-libs/pytest-aiohttp")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hqj6fqqhvyy61ikwqjcs1p4g3s7z01prsjgck19r8as4pfih5g7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Patch based on
          ;; https://github.com/aio-libs/pytest-aiohttp/pull/115/files
          (add-after 'unpack 'create-pytest-ini
            (lambda _
              (call-with-output-file "pytest.ini"
                (lambda (port)
                  (format port "[pytest]
asyncio_default_fixture_loop_scope = function"))))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-aiohttp
           python-pytest-asyncio))
    (home-page "https://github.com/aio-libs/pytest-aiohttp/")
    (synopsis "Pytest plugin for aiohttp support")
    (description "This package provides a pytest plugin for aiohttp support.")
    (license license:asl2.0)))

(define-public python-pytest-arraydiff
  (package
    (name "python-pytest-arraydiff")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-arraydiff" version))
       (sha256
        (base32 "1pk7v96rkypx4ld59f6p8fh5bq371ka8g7bh4h7n4df91x2v2dr9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-append
                    ;; Disable tests requiring python-astropy, to break cycle.
                    "not test_succeeds_func_fits_hdu"
                    " and not test_fails"
                    " and not test_generate"
                    " and not test_default_format"))))
    (native-inputs
     (list python-pytest python-setuptools-scm
           python-setuptools python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/astropy/pytest-arraydiff")
    (synopsis "Pytest plugin to help with comparing array output from tests")
    (description
     "This is a py.test plugin to facilitate the generation and comparison of
data arrays produced during tests, in particular in cases where the arrays
are too large to conveniently hard-code them in the tests.")
    (license license:bsd-3)))

(define-public python-pytest-asdf-plugin
  (package
    (name "python-pytest-asdf-plugin")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_asdf_plugin" version))
       (sha256
        (base32 "0bcfl1s7yrnr2rlpr3hswcg9jyq6gnj0ppmpzppw9xgj796ycfb5"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f         ;to avoid import astronomy module
           #:phases
           #~(modify-phases %standard-phases
               (delete 'sanity-check))))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))
    (home-page "https://github.com/asdf-format/pytest-asdf-plugin")
    (synopsis "Pytest plugin for testing ASDF schemas")
    (description
     "This package provides a Pytest plugin for testing ASDF schemas.")
    (license license:bsd-3)))

(define-public python-pytest-astropy
  (package
    (name "python-pytest-astropy")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-astropy" version))
       (sha256
        (base32 "1d9rcxnc57rjp96xag1gq725pwl11b3k5hdaz7c3w5lixncsmbjf"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f)) ; there are no tests
    (native-inputs
     (list python-attrs
           python-pytest-mock
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-hypothesis
           python-pytest-arraydiff
           python-pytest-astropy-header
           python-pytest-cov
           python-pytest-doctestplus
           python-pytest-filter-subpackage
           python-pytest-openfiles
           python-pytest-remotedata))
    (home-page "https://github.com/astropy/pytest-astropy")
    (synopsis
     "Metapackage for all the testing machinery used by the Astropy Project")
    (description
     "This is a meta-package that pulls in the dependencies that are used by
astropy related packages.")
    (license license:bsd-3)))

(define-public python-pytest-astropy-header
(package
  (name "python-pytest-astropy-header")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "pytest-astropy-header" version))
      (sha256
        (base32 "046v4arinv8b5jz05pvhnc0n1aqqndwvhlsl635ahxabr40i32bp"))))
  (build-system pyproject-build-system)
  (native-inputs
   (list python-numpy
         python-pytest
         python-setuptools
         python-setuptools-scm))
  (home-page "https://www.astropy.org/")
  (synopsis
   "Pytest plugin adding diagnostic data to the header of the test output")
  (description
    "This plugin package provides a way to include information about the system,
Python installation, and select dependencies in the header of the output when
running pytest.  It can be used with packages that are not affiliated with the
Astropy project, but is optimized for use with astropy-related projects.")
  (license license:bsd-3)))

(define-public python-pytest-benchmark
  (package
    (name "python-pytest-benchmark")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-benchmark" version))
       (sha256
        (base32
         "01d1mk951ldkw589z7f0w8c22sp5341hphflghgj7s4jqb6n39ly"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ;XXX: cycles with python-nbmake
    (propagated-inputs
     (list python-py-cpuinfo))
    (native-inputs
     (list python-pytest-bootstrap python-setuptools))
    (home-page "https://github.com/ionelmc/pytest-benchmark")
    (synopsis "Pytest fixture for benchmarking code")
    (description
     "This package provides a pytest fixture that will group the tests into
rounds that are calibrated to the chosen timer.")
    (license license:bsd-2)))

(define-public python-pytest-black
  (package
    (name "python-pytest-black")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_black" version))
       (sha256
        (base32
         "04dmhv8dzh356qdxz6hrwfz3nk3mlc9shicgpns5r03rydap9dzc"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-8
           python-setuptools))
    (propagated-inputs
     (list python-black
           python-toml))
    ;; Project maintenance has been changed, see
    ;; <https://github.com/shopkeep/pytest-black/issues/70>.
    (home-page "https://github.com/coherent-oss/pytest-black")
    (synopsis "Pytest plugin to enable format checking with black")
    (description
     "This package provides a pytest plugin to enable format checking with the
Python code formatter \"black\".")
    (license license:expat)))

(define-public python-pytest-cases
  (package
    (name "python-pytest-cases")
    (version "3.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_cases" version))
       (sha256
        (base32 "13vzivzca36g3rbz3k3zny7jqv35vsl2z0fl32ik3j95npqq3qf4"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-asyncio
           python-pytest-bootstrap
           python-pytest-harvest
           python-pytest-steps
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-decopatch
           python-makefun
           python-packaging))
    (home-page "https://github.com/smarie/python-pytest-cases")
    (synopsis "Separate test code from test cases in pytest.")
    (description
     "This package provides a Pytest plugin which leverages
@code{@@pytest.mark.parametrize} decorator separating test cases from test
functions.")
    (license license:bsd-3)))

(define-public python-pytest-celery
  (package
    (name "python-pytest-celery")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/celery/pytest-celery")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04s5j456rl2lj4zxrzkynx1lb09bb8qrkn82pladj2q89pzqxh8k"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Disable tests that require docker/rabbit/redis running.
      #~(list "--deselect=tests/integration"
              "-k" "not rabbit and not redis")))
    (native-inputs
     (list python-celery-minimal
           python-memcached             ;optional dependency, needed for tests
           python-poetry-core
           python-pytest
           python-pytest-cov            ;coverage options in pyproject.toml
           python-redis                 ;optional dependency, needed for tests
           python-requests))            ;for python-docker
    (propagated-inputs
     (list python-psutil
           python-pytest-docker-tools
           python-tenacity))
    (home-page "https://github.com/celery/pytest-celery")
    (synopsis "Pytest plugin designed for Celery application developers")
    (description
     "This package enables dynamic orchestration of Celery environments for
testing tasks in isolated conditions, leveraging Docker & pytest-docker-tools
for environment simulation.")
    (license license:bsd-3)))

(define-public python-pytest-check
  (package
    (name "python-pytest-check")
    (version "2.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_check" version))
       (sha256
        (base32 "1jkhmii6zrgzq0427sy9igm7a6nfvx7p4ms91h6d75f3fzgxfmr3"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling python-pytest))
    (home-page "https://github.com/okken/pytest-check")
    (synopsis "Pytest plugin to allow multiple failures")
    (description "This package provides a pytest plugin that allows multiple
failures per test.")
    (license license:expat)))

(define-public python-pytest-checkdocs
  (package
    (name "python-pytest-checkdocs")
    (version "2.10.0")  ;PyPI contains only the latest version
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jaraco/pytest-checkdocs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1amw07skzfwy88dqvcsh308lcds7avyyja0qzdqrk4739cm1g0vh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;tests require network access
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-docutils
           python-pypa-build))
    (home-page "https://github.com/jaraco/pytest-checkdocs")
    (synopsis "Check the README when running tests")
    (description
     "This package provides a pytest plugin that checks the long description
of the project to ensure it renders properly.")
    (license license:expat)))

(define-public python-pytest-click
  (package
    (name "python-pytest-click")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Stranger6667/pytest-click")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "197nvlqlyfrqpy5lrkmfh1ywpr6j9zipxl9d7syg2a2n7jz3a8rj"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-click python-pytest))
    (home-page "https://github.com/Stranger6667/pytest-click")
    (synopsis "Pytest plugin for Click")
    (description
     "This package provides a plugin to test Python click interfaces with
pytest.")
    (license license:expat)))

(define-public python-pytest-console-scripts
  (package
    (name "python-pytest-console-scripts")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-console-scripts" version))
       (sha256
        (base32 "15d8yi6g9wd7g6gkzhp0m3fpnbvnglfkhi4yxc1a5by09kc6x0js"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; FileNotFoundError: [Errno 2] No such file or directory:
                    ;; 'script.py'
                    (list "not test_elsewhere_in_the_path"
                          "test_shell"
                          "test_run_path")
                    " and not "))))
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools))
    (home-page "https://github.com/kvas-it/pytest-console-scripts")
    (synopsis "Pytest plugin for testing console scripts")
    (description
     "This package provides a pytest plugin for testing console scripts.")
    (license license:expat)))

(define-public python-pytest-cookies
  (package
    (name "python-pytest-cookies")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch) ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/hackebrot/pytest-cookies")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x7ny6mx1siy9law1cv1i63nvv9ds2g1dlagm40l8qymxry43mjn"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-cookiecutter))
    (home-page "https://github.com/hackebrot/pytest-cookies")
    (synopsis "Pytest plugin for Cookiecutter templates")
    (description
     "This Pytest plugin adds a @code{cookies} fixture, which is a
wrapper for the Cookiecutter API.  This fixture helps you verify that
your template is working as expected and takes care of cleaning up after
running the tests.")
    (license license:expat)))

(define-public python-pytest-csv
  (package
    (name "python-pytest-csv")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)               ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/nicoulaj/pytest-csv")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17518f2fn5l98lyk9p8r7215c1whi61imzrh6ahrmcksr8w0zz04"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-8
           python-pytest-flake8
           python-pytest-xdist
           python-setuptools
           python-tabulate
           python-wheel))
    (propagated-inputs
     (list python-six))
    (home-page "https://github.com/nicoulaj/pytest-csv")
    (synopsis "CSV reporter for Pytest")
    (description "This package provides a plugin for Pytest that enables a
CSV output mode for Pytest.  It can be enabled via the @option{--csv} option
it adds to the Pytest command line interface (CLI).")
    (license license:gpl3+)))

(define-public python-pytest-cython
  (package
    (name "python-pytest-cython")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-cython" version))
       (sha256
        (base32 "0ma496dgmmrpgqd3zk6vin29dgajcplh63yqd8jh2a3ai954fr22"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; E   ModuleNotFoundError: No module named 'setuptools.sandbox'
      #:tests? #f)) ;XXX: tests are broken
    (native-inputs (list python-cython python-setuptools python-wheel))
    (propagated-inputs (list python-pytest))
    (home-page "https://github.com/lgpage/pytest-cython")
    (synopsis "Cython extension modules testing plugin")
    (description
     "This package provides a plugin for testing Cython extension modules.")
    (license license:expat)))

(define-public python-pytest-datafiles
  (package
    (name "python-pytest-datafiles")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch) ; no tests in the PyPI tarball
       (uri (git-reference
             (url "https://github.com/omarkohl/pytest-datafiles")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wbgfsn4pcdm0bw61pwaaq707mlfnixlff3x8m5mpsf6jhrzql30"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/omarkohl/pytest-datafiles")
    (synopsis "Pytest plugin to create a tmpdir")
    (description
     "A pytest plugin to create a tmpdir containing a preconfigured set of
files and/or directories.")
    (license license:expat)))

(define-public python-pytest-dependency
  (package
    (name "python-pytest-dependency")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-dependency" version))
       (sha256
        (base32 "1hdcidq4miqd5fvg9khvzw3gm3waxnp7wgqr5h39anfr75m0wjwk"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools))
    (home-page "https://github.com/RKrahl/pytest-dependency")
    (synopsis "Manage dependencies of tests")
    (description
     "This pytest plugin manages dependencies of tests.  It allows
to mark some tests as dependent from other tests.  These tests will then be
skipped if any of the dependencies did fail or has been skipped.")
    (license license:asl2.0)))

(define-public python-pytest-docker-tools
  (package
    (name "python-pytest-docker-tools")
    (version "3.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Jc2k/pytest-docker-tools")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06w89kvxqma5ns6gndmk6g048qpv10wdwf61ynii1mm1n0xy11sr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; All other tests seem to require docker daemon running.
      #~(list "tests/test_utils.py")))
    (native-inputs
     (list python-pytest
           python-requests
           python-setuptools))
    (propagated-inputs
     (list python-docker))
    (home-page "https://github.com/Jc2k/pytest-docker-tools")
    (synopsis "Test your built docker image")
    (description
     "This package is a set of opinionated helpers for creating py.test fixtures
for your smoke testing and integration testing.  It strives to keep your
environment definition declarative, like a docker-compose.yml.  It embraces
 py.test fixture overloading.")
    (license license:asl2.0)))

(define-public python-pytest-doctest-custom
  (package
    (name "python-pytest-doctest-custom")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danilobellini/pytest-doctest-custom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hpdfazzvpgyhfr5la9n8k7a1j3z2nvqp76wiyzr73ha5wij33zl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs outputs tests? #:allow-other-keys)
              (when tests?
                (add-installed-pythonpath inputs outputs)
                (invoke "python" "test_pytest_doctest_custom.py")))))))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/danilobellini/pytest-doctest-custom")
    (synopsis
     "Pytest plugin to customize string representations of doctest results")
    (description
     "This package provides a Pytest plugin for customizing string
representations of doctest results.  It can change the display hook used by
doctest to render the object representations.")
    (license license:expat)))

(define-public python-pytest-doctestplus
  (package
    (name "python-pytest-doctestplus")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-doctestplus" version))
       (sha256
        (base32 "0ybn613rp0wqzm97hncwnpn8wx7bz91rajgnclplv8yfr2iahwi4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 44 passed, 6 deselected, 1 xfailed
      #:test-flags
      #~(list "-k" (string-join
                    ;; Tests requiring network access.
                    (list "not test_remote_data_url"
                          "test_remote_data_float_cmp"
                          "test_remote_data_ignore_whitespace"
                          "test_remote_data_ellipsis"
                          "test_remote_data_requires"
                          "test_remote_data_ignore_warnings")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-git-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "pytest_doctestplus/plugin.py"
                (("\"git\"")
                 (format #f "~s" (search-input-file inputs "/bin/git")))))))))
    (native-inputs
     (list git-minimal/pinned
           python-numpy
           python-pytest-bootstrap
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-packaging))
    (home-page "https://github.com/astropy/pytest-doctestplus")
    (synopsis "Pytest plugin with advanced doctest features")
    (description
     "This package contains a plugin for the Pytest framework that provides
advanced doctest support and enables the testing of reStructuredText files.")
    (license license:bsd-3)))

(define-public python-pytest-env
  (package
    (name "python-pytest-env")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_env" version))
       (sha256
        (base32 "1ky11hgb00wdz3mrsfk6zp17r56j99allimcfd83hhqfm909h84i"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-pytest))
    (home-page "https://github.com/MobileDynasty/pytest-env")
    (synopsis "Pytest plugin that allows you to add environment variables")
    (description
     "This is a @code{py.test} plugin that enables you to set environment
variables in the @file{pytest.ini} file.")
    (license license:expat)))

(define-public python-pytest-fail-slow
  (package
    (name "python-pytest-fail-slow")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jwodder/pytest-fail-slow")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xabggi5s6p87dji1f8gg08fxp74g9h2nbj9x9n5vkzmf3hy5l7i"))))
    (build-system pyproject-build-system)
    ;; Each test waits 2-5s which delays completion.
    ;; tests: 317 passed
    (native-inputs
     (list python-hatchling
           python-pytest-bootstrap))
    (propagated-inputs
     (list python-pluggy))
    (home-page "https://github.com/jwodder/pytest-fail-slow")
    (synopsis "Fail tests that take too long to run")
    (description "Pytest plugin for failing tests that take too long to run.")
    (license license:expat)))

(define-public python-pytest-filter-subpackage
  (package
    (name "python-pytest-filter-subpackage")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-filter-subpackage" version))
       (sha256
        (base32 "0mmgg2n8qk6s2kprycjl70lxcpm43dkapplmkf32i0ai6qdqyiiz"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-pytest-doctestplus
           python-setuptools-scm
           python-setuptools
           python-wheel))
    (home-page "https://github.com/astropy/pytest-filter-subpackage")
    (synopsis "Pytest plugin for filtering based on sub-packages")
    (description
     "This package contains a simple plugin for the pytest framework that
provides a shortcut to testing all code and documentation for a given
sub-package.")
    (license license:bsd-3)))

(define-public python-pytest-fixture-config
  ;; TODO: Master branch has removed all Python 2 support such Six, consider
  ;; to update and drop python-six from closure in the next update cyle.
  (package
    (name "python-pytest-fixture-config")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-fixture-config" version))
       (sha256
        (base32
         "03hvviv0a4y0r8pdnj5s3hdbr2d3k9cx0ipjafxbsl88w9gqjff7"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools
           python-six))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Fixture configuration utils for py.test")
    (description
     "This package provides fixture configuration utilities for the py.test
testing framework.")
    (license license:expat)))

(define-public python-pytest-flake8
  (package
    (name "python-pytest-flake8")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_flake8" version))
       (sha256
        (base32
         "1rhz7mxcg7x9dbabfcjai3zxikfgw7az07m4ddf92bg35ib3byw8"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-8
           python-setuptools))
    (propagated-inputs
     (list python-flake8))
    (home-page "https://github.com/tholo/pytest-flake8")
    (synopsis "Pytest plugin to check FLAKE8 requirements")
    (description
     "This package provides a pytest plugin for efficiently checking PEP8
compliance.")
    (license license:bsd-3)))

(define-public python-pytest-flake8-path
  (package
    (name "python-pytest-flake8-path")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)               ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/adamchainz/pytest-flake8-path")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k1lv34jmfirdwa2dpiim8803b6krqy3m7k2knc39fgmzbd6yc8z"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-flake8
           python-pytest))
    (home-page "https://github.com/adamchainz/pytest-flake8-path")
    (synopsis "Pytest fixture for testing flake8 plugins")
    (description
     "This package provides a pytest fixture for testing flake8 plugins.")
    (license license:expat)))

(define-public python-pytest-flakefinder
  (package
    (name "python-pytest-flakefinder")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-flakefinder" version))
       (sha256
        (base32 "03iy80xlkpgzjs2kxa9rrj8dbnp9awyhpcl3hy8fgf5x40cjlhg2"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-wheel python-setuptools))
    (propagated-inputs (list python-pytest))
    (home-page "https://github.com/dropbox/pytest-flakefinder")
    (synopsis "Pytest plugin for finding flaky tests")
    (description "This package provides a Pytest plugin to run tests multiple
times and detect flakyness.")
    (license license:asl2.0)))

(define-public python-pytest-flask
  (package
    (name "python-pytest-flask")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-flask" version))
       (sha256
        (base32 "0pm93xli1pvq9w053grndi84hxq087mr2xhagvac98qvnabirgjq"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-flask))
    (home-page "https://github.com/pytest-dev/pytest-flask")
    (synopsis "Pytest fixtures to test Flask applications")
    (description
     "This pytest plugin provides fixtures to simplify Flask app testing.")
    (license license:expat)))

(define-public python-pytest-freezer
  (package
    (name "python-pytest-freezer")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_freezer" version))
       (sha256
        (base32 "0an8y6ri3bhij4137gphdw2yg6rq7if4nb1qjj7zjsy4kjy1dgr1"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-flit-core))
    (propagated-inputs
     (list python-freezegun
           python-pytest))
    (home-page "https://github.com/pytest-dev/pytest-freezer/")
    (synopsis "Pytest plugin providing a fixture interface for spulec/freezegun")
    (description
     "Pytest plugin providing a fixture interface for
@url{https://github.com/spulec/freezegun, freezegun}.")
    (license license:expat)))

(define-public python-pytest-harvest
  (package
    (name "python-pytest-harvest")
    (version "1.10.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-harvest" version))
       (sha256
        (base32 "066lqx46hqlvllq6ppmyi47fjc1dww7jwa4wfkkx2hrf3z7s9kr7"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ;XXX: cycle with python-pytest-harvest
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-decopatch
           python-makefun
           python-packaging))
    (home-page "https://github.com/smarie/python-pytest-harvest")
    (synopsis "Pytest plugin to store data during runs")
    (description
     "This package implements a functionality to store data created during your
pytest tests execution, and retrieve it at the end of the session, e.g. for
applicative benchmarking purposes.")
    (license license:bsd-3)))

(define-public python-pytest-helpers-namespace
  (package
    (name "python-pytest-helpers-namespace")
    (version "2021.3.24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/saltstack/pytest-helpers-namespace")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ikwiwp9ycgg7px78nxdkqvg7j97krb6vzqlb8fq8fvbwrj4q4v2"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-setuptools-scm
                         python-setuptools-declarative-requirements
                         python-wheel))
    (home-page "https://github.com/saltstack/pytest-helpers-namespace")
    (synopsis "Pytest Helpers Namespace Plugin")
    (description
     "Pytest Helpers Namespace Plugin provides a helpers pytest namespace
which can be used to register helper functions without requiring someone to
import them in their actual tests to use them.")
    (license license:asl2.0)))

(define-public python-pytest-home
  (package
    (name "python-pytest-home")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jaraco/pytest-home")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "151xx48dahbh7yx2a9cr9f2iy2i6f7s3zsm4zn5apvgl9qmjhkk7"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pytest))
    (native-inputs
     (list git-minimal
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (home-page "https://github.com/jaraco/pytest-home")
    (synopsis "Home directory fixtures")
    (description
     "This package provides home directory fixtures for pytest.")
    (license license:expat)))

(define-public python-pytest-html
  (package
    (name "python-pytest-html")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_html" version))
       (sha256
        (base32 "01vgd2bbk3n9wcqzx9dv72qgkx684l8cp92n9c3ll3w0wn51x83h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; It requires running browser for selenium.
      #~(list "--ignore=testing/test_integration.py"
              "--ignore=testing/test_e2e.py")))
    (native-inputs
     (list python-hatchling
           python-hatch-vcs
           python-assertpy
           python-beautifulsoup4
           python-pytest))
    (propagated-inputs
     (list python-jinja2
           python-pytest-metadata))
    (home-page "https://github.com/pytest-dev/pytest-html")
    (synopsis "Pytest plugin for generating HTML reports")
    (description
     "This package provides a pytest plugin for generating HTML reports.")
    (license license:mpl2.0)))

(define-public python-pytest-httpx
  (package
    (name "python-pytest-httpx")
    (version "0.35.0")
    (source
     (origin
       ;; pypi package doesn't include the tests
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Colin-b/pytest_httpx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w8qwhcaq1l63kfj4ncsi3355ln37ws6066mxr0b9646g68wp69v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: 261 failed, 170 errors
      #:tests? #f))
    (native-inputs
     (list python-pytest-8
           python-pytest-asyncio
           python-setuptools
           python-wheel))
    (propagated-inputs (list python-httpx))
    (home-page "https://colin-b.github.io/pytest_httpx/")
    (synopsis "Pytest plugin to mock httpx")
    (description "This package provides a pytest fixture to mock httpx
requests to be replied to with user provided responses.")
    (license license:expat)))

(define-public python-pytest-isort
  (package
    (name "python-pytest-isort")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/stephrdev/pytest-isort")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bhcmn4589l228n05bn8mgk7chyr9262bm9zxpwhcdiyhyspdjvw"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-isort python-pytest))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/stephrdev/pytest-isort")
    (synopsis "Pytest plugin to check import ordering using isort")
    (description
     "This package provides a pytest plugin to check import ordering using
isort.")
    (license license:bsd-3)))

(define-public python-pytest-metadata
  (package
    (name "python-pytest-metadata")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_metadata" version))
       (sha256
        (base32 "1j0ph028mj81314vxb027d5b98xii3zl2vd9i8b3zh7val1rp8nj"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling
           python-hatch-vcs
           python-pytest
           python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-metadata")
    (synopsis "Access test session metadata with Pytest")
    (description
     "@code{pytest-metadata} is a @command{pytest} plugin that provides
access to test session metadata.")
    (license license:mpl2.0)))

(define-public python-pytest-mockito
  (package
    (name "python-pytest-mockito")
    (version "0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kaste/pytest-mockito")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rq4mb1ycs3l1mpl682ybycvywmf4cp3vlrv9r1a9d2cb6qdwz8r"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-pytest-bootstrap
           python-setuptools-scm))
    (propagated-inputs
     (list python-mockito))
    (home-page "https://github.com/kaste/pytest-mockito")
    (synopsis "Mockito base fixtures for Pytest")
    (description
     "The @code{pytest-mockito} plugin provides base Mockito fixtures for
Pytest.  It covers the main entry points of the Mockito mocking framework and
makes it easy to undo any monkey patching.

The fixtures are:
@itemize
@item when
@item when2
@item expect
@item patch
@item unstub
@item spy2
@end itemize")
     (license license:expat)))

(define-public python-pytest-mpi
  (package
    (name "python-pytest-mpi")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mpi" version))
       (sha256
        (base32 "1a954cai5lr327np5f38mg8gw91p4akx8m2z416wvwzq24swvcq9"))))
    (build-system pyproject-build-system)
    (arguments
     ;; See <https://github.com/aragilar/pytest-mpi/issues/4>.
     (list #:test-flags
           #~(list "-p" "pytester"
                   #$@(if (target-aarch64?)
                          ;; Some tests fail on non x86_64 more likely the
                          ;; project does not support any other ones, see
                          ;; <ci/azure-pipelines-steps.yml>.
                          ;; ValueError: Pytest terminal summary report not found
                          '("-k" (string-join
                                  (list "not test_mpi_file_name"
                                        "test_mpi_only_mpi"
                                        "test_mpi_skip_under_mpi"
                                        "test_mpi_tmp_path"
                                        "test_mpi_tmpdir"
                                        "test_mpi_with_mpi"
                                        "test_mpi_xfail_under_mpi")
                                  " and not "))
                          '()))))
    (native-inputs
     (list openmpi
           python-pytest
           python-setuptools
           python-sybil
           python-wheel))
    (home-page "https://pytest-mpi.readthedocs.io")
    (synopsis "Pytest plugin to collect information from tests")
    (description
     "@code{pytest_mpi} is a plugin for pytest providing some useful tools
when running tests under MPI, and testing MPI-related code.")
    (license license:bsd-3)))

(define-public python-pytest-mpl
  (package
    (name "python-pytest-mpl")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mpl" version))
       (sha256
        (base32 "1inaaafzxgbxldz4xqvx68gpj8p5i30qlsgva8sb7d34wvbhbvzv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-m" "mpl_image_compare")))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-jinja2
           python-matplotlib
           python-packaging
           python-pillow))
    (home-page "https://github.com/matplotlib/pytest-mpl")
    (synopsis "Pytest plugin to help with testing figures output from Matplotlib")
    (description
     "This is a plugin to facilitate image comparison for Matplotlib figures
in Pytest.")
    (license license:bsd-3)))

(define-public python-pytest-openfiles
  (package
    (name "python-pytest-openfiles")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_openfiles" version))
       (sha256
        (base32 "14x9f1l9a5ghf527i5qfcfa003mkrky1dhx2hfwq5nma9v1n0lgz"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-psutil))
    (home-page "https://github.com/astropy/pytest-openfiles")
    (synopsis "Pytest plugin for detecting inadvertent open file handles")
    (description
     "This package provides a plugin for the pytest framework that allows
developers to detect whether any file handles or other file-like objects
were inadvertently left open at the end of a unit test.")
    (license license:bsd-3)))

(define-public python-pytest-order
  (package
    (name "python-pytest-order")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_order" version))
       (sha256
        (base32 "1pixy83l6hcg16gjc04vp4misk2w989alkd9msnw1s9y7pn8yq2i"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: 4 failed, 18 errors
      #:tests? #f))
    (native-inputs
     (list python-pytest python-pytest-xdist
           python-setuptools python-wheel))
    (home-page "https://github.com/pytest-dev/pytest-order")
    (synopsis "Pytest plugin to run your tests in a specific order")
    (description
     "This plugin defines Pytest markers to ensure that some tests, or groups
of tests run in a specific order.")
    (license license:expat)))

(define-public python-pytest-parawtf
  (package
    (name "python-pytest-parawtf")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-parawtf" version))
       (sha256
        (base32 "08s86hy58lvrd90cnayzydvac4slaflj0ph9yknakcc42anrm023"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; https://github.com/flub/pytest-parawtf/issues/1
      #~(list "-k" "not test_mark")))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-pytest))
    (home-page "https://github.com/flub/pytest-parawtf/")
    (synopsis "Finally spell paramete?ri[sz]e correctly")
    (description
     "Pytest uses one of four different spellings of parametrize.  This plugin
allows you to use all four.")
    (license license:expat)))

(define-public python-pytest-pycodestyle
  (package
    (name "python-pytest-pycodestyle")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_pycodestyle" version))
       (sha256
        (base32
         "1jdm5arsh150fvph0960kycb1cwj728mksfwxb65bbbl4zaypkr7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: pytest failed to import 'py.io', while python can.
      #:tests? #f))
    (propagated-inputs
     (list python-py python-pycodestyle python-pytest))
    (native-inputs
     (list python-pytest-isort python-setuptools python-wheel))
    (home-page "https://github.com/henry0312/pytest-pycodestyle")
    (synopsis "Pytest plugin to run pycodestyle")
    (description "This package provides a plugin to run @code{pycodestyle}
for the @code{pytest} framework.")
    (license license:expat)))

(define-public python-pytest-pydocstyle
  (package
    (name "python-pytest-pydocstyle")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/henry0312/pytest-pydocstyle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08jaz92pzq6lqg64jbl9f6j0gdb622wl0qb2llfcy82grx2vv09q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: pytest failed to import 'py.io', while python can.
      #:tests? #f))
    (native-inputs
     (list python-pytest
           python-pytest-isort
           python-pytest-pycodestyle
           python-setuptools
           python-wheel))
    (propagated-inputs (list python-pydocstyle python-pytest)) ;apparently required
    (home-page "https://github.com/henry0312/pytest-pydocstyle")
    (synopsis "Pytest plugin to run @command{pydocstyle}")
    (description
     "This package provides a Pytest plugin to run @command{pydocstyle}.")
    (license license:expat)))

(define-public python-pytest-pylint
  (package
    (name "python-pytest-pylint")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-pylint" version))
       (sha256
        (base32 "0gjm9qy1rsngvli042szqc45y0q5zk1crq28ja01iyjw3n74nxl8"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-8
           python-setuptools))
    (propagated-inputs
     (list python-pylint))
    (home-page "https://github.com/carsongee/pytest-pylint")
    (synopsis "Pytest plugin to check source code with Pylint")
    (description
     "This plugin allows running Pylint with Pytest and have configurable rule
types (i.e. Convention, Warn, and Error) fail the build.")
    (license license:expat)))

(define-public python-pytest-qt
  (package
    (name "python-pytest-qt")
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_qt" version))
       (sha256
        (base32 "17ridbk4gsr1aclgs1ai55r6k2ngrg0wn9b40g96bw48qh0hwqji"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
         (add-before 'check 'set-qpa
           (lambda _ (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs
     (list python-pyqt-6
           python-pytest-bootstrap
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-pluggy
           python-typing-extensions))
    (home-page "https://github.com/pytest-dev/pytest-qt")
    (synopsis "Pytest support for PyQt and PySide applications")
    (description
     "@code{pytest-qt} is a Pytest plugin that allows programmers to write
tests for PyQt5 and PySide2 applications.

The main usage is to use the @code{qtbot} fixture, responsible for handling
@code{qApp} creation as needed and provides methods to simulate user
interaction, like key presses and mouse clicks.")
    (license license:expat)))

(define-public python-pytest-randomly
  (package
    (name "python-pytest-randomly")
    (version "4.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pytest-dev/pytest-randomly")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zglnyl3wc2ri9dhkvd1z0ywksk2v1abpdlclc253c8xivv4c3ai"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 23 passed, 13 deselected
      #:test-flags
      #~(list "-p" "no:randomly"
              ;; Skip tests requireing python-factory-boy, python-faker,
              ;; python-numpy, and python-pytest-xdist to reduce closure size.
              "-k" (string-join
                    (list "not test_entrypoint_injection"
                          "test_factory_boy"
                          "test_faker"
                          "test_faker_fixture"
                          "test_it_runs_before_stepwise"
                          "test_model_bakery"
                          "test_numpy"
                          "test_numpy_doesnt_crash_with_large_seed"
                          "test_xdist")
                    " and not "))))
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools))
    (home-page "https://github.com/pytest-dev/pytest-randomly")
    (synopsis "Pytest plugin to randomly order tests")
    (description
     "This is a Pytest plugin to randomly order tests and control Python's
@code{random.seed}.")
    (license license:expat)))


(define-public python-pytest-recording
  (package
    (name "python-pytest-recording")
    (version "0.13.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_recording" version))
       (sha256
        (base32 "133nj8vha63gv226f0gvqn16gnazbn2rqh8amv2fx4jrm2r693an"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ;XXX: more than 50% tets failed
    (native-inputs
     (list python-pytest-bootstrap
           python-hatchling))
    (propagated-inputs
     (list  python-vcrpy))
    (home-page "https://github.com/kiwicom/pytest-recording")
    (synopsis "Pytest support for recording and replaying HTTP traffic")
    (description
     "This package provides a Pytest plugin powered by
@url{https://vcrpy.readthedocs.io/en/latest/, VCR.py} to record and replay
HTTP traffic.")
    (license license:expat)))

(define-public python-pytest-regressions
  (package
    (name "python-pytest-regressions")
    (version "2.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_regressions" version))
       (sha256
        (base32 "08fjzhsp4akdzn08d0nx2b9k16iad7wvdw4fqwv3sap0pq40gn8s"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; To reduce closure size it prevents including python-numpy,
      ;; python-pandas and python-matplotlib.
      #~(list "--ignore=tests/test_dataframe_regression.py"
              "--ignore=tests/test_ndarrays_regression.py"
              "--ignore=tests/test_num_regression.py"
              "--deselect=tests/test_filenames.py::test_foo"
              "--deselect=tests/test_filenames.py::TestClass::test_foo"
              "--deselect=tests/test_filenames.py::TestClassWithIgnoredName::test_foo"
              "--deselect=tests/test_image_regression.py::test_image_regression"
              "--deselect=tests/test_image_regression.py::test_image_regression_workflow")))
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-pytest-datadir
           python-pyyaml))
    (home-page "https://github.com/ESSS/pytest-regressions")
    (synopsis "Easy to use fixtures to write regression tests")
    (description
     "This plugin makes it simple to test general data, images, files, and
numeric tables by saving expected data in a data directory (courtesy of
pytest-datadir) that can be used to verify that future runs produce the same
data.")
    (license license:expat)))

(define-public python-pytest-remotedata
  (package
    (name "python-pytest-remotedata")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-remotedata" version))
       (sha256
        (base32 "0ndvnj9zghfj17haphrygiri9iy38wb8lwq1xdkfvlfd73v8ph05"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "-k" (string-join
                    ;; Network access is required.
                    (list "not test_internet_access"
                          ;; Failed with assertion error.
                          "test_default_behavior"
                          "test_strict_with_decorator")
                    " and not "))))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-packaging))
    (home-page "https://github.com/astropy/pytest-remotedata")
    (synopsis "Pytest plugin for controlling remote data access")
    (description
     "This package provides a plugin for the Pytest framework that allows
developers to control unit tests that require access to data from the
internet.")
    (license license:bsd-3)))

(define-public python-pytest-repeat
  (package
    (name "python-pytest-repeat")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_repeat" version))
       (sha256
        (base32 "11a449zn7lhjyjjw40sv2c63i0mwr7q1cpbyj7kczzx6z96w2anr"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling
           python-pytest-bootstrap))
    (home-page "https://github.com/pytest-dev/pytest-repeat")
    (synopsis "Pytest plugin for repeating tests")
    (description "@code{pytest-repeat} is a plugin for Pytest that makes it
enables repeating a single test, or multiple tests, a specific number of
times.")
    (license license:mpl2.0)))

(define-public python-pytest-rerunfailures
  (package
    (name "python-pytest-rerunfailures")
    (version "15.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_rerunfailures" version))
       (sha256
        (base32 "106fiqn3d86xcl2cwmc76svi2mkfgphqna37bf617f6pmdl06166"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools))
    (propagated-inputs
     (list python-packaging))
    (home-page "https://github.com/pytest-dev/pytest-rerunfailures")
    (synopsis "Pytest plugin to re-run flaky tests")
    (description "This package provides a pytest plugin to re-run tests to
eliminate flaky failures.")
    (license license:mpl2.0)))

(define-public python-pytest-retry
  (package
    (name "python-pytest-retry")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_retry" version))
       (sha256
        (base32 "03zqgl2y16pcf0w0sn7z9n1gaqmkspl9xfhigks9v50yy0wj7mgq"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools))
    (home-page "https://github.com/str0zzapreti/pytest-retry")
    (synopsis "Pytest plugin to retry flaky tests in CI environments")
    (description
     "This package provides a plugin for Pytest which adds the ability to retry
flaky tests, thereby improving the consistency of the test suite results.")
    (license license:expat)))

(define-public python-pytest-run-parallel
  (package
    (name "python-pytest-run-parallel")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_run_parallel" version))
       (sha256
        (base32 "1qiz0kpjzqad9s5kr1rx4zxncy4zfghzb85cl7hjzca0j34agbji"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-bootstrap
           python-pytest-order
           python-pytest-xdist
           python-setuptools))
    (home-page "https://github.com/Quansight-Labs/pytest-run-parallel")
    (synopsis "Pytest plugin to run tests concurrently")
    (description
     "This package provides a simple pytest plugin to run tests concurrently.
 The main goal of pytest-run-parallel is to discover thread-safety issues that
could exist when using C libraries, this is of vital importance after
@url{https://peps.python.org/pep-0703/, PEP703}, which provides a path for a
CPython implementation without depending on the Global Interpreter Lock (GIL),
thus allowing for proper parallelism in programs that make use of the CPython
interpreter.")
    (license license:expat)))

;; This is only used by python-sanic
(define-public python-pytest-sanic
  (package
    (name "python-pytest-sanic")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-sanic" version))
              (sha256
               (base32
                "0shq1bqnydj0l3ipb73j1qh5kqcjvzkps30zk8grq3dwmh3wmnkr"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Tests depend on python-sanic.
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-poetry-core
            (lambda _
              ;; Patch to use the core poetry API.
              (substitute* "pyproject.toml"
                (("poetry.masonry.api") "poetry.core.masonry.api")))))))
    (native-inputs
     (list python-poetry-core))
    (propagated-inputs
     (list python-httpx python-async-generator python-pytest
           python-websockets))
    (home-page "https://github.com/yunstanford/pytest-sanic")
    (synopsis "Pytest plugin for Sanic")
    (description "This package provides a pytest plugin for Sanic.  It helps
you to test your code asynchronously.")
    (license license:expat)))

(define-public python-pytest-services
  (package
    (name "python-pytest-services")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pytest-dev/pytest-services")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "123s2vd3h5knfs6lz7b83z0wl2miqsbya3w71cm8xk6hgyb10nmv"))))
    (build-system pyproject-build-system)
    (arguments
     ;; XXX: Tests require running memcached, mysql and X servers.
     (list #:tests? #f))
    (propagated-inputs (list python-psutil python-requests))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/pytest-dev/pytest-services")
    (synopsis "Services plugin for pytest testing framework")
    (description
     "This plugin provides a set of fixtures and utility functions to start
service processes for your tests with pytest.")
    (license license:expat)))

(define-public python-pytest-shard
  (let ((commit "64610a08dac6b0511b6d51cf895d0e1040d162ad")
        (revision "0"))
    (package
      (name "python-pytest-shard")
      (version (git-version "0.1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AdamGleave/pytest-shard")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1h31m68igz670bzl307hazjrfbr8pk14mxflllar18ydmlrnl677"))))
      (build-system pyproject-build-system)
      (native-inputs (list python-setuptools python-wheel))
      (propagated-inputs (list python-pytest))
      (home-page "https://github.com/AdamGleave/pytest-shard")
      (synopsis "Pytest plugin for sharding tests")
      (description "This package provides a Pytest extension for sharding
tests at the granularity of individual test cases, which can be run in
parallel and on multiple machines.")
      (license license:expat))))

(define-public python-pytest-shutil
  (package
    (name "python-pytest-shutil")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-shutil" version))
       (sha256
        (base32
         "18283zgs3z61paymzf0pp5x3di3hg3m91pvb3v7bmz3fggphdl5a"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              (mkdir "/tmp/bin")
              (substitute* "tests/integration/test_cmdline_integration.py"
                (("dirname = '/bin'")
                 "dirname = '/tmp/bin'")
                (("bindir = os.path.realpath\\('/bin'\\)")
                 "bindir = os.path.realpath('/tmp/bin')")))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-git
           python-wheel))
    (propagated-inputs
     (list python-execnet
           python-mock
           python-path
           python-six
           python-termcolor))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Assorted shell and environment tools for py.test")
    (description
     "This package provides assorted shell and environment tools for the
py.test testing framework.")
    (license license:expat)))

(define-public python-pytest-snapshot
  (package
    (name "python-pytest-snapshot")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-snapshot" version))
       (sha256
        (base32 "1wxp9pv5yqpj3fk450ld1mjhhdxyvssgi6gqxyghz1iyphx3q0f7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Skip failing test. Related upstream issue:
      ;; <https://github.com/joseph-roitman/pytest-snapshot/issues/71>.
      #:test-flags #~(list "-k" "not test_assert_match_failure_bytes")))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/joseph-roitman/pytest-snapshot")
    (synopsis "Pytest plugin for snapshot testing")
    (description
     "This package provides a plugin for snapshot testing with pytest.  It can
be used to test that the value of an expression does not change
unexpectedly.")
    (license license:expat)))

(define-public python-pytest-socket
  (package
    (name "python-pytest-socket")
    (version "0.7.0")
    (source
     (origin
       ;; There are no tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/miketheman/pytest-socket")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m6s07gvljq82hiajzy1v123kpkciziiqdjqfnas169rmzg0bmnp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-append
                    ;; Disable test requiring network access.
                    "not test_disable_socket_urllib"
                    " and not test_parametrize_with_socket_enabled_and_allow_hosts"
                    " and not test_global_disable_and_allow_host"
                    " and not test_asynctest"
                    " and not test_httpx_fails"
                    " and not test_disabled_urllib_fails"
                    " and not test_urllib_succeeds_by_default"
                    " and not test_enabled_urllib_succeeds"
                    " and not test_single_cli_arg_connect_disabled_hostname_resolved"))
     #:phases
       #~(modify-phases %standard-phases
           ;; See <https://github.com/miketheman/pytest-socket/issues/308>
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* (list "tests/test_async.py"
                                "tests/test_socket.py"
                                "tests/test_precedence.py")
               (("from tests.common import assert_socket_blocked")
                "from common import assert_socket_blocked")))))))
    (native-inputs (list python-httpx
                         python-poetry-core
                         python-pypa-build
                         python-pytest
                         python-pytest-httpbin
                         python-pytest-randomly
                         python-starlette))
    (home-page "https://pypi.org/project/pytest-socket/")
    (synopsis "Pytest plugin to disable socket calls during tests")
    (description
     "This package provides Pytest extension which disables all network calls flowing
through Python's socket interface")
    (license license:expat)))

(define-public python-pytest-split
  (package
    (name "python-pytest-split")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jerry-git/pytest-split")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "1w42zkw22h0ydfhbjdjp93frbrzi1rlkr17ifb9kavcbv7kfqxfl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                (("--cov.*") "")
                (("--no-cov-on-fail.*") "")))))))
    (native-inputs
     (list python-poetry-core
           python-pytest-bootstrap))
    (home-page "https://jerry-git.github.io/pytest-split/")
    (synopsis "Pytest plugin to split the test suite to equally sized sub sutes")
    (description
     "This package provides Pytest plugin which splits the test suite to equally
sized sub suites based on
test execution time.")
    (license license:expat)))

(define-public python-pytest-steps
  (package
    (name "python-pytest-steps")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-steps" version))
       (sha256
        (base32 "05r2ch7191saj7sw6d47bfa5vnyyj157dl8hvlcc78xx6jyxy46j"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ;XXX: cycles with python-pytest-harvest
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-makefun
           python-wrapt))
    (home-page "https://github.com/smarie/python-pytest-steps")
    (synopsis "Pytest plugin to create step-wise / incremental tests")
    (description
     "This package implements a functionality to share a state / intermediate
 results across test steps.")
    (license license:bsd-3)))

(define-public python-pytest-subprocess
  (package
    (name "python-pytest-subprocess")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)               ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/aklajnert/pytest-subprocess")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1yb5y6dqzf6k5a07yzdpw8w50bm7zbsdvv06ii7c7vyg9wx5iw6y"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; 134 passed, 4 skipped, 4 deselected
      #:test-flags
      ;; XXX: Skip tests fixed on master in
      ;; be30d9a94ba45afb600717e3fcd95b8b2ff2c60e commit, not critical,
      ;; re-chech in the next refresh cycle.
      #~(list "--deselect=tests/test_examples.py::test_documentation[README.rst]"
              "--deselect=tests/test_examples.py::test_documentation[docs/index.rst]"
              "--deselect=tests/test_subprocess.py::test_text[False]"
              "--deselect=tests/test_subprocess.py::test_universal_newlines[False]")))
    (native-inputs
     (list python-anyio
           python-docutils
           python-pygments
           python-pytest
           python-pytest-asyncio
           python-pytest-rerunfailures
           python-setuptools))
    (home-page "https://github.com/aklajnert/pytest-subprocess")
    (synopsis "Fake subprocess for Pytest")
    (description
     "This package provides a plugin to fake subprocess for Pytest.")
    (license license:expat)))

;; XXX: This project was archived by the owner on Nov 11, 2025. It is now
;; read-only.
(define-public python-pytest-subtests
  (package
    (name "python-pytest-subtests")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_subtests" version))
       (sha256
        (base32 "0z487yinp3gxh2dk6z52d4a17d7d4ymdz2hbix5ph6sm0pg5njfb"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-8
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-attrs))
    (home-page "https://github.com/pytest-dev/pytest-subtests")
    (synopsis "Unittest subTest() support and subtests fixture")
    (description "This Pytest plugin provides unittest @code{subTest()}
support and @code{subtests} fixture.")
    (license license:expat)))

(define-public python-pytest-testmon
  (package
    (name "python-pytest-testmon")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tarpas/pytest-testmon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f3rz17w6pb3gi8c1bc4sjfmdxpdqzhm371fsh36svdc9fn3nm05"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #false)) ;there are none
    (native-inputs
     (list python-coverage
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/tarpas/pytest-testmon")
    (synopsis "Selects tests affected by changed files and methods")
    (description
     "This plug-in auto-selects and reruns tests impacted by recent changes.")
    (license license:expat)))

(define-public python-pytest-textual-snapshot
  (package
    (name "python-pytest-textual-snapshot")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Textualize/pytest-textual-snapshot")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16zwybmjw16pxcm9qdql14xh3fj4iwry8r219yzjd5z7w1l31p12"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; no tests in PyPI or Git
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                (("syrupy = .*")
                 "syrupy = \"*\"\n"))))
          (add-after 'unpack 'patch-path
            (lambda _
              ;; Taken from NixOS package definition.
              (substitute* "pytest_textual_snapshot.py"
                (("this_file_path.parent")
                 (string-append "Path('" #$output
                                "/share/pytest-textual-snapshot/')")))))
          (add-after 'install 'post-install
            (lambda _
              (install-file "./resources/snapshot_report_template.jinja2"
                            (string-append #$output
                                           "/share/pytest-textual-snapshot/resources/")))))))
    (native-inputs
     (list python-pytest
           python-poetry-core))
    (propagated-inputs
     (list python-jinja2
           python-rich
           python-syrupy
           python-textual))
    (home-page "https://github.com/Textualize/pytest-textual-snapshot")
    (synopsis "Pytest plugin for snapshot testing Textual applications")
    (description
     "This package implements a functionality to save an SVG screenshot of a
running Textual app to disk.  The next time the test runs, it takes another
screenshot and compares it to the saved one.  If the new screenshot differs
from the old one, the test fails.  This is a convenient way to quickly and
automatically detect visual regressions in your applications.")
    (license license:expat)))

(define-public python-pytest-tornado
  (package
    (name "python-pytest-tornado")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)        ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/eugeniy/pytest-tornado")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05hgq1m9g35kpc01im7ci1wd85xi1rdxnyms9izjg65c9976zn6x"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-wheel))
    (propagated-inputs
     (list python-pytest
           python-setuptools
           python-tornado))
    (home-page "https://github.com/eugeniy/pytest-tornado")
    (synopsis "Pytest plugin to ease testing tornado applications")
    (description
     "This package provides a py.test plugin providing fixtures and markers to
simplify testing of asynchronous tornado applications.")
    (license license:asl2.0)))

(define-public python-pytest-tornasync
  (package
    (name "python-pytest-tornasync")
    (version "0.6.0.post2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/eukaryote/pytest-tornasync")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iwaxvaxx9v0s1sx4kh90kpf1krzwqh73sg6lv3f2gvh0wjym85f"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools))
    (propagated-inputs
     (list python-tornado))
    (home-page "https://github.com/eukaryote/pytest-tornasync")
    (synopsis "Pytest plugin for testing Tornado code")
    (description
     "This package provides a simple pytest plugin that provides some helpful
fixtures for testing Tornado (version 5.0 or newer) apps and easy handling of
plain (undecoratored) native coroutine tests.")
    (license license:expat)))

(define-public python-pytest-trio
  (package
    (name "python-pytest-trio")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-trio" version))
       (sha256
        (base32 "0bmmdyjqj5v4a637i4rzm55crv6v3nj268as6x9nr7m76rixnqw3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests are broken, see
      ;; <https://github.com/python-trio/pytest-trio/issues/84>.
      #:tests? #f))
    (native-inputs
     (list python-hypothesis
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-async-generator
           python-outcome
           python-pytest
           python-trio))
    (home-page "https://github.com/python-trio/pytest-trio")
    (synopsis "Pytest plugin for trio")
    (description
     "This is a pytest plugin to help you test projects that use Trio, a
friendly library for concurrency and async I/O in Python.")
    ;; Either license applies.
    (license (list license:expat license:asl2.0))))

(define-public python-pytest-twisted
  (package
    (name "python-pytest-twisted")
    (version "1.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_twisted" version))
       (sha256
        (base32 "0gkz7ybdj45v4mmfyyryx6lz75hizi23zi9n5mcsdnqfpk5m1q9p"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-append
                    ;; AssertionError
                    "not test_sigint_for_regular_tests"
                    ;; TimeoutExpired
                    " and not test_sigint_for_inline_callbacks_tests"))))
    (propagated-inputs (list python-decorator python-greenlet))
    (native-inputs (list python-pytest
                         python-setuptools
                         python-twisted
                         python-wheel))
    (home-page "https://github.com/pytest-dev/pytest-twisted")
    (synopsis "Twisted plugin for Pytest")
    (description "This package provides a Twisted plugin for Pytest.")
    (license license:bsd-3)))

(define-public python-pytest-vcr
  ;; This commit fixes integration with pytest-5
  (let ((commit "4d6c7b3e379a6a7cba0b8f9d20b704dc976e9f05")
        (revision "1"))
    (package
      (name "python-pytest-vcr")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ktosiek/pytest-vcr")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1yk988zi0la6zpcm3fff0mxf942di2jiymrfqas19nyngj5ygaqs"))))
      (build-system pyproject-build-system)
      (native-inputs (list python-urllib3 python-setuptools python-wheel))
      (propagated-inputs (list python-pytest python-vcrpy))
      (home-page "https://github.com/ktosiek/pytest-vcr")
      (synopsis "Plugin for managing VCR.py cassettes")
      (description
       "This package is a pytest plugin for managing VCR.py cassettes.")
      (license license:expat))))

(define-public python-pytest-virtualenv
  (package
    (name "python-pytest-virtualenv")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-virtualenv" version))
       (sha256
        (base32
         "1ig1jwgs89r9vxdr12fgxvv9r05bnf5d18lxyn13xciivwwi16al"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--ignore=tests/integration/")
      #:phases
      #~(modify-phases %standard-phases
          ;; Reference the virtualenv executable directly, to avoid the need
          ;; for PYTHONPATH, which gets cleared when instantiating a new
          ;; virtualenv with pytest-virtualenv.
          (add-after 'unpack 'patch-virtualenv-executable
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((virtualenv #$(this-package-input "python-virtualenv"))
                     (virtualenv-bin (string-append virtualenv
                                                    "/bin/virtualenv")))
                (substitute* "pytest_virtualenv.py"
                  (("^DEFAULT_VIRTUALENV_FIXTURE_EXECUTABLE.*$")
                   (format #f "DEFAULT_VIRTUALENV_FIXTURE_EXECUTABLE = '~a'"
                           virtualenv-bin)))))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-git
           python-wheel))
    (propagated-inputs
     (list python-importlib-metadata
           python-pytest-shutil
           python-pytest-fixture-config
           python-virtualenv))
    (home-page "https://github.com/man-group/pytest-plugins")
    (synopsis "Virtualenv fixture for py.test")
    (description "This package provides a virtualenv fixture for the py.test
framework.")
    (license license:expat)))

(define-public python-pytest-xvfb
  (package
    (name "python-pytest-xvfb")
    (version "3.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/The-Compiler/pytest-xvfb")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p27h1idqja2lz2xnk4fsb9c8kafvgy1zpa84lg9d1hlamr221ja"))))
    (build-system pyproject-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
         (add-before 'build 'prepare-tests
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0")
             ;; This test is meant to run on Windows.
             (delete-file "tests/test_xvfb_windows.py"))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           xorg-server-for-tests))
    (propagated-inputs
     (list python-pyvirtualdisplay))
    (home-page "https://github.com/The-Compiler/pytest-xvfb")
    (synopsis "Pytest plugin to run Xvfb for tests")
    (description
     "This package provides a Pytest plugin to run Xvfb for tests.")
    (license license:expat)))

(define-public python-re-assert
  (package
    (name "python-re-assert")
    (version "1.1.0")
    (source
     (origin
       ;; There are no tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asottile/re-assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rssq4wpqmx1c17hjfx5l3sn3zmnlz9jffddiqrs4f6h7m6cadai"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-setuptools))
    (propagated-inputs
     (list python-regex))
    (home-page "https://github.com/asottile/re-assert")
    (synopsis "Show where your regex match assertion failed")
    (description
     "@code{re-assert} provides a helper class to make assertions of regexes
simpler.")
    (license license:expat)))

(define-public python-respx
  (package
    (name "python-respx")
    (version "0.22.0")
    (source
     (origin
       ;; There are no tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lundberg/respx/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pl6vlyva837bnz3cy9mwmvvh8fq943rkrbq3mzj34bjf8swnw2g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              ;; Drop test coverage requirements.
              (substitute* "setup.cfg"
                (("--cov(-[^ ]*)?=[^ ]*")
                 "\n")
                (("--cov-fail-under [^ ]*")
                 "\n")))))))
    (propagated-inputs (list python-httpx))
    (native-inputs (list nss-certs-for-test
                         python-starlette
                         python-flask
                         python-pytest
                         python-pytest-asyncio
                         python-setuptools
                         python-wheel))
    (home-page "https://lundberg.github.io/respx/")
    (synopsis "Mocking for Python libraries HTTPX and HTTPCore")
    (description
     "This package provides a utility for mocking out the Python libraries HTTPX and
HTTPCore.")
    (license license:bsd-3)))

(define-public python-robber
  (package
    (name "python-robber")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "robber" version))
       (sha256
        (base32 "0xp5csgv2g9q38hscml6bc5i1nm4xy5lzqqiimm2drxsf0hw2nq5"))))
    (build-system pyproject-build-system)
    ;; There are no tests in the tarball downloaded from PyPI.
    ;; The last version tagged in Github (0.1.0) is older than the one on PyPI.
    ;; Reported upstream: <https://github.com/vesln/robber.py/issues/20>.
    (arguments
     '(#:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-mock python-termcolor))
    ;; URL of the fork used to generate the package available on PyPI.
    (home-page "https://github.com/EastAgile/robber.py")
    (synopsis "Test-driven development (TDD) assertion library for Python")
    (description "Robber is a Python assertion library for test-driven and
behavior-driven development (TDD and BDD).")
    (license license:expat)))

(define-public python-robotframework-jsonlibrary
  (package
    (name "python-robotframework-jsonlibrary")
    (version "0.5")
    (source
     (origin
       (method git-fetch)   ; no tests data in PyPi package
       (uri (git-reference
             (url (string-append "https://github.com/robotframework-thailand/"
                                 "robotframework-jsonlibrary"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zkhcmwlp9gy9a0262ylykr9pljq9mpkaa69340hhfkzygzi30dc"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-jsonpath-ng
           python-jsonschema
           python-robotframework))
    (home-page "https://github.com/robotframework-thailand/robotframework-jsonlibrary")
    (synopsis "Robot Framework test library for manipulating JSON Object")
    (description
     "@code{robotframework-jsonlibrary} is a Robot Framework test library for
manipulating JSON Object.  You can manipulate your JSON object using JSONPath")
    ;; This is free and unencumbered software released into the public domain.
    (license license:unlicense)))

(define-public python-scspell3k
  (let ((commit "df550351f255c572c1a74852d233c83bbfbd49fb")
        (revision "0"))
    (package
      (name "python-scspell3k")
      (version (git-version "2.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/myint/scspell")
               (commit commit)))
         (sha256
          (base32 "0d7yhja9hrw4w7vm10h56hm1dqyhrnwia7wzc3ap9f15ldkkp9cs"))
         (file-name (git-file-name name version))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-project-license
              (lambda _
                (substitute* "pyproject.toml"
                  (("\"GPL-2.0-only\"")
                   "{ text = \"GPL-2.0-only\" }"))))
            (add-before 'check 'pre-check
              (lambda _
                (setenv "HOME" "/tmp")))
            (add-after 'check 'run-cram-tests
              (lambda _
                (invoke "cram" "--indent=4" "--verbose" "./test.cram"))))))
      (native-inputs
       (list python-cram
             python-pytest
             python-setuptools
             python-wheel))
      (home-page "https://github.com/myint/scspell")
      (synopsis "Conservative interactive spell checker for source code")
      (description
       "This package implements a spell checker for source code that does not
try to be particularly smart and instead does the simplest thing that can
possibly work.")
      (license license:gpl2))))

(define-public python-slotscheck
  (package
    (name "python-slotscheck")
    (version "0.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ariebovenberg/slotscheck")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lakwgk20aq92sqdwsswnll2w3y6p42x8abb9q8fc2qvw3xhw2vh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Optional: ModuleNotFoundError: No module named 'mypyc'
      #:test-flags #~(list "-k" "not test_extension_package")))
    (native-inputs
     (list python-poetry-core
           python-pytest
           python-pytest-mock
           python-ujson))
    (propagated-inputs
     (list python-click
           python-tomli))
    (home-page "https://github.com/ariebovenberg/slotscheck")
    (synopsis "Ensure @code{__slots__} are working properly")
    (description
     "@code{slotscheck} is a tool to validate Python class @code{__slots__}.")
    (license license:expat)))

(define-public python-stestr
  (package
    (name "python-stestr")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stestr" version))
       (sha256
        (base32 "17623fqkg3a0z7rx8jcxwvgx6afg6wzvj4q6cgip5hqw5ngn7v25"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--test-path" "stestr/tests"
              "--exclude-regex" (string-join
                                 (list "test_pass"
                                       "test_pass_list"
                                       "test_unexpected_pass"
                                       "test_load_from_stdin_quiet"
                                       "test_trace_with_all_skips")
                                 "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'configure-check
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list python-ddt
           python-iso8601
           python-flit-core
           python-setuptools))
    (propagated-inputs
     (list python-cliff-bootstrap
           python-fixtures
           python-pyyaml
           python-subunit
           python-testtools
           python-tomlkit
           python-voluptuous))
    (home-page "https://stestr.readthedocs.io/en/latest/")
    (synopsis "Parallel Python test runner")
    (description
     "This package provides the @command{stestr} command, a parallel Python
test runner built around @code{subunit}.  It is designed to execute
@code{unittest} test suites using multiple processes to split up execution of
a test suite.  It will also store a history of all test runs to help in
debugging failures and optimizing the scheduler to improve speed.")
    (license license:asl2.0)))

(define-public python-sure
  ;; No release for 2y but the master branch has fresh changes, use the latest
  ;; commit for now, see <https://github.com/gabrielfalcao/sure/issues/184>,
  ;; <https://github.com/gabrielfalcao/sure/issues/182>.
  (let ((commit "acf823a2e240a2efe93360316d3816e90366439a")
        (revision "0"))
    (package
      (name "python-sure")
      (version (git-version "2.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gabrielfalcao/sure")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12qd3as4ixhwvf8ppx1dwhghda2kcb85ygd4x3ci9mbvkh25fs01"))))
      (build-system pyproject-build-system)
      (native-inputs
       (list python-mock
             python-pytest
             python-pytest-cov
             python-setuptools
             python-wheel))
      (propagated-inputs
       (list python-couleur))
      (home-page "https://github.com/gabrielfalcao/sure")
      (synopsis "Automated testing library in python for python")
      (description
       "Sure is a python library that leverages a DSL for writing
assertions. Sure is heavily inspired by @code{RSpec Expectations} and
@code{should.js}.")
      (license license:gpl3+))))

(define-public python-sybil
  (package
    (name "python-sybil")
    (version "9.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/simplistix/sybil")
              (commit version)))
        (sha256
         (base32 "0r491k91fi2nb0kdd6di8cb2kxcvsk1xzw3sgwsxhhg4qynsp3bi"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-myst-parser
                         python-pytest
                         python-pyyaml
                         python-seedir
                         python-setuptools
                         python-testfixtures))
    (home-page "https://github.com/simplistix/sybil")
    (synopsis "Automated testing for examples in code and documentation")
    (description
      "This library provides a way to check examples in your code and
documentation by parsing them from their source and evaluating the
parsed examples as part of your normal test run.  Integration is
provided for the main Python test runners.")
    (license license:expat)))

(define-public python-syrupy
  (package
    (name "python-syrupy")
    (version "5.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/syrupy-project/syrupy")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ysm42an2pf4ppd1i5yzh11bq1rfydhg6rmmh5v91gcixpvi872d"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; XXX: This test is incompatible with running with xdist.
      #~(list "-k" "not test_update_failure_shows_snapshot_diff[xdist_two]")))
    (native-inputs
     (list python-poetry-core
           python-pytest-bootstrap
           python-pytest-xdist
           python-setuptools-scm))
    (home-page "https://github.com/syrupy-project/syrupy")
    (synopsis "Pytest Snapshot Test Utility")
    (description
     "This package implements a functionality to write tests which assert
immutability of computed results.")
    (license license:asl2.0)))

(define-public python-tappy
  (package
    (name "python-tappy")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tap_py" version))
       (sha256
        (base32 "026n47b46z07yh5z5vpffcfq2xp6850g2s8w9ycssvx5y1m9wg6h"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-backend #~'custom
                     #:test-flags #~(list "tests/run.py")))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/python-tap/tappy")
    (synopsis "Tools for Test Anything Protocol")
    (description "Tappy is a set of tools for working with the Test Anything
Protocol (TAP) in Python.  TAP is a line based test protocol for recording test
data in a standard way.")
    (license license:bsd-3)))

(define-public python-test-utils
  (package
    (name "python-test-utils")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "test-utils" version))
       (sha256
        (base32 "0cs0gyihnkj8ya4yg3ld3ly73mpxrkn2gq9acamclhqvhxsv7zd6"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))             ; No tests, neither on pypi nor upstream.
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/Kami/python-test-utils/")
    (synopsis "Utilities for functional and integration tests")
    (description
     "This package provides a collection of utility functions and classes
which make writing and running functional and integration tests easier.")
    (license license:asl2.0)))

(define-public python-test2ref
  (package
    (name "python-test2ref")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "test2ref" version))
       (sha256
        (base32 "1jx7cdqwpyq3gs9czvz0fwijkqhvmbny5h3zgdqlbrw8y3miv4gq"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pdm-backend
           python-pytest
           python-pytest-cov))
    (propagated-inputs
     (list python-binaryornot))
    (home-page "https://github.com/nbiotcloud/test2ref")
    (synopsis "Testing Against Learned Reference Data")
    (description
     "This package provides a unit tests framework backed by ML features and
working in two modes:

@itemize
@item Testing: Test result in @code{tmp_path} is compared against a known
reference. Any deviation in the files, causes a fail.
@item Learning: The test result in @code{tmp_path} is taken as reference and
is copied to the reference folder, which should be committed to version
control and kept as reference.
@end itemize")
    (license license:expat)))

(define-public python-testfixtures
  (package
    (name "python-testfixtures")
    (version "9.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testfixtures" version))
       (sha256
        (base32 "10hlw2y1rjzmfm87dlisbiwf0zyjbnj0q471799j69wlagrrqzji"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))          ; PyTest-Django fails to build in master
    (native-inputs
     (list python-setuptools))
    (home-page "https://testfixtures.readthedocs.io/en/latest/")
    (synopsis "Tests components for Python")
    (description
     "Testfixtures is a collection of helpers and mock objects that are useful
when writing automated tests in Python.")
    (license license:expat)))

(define-public python-time-machine
  (package
    (name "python-time-machine")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "time_machine" version))
       (sha256
        (base32 "1qn7cj9lx3m7pwa8ak1106f9c54yvpa996x84gfqmyfjfg1ar6aa"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))          ;no tests in PyPI archive
    (propagated-inputs (list python-dateutil))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/adamchainz/time-machine")
    (synopsis "Travel through time in your tests.")
    (description "This package lets you set a different time for your tests.")
    (license license:expat)))

(define-public python-tox
  ;; NOTE: Try to avoid including it in inputs, it's for the local development
  ;; only.
  (package
    (name "python-tox")
    (version "4.23.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tox" version))
       (sha256
        (base32
         "0b2a5wrjwryjzg58v1mwzx3wn7pr2wwk7z2cwy16xpsmwl05w1w6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; These freeze the test suite
                    (list "not test_parallel"
                          "test_parallel_live"
                          ;; Needs internet access
                          "test_build_wheel_external"
                          "test_run_installpkg_targz"
                          "test_python_generate_hash_seed"
                          ;; XXX Tries to call python-wrapper-3.10.7/bin/tox
                          "test_call_as_exe"
                          ;; assert 'covdefaults>=1.2; python_version == "2.7"
                          ;; or python_version == "3.11"' == 'sphinx>=3'
                          "test_load_dependency_many_extra")
                    " and not "))))
    (propagated-inputs
     (list python-cachetools
           python-chardet
           python-colorama
           python-filelock
           python-packaging
           python-platformdirs
           python-pluggy
           python-pyproject-api
           python-tomli
           python-typing-extensions
           python-virtualenv))
    (native-inputs
     (list python-devpi-process
           python-flaky
           python-hatch-vcs
           python-hatchling
           python-psutil
           python-pytest
           python-pytest-mock
           python-pytest-xdist
           python-re-assert
           python-time-machine
           python-wheel))
    (home-page "https://tox.readthedocs.io")
    (synopsis "Virtualenv-based automation of test activities")
    (description "Tox is a generic virtualenv management and test command line
tool.  It can be used to check that a package installs correctly with
different Python versions and interpreters, or run tests in each type of
supported environment, or act as a frontend to continuous integration
servers.")
    (license license:expat)))

(define-public python-validate-pyproject
  (package
    (name "python-validate-pyproject")
    (version "0.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "validate_pyproject" version))
       (sha256
        (base32 "0wbbksrfaxc2c7y305wjinkk4y1jxdnc0vzfbf79ipaa6m8zr0p1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; TODO: Full test suite requires schemas from
      ;; <https://json.schemastore.org/pyproject.json> and obtained during CI
      ;; just before running tests, figure out how to get schema files to
      ;; cover all test cases, see <tools/cache_urls_for_tests.py> and
      ;; <.github/workflows/ci.yml>.
      #~(list "--ignore=tests/test_examples.py"
              "--ignore=tests/test_pre_compile.py"
              "-k" (string-join
                    (list "not test_downloaded"
                          "test_valid_download_only_once"
                          "test_cache_open_url")
                    " and not "))))
    (native-inputs
     (list python-pytest
           python-packaging
           python-pytest-cov
           python-setuptools
           python-setuptools-scm
           python-trove-classifiers
           python-wheel))
    (propagated-inputs
     (list python-fastjsonschema))
    (home-page "https://github.com/abravalheri/validate-pyproject/")
    (synopsis "Validation library for simple check on @code{pyproject.toml}")
    (description
     "Validation library and CLI tool for checking on @code{pyproject.toml}
files using JSON Schema.")
    (license (list license:mpl2.0
                   license:expat
                   license:bsd-3))))

(define-public python-vcrpy
  (package
    (name "python-vcrpy")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vcrpy" version))
       (sha256
        (base32 "0l1sdfc51024jclqv9104nagpirxx8w0gcn5h0bdxv950jnr2qqp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--ignore=tests/integration"
              "-k" (string-join
                    ;; These tests require network access.
                    (list "not testing_connect"
                          "test_get_vcr_with_matcher"
                          "test_testcase_playback")
                    " and not "))))
    (native-inputs
     (list python-aiohttp
           python-boto3
           python-httplib2
           python-httpx
           python-pytest
           python-pytest-cov
           python-pytest-aiohttp
           python-pytest-asyncio
           python-pytest-httpbin
           python-requests
           python-setuptools
           python-tornado
           python-urllib3
           python-werkzeug
           python-wheel))
    (propagated-inputs
     (list python-pyyaml python-wrapt python-yarl))
    (home-page "https://github.com/kevin1024/vcrpy")
    (synopsis "Automatically mock your HTTP interactions")
    (description
     "VCR.py simplifies and speeds up tests that make HTTP requests.  The first
time you run code that is inside a VCR.py context manager or decorated function,
VCR.py records all HTTP interactions that take place through the libraries it
supports and serializes and writes them to a flat file (in yaml format by
default).  This flat file is called a cassette.  When the relevant piece of code
is executed again, VCR.py will read the serialized requests and responses from
the aforementioned cassette file, and intercept any HTTP requests that it
recognizes from the original test run and return the responses that corresponded
to those requests.  This means that the requests will not actually result in
HTTP traffic, which confers several benefits including:
@enumerate
@item The ability to work offline
@item Completely deterministic tests
@item Increased test execution speed
@end enumerate
If the server you are testing against ever changes its API, all you need to do
is delete your existing cassette files, and run your tests again.  VCR.py will
detect the absence of a cassette file and once again record all HTTP
interactions, which will update them to correspond to the new API.")
    (license license:expat)))

(define-public python-vulture
  (package
    (name "python-vulture")
    (version "2.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vulture" version))
       (sha256
        (base32 "05c4kfg6s2zf7lzplq53ihjf19knf3pmpv4nnzmdwf0i5a87g0nb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    (list
                     ;; Skip test that uses python-pint pint has many
                     ;; dependencies.
                     "not test_whitelists_with_python"
                     ;; FileNotFoundError: [Errno 2] No such file or
                     ;; directory: 'pytype'
                     "test_pytype")
                    " and not "))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-toml))
    (home-page "https://github.com/jendrikseipp/vulture")
    (synopsis "Find dead Python code")
    (description
     "Vulture finds unused code in Python programs.  This is useful for
cleaning up and finding errors in large code bases.  If you run Vulture on
both your library and test suite you can find untested code.  Due to Python's
dynamic nature, static code analyzers like Vulture are likely to miss some
dead code.  Also, code that is only called implicitly may be reported as
unused.")
    (license license:expat)))

(define-public python-xdoctest
  (package
    (name "python-xdoctest")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xdoctest" version))
       (sha256
        (base32 "1m69yvc3bl9jj5av89p9jl08w9lsn0k3lqclpdbiq0g67fdbjb7r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; A writable HOME is needed by the 'import_module_from_path'
              ;; test.
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list cmake-minimal
           ninja
           pybind11
           python-pytest-bootstrap
           python-scikit-build
           python-setuptools))
    (home-page "https://github.com/Erotemic/xdoctest")
    (synopsis "Rewrite of the Python builtin doctest module")
    (description
     "This package provides a rewrite of the builtin doctest module which
 leverages the Python @acronym{AST, Abstract Syntax Tree} instead of
@acronym{REGEXPs, regular expressions}.")
    (license license:asl2.0)))

(define-public tms
  (package
    (name "tms")
    (version "0.1.2")
    (source
     (origin
       (method hg-fetch)
       (uri (hg-reference
              (url "https://hg.sr.ht/~olly/tms")
              (changeset (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "1k8v8vx0klz3zfj81g9d1rancn819sv51lgs5j94x69kqzgn3fsw"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-backend #~'custom
           #:test-flags #~(list "tests.py")))
    (native-inputs (list python-setuptools))
    (home-page "https://hg.sr.ht/~olly/tms")
    (synopsis "Test Match Special for test assertions")
    (description
     "This package provides match data structures and types in test code.")
    (license license:bsd-2)))
