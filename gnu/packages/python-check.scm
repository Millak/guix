;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2018, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019, 2021-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020, 2021, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2022 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2021-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Sebastian Gibb <mail@sebastiangibb.de>
;;; Copyright © 2022 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Tomasz Jeneralczyk <tj@schwi.pl>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2024-2025 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Navid Afkhami <navid.afkhami@mdc-berlin.de>
;;; Copyright © 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2024 Markku Korkeala <markku.korkeala@iki.fi>
;;; Copyright © 2025 Evgeny Pisemsky <mail@pisemsky.site>
;;; Copyright © 2025 Florent Pruvost <florent.pruvost@inria.fr>
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
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages django)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages maths)
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
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public python-aioresponses
  (package
    (name "python-aioresponses")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aioresponses" version))
       (sha256
        (base32 "16p8mdyfirddrsay62ji7rwcrqmmzxzf2isdbfm9cj5p338rbr42"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke
                "pytest" "-vv" "tests" "-k"
                (string-append
                 ;; These tests require network access.
                 "not test_address_as_instance_of_url_combined_with_pass_through "
                 "and not test_pass_through_with_origin_params"))))))))
    (native-inputs
     (list python-pbr python-ddt python-pytest))
    (propagated-inputs
     (list python-aiohttp python-setuptools))
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
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "allpairspy" version))
       (sha256
        (base32 "1c987h13dly9919d15w3h747rgn50ilnv7dginhlprxbj564hn4k"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-pytest-runner))
    (home-page "https://github.com/thombashi/allpairspy")
    (synopsis "Pairwise test combinations generator")
    (description
     "This is a Python library for test combinations generator.  The generator
allows one to create a set of tests using @emph{pairwise combinations} method,
reducing a number of combinations of variables into a lesser set that covers
most situations.")
    (license license:expat)))

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
       (method url-fetch)
       (uri (pypi-uri "assertpy" version))
       (sha256
        (base32 "0cs8xya465wvb9dw0kdl7cvkxwrslhbma66y44r1mmsajcll7imc"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
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
    (build-system python-build-system)
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
    (native-inputs (list bash-minimal coreutils-minimal perl sudo))
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
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bandit" version))
       (sha256
        (base32 "0fhr0rsvh44ix31dwxjw8aj0wklj95368djwk0i98c2dcpmpp17m"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Two tets fail.
      #~(list "--exclude-regex" "test_no_arguments|test_help_arg")
      #:phases
      #~(modify-phases %standard-phases
          ;; TODO: Implement in pypproject-build-system's  test-backends.
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (native-inputs
     (list python-beautifulsoup4
           python-fixtures
           python-setuptools
           python-stestr
           python-testscenarios
           python-testtools
           python-wheel))
    (propagated-inputs
     (list python-gitpython
           python-jschema-to-python
           python-pyyaml
           python-rich
           python-sarif-om
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
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "beartype" version))
       (sha256
        (base32 "0wv598iv9c2s6ivfiara9pnkdlnas8xjw063wvyi0dswpb0xyhny"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling python-numpy python-pygments python-pytest))
    (home-page "https://github.com/beartype/beartype")
    (synopsis "Fast runtime type checking for Python")
    (description "Beartype aims to be a very fast runtime type checking tool
written in pure Python.")
    (license license:expat)))

(define-public python-codacy-coverage
  (package
    (name "python-codacy-coverage")
    (version "1.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "codacy-coverage" version))
        (sha256
         (base32
          "1g0c0w56xdkmqb8slacyw5qhzrkp814ng3ddh2lkiij58y9m2imr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
     (list python-check-manifest python-requests))
    (home-page "https://github.com/codacy/python-codacy-coverage")
    (synopsis "Codacy coverage reporter for Python")
    (description "This package analyses Python test suites and reports how much
of the code is covered by them.  This tool is part of the Codacy suite for
analysing code quality.")
    (license license:expat)))

(define-public python-covdefaults
  (package
    (name "python-covdefaults")
    (version "1.1.0")
    (source
     (origin
       ;; The PyPI tarball does not include tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asottile/covdefaults")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11a24c0wzv01n55fy4kdpnyqna4m9k0mp58kmhiaks34xw4k37hq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-coverage python-pytest))
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
    (home-page "https://github.com/coveralls-clients/coveralls-python")
    (source
     (origin
       ;; The PyPI release lacks tests, so we pull from git instead.
       (method git-fetch)
       (uri (git-reference (url home-page) (commit version)))
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
    (propagated-inputs
     (list python-coverage python-docopt python-pyyaml python-requests))
    (native-inputs
     (list poetry python-mock python-pytest python-responses))
    (synopsis "Show coverage stats online via coveralls.io")
    (description
     "Coveralls.io is a service for publishing code coverage statistics online.
This package provides seamless integration with coverage.py (and thus pytest,
nosetests, etc...) in Python projects.")
    (license license:expat)))

(define-public python-crosshair
  (package
    (name "python-crosshair")
    (version "0.0.86")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "crosshair_tool" version))
       (sha256
        (base32 "19zrv6gsap0qwn4rrs1wwajg0gkq7ys8qijsilmjrhc73dylgl72"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              ;; check_examples_test.py contains failing tests that
              ;; show what happens if a counterexample is found.
              "--ignore=crosshair/examples/check_examples_test.py"
              "--ignore=crosshair/lsp_server_test.py") ;requires pygls
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-dependencies
            (lambda _
              (substitute* "setup.py"
                ;; pygls is only used by crosshair/lsp_server.py.
                (("pygls>=1.0.0") "")
                ;; 'sanity-check fails for z3-solver, although it is
                ;; included in 'propagated-inputs.
                (("z3-solver>=4.13.0.0") ""))))
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
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-importlib-metadata
           python-packaging
           ;; python-pygls
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
     (list python-pathpy
           python-pytest
           python-pytest-html
           python-pyyaml
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/cucumber/tag-expressions")
    (synopsis "Tag-expression parser for cucumber/behave")
    (description
     "This package provides a tag-expression parser for Cucumber and
@command{behave}.")
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

(define-public python-eradicate
  (package
    (name "python-eradicate")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "eradicate" version))
       (sha256
        (base32
         "1j30g9jfmbfki383qxwrfds8b23yiwywj40lng4lqcf5yab4ahr7"))))
    (build-system python-build-system)
    (home-page "https://github.com/myint/eradicate")
    (synopsis "Remove commented-out code from Python sources")
    (description "The @command{eradicate} command removes commented-out code
from Python files.  It does this by detecting block comments that contain
valid Python syntax that are likely to be commented out code.")
    (license license:expat)))

(define-public python-expecttest
  (let ((commit "683b09a352cc426851adc2e3a9f46e0ab25e4dee")
        (revision "0"))
    (package
      (name "python-expecttest")
      (version (git-version "0.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ezyang/expecttest")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1djwxp9x1hczzxbimv1b1bmd083am88v27l82nmlkhvzyg2cmpvv"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  ;; The test runs tests expected to fail, so the output is
                  ;; confusing
                  (invoke "python3" "test_expecttest.py")))))))
      (native-inputs (list python-hypothesis poetry))
      (home-page "https://github.com/ezyang/expecttest")
      (synopsis "Python module for expect tests")
      (description "@code{expecttest} is a Python module for expect tests, where
the initial expected value of a test can be automatically set by running the
test itself.")
      (license license:expat))))

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
      #:test-flags #~(list "-vr" "green")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "python" "-m" "green" test-flags)))))))
    (native-inputs
     (list python-mypy
           python-setuptools
           python-testtools
           python-wheel))
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
       (method url-fetch)
       (uri (pypi-uri "hiro" version))
       (sha256
        (base32 "0s2xz72i7kbm0l75vr04cqq2war74p3p376wm76999f93npkjcys"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://hiro.readthedocs.io/")
    (synopsis "Time manipulation utilities for testing in Python")
    (description "Hiro provides context managers and utilities to either
freeze, accelerate or decelerate and jump between different points in time.
Functions exposed by the standard library’s @code{time}, @code{datetime} and
@code{date} modules are patched within the contexts exposed.")
    (license license:expat)))

(define-public python-httmock
  (package
    (name "python-httmock")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "httmock" version))
        (sha256
         (base32
          "1zj1fcm0n6f0wr9mr0hmlqz9430fnr5cdwd5jkcvq9j44bnsrfz0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
     (list python-requests))
    (home-page "https://github.com/patrys/httmock")
    (synopsis "Mocking library for requests")
    (description "This package provides a library for replying fake data to
Python software under test, when they make an HTTP query.")
    (license license:asl2.0)))

(define-public python-icontract
  (package
    (name "python-icontract")
    (version "2.7.1")
    (source
     (origin
       ;; There are no tests in the PyPI tarball.
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
          (add-before 'check 'set-icontract-slow
            (lambda _
              ;; Setting ICONTRACT_SLOW, does not enable a slow test suite.
              ;; It only causes a single test to run, that checks the value of
              ;; icontract.SLOW is set correctly.
              (setenv "ICONTRACT_SLOW" "1"))))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-astor
           python-asyncstdlib
           python-mypy
           python-numpy
           python-setuptools
           python-typeguard
           python-wheel))
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
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "inline_snapshot" version))
       (sha256
        (base32 "09pqgz4phal2pjkv03wg3gvj7jr89rrb93rfw4hd2x9v8px4mqqv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Missing "freezer" fixture
      '(list "--ignore=tests/test_external.py"
             "--ignore=tests/test_pytest_plugin.py"
             "-k"
             (string-append
              "not test_trailing_comma"
              ;; Cannot use inline-snapshop when xdist is available.
              " and not test_xdist"
              " and not test_xdist_disabled"
              " and not test_xdist_and_disable"
              " and not test_typing"))))
    (propagated-inputs (list python-asttokens
                             python-black
                             python-click
                             python-executing
                             python-mkdocs
                             python-rich
                             python-tomli
                             python-typing-extensions))
    (native-inputs
     (list python-dirty-equals
           python-freezegun
           python-hatchling
           python-pydantic
           python-pytest
           python-pytest-mock
           python-pytest-subtests))
    (home-page "https://pypi.org/project/inline-snapshot/")
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
      (build-system python-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'check
                      (lambda _
                        (invoke "pytest" "-vv"))))))
      (native-inputs
       (list python-pytest))
      (propagated-inputs
       (list python-six))
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
     (list python-cython-3
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
        (base32
         "0fg8jflcf4c929gd4zbcrk73d08waaqjfjmdjrgnv54mzl35pjxl"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (native-inputs
     (list python-numpy python-pytest))
    (home-page "https://github.com/kaste/mockito-python")
    (synopsis "Mocking library for Python")
    (description "This package provides a Python implementation of the Java
library of the same name.  It eases monkey patching, for example to stub out
side effects when unit testing.")
    (license license:expat)))


(define-public python-mypy
  (package
    (name "python-mypy")
    (version "1.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mypy" version))
       (sha256
        (base32
         "0pl3plw815824z5gsncnjg3yn2v5wz0gqp20wdrncgmzdwdsd482"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; It tries to download hatchling and install aditional test
      ;; dependencies.
      #:test-flags #~(list "--ignore=mypy/test/testpep561.py")
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
           python-attrs
           python-lxml
           python-psutil
           python-pytest
           python-pytest-forked
           python-pytest-xdist
           python-setuptools
           python-virtualenv
           python-wheel))
    (propagated-inputs
     (list python-mypy-extensions
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
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mypy_extensions" version))
              (sha256
               (base32
                "10h7mwjjfbwxzq7jzaj1pnv9g6laa1k0ckgw72j44160bnazinvm"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;no tests
    (home-page "https://github.com/python/mypy_extensions")
    (synopsis "Experimental extensions for MyPy")
    (description
     "The @code{python-mypy-extensions} module defines
experimental extensions to the standard @code{typing} module that are
supported by the MyPy typechecker.")
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
      '(list
        ;; This test fails because of a mismatch in the output of LaTeX
        ;; equation environments.  Seems OK to skip.
        "--ignore=tests/test_nbdime_reporter.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'fix-test
            (lambda _
              ;; This test fails because of a mismatch in the output of LaTeX
              ;; equation environments.  Seems OK to skip.
              (delete-file
               "tests/ipynb-test-samples/test-latex-pass-correctouput.ipynb"))))))
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-setuptools
           python-sympy
           python-wheel))
    (propagated-inputs
     (list python-coverage
           python-ipykernel
           python-jupyter-client
           python-nbformat
           python-six))
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
    (version "2024.10.09")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wntrblm/nox")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gvv6hcwmmmg1sgwar42061ahx5p773d5fzx3c7sq81wh3gp7lqr"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-argcomplete
           python-colorlog
           python-packaging
           python-py
           python-virtualenv))
    (native-inputs
     (list python-hatchling
           python-jinja2
           python-pytest
           python-tox))
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
         "--ignore=tests/test_wheel.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-source-date-epoch
            (lambda _
              ;; Otherwise the wheel building test would fail with "ZIP does
              ;; not support timestamps before 1980".
              (setenv "SOURCE_DATE_EPOCH" "315532800"))))))
    (native-inputs
     (list python-beartype
           python-feedparser
           python-mypy
           python-pandas
           python-pytest
           python-setuptools
           python-typeguard
           python-wheel))
    (propagated-inputs
     (list python-numpy
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

(define-public python-pyannotate
  (package
    (name "python-pyannotate")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyannotate" version))
       (sha256
        (base32
         "16bm0mf7wxvy0lgmcs1p8n1ji8pnvj1jvj8zk3am70dkp825iv84"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-mypy-extensions python-six))
    (home-page
     "https://github.com/dropbox/pyannotate")
    (synopsis "Auto-generate PEP-484 annotations")
    (description "This package, PyAnnotate, is used to auto-generate PEP-484
annotations.")
    (license license:asl2.0)))

(define-public python-pycotap
  (package
    (name "python-pycotap")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycotap" version))
       (sha256
        (base32 "1v69fxial9i5wlap6wc4igq3hydvxbak7dlgb7cikk8wjgafqf7r"))))
    (build-system python-build-system)
    (home-page "https://el-tramo.be/pycotap")
    (synopsis "Tiny Python TAP test runner")
    (description "This package provides a simple Python test runner for
unittest that outputs Test Anything Protocol (TAP) results to standard
output.  Contrary to other TAP runners for Python, pycotap...
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

(define-public python-pyinstrument
  (package
    (name "python-pyinstrument")
    (version "4.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyinstrument" version))
       (sha256
        (base32 "1xnp1pjhcj1xl4dq20yzzj9599cmiyxb2azblsyjnl6qgr8yw0h0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-append
                    ;; Disable some failing tests.
                    "not test_script_execution_details"
                    " and not test_path_execution_details"
                    " and not test_module_execution_details"
                    " and not test_program_passed_as_string_execution_details"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-flaky
           python-greenlet
           python-pytest
           python-pytest-asyncio
           python-pytest-trio
           python-setuptools
           python-wheel))
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

(define-public python-pytest-aiohttp
  (package
    (name "python-pytest-aiohttp")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-aiohttp" version))
       (sha256
        (base32
         "0kx4mbs9bflycd8x9af0idcjhdgnzri3nw1qb0vpfyb3751qaaf9"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-aiohttp))
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
    (build-system python-build-system)
    (arguments (list #:tests? #f)) ; there are no tests
    (native-inputs
     (list python-attrs python-pytest-mock python-setuptools-scm))
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
  (build-system python-build-system)
  (native-inputs
   (list python-pytest python-setuptools-scm))
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
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-benchmark" version))
       (sha256
        (base32
         "1la802m5r49y1zqilmhqh0qvbnz139lw0qb3jmm9lngy7sw8a1zv"))))
    (build-system python-build-system)
    (arguments
     '(#:test-target "check"))
    (propagated-inputs
     (list python-py-cpuinfo))
    (native-inputs
     (list python-pytest))
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
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-black
           python-pytest
           python-toml))
    ;; Project maintenance has been changed, see
    ;; <https://github.com/shopkeep/pytest-black/issues/70>.
    (home-page "https://github.com/coherent-oss/pytest-black")
    (synopsis "Pytest plugin to enable format checking with black")
    (description
     "This package provides a pytest plugin to enable format checking with the
Python code formatter \"black\".")
    (license license:expat)))

(define-public python-pytest-celery
  (package
    (name "python-pytest-celery")
    (version "0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-celery" version))
       (sha256
        (base32 "01pli108qqiiyrn8qsqqabcpazrzj27r7cji9wgglsk76by61l6g"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests and circular dependency on python-celery
       #:phases
       (modify-phases %standard-phases
         (delete 'sanity-check)))) ; checks for celery
    (home-page "https://github.com/graingert/pytest-celery")
    (synopsis "Shim pytest plugin to enable @code{celery.contrib.pytest}")
    (description
     "This package provides a shim Pytest plugin to enable a Celery marker.")
    (license license:bsd-3)))

(define-public python-pytest-check
  (package
    (name "python-pytest-check")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_check" version))
       (sha256
        (base32 "0l7n2jhadbkmqr8kzja8zwclhjvhc87qsgr5v867zgsry37fy92j"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-flit-core))
    (propagated-inputs (list python-pytest))
    (home-page "https://github.com/okken/pytest-check")
    (synopsis "Pytest plugin to allow multiple failures")
    (description "This package provides a pytest plugin that allows multiple
failures per test.")
    (license license:expat)))

(define-public python-pytest-checkdocs
  (package
    (name "python-pytest-checkdocs")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-checkdocs" version))
       (sha256
        (base32 "1bn1wr3yz8avkwacffyh26za7mg20f9pajpakfk4cn7yvmgbhcrb"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))      ;no tests in pypi archive
    (propagated-inputs
     (list python-docutils
           python-importlib-metadata
           python-pep517
           python-pytest))
    (native-inputs (list python-setuptools-scm))
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
       (method url-fetch)
       (uri
        (pypi-uri "pytest_click" version))
       (sha256
        (base32 "1rcv4m850rl7djzdgzz2zhjd8g5ih8w6l0sj2f9hsynymlsq82xl"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-click python-pytest))
    (home-page "https://github.com/Stranger6667/pytest-click")
    (synopsis "Py.test plugin for Click")
    (description "This package provides a plugin to test Python click
interfaces with pytest.")
    (license license:expat)))

(define-public python-pytest-console-scripts
  (package
    (name "python-pytest-console-scripts")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-console-scripts" version))
       (sha256
        (base32
         "1qsw3i2h3psyi5avwf14panx8wxqfik2z7294dy37w8ha415iwn7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "--verbose"
                       ;; This one test fails because of PATH assumptions
                       "-k" "not test_elsewhere_in_the_path")))))))
    (propagated-inputs
     (list python-mock python-pytest))
    (native-inputs
     (list python-setuptools-scm))
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
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hackebrot/pytest-cookies")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x7ny6mx1siy9law1cv1i63nvv9ds2g1dlagm40l8qymxry43mjn"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "pytest" "-vv")))))))
    (native-inputs (list python-pytest))
    (propagated-inputs (list python-cookiecutter))
    (home-page "https://github.com/hackebrot/pytest-cookies")
    (synopsis "Pytest plugin for Cookiecutter templates")
    (description
     "This Pytest plugin adds a @code{cookies} fixture, which is a
wrapper for the Cookiecutter API.  This fixture helps you verify that
your template is working as expected and takes care of cleaning up after
running the tests.")
    (license license:expat)))

(define-public python-pytest-cram
  (package
    (name "python-pytest-cram")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-cram" version))
              (sha256
               (base32
                "0405ymmrsv6ii2qhq35nxfjkb402sdb6d13xnk53jql3ybgmiqq0"))))
    (build-system python-build-system)
    (propagated-inputs (list python-cram python-pytest))
    (home-page "https://github.com/tbekolay/pytest-cram")
    (synopsis "Run cram tests with pytest")
    (description "Cram tests command line applications; Pytest tests Python
applications.  @code{pytest-cram} tests Python command line applications by
letting you write your Python API tests with pytest, and your command line
tests in cram.")
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
     (list python-pytest-flake8
           python-pytest-xdist
           python-setuptools
           python-tabulate
           python-wheel))
    (propagated-inputs
     (list python-pytest python-six))
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
      #:test-flags
      #~(list "tests"
              ;; FIXME: Failed: nomatch: '*sqr*PASSED*
              "-k" (string-append
                    "not test_wrap_cpp_ext_module[importlib]"
                    " and not test_wrap_c_ext_module[importlib]"
                    " and not test_cython_ext_module[importlib]"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              (with-directory-excursion "tests/example-project"
                (invoke "python" "setup.py" "build_ext" "--inplace")))))))
    (native-inputs (list python-cython-3 python-setuptools python-wheel))
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

(define-public python-pytest-doctest-custom
  (package
    (name "python-pytest-doctest-custom")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-doctest-custom" version))
       (sha256
        (base32 "0kxkdd6q9c3h31kc88lbyfll4c45b0zjd24cbr4c083fcvcy7lip"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "test_pytest_doctest_custom.py")))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/danilobellini/pytest-doctest-custom")
    (synopsis
     "Pytest plugin to customize string representations of doctest results")
    (description "This package provides a Pytest plugin for customizing string
representations of doctest results.  It can change the display hook used by
doctest to render the object representations.")
    (license license:expat)))

(define-public python-pytest-doctestplus
  (package
    (name "python-pytest-doctestplus")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-doctestplus" version))
       (sha256
        (base32 "0ybn613rp0wqzm97hncwnpn8wx7bz91rajgnclplv8yfr2iahwi4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
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
            (lambda _
              (substitute* "pytest_doctestplus/plugin.py"
                (("\"git\"")
                 (format #f "'~a/bin/git'"
                         #$(this-package-native-input "git-minimal")))))))))
    (native-inputs
     (list git-minimal/pinned
           python-numpy
           python-pytest
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-setuptools)) ;for pkg_resources
    (home-page "https://github.com/astropy/pytest-doctestplus")
    (synopsis "Pytest plugin with advanced doctest features")
    (description
     "This package contains a plugin for the Pytest framework that provides
advanced doctest support and enables the testing of reStructuredText files.")
    (license license:bsd-3)))

(define-public python-pytest-env
  (package
    (name "python-pytest-env")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-env" version))
       (sha256
        (base32 "1hl0ln0cicdid4qjk7mv90lw9xkb0v71dlj7q7rn89vzxxm9b53y"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/MobileDynasty/pytest-env")
    (synopsis "Pytest plugin that allows you to add environment variables")
    (description
     "This is a @code{py.test} plugin that enables you to set environment
variables in the @file{pytest.ini} file.")
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
  (package
    (name "python-pytest-fixture-config")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-fixture-config" version))
       (sha256
        (base32
         "13i1qpz22w3x4dmw8vih5jdnbqfqvl7jiqs0dg764s0zf8bp98a1"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-setuptools-git))
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
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-flake8))
    (home-page "https://github.com/tholo/pytest-flake8")
    (synopsis "Pytest plugin to check FLAKE8 requirements")
    (description
     "This package provides a pytest plugin for efficiently checking PEP8
compliance.")
    (license license:bsd-3)))

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

(define-public python-pytest-helpers-namespace
  (package
    (name "python-pytest-helpers-namespace")
    (version "2021.3.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-helpers-namespace" version))
       (sha256
        (base32
         "0pyj2d45zagmzlajzqdnkw5yz8k49pkihbydsqkzm413qnkzb38q"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Make the installed plugin discoverable by Pytest.
              (add-installed-pythonpath inputs outputs)
              (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-pytest python-setuptools ; needs setuptools >= 50.3.2
           python-setuptools-scm python-setuptools-declarative-requirements))
    (home-page "https://github.com/saltstack/pytest-helpers-namespace")
    (synopsis "Pytest Helpers Namespace Plugin")
    (description "Pytest Helpers Namespace Plugin provides a helpers pytest
namespace which can be used to register helper functions without requiring
someone to import them in their actual tests to use them.")
    (license license:asl2.0)))

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
     (list python-pytest
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
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_isort" version))
       (sha256
        (base32 "0v0qa5l22y3v0nfkpvghbinzyj2rh4f54k871lrp992lbvf02y06"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; No tests in PyPi tarball.
    (propagated-inputs
     (list python-isort python-pytest))
    (home-page "https://github.com/moccu/pytest-isort/")
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
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/kaste/pytest-mockito")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hnpazaw3mglx1c405z2hkavgan99rqb3wgrcqk8x5kmhpay53xx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "-vv")))))))
    (propagated-inputs
     (list python-mockito python-pytest))
    (home-page "https://github.com/kaste/pytest-mockito")
    (synopsis "Mockito base fixtures for Pytest")
    (description "The @code{pytest-mockito} plugin provides base Mockito
fixtures for Pytest.  It covers the main entry points of the Mockito mocking
framework and makes it easy to undo any monkey patching.  The fixtures are:
@itemize
@item when
@item when2
@item expect
@item patch
@item unstub
@item spy2
@end itemize")
    (license license:expat)))

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
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-parawtf" version))
              (sha256
               (base32
                "08s86hy58lvrd90cnayzydvac4slaflj0ph9yknakcc42anrm023"))))
    (build-system python-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 ;; https://github.com/flub/pytest-parawtf/issues/1
                 (invoke "pytest" "-k" "not test_mark")))))))
    (propagated-inputs (list python-pytest))
    (home-page "https://github.com/flub/pytest-parawtf/")
    (synopsis "Finally spell paramete?ri[sz]e correctly")
    (description
"@code{python-pytest} uses one of four different spellings of
parametrize.  This plugin allows you to use all four.")
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
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/henry0312/pytest-pydocstyle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w6fivz4nb4b70wzmi5sk17qs9pd05rnh03fmch6v00r3dmfpk39"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; test requires the package itself
    (propagated-inputs
     (list python-pydocstyle
           python-pytest))              ;apparently required
    (home-page "https://github.com/henry0312/pytest-pydocstyle")
    (synopsis "Pytest plugin to run @command{pydocstyle}")
    (description "This package provides a Pytest plugin to run
@command{pydocstyle}.")
    (license license:expat)))

(define-public python-pytest-qt
  (package
    (name "python-pytest-qt")
    (version "4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-qt" version))
       (sha256
        (base32 "0i38qp2rqb44grbk9rn7zr5ffjvdlcl6k380759ji920m51632bn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;;#:test-target "pytest"
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'set-qpa
           (lambda _ (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (propagated-inputs
     (list python-pluggy python-pyqt python-pytest))
    (native-inputs
     (list python-pre-commit
           python-setuptools
           python-setuptools-scm
           python-tox
           python-wheel))
    (home-page "https://github.com/pytest-dev/pytest-qt")
    (synopsis "Pytest support for PyQt and PySide applications")
    (description
     "@code{pytest-qt} is a Pytest plugin that allows programmers to write
tests for PyQt5 and PySide2 applications.

The main usage is to use the @code{qtbot} fixture, responsible for handling
@code{qApp} creation as needed and provides methods to simulate user
interaction, like key presses and mouse clicks.")
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
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-repeat" version))
       (sha256
        (base32 "0nxdbghjz6v4xidl5ky9wlx6z4has3vygj5r7va5ccdb8nbjilsw"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (propagated-inputs
     (list python-pytest))
    (native-inputs
     (list python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-repeat")
    (synopsis "Pytest plugin for repeating tests")
    (description "@code{pytest-repeat} is a plugin for Pytest that makes it
enables repeating a single test, or multiple tests, a specific number of
times.")
    (license license:mpl2.0)))

(define-public python-pytest-rerunfailures
  (package
    (name "python-pytest-rerunfailures")
    (version "10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-rerunfailures" version))
       (sha256
        (base32 "15v68kggjvkflbqr0vz8gp5yp3pcsk0rz05bpg2l4xp0a6nin7ly"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pytest python-setuptools))
    (home-page "https://github.com/pytest-dev/pytest-rerunfailures")
    (synopsis "Pytest plugin to re-run flaky tests")
    (description "This package provides a pytest plugin to re-run tests to
eliminate flaky failures.")
    (license license:mpl2.0)))

(define-public python-pytest-rerunfailures-13
  (package
    (inherit python-pytest-rerunfailures)
    (version "13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-rerunfailures" version))
       (sha256
        (base32 "16cin0chv59w4rvnd6r0fisp0s8avmp07rwn9da6yixw43jdncp1"))))))

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
        (method url-fetch)
        (uri (pypi-uri "pytest-services" version))
        (sha256
         (base32
          "0b2zfv04w6m3gp2v44ifdhx22vcji069qnn95ry3zcyxib7cjnq3"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests not included in release tarball.
    (propagated-inputs
     (list python-psutil python-requests))
    (native-inputs
     (list python-pytest))
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
    (native-inputs
     (list python-anyio
           python-docutils
           python-pygments
           python-pytest
           python-pytest-asyncio
           python-pytest-rerunfailures
           python-setuptools
           python-wheel))
    (home-page "https://github.com/aklajnert/pytest-subprocess")
    (synopsis "Fake subprocess for Pytest")
    (description
     "This package provides a plugin to fake subprocess for Pytest.")
    (license license:expat)))

(define-public python-pytest-subtests
  (package
    (name "python-pytest-subtests")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-subtests" version))
       (sha256
        (base32 "05zvnxx0hdrd9w4z51qhchg3nkz5s47agryw68g8q7krq5kim5nr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest")))))))
    (native-inputs (list python-pytest python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-subtests")
    (synopsis "Unittest subTest() support and subtests fixture")
    (description "This Pytest plugin provides unittest @code{subTest()}
support and @code{subtests} fixture.")
    (license license:expat)))

(define-public python-pytest-testmon
  (package
    (name "python-pytest-testmon")
    (version "2.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tarpas/pytest-testmon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01qhbkb3n8c5c4id94w6b06q9wb7b6a33mqwyrkdfzk5pzv1gcyd"))))
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
       (method url-fetch)
       (uri (pypi-uri "pytest-tornasync" version))
       (sha256
        (base32
         "0pdyddbzppkfqwa7g17sdfl4w2v1hgsky78l8f4c1rx2a7cvd0fp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false ; TODO: fails at "from test import MESSAGE, PAUSE_TIME"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "--verbose")))))))
    (propagated-inputs
     (list python-pytest python-tornado-6))
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
           (base32
            "1yk988zi0la6zpcm3fff0mxf942di2jiymrfqas19nyngj5ygaqs"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "tests/"))))))
      (native-inputs
       (list python-urllib3))
      (propagated-inputs
       (list python-pytest python-vcrpy))
      (home-page "https://github.com/ktosiek/pytest-vcr")
      (synopsis "Plugin for managing VCR.py cassettes")
      (description
       "Plugin for managing VCR.py cassettes.")
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-xvfb" version))
       (sha256
        (base32 "1kyq5rg27dsnj7dc6x9y7r8vwf8rc88y2ppnnw6r96alw0nn9fn4"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "pytest"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-tests
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0")

             ;; This test is meant to run on Windows.
             (delete-file "tests/test_xvfb_windows.py")
             #t)))))
    (native-inputs
     (list python-pytest python-pytest-runner xorg-server-for-tests))
    (propagated-inputs
     (list python-pyvirtualdisplay))
    (home-page "https://github.com/The-Compiler/pytest-xvfb")
    (synopsis "Pytest plugin to run Xvfb for tests")
    (description
     "This package provides a Pytest plugin to run Xvfb for tests.")
    (license license:expat)))

(define-public python-pyux
  (package
    (name "python-pyux")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyux" version))
       (sha256
        (base32
         "1i17xh4dy238ibrjdgh8vn78fk5q6dj37mcznpvdfzidj57js7ca"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                  ;the mini test suite fails
    (home-page "https://github.com/farizrahman4u/pyux")
    (synopsis "Utility to check API integrity in Python libraries")
    (description "The pyux utility detects API changes in Python
libraries.")
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
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-covdefaults python-coverage python-pytest))
    (propagated-inputs
     (list python-regex))
    (home-page "https://github.com/asottile/re-assert")
    (synopsis "Show where your regex match assertion failed")
    (description
     "@code{re-assert} provides a helper class to make assertions of regexes
simpler.")
    (license license:expat)))

(define-public python-robber
  (package
    (name "python-robber")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "robber" version))
              (sha256
               (base32
                "0xp5csgv2g9q38hscml6bc5i1nm4xy5lzqqiimm2drxsf0hw2nq5"))))
    (build-system python-build-system)
    ;; There are no tests in the tarball downloaded from PyPI.
    ;; The last version tagged in Github (0.1.0) is older than the one on PyPI.
    ;; Reported upstream: <https://github.com/vesln/robber.py/issues/20>.
    (arguments '(#:tests? #f))
    (propagated-inputs
     (list python-mock python-termcolor))
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
    ;; XXX: The latest version needs flit-core=>3.12.
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stestr" version))
       (sha256
        (base32 "12p96kzanzzssr6z4hq6k62pdbsql4mf369ms69c4qyfxrlw6qaz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Two tets fail.
      #~(list "--exclude-regex" (string-join
                                 (list "test_initialise_expands_user_directory"
                                       "test_open_expands_user_directory")
                                 "|"))
      #:phases
      #~(modify-phases %standard-phases
          ;; TODO: Implement in pypproject-build-system's  test-backends.
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (let ((stestr (string-append #$output "/bin/stestr")))
                  (apply invoke stestr "run" test-flags))))))))
    (native-inputs
     (list python-ddt
           python-iso8601
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-cliff
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
    (native-inputs (list python-mypy
                         python-myst-parser
                         python-pytest
                         python-pytest-cov
                         python-pyyaml
                         python-seedir
                         python-setuptools
                         python-testfixtures
                         python-wheel))
    (home-page "https://github.com/simplistix/sybil")
    (synopsis "Automated testing for examples in code and documentation")
    (description
      "This library provides a way to check examples in your code and
documentation by parsing them from their source and evaluating the
parsed examples as part of your normal test run.  Integration is
provided for the main Python test runners.")
    (license license:expat)))

(define-public python-tappy
  (package
    (name "python-tappy")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tap.py" version))
       (sha256
        (base32
         "0w4w6pqjkv54j7rv6vdrpfxa72c5516bnlhpcqr3vrb4zpmyxvpm"))))
    (build-system python-build-system)
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
    (build-system python-build-system)
    (home-page "https://github.com/Kami/python-test-utils/")
    (synopsis "Utilities for functional and integration tests")
    (description
     "This package provides a collection of utility functions and classes
which make writing and running functional and integration tests easier.")
    (license license:asl2.0)))

(define-public python-testfixtures
  (package
    (name "python-testfixtures")
    (version "6.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testfixtures" version))
       (sha256
        (base32 "1nlv2hz20czjp4a811ichl5kwg99rh84l0mw9wq4rk3idzfs1hsy"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))          ; PyTest-Django fails to build in master
    (native-inputs
     (list python-pytest python-pytest-cov
           ;;("python-pytest-django" ,python-pytest-django)
           python-twine python-wheel))
    (synopsis "Tests components for Python")
    (description "Testfixtures is a collection of helpers and mock objects that
are useful when writing automated tests in Python.")
    (home-page "https://testfixtures.readthedocs.io/en/latest/")
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
    (propagated-inputs (list python-dateutil))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/adamchainz/time-machine")
    (synopsis "Travel through time in your tests.")
    (description "This package lets you set a different time for your tests.")
    (license license:expat)))

(define-public python-tox
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
      '(list "-k"
             (string-join
              (map (lambda (test)
                     (string-append "not test_" test))
                   '( ;; These freeze the test suite
                     "parallel"
                     "parallel_live"
                     ;; Needs internet access
                     "build_wheel_external"
                     "run_installpkg_targz"
                     "python_generate_hash_seed"
                     ;; XXX Tries to call python-wrapper-3.10.7/bin/tox
                     "call_as_exe"))
              " and "))))
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

(define-public python-xunitparser
  (package
    (name "python-xunitparser")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xunitparser" version))
       (sha256
        (base32 "00lapxi770mg7jkw16zy3a91hbdfz4a9h43ryczdsgd3z4cl6vyf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; See https://github.com/laurentb/xunitparser/pull/11
         (add-after 'unpack 'fix-test-suite
           (lambda _
             (substitute* "xunitparser.py"
               (("(^ +)self.stderr = None" m indent)
                (string-append m "\n" indent "self._cleanup = False\n"))))))))
    (home-page "http://git.p.engu.in/laurentb/xunitparser/")
    (synopsis "Read JUnit/XUnit XML files and map them to Python objects")
    (description "xunitparser reads a JUnit/XUnit XML file and maps it to
Python objects.  It tries to use the objects available in the standard
@code{unittest} module.")
    (license license:expat)))

(define-public python-xvfbwrapper
  (package
    (name "python-xvfbwrapper")
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "xvfbwrapper" version))
              (sha256
               (base32
                "097wxhvp01ikqpg1z3v8rqhss6f1vwr399zpz9a05d2135bsxx5w"))))
    (build-system python-build-system)
    (propagated-inputs (list xorg-server-for-tests))
    (home-page "https://github.com/cgoldberg/xvfbwrapper")
    (synopsis "Python module for controlling virtual displays with Xvfb")
    (description
     "Xvfb (X virtual framebuffer) is a display server implementing
the X11 display server protocol.  It runs in memory and does not require a
physical display.  Only a network layer is necessary.  Xvfb is useful for
running acceptance tests on headless servers.")
    (license license:expat)))
