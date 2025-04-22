;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015, 2016, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2019, 2021 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages openstack)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:select (asl2.0))
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public python-bandit
  (package
    (name "python-bandit")
    (version "1.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bandit" version))
       (sha256
        (base32
         "1lkvf5ffdqa9504mm5fd9vvq0q5wdyqbkm32i4rswys1fg2shqrd"))))
    (build-system python-build-system)
    (arguments
     ;; The tests are disabled to avoid a circular dependency with
     ;; python-stestr.
     `(#:tests? #f))
    (propagated-inputs
     (list python-gitpython python-pyyaml python-six python-stevedore))
    (native-inputs
     (list python-pbr))
    (home-page "https://github.com/PyCQA/bandit")
    (synopsis "Security oriented static analyser for python code")
    (description "Bandit is a tool designed to find common security issues in
Python code.  To do this Bandit processes each file, builds an AST from it,
and runs appropriate plugins against the AST nodes.  Once Bandit has finished
scanning all the files it generates a report.")
    (license asl2.0)))

(define-public python-cliff
  (package
    (name "python-cliff")
    (version "3.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cliff" version))
       (sha256
        (base32
         "180059m5ky3hlw2m9fszh4h2ykja8zl7ql5dsxjijiv47hzywnh4"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     (list python-pbr))
    (propagated-inputs
     (list python-autopage
           python-cmd2
           python-prettytable
           python-pyparsing
           python-pyyaml
           python-stevedore))
    (home-page "https://opendev.org/openstack/cliff")
    (synopsis "Framework for building command line programs")
    (description "The @code{cliff} framework allows creating multi-level
commands such as those of @command{subversion} and @command{git}, where the
main program handles some basic argument parsing and then invokes a
sub-command to do the work.  It uses plugins to define sub-commands, output
formatters, and other extensions.")
    (license asl2.0)))

(define-public python-debtcollector
  (package
    (name "python-debtcollector")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "debtcollector" version))
        (sha256
         (base32 "0vzarkvjclci98d8lvkix6qj59f7rxp1qg2x6q6is7qfbg91g29a"))
        (modules '((guix build utils)))
        (snippet #~(begin
                     (substitute* "test-requirements.txt"
                       (("^(coverage|hacking|pre-commit).*")
                        ""))))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-pbr python-wrapt))
    (native-inputs
     (list python-doc8
           python-fixtures
           python-openstackdocstheme
           python-reno
           python-setuptools
           python-stestr
           python-testtools
           python-wheel))
    (home-page "https://www.openstack.org/")
    (synopsis
     "Find deprecated patterns and strategies in Python code")
    (description
      "This package provides a collection of Python deprecation patterns and
strategies that help you collect your technical debt in a non-destructive
manner.")
    (license asl2.0)))

(define-public python-hacking
  (package
    (name "python-hacking")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hacking" version))
       (sha256
        (base32
         "1dya2mbnm8cbkwrzmhsacdpmp37nhfcy13y5hbx58z8g53jw5dmr"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-flake8))
    (native-inputs
     (list python-coverage
           python-ddt
           python-eventlet
           python-fixtures
           python-setuptools
           python-stestr
           python-subunit
           python-testscenarios
           python-testtools
           python-wheel))
    (home-page "https://github.com/openstack-dev/hacking")
    (synopsis "OpenStack hacking guideline enforcement")
    (description
     "Python-hacking is a set of flake8 plugins that test and enforce the
@uref{http://docs.openstack.org/developer/hacking/, OpenStack style
guidelines}.")
    (license asl2.0)))

(define-public python-mox3
  (package
    (name "python-mox3")
    (version "0.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mox3" version))
        (patches (search-patches "python-mox3-python3.6-compat.patch"))
        (sha256
          (base32
           "0w58adwv7q9wzvmq9mlrk2asfk73myq9fpwy7mjkzsz3baa95zf5"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-fixtures python-pbr))
    (native-inputs
      (list python-openstackdocstheme python-sphinx python-subunit
            python-testrepository python-testtools))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-for-python-3.11'
                 (lambda _
                   ;; The getargspec function has been removed in python 3.11.
                   (substitute* "mox3/mox.py"
                     (("self\\._args, varargs, varkw, defaults = inspect\\.getargspec\\(method\\)")
                      "inspect_result = inspect.getfullargspec(method)
            self._args = inspect_result.args
            varargs = inspect_result.varargs
            varkw = inspect_result.varkw
            defaults = inspect_result.defaults")))))))
    (home-page "https://www.openstack.org/")
    (synopsis "Mock object framework for Python")
    (description
      "Mox3 is an unofficial port of the @uref{https://code.google.com/p/pymox/,
Google mox framework} to Python 3.  It was meant to be as compatible
with mox as possible, but small enhancements have been made.")
    (license asl2.0)))

(define-public python-openstackdocstheme
  (package
    (name "python-openstackdocstheme")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openstackdocstheme" version))
       (sha256
        (base32 "0f8vk9556cx3h2z2dwqqkylv3rijd1p15qjy4xjv9sxxcfngdx1q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ; no tests in PyPI archive or git checkout
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-dulwich
           python-pbr
           python-sphinx))
    (home-page "https://docs.openstack.org/openstackdocstheme/latest/")
    (synopsis "OpenStack Docs Theme")
    (description
     "This package provides themes and extensions for Sphinx for publishing
to docs.openstack.org and developer.openstack.org.")
    (license asl2.0)))

(define-public python-os-client-config
  (package
    (name "python-os-client-config")
    (version "1.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "os-client-config" version))
        (sha256
          (base32
           "1vjn7667pswnmpqv6ngwyqm2xn46w90hi5b4pv2grwfz751cn1lf"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; Circular dependency with python-oslotest
    (propagated-inputs
      (list python-appdirs python-pyyaml))
    (native-inputs
      (list python-pbr
            python-fixtures
            python-mimeparse
            python-testrepository
            python-testscenarios
            python-testtools))
    (home-page "https://www.openstack.org/")
    (synopsis
      "OpenStack Client Configuration Library")
    (description
      "The OpenStack Client Configuration Library is a library for collecting
  client configuration for using an OpenStack cloud in a consistent and
  comprehensive manner.")
    (license asl2.0)))

(define-public python-os-service-types
  (package
    (name "python-os-service-types")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "os-service-types" version))
              (sha256
               (base32
                "0v4chwr5jykkvkv4w7iaaic7gb06j6ziw7xrjlwkcf92m2ch501i"))))
    (build-system python-build-system)
    (arguments
     ;; The tests are disabled to avoid a circular dependency with
     ;; python-keystoneauth1.
     `(#:tests? #f))
    (native-inputs (list python-pbr))
    (home-page "https://docs.openstack.org/os-service-types/latest/")
    (synopsis "Library for consuming OpenStack Service Types Authority data")
    (description "The @emph{OpenStack Service Types Authority} contains
information about officiag OpenStack services and their historical
service-type aliases.  The data is in JSON and the latest data should always
be used.  This simple library exists to allow for easy consumption of the
data, along with a built-in version of the data to use in case network access
is for some reason not possible and local caching of the fetched data.")
    (license asl2.0)))

(define-public python-os-testr
  (package
    (name "python-os-testr")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "os-testr" version))
       (sha256
        (base32
         "10xaqg3wxly13652hdvh9c69y4s12ird0ircffya3kvpl5pky0pz"))))
    (build-system python-build-system)
    (arguments
     ;; os-testr uses itself to run the tests. It seems like pbr writes the
     ;; exectuable in the virtualenv when using tox. Not sure how to do this
     ;; when building the package. Skip the tests for now.
     `(#:tests? #f))
    (propagated-inputs
     (list python-stestr))
    (native-inputs
     (list python-babel python-pbr python-testrepository python-testtools))
    (home-page "https://www.openstack.org/")
    (synopsis "Testr wrapper to provide functionality for OpenStack projects")
    (description
      "Os-testr provides developers with a testr wrapper and an output filter
  for subunit.")
    (license asl2.0)))

(define-public python-stevedore
  (package
    (name "python-stevedore")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stevedore" version))
       (sha256
        (base32
         "1w11lm293afzb73iq0ba9wnmr2rjwymnhr92km4a4xrs7a5qcigq"))))
    (build-system python-build-system)
    (arguments
     ;; The tests are disabled to avoid a circular dependency with
     ;; python-stestr.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-pbr-3
           (lambda _
             (substitute* '("setup.py"
                            "requirements.txt")
               (("pbr!=2.1.0,>=2.0.0") "pbr>=3.0.0")))))))
    (propagated-inputs
     (list python-pbr))
    (home-page "https://github.com/dreamhost/stevedore")
    (synopsis "Manage dynamic plugins for Python applications")
    (description
      "Python makes loading code dynamically easy, allowing you to configure
and extend your application by discovering and loading extensions (\"plugins\")
at runtime.  Many applications implement their own library for doing this,
using __import__ or importlib.  Stevedore avoids creating yet another extension
mechanism by building on top of setuptools entry points.  The code for managing
entry points tends to be repetitive, though, so stevedore provides manager
classes for implementing common patterns for using dynamically loaded
extensions.")
    (license asl2.0)))

(define-public python-tempest
  (package
    (name "python-tempest")
    (version "31.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tempest" version))
              (sha256
               (base32
                "1bh250n0cf68jm68jd7pcrgf7zbsv74cq590ar1n002sijfcb80i"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "test-requirements.txt"
               ;; unused, code-quality checks only
               (("hacking[<>!=]" line) (string-append "# " line))
               (("flake8-.*[<>!=]" line) (string-append "# " line))
               (("pycodestyle[<>!=]" line) (string-append "# " line))
               (("coverage[<>!=]" line) (string-append "# " line)))))
         (add-before 'check 'setup-check
           (lambda _
             (substitute* "tempest/tests/lib/cli/test_execute.py"
               (("cli_base.execute\\(\"env\",")
                (string-append "cli_base.execute('" (which "env") "',")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "stestr" "--test-path" "./tempest/tests" "run")))))))
    (propagated-inputs (list python-cliff
                             python-cryptography
                             python-debtcollector
                             python-fixtures
                             python-jsonschema
                             python-netaddr
                             python-oslo-concurrency
                             python-oslo-config
                             python-oslo-log
                             python-oslo-serialization
                             python-oslo-utils
                             python-paramiko
                             python-prettytable
                             python-pyyaml
                             python-stevedore
                             python-subunit
                             python-testtools
                             python-urllib3))
    (native-inputs (list python-oslotest python-pbr python-stestr python-hacking))
    (home-page "https://docs.openstack.org/tempest/latest/")
    (synopsis "OpenStack Integration Testing")
    (description "This is a set of integration tests to be run against a live
OpenStack cluster.  Tempest has batteries of tests for OpenStack API
validation, scenarios, and other specific tests useful in validating an
OpenStack deployment.")
    (license asl2.0)))

(define-public python-tempest-lib
  (package
    (name "python-tempest-lib")
    (version "1.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "tempest-lib" version))
      (sha256
       (base32
        "1cpp2vwmawpd29hjsklsps181lq2ah91cl412qvpnz228nf9sqn5"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; FIXME: Requires oslo.log >= 1.14.0.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "requirements.txt"
               (("jsonschema[<>!=].*") "jsonschema\n"))))
         (add-before
          'check 'pre-check
          (lambda _
            (substitute* "tempest_lib/tests/cli/test_execute.py"
              (("/bin/ls") (which "ls"))))))))
    (propagated-inputs
      (list python-fixtures
            python-httplib2
            python-iso8601
            python-jsonschema
            python-oslo-log
            python-paramiko
            python-six))
    (native-inputs
      (list python-babel python-mock python-os-testr python-oslotest
            python-pbr))
    (home-page "https://www.openstack.org/")
    (synopsis "OpenStack functional testing library")
    (description
      "Tempest-lib is a functional testing library for OpenStack.  It provides
common features used in Tempest.")
    (license asl2.0)))


;;;
;;; Packages from the Oslo library
;;;

(define-public python-oslo-concurrency
  (package
    (name "python-oslo-concurrency")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "oslo.concurrency" version))
              (sha256
               (base32
                "0zl9wyxvs69i78wja5c3cacd6gadk8cc8ggy2ips0wlakxp98ilz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "test-requirements.txt"
               (("hacking[<>!=]" line) (string-append "# " line))
               (("coverage[<>!=]" line) (string-append "# " line))
               (("bandit[<>!=]" line) (string-append "# " line))
               (("pre-commit[<>!=]" line) (string-append "# " line)))))
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* "oslo_concurrency/tests/unit/test_processutils.py"
               (("#!/bin/bash") (string-append "#!" (which "bash")))
               (("#!/bin/sh") (string-append "#!" (which "sh")))
               (("'/usr/bin/env'") (string-append "'" (which "env") "'"))
               (("'/usr/bin/env ") (string-append "'" (which "env") " "))
               (("'/bin/true'") (string-append "'" (which "true") "'"))))))))
    (native-inputs (list python-pbr
                         ;; for tests:
                         python-oslotest
                         python-fixtures
                         python-stestr
                         python-eventlet))
    (propagated-inputs (list python-fasteners python-oslo-config
                             python-oslo-i18n python-oslo-utils))
    (home-page "https://docs.openstack.org/oslo.concurrency/latest/")
    (synopsis "Oslo Concurrency library")
    (description "The Oslo Concurrency Library provides utilities for safely
running multi-thread, multi-process applications using locking mechanisms and
for running external processes.")
    (license asl2.0)))

(define-public python-oslo-config
  (package
    (name "python-oslo-config")
    (version "8.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.config" version))
       (sha256
        (base32
         "0q3v4yicqls9zsfxkmh5mrgz9dailaz3ir25p458gj6dg3bldhx0"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;XXX circular dependency on oslo.log
    (propagated-inputs
     (list python-debtcollector
           python-netaddr
           python-oslo-i18n
           python-rfc3986
           python-requests
           python-stevedore
           python-pyyaml))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo Configuration API")
    (description
     "The Oslo configuration API supports parsing command line arguments and
.ini style configuration files.")
    (license asl2.0)))

(define-public python-oslo-context
  (package
    (name "python-oslo-context")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.context" version))
       (sha256
        (base32
         "091j2cjh1b60nx6s0a4amb2idh9awijnbmppc3an0738fv8cdh48"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'relax-requirements
                    (lambda _
                      (substitute* "test-requirements.txt"
                        (("hacking[<>!=].*") "hacking\n")
                        ;; unused, code-quality checks only
                        (("bandit[<>!=]" line) (string-append "# " line))
                        (("pre-commit[<>!=]" line) (string-append "# " line))))))))
    (propagated-inputs
     (list python-debtcollector))
    (native-inputs
     (list python-coverage
           python-fixtures
           python-hacking
           python-mypy
           python-oslotest
           python-pbr
           python-stestr))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo context library")
    (description
      "The Oslo context library has helpers to maintain useful information
about a request context.  The request context is usually populated in the WSGI
pipeline and used by various modules such as logging.")
    (license asl2.0)))

(define-public python-oslo-i18n
  (package
    (name "python-oslo-i18n")
    (version "3.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.i18n" version))
       (sha256
        (base32
         "0kjcdw4bk3mi4vqmqwhhq053kxbbbj05si6nwxd1pzx33z067ky3"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))                 ;avoid circular dependency on oslo.config
    (propagated-inputs
     (list python-babel python-six))
    (native-inputs
     (list python-pbr))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo internationalization (i18n) library")
    (description
     "The oslo.i18n library contain utilities for working with
internationalization (i18n) features, especially translation for text strings
in an application or library.")
    (license asl2.0)))

(define-public python-oslo-log
  (package
  (name "python-oslo-log")
  (version "5.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "oslo.log" version))
      (sha256
        (base32
          "00adkm465xcaxg15pncsmwxhicdj3kx4v1vcabghpmd2m0s75avk"))))
  (build-system python-build-system)
  (arguments
   '(#:phases (modify-phases %standard-phases
                (replace 'check
                  (lambda* (#:key tests? #:allow-other-keys)
                    (when tests? (invoke "stestr" "run")))))))
  (propagated-inputs
   (list python-dateutil
         python-debtcollector
         python-oslo-config
         python-oslo-context
         python-oslo-i18n
         python-oslo-utils
         python-oslo-serialization
         python-pyinotify))
  (native-inputs
   (list python-fixtures python-oslotest python-stestr python-testtools
         python-pbr))
  (home-page "https://launchpad.net/oslo")
  (synopsis "Python logging library of the Oslo project")
  (description
    "The oslo.log (logging) configuration library provides standardized
configuration for all OpenStack projects.  It also provides custom formatters,
handlers and support for context specific logging (like resource id’s etc).")
  (license asl2.0)))

(define-public python-oslo-serialization
  (package
    (name "python-oslo-serialization")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.serialization" version))
       (sha256
        (base32
         "10sdgvyb0d3lcmb8b4l5gs40bkfbai08kvsdwp658dxd2yqf21rh"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests? (invoke "stestr" "run")))))))
    (propagated-inputs
      (list python-msgpack python-oslo-utils python-pbr python-pytz))
    (native-inputs
     ;; For tests.
      (list python-netaddr python-oslo-i18n python-oslotest python-stestr))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo serialization library")
    (description
      "The oslo.serialization library provides support for representing objects
in transmittable and storable formats, such as JSON and MessagePack.")
    (license asl2.0)))

(define-public python-reno
  (package
    (name "python-reno")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "reno" version))
       (sha256
        (base32 "0w2kc9znm3ffcfsrwhvqkq6878jk3l9hibs7vv4mw88nppyz34pr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              ;; reno expects a git repo
              (invoke "git" "init"))))))
    (native-inputs
     (list git-minimal/pinned
           gnupg
           python-docutils
           python-openstackdocstheme
           python-pytest
           python-setuptools
           python-sphinx
           python-stestr
           python-subunit
           python-testscenarios
           python-testtools
           python-wheel))
    (propagated-inputs
     (list python-dulwich
           python-packaging
           python-pbr
           python-pyyaml))
    (home-page "https://docs.openstack.org/reno/latest/")
    (synopsis "Release notes manager")
    (description "Reno is a tool for storing release notes in a git repository
and building documentation from them.")
    (license asl2.0)))

(define-public python-oslosphinx
  (package
    (name "python-oslosphinx")
    (version "4.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslosphinx" version))
       (sha256
        (base32
         "1xm41857vzrzjmnyi6bqirg4i5qa61v7wxcsdc4q1nzgr3ndgz5k"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "test-requirements.txt"
               (("hacking!=0.13.0,<0.14,>=0.12.0")
                "hacking!=0.13.0,>=0.12.0"))
             #t)))))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-hacking python-openstackdocstheme python-pbr
           python-reno python-sphinx))
    (home-page "https://www.openstack.org/")
    (synopsis "OpenStack sphinx extensions and theme")
    (description "This package provides themes and extensions for Sphinx
documentation from the OpenStack project.")
    (license asl2.0)))

(define-public python-oslotest
  (package
    (name "python-oslotest")
    (version "4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslotest" version))
       (sha256
        (base32
         "0r50sz55m8ljv2vk1k7sp88iz1iqq4p9w6kb8hn8g8c50r9zdi5i"))))
    (build-system python-build-system)
    (arguments
     ;; The tests are disabled to avoid a circular dependency with oslo.config.
     `(#:tests? #f))
    (propagated-inputs
     (list python-fixtures python-six python-subunit python-testtools))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo test framework")
    (description "The Oslo Test framework provides common fixtures, support
for debugging, and better support for mocking results.")
    (license asl2.0)))

(define-public python-oslo-utils
  (package
    (name "python-oslo-utils")
    (version "4.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "oslo.utils" version))
        (sha256
          (base32
           "0kfgr6lr3r34nzmkvnyywr0x3lkwpwy35m1dj4rkk3ydqvi1xaip"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests? (invoke "stestr" "run")))))))
    (propagated-inputs
      (list python-debtcollector
            python-oslo-i18n
            python-iso8601
            python-netaddr
            python-netifaces
            python-pbr
            python-packaging
            python-pyparsing
            python-pytz))
    (native-inputs
     ;; For tests.
      (list python-ddt
            python-eventlet
            python-fixtures
            python-oslotest
            python-stestr
            python-testscenarios
            python-testtools))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo utility library")
    (description
      "The @code{oslo.utils} library provides support for common utility type
functions, such as encoding, exception handling, string manipulation, and time
handling.")
    (license asl2.0)))

(define-public python-keystoneauth1
  (package
    (name "python-keystoneauth1")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "keystoneauth1" version))
              (sha256
               (base32
                "08s36dqxrxqx37sdl28cr7fx2iwr8wfxaa53hwq2dzcx9h25zfvf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "test-requirements.txt"
               (("hacking[<>!=].*") "hacking\n")
               ;; unused, code-quality checks only
               (("flake8-.*[<>!=]" line) (string-append "# " line))
               (("pycodestyle[<>!=]" line) (string-append "# " line))
               (("bandit[<>!=]" line) (string-append "# " line))
               (("coverage[<>!=]" line) (string-append "# " line))
               (("reno[<>!=]" line) (string-append "# " line)))))
         (add-before 'check 'check-setup
           (lambda _
             ;; remove code-quality checks
             (delete-file "keystoneauth1/tests/unit/test_hacking_checks.py")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "stestr" "run")))))))
    (propagated-inputs (list python-iso8601
                             python-os-service-types
                             python-requests
                             python-six
                             python-stevedore))
    (native-inputs (list python-betamax
                         python-fixtures
                         python-hacking
                         python-lxml
                         python-oauthlib
                         python-oslo-config
                         python-oslo-utils
                         python-oslotest
                         python-pbr
                         python-pyyaml
                         python-requests-kerberos
                         python-requests-mock
                         python-stestr
                         python-testresources
                         python-testtools))
    (home-page "https://docs.openstack.org/keystoneauth/latest/")
    (synopsis "Authentication Library for OpenStack Identity")
    (description "Keystoneauth provides a standard way to do authentication
and service requests within the OpenStack ecosystem.  It is designed for use
in conjunction with the existing OpenStack clients and for simplifying the
process of writing new clients.")
    (license asl2.0)))

(define-public python-keystoneclient
  (package
    (name "python-keystoneclient")
    (version "5.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-keystoneclient" version))
        (sha256
         (base32
          "0gza5fx3xl3l6vrc6pnhbzhipz1fz9h98kwxqp7mmd90pwrxll0g"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f   ; FIXME: Many tests are failing.
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'relax-requirements
                    (lambda _
                      (substitute* "test-requirements.txt"
                        ;; unused, code-quality checks only
                        (("hacking[<>!=]" line) (string-append "# " line))
                        (("flake8-.*[<>!=]" line) (string-append "# " line))
                        (("pycodestyle[<>!=]" line) (string-append "# " line))
                        (("bandit[<>!=]" line) (string-append "# " line))
                        (("coverage[<>!=]" line) (string-append "# " line))
                        (("reno[<>!=]" line) (string-append "# " line))))))))
    (native-inputs
     (list openssl
           python-fixtures
           python-keyring
           python-lxml
           python-mock
           python-oauthlib
           python-oslotest
           python-pbr
           python-requests-mock
           python-stestr
           python-tempest-lib
           python-testresources
           python-testscenarios
           python-testtools))
    (propagated-inputs
     (list python-babel
           python-debtcollector
           python-iso8601
           python-keystoneauth1
           python-netaddr
           python-oslo-config
           python-oslo-i18n
           python-oslo-serialization
           python-oslo-utils
           python-prettytable
           python-requests
           python-six
           python-stevedore))
    (home-page "https://www.openstack.org/")
    (synopsis "Client Library for OpenStack Identity")
    (description
     "Python-keystoneclient is the identity service used by OpenStack for
authentication (authN) and high-level authorization (authZ).  It currently
supports token-based authN with user/service authZ, and is scalable to support
OAuth, SAML, and OpenID in future versions.  Out of the box, Keystone uses
SQLite for its identity store database, with the option to connect to external
LDAP.")
    (license asl2.0)))

(define-public python-swiftclient
  (package
    (name "python-swiftclient")
    (version "4.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-swiftclient" version))
        (sha256
         (base32
          "1zwb4zcln454fzcnbwqhyzxb68wrsr1i2vvvrn5c7yy5k4vcfs1v"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'relax-requirements
                    (lambda _
                      (delete-file "test-requirements.txt")))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "stestr" "run")))))))
    (native-inputs
     (list python-keystoneclient
           python-keystoneauth1
           python-openstacksdk
           python-stestr))
    (propagated-inputs
     (list python-requests))
    (home-page "https://www.openstack.org/")
    (synopsis "OpenStack Object Storage API Client Library")
    (description
     "OpenStack Object Storage (code-named Swift) creates redundant, scalable
object storage using clusters of standardized servers to store petabytes of
accessible data.  It is not a file system or real-time data storage system, but
rather a long-term storage system for a more permanent type of static data that
can be retrieved, leveraged, and then updated if necessary.  Primary examples of
data that best fit this type of storage model are virtual machine images, photo
storage, email storage and backup archiving.  Having no central \"brain\" or
master point of control provides greater scalability, redundancy and
permanence.")
    (license asl2.0)))

(define-public python-git-review
  (package
    (name "python-git-review")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "git-review" version))
       (sha256
        (base32 "1mhywsbisyv028lsj2ksg4g5l8kyimpwxgwzqi08rymi8mb7fv1s"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                     ; tests require a running Gerrit server
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (git (assoc-ref inputs "git"))
                    (openssh (assoc-ref inputs "openssh")))
               (wrap-program (string-append out "/bin/git-review")
                 `("PATH" ":" prefix
                   ,(map (lambda (dir)
                           (string-append dir "/bin"))
                         (list git openssh))))))))))
    (native-inputs (list python-pbr))
    (propagated-inputs (list python-requests))
    (inputs (list bash-minimal git openssh))
    (home-page "https://docs.openstack.org/infra/git-review/")
    (synopsis "Command-line tool for Gerrit")
    (description
     "Git-review is a command-line tool that helps submitting Git branches to
Gerrit for review, or fetching existing ones.")
    (license asl2.0)))

(define-public python-requestsexceptions
  (package
    (name "python-requestsexceptions")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "requestsexceptions" version))
              (sha256
               (base32
                "0r9hp9yzgj8r81q5gc6r8sgxldqc09xi6ax0b7a6dw0qfv3wp5dh"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))  ; no tests
    (native-inputs (list python-pbr))
    (home-page "https://www.openstack.org/")
    (synopsis "Import exceptions from potentially bundled packages in requests")
    (description "The Python requests library bundles the urllib3 library,
however, some software distributions modify requests to remove the bundled
library.  This makes some operations difficult, such as suppressing the
“insecure platform warning” messages that urllib emits.  This package is a
simple library to find the correct path to exceptions in the requests library
regardless of whether they are bundled or not.")
    (license asl2.0)))

(define-public python-openstacksdk
  (package
    (name "python-openstacksdk")
    (version "0.100.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "openstacksdk" version))
              (sha256
               (base32
                "0iq7rxw59ibl6xsqh3jw56yg3zfbz3cqgx1239n6xd9iv86mcgq1"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (with-output-to-file "exclusion-list.txt"
                          (lambda _
	                    (display
                             (string-append
                              ;; tests timing out
                              "test_create_dynamic_large_object$\n"
                              "test_create_object_index_rax$\n"
                              "test_create_object_skip_checksum$\n"
                              "test_inspect_machine_inspect_failed$\n"
                              "test_inspect_machine_wait$\n"
                              "test_status_fails_different_attribute$\n"
                              "test_status_match$\n"
                              "test_status_match_different_attribute$\n"
                              "test_status_match_with_none$\n"
                              "test_wait_for_baremetal_node_lock_locked$\n"
                              "test_wait_for_task_error_396$\n"
                              "test_wait_for_task_wait$\n"))))
                        (invoke "stestr" "run"
                                "--exclude-list" "exclusion-list.txt")))))))
    (native-inputs (list python-ddt
                         python-hacking
                         python-jsonschema
                         python-pbr
                         python-prometheus-client
                         python-requests-mock
                         python-statsd
                         python-stestr
                         python-testscenarios
                         python-oslo-config
                         python-oslotest))
    (propagated-inputs (list python-appdirs
                             python-cryptography
                             python-decorator
                             python-dogpile-cache
                             python-importlib-metadata
                             python-iso8601
                             python-jmespath
                             python-jsonpatch
                             python-keystoneauth1
                             python-munch
                             python-netifaces
                             python-os-service-types
                             python-pbr   ; run-time dependency actually
                             python-pyyaml
                             python-requestsexceptions))
    (home-page "https://docs.openstack.org/openstacksdk/latest/")
    (synopsis "SDK for building applications to work with OpenStack")
    (description "This package provides a client library for building
applications to work with OpenStack clouds.  The SDK aims to provide a
consistent and complete set of interactions with OpenStack’s many services,
along with complete documentation, examples, and tools.")
    (license asl2.0)))
