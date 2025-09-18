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
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module ((guix licenses) #:prefix license:)
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
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module ((guix build-system python) #:select (pypi-uri))
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public python-cliff
  (package
    (name "python-cliff")
    (version "4.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cliff" version))
       (sha256
        (base32 "0vd8b4lypkc65xb4cih2b4l9qkhxyj52xj078q63p8214xl5n7wc"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-coverage
           python-fixtures
           python-setuptools
           python-sphinx-5
           python-stestr
           python-testscenarios
           python-wheel))
    (propagated-inputs
     (list python-autopage
           python-cmd2
           python-prettytable
           python-pyyaml
           python-stevedore))
    (home-page "https://opendev.org/openstack/cliff")
    (synopsis "Framework for building command line programs")
    (description "The @code{cliff} framework allows creating multi-level
commands such as those of @command{subversion} and @command{git}, where the
main program handles some basic argument parsing and then invokes a
sub-command to do the work.  It uses plugins to define sub-commands, output
formatters, and other extensions.")
    (license license:asl2.0)))

(define-public python-cliff-bootstrap
  (hidden-package
   (package/inherit python-cliff
     (arguments
      (substitute-keyword-arguments (package-arguments python-cliff)
        ((#:tests? t? #t)
         #f)
        ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (delete 'sanity-check)))))
     (native-inputs
      (list python-setuptools python-wheel))
     (propagated-inputs
      (modify-inputs (package-propagated-inputs python-cliff)
        (replace "python-stevedore" python-stevedore-bootstrap))))))

(define-public python-debtcollector
  (package
    (name "python-debtcollector")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "debtcollector" version))
        (sha256
         (base32 "0vzarkvjclci98d8lvkix6qj59f7rxp1qg2x6q6is7qfbg91g29a"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                (("^(coverage|hacking|pre-commit|reno).*")
                 "")
                (("^(doc8|sphinx|openstackdocstheme).*")
                 "")))))))
    (propagated-inputs
     (list python-pbr python-wrapt))
    (native-inputs
     (list python-fixtures
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
    (license license:asl2.0)))

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
    (license license:asl2.0)))

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
          (base32 "0w58adwv7q9wzvmq9mlrk2asfk73myq9fpwy7mjkzsz3baa95zf5"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-fixtures python-pbr))
    (native-inputs
     (list python-openstackdocstheme
           python-setuptools
           python-sphinx
           python-subunit
           python-testrepository
           python-testtools
           python-wheel))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-for-python-3.11
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
    (license license:asl2.0)))

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
    (license license:asl2.0)))

(define-public python-os-service-types
  (package
    (name "python-os-service-types")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "os_service_types" version))
       (sha256
        (base32 "0gk0lgg862pwpisjz36jlrnr5ij75c8ja01znb1398rc2d7yf349"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                (("(coverage|hacking).*")
                 ""))))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (native-inputs
     (list python-keystoneauth1
           python-oslotest
           python-pbr-next
           python-requests-mock
           python-setuptools
           python-stestr
           python-testscenarios
           python-wheel))
    (home-page "https://docs.openstack.org/os-service-types/latest/")
    (synopsis "Library for consuming OpenStack Service Types Authority data")
    (description "The @emph{OpenStack Service Types Authority} contains
information about officiag OpenStack services and their historical
service-type aliases.  The data is in JSON and the latest data should always
be used.  This simple library exists to allow for easy consumption of the
data, along with a built-in version of the data to use in case network access
is for some reason not possible and local caching of the fetched data.")
    (license license:asl2.0)))

(define-public python-os-service-types-bootstrap
  (hidden-package
   (package/inherit python-os-service-types
     (arguments
      (substitute-keyword-arguments (package-arguments python-os-service-types)
        ((#:tests? t? #t)
         #f)
        ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (delete 'sanity-check)))))
     (native-inputs
      (list python-pbr-next python-setuptools python-wheel)))))

(define-public python-os-testr
  (package
    (name "python-os-testr")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "os-testr" version))
       (sha256
        (base32 "0vik5sjl0qhz6xqqg6gnaf5jva31m7xykyc0azb53jfq7y57ladv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                (("(coverage|hacking).*")
                 "")))))))
    (propagated-inputs
     (list python-stestr))
    (native-inputs
     (list python-babel
           python-ddt
           python-oslotest
           python-pbr
           python-setuptools
           python-testrepository
           python-testscenarios
           python-testtools
           python-wheel))
    (home-page "https://www.openstack.org/")
    (synopsis "Testr wrapper to provide functionality for OpenStack projects")
    (description
      "Os-testr provides developers with a testr wrapper and an output filter
  for subunit.")
    (license license:asl2.0)))

(define-public python-stevedore
  (package
    (name "python-stevedore")
    (version "5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stevedore" version))
       (sha256
        (base32
         "0jvgrn2mk7psrgly61k16p6pywnb191gzfliy9p824pya2pbad9i"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                (("sphinx.*")
                 "sphinx\n"))))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (propagated-inputs (list python-pbr))
    (native-inputs
     (list python-coverage
           python-setuptools
           python-sphinx
           python-stestr
           python-wheel))
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
    (license license:asl2.0)))

(define-public python-stevedore-bootstrap
  (hidden-package
   (package/inherit python-stevedore
     (arguments
      (substitute-keyword-arguments (package-arguments python-stevedore)
        ((#:tests? t? #t)
         #f)
        ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (delete 'sanity-check)))))
     (native-inputs
      (list python-setuptools python-wheel)))))

(define-public python-tempest
  (package
    (name "python-tempest")
    (version "42.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tempest" version))
       (sha256
        (base32 "03jc474y57k4c2ydlzw8qx327am9ag15s8j4r4jadrs2x149qvlx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--test-path" "./tempest/tests"
              ;; XXX: Probably requires some setup.
              "--exclude-regex" (string-join
                                 (list "test_load_json_resource_list"
                                       "test_load_json_saved_state"
                                       "test_take_action_got_exception"
                                       "test_hacking")
                                 "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-tests
            (lambda _
              ;; XXX: Most of those tests require network connection.
              (delete-file-recursively "tempest/tests/lib/services/volume")
              (delete-file "tempest/tests/test_hacking.py")))
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                ;; unused, code-quality checks only
                (("(hacking|flake8-.*|pycodestyle|coverage)[<>!=]" line)
                 (string-append "# " line)))))
          (add-before 'check 'setup-check
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "tempest/tests/lib/cli/test_execute.py"
                (("cli_base.execute\\(\"env\",")
                 (string-append "cli_base.execute('"
                                (search-input-file inputs "bin/env") "',")))))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (propagated-inputs (list python-cliff
                             python-cryptography
                             python-defusedxml
                             python-fasteners
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
    (native-inputs (list python-oslotest
                         python-pbr
                         python-setuptools
                         python-stestr
                         python-testscenarios
                         python-wheel))
    (home-page "https://docs.openstack.org/tempest/latest/")
    (synopsis "OpenStack Integration Testing")
    (description "This is a set of integration tests to be run against a live
OpenStack cluster.  Tempest has batteries of tests for OpenStack API
validation, scenarios, and other specific tests useful in validating an
OpenStack deployment.")
    (license license:asl2.0)))

(define-public python-tempest-lib
  (package
    (name "python-tempest-lib")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tempest-lib" version))
       (sha256
        (base32 "1cpp2vwmawpd29hjsklsps181lq2ah91cl412qvpnz228nf9sqn5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; FIXME: Requires a lot of ancient packages.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "requirements.txt"
                (("jsonschema[<>!=].*") "jsonschema\n"))))
          (add-before 'check 'pre-check
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "tempest_lib/tests/cli/test_execute.py"
                (("/bin/ls")
                 (search-input-file inputs "bin/ls"))))))))
    (propagated-inputs
     (list python-fixtures
           python-httplib2
           python-iso8601
           python-jsonschema
           python-oslo-log
           python-paramiko))
    (native-inputs
     (list python-babel
           python-mock
           python-os-testr
           python-oslotest
           python-pbr
           python-setuptools
           python-wheel))
    (home-page "https://www.openstack.org/")
    (synopsis "OpenStack functional testing library")
    (description
     "Tempest-lib is a functional testing library for OpenStack.  It provides
common features used in Tempest.")
    (license license:asl2.0)))


;;;
;;; Packages from the Oslo library
;;;

(define-public python-oslo-concurrency
  (package
    (name "python-oslo-concurrency")
    (version "7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo_concurrency" version))
       (sha256
        (base32 "1jqlshynn8ddix3jx9ma969d6829xyy0vryhy5lpvc02h1zqg2nz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: At least another test is failing, but blocks the rest.
      #:tests? #f
      ;; XXX: Disable failing tests.
      #:test-flags
      #~(list "--exclude-regex" (string-join
                                 (list "test_fair_lock_with_spawn"
                                       "test_fair_lock_with_spawn_n"
                                       "test_with_stdout")
                                 "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                (("(hacking|coverage|bandit|pre-commit)[<>!=]" line)
                 (string-append "# " line)))))
          (add-before 'check 'fix-tests
            (lambda* (#:key inputs #:allow-other-keys)
              (define (cross-which bin)
                (search-input-file inputs (string-append "bin/" bin)))
              (substitute* "oslo_concurrency/tests/unit/test_processutils.py"
                (("#!/bin/(bash|sh)" all sh)
                 (string-append "#!" (cross-which sh)))
                (("'/usr/bin/env('| )" all rest)
                 (string-append "'" (cross-which "env") rest))
                (("'/bin/true'")
                 (string-append "'" (cross-which "true") "'")))))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (native-inputs
     (list python-coverage
           python-eventlet
           python-fixtures
           python-oslotest
           python-pbr
           python-setuptools
           python-stestr
           python-wheel))
    (propagated-inputs
     (list python-fasteners
           python-oslo-config
           python-oslo-i18n
           python-oslo-utils))
    (home-page "https://docs.openstack.org/oslo.concurrency/latest/")
    (synopsis "Oslo Concurrency library")
    (description "The Oslo Concurrency Library provides utilities for safely
running multi-thread, multi-process applications using locking mechanisms and
for running external processes.")
    (license license:asl2.0)))

(define-public python-oslo-config
  (package
    (name "python-oslo-config")
    (version "8.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.config" version))
       (sha256
        (base32 "0q3v4yicqls9zsfxkmh5mrgz9dailaz3ir25p458gj6dg3bldhx0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Disable failing tests.
      #:test-flags
      #~(list "--exclude-regex" (string-join
                                 (list "test_print_help"
                                       "test_print_strOpt_with_choices_help")
                                 "|"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (propagated-inputs
     (list python-netaddr
           python-oslo-i18n-bootstrap
           python-pyyaml
           python-requests
           python-rfc3986
           python-stevedore))
    (native-inputs
     (list python-coverage
           python-docutils
           python-fixtures
           python-mypy
           python-oslo-log-bootstrap
           python-oslotest-bootstrap
           python-pbr
           python-requests-mock
           python-setuptools
           python-sphinx
           python-stestr
           python-testscenarios
           python-testtools
           python-wheel))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo Configuration API")
    (description
     "The Oslo configuration API supports parsing command line arguments and
.ini style configuration files.")
    (license license:asl2.0)))

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
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                ;; unused, code-quality checks only
                (("(bandit|hacking|pre-commit)[<>!=]" line)
                 (string-append "# " line))))))))
    (propagated-inputs
     (list python-debtcollector))
    (native-inputs
     (list python-coverage
           python-fixtures
           python-mypy
           python-oslotest-bootstrap
           python-pbr
           python-setuptools
           python-stestr
           python-wheel))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo context library")
    (description
      "The Oslo context library has helpers to maintain useful information
about a request context.  The request context is usually populated in the WSGI
pipeline and used by various modules such as logging.")
    (license license:asl2.0)))

(define-public python-oslo-i18n
  (package
    (name "python-oslo-i18n")
    (version "6.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo_i18n" version))
       (sha256
        (base32 "178h7grww0brsdcp2iz87giappl928ir82arnvppcz5gqmq6m1ga"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-coverage
           python-debtcollector
           python-oslo-config
           python-oslotest
           python-pbr
           python-setuptools
           python-stestr
           python-testscenarios
           python-wheel))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo internationalization (i18n) library")
    (description
     "The oslo.i18n library contain utilities for working with
internationalization (i18n) features, especially translation for text strings
in an application or library.")
    (license license:asl2.0)))

(define-public python-oslo-i18n-bootstrap
  (hidden-package
   (package/inherit python-oslo-i18n
     (arguments
      (substitute-keyword-arguments (package-arguments python-oslo-i18n)
        ((#:tests? t? #t)
         #f)
        ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (delete 'sanity-check)))))
     (native-inputs
      (list python-pbr python-setuptools python-wheel)))))

(define-public python-oslo-log
  (package
  (name "python-oslo-log")
  (version "7.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "oslo_log" version))
      (sha256
        (base32 "1siw60x15ysfbf88hiisdn556y4anixj3clciky4xcwsiz8a6gwn"))))
  (build-system pyproject-build-system)
  (arguments
   (list
    #:phases
    #~(modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? test-flags #:allow-other-keys)
            (when tests?
              (apply invoke "stestr" "run" test-flags)))))))
  (propagated-inputs
   (list python-dateutil
         python-oslo-config
         python-oslo-context
         python-oslo-i18n-bootstrap
         python-oslo-serialization
         python-oslo-utils))
  (native-inputs
   (list python-coverage
         python-eventlet
         python-fixtures
         python-oslotest-bootstrap
         python-pbr-next
         python-setuptools
         python-stestr
         python-testtools
         python-wheel))
  (home-page "https://launchpad.net/oslo")
  (synopsis "Python logging library of the Oslo project")
  (description
    "The oslo.log (logging) configuration library provides standardized
configuration for all OpenStack projects.  It also provides custom formatters,
handlers and support for context specific logging (like resource id’s etc).")
  (license license:asl2.0)))

(define-public python-oslo-log-bootstrap
  (hidden-package
   (package/inherit python-oslo-log
     (arguments
      (substitute-keyword-arguments (package-arguments python-oslo-log)
        ((#:tests? t? #t)
         #f)
        ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (delete 'sanity-check)))))
     (propagated-inputs
      (modify-inputs (package-propagated-inputs python-oslo-log)
        (delete "python-oslo-config"))))))

(define-public python-oslo-serialization
  (package
    (name "python-oslo-serialization")
    (version "5.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo_serialization" version))
       (sha256
        (base32 "1rrvdhv82gklj45z6xs1h4l51jcz8fmdjijya2rkj1mqjzfx7i5x"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (propagated-inputs
      (list python-msgpack python-oslo-utils python-tzdata))
    (native-inputs
     (list python-netaddr
           python-oslo-i18n-bootstrap
           python-oslotest-bootstrap
           python-setuptools
           python-stestr
           python-wheel))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo serialization library")
    (description
      "The oslo.serialization library provides support for representing objects
in transmittable and storable formats, such as JSON and MessagePack.")
    (license license:asl2.0)))

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
           python-pbr
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
           python-pyyaml))
    (home-page "https://docs.openstack.org/reno/latest/")
    (synopsis "Release notes manager")
    (description "Reno is a tool for storing release notes in a git repository
and building documentation from them.")
    (license license:asl2.0)))

(define-public python-oslosphinx
  (package
    (name "python-oslosphinx")
    (version "4.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslosphinx" version))
       (sha256
        (base32 "1xm41857vzrzjmnyi6bqirg4i5qa61v7wxcsdc4q1nzgr3ndgz5k"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                (("(hacking|reno).*")
                 "")))))))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-openstackdocstheme
           python-pbr
           python-setuptools
           python-sphinx
           python-wheel))
    (home-page "https://www.openstack.org/")
    (synopsis "OpenStack sphinx extensions and theme")
    (description "This package provides themes and extensions for Sphinx
documentation from the OpenStack project.")
    (license license:asl2.0)))

(define-public python-oslotest
  (package
    (name "python-oslotest")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslotest" version))
       (sha256
        (base32 "1vp85v81p2vx66j973hc7fa65shp0ilhaypyyny01jwcip94152s"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-fixtures python-subunit python-testtools))
    (native-inputs
     (list python-coverage
           python-debtcollector
           python-oslo-config
           python-stestr
           python-setuptools
           python-wheel))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo test framework")
    (description "The Oslo Test framework provides common fixtures, support
for debugging, and better support for mocking results.")
    (license license:asl2.0)))

(define-public python-oslotest-bootstrap
  (hidden-package
   (package/inherit python-oslotest
     (arguments
      (substitute-keyword-arguments (package-arguments python-oslotest)
        ((#:tests? t? #t)
         #f)
        ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (delete 'sanity-check)))))
     (native-inputs
      (list python-setuptools python-wheel)))))

(define-public python-oslo-utils
  (package
    (name "python-oslo-utils")
    (version "7.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "oslo.utils" version))
        (sha256
          (base32 "1kiynw1xhw88iimazjzmjrf4h8bzdyjyvkgj6jsz9p85m9fwnpda"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Disable failing test.
      #:test-flags
      #~(list "--exclude-regex" "test_format_6_luks")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (propagated-inputs
      (list python-debtcollector
            python-iso8601
            python-netaddr
            python-netifaces
            python-oslo-i18n-bootstrap
            python-packaging
            python-pbr
            python-psutil
            python-pyparsing
            python-pyyaml
            python-tzdata))
    (native-inputs
      (list python-ddt
            python-eventlet
            python-fixtures
            python-oslotest-bootstrap
            python-setuptools
            python-stestr
            python-testscenarios
            python-testtools
            python-wheel
            qemu-minimal))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo utility library")
    (description
      "The @code{oslo.utils} library provides support for common utility type
functions, such as encoding, exception handling, string manipulation, and time
handling.")
    (license license:asl2.0)))

(define-public python-keystoneauth1
  (package
    (name "python-keystoneauth1")
    (version "5.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "keystoneauth1" version))
       (sha256
        (base32 "1wwicmgmga7ylyb2a15nh5y29r41vmxhyijs7ynjqjvzkg214vw0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Disable failing test.
      #:test-flags
      #~(list "--exclude-regex" "test_keystoneauth_betamax_fixture")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                ;; unused, code-quality checks only
                (("(hacking|coverage|bandit|reno)[<>!=]"
                  line)
                 (string-append "# " line)))))
          (add-before 'check 'check-setup
            (lambda _
              ;; remove code-quality checks
              (delete-file "keystoneauth1/tests/unit/test_hacking_checks.py")))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (propagated-inputs
     (list python-iso8601
           python-os-service-types-bootstrap
           python-pbr
           python-requests
           python-stevedore
           python-typing-extensions))
    (native-inputs
     (list python-betamax
           python-fixtures
           python-lxml
           python-oauthlib
           python-oslo-config
           python-oslo-utils
           python-oslotest
           python-pyyaml
           python-requests-kerberos
           python-requests-mock
           python-setuptools
           python-stestr
           python-testresources
           python-testtools
           python-wheel))
    (home-page "https://docs.openstack.org/keystoneauth/latest/")
    (synopsis "Authentication Library for OpenStack Identity")
    (description "Keystoneauth provides a standard way to do authentication
and service requests within the OpenStack ecosystem.  It is designed for use
in conjunction with the existing OpenStack clients and for simplifying the
process of writing new clients.")
    (license license:asl2.0)))

(define-public python-keystoneclient
  (package
    (name "python-keystoneclient")
    (version "5.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python_keystoneclient" version))
        (sha256
         (base32
          "1rqhxvych2a41dxlizz0qy37vs8jcxxy4kk7khw7c03iqypf47bj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-tests
            (lambda _
              ;; XXX: Mostly tests for outdated os-client-config.
              (delete-file-recursively "keystoneclient/tests/functional/v3")))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags))))
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                ;; XXX: Outdated/discontinued.
                (("os-client-config[<>!=]" line) (string-append "# " line))
                ;; unused, code-quality checks only
                (("(hacking|flake8-.*|pycodestyle|coverage|bandit|reno)[<>!=]"
                  line)
                 (string-append "# " line))))))))
    (native-inputs
     (list openssl
           python-bandit
           python-fixtures
           python-keyring
           python-lxml
           python-mock
           python-oauthlib
           python-oslotest
           python-pbr
           python-requests-mock
           python-setuptools
           python-stestr
           python-tempest
           python-tempest-lib
           python-testresources
           python-testscenarios
           python-testtools
           python-wheel))
    (propagated-inputs
     (list python-keystoneauth1
           python-oslo-config
           python-oslo-i18n
           python-oslo-serialization
           python-oslo-utils
           python-packaging
           python-pbr
           python-requests
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
    (license license:asl2.0)))

(define-public python-swiftclient
  (package
    (name "python-swiftclient")
    (version "4.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python_swiftclient" version))
        (sha256
         (base32 "0qrq9fdcmqhg1374hsj58hddpfd2jm78q32yqbywls4k8smjq5j4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'relax-requirements
            (lambda _
              (delete-file "test-requirements.txt")))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (native-inputs
     (list python-keystoneauth1
           python-keystoneclient
           python-openstacksdk
           python-pbr
           python-setuptools
           python-stestr
           python-wheel))
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
    (license license:asl2.0)))

(define-public python-git-review
  (package
    (name "python-git-review")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "git_review" version))
       (sha256
        (base32 "1a7h3i1wsq0gsclb2mififypr9q0sz3ni0kf0qxmm2l40bpzmkqv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f                     ; tests require a running Gerrit server
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'wrap 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/git-review")
                `("PATH" ":" prefix
                  ,(map (lambda (bin)
                          (search-input-file inputs
                                             (string-append "bin/" bin)))
                        (list "git" "ssh")))))))))
    (native-inputs (list python-pbr python-setuptools python-wheel))
    (propagated-inputs (list python-requests))
    (inputs (list bash-minimal git-minimal openssh-sans-x))
    (home-page "https://docs.openstack.org/infra/git-review/")
    (synopsis "Command-line tool for Gerrit")
    (description
     "Git-review is a command-line tool that helps submitting Git branches to
Gerrit for review, or fetching existing ones.")
    (license license:asl2.0)))

(define-public python-requestsexceptions
  (package
    (name "python-requestsexceptions")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requestsexceptions" version))
       (sha256
        (base32 "0r9hp9yzgj8r81q5gc6r8sgxldqc09xi6ax0b7a6dw0qfv3wp5dh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                (("hacking.*")
                 "")))))))
    (native-inputs (list python-pbr python-setuptools python-wheel))
    (home-page "https://www.openstack.org/")
    (synopsis "Import exceptions from potentially bundled packages in requests")
    (description "The Python requests library bundles the urllib3 library,
however, some software distributions modify requests to remove the bundled
library.  This makes some operations difficult, such as suppressing the
“insecure platform warning” messages that urllib emits.  This package is a
simple library to find the correct path to exceptions in the requests library
regardless of whether they are bundled or not.")
    (license license:asl2.0)))

(define-public python-openstacksdk
  (package
    (name "python-openstacksdk")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openstacksdk" version))
       (sha256
        (base32 "0gbxjz8dg5ar5mgbk896cpq5vgp4j3a1iri8aakaxs9j8xn1czp4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--exclude-regex"
              (string-join
               (list
                ;; tests timing out
                "test_create_dynamic_large_object"
                "test_create_object_index_rax"
                "test_create_object_skip_checksum"
                "test_inspect_machine_inspect_failed"
                "test_inspect_machine_wait"
                "test_status_fails_different_attribute"
                "test_status_match"
                "test_status_match_different_attribute"
                "test_status_match_with_none"
                "test_wait_for_baremetal_node_lock_locked"
                "test_wait_for_task_error_396"
                "test_wait_for_task_wait"
                ;; XXX: Most fail due to network issues.
                "test_callback"
                "test_callback_without_progress"
                "test_create_data"
                "test_create_image_task"
                "test_create_no_data"
                "test_delete_autocreated_image_objects"
                "test_delete_firewall_policy"
                "test_delete_firewall_policy_filters"
                "test_delete_firewall_rule"
                "test_delete_firewall_rule_filters"
                "test_delete_image_task"
                "test_delete_volume_backup_wait"
                "test_get_object"
                "test_ost_version"
                "test_slo_manifest_fail"
                "test_status"
                "test_stream_object"
                "test_success_not_found")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "stestr" "run" test-flags)))))))
    (native-inputs
     (list python-debtcollector
           python-ddt
           python-hacking
           python-jsonschema
           python-oslo-config
           python-oslotest
           python-pbr
           python-prometheus-client
           python-requests-mock
           python-setuptools
           python-statsd
           python-stestr
           python-testscenarios
           python-wheel))
    (propagated-inputs
     (list python-cryptography
           python-decorator
           python-dogpile-cache
           python-iso8601
           python-jmespath
           python-jsonpatch
           python-keystoneauth1
           python-os-service-types
           python-pbr
           python-platformdirs
           python-psutil
           python-pyyaml
           python-requestsexceptions
           python-typing-extensions))
    (home-page "https://docs.openstack.org/openstacksdk/latest/")
    (synopsis "SDK for building applications to work with OpenStack")
    (description "This package provides a client library for building
applications to work with OpenStack clouds.  The SDK aims to provide a
consistent and complete set of interactions with OpenStack’s many services,
along with complete documentation, examples, and tools.")
    (license license:asl2.0)))
