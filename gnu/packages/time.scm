;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013, 2017, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015-2019, 2021, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2021 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Pradana AUMARS <paumars@courrier.dev>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages time)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public termdown
  (package
    (name "termdown")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "termdown" version))
       (sha256
        (base32
         "07nxsqpwnpr9jkvif2ngjlcq05z0ldnmqxd15d1l593lzmxdyrci"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-click
           python-pyfiglet
           python-dateutil))
    (home-page "https://github.com/trehn/termdown")
    (synopsis "Countdown timer for your terminal")
    (description
     "Termdown provides a fancy text display while it counts down to zero from
a starting point you provide.  The user can pause and resume the countdown
from the text user interface.  It can also be used in stop watch mode which
counts forward or for just showing the current time.")
    (license gpl3)))

(define-public time
  (package
    (name "time")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/time/time-"
                           version ".tar.gz"))
       (sha256
        (base32
         "07jj7cz6lc13iqrpgn81ivqh8rkm73p4rnivwgrrshk23v4g1b7v"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/time/")
    (synopsis "Run a command, then display its resource usage")
    (description
     "Time is a command that displays information about the resources that a
program uses.  The display output of the program can be customized or saved
to a file.")
    (license gpl3+)))

(define-public pps-tools
  ;; Last tagged release was in 2021
  (let ((commit "e5083fe1481a34373dee2acfabb63001ee9c40e0")
        (revision "1"))
    (package
      (name "pps-tools")
      (version (git-version "1.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/redlab-i/pps-tools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q3yvkwgqzafpx940cgqjn0harziv9gix1k3r3ymidmip0i5z1cp"))))
      (arguments
       (list
        #:tests? #f ;There is no test suite.
        #:make-flags
        #~(list "CC=gcc"
                (string-append "DESTDIR=" %output))
        #:phases
        #~(modify-phases %standard-phases
            ;; No configure script
            (delete 'configure)
            (add-after 'unpack 'patch-makefile
              (lambda _
                (substitute* "Makefile"
                  (("/usr/")
                   "/")))))))
      (build-system gnu-build-system)
      (home-page "https://github.com/redlab-i/pps-tools")
      (synopsis "User-space tools and headers for LinuxPPS")
      (description
       "This package includes the necessary headers for using
@url{http://linuxpps.org/, LinuxPPS} PPSAPI kernel interface in user-space
applications, and several support tools.")
      (license gpl2+))))

(define-public python-pytimeparse
  (package
    (name "python-pytimeparse")
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytimeparse" version))
       (sha256
        (base32
         "02kaambsgpjx3zi42j6l11rwms2p35b9hsk4f3kdf979gd3kcqg8"))))
    (native-inputs
     (list python-nose))
    (build-system python-build-system)
    (home-page "https://github.com/wroberts/pytimeparse")
    (synopsis "Time expression parser")
    (description "This small Python module parses various kinds of time
expressions.")
    (license expat)))

(define-public python-pytzdata
  (package
    (name "python-pytzdata")
    (version "2020.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytzdata" version))
       (sha256
        (base32
         "0h0md0ldhb8ghlwjslkzh3wcj4fxg3n43bj5sghqs2m06nri7yiy"))))
    (build-system python-build-system)
    ;; XXX: The PyPI distribution contains no tests, and the upstream
    ;; repository lacks a setup.py!  How to build from git?
    (arguments '(#:tests? #f))
    (propagated-inputs
     (list python-cleo))
    (home-page "https://github.com/sdispater/pytzdata")
    (synopsis "Timezone database for Python")
    (description
     "This library provides a timezone database for Python.")
    (license expat)))

(define-public python-tzdata
  (package
    (name "python-tzdata")
    ;; This package should be kept in sync with tzdata in (gnu packages base).
    (version "2023.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tzdata" version))
       (sha256
        (base32 "1ja8c6ybwhbzr37a0r56g4j555gxzss4k5irfwn54ra7557wjm6x"))
       (modules '((guix build utils)))
       (snippet #~(delete-file-recursively "src/tzdata/zoneinfo"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'ensure-no-mtimes-pre-1980 'unpack-tzdata
                 (lambda* (#:key inputs #:allow-other-keys)
                   (copy-recursively
                    (search-input-directory inputs "share/zoneinfo")
                    "src/tzdata/zoneinfo")
                   (delete-file "src/tzdata/zoneinfo/posix")
                   (call-with-output-file "src/tzdata/zoneinfo/__init__.py"
                     (const #t)))))))
    (inputs (list tzdata))
    (native-inputs
     (list python-pytest
           python-pytest-subtests
           python-setuptools
           python-wheel))
    (home-page "https://github.com/python/tzdata")
    (synopsis "Python wrapper of IANA time zone data")
    (description "This package provides a thin Python wrapper around tzdata.")
    (license asl2.0)))

(define-public python-pytz-deprecation-shim
  (package
    (name "python-pytz-deprecation-shim")
    (version "0.1.0.post0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytz_deprecation_shim" version))
       (sha256
        (base32 "17d58msbi18dc4lk29hcrgylvrv9vhniwi24axfdwvb13fp7n2dg"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-dateutil python-tzdata))
    (native-inputs (list python-pytest
                         python-pytz
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/pganssle/pytz-deprecation-shim")
    (synopsis "Shims to make deprecation of pytz easier")
    (description
     "This package aims to make the transition away from @code{pytz} easier.
It is intended for temporary usage only, and should allow you to drop your
dependency on @code{pytz} while also giving your users notice that eventually
you will remove support for the pytz-specific interface.")
    (license asl2.0)))

(define-public python-pytz
  (package
    (name "python-pytz")
    ;; This package should be kept in sync with tzdata in (gnu packages base).
    (version "2023.3.post1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "pytz" version))
      (sha256
       (base32
        "0yrxykwhk18x22lp0hjlj98mgnzrzlclz8kxam5vl7jap6zdskvv"))))
    (build-system python-build-system)
    (home-page "http://pythonhosted.org/pytz")
    (synopsis "Python timezone library")
    (description "This library brings the Olson tz database into Python.  It
allows accurate and cross platform timezone calculations using Python 2.4 or
higher.  It also solves the issue of ambiguous times at the end of daylight
saving time.  Almost all of the Olson timezones are supported.")
    (license expat)))

(define-public python-pendulum
  (package
    (name "python-pendulum")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pendulum" version))
       (sha256
        (base32 "01zjc245w08j0xryrgrq9vng59q1cl5ry0hcpw5rj774pyhhqsmh"))))
    (build-system python-build-system)
    ;; XXX: The PyPI distribution lacks tests, and the upstream repository
    ;; lacks a setup.py!
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Add setup.py to fix the build. Otherwise, the build will fail with
         ;; "no setup.py found".
         ;;
         ;; Upstream uses Poetry to build python-pendulum, including parts
         ;; written in C. Here, we simply add a setup.py file and do not build
         ;; the parts written in C. This is possible because python-pendulum
         ;; falls back on pure Python code when the C parts are not available
         ;; (reference: build.py).
         (add-after 'unpack 'add-setup.py
           (lambda _
             (call-with-output-file "setup.py"
               (lambda (port)
                 (format port
                         "from setuptools import find_packages, setup
setup(name='pendulum',
      version='~a',
      packages=find_packages())
"
                         ,version))))))
       ;; XXX: The PyPI distribution lacks tests.
       #:tests? #f))
    (propagated-inputs
     (list python-dateutil python-pytzdata))
    (home-page "https://github.com/sdispater/pendulum")
    (synopsis "Alternate API for Python datetimes")
    (description "Pendulum is a drop-in replacement for the standard
@code{datetime} class, providing an alternative API.  As it inherits from the
standard @code{datetime} all @code{datetime} instances can be replaced by
Pendulum instances.")
    (license expat)))

(define-public python-dateutil
  (package
    (name "python-dateutil")
    (version "2.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-dateutil" version))
       (patches (search-patches "python-dateutil-pytest-compat.patch"))
       (sha256
        (base32 "11iy7m4bp2lgfkcl0r6xzf34bvk7ppjmsyn2ygfikbi72v6cl8q1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list  ; avoid freezegun dependency
                     "--ignore=dateutil/test/test_utils.py"
                     "--ignore=dateutil/test/test_rrule.py"
                     ;; XXX: Fails to get timezone from /etc/localtime.
                     "--ignore=dateutil/test/test_tz.py")))
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-six))
    (home-page "https://dateutil.readthedocs.io/en/stable/")
    (synopsis "Extensions to the standard datetime module")
    (description
     "The dateutil module provides powerful extensions to the standard
datetime module, available in Python 2.3+.")
    ;; The license was changed from the three-clause BSD license to a dual
    ;; Apache 2.0/BSD-3 variant at 2017-12-01.  Some code is only available as
    ;; BSD-3 still; but all new code is dual licensed (the user can choose).
    (license (list bsd-3 asl2.0))))

(define-public python-dateutils
  (package
    (name "python-dateutils")
    (version "0.6.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dateutils" version))
              (sha256
               (base32
                "1wg3f3imjq3snvjccv64h5498pqv9xz664xhni7bsh8mnay91p83"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-dateutil python-pytz))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/jmcantrell/python-dateutils")
    (synopsis "Various utilities for working with date and datetime objects")
    (description
     "The main purpose of this package is to provide more complex arithmetic
operations on dates/times.  Heavy use is made of the @code{relativedelta} type
from the @code{dateutil} library.  Much of this package is just a light
wrapper on top of this with some added features such as range generation and
business day calculation.")
    (license bsd-0)))

(define-public python-parsedatetime
  (package
    (name "python-parsedatetime")
    (version "2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parsedatetime" version))
       (sha256
        (base32
         "0mfl0ixshqkwx7z5siaib7ix5j2iahb1jqfpyhqp42wan7xnicsc"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose python-pyicu python-pytest python-pytest-runner))
    (propagated-inputs
     (list python-future))
    (home-page "https://github.com/bear/parsedatetime/")
    (synopsis "Parse human-readable date/time text")
    (description
     "Parse human-readable date/time text.")
    (license asl2.0)))

(define-public python-ciso8601
  (package
    (name "python-ciso8601")
    (version "2.3.2")
    (source
     (origin
       (method git-fetch)
       ;; The PyPi distribution doesn't include the tests.
       (uri (git-reference
             (url "https://github.com/closeio/ciso8601")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "189adk14ygs1cx3ncm1wqqfh18r72gl299zkllncynp1y79d0nd1"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/closeio/ciso8601")
    (synopsis "Fast ISO8601 date time parser")
    (description
     "The package ciso8601 converts ISO 8601 or RFC 3339 date time strings
into Python datetime objects.")
    (license expat)))

(define-public python-relativetimebuilder
  (package
    (name "python-relativetimebuilder")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "relativetimebuilder" version))
       (sha256
        (base32 "1x83vzwajz8rmml8x4ysr4cnxh6x0w42wkhw4zivd8qsbi9zcwzm"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-aniso8601
           python-dateutil))
    (home-page "https://bitbucket.org/nielsenb/relativetimebuilder")
    (synopsis "ANISO8601 builder for dateutil relativedeltas")
    (description
     "This package provides functionality for utilizing the relativedelta
feature from the dateutil library, ensuring calendar precision with
aniso8601.")
    ;; setup.py and PyPI: "License :: OSI Approved :: BSD License"
    (license bsd-3)))

(define-public python-timezonefinder
  (package
    (name "python-timezonefinder")
    (version "6.2.0")
    (source
     (origin
       (method git-fetch)
       ;; The PyPi distribution doesn't include the tests.
       (uri (git-reference
             (url "https://github.com/jannikmi/timezonefinder")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qv9rh6j8c1cqwh4sxrfl1m1ah4aqrf0w2kyrf5cgrpfxi6xr94z"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; TODO: Guix has lower python-pytz than required in the latest
      ;; version:  pytz.exceptions.UnknownTimeZoneError:
      ;; 'America/Ciudad_Juarez'
      ;; It's optional, remove this constrain where python-pytz is updated.
      #:test-flags #~(list "-k" "not test_with_pytz")))
    (native-inputs
     (list python-poetry-core python-pytest python-setuptools))
    (propagated-inputs
     (list python-cffi python-h3-3 python-numba python-numpy python-pytz))
    (home-page "https://timezonefinder.michelfe.it/gui")
    (synopsis "Finding the timezone of any coordinates on Earth offline")
    (description "This is a python package for looking up the corresponding
timezone for given coordinates on earth entirely offline.")
    (license expat)))

(define-public python-tzlocal
  (package
    (name "python-tzlocal")
    (version "5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/regebro/tzlocal")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1apa3i5fsfw28jnaaaa7jr976y5wbifl3h04id0bvplvsb9zpmy7"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-tzdata))
    (native-inputs
     (list python-check-manifest
           python-pytest
           python-pytest-cov
           python-pytest-mock))
    (home-page "https://github.com/regebro/tzlocal")
    (synopsis "Local timezone information for Python")
    (description
     "Tzlocal returns a tzinfo object with the local timezone information.
This module attempts to fix a glaring hole in pytz, that there is no way to
get the local timezone information, unless you know the zoneinfo name, and
under several distributions that's hard or impossible to figure out.")
    (license expat)))

(define-public python-isodate
  (package
    (name "python-isodate")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "isodate" version))
       (sha256
        (base32
         "1rjkm5qj3lz60sgva5g38cpfqd8byj2jlaf0qskg8xna8c7smlac"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/gweis/isodate/")
    (synopsis "Python date parser and formatter")
    (description
     "Python-isodate is a python module for parsing and formatting
ISO 8601 dates, time and duration.")
    (license bsd-3)))

(define-public python-iso8601
  (package
    (name "python-iso8601")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iso8601" version))
       (sha256
        (base32
         "1ccl6plks706hxm35cn1wsvxhqh3bfwi5cjgjpdxjib81qi07x97"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv" "iso8601"))))))
    (native-inputs
     (list python-pytest python-pytz))
    (home-page "https://github.com/micktwomey/pyiso8601")
    (synopsis "Module to parse ISO 8601 dates")
    (description
     "This module parses the most common forms of ISO 8601 date strings (e.g.
@code{2007-01-14T20:34:22+00:00}) into @code{datetime} objects.")
    (license expat)))

(define-public python-monotonic
  (package
    (name "python-monotonic")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "monotonic" version))
       (sha256
        (base32
         "1c6z46yb600klbfhqadyl7vq0jdjdxkm72k43ra3iw3d0xakv593"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://github.com/atdt/monotonic")
    (synopsis "Implementation of time.monotonic() for Python 2 & < 3.3")
    (description
     "This module provides a @code{monotonic()} function which returns the
value (in fractional seconds) of a clock which never goes backwards.")
    (license asl2.0)))

(define-public python-pyrfc3339
  (package
    (name "python-pyrfc3339")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyRFC3339" version))
       (sha256
        (base32
         "06jv7ar7lpvvk0dixzwdr3wgm0g1lipxs429s2z7knwwa7hwpf41"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytz))
    (native-inputs
     (list python-nose))
    (home-page "https://github.com/kurtraschke/pyRFC3339")
    (synopsis "Python timestamp library")
    (description "Python library for generating and parsing RFC 3339-compliant
timestamps.")
    (license expat)))

(define-public python-arrow
  (package
    (name "python-arrow")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "arrow" version))
              (sha256
               (base32
                "189knrgxb3x21lzvqac6qlpd32308hcmpccxdlvr5wmrl46b6d1r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "tests"
                          ;; python-dateutil doesn't recognize America/Nuuk.
                          ;; Remove when python-dateutil > 2.8.1.
                          "-k" "not test_parse_tz_name_zzz")))
    (native-inputs
     (list python-chai
           python-pytest
           python-pytest-cov
           python-pytest-mock
           python-pytz
           python-setuptools
           python-simplejson
           python-wheel))
    (propagated-inputs
     (list python-dateutil))
    (home-page "https://github.com/arrow-py/arrow")
    (synopsis "Dates and times for Python")
    (description
     "Arrow is a Python library to creating, manipulating, formatting and
converting dates, times, and timestamps.  It implements and updates the
datetime type.")
    (license asl2.0)))

(define-public python-aniso8601
  (package
    (name "python-aniso8601")
    (version "9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aniso8601" version))
       (sha256
        (base32
         "0wxry6riyqajl02mkad8g2q98sx5jr13zndj3fandpzfcxv13qvj"))))
    (build-system python-build-system)
    (home-page "https://bitbucket.org/nielsenb/aniso8601")
    (synopsis "Python library for parsing ISO 8601 strings")
    (description
     "This package contains a library for parsing ISO 8601 datetime strings.")
    (license bsd-3)))

(define-public rdate
  (let ((commit "91d84610e3695e90a884e2953908e95a856a9b74")
        (revision "1"))
    (package
      (name "rdate")
      (version (git-version "1.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/njh/rdate")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "002ryjq8gj1ml5la4v6dr3bh1zw5kxwy65rpziq8d2ccccarhv59"))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf automake))
      (synopsis "Get date and time based on RFC 868")
      (description
       "@code{rdate} connects to an RFC 868 time server over a TCP/IP network,
printing the returned time and/or setting the system clock.")
      (home-page "https://www.aelius.com/njh/rdate/")
      (license gpl2+))))

(define-public datefudge
  (package
    (name "datefudge")
    (version "1.26")
    (source (origin
              ;; Source code is available from
              ;; <https://salsa.debian.org/debian/datefudge.git>.  However,
              ;; for bootstrapping reasons, we do not rely on 'git-fetch' here
              ;; (since Git -> GnuTLS -> datefudge).
              (method url-fetch)
              (uri (list
                     (string-append
                       "mirror://debian/pool/main/d/datefudge/datefudge_"
                       version ".tar.xz")
                     ;; Update the Debian snapshot URL when updating the package.
                     (string-append
                       "https://snapshot.debian.org/archive/debian/"
                       "20240115T092401Z/pool/main/d/datefudge/"
                       "datefudge_1.26.tar.xz")))
              (sha256
               (base32
                "09cjds76gzkwk6ssmsk3cgkcfhglfi9kmbahi1h17v4311v432iz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "VERSION=" ,version)
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list perl))
    (home-page "https://salsa.debian.org/debian/datefudge")
    (synopsis "Pretend the system date is different")
    (description
     "Utility that fakes the system time by pre-loading a small library that
modifies the @code{time}, @code{gettimeofday} and @code{clock_gettime} system
calls.")
    (license gpl2)))

(define-public tz
  (package
    (name "tz")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oz/tz")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zf5w6338y0s0pf0jlpbqzlbxbx39s93z0bmdaa0cxkxs8cz8xij"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/oz/tz"))
    (inputs
     (list go-github-com-charmbracelet-bubbletea
           go-github-com-muesli-termenv
           go-github-com-tkuchiki-go-timezone))
    (home-page "https://github.com/oz/tz")
    (synopsis "TUI time zone helper")
    (description
     "@command{tz} helps you schedule things across time zones.  It is an
interactive TUI program that displays time across a few time zones of your
choosing.")
    (license gpl3+)))

(define-public countdown
  (package
    (name "countdown")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/antonmedv/countdown")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0snz26dvj8v58fyzd51bcf07b5yp2akcyy26w7b0pnkmlh3lknmk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/antonmedv/countdown"))
    (native-inputs
     (list go-github-com-nsf-termbox-go))
    (home-page "https://github.com/antonmedv/countdown")
    (synopsis "Counts to zero with a text user interface")
    (description
     "Countdown provides a fancy text display while it counts down to zero
from a starting point you provide.  The user can pause and resume the
countdown from the text user interface.")
    (license expat)))
