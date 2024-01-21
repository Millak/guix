;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018, 2019, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
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

(define-module (gnu packages dav)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public radicale
  (package
    (name "radicale")
    (version "3.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kozea/Radicale")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qy2azn02bw772yhzgqvyf1pyl0ijj9ccvl1078w9icl261yljap"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-pytest-flake8
           python-pytest-isort
           python-pytest-runner
           python-waitress))
    (propagated-inputs
     (list python-dateutil python-defusedxml python-passlib
           python-vobject))
    (synopsis "Basic CalDAV and CardDAV server")
    (description "Radicale is a CalDAV and CardDAV server for UNIX-like
platforms.  Calendars and address books are available for both local and remote
access, possibly limited through authentication policies.  They can be viewed
and edited by calendar and contact clients on mobile phones or computers.

Radicale intentionally does not fully comply with the CalDAV and CardDAV RFCs.
Instead, it supports the CalDAV and CardDAV implementations of popular
clients.")
    (home-page "https://radicale.org/")
    (license gpl3+)))

(define-public xandikos
  (package
    (name "xandikos")
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xandikos" version))
       (sha256
        (base32 "00ghmzcc37b17pp0r6d9v5vpxmz500kzxqj1k9ppcjhbbpvp9w8n"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-aiohttp
           python-defusedxml
           python-dulwich
           python-icalendar
           python-jinja2
           python-multidict))
    (home-page "https://www.xandikos.org/")
    (synopsis "Lightweight CalDAV/CardDAV server")
    (description
     "Xandikos is a lightweight yet complete CardDAV/CalDAV server that backs
onto a Git repository.

Features:

@itemize
@item Easy to set up
@item Share calendars (events, todo items, journal entries) via CalDAV and
contacts (vCard) via CardDAV
@item Automatically keep history and back up changes in Git
@item Supports synchronization extensions for CalDAV/CardDAV for quick and
efficient syncing
@item Automatically keep history and back up
@item Works with all tested CalDAV and CardDAV clients
@end itemize")
    (license gpl3+)))

(define-public vdirsyncer
  (package
    (name "vdirsyncer")
    (version "0.19.2")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri name version))
             (sha256
              (base32
               "1fl21m10ghrpmkqa12g0qri99cxk9879pkb60jd4b4w2mgp8q1gx"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ; the test suite is very flakey
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs outputs tests? #:allow-other-keys)
              (add-installed-pythonpath inputs outputs)
              (setenv "DETERMINISTIC_TESTS" "true")
              (setenv "DAV_SERVER" "radicale")
              (setenv "REMOTESTORAGE_SERVER" "skip")
              (if tests?
                  (invoke "make" "test"))))
          (add-after 'unpack 'patch-version-call
            (lambda _
              (substitute* "docs/conf.py"
                (("^release.*")
                 (string-append "release = '" #$version "'\n"))))))))
    (native-inputs
     (list python-setuptools-scm
           python-sphinx
           ;; Required for testing
           python-aioresponses
           python-hypothesis
           python-trustme
           python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-pytest-httpserver
           radicale))
    (inputs
     (list python-aiohttp
           python-aiostream
           python-atomicwrites
           python-click
           python-click-log
           python-requests
           python-requests-toolbelt))
    (synopsis "Synchronize calendars and contacts")
    (description "Vdirsyncer synchronizes your calendars and addressbooks
between two storage locations.  The most popular purpose is to
synchronize a CalDAV or CardDAV server with a local folder or file.  The
local data can then be accessed via a variety of programs, none of which
have to know or worry about syncing to a server.")
    (home-page "https://github.com/pimutils/vdirsyncer")
    (license bsd-3)))
