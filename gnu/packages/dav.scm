;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018, 2019, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2022, 2024 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Junker <dk@junkeria.club>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml))

(define-public cadaver
  (package
    (name "cadaver")
    (version "0.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://notroj.github.io/cadaver/cadaver-"
                           version ".tar.gz"))
       (sha256
        (base32 "0mbv6mkdhxqhdq5kgn821if10h184m1xlpqq0vpxj19mvwyf8dlj"))))
    (build-system gnu-build-system)
    (inputs (list neon
                  openssl
                  libxml2))
    (native-inputs (list pkg-config))
    (arguments '(#:configure-flags (list "--with-ssl=openssl")
                 #:tests? #f)) ; no check target
    (home-page "https://notroj.github.io/cadaver/")
    (synopsis "Command-line WebDAV client")
    (description
     "Cadaver is a command-line client for WebDAV server operations.  It
supports a variety of WebDAV features and provides an interactive
command-line environment with support for file manipulation on remote WebDAV
servers.")
    (license license:gpl2+)))

(define-public radicale
  (package
    (name "radicale")
    (version "3.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kozea/Radicale")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0walj1b7jrcc85maav04ciiipcqsl0mwbq3icksql26vr4i3y19z"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-pytest-flake8
           python-pytest-isort
           python-setuptools
           python-waitress
           python-wheel))
    (propagated-inputs
     (list python-defusedxml
           python-passlib
           python-pika
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
    (license license:gpl3+)))

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
    (license license:gpl3+)))

(define-public vdirsyncer
  (package
    (name "vdirsyncer")
    (version "0.19.3")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri name version))
             (sha256
              (base32
               "13xjzqphj0v611b9kqgp0c5rn46xysf8ykv58hsyqpcqxcgqadz4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "-k" "not test_request_ssl")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-version-call
            (lambda _
              (substitute* "docs/conf.py"
                (("^release.*")
                 (string-append "release = '" #$version "'\n"))))))))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-sphinx
           python-wheel
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
    (license license:bsd-3)))
