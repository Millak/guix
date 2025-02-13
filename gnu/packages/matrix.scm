;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Alex ter Weele <alex.ter.weele@gmail.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2023 Miguel Ángel Moreno <mail@migalmoreno.com>
;;; Copyright © 2023 conses <contact@conses.eu>
;;; Copyright © 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2025 Arjan Adriaanse <arjan@adriaan.se>
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

(define-module (gnu packages matrix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public mautrix-whatsapp
  (package
    (name "mautrix-whatsapp")
    (version "0.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mautrix/whatsapp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wn9kldhh865w5mbg3cl61m0db9nr7zl1j8lqs2dixsaihv1lbnx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:install-source? #f
      #:tests? #f ; no tests provided
      #:import-path "go.mau.fi/mautrix-whatsapp/cmd/mautrix-whatsapp"
      #:unpack-path "go.mau.fi/mautrix-whatsapp"
      #:build-flags
      #~(list (string-append "-ldflags=-X main.Tag=" #$version))
      #:embed-files
      #~(list
         ;; go.mau.fi/whatsmeow/proto/<...> pattern <...>.pb.raw: cannot embed
         ;; irregular file <...>.pb.raw
         ".*\\.pb\\.raw$"
         ;; maunium.net/go/mautrix/bridgev2/database/upgrades/upgrades.go
         ;; maunium.net/go/mautrix/crypto/sql_store_upgrade/upgrade.go
         ;; maunium.net/go/mautrix/sqlstatestore/statestore.go
         ;;
         ;; pattern *.sql: cannot embed irregular file <...>.sql
         ".*\\.sql"
         ;; golang.org/x/net/publicsuffix/table.go:63:12: pattern
         ;; data/children: cannot embed irregular file data/children
         "children"
         ;; golang.org/x/net/publicsuffix/table.go:48:12: pattern data/nodes:
         ;; cannot embed irregular file data/nodes
         "nodes"
         ;; golang.org/x/net/publicsuffix/table.go:33:12: pattern data/text:
         ;; cannot embed irregular file data/text
         "text"
         ;; maunium.net/go/mautrix/bridgev2/matrix/mxmain/config.go:17:12:
         ;; pattern example-config.yaml: cannot embed irregular file
         ;; example-config.yaml
         "example-config\\.yaml"
         ;; go.mau.fi/webp/webp.go:14:12: pattern internal: cannot embed
         ;; directory internal: contains no embeddable files
         ".*\\.c"
         ".*\\.h")))
    (native-inputs
     (list olm
           go-github-com-gorilla-mux
           go-github-com-gorilla-websocket
           go-github-com-lib-pq
           go-github-com-rs-zerolog
           go-go-mau-fi-util
           go-go-mau-fi-webp
           go-go-mau-fi-whatsmeow
           go-golang-org-x-exp
           go-golang-org-x-image
           go-golang-org-x-net
           go-golang-org-x-sync
           go-google-golang-org-protobuf
           go-gopkg-in-yaml-v3
           go-maunium-net-go-mautrix))
    (home-page "https://go.mau.fi/mautrix-whatsapp")
    (synopsis "Matrix <-> WhatsApp puppeting bridge")
    (description
     "This package provides a @code{Matrix<->WhatsApp} puppeting bridge based
on @url{https://github.com/tulir/whatsmeow, whatsmeow}.")
    (license license:agpl3+)))

(define-public python-matrix-client
  (package
    (name "python-matrix-client")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matrix-client" version))
       (sha256
        (base32
         "1mgjd0ymf9mvqjkvgx3xjhxap7rzdmpa21wfy0cxbw2xcswcrqyw"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-pytest python-pytest-runner python-responses))
    (home-page
     "https://github.com/matrix-org/matrix-python-sdk")
    (synopsis "Client-Server SDK for Matrix")
    (description "This package provides client-server SDK for Matrix.")
    (license license:asl2.0)))

(define-public python-matrix-synapse-ldap3
  (package
    (name "python-matrix-synapse-ldap3")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matrix-synapse-ldap3" version))
       (sha256
        (base32
         "01bms89sl16nyh9f141idsz4mnhxvjrc3gj721wxh1fhikps0djx"))))
    (build-system python-build-system)
    (arguments
     ;; tests require synapse, creating a circular dependency.
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  ;; Also, auth_provider.py attempts to import synapse.
                  (delete 'sanity-check))))
    (propagated-inputs
     (list python-twisted python-ldap3 python-service-identity))
    (home-page "https://github.com/matrix-org/matrix-synapse-ldap3")
    (synopsis "LDAP3 auth provider for Synapse")
    (description
     "This package allows Synapse to use LDAP as a password provider.
This lets users log in to Synapse with their username and password from
an LDAP server.")
    (license license:asl2.0)))

(define-public synapse
  (package
    (name "synapse")
    (version "1.29.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "matrix-synapse" version))
              (sha256
               (base32
                "0if2yhpz8psg0661401mvxznldbfhk2j9rhbs25jdaqm9jsv6907"))))
    (build-system python-build-system)
    ;; TODO Run tests with ‘PYTHONPATH=. trial3 tests’.
    (propagated-inputs
     (list python-simplejson ; not attested but required
           ;; requirements (synapse/python_dependencies.py)
           python-jsonschema
           python-frozendict
           python-unpaddedbase64
           python-canonicaljson
           python-signedjson
           python-pynacl
           python-idna
           python-service-identity
           python-twisted
           python-treq
           python-pyopenssl
           python-pyyaml
           python-pyasn1
           python-pyasn1-modules
           python-daemonize
           python-bcrypt
           python-pillow
           python-sortedcontainers
           python-pymacaroons
           python-msgpack
           python-phonenumbers
           python-six
           python-prometheus-client
           python-attrs
           python-netaddr
           python-jinja2
           python-bleach
           python-typing-extensions
           ;; conditional requirements (synapse/python_dependencies.py)
           ;;("python-hiredis" ,python-hiredis)
           python-matrix-synapse-ldap3
           python-psycopg2
           python-jinja2
           python-txacme
           python-pysaml2
           python-lxml
           python-packaging
           ;; sentry-sdk, jaeger-client, and opentracing could be included, but
           ;; all are monitoring aids and not essential.
           python-pyjwt))
    (native-inputs
     (list python-mock python-parameterized))
    (home-page "https://github.com/matrix-org/synapse")
    (synopsis "Matrix reference homeserver")
    (description "Synapse is a reference \"homeserver\" implementation of
Matrix from the core development team at matrix.org, written in
Python/Twisted.  It is intended to showcase the concept of Matrix and let
folks see the spec in the context of a codebase and let you run your own
homeserver and generally help bootstrap the ecosystem.")
    (license license:asl2.0)))

(define-public python-matrix-nio
  (package
    (name "python-matrix-nio")
    (version "0.25.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/poljar/matrix-nio.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07prfdnkr13d0pvzhnicwnpn562fwq9zx05d6wza230s7vj0mmk4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This test requires an Internet connection
      '(list "tests" "-k" "not test_connect_wrapper")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "tests/helpers.py"
               (("from nio.crypto import OlmAccount, OlmDevice")
                "from nio.crypto.device import OlmDevice
from nio.crypto.sessions import OlmAccount")))))))
    (native-inputs
     (list python-aioresponses
           python-faker
           python-hpack
           python-hyperframe
           python-hypothesis
           python-mypy
           python-mypy-extensions
           python-poetry-core
           python-pytest
           python-pytest-aiohttp
           python-pytest-asyncio
           python-pytest-benchmark
           python-pytest-cov
           python-pytest-flake8
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-aiofiles
           python-aiohttp
           python-aiohttp-socks
           python-atomicwrites
           python-cachetools
           python-h11
           python-h2
           python-jsonschema
           python-olm
           python-peewee
           python-pycryptodome
           python-unpaddedbase64))
    (home-page "https://github.com/poljar/matrix-nio")
    (synopsis
     "Python Matrix client library, designed according to sans I/O principles")
    (description
     "Matrix nio is a multilayered Matrix client library.  The underlying base
layer doesn't do any network IO on its own, but on top of that is a full
fledged batteries-included asyncio layer using aiohttp.")
    (license license:isc)))

(define-public pantalaimon
  (let ((commit "257ef6a2e5e5668cd43347037c09ba036f91d997")
        (revision "0"))
    (package
      (name "pantalaimon")
      (version (git-version "0.10.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/matrix-org/pantalaimon")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1i18mjlc143d2xwlha09i5ny06vipmy8fii05427zq5vjz8rysgx"))))
      (build-system python-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'relax-requirements
              (lambda _
                (substitute* "setup.py"
                  ;; Newer version is packaged.
                  (("\"matrix-nio\\[e2e\\] >= 0\\.24, < 0\\.25\\.2\"")
                   "\"matrix-nio[e2e] >= 0.24, <= 0.25.2\""))))
            (add-after 'install 'install-doc
              (lambda _
                (with-directory-excursion "docs/man"
                  (let ((man (string-append #$output "/share/man")))
                    (install-file "panctl.1" (string-append man "/man1"))
                    (install-file "pantalaimon.5" (string-append man "/man5"))
                    (install-file "pantalaimon.8" (string-append man "/man8"))))))
            (replace 'check
              (lambda* (#:key tests? inputs outputs #:allow-other-keys)
                (when tests?
                  (add-installed-pythonpath inputs outputs)
                  (invoke "pytest" "-vv" "tests"
                          ;; These tests hang.
                          "--ignore=tests/proxy_test.py"
                          "-k" "not test_start_loop")))))))
      (native-inputs
       (list python-aioresponses
             python-faker
             python-pytest
             python-pytest-aiohttp
             python-pytest-asyncio))
      (propagated-inputs
       (list python-aiohttp
             python-attrs
             python-cachetools
             python-click
             python-dbus
             python-janus
             python-keyring
             python-logbook
             python-matrix-nio
             python-notify2
             python-peewee
             python-platformdirs
             python-prompt-toolkit
             python-pydbus
             python-pygobject))
      (home-page "https://github.com/matrix-org/pantalaimon")
      (synopsis "Matrix proxy daemon that adds E2E encryption capabilities")
      (description
       "Pantalaimon is an end-to-end encryption aware Matrix reverse proxy
daemon.  Pantalaimon acts as a good man in the middle that handles the
encryption for you.  Messages are transparently encrypted and decrypted for
clients inside of pantalaimon.")
      (license license:asl2.0))))
