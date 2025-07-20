;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2024 normally_js <normally_js@posteo.net>
;;; Copyright © 2024, 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 TakeV <takev@disroot.org>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Vinicius Monego <monego@posteo.net>
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

(define-module (gnu packages magic-wormhole)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix build-system python) #:select (pypi-uri))
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public magic-wormhole-mailbox-server
  (package
    (name "magic-wormhole-mailbox-server")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "magic-wormhole-mailbox-server" version))
       (sha256
        (base32
         "18cc00dfmri648psx6kzqlbmhvax0h1gbnw1frjh8ci9f8va01x0"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-mock
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-attrs
           python-autobahn
           python-idna
           python-service-identity
           python-treq))
    (home-page "https://github.com/magic-wormhole/magic-wormhole-mailbox-server")
    (synopsis "Magic-Wormhole central mailbox server")
    (description
     "This package provides the main server that Magic-Wormhole clients
connect to.  The server performs store-and-forward delivery for small
key-exchange and control messages.  Bulk data is sent over a direct TCP
connection, or through a transit-relay.")
    (license license:expat)))

(define-public magic-wormhole-transit-relay
  (package
    (name "magic-wormhole-transit-relay")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "magic-wormhole-transit-relay" version))
       (sha256
        (base32
         "134yh50xyrr2jrws3w4q1gy660l0wnswj78ifxn2c48vl9fq6bci"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-docs
            (lambda _
              (let* ((out #$output)
                     (docs (string-append out "/share/doc/magic-wormhole-transit-relay")))
                (for-each (lambda (file)
                            (install-file file docs))
                          (find-files "docs/")))))
          (add-before 'check 'fix-test
            (lambda _
              ;; Fix for failing test (test_buffer_fills).
              (substitute* "src/wormhole_transit_relay/test/test_backpressure.py"
                (("reactor\\.spawnProcess\\(proto, exe, args\\)")
                 "reactor.spawnProcess(proto, exe, args, None)")))))))
    (native-inputs
     (list python-mock
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     ;; python-service-identity should be propagated from python-twisted.
     (list python-autobahn python-service-identity python-twisted))
    (home-page "https://github.com/magic-wormhole/magic-wormhole-transit-relay")
    (synopsis "Magic-Wormhole relay server")
    (description
     "This package provides the Magic-Wormhole Transit Relay server, which
helps clients establish bulk-data transit connections even when both are
behind NAT boxes.  Each side makes a TCP connection to this server and
presents a handshake.  Two connections with identical handshakes are glued
together, allowing them to pretend they have a direct connection.")
    (license license:expat)))

(define-public magic-wormhole
  (package
    (name "magic-wormhole")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "magic_wormhole" version))
       (sha256
        (base32
         "0wbwz5kzqgr4352xbp08z8syg9cl8dkqy8rsa3y4rzq9ry5agd5j"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-k" (string-append
                                 "not test_receive_send"
                                 " and not test_receive_receive"))
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX I can't figure out how to build the docs properly.
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34515#101
          (add-after 'install 'install-docs
            (lambda _
              (install-file "docs/wormhole.1"
                            (string-append #$output "/share/man/man1")))))))
    (native-inputs
     (list python-mock
           python-pytest
           python-pytest-twisted
           python-setuptools
           python-wheel
           magic-wormhole-mailbox-server
           magic-wormhole-transit-relay))
    (propagated-inputs
     (list python-attrs
           python-autobahn
           python-automat
           python-click
           python-humanize
           python-iterable-io
           python-noiseprotocol
           python-pynacl
           python-qrcode
           python-spake2
           python-tqdm
           python-twisted
           python-txtorcon
           python-zipstream-ng))
    (home-page "https://github.com/magic-wormhole/magic-wormhole")
    (synopsis "Securely transfer data between computers")
    (description
     "Magic-Wormhole is a library and a command-line tool named wormhole,
which makes it possible to securely transfer arbitrary-sized files and
directories (or short pieces of text) from one computer to another.  The two
endpoints are identified by using identical \"wormhole codes\": in general,
the sending machine generates and displays the code, which must then be typed
into the receiving machine.

The codes are short and human-pronounceable, using a phonetically-distinct
wordlist.  The receiving side offers tab-completion on the codewords, so
usually only a few characters must be typed.  Wormhole codes are single-use
and do not need to be memorized.")
   (license license:expat)))
