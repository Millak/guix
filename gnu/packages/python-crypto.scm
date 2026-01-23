;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015-2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2020, 2022, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017, 2019, 2022, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016, 2017, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
;;; Copyright © 2019, 2025 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Justus Winter <justus@sequoia-pgp.org>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2023 Juliana Sims <juli@incana.org>
;;; Copyright © 2023, 2025 Zheng Junjie <z572@z572.online>
;;; Copyright © 2024 jgart <jgart@dismail.de>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages python-crypto)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1))

(define-public python-base58
  (package
    (name "python-base58")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "base58" version))
       (sha256
        (base32 "1317ly0db7nnjg5k58f6nqa0svfcvn446xd5bpiyi0bfbczwpl65"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pyhamcrest
           python-pytest
           python-pytest-benchmark
           python-setuptools
           python-wheel))
    (home-page "https://github.com/keis/base58")
    (synopsis "Base58 and Base58Check implementation")
    (description
     "Base58 and Base58Check implementation compatible with what is used by
the Bitcoin network.")
    (license license:expat)))

(define-public python-bcrypt
  (package
    (name "python-bcrypt")
    (version "3.2.2")    ;the latest not Rust version
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bcrypt" version))
       (sha256
        (base32 "1yvbdfmkssx1jf2lrhbs58xljmyi3p82r7rav82pf1bp44642g23"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-cffi))
    (home-page "https://github.com/pyca/bcrypt/")
    (synopsis "Modern password hashing library")
    (description
     "Bcrypt is a Python module which provides a password hashing method based
on the Blowfish password hashing algorithm, as described in
@url{http://static.usenix.org/events/usenix99/provos.html,\"A Future-Adaptable
Password Scheme\"} by Niels Provos and David Mazieres.")
    (license license:asl2.0)))

(define-public python-murmurhash
  (package
    (name "python-murmurhash")
    (version "1.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/explosion/murmurhash")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02ny4391kbrssq9bf7kq75615ragvbjhsqi9hgv7wiaiz6yai1k8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'fix-installation
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (with-directory-excursion
                  (string-append (site-packages inputs outputs) "/murmurhash")
                (delete-file-recursively "tests")
                (delete-file "mrmr.pyx")
                (for-each
                 (lambda (file)
                   (chmod file #o555))
                 (find-files "." "\\.so$")))))
          ;; XXX: Otherwise ModuleNotFoundError, and --pyargs doesn't seem
          ;; to fix the issue.
          (replace 'check
            (lambda args
              (copy-recursively "murmurhash/tests" "tests")
              (delete-file-recursively "murmurhash")
              (with-directory-excursion "tests"
                (apply (assoc-ref %standard-phases 'check) args)))))))
    (native-inputs
     (list python-cython
           python-murmurhash3
           python-pytest
           python-setuptools))
    (home-page "https://github.com/explosion/murmurhash")
    (synopsis "Cython bindings for MurmurHash2")
    (description
     "This package provides Cython bindings for MurmurHash2.")
    (license license:expat)))

(define-public python-passlib
  (package
    (name "python-passlib")
    (version "1.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "passlib" version))
       (sha256
        (base32 "015y5qaw9qnxr29lg60dml1g5rbqd4586wy5n8m41ib55gvm1zfy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-PYTHON_EGG_CACHE
            ;; Some tests require access to "$HOME/.cython".
            (lambda _
              (setenv "PYTHON_EGG_CACHE" "/tmp"))))))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-argon2-cffi python-bcrypt python-cryptography))
    (home-page "https://bitbucket.org/ecollins/passlib")
    (synopsis "Comprehensive password hashing framework")
    (description
     "Passlib is a password hashing library for Python 2 & 3, which provides
cross-platform implementations of over 30 password hashing algorithms, as well
as a framework for managing existing password hashes.  It's designed to be
useful for a wide range of tasks, from verifying a hash found in /etc/shadow,
to providing full-strength password hashing for multi-user application.")
    (license license:bsd-3)))

(define-public python-paramiko
  (package
    (name "python-paramiko")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "paramiko" version))
       (sha256
        (base32 "17ynnmagd44gpp76r26rz1kldb37f79hm4ibinlckj8c71xz09ba"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-icecream
           python-pytest
           python-pytest-relaxed
           python-setuptools))
    (propagated-inputs
     (list python-bcrypt
           python-cryptography
           python-invoke
           python-pyasn1
           python-pynacl))
    (home-page "https://www.paramiko.org/")
    (synopsis "SSHv2 protocol library")
    (description "Paramiko is a python implementation of the SSHv2 protocol,
providing both client and server functionality.  While it leverages a Python C
extension for low level cryptography (PyCrypto), Paramiko itself is a pure
Python interface around SSH networking concepts.")
    (license license:lgpl2.1+)))

(define-public python-ecdsa
  (package
    (name "python-ecdsa")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ecdsa" version))
       (sha256
        (base32 "1y3bmx6aw5klx143jas3czwbsfvr5d3fs8gm1bfh16b5k48svsk0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Test failes with error: AssertionError: KeyboardInterrupt not raised.
      #:test-flags #~(list "-k" "not test_multithreading_with_interrupts")))
    (native-inputs
     (list openssl
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-six))
    (home-page "https://github.com/warner/python-ecdsa")
    (synopsis "ECDSA cryptographic signature library (pure python)")
    (description
     "This is an easy-to-use implementation of ECDSA cryptography (Elliptic
Curve Digital Signature Algorithm), implemented purely in Python.  With this
library, you can quickly create key pairs (signing key and verifying key), sign
messages, and verify the signatures.  The keys and signatures are very short,
making them easy to handle and incorporate into other protocols.")
    (license license:expat)))

(define-public python-kerberos
  (package
    (name "python-kerberos")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kerberos" version))
       (sha256
        (base32 "0b0a8rrwgfjrslz3jd3r5l7vr7jx5bc17sq0dbwn002f58a4dl6d"))))
    (build-system pyproject-build-system)
    (arguments
     ;; No tests in PyPI, provided in Git but all of them require networking.
     (list #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (inputs
     (list mit-krb5))
    (home-page "https://github.com/apple/ccs-pykerberos")
    (synopsis "Python Kerberos library used by CalendarServer")
    (description
     "This Python package is a high-level wrapper for Kerberos (GSSAPI)
operations.  The goal is to avoid having to build a module that wraps the
entire Kerberos.framework, and instead offer a limited set of functions that
do what is needed for client/server Kerberos authentication based on
<http://www.ietf.org/rfc/rfc4559.txt>.")
    (license license:asl2.0)))

(define-public python-keyring
  (package
    (name "python-keyring")
    (version "23.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "keyring" version))
       (sha256
        (base32
         "19f4jpsxng9sjfqi8ww5hgg196r2zh1zb8g71wjr1xa27kc1vc39"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "-c" "/dev/null") ;avoid extra test dependencies
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'workaround-test-failure
            (lambda _
              ;; Workaround a failure in the test_entry_point test (see:
              ;; https://github.com/jaraco/keyring/issues/526).
              (delete-file-recursively "keyring.egg-info"))))))
    (native-inputs
     (list python-toml
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-jaraco-classes
           python-secretstorage))
    (home-page "https://github.com/jaraco/keyring")
    (synopsis "Store and access your passwords safely")
    (description
     "The Python keyring lib provides a easy way to access the system keyring
service from python.  It can be used in any application that needs safe
password storage.")
    ;; "MIT" and PSF dual license
    (license license:x11)))

(define-public python-keyrings-alt
  (package
    (name "python-keyrings-alt")
    (version "5.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keyrings_alt" version))
        (sha256
         (base32 "1yv9gnmkw6kpsjgnid0k1qcd49n9csqcvf02cl88bcf8knz7w2cg"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-keyring
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-jaraco-classes
           python-jaraco-context))
    (home-page "https://github.com/jaraco/keyrings.alt")
    (synopsis "Alternate keyring implementations")
    (description
     "Keyrings in this package may have security risks or other implications.
These backends were extracted from the main keyring project to make them
available for those who wish to employ them, but are discouraged for general
production use.  Include this module and use its backends at your own risk.")
    (license license:expat)))

(define-public python-blake3
  (package
    (name "python-blake3")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "blake3" version))
       (sha256
        (base32 "1p6z6jfk8n1lshz4cp6dgz2i8zmqdxwr8d9m86ypp3m1kp70k5xk"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:imported-modules `(,@%pyproject-build-system-modules
                           ,@%cargo-build-system-modules)
      #:modules `((guix build cargo-build-system)
                  ((guix build pyproject-build-system) #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'build-python-module
            (assoc-ref py:%standard-phases 'build))
          (add-after 'build-python-module 'install-python-module
            (assoc-ref py:%standard-phases 'install)))))
    (native-inputs
     (list python-wrapper
           maturin))
    (inputs (cargo-inputs 'python-blake3))
    (home-page "https://github.com/oconnor663/blake3-py")
    (synopsis "Python bindings for the Rust blake3 crate")
    (description "This package provides Python bindings for the Rust crate of
blake3, a cryptographic hash function.")
    ;; This work is released into the public domain with CC0
    ;; 1.0. Alternatively, it is licensed under the Apache License 2.0.
    (license (list license:asl2.0 license:cc0))))

(define-public python-certauth
  (package
    (name "python-certauth")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "certauth" version))
       (sha256
        (base32
         "1yxqfb5131wahjyw9pxz03bq476rcfx62s6k53xx4cqbzzgdaqkq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    (list
                     ;; Those tests uses PKCS12, which has been removed in
                     ;; pyopenssl 23.3.0:
                     "not test_custom_not_before_not_after"
                     "test_ca_cert_in_mem")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-test
            (lambda _
              ;; Newer PyOpenSSL no longer separates extensions with
              ;; newline (this can be removed for >1.3.0).
              (substitute* "test/test_certauth.py"
                (("7334\\\\n, DNS") "7334, DNS")))))))
    (propagated-inputs
     (list python-pyopenssl python-tldextract))
    (native-inputs
     (list nss-certs-for-test python-pytest python-setuptools))
    (home-page "https://github.com/ikreymer/certauth")
    (synopsis "Certificate authority creation tool")
    (description "This package provides a small library, built on top of
pyOpenSSL, which allows for creating a custom certificate authority (CA)
certificate, and generating on-demand dynamic host certs using that CA
certificate.  It is most useful for use with a man-in-the-middle HTTPS proxy,
for example, for recording or replaying web content.")
    (license license:expat)))

(define-public python-certifi
  (package
    (name "python-certifi")
    (version "2025.06.15")
    (source
     (origin
       (method git-fetch)   ; no tests in PyPI package
       (uri (git-reference
             (url "https://github.com/certifi/python-certifi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yyy3c64xbwfg2b8hi1fd6vwh56fzc90jfy3czimv5i91gwrl7ba"))
       (snippet #~(begin
                    (delete-file "certifi/cacert.pem")
                    (delete-file "certifi/core.py")
                    (with-output-to-file "certifi/core.py"
                      (lambda _
                        (display
                         "\"\"\"
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
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://certifi.io/")
    (synopsis "Python CA certificate bundle")
    (description
     "Certifi is a Python library that contains a CA certificate bundle, which
is used by the Requests library to verify HTTPS requests.")
    (license license:asl2.0)))

(define-public python-cryptography-vectors
  (package
    (name "python-cryptography-vectors")
    (version "44.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography_vectors" version))
       (sha256
        (base32
         "1aw06msy65rs27yxfp4xlwfq432ny1af5cx8s7zsbfa5div2hqhh"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))  ; No tests included.
    (native-inputs
     (list python-flit-core))
    (home-page "https://github.com/pyca/cryptography")
    (synopsis "Test vectors for the cryptography package")
    (description
     "This package contains test vectors for the cryptography package.")
    ;; Distributed under either BSD-3 or ASL2.0
    (license (list license:bsd-3 license:asl2.0))))

(define-public python-cryptography
  (package
    (name "python-cryptography")
    (version "44.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography" version))
       (sha256
        (base32
         "00is2nzcl2pyhr90llga5mnbw3rvakn75rq10x1r6hhb6i7q6knd"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (for-each delete-file
                           (find-files "." "Cargo\\.lock$"))
                 (substitute* "pyproject.toml"
                   (("locked = true") "offline = true"))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules
                           ,@%pyproject-build-system-modules)
      #:modules '(((guix build cargo-build-system) #:prefix cargo:)
                  (guix build pyproject-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-cargo-build-system
            (lambda args
              (for-each
               (lambda (phase)
                 (format #t "Running cargo phase: ~a~%" phase)
                 (apply (assoc-ref cargo:%standard-phases phase)
                        #:cargo-target #$(cargo-triplet)
                        args))
               '(unpack-rust-crates
                 configure
                 check-for-pregenerated-files
                 patch-cargo-checksums)))))))
    (native-inputs
     (append
      (list python-certifi
            python-cffi
            python-click
            python-cryptography-vectors
            python-pretend
            python-pytest
            python-pytest-benchmark
            python-pytest-randomly
            python-pytest-xdist
            python-setuptools
            rust
            `(,rust "cargo"))
      (or (and=> (%current-target-system)
                 (compose list make-rust-sysroot))
          '())))
    (inputs
     (cons* maturin openssl (cargo-inputs 'python-cryptography)))
    (propagated-inputs (list python-cffi))
    (home-page "https://github.com/pyca/cryptography")
    (synopsis "Cryptographic recipes and primitives for Python")
    (description
     "@code{cryptography} is a package which provides cryptographic recipes
and primitives to Python developers.  It aims to be the “cryptographic
standard library” for Python.  The package includes both high level recipes,
and low level interfaces to common cryptographic algorithms such as symmetric
ciphers, message digests and key derivation functions.")
    ;; Distributed under either BSD-3 or ASL2.0
    (license (list license:bsd-3 license:asl2.0))))

(define-public python-pyopenssl
  (package
    (name "python-pyopenssl")
    (version "24.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyopenssl" version))
       (sha256
        (base32
         "0dmv720kn5ws7bs1rkn59qmhzv5wxkkgriampi34g0vxawcs1xs9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; PyOpenSSL runs tests against a certificate with a fixed
                ;; expiry time.  To ensure successful builds in the future,
                ;; set the time to roughly the release date.
                (invoke "faketime" "2024-07-20" "pytest" "-vv"
                        "--deselect"
                        ;; This test seems to fail when using faketime, at
                        ;; least on aarch64-linux with OSError: [Errno 22]
                        ;; Invalid argument
                        "tests/test_ssl.py::TestDTLS::test_timeout"
                        "-k"
                        ;; This test tries to look up certificates from
                        ;; the compiled-in default path in OpenSSL, which
                        ;; does not exist in the build environment.
                        "not test_fallback_default_verify_paths ")))))))
    (propagated-inputs (list python-cryptography))
    (inputs (list openssl))
    (native-inputs
     (list libfaketime
           python-pretend
           python-pytest
           python-pytest-rerunfailures
           python-setuptools))
    (home-page "https://github.com/pyca/pyopenssl")
    (synopsis "Python wrapper module around the OpenSSL library")
    (description "PyOpenSSL is a high-level wrapper around a subset of the
OpenSSL library.")
    (properties `((updater-extra-inputs . ("openssl"))
                  (updater-extra-native-inputs . ("libfaketime"))))
    (license license:asl2.0)))

(define-public python-ed25519
  (package
    (name "python-ed25519")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/warner/python-ed25519")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0adxfm12wygh2gdsn83xmp1sw7w96ni7mr7v3z3y6q0mvh9n5x0p"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Use custom test back-end on python-team branch.
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests? (invoke "python" "test_ed25519_kat.py")))))))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/warner/python-ed25519")
    (synopsis "Ed25519 public-key signatures")
    (description
     "This package provides Ed25519 public-key signatures in Python.")
    (license license:expat)))

(define-public python-axolotl-curve25519
  (package
    (name "python-axolotl-curve25519")
    (version "0.4.1.post2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-axolotl-curve25519" version))
       (sha256
        (base32
         "18v3rfyv7xi26fb97nw1xc0l6x8wi0i4xj8dlq4gblpbjxiac187"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f                       ; No tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-gcc-14-strictness
            (lambda _
              (setenv "CFLAGS"
                      (string-append "-g -O2 -Wno-error=int-conversion")))))))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/tgalal/python-axolotl-curve25519")
    (synopsis "Python wrapper for curve25519 library")
    (description "This is a python wrapper for the curve25519 library
with ed25519 signatures.  The C code was pulled from
libaxolotl-android.  At the moment this wrapper is meant for use by
python-axolotl.")
    (license (list license:gpl3    ; Most files
                   license:bsd-3)))) ; curve/curve25519-donna.c

(define-public python-omemo-dr
  (package
    (name "python-omemo-dr")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://dev.gajim.org/gajim/omemo-dr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gx0znbfvs8jg9s754hha81l8wpghswkfsqx2jzpgv6gigf3sm8z"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-backend #~'unittest))
    (propagated-inputs
     (list python-cryptography python-protobuf-5 python-setuptools))
    (home-page "https://dev.gajim.org/gajim/omemo-dr")
    (synopsis "OMEMO cryptography library")
    (description "OMEMO cryptography library that was forked from python-axolotl.")
    (license license:gpl3)))

(define-public python-oscrypto
  (package
    (name "python-oscrypto")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/wbond/oscrypto/")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1v5wkmzcyiqy39db8j2dvkdrv2nlsc48556h73x4dzjwd6kg4q0a"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'custom
      #:test-flags
      #~(list "run.py"
              (format #f "use_openssl=~a,~a"
                      (search-input-file %build-inputs "/lib/libcrypto.so")
                      (search-input-file %build-inputs "/lib/libssl.so"))
              "skip_internet=true"
              "tests")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-envs
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv
               "SSL_CERT_FILE"
               (search-input-file
                inputs "/etc/ssl/certs/ca-certificates.crt")))))))
    (propagated-inputs (list python-asn1crypto))
    (native-inputs (list nss-certs-for-test openssl python-setuptools))
    (home-page "https://github.com/wbond/oscrypto/")
    (synopsis "Compiler-free Python encryption library")
    (description
     "@code{Python-oscrypto} is a compilation-free encryption library which
integrates with the encryption library that is part of the operating system.
It supports TLS (SSL) sockets, key generation, encryption, decryption,
signing, verification and KDFs using the OS crypto libraries.")
    (license license:expat)))

(define-public python-pyaes
  ;; XXX: Last updated in 2017.
  (package
    (name "python-pyaes")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyaes" version))
       (sha256
        (base32 "13vdaff15k0jyfcss4b4xvfgm8xyv0nrbyw5n1qc7lrqbi0b3h82"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))         ;no tests in PyPI
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/ricmoo/pyaes")
    (synopsis "Implementation of AES in Python")
    (description "This package contains a pure-Python implementation of the
AES block cipher algorithm and the common modes of operation (CBC, CFB, CTR,
ECB and OFB).")
    (license license:expat)))

(define-public python-asn1crypto
  (package
    (name "python-asn1crypto")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wbond/asn1crypto")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qsg06qrqnzixmrm65ibg503y2gffd675h1si4jgh92s315w1jrk"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-backend #~'unittest))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/wbond/asn1crypto")
    (synopsis "ASN.1 parser and serializer in Python")
    (description
     "@code{asn1crypto} is an ASN.1 parser and serializer with definitions for
private keys, public keys, certificates, CRL, OCSP, CMS, PKCS#3, PKCS#7, PKCS#8,
PKCS#12, PKCS#5, X.509 and TSP.")
    (license license:expat)))

(define-public python-pynacl
  (package
    (name "python-pynacl")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyNaCl" version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled libsodium.
           (delete-file-recursively "src/libsodium")))
       (sha256
        (base32
         "1fi0jbxhh3svajzldlb6gj5sr5a48v11xlmx0wb831db167l9iwa"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-before 'build 'use-system-sodium
           (lambda _
             (setenv "SODIUM_INSTALL" "system"))))))
    (native-inputs
     (list python-hypothesis python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-cffi python-six libsodium))
    (home-page "https://github.com/pyca/pynacl/")
    (synopsis "Python bindings to libsodium")
    (description
     "PyNaCl is a Python binding to libsodium, which is a fork of the
Networking and Cryptography library.  These libraries have a stated goal
of improving usability, security and speed.")
    (license license:asl2.0)))

(define-public python-crcmod
  (package
    (name "python-crcmod")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "crcmod" version))
              (sha256
               (base32
                "07k0hgr42vw2j92cln3klxka81f33knd7459cn3d8aszvfh52w6w"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'custom
      #:test-flags #~(list "-m" "crcmod.test")))
    (native-inputs (list python-setuptools))
    (synopsis "CRC generator for Python")
    (description "Python module for generating objects that compute the
Cyclic Redundancy Check.")
    (home-page "https://crcmod.sourceforge.net/")
    (license license:expat)))

(define-public python-blurhash
  (package
    (name "python-blurhash")
    (version "1.1.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/halcy/blurhash-python")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "16xcjxiya773fp7bxs9i6y1wsjx88pdvlwj6bqyy9vf3ckcffcwm"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list "-c" "/dev/null")))
    (native-inputs
     (list python-numpy python-pillow python-pytest python-setuptools))
    (home-page "https://github.com/halcy/blurhash-python")
    (synopsis "Pure-Python implementation of the blurhash algorithm")
    (description
     "This package provides a pure Python implementation of the blurhash
algorithm.")
    (license license:expat)))

(define-public python-ecpy
  (package
    (name "python-ecpy")
    (version "1.2.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cslashm/ECPy")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0p642bpqicxjkwqk4q46wqkbxhad1qiir6xz4w7xx0d4cdq7yps8"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;no tests
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/cslashm/ECPy")
    (synopsis "Pure Python Elliptic Curve Library")
    (description "This package provides a Elliptic Curve Library in pure
Python.")
    (license license:asl2.0)))

(define-public python-josepy
  (package
    (name "python-josepy")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/certbot/josepy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zplnfrmc4nps9lgl3fz434ja7lmi9v8waydflzvpi75kf5fqxc1"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core python-pytest))
    (propagated-inputs (list python-cryptography))
    (home-page "https://github.com/certbot/josepy")
    (synopsis "JOSE protocol implementation in Python")
    (description
     "This package provides a Python implementation of the JOSE protocol
(Javascript Object Signing and Encryption).")
    (license license:asl2.0)))

(define pycryptodome-unbundle-tomcrypt-snippet
  #~(begin
      ;; Unbundle libtomcrypt.
      (delete-file-recursively "src/libtom")
      (substitute* "src/DES.c"
        (("#include \"libtom/tomcrypt_des.c\"")
         "#include <tomcrypt.h>"))
      (substitute* "setup.py"
        (("include_dirs=\\['src/', 'src/libtom/'\\]")
         ;; FIXME: why does '-ltomcrypt' need to be added
         ;; manually, even when 'tomcrypt' is added to 'libraries'?
         ;; This behaviour is not documented at
         ;; <https://docs.python.org/3/extending/building.html>.
         "include_dirs=['src/'], libraries=['tomcrypt', 'tommath'],
 extra_link_args=['-ltomcrypt', '-ltommath']"))))

(define-public python-pycryptodome
  (package
    (name "python-pycryptodome")
    (version "3.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Legrandin/pycryptodome")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j1dk55y0q7gqmig1l5b8774w63w2i7qrii1xzkpzz0c3i229i67"))
       (modules '((guix build utils)))
       (snippet pycryptodome-unbundle-tomcrypt-snippet)))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 42912 passed
      #:test-backend #~'custom
      ;; As seen in
      ;; <https://github.com/Legrandin/pycryptodome/blob/v3.23.0/INSTALL.rst>.
      #:test-flags
      #~(list "-m" "Crypto.SelfTest")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-gmp-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "lib/Crypto/Math/_IntegerGMP.py"
                (("load_lib\\(\"gmp\"")
                 (format #f "load_lib(~s"
                         (search-input-file inputs "/lib/libgmp.so.10")))))))))
    (native-inputs
     (list python-pycryptodome-test-vectors
           python-setuptools))
    (inputs
     (list gmp
           libtomcrypt
           libtommath))
    (home-page "https://www.pycryptodome.org")
    (synopsis "Low-level cryptographic Python library")
    (description
     "PyCryptodome is a self-contained Python package of low-level
cryptographic primitives.  It's not a wrapper to a separate C library like
OpenSSL.  To the largest possible extent, algorithms are implemented in pure
Python.  Only the pieces that are extremely critical to performance (e.g.,
block ciphers) are implemented as C extensions.

You are expected to have a solid understanding of cryptography and security
engineering to successfully use these primitives.  You must also be able to
recognize that some are obsolete (e.g., TDES) or even insecure (RC4).

It provides many enhancements over the last release of PyCrypto (2.6.1):

@itemize
@item Authenticated encryption modes (GCM, CCM, EAX, SIV, OCB)
@item Accelerated AES on Intel platforms via AES-NI
@item First-class support for PyPy
@item Elliptic curves cryptography (NIST P-256 curve only)
@item Better and more compact API (nonce and iv attributes for ciphers,
automatic generation of random nonces and IVs, simplified CTR cipher mode, and
more)
@item SHA-3 (including SHAKE XOFs) and BLAKE2 hash algorithms
@item Salsa20 and ChaCha20 stream ciphers
@item scrypt and HKDF
@item Deterministic (EC)DSA
@item Password-protected PKCS#8 key containers
@item Shamir’s Secret Sharing scheme
@item Random numbers get sourced directly from the OS (and not from a CSPRNG
in userspace)
@item Cleaner RSA and DSA key generation (largely based on FIPS 186-4)
@item Major clean-ups and simplification of the code base
@end itemize

This package provides drop-in compatibility with PyCrypto.  It is one of two
PyCryptodome variants, the other being python-pycryptodomex.")
    (license (list license:bsd-2
                   license:public-domain)))) ; code inherited from PyCrypto

(define-public python-pycryptodome-test-vectors
  (package
    (name "python-pycryptodome-test-vectors")
    (version "1.0.22")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycryptodome_test_vectors" version))
       (sha256
        (base32 "19bn19fpnkj39nki2dp8k26is5fpqa3q7agkhk9rvfqfvzvgx3nn"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ;no tests
    (native-inputs
     (list python-setuptools)) 
    (home-page "https://www.pycryptodome.org")
    (synopsis "Test vectors for PyCryptodome")
    (description
     "This package provides test vectors for @code{PyCryptodome}.")
    (license (list license:bsd-2
                   license:asl2.0))))

(define-public python-pycryptodomex
  (package/inherit python-pycryptodome
    (name "python-pycryptodomex")
    (arguments
     (substitute-keyword-arguments (package-arguments python-pycryptodome)
       ((#:test-flags _)
        #~(list "-m" "Cryptodome.SelfTest"))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'build 'set-separate-namespace
              (lambda _
                ;; This changes the build logic, see setup.py.
                (with-output-to-file ".separate_namespace"
                  (lambda _ (display "")))))))))
    (synopsis
     "Low-level cryptographic Python library independent of the old PyCrypto")
    (description
     "PyCryptodome is a self-contained Python package of low-level
cryptographic primitives.  It's not a wrapper to a separate C library like
OpenSSL.  To the largest possible extent, algorithms are implemented in pure
Python.  Only the pieces that are extremely critical to performance (e.g.,
block ciphers) are implemented as C extensions.

You are expected to have a solid understanding of cryptography and security
engineering to successfully use these primitives.  You must also be able to
recognize that some are obsolete (e.g., TDES) or even insecure (RC4).

It provides many enhancements over the last release of PyCrypto (2.6.1):

@itemize
@item Authenticated encryption modes (GCM, CCM, EAX, SIV, OCB)
@item Accelerated AES on Intel platforms via AES-NI
@item First-class support for PyPy
@item Elliptic curves cryptography (NIST P-256 curve only)
@item Better and more compact API (nonce and iv attributes for ciphers,
automatic generation of random nonces and IVs, simplified CTR cipher mode, and
more)
@item SHA-3 (including SHAKE XOFs) and BLAKE2 hash algorithms
@item Salsa20 and ChaCha20 stream ciphers
@item scrypt and HKDF
@item Deterministic (EC)DSA
@item Password-protected PKCS#8 key containers
@item Shamir’s Secret Sharing scheme
@item Random numbers get sourced directly from the OS (and not from a CSPRNG
in userspace)
@item Cleaner RSA and DSA key generation (largely based on FIPS 186-4)
@item Major clean-ups and simplification of the code base
@end itemize

PyCryptodomex is the stand-alone version of PyCryptodome that no longer
provides drop-in compatibility with PyCrypto.")))

(define-public python-m2crypto
  (package
    (name "python-m2crypto")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "M2Crypto" version))
       (sha256
        (base32 "1jyynaybncgjp8rndrwjpj5gnbrislngimf6ind9874h6052dwlr"))))
    (build-system pyproject-build-system)
    (arguments
     `(;; FIXME: Tests start failing with time due to date checks in TLS
       ;; certificates.
       #:tests? #f))
    (inputs (list openssl))
    (native-inputs (list swig python-setuptools python-wheel))
    (home-page "https://gitlab.com/m2crypto/m2crypto")
    (synopsis "Python crypto and TLS toolkit")
    (description "@code{M2Crypto} is a complete Python wrapper for OpenSSL
featuring RSA, DSA, DH, EC, HMACs, message digests, symmetric ciphers
(including AES); TLS functionality to implement clients and servers; HTTPS
extensions to Python's httplib, urllib, and xmlrpclib; unforgeable HMAC'ing
AuthCookies for web session management; FTP/TLS client and server; S/MIME;
M2Crypto can also be used to provide TLS for Twisted.  Smartcards supported
through the Engine interface.")
    (license license:expat)))

(define-public python-pykeepass
  (package
    (name "python-pykeepass")
    (version "4.1.1.post1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libkeepass/pykeepass")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ayvgklq3naydx01m7hyvf44ywczk5yzy2pk48bpfayl7bgk7q8d"))))
    (build-system pyproject-build-system)
    (arguments
     ;; tests: 115 passed
     (list #:test-backend #~'unittest))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-argon2-cffi
           python-construct
           python-lxml
           python-pycryptodomex
           python-pyotp))
    (home-page "https://github.com/libkeepass/pykeepass")
    (synopsis "Python library to interact with keepass databases")
    (description
     "This library allows you to write entries to a KeePass database.  It
supports KDBX3 and KDBX4.")
    ;; There are no copyright headers in the source code.  The LICENSE file
    ;; indicates GPL3.
    (license license:gpl3+)))

(define-public python-pylibscrypt
  (package
    (name "python-pylibscrypt")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jvarho/pylibscrypt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hshivwl3xznlqhrvwbylp40k1bfx4gnyzmxwldkwjhf1260zan1"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/jvarho/pylibscrypt")
    (synopsis "Scrypt for Python")
    (description
     "This package provides another Scrypt module for Python.")
    (license license:isc)))

(define-public python-libnacl
  (package
    (name "python-libnacl")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "libnacl" version))
       (sha256
        (base32
         "0q18j8kfibhi5qckakhf0b0psf8nkll91nfp3yqxkri9vykqshgk"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'locate-libsodium
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libnacl/__init__.py"
               (("ctypes\\.util\\.find_library\\('sodium'\\)")
                (string-append "'"
                               (search-input-file inputs "/lib/libsodium.so")
                               "'")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "unittest" "discover"
                       "--start-directory" "tests" "-v")))))))
    (native-inputs
     (list python-poetry-core))
    (inputs
     (list libsodium))
    (home-page "https://libnacl.readthedocs.org/")
    (synopsis "Python bindings for libsodium based on ctypes")
    (description "@code{libnacl} is used to gain direct access to the
functions exposed by @code{NaCl} library via @code{libsodium}.  It has
been constructed to maintain extensive documentation on how to use
@code{NaCl} as well as being completely portable.")
    (license license:asl2.0)))

(define-public python-pyotp
  (package
    (name "python-pyotp")
    (version "2.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pyauth/pyotp")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fx253649bvs2w31jdwnr51dm0b6v1zhp7mprnis37c57vj4qczz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "test.py")))
    (native-inputs
     (list python-setuptools
           python-pytest))
    (home-page "https://github.com/pyauth/pyotp")
    (synopsis "Python One Time Password Library")
    (description
     "PyOTP is a Python library for generating and verifying one-time
passwords.  It can be used to implement two-factor (2FA) or multi-factor
(MFA) authentication methods in web applications and in other systems that
require users to log in.")
    (license license:expat)))

;; XXX: This project was archived by the owner on Apr 20, 2025. It is now
;; read-only.  Consider to remove when starts failing to build and nothing
;; depends on it.
(define-public python-rsa
  (package
   (name "python-rsa")
   (version "4.9.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "rsa" version))
     (sha256
      (base32 "0xdfs3mhdb9wjpckj7i4zyv9sr82m7hk0mfkvw3lrnlpakdvzgg7"))))
   (build-system pyproject-build-system)
   (arguments
    (list #:tests? #f))         ;tests are not included in PyPI archive
   (native-inputs
    (list python-poetry-core))
   (propagated-inputs
    (list python-pyasn1))
   (home-page "https://stuvel.eu/rsa")
   (synopsis "Pure-Python RSA implementation")
   (description "Python-RSA is a pure-Python RSA implementation.  It supports
encryption and decryption, signing and verifying signatures, and key
generation according to PKCS#1 version 1.5.  It can be used as a Python
library as well as on the command line.")
   (license license:asl2.0)))

(define-public python-rsa-for-awscli-1
  (hidden-package
   (package
     (inherit python-rsa)
     (version "3.4.2")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rsa" version))
        (sha256
         (base32 "1dcxvszbikgzh99ybdc7jq0zb9wspy2ds8z9mjsqiyv3q884xpr5"))))
     (native-inputs
      (list python-setuptools)))))

(define-public python-scrypt
  (package
    (name "python-scrypt")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/holgern/py-scrypt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07mr8slhplk0pfji3pfb86281wf035c3vxg7w4g17vgry1l5fdg2"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (inputs (list openssl))
    (home-page "https://github.com/holgern/py-scrypt")
    (synopsis "Bindings for the scrypt key derivation function library")
    (description
     "This package is a set of Python bindings for the scrypt key derivation
function.")
    (license license:bsd-2)))

(define-public python-service-identity
  (package
    (name "python-service-identity")
    (version "24.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "service_identity" version))
       (sha256
        (base32 "02cfpry5alap6mf3ffq1gdq6s7a2cmgjqpb2bp6wcf8d7yhkns5q"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatch-fancy-pypi-readme
           python-hatch-vcs
           python-hatchling
           python-idna
           python-pytest))
    (propagated-inputs
     (list python-attrs
           python-cryptography
           python-pyasn1
           python-pyasn1-modules
           python-six))
    (home-page "https://service-identity.readthedocs.io/")
    (synopsis "Service identity verification for PyOpenSSL")
    (description
     "@code{service_identity} aspires to give you all the tools you need for
verifying whether a certificate is valid for the intended purposes.  In the
simplest case, this means host name verification.  However, service_identity
implements RFC 6125 fully and plans to add other relevant RFCs too.")
    (license license:expat)))

(define-public python-google-crc32c
  (package
    (name "python-google-crc32c")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/googleapis/python-crc32c")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "005ra4pfv71rq53198k7q6k63f529q3g6hkbxbwfcf82jr77hxga"))))
    (build-system pyproject-build-system)
    (inputs (list crc32c))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/googleapis/python-crc32c")
    (synopsis "Python wrapper of Google CRC32C")
    (description
     "This package provides a Python wrapper of the C library implementation
of the CRC32C hashing algorithm.")
    (license license:asl2.0)))

(define-public python-spake2
  (package
    (name "python-spake2")
    (version "0.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "spake2" version))
        (sha256
         (base32 "0d4kbaxi4cv8klyqh6yb0p0qiwfdwvczy1h2mzvmlfdcsnlc87s2"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools))
    (propagated-inputs (list python-cryptography))
    (home-page "https://github.com/warner/python-spake2")
    (synopsis "SPAKE2 password-authenticated key exchange in Python")
    (description "This package provides a Python implementation of the SPAKE2
Password-Authenticated Key Exchange algorithm.")
    (license license:expat)))

(define-public python-keyutils
  (package
    (name "python-keyutils")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "keyutils" version))
       (sha256
        (base32 "0lipygpzhwzzsq2k5imb1jgkmj8y4khxdwhzadjs3bd56g6bmkx9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'regenerate-c-file
            (lambda _
              (invoke "cython" "keyutils/_keyutils.pyx"))))))
    (native-inputs
     (list python-cython python-pytest python-setuptools))
    (inputs
     (list keyutils))
    (home-page "https://github.com/sassoftware/python-keyutils")
    (synopsis "Python bindings for keyutils")
    (description
     "This is a set of python bindings for keyutils, a key management suite
that leverages the infrastructure provided by the Linux kernel for safely
storing and retrieving sensitive information in your programs.")
    (license license:asl2.0)))

(define-public python-mcuboot-imgtool
  (package
    (name "python-mcuboot-imgtool")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mcu-tools/mcuboot")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m1csyvzq4jx81zg635ssy1n7sc0z539z0myh872ll3nwqx7wa0q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-broken-test
            (lambda _
              (substitute* "scripts/imgtool/keys/ed25519_test.py"
                (("raw_sign")
                 "sign_digest"))))
          (add-before 'build 'change-directory
            (lambda _
              (chdir "scripts"))))))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-click python-intelhex python-cryptography))
    (home-page "https://mcuboot.com")
    (synopsis "Tool to securely sign firmware images for booting by MCUboot")
    (description
     "MCUboot is a secure bootloader for 32-bit MCUs.  This package provides a
tool to securely sign firmware images for booting by MCUboot.")
    (license license:expat)))

(define-public python-ntlm-auth
  (package
    (name "python-ntlm-auth")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jborean93/ntlm-auth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00dpf5bfsy07frsjihv1k10zmwcyq4bvkilbxha7h6nlwpcm2409"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-requests python-setuptools python-wheel))
    (propagated-inputs (list python-cryptography))
    (home-page "https://github.com/jborean93/ntlm-auth")
    (synopsis "Calculates NTLM Authentication codes")
    (description
     "This library handles the low-level details of NTLM authentication for
use in authenticating with a service that uses NTLM.  It will create and parse
the 3 different message types in the order required and produce a base64
encoded value that can be attached to the HTTP header.

The goal of this library is to offer full NTLM support including signing and
sealing of messages as well as supporting MIC for message integrity and the
ability to customise and set limits on the messages sent.  Please see Features
and Backlog for a list of what is and is not currently supported.")
    (license license:expat)))

(define-public python-secretstorage
  (package
    (name "python-secretstorage")
    (version "3.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "SecretStorage" version))
        (sha256
         (base32 "0xxxxr77sgmjm1rqzdd1rkan9xg0qmv8awc1pb9adv39ycz560r4"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ; Tests require a running dbus service.
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-cryptography python-jeepney))
    (home-page "https://github.com/mitya57/secretstorage")
    (synopsis "Python bindings to FreeDesktop.org Secret Service API")
    (description
     "@code{python-secretstorage} provides a way for securely storing passwords
and other secrets.  It uses D-Bus Secret Service API that is supported by GNOME
Keyring (since version 2.30) and KSecretsService.  SecretStorage supports most
of the functions provided by Secret Service, including creating and deleting
items and collections, editing items, locking and unlocking collections
(asynchronous unlocking is also supported).")
    (license license:bsd-3)))

(define-public python-trustme
  (package
    (name "python-trustme")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trustme" version))
       (sha256
        (base32 "0zz49agi7hy82fbr03xs6k9vjgiy7v8qsp426cgv8bbzphmvla35"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling
           python-pyopenssl
           python-pytest
           python-service-identity))
    (propagated-inputs
     (list python-cryptography
           python-idna))
    (home-page "https://trustme.readthedocs.io")
    (synopsis "Fake a certificate authority for tests")
    (description
     "@code{trustme} is a tiny Python package that does one thing: it gives you
a fake certificate authority (CA) that you can use to generate fake TLS certs to
use in your tests.")
    ;; Either license applies.
    (license (list license:expat license:asl2.0))))

(define-public python-certipy
  (package
    (name "python-certipy")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "certipy" version))
       (sha256
        (base32 "0pm0kf079ws9s93vpkxnw3gj6a1hi34p34bicx69rqlyh7cg7wgy"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cryptography))
    (native-inputs (list python-pypa-build
                         python-flask
                         python-pytest
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/LLNL/certipy")
    (synopsis "Utility to create and sign CAs and certificates")
    (description
     "Certipy was made to simplify the certificate creation process.  To that
end, certipy exposes methods for creating and managing certificate authorities,
certificates, signing and building trust bundles.")
    (license license:bsd-3)))

(define-public python-jeepney
  (package
    (name "python-jeepney")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jeepney" version))
        (sha256
         (base32 "01jqrk7pn94i7bpmj834pjrw7id659gfag6wpbv04fcpap94izjy"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-async-timeout
           python-testpath
           python-trio
           python-pytest
           python-pytest-asyncio
           python-pytest-trio
           ;; For the build backend
           python-flit-core))
    (home-page "https://gitlab.com/takluyver/jeepney")
    (synopsis "Low-level, pure Python DBus protocol wrapper")
    (description
     "This is a low-level, pure Python DBus protocol client.  It has an
I/O-free core, and integration modules for different event loops.")
    (license license:expat)))

(define-public python-argon2-cffi
  (package
    (name "python-argon2-cffi")
    (version "25.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "argon2_cffi" version))
       (sha256
        (base32 "1ha62fg9blw38q3qayjid8608fp2a57fd81cpzic9x22ib6fajk9"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatch-fancy-pypi-readme
           python-hatch-vcs
           python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-argon2-cffi-bindings))
    (home-page "https://argon2-cffi.readthedocs.io/")
    (synopsis "Secure Password Hashes for Python")
    (description
     "Argon2 is a secure password hashing algorithm.  It is designed to have
both a configurable runtime as well as memory consumption.  This means that you
can decide how long it takes to hash a password and how much memory is required.")
    (license license:expat)))

(define-public python-argon2-cffi-bindings
  (package
    (name "python-argon2-cffi-bindings")
    (version "25.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "argon2_cffi_bindings" version))
       (sha256
        (base32 "179256zsrh5c51zmv9k1sc9p102j152nzxqgwhhdhmadxbkg6mxr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'use-system-argon2
            (lambda _
              (setenv "ARGON2_CFFI_USE_SYSTEM" "1"))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (inputs
     (list argon2))
    (propagated-inputs
     (list python-cffi))
    (home-page "https://github.com/hynek/argon2-cffi-bindings")
    (synopsis "Low-level CFFI bindings for Argon2")
    (description
     "argon2-cffi-bindings provides low-level CFFI bindings to the official
implementation of the Argon2 password hashing algorithm.")
    (license license:expat)))

(define-public python-privy
  (package
    (name "python-privy")
    (version "6.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               ;; Releases are untagged
               (url "https://github.com/ofek/privy")
               (commit "2838db3df239797c71bddacc48a4c49a83f35747")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1m32dh5fqc8cy7jyf1z5fs6zvmdkbq5fi98hr609gbl7s0l0y0i9"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools))
    (propagated-inputs (list python-argon2-cffi python-cryptography))
    (home-page "https://www.dropbox.com/developers")
    (synopsis "Library to password-protect your data")
    (description
     "Privy is a small and fast utility for password-protecting secret
data such as API keys, cryptocurrency wallets, or seeds for digital
signatures.")
    (license (list license:expat license:asl2.0)))) ; dual licensed

(define-public python-pgpy
  (package
    (name "python-pgpy")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "PGPy" version))
        (sha256
         (base32 "10w3h934fi1ijx72ppn67a50yhkf8n1db6xx02gk2fjc7wsjx717"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           ;; All broken tests are in this file.
           ;; They fail with ValueError: key_size must be at least 1024-bits.
           #~(list "--ignore" "tests/test_10_exceptions.py")))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-cryptography python-pyasn1))
    (home-page "https://github.com/SecurityInnovation/PGPy")
    (synopsis "Python implementation of OpenPGP")
    (description
     "Currently, PGPy can load keys and signatures of all kinds in both ASCII
armored and binary formats.

It can create and verify RSA, DSA, and ECDSA signatures, at the moment.  It
can also encrypt and decrypt messages using RSA and ECDH.")
    (license license:bsd-3)))

(define-public python-pyu2f
  (package
    (name "python-pyu2f")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/pyu2f/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jlpplw7hlrh8bgmp37g18panij0p7ism6r4d981my4dc73lbwik"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; This test requires the fakefs package.
      #:test-flags #~(list "--ignore=pyu2f/tests/hid/linux_test.py")))
    (native-inputs (list python-mock python-pytest python-setuptools))
    (propagated-inputs (list python-six))
    (home-page "https://github.com/google/pyu2f/")
    (synopsis "U2F host library for interacting with a U2F device over USB")
    (description
     "Pyu2f is a Python-based U2F host library.  It provides functionality for
interacting with a U2F device over USB.")
    (license license:asl2.0)))

(define-public python-sop
  (package
    (name "python-sop")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.com/dkg/python-sop")
              (commit (string-append "sop-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sin5miiiqgww0s52jz098x44nbnx003vfd4jn55bs5mgca60lll"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; XXX: No tests upstream.
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://gitlab.com/dkg/python-sop")
    (synopsis "Stateless OpenPGP Command-Line Interface")
    (description
     "The Stateless OpenPGP Command-Line Interface (or sop) is a
specification that encourages OpenPGP implementors to provide a common,
relatively simple command-line API for purposes of object security.

This Python module helps implementers build such a CLI from any implementation
accessible to the Python interpreter.

It does not provide such an implementation itself -- this is just the
scaffolding for the command line, which should make it relatively easy to
supply a handful of python functions as methods to a class.")
    (license license:expat))) ; MIT license

(define-public python-starkbank-ecdsa
  (package
    (name "python-starkbank-ecdsa")
    (version "2.2.0")
    (home-page "https://github.com/starkbank/ecdsa-python")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01zy16csrbyfhjl0kay9rbpbwc3dpr7kh2qkrbdy5a1n644fbahx"))))
    (arguments
     (list
      #:test-backend #~'unittest
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-broken-test
            (lambda _
              (delete-file "tests/testOpenSSL.py"))))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools))
    (synopsis "Python ECDSA library")
    (description "This package provides a Python ECDSA library, optimized for
speed but without C extensions.")
    (license license:expat)))

(define-public python-zxcvbn
  (package
    (name "python-zxcvbn")
    (version "4.5.0")
    (source
     (origin
       (method git-fetch) ;for tests
       (uri (git-reference
             (url "https://github.com/dwolfhub/zxcvbn-python")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x39yi4zc1n6mjjk3rx32ick0hysyi9hv9ln69apcch4jf84j9fi"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/dwolfhub/zxcvbn-python")
    (synopsis "Realistic password strength estimator Python library")
    (description
     "This is a Python implementation of the @code{zxcvbn} library created at
Dropbox.  The original library, written for JavaScript, can be found
@url{https://github.com/dropbox/zxcvbn, here}.  This port includes features
such as:
@enumerate
@item Accepts user data to be added to the dictionaries that are tested
against (name, birthdate, etc.)
@item Gives a score to the password, from 0 (terrible) to 4 (great).
@item Provides feedback on the password and ways to improve it.
@item Returns time estimates on how long it would take to guess the password
in different situations.
@end enumerate")
    (license license:expat)))

;; XXX: Not maintained since 2016.
(define-public python-pydes
  (package
    (name "python-pydes")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyDes" version))
       (sha256
        (base32 "04lh71f47y04vspfrdrq6a0hn060ibxvdp5z1pcr0gmqs8hqxaz2"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ;no tests in PyPI, I could not fine Git
    (native-inputs (list python-setuptools))
    (home-page "http://twhiteman.netfirms.com/des.html") ;XXX: Dead link
    (synopsis
     "Pure python implementation of the DES and TRIPLE DES encryption algorithms")
    (description
     "This package provides a pure Python implementation of the DES and
TRIPLE DES encryption algorithms.")
    (license license:public-domain)))
