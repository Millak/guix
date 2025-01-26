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
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Justus Winter <justus@sequoia-pgp.org>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2023 Juliana Sims <juli@incana.org>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 jgart <jgart@dismail.de>
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
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
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
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bcrypt" version))
       (sha256
        (base32 "0agvzdn7r7jx5y4scl5gjmrmr6njvizwmr9n7h1kmaahdrrc34sv"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pycparser python-pytest))
    (propagated-inputs
     (list python-cffi python-six))
    (home-page "https://github.com/pyca/bcrypt/")
    (synopsis
     "Modern password hashing library")
    (description
     "Bcrypt is a Python module which provides a password hashing method based
on the Blowfish password hashing algorithm, as described in
@url{http://static.usenix.org/events/usenix99/provos.html,\"A Future-Adaptable
Password Scheme\"} by Niels Provos and David Mazieres.")
    (license license:asl2.0)))

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
    (build-system python-build-system)
    (native-inputs
     (list python-nose))
    (propagated-inputs
     (list python-bcrypt))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-PYTHON_EGG_CACHE
           ;; Some tests require access to "$HOME/.cython".
           (lambda _ (setenv "PYTHON_EGG_CACHE" "/tmp"))))))
    (home-page "https://bitbucket.org/ecollins/passlib")
    (synopsis "Comprehensive password hashing framework")
    (description
     "Passlib is a password hashing library for Python 2 & 3, which provides
cross-platform implementations of over 30 password hashing algorithms, as well
as a framework for managing existing password hashes.  It's designed to be
useful for a wide range of tasks, from verifying a hash found in /etc/shadow,
to providing full-strength password hashing for multi-user application.")
    (license license:bsd-3)))

(define-public python-pyblake2
  (package
    (name "python-pyblake2")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyblake2" version))
       (sha256
        (base32
         "0gz9hgznv5zw4qjq43xa56y0yikimx30gffvibxzm0nv5sq7xk2w"))))
    (build-system python-build-system)
    (home-page "https://github.com/dchest/pyblake2")
    (synopsis "BLAKE2 hash function for Python")
    (description "BLAKE2 is a cryptographic hash function, which offers
stronger security while being as fast as MD5 or SHA-1, and comes in two
flavors: @code{BLAKE2b}, optimized for 64-bit platforms and produces digests
of any size between 1 and 64 bytes, and @code{BLAKE2s}, optimized for 8- to
32-bit platforms and produces digests of any size between 1 and 32 bytes.

This package provides a Python interface for BLAKE2.")
    ;; The COPYING file declares it as public domain, with the option to
    ;; alternatively use and redistribute it under a variety of permissive
    ;; licenses. cc0 is explicitly mentioned in setup.py and pyblake2module.c.
    (license (list license:public-domain license:cc0))))

(define-public python-paramiko
  (package
    (name "python-paramiko")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "paramiko" version))
       (sha256
        (base32 "0941n85xi32kvrh2mxppga527a0jz2iz2c99lpfwwmagv90fa4dd"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-icecream
           python-pytest-relaxed
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-cryptography
           python-bcrypt
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kerberos" version))
       (sha256
        (base32
         "19663qxmma0i8bfbjc2iwy5hgq0g4pfb75r023v5dps68zfvffgh"))))
    (build-system python-build-system)
    (inputs
     (list mit-krb5))
    (home-page "https://github.com/apple/ccs-pykerberos")
    (synopsis
     "Python Kerberos library used by CalendarServer")
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
     (list python-importlib-metadata
           python-jaraco-classes
           python-secretstorage))
    (home-page "https://github.com/jaraco/keyring")
    (synopsis "Store and access your passwords safely")
    (description
     "The Python keyring lib provides a easy way to access the system keyring
service from python.  It can be used in any application that needs safe
password storage.")
    ;; "MIT" and PSF dual license
    (license license:x11)))

(define-public python-keyrings.alt
  (package
    (name "python-keyrings.alt")
    (version "3.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keyrings.alt" version))
        (sha256
         (base32
          "0gdjdqpq2hf770p6iwi891mil0vbsdhvy88x0v8b2w4y4b28lcli"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file "keyrings/alt/_win_crypto.py")
            ;; Rely on python-keyring>20:
            ;; https://github.com/jaraco/keyrings.alt/issues/33
            (substitute* '("keyrings/alt/tests/test_Gnome.py"
                           "keyrings/alt/tests/test_Google.py"
                           "keyrings/alt/tests/test_Windows.py"
                           "keyrings/alt/tests/test_file.py"
                           "keyrings/alt/tests/test_pyfs.py")
              (("keyring.tests.test_backend") "keyring.testing.backend")
              (("keyring.tests.util") "keyring.testing.util"))
            #t))))
    (build-system python-build-system)
    (native-inputs
     (list python-keyring python-pytest python-setuptools-scm))
    (home-page "https://github.com/jaraco/keyrings.alt")
    (synopsis "Alternate keyring implementations")
    (description "Keyrings in this package may have security risks or other
implications.  These backends were extracted from the main keyring project to
make them available for those who wish to employ them, but are discouraged for
general production use.  Include this module and use its backends at your own
risk.")
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
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'build-python-module
            (lambda _
              ;; We don't use maturin.
              (delete-file "pyproject.toml")
              (call-with-output-file "pyproject.toml"
                (lambda (port)
                  (format port "\
[build-system]
build-backend = 'setuptools.build_meta'
requires = ['setuptools']
")))
              (call-with-output-file "setup.cfg"
                (lambda (port)
                  (format port "\

[metadata]
name = blake3
version = ~a

[options]
packages = find:

[options.packages.find]
exclude =
  src*
  c_impl*
  tests*
  Cargo.toml
" #$version)))
              ;; ZIP does not support timestamps before 1980.
              (setenv "SOURCE_DATE_EPOCH" "315532800")
              (invoke "python" "-m" "build" "--wheel" "--no-isolation" ".")))
          (add-after 'build-python-module 'install-python-module
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((whl (car (find-files "dist" "\\.whl$"))))
                (invoke "pip" "--no-cache-dir" "--no-input"
                        "install" "--no-deps" "--prefix" #$output whl))))
          (add-after 'install-python-module 'install-python-library
            (lambda _
              (let ((site (string-append #$output "/lib/python"
                                         #$(version-major+minor
                                            (package-version python))
                                         "/site-packages")))
                (mkdir-p site)
                (copy-file "target/release/libblake3.so"
                           (string-append site "/blake3.so"))))))
      #:cargo-inputs
      `(("rust-blake3" ,rust-blake3-1)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-pyo3" ,rust-pyo3-0.15)
        ("rust-rayon" ,rust-rayon-1))))
    (inputs (list rust-blake3-1))
    (native-inputs
     (list python-wrapper
           python-pypa-build
           python-wheel))
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
     (list nss-certs-for-test python-pytest-cov python-setuptools python-wheel))
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
    (version "2022.6.15")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "certifi" version))
              (sha256
               (base32
                "03c2l11lgljx0kz17cvdc4hlc3p1594ajdih9zq0a4dig285mj44"))
              (snippet
               #~(begin
                   (delete-file "certifi/cacert.pem")
                   (delete-file "certifi/core.py")
                   (with-output-to-file "certifi/core.py"
                     (lambda _
                       (display "\"\"\"
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
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;no tests
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
    (build-system cargo-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules
                           ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system) #:prefix py:)
                  (guix build utils))
      #:cargo-inputs
      (list rust-asn1-0.20
            rust-cc-1
            rust-cfg-if-1
            rust-foreign-types-0.3
            rust-foreign-types-shared-0.1
            rust-once-cell-1
            rust-openssl-0.10
            rust-openssl-sys-0.9
            rust-pem-3
            rust-pyo3-0.23
            rust-self-cell-1)
      #:install-source? #false
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'dont-vendor-self
            (lambda* (#:key vendor-dir #:allow-other-keys)
              ;; Don't keep the whole tarball in the vendor directory
              (delete-file-recursively
               (string-append vendor-dir "/cryptography-" #$version ".tar.zst"))))
          (replace 'build
            (assoc-ref py:%standard-phases 'build))
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? inputs outputs #:allow-other-keys)
              (when tests?
                (py:add-installed-pythonpath inputs outputs)
                (invoke "python" "-m" "pytest" "tests"))))
          (replace 'install
            (assoc-ref py:%standard-phases 'install)))))
    (native-inputs
     (list python-certifi
           python-cffi
           python-click
           python-cryptography-vectors
           python-mypy
           python-pretend
           python-pytest
           python-pytest-benchmark
           python-pytest-cov
           python-pytest-randomly
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (inputs (list maturin openssl python-wrapper))
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
                (invoke "faketime" "2024-07-20" "pytest" "-vv" "-k"
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
           python-wheel))
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
    (version "1.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ed25519" version))
        (sha256
          (base32
            "0ahx1nkxa0xis3cw0h5c4fpgv8mq4znkq7kajly33lc3317bk499"))))
    (build-system python-build-system)
    (home-page "https://github.com/warner/python-ed25519")
    (synopsis "Ed25519 public-key signatures")
    (description "Ed25519 public-key signatures")
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
    (build-system python-build-system)
    (home-page "https://github.com/tgalal/python-axolotl-curve25519")
    (synopsis "Python wrapper for curve25519 library")
    (description "This is a python wrapper for the curve25519 library
with ed25519 signatures.  The C code was pulled from
libaxolotl-android.  At the moment this wrapper is meant for use by
python-axolotl.")
    (license (list license:gpl3    ; Most files
                   license:bsd-3)))) ; curve/curve25519-donna.c

(define-public python-axolotl
  (package
    (name "python-axolotl")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-axolotl" version))
       (sha256
        (base32
         "1bwdp24fmriffwx91aigs9k162albb51iskp23nc939z893q23py"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Don't install tests
         (add-before 'install 'remove-tests
           (lambda _
             (for-each delete-file-recursively
                       '("axolotl/tests" "build/lib/axolotl/tests"))
             #t)))))
    (propagated-inputs
     (list python-axolotl-curve25519 python-cryptography python-protobuf))
    (home-page "https://github.com/tgalal/python-axolotl")
    (synopsis "Python port of libaxolotl-android")
    (description "This is a python port of libaxolotl-android.  This
is a ratcheting forward secrecy protocol that works in synchronous and
asynchronous messaging environments.")
    (license license:gpl3)))

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
        (base32
         "1gx0znbfvs8jg9s754hha81l8wpghswkfsqx2jzpgv6gigf3sm8z"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-axolotl-curve25519 python-cryptography python-protobuf-5))
    (home-page "https://dev.gajim.org/gajim/omemo-dr")
    (synopsis "OMEMO cryptography library")
    (description "OMEMO cryptography library that was forked from python-axolotl.")
    (license license:gpl3)))

(define-public python-pyaes
  (package
    (name "python-pyaes")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyaes" version))
       (sha256
        (base32
         "13vdaff15k0jyfcss4b4xvfgm8xyv0nrbyw5n1qc7lrqbi0b3h82"))))
    (build-system python-build-system)
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
        (base32
         "1qsg06qrqnzixmrm65ibg503y2gffd675h1si4jgh92s315w1jrk"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "python" "run.py" "tests")))))))
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
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; As per the Unit Testing subsection in the README.
                (invoke "python" "-m" "crcmod.test")))))))
    (synopsis "CRC generator for Python")
    (description "Python module for generating objects that compute the
Cyclic Redundancy Check.")
    (home-page "https://crcmod.sourceforge.net/")
    (license license:expat)))

(define-public python-blurhash
  (package
    (name "python-blurhash")
    (version "1.1.4")
    (source
      (origin
        ;; Tests not included in pypi release and releases not tagged in git repo.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/halcy/blurhash-python")
               (commit "22e081ef1c24da1bb5c5eaa2c1d6649724deaef8")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1qq6mhydlp7q3na4kmaq3871h43wh3pyfyxr4b79bia73wjdylxf"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (delete-file "setup.cfg")
             (invoke "pytest"))))))
    (native-inputs
     (list python-numpy python-pillow python-pytest))
    (home-page "https://github.com/halcy/blurhash-python")
    (synopsis
     "Pure-Python implementation of the blurhash algorithm")
    (description
     "Pure-Python implementation of the blurhash algorithm.")
    (license license:expat)))

(define-public python-ecpy
  (package
    (name "python-ecpy")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ECPy" version))
        (sha256
          (base32
            "1gc3i5s93zq6x1nkaxkq1dvmsc12vmrw0hns9f5s1hcb78ni52c8"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-future))
    (home-page "https://github.com/ubinity/ECPy")
    (synopsis "Pure Python Elliptic Curve Library")
    (description "This package provides a Elliptic Curve Library in pure
Python.")
    (license license:asl2.0)))

(define-public python-josepy
  (package
    (name "python-josepy")
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "josepy" version))
              (sha256
               (base32
                "1jaxqyp53paks2z8zyzr50gqvzfxbar7r2qf98kqak4aizrxlcc9"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: some test dependencies are missing (see pyproject.toml).
     '(#:tests? #f))
    (propagated-inputs
     (list python-cryptography python-pyopenssl))
    (home-page "https://github.com/certbot/josepy")
    (synopsis "JOSE protocol implementation in Python")
    (description "This package provides a Python implementation of the JOSE
protocol (Javascript Object Signing and Encryption).")
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
    (version "3.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycryptodome" version))
       (sha256
        (base32
         "15vjyjy686kgm4fnpwlah1wvxxy0wvr4q5vnp1iygnlv8q6pwy7p"))
       (modules '((guix build utils)))
       (snippet pycryptodome-unbundle-tomcrypt-snippet)))
    (build-system python-build-system)
    (inputs
     (list libtomcrypt libtommath))
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

(define-public python-pycryptodomex
  (package (inherit python-pycryptodome)
    (name "python-pycryptodomex")
    (version (package-version python-pycryptodome))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycryptodomex" version))
       (sha256
        (base32 "0v4y03ha7rm9kdcv9fkrmc94425z3q3mq1nn5p1jbpc1ag80nb92"))
       (modules '((guix build utils)))
       (snippet pycryptodome-unbundle-tomcrypt-snippet)))
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
    (version "3.2.1")
    (source
     (origin
       (method git-fetch)
       ;; Source tarball on PyPI doesn't include tests.
       (uri (git-reference
             (url "https://github.com/libkeepass/pykeepass")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1symxf4ahylynihnp9z4z3lh2vy65ipvg8s4hjrnn936hcaaxghk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-kdbx-writable
           ;; Tests have to write to the .kdbx files in the test directory.
           (lambda _
             (with-directory-excursion "tests"
               (for-each make-file-writable (find-files "."))
               #t)))
         (add-before 'build 'patch-requirements
           (lambda _
             ;; Update requirements from dependency==version
             ;; to dependency>=version.
             (substitute* "setup.py"
               (("==") ">="))
             #t)))))
    (propagated-inputs
     (list python-argon2-cffi
           python-construct
           python-dateutil
           python-future
           python-lxml
           python-pycryptodomex))
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
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pylibscrypt" version))
       (sha256
        (base32
         "1b3rgzl6dbzs08vhv41b6y4n5189wv7lr27acxn104hs45745abs"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'hard-code-path-to-libscrypt
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libscrypt (assoc-ref inputs "libscrypt")))
               (substitute* "pylibscrypt/pylibscrypt.py"
                 (("find_library\\('scrypt'\\)")
                  (string-append "'" libscrypt "/lib/libscrypt.so'")))
               #t))))
       ;; The library can use various scrypt implementations and tests all of
       ;; them.  Since we only provide a single implementation, most tests
       ;; fail.  Simply skip them.
       #:tests? #f))
    ;; FIXME: Using "libscrypt" is the second best choice.  The best one
    ;; requires "hashlib.scrypt", provided by Python 3.6+ built with OpenSSL
    ;; 1.1+.  Use that as soon as Guix provides it.
    (inputs
     (list libscrypt))
    (home-page "https://github.com/jvarho/pylibscrypt")
    (synopsis "Scrypt for Python")
    (description "There are a lot of different scrypt modules for Python, but
none of them have everything that I'd like, so here's one more.  It uses
@code{libscrypt}.")
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
       (method url-fetch)
       (uri (pypi-uri "pyotp" version))
       (sha256
        (base32 "0qvgsf86az9cqj6d8vim2shvyam89ik0p4sszysf7pfvw116csrl"))))
    (build-system python-build-system)
    (home-page "https://github.com/pyauth/pyotp")
    (synopsis "Python One Time Password Library")
    (description
     "PyOTP is a Python library for generating and verifying one-time
passwords.  It can be used to implement two-factor (2FA) or multi-factor
(MFA) authentication methods in web applications and in other systems that
require users to log in.")
    (license license:expat)))

(define-public python-scrypt
  (package
    (name "python-scrypt")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scrypt" version))
       (sha256
        (base32
         "0hjk71k3mgnl8siikm9lii9im8kv0rb7inkjzx78rnancra48xxr"))))
    (build-system python-build-system)
    (inputs
     (list openssl))
    (home-page "https://bitbucket.org/mhallin/py-scrypt")
    (synopsis "Bindings for the scrypt key derivation function library")
    (description "This is a set of Python bindings for the scrypt key
derivation function.")
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

(define-public python-hkdf
  (package
    (name "python-hkdf")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "hkdf" version))
        (sha256
         (base32
          "1jhxk5vhxmxxjp3zj526ry521v9inzzl8jqaaf0ma65w6k332ak2"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose))
    (home-page "https://github.com/casebeer/python-hkdf")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description "This package provides a Python implementation of the HMAC Key
Derivation function (HKDF) defined in RFC 5869.")
    (license license:bsd-2)))

(define-public python-spake2
  (package
    (name "python-spake2")
    (version "0.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "spake2" version))
        (sha256
         (base32
          "0d4kbaxi4cv8klyqh6yb0p0qiwfdwvczy1h2mzvmlfdcsnlc87s2"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cryptography python-hkdf))
    (home-page "https://github.com/warner/python-spake2")
    (synopsis "SPAKE2 password-authenticated key exchange in Python")
    (description "This package provides a Python implementation of the SPAKE2
Password-Authenticated Key Exchange algorithm.")
    (license license:expat)))

(define-public python-txtorcon
  (package
    (name "python-txtorcon")
    (version "24.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "txtorcon" version))
              (sha256
               (base32
                "1l4ajw4h7nay4vmllh6cs7zh3hnh8vj4yvgfnq3m734wil9ikzmy"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (inputs
     (list lsof))
    (propagated-inputs
     (list python-automat
           python-idna
           python-incremental
           python-pyopenssl
           python-service-identity
           python-twisted
           python-zope-interface))
    (home-page "https://github.com/meejah/txtorcon")
    (synopsis "Twisted-based Tor controller client")
    (description "This package provides a Twisted-based Tor controller client,
with state-tracking and configuration abstractions.")
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
        (base32
         "0lipygpzhwzzsq2k5imb1jgkmj8y4khxdwhzadjs3bd56g6bmkx9"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-pytest-runner))
    (inputs
     (list keyutils))
    (arguments
     '(#:tests? #f))
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
             (url "https://github.com/JuulLabs-OSS/mcuboot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m1csyvzq4jx81zg635ssy1n7sc0z539z0myh872ll3nwqx7wa0q"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-broken-test
           (lambda _
             (substitute* "scripts/imgtool/keys/ed25519_test.py"
               (("raw_sign") "sign_digest"))
             #t))
         (add-before 'build 'change-directory
           (lambda _
             (chdir "scripts")
             #t)))))
    (propagated-inputs
     (list python-click python-intelhex python-cryptography))
    (home-page "https://mcuboot.com")
    (synopsis "Tool to securely sign firmware images for booting by MCUboot")
    (description "MCUboot is a secure bootloader for 32-bit MCUs.  This
package provides a tool to securely sign firmware images for booting by
MCUboot.")
    (license license:expat)))

(define-public python-ntlm-auth
  (package
    (name "python-ntlm-auth")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ntlm-auth" version))
       (sha256
        (base32
         "16mavidki4ma5ip8srqalr19gz4f5yn3cnmmgps1fmgfr24j63rm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cryptography))
    (home-page "https://github.com/jborean93/ntlm-auth")
    (synopsis
     "Calculates NTLM Authentication codes")
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
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Tests require a running dbus service.
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
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trustme" version))
       (sha256
        (base32 "0v2qzszmyazfgc1snicdr4b4qdajpjd4pbinpgrn9vfff0yv41sy"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-more-itertools
           python-pyopenssl
           python-pytest
           python-pytest-cov
           python-service-identity
           python-setuptools
           python-wheel
           python-zipp))
    (propagated-inputs
     (list python-cryptography
           python-idna
           python-ipaddress))
    (home-page "https://github.com/python-trio/trustme")
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
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "certipy" version))
        (sha256
         (base32
          "0n980gqpzh0fm58h3i4mi2i10wgj606lscm1r5sk60vbf6vh8mv9"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pyopenssl))
    (native-inputs
     (list python-pytest))
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
    (version "21.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "argon2-cffi" version))
        (sha256
         (base32
          "0w5q5cdwmzpjgw3bl9f6b9a5xai87qvx3jryra9gd8fi0c8vc47p"))
        (modules '((guix build utils)))
        (snippet '(delete-file-recursively "extras"))))
    ;; TODO: with pyproject-build-system the install phase fails.
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (replace 'build
           (lambda _
             (setenv "ARGON2_CFFI_USE_SYSTEM" "1")
             (invoke "python" "setup.py" "build")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")
               (invoke "python" "-m" "argon2" "--help")
               ;; see tox.ini
               (invoke "python" "-m" "argon2" "-n" "1" "-t" "1" "-m" "8" "-p" "1")))))))
    (propagated-inputs
     (list python-cffi python-typing-extensions))
    (inputs (list argon2))
    (native-inputs
     (list python-hypothesis
           python-pytest))
    (home-page "https://argon2-cffi.readthedocs.io/")
    (synopsis "Secure Password Hashes for Python")
    (description
     "Argon2 is a secure password hashing algorithm.  It is designed to have
both a configurable runtime as well as memory consumption.  This means that you
can decide how long it takes to hash a password and how much memory is required.")
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
         (base32
          "1m32dh5fqc8cy7jyf1z5fs6zvmdkbq5fi98hr609gbl7s0l0y0i9"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "-m" "pytest"))))))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-argon2-cffi python-cryptography))
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
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (native-inputs
     (list python-pytest
           python-wheel))
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
       (method url-fetch)
       (uri (pypi-uri "pyu2f" version))
       (sha256
        (base32 "0srhzdbgdsqwpcw7awqm19yg3xbabqckfvrp8rbpvz2232hs7jm3"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;none included
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
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sop" version))
        (sha256
         (base32
          "0gljyjsdn6hdmwlwwb5g5s0c031p6izamvfxp0d39x60af8k5jyf"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; There are no tests, and unittest throws an error trying
                     ; to find some:
                     ;     TypeError: don't know how to make test from: 0.2.0
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
    (version "2.0.3")
    (home-page "https://github.com/starkbank/ecdsa-python")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k9h4p0frkgj76vrqfjim4mik98g09mivdxxcmxr6raa5jwr83sh"))))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'remove-broken-test
                          (lambda _
                            (delete-file "tests/testOpenSSL.py"))))))
    (build-system python-build-system)
    (native-inputs (list python-pytest))
    (synopsis "Python ECDSA library")
    (description "This package provides a Python ECDSA library, optimized for
speed but without C extensions.")
    (license license:expat)))

(define-public python-zxcvbn
  (package
    (name "python-zxcvbn")
    (version "4.4.28")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/dwolfhub/zxcvbn-python")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xzlsqc9h0llfy19w4m39jgfcnvzqviv8jhgwn3r75kip97i5mvs"))))
    (build-system python-build-system)
    (home-page "https://github.com/dwolfhub/zxcvbn-python")
    (synopsis "Realistic password strength estimator Python library")
    (description "This is a Python implementation of the @code{zxcvbn} library
created at Dropbox.  The original library, written for JavaScript, can be
found @url{https://github.com/dropbox/zxcvbn, here}.  This port includes
features such as:
@enumerate
@item Accepts user data to be added to the dictionaries that are tested
against (name, birthdate, etc.)
@item Gives a score to the password, from 0 (terrible) to 4 (great).
@item Provides feedback on the password and ways to improve it.
@item Returns time estimates on how long it would take to guess the password
in different situations.
@end enumerate")
    (license license:expat)))

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
    (native-inputs (list python-setuptools python-wheel))
    (home-page "http://twhiteman.netfirms.com/des.html")
    (synopsis
     "Pure python implementation of the DES and TRIPLE DES encryption algorithms")
    (description
     "This package provides a pure Python implementation of the DES and
TRIPLE DES encryption algorithms.")
    (license license:public-domain)))
