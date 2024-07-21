;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages sequoia)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)  ; glibc
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define-public rust-openpgp-cert-d-0.3
  (package
    (name "rust-openpgp-cert-d")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openpgp-cert-d" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kk2mr4rsib04cygv7jg55wvdxivakggqsfrn4apnaxxl5aknb0c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-fd-lock" ,rust-fd-lock-3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-sha1collisiondetection" ,rust-sha1collisiondetection-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-assert-fs" ,rust-assert-fs-1)
                                   ("rust-predicates" ,rust-predicates-3))))
    (home-page "https://gitlab.com/sequoia-pgp/pgp-cert-d")
    (synopsis "Shared OpenPGP Certificate Directory")
    (description "This package provides the shared code for a @code{OpenPGP}
Certificate Directory.")
    (license license:expat)))

(define-public rust-sequoia-autocrypt-0.25
  (package
    (name "rust-sequoia-autocrypt")
    (version "0.25.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sequoia-autocrypt" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ns121ggmx690m8czhc7zbb7rwz0jjv3l5gw4igs6mn1hznc0kz2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.21)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1))
       #:cargo-development-inputs
       (("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1))))
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list gmp nettle))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Deal with Autocrypt encoded data")
    (description "This crate implements low-level functionality like encoding
and decoding of Autocrypt headers and setup messages.  Note: Autocrypt is more
than just headers; it requires tight integration with the MUA.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-cert-store-0.5
  (package
    (name "rust-sequoia-cert-store")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-cert-store" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rjakcnhwdvwrm0952rpi9ky8cxvv5bnmylval49s3a087jqcm76"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-crossbeam" ,rust-crossbeam-0.8)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-openpgp-cert-d" ,rust-openpgp-cert-d-0.3)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rusqlite" ,rust-rusqlite-0.29)
                       ("rust-sequoia-net" ,rust-sequoia-net-0.28)
                       ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list gmp nettle openssl sqlite))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Certificate database interface")
    (description "This package provides a certificate database interface.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-ipc-0.33
  (package
    (name "rust-sequoia-ipc")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-ipc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h4b675m6r9r64ibv472fsyqkfh9xbx2wz4jaa4v01ivgsd7k3r1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-buffered-reader" ,rust-buffered-reader-1)
                       ("rust-capnp-rpc" ,rust-capnp-rpc-0.19)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-ctor" ,rust-ctor-0.2)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-fs2" ,rust-fs2-0.4)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-lalrpop" ,rust-lalrpop-0.17)
                       ("rust-lalrpop-util" ,rust-lalrpop-util-0.17)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memsec" ,rust-memsec-0.5)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-4)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
                                   ("rust-tokio" ,rust-tokio-1))))
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list nettle))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Interprocess communication infrastructure for Sequoia")
    (description
     "This package provides interprocess communication infrastructure for Sequoia.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-keystore-0.2
  (package
    (name "rust-sequoia-keystore")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-keystore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11rb2k5v2mc5nf2bafp78nydgcx4gizyxqa9b9lc3d1b73mqv2ad"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-capnp" ,rust-capnp-0.19)
        ("rust-capnpc" ,rust-capnpc-0.19)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-paste" ,rust-paste-1)
        ("rust-sequoia-ipc" ,rust-sequoia-ipc-0.33)
        ("rust-sequoia-keystore-backend" ,rust-sequoia-keystore-backend-0.2)
        ("rust-sequoia-keystore-softkeys" ,rust-sequoia-keystore-softkeys-0.2)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-util" ,rust-tokio-util-0.7))
       #:cargo-development-inputs
       (("rust-dircpy" ,rust-dircpy-0.3)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-test-log" ,rust-test-log-0.2)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (native-inputs (list capnproto clang pkg-config))
    (inputs (list nettle))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Sequoia's private key store server")
    (description "This package contains sequoia's private key store server.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-keystore-backend-0.2
  (package
    (name "rust-sequoia-keystore-backend")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-keystore-backend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07v6rc27v0di2v59mixshhc4fkkf1ig0yqkzgqz0v2si4z8slv3s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-blanket" ,rust-blanket-0.3)
                       ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1))))
    (native-inputs (list clang pkg-config))
    (inputs (list nettle))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Traits for private key store backends")
    (description "This package contains traits for private key store backends.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-keystore-softkeys-0.2
  (package
    (name "rust-sequoia-keystore-softkeys")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-keystore-softkeys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "151f1ai0bxvab8fi314qcybilv4vq26gfdcs3yp7r28xqn9hldw0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-sequoia-keystore-backend" ,rust-sequoia-keystore-backend-0.2)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-test-log" ,rust-test-log-0.2)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (native-inputs (list clang pkg-config))
    (inputs (list nettle))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "In-memory backend for Sequoia's private key store")
    (description
     "This package provides a soft key (in-memory key) backend for Sequoia's
private key store.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-net-0.28
  (package
    (name "rust-sequoia-net")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-net" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jw1p8gwf505q6dh1281fl7kmh8mr1f4hswl5crrycwqlq5q3gva"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hickory-client" ,rust-hickory-client-0.24)
                       ("rust-hickory-resolver" ,rust-hickory-resolver-0.24)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-z-base-32" ,rust-z-base-32-0.1))
       #:cargo-development-inputs (("rust-hyper" ,rust-hyper-0.14)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-reqwest" ,rust-reqwest-0.11)
                                   ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1))))
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list gmp nettle openssl))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Discover and publish OpenPGP certificates over the network")
    (description "This package provides a crate to access keyservers using the
HKP protocol, and searching and publishing Web Key Directories.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-openpgp-1
  (package
    (name "rust-sequoia-openpgp")
    (version "1.19.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-openpgp" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1x3d4yj8mhra8yhcxz6z73pb77pzk0zl1vgxx0yrimzk9b759wgb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("crypto-nettle")
       #:cargo-test-flags
       (list "--release" "--"
             "--skip=leak_tests::test_ed25519")
       #:cargo-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-aes-gcm" ,rust-aes-gcm-0.10)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-block-padding" ,rust-block-padding-0.3)
        ("rust-blowfish" ,rust-blowfish-0.9)
        ("rust-botan" ,rust-botan-0.10)
        ("rust-buffered-reader" ,rust-buffered-reader-1)
        ("rust-bzip2" ,rust-bzip2-0.4)
        ("rust-camellia" ,rust-camellia-0.1)
        ("rust-cast5" ,rust-cast5-0.11)
        ("rust-cfb-mode" ,rust-cfb-mode-0.8)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-cipher" ,rust-cipher-0.4)
        ("rust-des" ,rust-des-0.8)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-dsa" ,rust-dsa-0.6)
        ("rust-dyn-clone" ,rust-dyn-clone-1)
        ("rust-eax" ,rust-eax-0.5)
        ("rust-ecb" ,rust-ecb-0.1)
        ("rust-ecdsa" ,rust-ecdsa-0.16)
        ("rust-ed25519" ,rust-ed25519-2)
        ("rust-ed25519-dalek" ,rust-ed25519-dalek-2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-idea" ,rust-idea-0.5)
        ("rust-idna" ,rust-idna-0.5)
        ("rust-lalrpop" ,rust-lalrpop-0.20)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.20)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-md-5" ,rust-md-5-0.10)
        ("rust-memsec" ,rust-memsec-0.6)
        ("rust-nettle" ,rust-nettle-7)
        ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-p256" ,rust-p256-0.13)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-regex" ,rust-regex-1)
        ("rust-regex-syntax" ,rust-regex-syntax-0.8)
        ("rust-ripemd" ,rust-ripemd-0.1)
        ("rust-rsa" ,rust-rsa-0.9)
        ("rust-sha1collisiondetection" ,rust-sha1collisiondetection-0.3)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-twofish" ,rust-twofish-0.7)
        ("rust-typenum" ,rust-typenum-1)
        ("rust-win-crypto-ng" ,rust-win-crypto-ng-0.5)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x25519-dalek" ,rust-x25519-dalek-2)
        ("rust-xxhash-rust" ,rust-xxhash-rust-0.8))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rpassword" ,rust-rpassword-7))))
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list gmp nettle))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "OpenPGP data types and associated machinery")
    (description "This crate aims to provide a complete implementation of
OpenPGP as defined by RFC 4880 as well as some extensions (e.g., RFC 6637,
which describes ECC cryptography) for OpenPGP.  This includes support for
unbuffered message processing.

A few features that the OpenPGP community considers to be deprecated (e.g.,
version 3 compatibility) have been left out.  The developers have also updated
some OpenPGP defaults to avoid foot guns (e.g., they selected modern algorithm
defaults).

This Guix package is built to use the nettle cryptographic library.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-openpgp-0.9
  (package
    (inherit rust-sequoia-openpgp-1)
    (name "rust-sequoia-openpgp")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-openpgp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "007h2pi7lcph5jf5bxjydm7hjwjai33yk6dic3cxknki22lxlkfw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-buffered-reader" ,rust-buffered-reader-0.9)
        ("rust-bzip2" ,rust-bzip2-0.3)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-idna" ,rust-idna-0.1)
        ("rust-lalrpop" ,rust-lalrpop-0.17)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.17)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-memsec" ,rust-memsec-0.5)
        ("rust-nettle" ,rust-nettle-5)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-sequoia-rfc2822" ,rust-sequoia-rfc2822-0.9)
        ("rust-time" ,rust-time-0.1))))))

(define-public rust-sequoia-policy-config-0.6
  (package
    (name "rust-sequoia-policy-config")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sequoia-policy-config" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0x42h22kng4dsbfr0a6zdf2j9bcq14r0yr6xdw6rrggj139lazbm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1))))
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list gmp nettle))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Configure Sequoia using a configuration file")
    (description "Configure Sequoia using a configuration file.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-rfc2822-0.9
  (package
    (name "rust-sequoia-rfc2822")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-rfc2822" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aj34i6862718m162rqfv69fkmvdw063s6ws7hbp42n73gb08p5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-failure" ,rust-failure-0.1)
        ("rust-lalrpop" ,rust-lalrpop-0.17)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.17))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "RFC 2822 name-addr parser")
    (description "Currently, this crate only recognizes the RFC 2822 name-addr
and addr-spec productions, i.e., things of the form: @code{Name (Comment)
<email@@example.org>} and @code{email@@example.org}

Although the above appear simple to parse, RFC 2822's whitespace and comment
rules are rather complex.  This crate implements the whole grammar." )
    (license license:gpl3)))

(define-public rust-sequoia-wot-0.11
  (package
    (name "rust-sequoia-wot")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-wot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hb17adcqz357ci3d4v57pmywy4giq8591p1vb7p83h56zdk0sfi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-test-flags '("--release" "--"
                            ;; Not all files included.
                            "--skip=gpg_trust_roots")
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-complete" ,rust-clap-complete-4)
                       ("rust-clap-mangen" ,rust-clap-mangen-0.2)
                       ("rust-crossbeam" ,rust-crossbeam-0.8)
                       ("rust-dot-writer" ,rust-dot-writer-0.1)
                       ("rust-enumber" ,rust-enumber-0.3)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-sequoia-cert-store" ,rust-sequoia-cert-store-0.5)
                       ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
                       ("rust-sequoia-policy-config" ,rust-sequoia-policy-config-0.6)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-assert-cmd" ,rust-assert-cmd-2)
                                   ("rust-predicates" ,rust-predicates-2)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     (list nettle openssl sqlite))
    (native-inputs
     (list clang pkg-config))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Implementation of OpenPGP's web of trust")
    (description "An implementation of OpenPGP's web of trust.")
    (license license:lgpl2.0+)))

(define-public sequoia-sq
  (package
    (name "sequoia-sq")
    (version "0.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-sq" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0394xr4wxf5ymc8difnih5s9dpw7rpz9b0n7nnp6782gw65ch6lx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags
       (list "--release" "--"
             ;; The certificate has an expiration date.
             "--skip=sq_autocrypt_import")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-buffered-reader" ,rust-buffered-reader-1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-dot-writer" ,rust-dot-writer-0.1)
        ("rust-humantime" ,rust-humantime-2)
        ("rust-indicatif" ,rust-indicatif-0.17)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-roff" ,rust-roff-0.2)
        ("rust-rpassword" ,rust-rpassword-7)
        ("rust-sequoia-autocrypt" ,rust-sequoia-autocrypt-0.25)
        ("rust-sequoia-cert-store" ,rust-sequoia-cert-store-0.5)
        ("rust-sequoia-keystore" ,rust-sequoia-keystore-0.2)
        ("rust-sequoia-net" ,rust-sequoia-net-0.28)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-sequoia-policy-config" ,rust-sequoia-policy-config-0.6)
        ("rust-sequoia-wot" ,rust-sequoia-wot-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-subplot-build" ,rust-subplot-build-0.7)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-terminal-size" ,rust-terminal-size-0.2)
        ("rust-textwrap" ,rust-textwrap-0.15)
        ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-dircpy" ,rust-dircpy-0.3)
        ("rust-fehler" ,rust-fehler-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-predicates" ,rust-predicates-2)
        ("rust-regex" ,rust-regex-1)
        ("rust-subplotlib" ,rust-subplotlib-0.7))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-asset-out-dir
           (lambda _
             (setenv "ASSET_OUT_DIR" "target/assets")))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (bash-completions-dir
                     (string-append out "/etc/bash_completion.d"))
                    (zsh-completions-dir
                     (string-append share "/zsh/site-functions"))
                    (fish-completions-dir
                     (string-append share "/fish/vendor_completions.d"))
                    (elvish-completions-dir
                     (string-append share "/elvish/lib"))
                    (man1 (string-append share "/man/man1")))
               ;; The completions are generated in build.rs.
               (mkdir-p bash-completions-dir)
               (mkdir-p elvish-completions-dir)
               (for-each (lambda (file)
                           (install-file file man1))
                         (find-files "target/assets/man-pages" "\\.1$"))
               (copy-file "target/assets/shell-completions/sq.bash"
                          (string-append bash-completions-dir "/sq"))
               (install-file "target/assets/shell-completions/_sq"
                             zsh-completions-dir)
               (install-file "target/assets/shell-completions/sq.fish"
                             fish-completions-dir)
               (copy-file "target/assets/shell-completions/sq.elv"
                          (string-append elvish-completions-dir "/sq"))))))))
    (inputs
     (list nettle openssl sqlite))
    (native-inputs
     (list capnproto clang pkg-config))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Command-line frontend for Sequoia OpenPGP")
    (description "This package provides the command-line frontend for Sequoia
OpenPGP.

This Guix package is built to use the nettle cryptographic library.")
    (license license:lgpl2.0+)))

(define-public sequoia-sqv
  (package
    (name "sequoia-sqv")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-sqv" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0vzqahx7dk1wh2vp7lbzjgah8v7fqpvdf0dq0dydi9695ffm99lc"))))
    (build-system cargo-build-system)
    (inputs
     (list nettle openssl))
    (native-inputs
     (list clang pkg-config))
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1))
       #:cargo-development-inputs
       (("rust-assert-cli" ,rust-assert-cli-0.6))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Simple OpenPGP signature verification program")
    (description "@code{sqv} verifies detached OpenPGP signatures.  It is a
replacement for @code{gpgv}.  Unlike @code{gpgv}, it can take additional
constraints on the signature into account.

This Guix package is built to use the nettle cryptographic library.")
    (license license:lgpl2.0+)))

(define-public sequoia-wot
  (package
    (inherit rust-sequoia-wot-0.11)
    (name "sequoia-wot")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-sequoia-wot-0.11)
       ((#:install-source? _ #t) #f)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'install 'install-more
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out   (assoc-ref outputs "out"))
                      (share (string-append out "/share"))
                      (man1  (string-append share "/man/man1")))
                 (for-each (lambda (file)
                             (install-file file man1))
                           (find-files "target/release" "\\.1$"))
                 (mkdir-p (string-append out "/etc/bash_completion.d"))
                 (mkdir-p (string-append share "/fish/vendor_completions.d"))
                 (mkdir-p (string-append share "/elvish/lib"))
                 (copy-file (car (find-files "target/release" "sq-wot.bash"))
                            (string-append out "/etc/bash_completion.d/sq-wot"))
                 (copy-file (car (find-files "target/release" "sq-wot.fish"))
                            (string-append
                              share "/fish/vendor_completions.d/sq-wot.fish"))
                 (copy-file (car (find-files "target/release" "sq-wot.elv"))
                            (string-append share "/elvish/lib/sq-wot"))
                 (install-file (car (find-files "target/release" "_sq-wot"))
                               (string-append
                                 share "/zsh/site-functions")))))))))
    (description "An implementation of OpenPGP's web of trust.

This Guix package is built to use the nettle cryptographic library.")))

;;

(define-public sequoia
  (package
    (name "sequoia")
    (version "1.19.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build union)
                  (guix build gnu-build-system)
                  (guix build gremlin)
                  (guix elf))
      #:builder
      #~(begin
          (use-modules (guix build utils)
                       (guix build union)
                       (guix build gnu-build-system)
                       (ice-9 match))
          (let ((make-dynamic-linker-cache
                 (assoc-ref %standard-phases 'make-dynamic-linker-cache))
                (ld.so.cache
                 (string-append #$output "/etc/ld.so.cache")))
            (match %build-inputs
                   (((names . directories) ...)
                    (union-build #$output directories)))
            (delete-file ld.so.cache)
            (setenv "PATH"
                    (string-append (getenv "PATH") ":" #$glibc "/sbin"))
            (make-dynamic-linker-cache #:outputs %outputs)))))
    (inputs
     (list ;glibc ;; for ldconfig in make-dynamic-linker-cache
           sequoia-sq
           sequoia-sqv
           sequoia-wot))
    (home-page "https://sequoia-pgp.org")
    (synopsis "New OpenPGP implementation (meta-package)")
    (description "Sequoia is a new OpenPGP implementation, written in Rust,
consisting of several Rust crates/packages.  This Guix meta-package combines
these packages into a single one for convenience.  Anyhow, you should not
depend other packages on this one avoid excessive compile-times for users.")
    (license license:lgpl2.0+)))
