;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define-public rust-sequoia-autocrypt-0.25
  (package
    (name "rust-sequoia-autocrypt")
    (version "0.25.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sequoia-autocrypt" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0796mn8kwrpfc8qzliwyyy62mrg2w0j6ax8929jwrkibvwy2axi2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.13)
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

(define-public rust-sequoia-cert-store-0.3
  (package
    (name "rust-sequoia-cert-store")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sequoia-cert-store" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gmkqn2f23i2xwjwmnaj3dx9l4ir74dyylkw1qsxawxd95i8dk02"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-crossbeam" ,rust-crossbeam-0.8)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-openpgp-cert-d" ,rust-openpgp-cert-d-0.1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rusqlite" ,rust-rusqlite-0.29)
        ("rust-sequoia-net" ,rust-sequoia-net-0.27)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs
       (("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list gmp nettle openssl sqlite))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Certificate database interface")
    (description "This package provides a certificate database interface.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-ipc-0.30
  (package
    (name "rust-sequoia-ipc")
    (version "0.30.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-ipc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1fgqjwaw9rz74y394i3n2a6y2vvy0214daamzswn5ahidhycm3x3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-buffered-reader" ,rust-buffered-reader-1)
        ("rust-capnp-rpc" ,rust-capnp-rpc-0.14)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-ctor" ,rust-ctor-0.1)
        ("rust-dirs" ,rust-dirs-4)
        ("rust-fs2" ,rust-fs2-0.4)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-lalrpop" ,rust-lalrpop-0.19)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.19)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-memsec" ,rust-memsec-0.6)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-socket2" ,rust-socket2-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-3)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-tokio" ,rust-tokio-1))))
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list nettle))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Interprocess communication infrastructure for Sequoia")
    (description "Interprocess communication infrastructure for Sequoia")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-net-0.27
  (package
    (name "rust-sequoia-net")
    (version "0.27.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sequoia-net" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gyk5765hi3s05l64a744f9a4vynfisja92l51az9dpqgfkiw3wn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-hyper-tls" ,rust-hyper-tls-0.5)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-trust-dns-client" ,rust-trust-dns-client-0.22)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.22)
        ("rust-url" ,rust-url-2)
        ("rust-zbase32" ,rust-zbase32-0.1))
       #:cargo-development-inputs
       (("rust-hyper" ,rust-hyper-0.14)
        ("rust-rand" ,rust-rand-0.8)
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
    (version "1.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-openpgp" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1z0xl7hnm1p51pyhwdqyzcnl2dhzfjnvssz7hi15ps1hk4zzzvrh"))))
    (build-system cargo-build-system)
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list gmp nettle))
    (arguments
     `(#:features '("crypto-nettle")
       #:cargo-test-flags
       (list "--release" "--"
             ;; TODO: Figure out how this test is supposed to fail.
             "--skip=parse::test::panic_on_short_zip")
       #:cargo-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-block-padding" ,rust-block-padding-0.3)
        ("rust-blowfish" ,rust-blowfish-0.9)
        ("rust-botan" ,rust-botan-0.10)
        ("rust-buffered-reader" ,rust-buffered-reader-1)
        ("rust-bzip2" ,rust-bzip2-0.4)
        ("rust-cast5" ,rust-cast5-0.11)
        ("rust-cfb-mode" ,rust-cfb-mode-0.8)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-cipher" ,rust-cipher-0.4)
        ("rust-des" ,rust-des-0.8)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-dyn-clone" ,rust-dyn-clone-1)
        ("rust-eax" ,rust-eax-0.5)
        ("rust-ecb" ,rust-ecb-0.1)
        ("rust-ecdsa" ,rust-ecdsa-0.16)
        ("rust-ed25519" ,rust-ed25519-1)
        ("rust-ed25519-dalek" ,rust-ed25519-dalek-1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-idea" ,rust-idea-0.5)
        ("rust-idna" ,rust-idna-0.3)
        ("rust-lalrpop" ,rust-lalrpop-0.19)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.19)
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
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-regex" ,rust-regex-1)
        ("rust-regex-syntax" ,rust-regex-syntax-0.6)
        ("rust-ripemd" ,rust-ripemd-0.1)
        ("rust-rsa" ,rust-rsa-0.9)
        ("rust-sha-1" ,rust-sha-1-0.10)
        ("rust-sha1collisiondetection" ,rust-sha1collisiondetection-0.2)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-twofish" ,rust-twofish-0.7)
        ("rust-typenum" ,rust-typenum-1)
        ("rust-win-crypto-ng" ,rust-win-crypto-ng-0.5)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x25519-dalek-ng" ,rust-x25519-dalek-ng-1)
        ("rust-xxhash-rust" ,rust-xxhash-rust-0.8))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.4)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rpassword" ,rust-rpassword-6))))
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

(define-public rust-sequoia-wot-0.8
  (package
    (name "rust-sequoia-wot")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sequoia-wot" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rcp7ndjpdd4dkryhkkhakc8axbj93c1gr9qxxksdvrik803alfg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("sequoia-openpgp/crypto-nettle")
       #:cargo-test-flags
       (list "--release" "--"
             ;; Not all files included.
             "--skip=gpg_trust_roots")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-clap-mangen" ,rust-clap-mangen-0.2)
        ("rust-crossbeam" ,rust-crossbeam-0.8)
        ("rust-dot-writer" ,rust-dot-writer-0.1)
        ("rust-enumber" ,rust-enumber-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-openpgp-cert-d" ,rust-openpgp-cert-d-0.1)
        ("rust-sequoia-cert-store" ,rust-sequoia-cert-store-0.3)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-sequoia-policy-config" ,rust-sequoia-policy-config-0.6)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
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
    (version "0.30.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-sq" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0l3mlhvh93b8s1853gyzzfh1dznjdhbsbyxxcm3bbyxmkyr74wkd"))))
    (build-system cargo-build-system)
    (inputs
     (list nettle openssl sqlite))
    (native-inputs
     (list clang pkg-config))
    (arguments
     `(#:tests? #f  ; `(dyn std::fmt::Display + 'static)` cannot be sent between threads safely
       #:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-buffered-reader" ,rust-buffered-reader-1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-clap-mangen" ,rust-clap-mangen-0.2)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-dot-writer" ,rust-dot-writer-0.1)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-rpassword" ,rust-rpassword-6)
        ("rust-sequoia-autocrypt" ,rust-sequoia-autocrypt-0.25)
        ("rust-sequoia-cert-store" ,rust-sequoia-cert-store-0.3)
        ("rust-sequoia-net" ,rust-sequoia-net-0.27)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-sequoia-wot" ,rust-sequoia-wot-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-subplot-build" ,rust-subplot-build-0.7)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-fehler" ,rust-fehler-1)
        ("rust-predicates" ,rust-predicates-2)
        ("rust-subplotlib" ,rust-subplotlib-0.7))))
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
    (inherit rust-sequoia-wot-0.8)
    (name "sequoia-wot")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-sequoia-wot-0.8)
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
                 ;; TODO: Install _sq-wot.ps1, sq-wot.elv
                 (mkdir-p (string-append out "/etc/bash_completion.d"))
                 (mkdir-p (string-append share "/fish/vendor_completions.d"))
                 (copy-file (car (find-files "target/release" "sq-wot.bash"))
                            (string-append out "/etc/bash_completion.d/sq-wot"))
                 (copy-file (car (find-files "target/release" "sq-wot.fish"))
                            (string-append
                              share "/fish/vendor_completions.d/sq-wot.fish"))
                 (install-file (car (find-files "target/release" "_sq-wot"))
                               (string-append
                                 share "/zsh/site-functions")))))))))
    (description "An implementation of OpenPGP's web of trust.

This Guix package is built to use the nettle cryptographic library.")))

;;

(define-public sequoia
  (package
    (name "sequoia")
    (version "1.16.0")
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
