;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages crates-tls)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tls))

(define-public rust-async-native-tls-0.3
  (package
    (name "rust-async-native-tls")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cwv4vbrvcbv58b51y1azfbszzgzhrzxx92q5nl6hk6kkf97m7ly"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #true       ;XXX: build when rust-async-std-1 is packaged
       #:cargo-inputs
       (;;("rust-async-std" ,rust-async-std-1)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-threadpool" ,rust-threadpool-1))))
    (home-page "https://docs.rs/crate/async-native-tls/")
    (synopsis "Native TLS using futures")
    (description "Native TLS using futures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-tls-0.10
  (package
    (name "rust-async-tls")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "126by0la2wvfadazfnmw7b6ch07dk9ggny94a3vvzgk2qdpqn3fx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-rustls" ,rust-rustls-0.19)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.21))))
    (home-page "https://github.com/async-std/async-tls")
    (synopsis "Asynchronous TLS/SSL streams using Rustls")
    (description
     "This package provides asynchronous TLS/SSL streams using Rustls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-native-tls-0.2
  (package
    (name "rust-native-tls")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bmrlg0fmzxaycjpkgkchi93av07v2yf9k33gc12ca9gqdrn28h7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ; tests require network access
       #:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-schannel" ,rust-schannel-0.1)
        ("rust-security-framework" ,rust-security-framework-2)
        ("rust-security-framework-sys" ,rust-security-framework-sys-2)
        ("rust-tempfile" ,rust-tempfile-3))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3)
        ("rust-test-cert-gen" ,rust-test-cert-gen-0.7))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/sfackler/rust-native-tls")
    (synopsis "Wrapper over a platform's native TLS implementation")
    (description
     "This package provides a wrapper over a platform's native TLS
implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-0.21
  (package
    (name "rust-rustls")
    (version "0.21.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fmpzk3axnhkd99saqkvraifdfms4pkyi56lkihf8n877j0sdmgr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.17)
        ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-sct" ,rust-sct-0.7))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.21)
        ("rust-bencher" ,rust-bencher-0.1)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-webpki-roots" ,rust-webpki-roots-0.25))))
    (home-page "https://github.com/rustls/rustls")
    (synopsis "Modern TLS library written in Rust")
    (description
     "This package provides a modern TLS library written in Rust.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-0.20
  (package
    (inherit rust-rustls-0.21)
    (name "rust-rustls")
    (version "0.20.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0bqfymq5bjs1jxg1iw2nn4ab3kzz2lrk8a1vx3s98lhp9p3qzxzz"))))
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-sct" ,rust-sct-0.7)
        ("rust-webpki" ,rust-webpki-0.22))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-env-logger" ,rust-env-logger-0.9)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-webpki-roots" ,rust-webpki-roots-0.22))))))

(define-public rust-rustls-0.19
  (package
    (inherit rust-rustls-0.20)
    (name "rust-rustls")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02wqas2pcxk75s9l9c9f1r5am7258bmqprh68pnqfvkwz0gx4kq6"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-sct" ,rust-sct-0.6)
        ("rust-webpki" ,rust-webpki-0.21))))))

(define-public rust-rustls-0.18
  (package
    (inherit rust-rustls-0.19)
    (name "rust-rustls")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "108cf3bfw5high066shz9xrfv4jz7djdmnwqs3kwx4wfypf2c4ax"))))
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.12)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-sct" ,rust-sct-0.6)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-log" ,rust-log-0.4)
        ("rust-webpki-roots" ,rust-webpki-roots-0.20))))))

(define-public rust-rustls-0.17
  (package
    (inherit rust-rustls-0.18)
    (name "rust-rustls")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1q8m835viqrf4bbd2fa8rnmaj48fkd984saxf0238hb8blgs7m60"))))
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.11)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-sct" ,rust-sct-0.6)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-log" ,rust-log-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.19))))))

(define-public rust-rustls-0.16
  (package
    (inherit rust-rustls-0.17)
    (name "rust-rustls")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17n0fx3fpkg4fhpdplrdhkissnl003kj90vzbqag11vkpyqihnmj"))))
    (arguments
     `(#:tests? #f ;; 1/114 tests fail (test file not found)
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-sct" ,rust-sct-0.6)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.17))))))

(define-public rust-rustls-0.15
  (package
    (inherit rust-rustls-0.16)
    (name "rust-rustls")
    (version "0.15.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0vh93fhqfbn4ysw4xzkpkpqdz36xixz4mhs1qllgldfq5iay6wgj"))))
    (arguments
     `(#:tests? #f      ; API tests panic
       #:cargo-test-flags
       '("--release" "--"
         "--skip=msgs::message_test::test_read_fuzz_corpus")
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.14)
        ("rust-sct" ,rust-sct-0.5)
        ("rust-untrusted" ,rust-untrusted-0.6)
        ("rust-webpki" ,rust-webpki-0.19))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.16))))))

(define-public rust-rustls-0.14
  (package
    (inherit rust-rustls-0.18)
    (name "rust-rustls")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nal4qca7f7mhwnvx3m824ymdj6qmzfcl64sxmrmpis32dwr2y4b"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.13)
        ("rust-sct" ,rust-sct-0.4)
        ("rust-untrusted" ,rust-untrusted-0.6)
        ("rust-webpki" ,rust-webpki-0.18))))))

(define-public rust-rustls-ffi-0.8
  (package
    (name "rust-rustls-ffi")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls-ffi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "06kqrvm1d5ps9pml26zdd2hm8hh20j6svwvqibpnx7m5rh3jg9cx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-enum" ,rust-num-enum-0.5)
        ("rust-rustls" ,rust-rustls-0.20)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.2)
        ("rust-sct" ,rust-sct-0.7)
        ("rust-webpki" ,rust-webpki-0.22))
        #:cargo-development-inputs
        (("rust-cbindgen" ,rust-cbindgen-0.26))
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'adjust-cbindgen-requirement
            ;; The Cargo.toml in the git repository doesn't specify
            ;; a version requirement for cbindgen.
            (lambda _
              (substitute* "Cargo.toml"
                (("0\\.19\\.0") "*")))))))
    (home-page "https://github.com/rustls/rustls-ffi")
    (synopsis "Rustls bindings for non-Rust languages")
    (description "Rustls bindings for non-Rust languages")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-native-certs-0.6
  (package
    (name "rust-rustls-native-certs")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Tests want network access.
       #:cargo-inputs
       (("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-schannel" ,rust-schannel-0.1)
        ("rust-security-framework" ,rust-security-framework-2))
       #:cargo-development-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-rustls" ,rust-rustls-0.21)
        ("rust-rustls-webpki" ,rust-rustls-webpki-0.100)
        ("rust-serial-test" ,rust-serial-test-2)
        ("rust-untrusted" ,rust-untrusted-0.7)
        ("rust-webpki-roots" ,rust-webpki-roots-0.23)
        ("rust-x509-parser" ,rust-x509-parser-0.15))))
    (home-page "https://github.com/ctz/rustls-native-certs")
    (synopsis "Use the platform native certificate store with rustls")
    (description "@code{rustls-native-certs} allows rustls to use the platform
native certificate store.")
    (license
     (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-native-certs-0.5
  (package
    (inherit rust-rustls-native-certs-0.6)
    (name "rust-rustls-native-certs")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14i0bbbigk6r6262hvc51vz4dvqk1f3vg2f264wfvn2vi30vf1ss"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-rustls" ,rust-rustls-0.19)
        ("rust-schannel" ,rust-schannel-0.1)
        ("rust-security-framework" ,rust-security-framework-2))))))

(define-public rust-rustls-native-certs-0.4
  (package
    (inherit rust-rustls-native-certs-0.5)
    (name "rust-rustls-native-certs")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1f2rkvdkz92qcmwryyqiw9phkqkf95g4962ljpfq5nkjfsd477b2"))))
    (arguments
     `(#:tests? #f      ; Tests want network access
       #:cargo-inputs
       (("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-schannel" ,rust-schannel-0.1)
        ("rust-security-framework"
         ,rust-security-framework-1))
       #:cargo-development-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-untrusted" ,rust-untrusted-0.7)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.20))))))

(define-public rust-rustls-pemfile-2
  (package
    (name "rust-rustls-pemfile")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x34xidvzn4br2vl8f8xwmhgbjv4lmlb0ggv5whlnk4yl87rir1m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic parser for PEM formatted keys and certificates")
    (description "This package provides a very basic parser for the
PEM-encodings commonly used to store keys and certificates at rest.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-pemfile-1
  (package
    (inherit rust-rustls-pemfile-2)
    (name "rust-rustls-pemfile")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cplx6hgkr32nq31p3613b2sj7csrrq3zp6znx9vc1qx9c4qff9d"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.21))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1))))))

(define-public rust-rustls-pemfile-0.2
  (package
    (inherit rust-rustls-pemfile-1)
    (name "rust-rustls-pemfile")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jfi97lqnnnnxhmfy6ygrsp0x70m8wsdpaw45svvz1qc6vmymssy"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.13))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3))))))

(define-public rust-rustls-pki-types-1
  (package
    (name "rust-rustls-pki-types")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pki-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16rkx6gn5l2zximxy8fx9h2vzks1hfxi5z5cd9y97r0fl853wrz7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rustls/pki-types")
    (synopsis "Shared types for the rustls PKI ecosystem")
    (description "Shared types for the rustls PKI ecosystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-webpki-0.101
  (package
    (name "rust-rustls-webpki")
    (version "0.101.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls-webpki" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-ring" ,rust-ring-0.17)
        ("rust-untrusted" ,rust-untrusted-0.9))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.21)
        ("rust-bencher" ,rust-bencher-0.1)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-rcgen" ,rust-rcgen-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/rustls/webpki")
    (synopsis "Web PKI X.509 Certificate Verification")
    (description "Web PKI X.509 Certificate Verification.")
    (license license:isc)))

(define-public rust-rustls-webpki-0.100
  (package
    (inherit rust-rustls-webpki-0.101)
    (name "rust-rustls-webpki")
    (version "0.100.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls-webpki" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sxlgpcczd1wihmnbgv5qz00jim32dap5wzq2rwcm39xxpapq86n"))))
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-untrusted" ,rust-untrusted-0.7))
       #:cargo-development-inputs (("rust-base64" ,rust-base64-0.13))))))

(define-public rust-tls-parser-0.11
  (package
    (name "rust-tls-parser")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tls-parser" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1q0vd41sjf6pbcygp5bqlpqrxbqdd0qsqi4sm5zbzvb4vvi0d4j0"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Accept newer versions of rust-clap.
                 (substitute* "Cargo.toml"
                   (("~2\\.33") "^2.33"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;; requires assets not included in crates.io tarball
       #:cargo-inputs
       (("rust-cookie-factory" ,rust-cookie-factory-0.3)
        ("rust-enum-primitive" ,rust-enum-primitive-0.1)
        ("rust-nom" ,rust-nom-7)
        ("rust-nom-derive" ,rust-nom-derive-0.10)
        ("rust-phf" ,rust-phf-0.10)
        ("rust-phf-codegen" ,rust-phf-codegen-0.10)
        ("rust-rusticata-macros" ,rust-rusticata-macros-4))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.7))))
    (home-page "https://github.com/rusticata/tls-parser")
    (synopsis "Parser for the TLS protocol")
    (description "This package provides a Rust parser for the TLS protocol.")
    (license (list license:expat license:asl2.0))))
