;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-module (gnu packages crates-database)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages sqlite))

(define-public rust-libsqlite3-sys-0.30
  (package
    (name "rust-libsqlite3-sys")
    (version "0.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (inputs (list sqlite))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Native bindings to the libsqlite3 library")
    (description
     "This package provides native Rust bindings to the libsqlite3 library.")
    (license license:expat)))

(define-public rust-libsqlite3-sys-0.28
  (package
    (inherit rust-libsqlite3-sys-0.30)
    (name "rust-libsqlite3-sys")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gzwfw0n2wqgaihcgj65wzd3lclfxyy62gixq8sv6z04fi15h40c"))))
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))))

(define-public rust-libsqlite3-sys-0.27
  (package
    (inherit rust-libsqlite3-sys-0.28)
    (name "rust-libsqlite3-sys")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05pp60ncrmyjlxxjj187808jkvpxm06w5lvvdwwvxd2qrmnj4kng"))))
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))))

(define-public rust-libsqlite3-sys-0.26
  (package
    (inherit rust-libsqlite3-sys-0.27)
    (name "rust-libsqlite3-sys")
    (version "0.26.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libsqlite3-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09j3v5nhgvjdyskgwajhg9g6v3b2ij0lxiz8qqav2cxic7zjxhmg"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.64)
        ("rust-cc" ,rust-cc-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))))

(define-public rust-libsqlite3-sys-0.23
  (package
    (inherit rust-libsqlite3-sys-0.26)
    (name "rust-libsqlite3-sys")
    (version "0.23.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n6b4mlpw9l74cl5mahnpaanyjsgpmz5y517kmnk6v09fiygrjnj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.59)
        ("rust-cc" ,rust-cc-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))))

(define-public rust-libsqlite3-sys-0.22
  (package
    (inherit rust-libsqlite3-sys-0.23)
    (name "rust-libsqlite3-sys")
    (version "0.22.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17gqc2mwih81j3ds479gl5zmsxqzzrcrj3yyv62vh34bgy8n82r9"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       ;; build dependencies
       (("rust-bindgen" ,rust-bindgen-0.58)
        ("rust-cc" ,rust-cc-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))))

(define-public rust-libsqlite3-sys-0.20
  (package
    (inherit rust-libsqlite3-sys-0.22)
    (name "rust-libsqlite3-sys")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g9gbjjpm9phhs991abkzmacszibp94m5nrh331ycd99y9ci1lv4"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       ;; build dependencies
       (("rust-bindgen" ,rust-bindgen-0.55)
        ("rust-cc" ,rust-cc-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))))

(define-public rust-rusqlite-0.32
  (package
    (name "rust-rusqlite")
    (version "0.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vlx040bppl414pbjgbp7qr4jdxwszi9krx0m63zzf2f2whvflvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-csv" ,rust-csv-1)
        ("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
        ("rust-fallible-streaming-iterator" ,rust-fallible-streaming-iterator-0.1)
        ("rust-hashlink" ,rust-hashlink-0.9)
        ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.30)
        ("rust-rusqlite-macros" ,rust-rusqlite-macros-0.3)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-unicase" ,rust-unicase-2)
                                   ("rust-uuid" ,rust-uuid-1))))
    (inputs (list sqlite))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Wrapper for SQLite")
    (description "This crate provides a wrapper for SQLite.")
    (license license:expat)))

(define-public rust-rusqlite-0.31
  (package
    (inherit rust-rusqlite-0.32)
    (name "rust-rusqlite")
    (version "0.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bic69apqidimqf8gm80b98a832qzl9x6ns8myzah4yjg2ifnf5q"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-csv" ,rust-csv-1)
        ("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
        ("rust-fallible-streaming-iterator" ,rust-fallible-streaming-iterator-0.1)
        ("rust-hashlink" ,rust-hashlink-0.9)
        ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.28)
        ("rust-rusqlite-macros" ,rust-rusqlite-macros-0.2)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-unicase" ,rust-unicase-2)
                                   ("rust-uuid" ,rust-uuid-1))))))

(define-public rust-rusqlite-0.30
  (package
    (inherit rust-rusqlite-0.31)
    (name "rust-rusqlite")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kgjk7h53wp8k2di2j83ivf50f0bk6rg7bq8j36ygxb42lb4d057"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-csv" ,rust-csv-1)
        ("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
        ("rust-fallible-streaming-iterator" ,rust-fallible-streaming-iterator-0.1)
        ("rust-hashlink" ,rust-hashlink-0.8)
        ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.27)
        ("rust-rusqlite-macros" ,rust-rusqlite-macros-0.1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-unicase" ,rust-unicase-2)
                                   ("rust-uuid" ,rust-uuid-1))))))

(define-public rust-rusqlite-0.29
  (package
    (inherit rust-rusqlite-0.30)
    (name "rust-rusqlite")
    (version "0.29.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rusqlite" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wj12rmwa8g0bfhsk307fl84k0xcw8ji872xx3k447apdl1rv6sl"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-csv" ,rust-csv-1)
        ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
        ("rust-fallible-streaming-iterator" ,rust-fallible-streaming-iterator-0.1)
        ("rust-hashlink" ,rust-hashlink-0.8)
        ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.26)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-unicase" ,rust-unicase-2)
        ("rust-uuid" ,rust-uuid-1))))))

(define-public rust-rusqlite-0.26
  (package
    (inherit rust-rusqlite-0.29)
    (name "rust-rusqlite")
    (version "0.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19sh4nnw1i7a6wacqllz20qpqpdj96jsg3dzaq61cwmd3ywv10la"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-csv" ,rust-csv-1)
        ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
        ("rust-fallible-streaming-iterator"
         ,rust-fallible-streaming-iterator-0.1)
        ("rust-hashlink" ,rust-hashlink-0.7)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.23)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-0.8))))))

(define-public rust-rusqlite-macros-0.3
  (package
    (name "rust-rusqlite-macros")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusqlite-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i8fiv3jqwjcq2rdbwd5wycvh6fyfrw0y2wazinr2wpicifmxp7c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
                       ("rust-litrs" ,rust-litrs-0.4)
                       ("rust-sqlite3-parser" ,rust-sqlite3-parser-0.13))))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Private implementation detail of rusqlite crate")
    (description "This package provides a private implementation detail of the
@code{rusqlite} crate.")
    (license license:expat)))

(define-public rust-rusqlite-macros-0.2
  (package
    (inherit rust-rusqlite-macros-0.3)
    (name "rust-rusqlite-macros")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusqlite-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07y5887q4via4k7hdfxh61nmcwwz8r0bqlgxrk1p177lrkgz8cdp"))))
    (arguments
     `(#:cargo-inputs (("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
                       ("rust-litrs" ,rust-litrs-0.4)
                       ("rust-sqlite3-parser" ,rust-sqlite3-parser-0.12))))))

(define-public rust-rusqlite-macros-0.1
  (package
    (inherit rust-rusqlite-macros-0.2)
    (name "rust-rusqlite-macros")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusqlite-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bbgnp7pgk358js63666ix6a9p4fr4lgigaaf4av3aj45qksi4ix"))))
    (arguments
     `(#:cargo-inputs (("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
                       ("rust-sqlite3-parser" ,rust-sqlite3-parser-0.12))))))

