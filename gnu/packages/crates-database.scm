;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Domagoj Stolfa <domagoj.stolfa@gmail.com>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (guix gexp)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages sqlite))

(define-public rust-diesel-2
  (package
    (name "rust-diesel")
    (version "2.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ndmiv98xq2glkr4bqfq58fc3qncscfzx63xpj4ipwlqf30hbz03"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release"
         "--features" "sqlite")
       #:cargo-inputs (("rust-bigdecimal" ,rust-bigdecimal-0.1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-diesel-derives" ,rust-diesel-derives-2)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-ipnetwork" ,rust-ipnetwork-0.17)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.20)
                       ("rust-mysqlclient-sys" ,rust-mysqlclient-sys-0.2)
                       ("rust-num-bigint" ,rust-num-bigint-0.2)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pq-sys" ,rust-pq-sys-0.4)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-r2d2" ,rust-r2d2-0.8)
                       ("rust-serde-json" ,rust-serde-json-0.9)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.7))
       #:cargo-development-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                                   ("rust-dotenvy" ,rust-dotenvy-0.15)
                                   ("rust-ipnetwork" ,rust-ipnetwork-0.17)
                                   ("rust-quickcheck" ,rust-quickcheck-1))))
    (native-inputs (list sqlite))
    (home-page "https://diesel.rs")
    (synopsis "Safe, extensible ORM and Query Builder")
    (description "This package provides a safe, extensible ORM and Query
Builder for PostgreSQL, SQLite, and MySQL.")
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel-1
  (package
    (inherit rust-diesel-2)
    (name "rust-diesel")
    (version "1.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0kcfkfhsv5yv3ksj440ajgic930359i2bqi77ss4dm5pyvn3b0dj"))))
    (arguments
     `(#:cargo-test-flags
       '("--release"
         "--features" "sqlite"
         "--"
         "--skip=expression::count::count"
         "--skip=macros::internal::parse_type_args_with_bounds"
         "--skip=macros::internal::parse_type_args_with_bounds_containing_braces_and_commas"
         "--skip=macros::internal::parse_type_args_with_existentials_and_lifetimes"
         "--skip=macros::internal::parse_type_args_with_trailer")
       #:cargo-inputs
       (("rust-bigdecimal" ,rust-bigdecimal-0.1)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-diesel-derives" ,rust-diesel-derives-1)
        ("rust-ipnetwork" ,rust-ipnetwork-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.20)
        ("rust-mysqlclient-sys" ,rust-mysqlclient-sys-0.2)
        ("rust-num-bigint" ,rust-num-bigint-0.2)
        ("rust-num-integer" ,rust-num-integer-0.1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-pq-sys" ,rust-pq-sys-0.4)
        ("rust-quickcheck" ,rust-quickcheck-0.4)
        ("rust-r2d2" ,rust-r2d2-0.8)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-time" ,rust-time-0.1)
        ("rust-url" ,rust-url-1)
        ("rust-uuid" ,rust-uuid-0.5)
        ("rust-uuid" ,rust-uuid-0.8))
       #:cargo-development-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-dotenv" ,rust-dotenv-0.10)
        ("rust-quickcheck" ,rust-quickcheck-0.4)
        ("rust-tempdir" ,rust-tempdir-0.3))))))

(define-public rust-diesel-derives-2
  (package
    (name "rust-diesel-derives")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel_derives" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "036f3i1hsl2m2c0basg28adc9rh3vnr2vp0xwvzi9rsah75yw0jx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=derive_insertable"
         "--skip=derive_multiconnection"
         "--skip=derive_queryable"
         "--skip=derive_queryable_by_name")
       #:cargo-inputs
       (("rust-diesel-table-macro-syntax" ,rust-diesel-table-macro-syntax-0.1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                                   ("rust-diesel" ,rust-diesel-2)
                                   ("rust-dotenvy" ,rust-dotenvy-0.15))))
    (native-inputs (list sqlite))
    (home-page "https://diesel.rs")
    (synopsis "Crate internal to Diesel")
    (description "You should not use this crate directly, it is internal to
Diesel.")
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel-derives-1
  (package
    (inherit rust-diesel-derives-2)
    (name "rust-diesel-derives")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel_derives" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lsq133fwk0zj8xvxhdxqgg0xs31zf3abnwdyshaf0ldca7hkxa5"))))
    (arguments
     `(#:tests? #f      ; cannot find type `SqliteConnection` in this scope
       #:cargo-test-flags
       '("--release"
         "--features" "sqlite"
         "--"
         "--skip=expression::count::count")
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-diesel" ,rust-diesel-1)
        ("rust-dotenv" ,rust-dotenv-0.10))))))

(define-public rust-diesel-migrations-2
  (package
    (name "rust-diesel-migrations")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel_migrations" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b0ld4azk73rg2axwq7a4wnpwba3085f43jp3cw62n8c2bqb6dk0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-diesel" ,rust-diesel-2)
        ("rust-migrations-internals" ,rust-migrations-internals-2)
        ("rust-migrations-macros" ,rust-migrations-macros-2))
       #:cargo-development-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-dotenvy" ,rust-dotenvy-0.15)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://diesel.rs")
    (synopsis "Migration management for diesel")
    (description "This package provides migration management for Diesel.")
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel-migrations-1
  (package
    (inherit rust-diesel-migrations-2)
    (name "rust-diesel-migrations")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel_migrations" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k4g03ciqwya2xc1xvy5s9cs6q55k45wxa1gszswfg9m2f2dwg5z"))))
    (arguments
     `(#:tests? #f ;doctest_setup.rs: No such file or directory
       #:cargo-inputs
       (("rust-migrations-internals" ,rust-migrations-internals-1)
        ("rust-migrations-macros" ,rust-migrations-macros-1))
       #:cargo-development-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-diesel" ,rust-diesel-1)
        ("rust-dotenv" ,rust-dotenv-0.10))))))

(define-public rust-diesel-table-macro-syntax-0.1
  (package
    (name "rust-diesel-table-macro-syntax")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel_table_macro_syntax" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i9115qgsnargr6a707lqcjc45wqzq351a2gbvnnyw2kqkpmfmgw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-syn" ,rust-syn-2))))
    (home-page "https://diesel.rs")
    (synopsis "Internal diesel crate")
    (description "Internal diesel crate.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-mysqlclient-sys-0.2
  (package
    (name "rust-mysqlclient-sys")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mysqlclient-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16wndr59cbpc2wgli45zfgi0hi837pbrsh1aqh2k0ads50akh6zn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (native-inputs
     (list `(,mariadb "lib")))
    (home-page "https://github.com/sgrif/mysqlclient-sys")
    (synopsis "Auto-generated rust bindings for libmysqlclient")
    (description "This package provides auto-generated rust bindings for
libmysqlclient.")
    (license (list license:expat license:asl2.0))))

(define-public rust-postgres-0.19
  (package
    (name "rust-postgres")
    (version "0.19.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "postgres" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1hnid1d78zrr8ph12lpvp5b2cpx2fsqqgqs2yn1q23c6g7jix1y7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f          ; tests require postgres server.
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-postgres" ,rust-tokio-postgres-0.7))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/sfackler/rust-postgres")
    (synopsis "Native, synchronous PostgreSQL client")
    (description
     "This package provides a native, synchronous PostgreSQL client.")
    (license license:expat)))

(define-public rust-postgres-derive-0.4
  (package
    (name "rust-postgres-derive")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "postgres-derive" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0xqlf1gffy3q8hra3fm0vm9x8i5fkvi0qjl99d0xirxh3hidsmy8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/sfackler/rust-postgres")
    (synopsis "Internal crate used by postgres-types")
    (description
     "This is an internal crate used by postgres-types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-postgres-protocol-0.6
  (package
    (name "rust-postgres-protocol")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "postgres-protocol" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1wxzs78zvz00bh3bhbbp9hnq9hg77f8h5pzjmcy9481fsdq0ygpz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
        ("rust-hmac" ,rust-hmac-0.10)
        ("rust-md-5" ,rust-md-5-0.9)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-stringprep" ,rust-stringprep-0.1))))
    (home-page "https://github.com/sfackler/rust-postgres")
    (synopsis "Low level Postgres protocol APIs")
    (description
     "This package provides low level Postgres protocol APIs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-postgres-types-0.2
  (package
    (name "rust-postgres-types")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "postgres-types" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0brsqkydz0grfy60nc1d0hxa9jbpim0c7c52v467nrdpw4ql23s3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bit-vec" ,rust-bit-vec-0.6)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-eui48" ,rust-eui48-0.4)
        ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
        ("rust-geo-types" ,rust-geo-types-0.7)
        ("rust-geo-types" ,rust-geo-types-0.6)
        ("rust-postgres-derive" ,rust-postgres-derive-0.4)
        ("rust-postgres-protocol" ,rust-postgres-protocol-0.6)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-time" ,rust-time-0.2)
        ("rust-uuid" ,rust-uuid-0.8))))
    (home-page "https://github.com/sfackler/rust-postgres")
    (synopsis "Conversions between Rust and Postgres values")
    (description
     "This package provides a Rust implementation for conversions between Rust
and Postgres values.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pq-sys-0.4
  (package
    (name "rust-pq-sys")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pq-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1npz9756283pjq3lcpwss8xh1rw4sx8f6dz8cxdg90h5bbp5xhka"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (native-inputs
     (list postgresql))
    (home-page "https://crates.io/crates/pq-sys")
    (synopsis "Auto-generated rust bindings for libpq")
    (description "This package provides auto-generated rust bindings for
libpq.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-sqlite-0.36
  (package
    (name "rust-sqlite")
    (version "0.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13kimsfaxl81wm82j6qjzhycvyq4ljzi4kgbzr969vibdyqnzzjx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-sqlite3-sys" ,rust-sqlite3-sys-0.17))
       #:cargo-development-inputs (("rust-temporary" ,rust-temporary-0.6))))
    (inputs (list sqlite))
    (home-page "https://github.com/stainless-steel/sqlite")
    (synopsis "Interface to SQLite")
    (description "The package provides an interface to SQLite.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sqlite-0.30
  (package
    (inherit rust-sqlite-0.36)
    (name "rust-sqlite")
    (version "0.30.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lbfa0gjkqlhcmj4jy72kzfgd6a57z8gs1y7g34cbp4msvm4rk3f"))))
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-sqlite3-sys" ,rust-sqlite3-sys-0.15))
       #:cargo-development-inputs (("rust-temporary" ,rust-temporary-0.6))))))

(define-public rust-sqlite-0.27
  (package
    (inherit rust-sqlite-0.30)
    (name "rust-sqlite")
    (version "0.27.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11f1fw5gffni7mqr6mrliacr86v0yg9zmgvj3lhfdv1iz54vjv76"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-sqlite3-sys" ,rust-sqlite3-sys-0.14))
       #:cargo-development-inputs (("rust-temporary" ,rust-temporary-0.6))))))

(define-public rust-sqlite-0.26
  (package
    (inherit rust-sqlite-0.30)
    (name "rust-sqlite")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0snvg09bs0n8skcxkx52lcymdn0l130a2m8fpvxpdhkyq0sabc9z"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-sqlite3-sys" ,rust-sqlite3-sys-0.13))))))

(define-public rust-sqlite3-parser-0.13
  (package
    (name "rust-sqlite3-parser")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02sjybc8r2nwpgi54bcp2vjmzyaczxbdxvxxx067716bsvd0flzb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-phf" ,rust-phf-0.11)
                       ("rust-phf-codegen" ,rust-phf-codegen-0.11)
                       ("rust-phf-shared" ,rust-phf-shared-0.11)
                       ("rust-uncased" ,rust-uncased-0.9)
                       ("rust-uncased" ,rust-uncased-0.9))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.11))))
    (home-page "https://github.com/gwenn/lemon-rs")
    (synopsis "SQL parser (as understood by SQLite)")
    (description "This package provides an SQL parser (as understood by SQLite).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sqlite3-parser-0.12
  (package
    (inherit rust-sqlite3-parser-0.13)
    (name "rust-sqlite3-parser")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dx8j16ki2fsrwn1p36wnf079pvcs17549rin29x99vhkcpjbpcs"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-phf" ,rust-phf-0.11)
                       ("rust-phf-codegen" ,rust-phf-codegen-0.11)
                       ("rust-phf-shared" ,rust-phf-shared-0.11)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-uncased" ,rust-uncased-0.9)
                       ("rust-uncased" ,rust-uncased-0.9))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.10))))))

(define-public rust-sqlite3-src-0.6
  (package
    (name "rust-sqlite3-src")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jkvjhgrfsq5m2ps3hh792mamwv8v6kf2gdj3wldn9vwyxnllk8p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (inputs (list sqlite))
    (home-page "https://github.com/stainless-steel/sqlite3-src")
    (synopsis "Provider of SQLite")
    (description "The package provides SQLite.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sqlite3-src-0.5
  (package
    (inherit rust-sqlite3-src-0.6)
    (name "rust-sqlite3-src")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m74wrkpify3z0xvrw4i2yssn9m9sjwqa5ipk6aq6f7fl58mmjdz"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Only allow for linking to system sqlite3.
                 (delete-file-recursively "source")
                 (delete-file "build.rs")
                 (with-output-to-file "build.rs"
                   (lambda _
                     (format #t "fn main (){~@
                             println!(\"cargo:rustc-link-lib=sqlite3\");~@
                             }~%")))))))
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-sqlite3-src-0.4
  (package
    (inherit rust-sqlite3-src-0.5)
    (name "rust-sqlite3-src")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14ancc9jafw5ql9carg27icjxcfrdz5izxk4bj7fp5n909x5m0fi"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Only allow for linking to system sqlite3.
                 (delete-file-recursively "source")
                 (delete-file "build.rs")
                 (with-output-to-file "build.rs"
                   (lambda _
                     (format #t "fn main (){~@
                             println!(\"cargo:rustc-link-lib=sqlite3\");~@
                             }~%")))))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-sqlite3-src-0.3
  (package
    (inherit rust-sqlite3-src-0.5)
    (name "rust-sqlite3-src")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18ygmfcpkccs8s9m5s9q31rrx1mrdps387w9yp3481jswxyb0q52"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-sqlite3-sys-0.17
  (package
    (name "rust-sqlite3-sys")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rdds3kzxbxwy3lpsvgy7g8nh609nzqpxv4jvj23ag0c16kss09r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-sqlite3-src" ,rust-sqlite3-src-0.6))))
    (inputs (list sqlite))
    (home-page "https://github.com/stainless-steel/sqlite3-sys")
    (synopsis "Bindings to SQLite")
    (description "The package provides bindings to SQLite.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sqlite3-sys-0.15
  (package
    (inherit rust-sqlite3-sys-0.17)
    (name "rust-sqlite3-sys")
    (version "0.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fq6m21dnd5yqrzknsmnl2565nahdwa29s7x12xhxr1kjik2qxgj"))))
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-sqlite3-src" ,rust-sqlite3-src-0.5))
       #:cargo-development-inputs (("rust-temporary" ,rust-temporary-0.6))))))

(define-public rust-sqlite3-sys-0.14
  (package
    (inherit rust-sqlite3-sys-0.15)
    (name "rust-sqlite3-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vmrzgchmbqk9jk1dq1jp1lq6id0p3h8vwna02x60ly59y19jz6l"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-sqlite3-src" ,rust-sqlite3-src-0.4))
       #:cargo-development-inputs (("rust-temporary" ,rust-temporary-0.6))))))

(define-public rust-sqlite3-sys-0.13
  (package
    (inherit rust-sqlite3-sys-0.15)
    (name "rust-sqlite3-sys")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m1f5r4xg5i3r6795q8vwqfdcq3gh1qlfjwkywnka57bz8lg1lh4"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-sqlite3-src" ,rust-sqlite3-src-0.3))))))

(define-public rust-sqlparser-0.43
  (package
    (name "rust-sqlparser")
    (version "0.43.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m7mddhgj9j0hpw8lxzxbbzkrrmd4q019xq6sl5x6z5sbap4np7r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bigdecimal" ,rust-bigdecimal-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sqlparser-derive" ,rust-sqlparser-derive-0.2))
       #:cargo-development-inputs (("rust-matches" ,rust-matches-0.1)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-simple-logger" ,rust-simple-logger-4))))
    (home-page "https://github.com/sqlparser-rs/sqlparser-rs")
    (synopsis "Extensible SQL Lexer and Parser")
    (description
     "Extensible SQL Lexer and Parser with support for ANSI SQL:2011.")
    (license license:asl2.0)))

(define-public rust-sqlparser-0.39
  (package
    (inherit rust-sqlparser-0.43)
    (name "rust-sqlparser")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mrbqjdqr179qnhy43d0dnrl3yipsp4qyji5rc68j4fyrg14sfvl"))))
    (arguments
     `(#:cargo-inputs (("rust-bigdecimal" ,rust-bigdecimal-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sqlparser-derive" ,rust-sqlparser-derive-0.1))
       #:cargo-development-inputs
       (("rust-matches" ,rust-matches-0.1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-simple-logger" ,rust-simple-logger-4))))))

(define-public rust-sqlparser-derive-0.2
  (package
    (name "rust-sqlparser-derive")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlparser_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m05d4cxcsk1ljgy8zx79dibq62pdfbgp4zmfm9z2r2ma62y3ch1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/sqlparser-rs/sqlparser-rs")
    (synopsis "Implementation for sqlparser")
    (description
     "This package contains the implementation details for sqlparser.")
    (license license:asl2.0)))

(define-public rust-sqlparser-derive-0.1
  (package
    (inherit rust-sqlparser-derive-0.2)
    (name "rust-sqlparser-derive")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlparser_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07knj4cvqd9r7jb7b6fzdifxipabv34bnzbcw1x7yk1n9b5pbzjm"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))))

(define-public rust-sqlx-mysql-0.7
  (package
    (name "rust-sqlx-mysql")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-mysql" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "066lxhb80xgb8r5m2yy3a7ydjvp0b6wsk9s7whwfa83d46817lqy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--"
                            "--skip=options::MySqlConnectOptions (line 35)")
       #:cargo-inputs (("rust-atoi" ,rust-atoi-2)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crc" ,rust-crc-3)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-dotenvy" ,rust-dotenvy-0.15)
                       ("rust-either" ,rust-either-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rsa" ,rust-rsa-0.9)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sqlx-core" ,rust-sqlx-core-0.7)
                       ("rust-stringprep" ,rust-stringprep-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis "MySQL driver implementation for SQLx")
    (description
     "This package provides @code{MySQL} driver implementation for SQLx.  Not
for direct use; see the `sqlx` crate for details.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-postgres-0.7
  (package
    (name "rust-sqlx-postgres")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-postgres" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zjp30wj4n2f25dnb32vsg6jfpa3gw6dmfd0i5pr4kw91fw4x0kw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--"
                            "--skip=advisory_lock::PgAdvisoryLock::new"
                            "--skip=listener::PgListener::recv"
                            "--skip=listener::PgListener::try_recv"
                            "--skip=options::PgConnectOptions")
       #:cargo-inputs (("rust-atoi" ,rust-atoi-2)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bit-vec" ,rust-bit-vec-0.6)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crc" ,rust-crc-3)
                       ("rust-dotenvy" ,rust-dotenvy-0.15)
                       ("rust-etcetera" ,rust-etcetera-0.8)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-ipnetwork" ,rust-ipnetwork-0.20)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mac-address" ,rust-mac-address-1)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sqlx-core" ,rust-sqlx-core-0.7)
                       ("rust-stringprep" ,rust-stringprep-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis "PostgreSQL driver implementation for SQLx")
    (description
     "This package provides @code{PostgreSQL} driver implementation for SQLx.
Not for direct use; see the `sqlx` crate for details.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-sqlite-0.7
  (package
    (name "rust-sqlx-sqlite")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ap0bb2hazbrdgd7mhnckdg9xcchx0k094di9gnhpnhlhh5fyi5j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; cannot index into a value of type `IntMap<ColumnType>`
       #:cargo-inputs (("rust-atoi" ,rust-atoi-2)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-flume" ,rust-flume-0.11)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-intrusive" ,rust-futures-intrusive-0.5)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.27)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sqlx-core" ,rust-sqlx-core-0.7)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-urlencoding" ,rust-urlencoding-2)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-sqlx" ,rust-sqlx-0.7))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis "SQLite driver implementation for SQLx")
    (description
     "This package provides SQLite driver implementation for SQLx.  Not for
direct use; see the `sqlx` crate for details.")
    (license (list license:expat license:asl2.0))))

