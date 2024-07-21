;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020-2022, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2023 VÖRÖSKŐI András <voroskoi@gmail.com>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2024 Nguyễn Gia Phong <mcsinyx@disroot.org>
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

(define-module (gnu packages crates-web)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit))

(define-public rust-actix-0.10
  (package
    (name "rust-actix")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q6cd08d0xikilj9l3gfsyhva5b91y55lfxy7yd7w7ivizw43qhv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #false                  ;doc test fails
       #:cargo-inputs
       (("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-derive" ,rust-actix-derive-0.5)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.4)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-util" ,rust-tokio-util-0.3)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.19)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.19))))
    (home-page "https://actix.rs")
    (synopsis "Actor framework for Rust")
    (description "This package provides Actix actor framework for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-0.8
  (package
    (inherit rust-actix-0.10)
    (name "rust-actix")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xqyrwq7hgi640h5czy73zrkxl1s0yhm7laxga13dwhkfg9f6737"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-http" ,rust-actix-http-0.2)
        ("rust-actix-rt" ,rust-actix-rt-0.2)
        ("rust-actix-derive" ,rust-actix-derive-0.4)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.3)
        ("rust-derive-more" ,rust-derive-more-0.14)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-hashbrown" ,rust-hashbrown-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-parking-lot" ,rust-parking-lot-0.8)
        ("rust-smallvec" ,rust-smallvec-0.6)
        ("rust-tokio-codec" ,rust-tokio-codec-0.1)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.11))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3))))))

(define-public rust-actix-codec-0.5
  (package
    (name "rust-actix-codec")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-codec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12m2jxysk2xpxi193340zv4w215cv9fyyna7rxvzh6wck0hhlysz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `tokio_test`
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis "Codec utilities for working with framed protocols")
    (description
     "This package provides codec utilities for working with framed protocols.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-codec-0.3
  (package
    (inherit rust-actix-codec-0.5)
    (name "rust-actix-codec")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-codec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w7506qd2f8q83z6l5lqx1363ks0ysx8f7qgvy8fknrq70xq7lbq"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-sink" ,rust-futures-sink-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-util" ,rust-tokio-util-0.3))))))

(define-public rust-actix-codec-0.2
  (package
    (inherit rust-actix-codec-0.3)
    (name "rust-actix-codec")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-codec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "100k0n155fnnjqjz2s1gnwim2fp7s1mw942x0famg89cbh55zr89"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-sink" ,rust-futures-sink-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-util" ,rust-tokio-util-0.2))))))

(define-public rust-actix-codec-0.1
  (package
    (inherit rust-actix-codec-0.3)
    (name "rust-actix-codec")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-codec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lv42xf57y3kwy8nl2a9pkz35yvbspd9250virfr7p069fpi2b4z"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-tokio-codec" ,rust-tokio-codec-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1))))))

(define-public rust-actix-connect-2
  (package
    (name "rust-actix-connect")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-connect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p6hh5rj9zpx4wx0h87d56ahk68hmhpw2gmfsfl5pwb312hkfy0p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.3)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-utils" ,rust-actix-utils-2)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-either" ,rust-either-1)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.4)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.14)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.19)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.19)
        ("rust-webpki" ,rust-webpki-0.21))))
    (home-page "https://actix.rs")
    (synopsis "TCP connector service for Actix ecosystem")
    (description
     "This package provides a TCP connector service for Actix ecosystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-connect-1
  (package
    (inherit rust-actix-connect-2)
    (name "rust-actix-connect")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-connect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v77m394gzbrrzg12xkqgli11vwhig0zcxy3yhmq1s91j9bcjp69"))))
    (arguments
     `(#:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.2)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-utils" ,rust-actix-utils-1)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-either" ,rust-either-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.4)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18.0-alpha.2)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.18.0-alpha.2)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-actix-testing" ,rust-actix-testing-1))))))

(define-public rust-actix-connect-0.2
  (package
    (inherit rust-actix-connect-2)
    (name "rust-actix-connect")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-connect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "187whz05gjkp9pcza4i714v0a8yxlg3jdrzii7gaqsxl9fyykbcz"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.1)
        ("rust-actix-rt" ,rust-actix-rt-0.2)
        ("rust-actix-service" ,rust-actix-service-0.4)
        ("rust-actix-utils" ,rust-actix-utils-0.4)
        ("rust-derive-more" ,rust-derive-more-0.15)
        ("rust-either" ,rust-either-1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-tokio-current-thread" ,rust-tokio-current-thread-0.1)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.3)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.9)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.11)
        ("rust-webpki" ,rust-webpki-0.19))
       #:cargo-development-inputs
       (("rust-actix-server-config" ,rust-actix-server-config-0.1)
        ("rust-actix-test-server" ,rust-actix-test-server-0.2)
        ("rust-bytes" ,rust-bytes-0.4))))))

(define-public rust-actix-derive-0.5
  (package
    (name "rust-actix-derive")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k1kg4gkp2jhi5fgdfd0cq2qfbyy3gfgwqjrvzq1hzrjmynwwnmr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #true              ;bootsrapping issues with rust-actix
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))
       ;; #:cargo-development-inputs
       ;; (("rust-actix" ,rust-actix-0.8))
       ))
    (home-page "https://github.com/actix/actix-derive/")
    (synopsis "Proc macros for Actix Rust actor framework")
    (description
     "This package provides proc macros for the Rust actor framework Actix.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-derive-0.4
  (package
    (inherit rust-actix-derive-0.5)
    (name "rust-actix-derive")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v0mvq883aq5z6d0893bh32bfddvfajh5bm7nkl0l8idpzbzdx8b"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-0.6)
        ("rust-syn" ,rust-syn-0.15))))))

(define-public rust-actix-http-3
  (package
    (name "rust-actix-http")
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hx7rjc1cwrrql5qmihl31hf7nblwyd6a4mvhcghvz41shzv28yj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `once_cell`
       #:cargo-inputs (("rust-actix-codec" ,rust-actix-codec-0.5)
                       ("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-tls" ,rust-actix-tls-3)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-ahash" ,rust-ahash-0.8)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-brotli" ,rust-brotli-3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-bytestring" ,rust-bytestring-1)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-language-tags" ,rust-language-tags-0.3)
                       ("rust-local-channel" ,rust-local-channel-0.1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-zstd" ,rust-zstd-0.13))))
    (home-page "https://actix.rs")
    (synopsis "HTTP primitives for the Actix ecosystem")
    (description
     "This package provides HTTP primitives for the Actix ecosystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-http-2
  (package
    (inherit rust-actix-http-3)
    (name "rust-actix-http")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x78h9lzqdhp06v1kf4dhbiqp8sc911w4lqfj5rmdbhpg3l9j8j5"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix" ,rust-actix-0.10)
        ("rust-actix-codec" ,rust-actix-codec-0.3)
        ("rust-actix-connect" ,rust-actix-connect-2)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-threadpool" ,rust-actix-threadpool-0.3)
        ("rust-actix-tls" ,rust-actix-tls-2)
        ("rust-actix-utils" ,rust-actix-utils-2)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-brotli2" ,rust-brotli2-0.3)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-cookie" ,rust-cookie-0.14)
        ("rust-copyless" ,rust-copyless-0.1)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-either" ,rust-either-1)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-fxhash" ,rust-fxhash-0.2)
        ("rust-h2" ,rust-h2-0.2)
        ("rust-http" ,rust-http-0.2)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-itoa" ,rust-itoa-0.4)
        ("rust-language-tags" ,rust-language-tags-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pin-project" ,rust-pin-project-1)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-time" ,rust-time-0.2))))))

(define-public rust-actix-http-1
  (package
    (inherit rust-actix-http-2)
    (name "rust-actix-http")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06chrs9asbxmxzgiw5sw7ky97yrin9g88nmd6w407a6y9z668rn1"))))
    ;; XXX: The crate fails to't build with with the same error as
    ;; rust-actix-connect.  Skip build for now.
    (arguments
     `(#:skip-build? #true
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.2)
        ("rust-actix-connect" ,rust-actix-connect-1)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-threadpool" ,rust-actix-threadpool-0.3)
        ("rust-actix-tls" ,rust-actix-tls-1)
        ("rust-actix-utils" ,rust-actix-utils-1)
        ("rust-base64" ,rust-base64-0.11)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-brotli2" ,rust-brotli2-0.3)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-copyless" ,rust-copyless-0.1)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-either" ,rust-either-1)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-fxhash" ,rust-fxhash-0.2)
        ("rust-h2" ,rust-h2-0.2)
        ("rust-http" ,rust-http-0.2)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-language-tags" ,rust-language-tags-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-regex" ,rust-regex-1)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6)
        ("rust-sha1" ,rust-sha1-0.6)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-time" ,rust-time-0.1))
       #:cargo-development-inputs
       (("rust-actix-http-test" ,rust-actix-http-test-1))))))

(define-public rust-actix-http-0.2
  (package
    (inherit rust-actix-http-2)
    (name "rust-actix-http")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fdi9pi33am22qbqni5dn2in11xfbchgsjnm9ws0s918rmvhzdgw"))))
    ;; XXX: The crate fails to't build without rust-actix-http-test-0.2 making
    ;; a circular dependency with rust-awc-0.2
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.1)
        ("rust-actix-connect" ,rust-actix-connect-0.2)
        ("rust-actix-server-config" ,rust-actix-server-config-0.1)
        ("rust-actix-service" ,rust-actix-service-0.4)
        ("rust-actix-threadpool" ,rust-actix-threadpool-0.1)
        ("rust-actix-utils" ,rust-actix-utils-0.4)
        ("rust-base64" ,rust-base64-0.10)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-brotli2" ,rust-brotli2-0.3)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-copyless" ,rust-copyless-0.1)
        ("rust-derive-more" ,rust-derive-more-0.15)
        ("rust-either" ,rust-either-1)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-h2" ,rust-h2-0.1)
        ("rust-hashbrown" ,rust-hashbrown-0.6)
        ("rust-http" ,rust-http-0.1)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-language-tags" ,rust-language-tags-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-regex" ,rust-regex-1)
        ("rust-ring" ,rust-ring-0.14)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6)
        ("rust-sha1" ,rust-sha1-0.6)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-time" ,rust-time-0.1)
        ("rust-tokio-current-thread" ,rust-tokio-current-thread-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.11)
        ("rust-webpki-roots" ,rust-webpki-roots-0.16))))))

(define-public rust-actix-http-test-1
  (package
    (name "rust-actix-http-test")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-http-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06z6iy9ffsjcw3g8zwwghky5zpyg7c1z823x35lgc4y1yjzxfizq"))))
    (build-system cargo-build-system)
    (arguments
     ;; XXX: The crate fails to't build with with the same error as
     ;; rust-actix-connect.  Skip build for now.
     `(#:skip-build? #true
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.2)
        ("rust-actix-connect" ,rust-actix-connect-1)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-server" ,rust-actix-server-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-testing" ,rust-actix-testing-1)
        ("rust-actix-utils" ,rust-actix-utils-1)
        ("rust-awc" ,rust-awc-1)
        ("rust-base64" ,rust-base64-0.11)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6)
        ("rust-sha1" ,rust-sha1-0.6)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-time" ,rust-time-0.1))
       #:cargo-development-inputs
       (("rust-actix-http" ,rust-actix-http-1))))
    (home-page "https://actix.rs")
    (synopsis "Helpers for Actix applications to use during testing")
    (description
     "This package provides various helpers for Actix applications to use
during testing.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-http-test-0.2
  (package
    (inherit rust-actix-http-test-1)
    (name "rust-actix-http-test-2")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-http-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m1ghgllf7ws5wk51x8phcdjq21phylawmvp7wb29zd1d0aw2aac"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.1)
        ("rust-actix-connect" ,rust-actix-connect-0.2)
        ("rust-actix-rt" ,rust-actix-rt-0.2)
        ("rust-actix-server" ,rust-actix-server-0.6)
        ("rust-actix-service" ,rust-actix-service-0.4)
        ("rust-actix-utils" ,rust-actix-utils-0.4)
        ("rust-awc" ,rust-awc-0.2)
        ("rust-base64" ,rust-base64-0.10)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6)
        ("rust-sha1" ,rust-sha1-0.6)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-time" ,rust-time-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2))))))

(define-public rust-actix-macros-0.2
  (package
    (name "rust-actix-macros")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jsmhq9k5nsms8sci2lqkihym5nrhlpfv8dgd0n4539g1cad67p0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `rustversion`
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://actix.rs")
    (synopsis "Actix runtime macros")
    (description "This package provides Actix runtime macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-macros-0.1
  (package
    (inherit rust-actix-macros-0.2)
    (name "rust-actix-macros")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mfnprr8gy1gb5xcr18iwsv781hysvh7sr5xxg6ghyi61gh8rjml"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=compile_macros")
       #:cargo-inputs
       (("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs
       (("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-trybuild" ,rust-trybuild-1))))))

(define-public rust-actix-router-0.5
  (package
    (name "rust-actix-router")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-router" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04f5cdag2h9lbrgb0pzwznpfrl3ajbdxlsvb8a2kci1rcmcpa96j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; cannot find derive macro `Deserialize` in this scope
       #:cargo-inputs (("rust-bytestring" ,rust-bytestring-0.1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/actix/actix-web")
    (synopsis "Resource path matching and router library")
    (description
     "This package provides resource path matching and router library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-router-0.2
  (package
    (inherit rust-actix-router-0.5)
    (name "rust-actix-router")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-router" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0df2626hk4n4yki6j88v3k0gcm8pi5hdnm1mldyvyi8nvbdzgldv"))))
    (arguments
     ;; Tests fail with "error[E0432]: unresolved import `serde_derive`".
     `(#:tests? #false
       #:cargo-inputs
       (("rust-bytestring" ,rust-bytestring-0.1)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1))))))

(define-public rust-actix-router-0.1
  (package
    (inherit rust-actix-router-0.2)
    (name "rust-actix-router")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-router" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xyc0kzawfwjfiw4znb7xx6hh4r7nnwjq44i08fjc1724ysln8i3"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-http" ,rust-http-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-string" ,rust-string-0.2))))))

(define-public rust-actix-rt-2
  (package
    (name "rust-actix-rt")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-rt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "078mjccgha4xlqk2hjb9hxfg26pmpra9v2h2w0m40gvx5102vwr8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `hyper`
       #:cargo-inputs (("rust-actix-macros" ,rust-actix-macros-0.2)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-uring" ,rust-tokio-uring-0.4))))
    (home-page "https://actix.rs")
    (synopsis "Actix runtime")
    (description "This package provides Actix runtime.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-rt-1
  (package
    (inherit rust-actix-rt-2)
    (name "rust-actix-rt")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-rt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09xj7pxy0ng13rd6hya1md98dhk0586p4bsfrwmxxlg028lwqgql"))))
    (arguments
     `(#:cargo-inputs
       (("rust-actix-macros" ,rust-actix-macros-0.1)
        ("rust-actix-threadpool" ,rust-actix-threadpool-0.3)
        ("rust-copyless" ,rust-copyless-0.1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-tokio" ,rust-tokio-0.2))))))

(define-public rust-actix-rt-0.2
  (package
    (inherit rust-actix-rt-1)
    (name "rust-actix-rt")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-rt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13h9dph54lhxlzcz6wxmsv96qqpbh1dzr4365gn84gb00qfxmjc8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-actix-threadpool" ,rust-actix-threadpool-0.1)
        ("rust-copyless" ,rust-copyless-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-tokio-current-thread" ,rust-tokio-current-thread-0.1)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2))))))

(define-public rust-actix-server-2
  (package
    (name "rust-actix-server")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m62qbg7vl1wddr6mm8sd4rnvd3w5v3zcn8fmdpfl8q4xxz3xc9y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; unresolved import `actix_codec`
       #:cargo-inputs (("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-uring" ,rust-tokio-uring-0.4)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://actix.rs")
    (synopsis "General purpose TCP server built for the Actix ecosystem")
    (description
     "This package provides a general purpose TCP server built for the Actix
ecosystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-server-1
  (package
    (inherit rust-actix-server-2)
    (name "rust-actix-server")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13khzd6pz9pqksxmw2syipfwq2gi5v9warx6pa24g8iccxp7wh25"))))
    (arguments
     ;; Tests fail with "error[E0432]: unresolved import `bytes`" error.
     `(#:tests? #false
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.3)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-utils" ,rust-actix-utils-2)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-mio-uds" ,rust-mio-uds-0.6)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-socket2" ,rust-socket2-0.3))))))

(define-public rust-actix-server-0.6
  (package
    (inherit rust-actix-server-1)
    (name "rust-actix-server")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19b2sl8dz01xfrynmf4iixq4a15g0gk1z43lby7762ldmws6aqnx"))))
    (arguments
     `(#:cargo-inputs
       (("rust-actix-rt" ,rust-actix-rt-0.2)
        ("rust-actix-server-config" ,rust-actix-server-config-0.1)
        ("rust-actix-service" ,rust-actix-service-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-mio-uds" ,rust-mio-uds-0.6)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.3)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.9)
        ("rust-tokio-signal" ,rust-tokio-signal-0.2)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2)
        ("rust-tokio-uds" ,rust-tokio-uds-0.2)
        ("rust-webpki" ,rust-webpki-0.19)
        ("rust-webpki-roots" ,rust-webpki-roots-0.16))
       #:cargo-development-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.1)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-env-logger" ,rust-env-logger-0.6))))))

(define-public rust-actix-server-0.5
  (package
    (inherit rust-actix-server-1)
    (name "rust-actix-server")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fyxkkgm3cbyzxgx0qw86i1dq9hrr891n1c7mc7450n8arir735s"))))
    (arguments
     `(#:cargo-inputs
       (("rust-actix-rt" ,rust-actix-rt-0.2)
        ("rust-actix-server-config" ,rust-actix-server-config-0.1)
        ("rust-actix-service" ,rust-actix-service-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.3)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.9)
        ("rust-tokio-signal" ,rust-tokio-signal-0.2)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2)
        ("rust-webpki" ,rust-webpki-0.19)
        ("rust-webpki-roots" ,rust-webpki-roots-0.16))
       #:cargo-development-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.1)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-env-logger" ,rust-env-logger-0.6))))))

(define-public rust-actix-server-config-0.1
  (package
    (name "rust-actix-server-config")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-server-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c7zp4l63n5skljbpq6j0a0avdjv6w067bdc5ca96bb8kjc38fj8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.3)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.9)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-uds" ,rust-tokio-uds-0.2))))
    (home-page "https://actix.rs")
    (synopsis "Actix server config utils")
    (description "Actix server config utils.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-service-2
  (package
    (name "rust-actix-service")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-service" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fipjcc5kma7j47jfrw55qm09dakgvx617jbriydrkqqz10lk29v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `actix_rt`
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis "Service trait and combinators for asynchronous request/response")
    (description
     "This package provides a service trait and combinators for representing
asynchronous request/response operations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-service-1
  (package
    (inherit rust-actix-service-2)
    (name "rust-actix-service")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-service" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fw2b1cpxrpqk778mpvxv0cazj0pwjyb6khzs4syhnqvb1fl6lh0"))))
    (arguments
     `(#:cargo-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-pin-project" ,rust-pin-project-0.4))
       #:cargo-development-inputs
       (("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-criterion" ,rust-criterion-0.3))))))

(define-public rust-actix-service-0.4
  (package
    (inherit rust-actix-service-1)
    (name "rust-actix-service")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-service" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gvpw11hcr1zmi5qzq3np6qzd0j51mdxn7yfgmzgyhc8ja7b99dw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.1))
       #:cargo-development-inputs
       (("rust-actix-rt" ,rust-actix-rt-0.2))))))

(define-public rust-actix-testing-1
  (package
    (name "rust-actix-testing")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-testing" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "073r3rlnz9km7w7zfhpj6snb453hhp7d354adbp79awrhyirq8s7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-actix-macros" ,rust-actix-macros-0.1)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-server" ,rust-actix-server-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-socket2" ,rust-socket2-0.3))))
    (home-page "https://actix.rs")
    (synopsis "Actix testing utils")
    (description "This package provides Actix testing utils.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-testing-0.1
  (package
    (inherit rust-actix-testing-1)
    (name "rust-actix-testing")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-testing" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w9p7wv2n2wda8ph3ahp8fqslmbh12vs206l4i49jl37mjbiw05g"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-rt" ,rust-actix-rt-0.2)
        ("rust-actix-server" ,rust-actix-server-0.6)
        ("rust-actix-server-config" ,rust-actix-server-config-0.1)
        ("rust-actix-service" ,rust-actix-service-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1))))))

(define-public rust-actix-test-server-0.2
  (package
    (name "rust-actix-test-server")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-test-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lqx8adsl3nlhbnvvjrmy9mkfa0d8wmwyy4gdz5ik8xhbwibxnn2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-actix-rt" ,rust-actix-rt-0.2)
        ("rust-actix-server" ,rust-actix-server-0.5)
        ("rust-actix-server-config" ,rust-actix-server-config-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.9)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-webpki" ,rust-webpki-0.19)
        ("rust-webpki-roots" ,rust-webpki-roots-0.16))
       #:cargo-development-inputs
       (("rust-actix-service" ,rust-actix-service-0.4))))
    (home-page "https://actix.rs")
    (synopsis "Actix test server")
    (description "Actix test server.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-threadpool-0.3
  (package
    (name "rust-actix-threadpool")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-threadpool" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c0frk19ml94d01mvgv5g60mhq86gfi34c3lsfpvjm18016z02fj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-threadpool" ,rust-threadpool-1))))
    (home-page "https://actix.rs")
    (synopsis "Actix thread pool for sync code")
    (description "This package provides Actix thread pool for sync code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-threadpool-0.1
  (package
    (inherit rust-actix-threadpool-0.3)
    (name "rust-actix-threadpool")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-threadpool" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pfk6k09cdw0w63wh8glqm6bvqz0hlqwhyqvdfw6yzns2dfyhnkb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-derive-more" ,rust-derive-more-0.15)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-parking-lot" ,rust-parking-lot-0.9)
        ("rust-threadpool" ,rust-threadpool-1))))))

(define-public rust-actix-tls-3
  (package
    (name "rust-actix-tls")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pxyqpr5nangbl55gdc5zpc84viah7qhmpjw5v3pnirb5w5fdk6l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `actix_server`
       #:cargo-inputs (("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http" ,rust-http-1)
                       ("rust-impl-more" ,rust-impl-more-0.1)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis "TLS acceptor services for Actix ecosystem")
    (description
     "This package provides TLS acceptor services for Actix ecosystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-tls-2
  (package
    (inherit rust-actix-tls-3)
    (name "rust-actix-tls")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yqmlyn02c72a1rrmjkfx5hnz286130y3sq4ll1mbkv1fdyrny14"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.3)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-utils" ,rust-actix-utils-2)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.4)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.14)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.20))))))

(define-public rust-actix-tls-1
  (package
    (inherit rust-actix-tls-2)
    (name "rust-actix-tls")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a4m96jz6vzmknpk5m803c337c6dillnqq4w71nrlphhmzxb9rd4"))))
    (arguments
     `(#:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.2)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-utils" ,rust-actix-utils-1)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-either" ,rust-either-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.4)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.17))
       #:cargo-development-inputs
       (("rust-actix-testing" ,rust-actix-testing-1))))))

(define-public rust-actix-utils-3
  (package
    (name "rust-actix-utils")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n05nzwdkx6jhmzr6f9qsh57a8hqlwv5rjz1i0j3qvj6y7gxr8c8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `futures_util`
       #:cargo-inputs (("rust-local-waker" ,rust-local-waker-0.1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis "Network related services and utilities for the Actix ecosystem")
    (description
     "This package provides various network related services and utilities for
the Actix ecosystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-utils-2
  (package
    (inherit rust-actix-utils-3)
    (name "rust-actix-utils")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nkby6wpwcmjr3zcghd962l2hyjry0aayncyjzbx2ck6qpg2541f"))))
    (arguments
     `(#:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.3)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-either" ,rust-either-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-sink" ,rust-futures-sink-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-slab" ,rust-slab-0.4))))))

(define-public rust-actix-utils-1
  (package
    (inherit rust-actix-utils-2)
    (name "rust-actix-utils")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kkz2hfz8r2k1gxcjk2qq1h1qxlb487g023q4v1dw6ph3dizby7w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.2)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-either" ,rust-either-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-slab" ,rust-slab-0.4))))))

(define-public rust-actix-utils-0.4
  (package
    (inherit rust-actix-utils-2)
    (name "rust-actix-utils")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15bv06z7pccnmh067l5zj0fvpmfagnil7lvznnl3fp4gjh4k334h"))))
    (arguments
     `(#:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.1)
        ("rust-actix-service" ,rust-actix-service-0.4)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-either" ,rust-either-1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-tokio-current-thread" ,rust-tokio-current-thread-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2))
       #:cargo-development-inputs
       (("rust-actix-rt" ,rust-actix-rt-0.2))))))

(define-public rust-actix-web-4
  (package
    (name "rust-actix-web")
    (version "4.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1159grrp031zy9j97vr3c376w8pdawr8akbib0iqqqxvvrnmb9j3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; could not find `brotli` in the list of imported crates
       #:cargo-inputs (("rust-actix-codec" ,rust-actix-codec-0.5)
                       ("rust-actix-http" ,rust-actix-http-3)
                       ("rust-actix-macros" ,rust-actix-macros-0.2)
                       ("rust-actix-router" ,rust-actix-router-0.5)
                       ("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-server" ,rust-actix-server-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-tls" ,rust-actix-tls-3)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-actix-web-codegen" ,rust-actix-web-codegen-4)
                       ("rust-ahash" ,rust-ahash-0.8)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-bytestring" ,rust-bytestring-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cookie" ,rust-cookie-0.16)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-language-tags" ,rust-language-tags-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2))))
    (native-inputs (list pkg-config))
    (inputs (list (list zstd "lib")))
    (home-page "https://actix.rs")
    (synopsis "Powerful, pragmatic, and fast web framework for Rust")
    (description
     "Actix Web is a powerful, pragmatic, and fast web framework for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-web-3
  (package
    (inherit rust-actix-web-4)
    (name "rust-actix-web")
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11kv8y1p9dw78lnhrw3rqavhmazmy7s0z8j14a3a1yp7fahx8hg6"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.3)
        ("rust-actix-http" ,rust-actix-http-2)
        ("rust-actix-macros" ,rust-actix-macros-0.1)
        ("rust-actix-router" ,rust-actix-router-0.2)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-server" ,rust-actix-server-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-testing" ,rust-actix-testing-1)
        ("rust-actix-threadpool" ,rust-actix-threadpool-0.3)
        ("rust-actix-tls" ,rust-actix-tls-2)
        ("rust-actix-utils" ,rust-actix-utils-2)
        ("rust-actix-web-codegen" ,rust-actix-web-codegen-0.4)
        ("rust-awc" ,rust-awc-2)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-fxhash" ,rust-fxhash-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-pin-project" ,rust-pin-project-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
        ("rust-socket2" ,rust-socket2-0.3)
        ("rust-time" ,rust-time-0.2)
        ("rust-tinyvec" ,rust-tinyvec-1)
        ("rust-url" ,rust-url-2))))))

(define-public rust-actix-web-2
  (package
    (inherit rust-actix-web-3)
    (name "rust-actix-web")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dgnn7xiw2yhvrx7l7b57gwra7yfqawka5xz1lpq4h0h8qifhn1i"))))
    (arguments
     ;; XXX: The crate fails to't build with with the same error as
     ;; rust-actix-connect.  Skip build for now.
     `(#:skip-build? #true
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.2)
        ("rust-actix-http" ,rust-actix-http-1)
        ("rust-actix-macros" ,rust-actix-macros-0.1)
        ("rust-actix-router" ,rust-actix-router-0.2)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-server" ,rust-actix-server-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-actix-testing" ,rust-actix-testing-1)
        ("rust-actix-threadpool" ,rust-actix-threadpool-0.3)
        ("rust-actix-tls" ,rust-actix-tls-1)
        ("rust-actix-utils" ,rust-actix-utils-1)
        ("rust-actix-web-codegen" ,rust-actix-web-codegen-0.2)
        ("rust-awc" ,rust-awc-1)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-fxhash" ,rust-fxhash-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-regex" ,rust-regex-1)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6)
        ("rust-time" ,rust-time-0.1)
        ("rust-url" ,rust-url-2))))))

(define-public rust-actix-web-1
  (package
    (inherit rust-actix-web-3)
    (name "rust-actix-web")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00wvayn7v2s61hylisr53f48s2bzg8jp3bmrqh1vkb6vgjb1nfmg"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.1)
        ("rust-actix-http" ,rust-actix-http-0.2)
        ("rust-actix-router" ,rust-actix-router-0.1)
        ("rust-actix-rt" ,rust-actix-rt-0.2)
        ("rust-actix-server" ,rust-actix-server-0.6)
        ("rust-actix-server-config" ,rust-actix-server-config-0.1)
        ("rust-actix-service" ,rust-actix-service-0.4)
        ("rust-actix-testing" ,rust-actix-testing-0.1)
        ("rust-actix-threadpool" ,rust-actix-threadpool-0.1)
        ("rust-actix-utils" ,rust-actix-utils-0.4)
        ("rust-actix-web-codegen" ,rust-actix-web-codegen-0.1)
        ("rust-awc" ,rust-awc-0.2)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-derive-more" ,rust-derive-more-0.15)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-hashbrown" ,rust-hashbrown-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-parking-lot" ,rust-parking-lot-0.9)
        ("rust-regex" ,rust-regex-1)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6)
        ("rust-time" ,rust-time-0.1)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-actix" ,rust-actix-0.8)
        ("rust-actix-connect" ,rust-actix-connect-0.2)
        ("rust-actix-http-test" ,rust-actix-http-test-0.2)
        ("rust-brotli2" ,rust-brotli2-0.3)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2))))))

(define-public rust-actix-web-codegen-4
  (package
    (name "rust-actix-web-codegen")
    (version "4.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xalrv1s7imzfgxyql6zii5bpxxkk11rlcc8n4ia3v1hpgmm07zb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `rustversion`
       #:cargo-inputs (("rust-actix-router" ,rust-actix-router-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://actix.rs")
    (synopsis "Routing and runtime macros for Actix Web")
    (description
     "This package provides routing and runtime macros for Actix Web.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-web-codegen-0.4
  (package
    (inherit rust-actix-web-codegen-4)
    (name "rust-actix-web-codegen")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ys3f6q0hgflqvp271s49q88m41db3iynm7ydxy0wgikjdqgf9md"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-actix-web-codegen-0.2
  (package
    (inherit rust-actix-web-codegen-0.4)
    (name "rust-actix-web-codegen")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rjpzwsm51nfjqsz269jwbkiic9d454bnsk9ng882wp0rdsz86x7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs
       (("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-web" ,rust-actix-web-3))))))

(define-public rust-actix-web-codegen-0.1
  (package
    (inherit rust-actix-web-codegen-0.4)
    (name "rust-actix-web-codegen")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1frs0id6k1vjczhnfhwh8q8birp27imlvgi6jylfxh911r9372h6"))))
    (arguments
     `(#:tests? #f      ; cannot subtract `chrono::Duration` from `Tm`
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs
       (("rust-actix-http" ,rust-actix-http-0.2)
        ("rust-actix-http-test" ,rust-actix-http-test-0.2)
        ("rust-actix-web" ,rust-actix-web-1)
        ("rust-futures" ,rust-futures-0.1))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))))

(define-public rust-ammonia-4
  (package
    (name "rust-ammonia")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ammonia" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nd432yg1cl9kj4i9c37a9hvwffayqh6zsvb4fmh31g5bsp9xf8s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("html5ever" ,rust-html5ever-0.27)
                       ("maplit" ,rust-maplit-1)
                       ("tendril" ,rust-tendril-0.4)
                       ("url" ,rust-url-2)
                       ("once_cell" ,rust-once-cell-1))
       #:cargo-development-inputs (("version-sync" ,rust-version-sync-0.9)
                                   ("env_logger" ,rust-env-logger-0.10))))
    (home-page "https://github.com/rust-ammonia/ammonia")
    (synopsis "Repair and secure untrusted HTML")
    (description "Ammonia is a whitelist-based HTML sanitization library.
It is designed to prevent cross-site scripting, layout breaking,
and clickjacking caused by untrusted user-provided HTML
being mixed into a larger web page.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-h1-2
  (package
    (name "rust-async-h1")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-h1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p6ls50p1ixnxrhmx4sdb5d2kqrl1kfhcb0lkqlhzzqjz1sqmip5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-async-channel" ,rust-async-channel-1)
        ("rust-async-dup" ,rust-async-dup-1)
        ("rust-async-std" ,rust-async-std-1)
        ("rust-byte-pool" ,rust-byte-pool-0.2)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-http-types" ,rust-http-types-2)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-pin-project" ,rust-pin-project-1))))
    (home-page "https://github.com/http-rs/async-h1")
    (synopsis "Asynchronous HTTP 1.1 parser")
    (description
     "This package provides an asynchronous HTTP 1.1 parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-awc-2
  (package
    (name "rust-awc")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "awc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14g6m53zmxw3f1sf990l7ps3w2fq2c29n1slpizc7kxhwy8f90dk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.3)
        ("rust-actix-http" ,rust-actix-http-2)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7))))
    (home-page "https://actix.rs")
    (synopsis "Async HTTP and WebSocket client library")
    (description
     "This package provides async HTTP and WebSocket client library
built on the Actix ecosystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-awc-1
  (package
    (inherit rust-awc-2)
    (name "rust-awc")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "awc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1idacmq7n3irmdjkbxc5kdwspxk9w1gip94pcmfk7wky3m6isq6p"))))
    ;; XXX: The crate fails to't build with with the same error as
    ;; rust-actix-connect.  Skip build for now.
    (arguments
     `(#:skip-build? #true
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.2)
        ("rust-actix-http" ,rust-actix-http-1)
        ("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-actix-service" ,rust-actix-service-1)
        ("rust-base64" ,rust-base64-0.11)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6))
       #:cargo-development-inputs
       (("rust-actix-http-test" ,rust-actix-http-test-1)
        ("rust-actix-web" ,rust-actix-web-2)
        ("rust-brotli" ,rust-brotli-3))))))

(define-public rust-awc-0.2
  (package
    (inherit rust-awc-2)
    (name "rust-awc-2")
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "awc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i5qinnh37bwpx86m2yyq1q1bnsa31vlwlz7942bzlwd4y1m56ay"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-actix-codec" ,rust-actix-codec-0.1)
        ("rust-actix-http" ,rust-actix-http-0.2)
        ("rust-actix-service" ,rust-actix-service-0.4)
        ("rust-base64" ,rust-base64-0.10)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-derive-more" ,rust-derive-more-0.15)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2))))))

(define-public rust-axum-0.7
  (package
    (name "rust-axum")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17kv7v8m981cqmfbv5m538fzxhw51l9bajv06kfddi7njarb8dhj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum-core" ,rust-axum-core-0.4)
                       ("rust-axum-macros" ,rust-axum-macros-0.4)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-matchit" ,rust-matchit-0.7)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-multer" ,rust-multer-3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.21)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-http" ,rust-tower-http-0.5)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-axum-macros" ,rust-axum-macros-0.4)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.21)
        ("rust-tower" ,rust-tower-0.4)
        ("rust-tower-http" ,rust-tower-http-0.5)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Web framework that focuses on ergonomics and modularity")
    (description "Web framework that focuses on ergonomics and modularity.")
    (license license:expat)))

(define-public rust-axum-0.6
  (package
    (inherit rust-axum-0.7)
    (name "rust-axum")
    (version "0.6.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gynqkg3dcy1zd7il69h8a3zax86v6qq5zpawqyn87mr6979x0iv"))))
    (arguments
     `(#:cargo-test-flags '("--release" "--lib" "--bins" "--tests" "--"
                            "--skip=routing::tests::logging_rejections")
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum-core" ,rust-axum-core-0.3)
                       ("rust-axum-macros" ,rust-axum-macros-0.3)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-matchit" ,rust-matchit-0.7)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.20)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-http" ,rust-tower-http-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-axum-macros" ,rust-axum-macros-0.3)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-tower" ,rust-tower-0.4)
        ("rust-tower-http" ,rust-tower-http-0.4)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-uuid" ,rust-uuid-1))))))

(define-public rust-axum-core-0.4
  (package
    (name "rust-axum-core")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qx28wg4j6qdcdrisqwyaavlzc0zvbsrcwa99zf9456lfbyn6p51"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       ;; The doc tests contain references to axum_extra.
       '("--release" "--lib" "--bins" "--tests")
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
                       ("rust-tower-http" ,rust-tower-http-0.5)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.7)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-hyper" ,rust-hyper-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower-http" ,rust-tower-http-0.5))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Core types and traits for @code{axum}")
    (description "Core types and traits for @code{axum}.")
    (license license:expat)))

(define-public rust-axum-core-0.3
  (package
    (inherit rust-axum-core-0.4)
    (name "rust-axum-core")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b1d9nkqb8znaba4qqzxzc968qwj4ybn4vgpyz9lz4a7l9vsb7vm"))))
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-tower-http" ,rust-tower-http-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.6)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-hyper" ,rust-hyper-0.14)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower-http" ,rust-tower-http-0.4))))))

(define-public rust-axum-macros-0.4
  (package
    (name "rust-axum-macros")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ahfy8z5z2xlibj41gyr2jm84di1x1s660g13jcfajh15pp5bh00"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Tests unexpectedly pass.
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.7)
                                   ("rust-axum-extra" ,rust-axum-extra-0.9)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-syn" ,rust-syn-2)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Macros for axum")
    (description "This package provides macros for axum.")
    (license license:expat)))

(define-public rust-axum-extra-0.9
  (package
    (name "rust-axum-extra")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-extra" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13yxdzc0jj9d2q4yqf13qs4psrx9l86vk3zn79vifrh1f8pz8pw9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-axum" ,rust-axum-0.7)
                       ("rust-axum-core" ,rust-axum-core-0.4)
                       ("rust-axum-macros" ,rust-axum-macros-0.4)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cookie" ,rust-cookie-0.18)
                       ("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.4)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-multer" ,rust-multer-3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-prost" ,rust-prost-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-html-form" ,rust-serde-html-form-0.2)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.7)
                                   ("rust-hyper" ,rust-hyper-1)
                                   ("rust-reqwest" ,rust-reqwest-0.11)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower" ,rust-tower-0.4)
                                   ("rust-tower-http" ,rust-tower-http-0.5))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Extra utilities for axum")
    (description "Extra utilities for axum.")
    (license license:expat)))

(define-public rust-axum-extra-0.7
  (package
    (inherit rust-axum-extra-0.9)
    (name "rust-axum-extra")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-extra" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gr8mdpi5xfq964zhaygap0qjvzwspvj8fdg41rp6b1qx4xl6gm9"))))
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-axum" ,rust-axum-0.6)
                       ("rust-axum-core" ,rust-axum-core-0.3)
                       ("rust-axum-macros" ,rust-axum-macros-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cookie" ,rust-cookie-0.17)
                       ("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-html-form" ,rust-serde-html-form-0.2)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.6)
                                   ("rust-axum-macros" ,rust-axum-macros-0.3)
                                   ("rust-http-body" ,rust-http-body-0.4)
                                   ("rust-hyper" ,rust-hyper-0.14)
                                   ("rust-reqwest" ,rust-reqwest-0.11)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower" ,rust-tower-0.4)
                                   ("rust-tower-http" ,rust-tower-http-0.4))))))

(define-public rust-axum-macros-0.3
  (package
    (name "rust-axum-macros")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qkb5cg06bnp8994ay0smk57shd5hpphcmp90kd7p65dxh86mjnd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=debug_handler")
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.6)
                                   ("rust-axum-extra" ,rust-axum-extra-0.7)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-syn" ,rust-syn-2)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Macros for @code{axum}")
    (description "Macros for @code{axum}.")
    (license license:expat)))

(define-public rust-chardetng-0.1
  (package
    (name "rust-chardetng")
    (version "0.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chardetng" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1spikjcnblwa5n1nnk46fxkwn86yfiqxgs47h4yaw23vbfvg1f0l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; cannot find macro `println` in this scope
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-rayon" ,rust-rayon-1))
       #:cargo-development-inputs (("rust-detone" ,rust-detone-1))))
    (home-page "https://docs.rs/chardetng/")
    (synopsis "Character encoding detector for legacy Web content")
    (description
     "This package provides a character encoding detector for legacy Web content.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-chunked-transfer-1
  (package
    (name "rust-chunked-transfer")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chunked_transfer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "00a9h3csr1xwkqrzpz5kag4h92zdkrnxq4ppxidrhrx29syf6kbf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/frewsxcv/rust-chunked-transfer")
    (synopsis "Encoder and decoder for HTTP chunked transfer coding")
    (description "This package provides an encoder and decoder for HTTP chunked
transfer coding.")
    (license license:asl2.0)))

(define-public rust-chunked-transfer-0.3
  (package
    (inherit rust-chunked-transfer-1)
    (name "rust-chunked-transfer")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chunked_transfer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11yghnd24w0i9p8g368c3pg7qh9nfz7kgri6pywja9pnmakj13a9"))))
    (arguments `())))

(define-public rust-gemini-fetch-0.2
  (package
    (name "rust-gemini-fetch")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gemini-fetch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1851jk1a7w6pcb9qcgmgp7vvg3dfihwr7ldlh5k9qxmyaymdy4ma"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rustls" ,rust-rustls-0.19)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
                       ("rust-url" ,rust-url-2)
                       ("rust-webpki" ,rust-webpki-0.21)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.21)
                       ("rust-x509-signature" ,rust-x509-signature-0.5))))
    (home-page "https://github.com/jsvana/gemini-fetch/")
    (synopsis "Fetch gemini pages")
    (description "This package provides a library to fetch gemini pages.")
    (license license:expat)))

(define-public rust-h2-0.4
  (package
    (name "rust-h2")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sc0ymhiqp4hbz39d405cjbga77wnz2pprbgyc498xs58hlwfvl1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-webpki-roots" ,rust-webpki-roots-0.25))))
    (home-page "https://github.com/hyperium/h2")
    (synopsis "HTTP/2 client and server")
    (description "This package provides an HTTP/2 client and server.")
    (license license:expat)))

(define-public rust-h2-0.3
  (package
    (inherit rust-h2-0.4)
    (name "rust-h2")
    (version "0.3.25")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sqq8rnv3vrwra7wzgc6099j9zw80rnm9aslhsa8d674qlh2igag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                                   ("rust-walkdir" ,rust-walkdir-2)
                                   ("rust-webpki-roots" ,rust-webpki-roots-0.25))))))

(define-public rust-h2-0.2
  (package
    (inherit rust-h2-0.3)
    (name "rust-h2")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0dd5jyxmmy88pdmvag7n41k9z1qs6sliagcyx4jss5292byjhisy"))))
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-sink" ,rust-futures-sink-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-util" ,rust-tokio-util-0.3)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-futures" ,rust-tracing-futures-0.2))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-hex" ,rust-hex-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.4)
        ("rust-rand" ,rust-rand-0.3)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-walkdir" ,rust-walkdir-1)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.17))))))

(define-public rust-h2-0.1
  (package
    (inherit rust-h2-0.2)
    (name "rust-h2")
    (version "0.1.26")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qn457y8xh03p7c7cpk76r22gqpyqxc58g5022j3iya7d0j4rcx5"))))
    (arguments
     `(#:skip-build? #t ;; TODO missing indirect dependency
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-string" ,rust-string-0.2)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-hex" ,rust-hex-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.4)
        ("rust-rand" ,rust-rand-0.3)
        ;;("rust-rustls" ,rust-rustls-0.12) requires 0.5
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-walkdir" ,rust-walkdir-1)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.17))))))

(define-public rust-h3-0.0.4
  (package
    (name "rust-h3")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04clhh6b5iqlgnbppikbz4zpxl78g4vkyhyrjgnyg4vfkrmqij5i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;not all files included
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-fastrand" ,rust-fastrand-2)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-1)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-assert-matches" ,rust-assert-matches-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-quinn" ,rust-quinn-0.10)
        ("rust-quinn-proto" ,rust-quinn-proto-0.10)
        ("rust-rcgen" ,rust-rcgen-0.11)
        ("rust-rustls" ,rust-rustls-0.21)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "Async HTTP/3 implementation")
    (description "This package provides an async HTTP/3 implementation.")
    (license license:expat)))

(define-public rust-h3-0.0.3
  (package
    (inherit rust-h3-0.0.4)
    (name "rust-h3")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "101vg73galsyk5gnjb49cjb6q40c9z2npcdxpfsj99ky2waijgmq"))))
    (arguments
     `(#:tests? #f                      ; Not all files included
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-fastrand" ,rust-fastrand-2)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-assert-matches" ,rust-assert-matches-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-quinn" ,rust-quinn-0.10)
        ("rust-quinn-proto" ,rust-quinn-proto-0.10)
        ("rust-rcgen" ,rust-rcgen-0.11)
        ("rust-rustls" ,rust-rustls-0.21)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))))

(define-public rust-h3-0.0.2
  (package
    (inherit rust-h3-0.0.3)
    (name "rust-h3")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17nbmkz6xs848257xv7gdhrnhyhagfb0dbqla82zv1nixr1wmrkd"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fastrand" ,rust-fastrand-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))))

(define-public rust-h3-quinn-0.0.5
  (package
    (name "rust-h3-quinn")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3-quinn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ii06bi5a19k4qfkppn5019nw8xca2wzfl66cax949jc1v66ny3k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-h3" ,rust-h3-0.0.4)
        ("rust-quinn" ,rust-quinn-0.10)
        ("rust-quinn-proto" ,rust-quinn-proto-0.10)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "QUIC transport implementation based on Quinn")
    (description
     "This package provides QUIC transport implementation based on Quinn.")
    (license license:expat)))

(define-public rust-h3-quinn-0.0.4
  (package
    (inherit rust-h3-quinn-0.0.5)
    (name "rust-h3-quinn")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3-quinn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r0sm0j51crlfpy2j1wfhgpg2lrfq2xmf5qjd98ksg3h9l0pb5mc"))))
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-h3" ,rust-h3-0.0.3)
                       ("rust-quinn" ,rust-quinn-0.10)
                       ("rust-quinn-proto" ,rust-quinn-proto-0.10)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))))

(define-public rust-h3-quinn-0.0.3
  (package
    (inherit rust-h3-quinn-0.0.4)
    (name "rust-h3-quinn")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3-quinn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kf6bqmm751gwj24dqgb2rrwq8ibhv7z5v7ix4pfiwz4ccbiljid"))))
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-h3" ,rust-h3-0.0.2)
                       ("rust-quinn" ,rust-quinn-0.10)
                       ("rust-quinn-proto" ,rust-quinn-proto-0.10)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))))

(define-public rust-headers-0.4
  (package
    (name "rust-headers")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "headers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1abari69kjl2yv2dg06g2x17qgd1a20xp7aqmmg2vfhcppk0c89j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-headers-core" ,rust-headers-core-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-sha1" ,rust-sha1-0.10))))
    (home-page "https://hyper.rs")
    (synopsis "Typed HTTP headers")
    (description "This package provides typed HTTP headers.")
    (license license:expat)))

(define-public rust-headers-0.3
  (package
    (inherit rust-headers-0.4)
    (name "rust-headers")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "headers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w62gnwh2p1lml0zqdkrx9dp438881nhz32zrzdy61qa0a9kns06"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.21)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-headers-core" ,rust-headers-core-0.2)
        ("rust-http" ,rust-http-0.2)
        ("rust-httpdate" ,rust-httpdate-1)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-sha1" ,rust-sha1-0.10))))))

(define-public rust-headers-0.2
  (package
    (inherit rust-headers-0.3)
    (name "rust-headers")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "headers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hmnrra00cjqpsn05klnr9cysrv2bm19akxl5lncwcrgfbcafb48"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-headers-core" ,rust-headers-core-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-sha-1" ,rust-sha-1-0.8)
        ("rust-time" ,rust-time-0.1))))))

(define-public rust-headers-core-0.3
  (package
    (name "rust-headers-core")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "headers-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r1w80i2bhmyh8s5mjr2dz6baqlrm6cak6yvzm4jq96lacjs5d2l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-http" ,rust-http-1))))
    (home-page "https://hyper.rs")
    (synopsis "Typed HTTP headers core trait")
    (description "This package provides typed HTTP headers core trait.")
    (license license:expat)))

(define-public rust-headers-core-0.2
  (package
    (inherit rust-headers-core-0.3)
    (name "rust-headers-core")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "headers-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ab469xfpd411mc3dhmjhmzrhqikzyj8a17jn5bkj9zfpy0n9xp7"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-http" ,rust-http-0.2))))))

(define-public rust-headers-core-0.1
  (package
    (inherit rust-headers-core-0.2)
    (name "rust-headers-core")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "headers-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ds20kg0igncs2r0jrcf26mq72k3j6ilanr0qwh7r7xak8kk2wcn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-http" ,rust-http-0.1))))))

(define-public rust-hickory-client-0.24
  (package
    (name "rust-hickory-client")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hickory-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bj6g69h86d7mbclrwaj7cgl1plr6pvllv8qn69xmpgh9h90hgkz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--lib" "--bins" "--tests" "--"
         ;; Some tests require network access.
         "--skip=client::async_client::tests::async_client")
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hickory-proto" ,rust-hickory-proto-0.24)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-radix-trie" ,rust-radix-trie-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (native-inputs
     (list openssl pkg-config))
    (home-page "https://hickory-dns.org/")
    (synopsis "Client library for Hickory DNS, with DNSSEC support")
    (description
     "Hickory DNS is a safe and secure DNS library.  This is the Client
library with DNSSEC support.  DNSSEC with NSEC validation for negative
records, is complete.  The client supports dynamic DNS with SIG0 authenticated
requests, implementing easy to use high level funtions.  Hickory DNS is based
on the Tokio and Futures libraries, which means it should be easily integrated
into other software that also use those libraries.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hickory-proto-0.24
  (package
    (name "rust-hickory-proto")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hickory-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kxvdzmwwi1d6wsp9bcs91ipmwxcyhpwbvz3an9h0q28ryy6y6h9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-enum-as-inner" ,rust-enum-as-inner-0.6)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-h3" ,rust-h3-0.0.2)
                       ("rust-h3-quinn" ,rust-h3-quinn-0.0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-idna" ,rust-idna-0.4)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-quinn" ,rust-quinn-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tinyvec" ,rust-tinyvec-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25))
       #:cargo-development-inputs
       (("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (native-inputs
     (list openssl pkg-config))
    (home-page "https://hickory-dns.org/")
    (synopsis
     "Foundational DNS protocol library for all Hickory DNS projects")
    (description
     "Hickory DNS is a safe and secure DNS library.  This is the foundational
DNS protocol library for all Hickory DNS projects.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hickory-recursor-0.24
  (package
    (name "rust-hickory-recursor")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hickory-recursor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "146ifrml22hjydrw16qgfw32kv3v9wvvl4dqh45pg6fymxvw8xgi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-recursion" ,rust-async-recursion-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-enum-as-inner" ,rust-enum-as-inner-0.6)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hickory-proto" ,rust-hickory-proto-0.24)
                       ("rust-hickory-resolver" ,rust-hickory-resolver-0.24)
                       ("rust-lru-cache" ,rust-lru-cache-0.1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://hickory-dns.org/")
    (synopsis
     "Hickory DNS Recursor is a DNS recursive resolver with DNSSEC support")
    (description
     "*WARNING* This library is experimental

Hickory DNS Recursor is a safe and secure DNS recursive resolver with DNSSEC
support.  Hickory DNS is based on the Tokio and Futures libraries, which means
it should be easily integrated into other software that also use those
libraries.  This library can be used as in the server and binary for
performing recursive lookups.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hickory-resolver-0.24
  (package
    (name "rust-hickory-resolver")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hickory-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s3486qczv9gaw8dap06c0bwb2bpqm23a0ihj169hsjf2qhz1f1m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--lib" "--bins" "--tests" "--"
         ;; Some tests require network access.
         "--skip=async_resolver::tests::test_domain_search"
         "--skip=async_resolver::tests::test_fqdn"
         "--skip=async_resolver::tests::test_idna"
         "--skip=async_resolver::tests::test_large_ndots"
         "--skip=async_resolver::tests::test_lookup_cloudflare"
         "--skip=async_resolver::tests::test_lookup_google"
         "--skip=async_resolver::tests::test_lookup_quad9"
         "--skip=async_resolver::tests::test_ndots"
         "--skip=async_resolver::tests::test_search_list"
         "--skip=hosts::tests::test_read_hosts_conf"
         "--skip=name_server::name_server::tests::test_name_server"
         "--skip=name_server::name_server_pool::tests::test_multi_use_conns"
         "--skip=resolver::tests::test_lookup"
         "--skip=system_conf::unix::tests::test_read_resolv_conf")
         #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hickory-proto" ,rust-hickory-proto-0.24)
                       ("rust-ipconfig" ,rust-ipconfig-0.3)
                       ("rust-lru-cache" ,rust-lru-cache-0.1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-resolv-conf" ,rust-resolv-conf-0.7)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25))
       #:cargo-development-inputs
       (("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://hickory-dns.org/")
    (synopsis
     "Hickory DNS Resolver library built on top of tokio's @code{async-io}")
    (description
     "Hickory DNS Resolver is a safe and secure DNS library.  The Resolver is
intended to be a high-level library for any DNS record resolution, see
@code{Resolver} and @code{AsyncResolver} for supported resolution types.  The
@code{Client} can be used for other queries.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hickory-server-0.24
  (package
    (name "rust-hickory-server")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hickory-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dbwsrmxgf9nxkn3cp5fpjw96wywrzihbirjax25dd6wqidv9fsg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=store::file::authority::tests::test_load_zone"
         "--skip=basic::file::test_a_lookup"
         "--skip=basic::file::test_aname"
         "--skip=basic::file::test_aname_a_lookup"
         "--skip=basic::file::test_aname_chain"
         "--skip=basic::file::test_cname"
         "--skip=basic::file::test_cname_alias"
         "--skip=basic::file::test_cname_chain"
         "--skip=basic::file::test_dots_in_name"
         "--skip=basic::file::test_invalid_lookup"
         "--skip=basic::file::test_mx"
         "--skip=basic::file::test_mx_to_null"
         "--skip=basic::file::test_ns"
         "--skip=basic::file::test_ns_lookup"
         "--skip=basic::file::test_soa"
         "--skip=basic::file::test_srv"
         "--skip=basic::file::test_update_errors"
         "--skip=basic::file::test_wildcard"
         "--skip=basic::file::test_wildcard_chain"
         "--skip=test_all_lines_are_loaded"
         "--skip=test_implicit_in_class"
         "--skip=test_ttl_wilcard")
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-basic-toml" ,rust-basic-toml-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-enum-as-inner" ,rust-enum-as-inner-0.6)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-h3" ,rust-h3-0.0.2)
                       ("rust-h3-quinn" ,rust-h3-quinn-0.0.3)
                       ("rust-hickory-proto" ,rust-hickory-proto-0.24)
                       ("rust-hickory-recursor" ,rust-hickory-recursor-0.24)
                       ("rust-hickory-resolver" ,rust-hickory-resolver-0.24)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-rusqlite" ,rust-rusqlite-0.29)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://hickory-dns.org/")
    (synopsis "Hickory DNS Server is a DNS server with DNSSEC support")
    (description
     "Hickory DNS Server is a safe and secure DNS server with DNSSEC support.
Eventually this could be a replacement for BIND9.  The DNSSEC support allows
for live signing of all records, in it does not currently support records
signed offline.  The server supports dynamic DNS with SIG0 authenticated
requests.  Hickory DNS is based on the Tokio and Futures libraries, which
means it should be easily integrated into other software that also use those
libraries.")
    (license (list license:expat license:asl2.0))))

(define-public rust-http-1
  (package
    (name "rust-http")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sllw565jn8r5w7h928nsfqq33x586pyasdfr7vid01scwwgsamk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-itoa" ,rust-itoa-1))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-indexmap" ,rust-indexmap-1.8)
                                   ("rust-quickcheck" ,rust-quickcheck-0.9)
                                   ("rust-rand" ,rust-rand-0.7)
                                   ("rust-seahash" ,rust-seahash-3)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/hyperium/http")
    (synopsis "Set of types for representing HTTP requests and responses")
    (description "This package provides a set of types for representing HTTP
requests and responses.")
    (license (list license:expat license:asl2.0))))

(define-public rust-http-0.2
  (package
    (inherit rust-http-1)
    (name "rust-http")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fwz3mhh86h5kfnr5767jlx9agpdggclq7xsqx930fflzakb2iw9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-itoa" ,rust-itoa-1))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-indexmap" ,rust-indexmap-1.8)
        ("rust-quickcheck" ,rust-quickcheck-0.9)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-seahash" ,rust-seahash-3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-http-0.1
  (package
    (inherit rust-http-0.2)
    (name "rust-http")
    (version "0.1.21")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1w21xnhd8f48zvbmm5njg2y1nb4p08ppn8r0cs2xi5d8wgnzbk6n"))))
    (arguments
     `(#:tests? #f          ; doc tests fail
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-itoa" ,rust-itoa-0.4))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-indexmap" ,rust-indexmap-1.8)
        ("rust-quickcheck" ,rust-quickcheck-0.6)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-seahash" ,rust-seahash-3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-http-auth-0.1
  (package
    (name "rust-http-auth")
    (version "0.1.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "http-auth" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0mf27f963mxpbyjw20sjmm0ziqvr9z9m7kdlsrbada2fdazrng34"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.21)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-http" ,rust-http-1)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-md-5" ,rust-md-5-0.10)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-reqwest" ,rust-reqwest-0.11))))
    (inputs
     (list openssl))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/scottlamb/http-auth")
    (synopsis "HTTP authentication for rust")
    (description "This package provices HTTP authentication; it can parse
challenge lists, respond to Basic and Digest challenges.  It is likely to be
extended with server support and additional auth schemes.")
    (license (list license:expat license:asl2.0))))

(define-public rust-http-body-1
  (package
    (name "rust-http-body")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hyn8n3iadrbwq8y0p1rl1275s4nm49bllw5wji29g4aa3dqbb0w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-1))))
    (home-page "https://github.com/hyperium/http-body")
    (synopsis "Asynchronous, streaming, HTTP request or response body")
    (description
     "This package provides a trait representing an asynchronous, streaming,
HTTP request or response body.")
    (license license:expat)))

(define-public rust-http-body-0.4
  (package
    (inherit rust-http-body-1)
    (name "rust-http-body")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-http" ,rust-http-0.2))))))

(define-public rust-http-body-0.3
  (package
    (inherit rust-http-body-0.4)
    (name "rust-http-body")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06qi0ni45lb92w3ml260c0bxbq5zd4snjmz0a9k69xq6021zzm8k"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-http" ,rust-http-0.2))))))

(define-public rust-http-body-0.1
  (package
    (inherit rust-http-body-0.3)
    (name "rust-http-body")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b99404k4mw6a92hvyr0qwzkqv4f866ykg0x7913limjq5cwhhb7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-tokio-buf" ,rust-tokio-buf-0.1))))))

(define-public rust-http-body-util-0.1
  (package
    (name "rust-http-body-util")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h78a6jj2vky0wmgmq5f1h541cmhmlij09gw63fxl59h77mpkjs1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))
       #:cargo-development-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/hyperium/http-body")
    (synopsis "Combinators and adapters for HTTP request/response bodies")
    (description
     "This package provides combinators and adapters for HTTP request or
response bodies.")
    (license license:expat)))

(define-public rust-http-client-6
  (package
    (name "rust-http-client")
    (version "6.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19g19jli98cd0ywrzcsbw5j34rypm8n43yszxa3gaaqyr46m2iqr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-async-h1" ,rust-async-h1-2)
        ("rust-async-native-tls" ,rust-async-native-tls-0.3)
        ("rust-async-std" ,rust-async-std-1)
        ("rust-async-tls" ,rust-async-tls-0.10)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-dashmap" ,rust-dashmap-5)
        ("rust-deadpool" ,rust-deadpool-0.7)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http-types" ,rust-http-types-2)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-hyper-tls" ,rust-hyper-tls-0.4)
        ("rust-isahc" ,rust-isahc-0.9)
        ("rust-js-sys" ,rust-js-sys-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
        ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/http-rs/http-client")
    (synopsis "Types and traits for HTTP clients")
    (description "This package provides types and traits for HTTP clients.")
    (license (list license:expat license:asl2.0))))

(define-public rust-http-range-header-0.4
  (package
    (name "rust-http-range-header")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-range-header" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zhigmvyrk50jzll6nmn7n1rxkfzh8xn0838dvdvnj52rlqyzr1w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/MarcusGrass/parse-range-headers")
    (synopsis "Zero-dependency range header parser")
    (description "This package provides a range header parser without any
dependencies.")
    (license license:expat)))

(define-public rust-http-range-header-0.3
  (package
    (inherit rust-http-range-header-0.4)
    (name "rust-http-range-header")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-range-header" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13vm511vq3bhschkw2xi9nhxzkw53m55gn9vxg7qigfxc29spl5d"))))))

(define-public rust-http-types-2
  (package
    (name "rust-http-types")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bgmfmvirsa1alcyw15mkh227j3a62aq1x47lkxxnfnnf9x1i6vf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-async-channel" ,rust-async-channel-1)
        ("rust-async-std" ,rust-async-std-1)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-cookie" ,rust-cookie-0.14)
        ("rust-futures-lite" ,rust-futures-lite-1)
        ("rust-http" ,rust-http-0.2)
        ("rust-infer" ,rust-infer-0.2)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-qs" ,rust-serde-qs-0.8)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-http" ,rust-http-0.2))))
    (home-page "https://github.com/http-rs/http-types")
    (synopsis "Common types for HTTP operations")
    (description
     "This package provides common types for HTTP operations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-httparse-1
  (package
    (name "rust-httparse")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "httparse" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "010rrfahm1jss3p022fqf3j3jmm72vhn4iqhykahb9ynpaag75yq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/seanmonstar/httparse")
    (synopsis "Zero-copy HTTP/1.x parser")
    (description
     "This package provides a tiny, safe, speedy, zero-copy HTTP/1.x parser.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-httpdate-1
  (package
    (name "rust-httpdate")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "httpdate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))
    (home-page "https://github.com/pyfisch/httpdate")
    (synopsis "HTTP date parsing and formatting")
    (description
     "This crates parses and formats HTTP datetime strings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper-1
  (package
    (name "rust-hyper")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fi6k7hz5fmdph0a5r8hw50d7h2n9zxkizmafcmb65f67bblhr8q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; unresolved imports `super::DecodedLength`, `super::Sender`
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.4)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-want" ,rust-want-0.3))
       #:cargo-development-inputs
       (("rust-form-urlencoded" ,rust-form-urlencoded-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http-body-util" ,rust-http-body-util-0.1)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-spmc" ,rust-spmc-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-test" ,rust-tokio-test-0.4)
        ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://hyper.rs")
    (synopsis "Fast and correct HTTP library")
    (description "This package provides a fast and correct HTTP library.")
    (license license:expat)))

(define-public rust-hyper-0.14
  (package
    (inherit rust-hyper-1)
    (name "rust-hyper")
    (version "0.14.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2l74p3harvjgb0bvaxlxgxq71vpfrzv0cqz2p9w8d8akbczcgz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-h2" ,rust-h2-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-http-body" ,rust-http-body-0.4)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-httpdate" ,rust-httpdate-1)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pin-project" ,rust-pin-project-lite-0.2)
        ("rust-socket2" ,rust-socket2-0.4)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-want" ,rust-want-0.3))
       #:cargo-development-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-matches" ,rust-matches-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-pnet-datalink" ,rust-pnet-datalink-0.27)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-spmc" ,rust-spmc-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-test" ,rust-tokio-test-0.4)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-tower" ,rust-tower-0.4)
        ("rust-url" ,rust-url-2))))))

(define-public rust-hyper-0.13
  (package
    (inherit rust-hyper-0.14)
    (name "rust-hyper")
    (version "0.13.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1symcnba2y03b8lj6xp2wd994lk3xyk3wizacjg5s60njzfshs1y"))))
    (arguments
     `(#:tests? #f      ; Not all files included
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-h2" ,rust-h2-0.2)
        ("rust-http" ,rust-http-0.2)
        ("rust-http-body" ,rust-http-body-0.3)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-itoa" ,rust-itoa-0.4)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-socket2" ,rust-socket2-0.3)
        ("rust-time" ,rust-time-0.1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-want" ,rust-want-0.3))
       #:cargo-development-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-matches" ,rust-matches-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-spmc" ,rust-spmc-0.3)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-test" ,rust-tokio-test-0.2)
        ("rust-tokio-util" ,rust-tokio-util-0.3)
        ("rust-tower-util" ,rust-tower-util-0.3)
        ("rust-url" ,rust-url-1))))))

(define-public rust-hyper-0.12
  (package
    (inherit rust-hyper-0.13)
    (name "rust-hyper")
    (version "0.12.35")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xnm8zi4bdjqhlnx3238kx8yjf29jjd1ww54apcql7wf8g8nxglx"))))
    (arguments
     `(#:skip-build? #t ;; fails due to some missing example file
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-futures-cpupool" ,rust-futures-cpupool-0.1)
        ("rust-h2" ,rust-h2-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-http-body" ,rust-http-body-0.1)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-iovec" ,rust-iovec-0.1)
        ("rust-itoa" ,rust-itoa-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-time" ,rust-time-0.1)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-buf" ,rust-tokio-buf-0.1)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-threadpool" ,rust-tokio-threadpool-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2)
        ("rust-want" ,rust-want-0.2))
       #:cargo-development-inputs
       (("rust-futures-timer" ,rust-futures-timer-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-rustc-version" ,rust-rustc-version-0.2)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-spmc" ,rust-spmc-0.3)
        ("rust-tokio-fs" ,rust-tokio-fs-0.1)
        ("rust-tokio-mockstream" ,rust-tokio-mockstream-1)
        ("rust-url" ,rust-url-1))))))

(define-public rust-hyper-0.10
  (package
    (inherit rust-hyper-0.13)
    (name "rust-hyper")
    (version "0.10.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wwjh9p3mzvg3fss2lqz5r7ddcgl1fh9w6my2j69d6k0lbcm41ha"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-language-tags" ,rust-language-tags-0.2)
        ("rust-log" ,rust-log-0.3)
        ("rust-mime" ,rust-mime-0.2)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-time" ,rust-time-0.1)
        ("rust-traitobject" ,rust-traitobject-0.1)
        ("rust-typeable" ,rust-typeable-0.1)
        ("rust-unicase" ,rust-unicase-1)
        ("rust-url" ,rust-url-1))))))

(define-public rust-hyper-native-tls-0.3
  (package
    (name "rust-hyper-native-tls")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s30y20qy0akzss91yxsq1x1q7rr04jy33i0cq72nx22yjc5advd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-antidote" ,rust-antidote-1)
        ("rust-hyper" ,rust-hyper-0.10)
        ("rust-native-tls" ,rust-native-tls-0.2))))
    (home-page "https://github.com/sfackler/hyper-native-tls")
    (synopsis "native-tls support for Hyper 0.10")
    (description "This package provides native-tls support for Hyper 0.10.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper-proxy-0.9
  (package
    (name "rust-hyper-proxy")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hyper-proxy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1k3mpq6d4rhz58dam1757sav14j32n39q8x37wjgpz943f4mm0fa"))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:cargo-inputs
       `(("rust-bytes" ,rust-bytes-1)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-headers" ,rust-headers-0.3)
         ("rust-http" ,rust-http-0.2)
         ("rust-hyper" ,rust-hyper-0.14)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tower-service" ,rust-tower-service-0.3)
         ("rust-hyper-rustls" ,rust-hyper-rustls-0.22)
         ("rust-hyper-tls" ,rust-hyper-tls-0.5)
         ("rust-native-tls" ,rust-native-tls-0.2)
         ("rust-openssl" ,rust-openssl-0.10)
         ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.5)
         ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
         ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
         ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
         ("rust-webpki" ,rust-webpki-0.21)
         ("rust-webpki-roots" ,rust-webpki-roots-0.21))
       #:cargo-development-inputs
       `(("rust-hyper" ,rust-hyper-0.14)
         ("rust-tokio" ,rust-tokio-1))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/tafia/hyper-proxy")
    (synopsis "Proxy connector for Hyper-based applications")
    (description "Proxy connector for the Hyper HTTP library.")
    (license license:expat)))

(define-public rust-hyper-rustls-0.26
  (package
    (name "rust-hyper-rustls")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b4m1jvs147hxi8677n2dxxib663s7c31xmfni7b5qkanihsggm0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;not all files included
       #:cargo-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-1)
        ("rust-hyper" ,rust-hyper-1)
        ("rust-hyper-util" ,rust-hyper-util-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.22)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
        ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.26))
       #:cargo-development-inputs
       (("rust-http-body-util" ,rust-http-body-util-0.1)
        ("rust-hyper-util" ,rust-hyper-util-0.1)
        ("rust-rustls" ,rust-rustls-0.22)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
        ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/rustls/hyper-rustls")
    (synopsis "Rustls+Hyper integration for pure Rust HTTPS")
    (description
     "This package provides Rustls+Hyper integration for pure Rust HTTPS.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-hyper-rustls-0.24
  (package
    (inherit rust-hyper-rustls-0.26)
    (name "rust-hyper-rustls")
    (version "0.24.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1475j4a2nczz4aajzzsq3hpwg1zacmzbqg393a14j80ff8izsgpc"))))
    (arguments
     `(#:tests? #f                      ; Not all files included.
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25))
       #:cargo-development-inputs (("rust-hyper" ,rust-hyper-0.14)
                                   ("rust-rustls" ,rust-rustls-0.21)
                                   ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                                   ("rust-tokio" ,rust-tokio-1))))))

(define-public rust-hyper-rustls-0.23
  (package
    (inherit rust-hyper-rustls-0.24)
    (name "rust-hyper-rustls")
    (version "0.23.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0736s6a32dqr107f943xaz1n05flbinq6l19lq1wsrxkc5g9d20p"))))
    (arguments
     `(#:tests? #f              ; Not all files included.
       #:cargo-inputs
       (("rust-http" ,rust-http-0.2)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.20)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
        ("rust-webpki-roots" ,rust-webpki-roots-0.22))
       #:cargo-development-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-rustls" ,rust-rustls-0.20)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-tokio" ,rust-tokio-1))))))

(define-public rust-hyper-rustls-0.22
  (package
    (inherit rust-hyper-rustls-0.23)
    (name "rust-hyper-rustls")
    (version "0.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r2szp06nzqx6gblcw69kwx8afjp218fc083kfpw0i3d66bpm7sz"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ct-logs" ,rust-ct-logs-0.8)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.19)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.5)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.21))))))

(define-public rust-hyper-rustls-0.21
  (package
    (inherit rust-hyper-rustls-0.22)
    (name "rust-hyper-rustls")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dmbj15fx6qyg26hji2jm7q9y383090jy3z9zjn5xs4f7v43qx1p"))))
    (arguments
     `(#:tests? #f              ; Not all files included.
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-ct-logs" ,rust-ct-logs-0.7)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.4)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.14)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.20))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.2))))))

(define-public rust-hyper-sync-rustls-0.3
  (package
    (name "rust-hyper-sync-rustls")
    (version "0.3.0-rc.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-sync-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16zirxhsk26kz5jxxxs37wxsm02id97h57kkqs512fj1j0x486kd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; not all files included
         "--skip=client"
         "--skip=server")
       #:cargo-inputs
       (("rust-hyper" ,rust-hyper-0.10)
        ("rust-rustls" ,rust-rustls-0.14)
        ("rust-webpki" ,rust-webpki-0.18)
        ("rust-webpki-roots" ,rust-webpki-roots-0.15))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.5))))
    (home-page "https://github.com/SergioBenitez/hyper-sync-rustls")
    (synopsis "Glue code for Rustls and synchronous Hyper")
    (description
     "This package provides glue code for Rustls and synchronous Hyper.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper-timeout-0.4
  (package
    (name "rust-hyper-timeout")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-timeout" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c8k3g8k2yh1gxvsx9p7amkimgxhl9kafwpj7jyf8ywc5r45ifdv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-hyper" ,rust-hyper-0.14)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-io-timeout" ,rust-tokio-io-timeout-1))))
    (home-page "https://github.com/hjr3/hyper-timeout")
    (synopsis "Connect, read and write timeout aware connector for Hyper")
    (description
     "This package provides a connect, read and write timeout aware connector
to be used with Hyper client.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper-tls-0.6
  (package
    (name "rust-hyper-tls")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q36x2yps6hhvxq5r7mc8ph9zz6xlb573gx0x3yskb0fi736y83h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-http-body-util" ,rust-http-body-util-0.1)
        ("rust-hyper" ,rust-hyper-1)
        ("rust-hyper-util" ,rust-hyper-util-0.1)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
        ("rust-tower-service" ,rust-tower-service-0.3))
       #:cargo-development-inputs
       (("rust-hyper-util" ,rust-hyper-util-0.1)
        ("rust-tokio" ,rust-tokio-1))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://hyper.rs")
    (synopsis "Default TLS implementation for use with hyper")
    (description "This package provides the default TLS implementation for use
with hyper.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper-tls-0.5
  (package
    (inherit rust-hyper-tls-0.6)
    (name "rust-hyper-tls")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01crgy13102iagakf6q4mb75dprzr7ps1gj0l5hxm1cvm7gks66n"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3))))))

(define-public rust-hyper-tls-0.4
  (package
    (inherit rust-hyper-tls-0.5)
    (name "rust-hyper-tls")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vcfyz7dxavf4brns15afmj5fxz88lbn05rrpbfqsnybdp2sqyfr"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.2))))))

(define-public rust-hyper-util-0.1
  (package
    (name "rust-hyper-util")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1akngan7j0n2n0wd25c6952mvqbkj9gp1lcwzyxjc0d37l8yyf6a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; could not find `client` in `hyper_util`
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-http-body-util" ,rust-http-body-util-0.1)
        ("rust-hyper" ,rust-hyper-1)
        ("rust-pnet-datalink" ,rust-pnet-datalink-0.34)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.5)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://hyper.rs")
    (synopsis "@code{hyper} utilities")
    (description "This package provides utilities for the @code{hyper} crate.")
    (license license:expat)))

(define-public rust-iron-0.6
  (package
    (name "rust-iron")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "iron" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s4mf8395f693nhwsr0znw3j5frzn56gzllypyl50il85p50ily6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-hyper" ,rust-hyper-0.10)
        ("rust-hyper-native-tls" ,rust-hyper-native-tls-0.3)
        ("rust-log" ,rust-log-0.3)
        ("rust-mime-guess" ,rust-mime-guess-1)
        ("rust-modifier" ,rust-modifier-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-plugin" ,rust-plugin-0.2)
        ("rust-typemap" ,rust-typemap-0.3)
        ("rust-url" ,rust-url-1))))
    (home-page "https://github.com/iron/iron")
    (synopsis "Extensible, concurrency focused web development in Rust")
    (description
     "Iron is a high level web framework built in and for Rust.  It is highly
concurrent and can scale horizontally on more machines behind a load balancer
or by running more threads on a more powerful machine.  Iron avoids the
bottlenecks encountered in highly concurrent code by avoiding shared writes
and locking in the core framework.")
    (license license:expat)))

(define-public rust-isahc-1
  (package
    (name "rust-isahc")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "isahc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1scfgyv3dpjbkqa9im25cd12cs6rbd8ygcaw67f3dx41sys08kik"))
       (modules '((guix build utils)))
       (snippet '(substitute* "Cargo.toml"
                   ((".*static-curl.*") "")))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; unresolved import `testserver`
       #:cargo-inputs (("rust-async-channel" ,rust-async-channel-1)
                       ("rust-castaway" ,rust-castaway-0.1)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-curl-sys" ,rust-curl-sys-0.4)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-event-listener" ,rust-event-listener-2)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.9)
                       ("rust-polling" ,rust-polling-2)
                       ("rust-publicsuffix" ,rust-publicsuffix-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-sluice" ,rust-sluice-0.5)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-futures" ,rust-tracing-futures-0.2)
                       ("rust-url" ,rust-url-2)
                       ("rust-waker-fn" ,rust-waker-fn-1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.9)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-indicatif" ,rust-indicatif-0.15)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-static-assertions" ,rust-static-assertions-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-test-case" ,rust-test-case-2)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.2))))
    (native-inputs (list pkg-config))
    (inputs (list curl openssl zlib))
    (home-page "https://github.com/sagebind/isahc")
    (synopsis "Practical and fun HTTP client")
    (description
     "Isahc is an acronym that stands for Incredible Streaming Asynchronous
HTTP Client.  It is an asynchronous HTTP client for the Rust language.  It
uses libcurl as an HTTP engine inside, and provides an easy-to-use API on top
that integrates with Rust idioms.")
    (license license:expat)))

(define-public rust-isahc-0.9
  (package
    (inherit rust-isahc-1)
    (name "rust-isahc")
    (version "0.9.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "isahc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12iqz5fj0509pr813pds2fgdk649a0b6ipvy3pqjwb1ywh68m572"))
       (modules '((guix build utils)))
       (snippet '(substitute* "Cargo.toml"
                   (("\"static-curl\", ") "")))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `testserver`
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-curl" ,rust-curl-0.4)
        ("rust-curl-sys" ,rust-curl-sys-0.4)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-flume" ,rust-flume-0.9)
        ("rust-futures-lite" ,rust-futures-lite-1)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-publicsuffix" ,rust-publicsuffix-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-sluice" ,rust-sluice-0.5)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-futures" ,rust-tracing-futures-0.2)
        ("rust-url" ,rust-url-2)
        ("rust-waker-fn" ,rust-waker-fn-1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-indicatif" ,rust-indicatif-0.15)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-test-case" ,rust-test-case-1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.2))))))

(define-public rust-json5-0.4
  (package
    (name "rust-json5")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "json5" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h9hni897zmn3vcixfbwwkj2gkz27h7z9dah8bk1qv37mwhxpc4n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-pest" ,rust-pest-2)
                       ("rust-pest-derive" ,rust-pest-derive-2)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-matches" ,rust-matches-0.1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/callum-oakley/json5-rs")
    (synopsis "Rust JSON5 serializer and deserializer which speaks Serde")
    (description
     "This package provides a Rust JSON5 serializer and deserializer
which speaks Serde.")
    (license license:isc)))

(define-public rust-jsonwebtoken-7
  (package
    (name "rust-jsonwebtoken")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jsonwebtoken" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ciz205wcjcn7n6i871zz5xlbzk863b0ybgiqi7li9ipwhawraxg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.12)
                       ("rust-pem" ,rust-pem-0.8)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-simple-asn1" ,rust-simple-asn1-0.4))
       #:cargo-development-inputs (("rust-chrono" ,rust-chrono-0.4))))
    (home-page "https://github.com/Keats/jsonwebtoken")
    (synopsis "Create and decode JWTs in a strongly typed way")
    (description "Create and decode JWTs in a strongly typed way.")
    (license license:expat)))

(define-public rust-mockito-1
  (package
    (name "rust-mockito")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mockito" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07mz6yyfnl1ka29wkl1qa6mvm66lwsjf8y3cl0m3kpsvm8iy1xnj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=test_assert_with_last_unmatched_request"
         "--skip=test_assert_with_last_unmatched_request_and_headers"
         "--skip=test_assert_with_last_unmatched_request_and_query")
       #:cargo-inputs (("rust-assert-json-diff" ,rust-assert-json-diff-2)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-similar" ,rust-similar-2)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.8)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-reqwest" ,rust-reqwest-0.11)
                                   ("rust-testing-logger" ,rust-testing-logger-0.1)
                                   ("rust-tokio" ,rust-tokio-1))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/lipanski/mockito")
    (synopsis "HTTP mocking for Rust")
    (description "This package provides HTTP mocking for Rust.")
    (license license:expat)))

(define-public rust-multipart-0.18
  (package
    (name "rust-multipart")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "multipart" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "10libwfbazqcyxcpgpcdf1a66jnzghwlmxlxnffg4rrqhqrwdph0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-buf-redux" ,rust-buf-redux-0.8)
        ("rust-clippy" ,rust-clippy-0.0)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-hyper" ,rust-hyper-0.10)
        ("rust-iron" ,rust-iron-0.6)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-nickel" ,rust-nickel-0.11)
        ("rust-quick-error" ,rust-quick-error-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rocket" ,rust-rocket-0.4)
        ("rust-safemem" ,rust-safemem-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-tiny-http" ,rust-tiny-http-0.6)
        ("rust-twoway" ,rust-twoway-0.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.5))))
    (home-page "https://github.com/abonander/multipart")
    (synopsis "Backend-agnostic extension for file uploads in HTTP libraries for Rust")
    (description "This package provides a backend-agnostic extension for HTTP
libraries that provides support for POST multipart/form-data requests on both
client and server.")
    (license (list license:expat license:asl2.0))))

(define-public rust-multipart-0.17
  (package
    (inherit rust-multipart-0.18)
    (name "rust-multipart")
    (version "0.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "multipart" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m3nrydgc56wjixsahipmvjgnxnw2cz7w8ryghsgahwjr3nswl6h"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-buf-redux" ,rust-buf-redux-0.8)
        ("rust-clippy" ,rust-clippy-0.0)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-hyper" ,rust-hyper-0.10)
        ("rust-iron" ,rust-iron-0.6)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-nickel" ,rust-nickel-0.11)
        ("rust-quick-error" ,rust-quick-error-1)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rocket" ,rust-rocket-0.4)
        ("rust-safemem" ,rust-safemem-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-tiny-http" ,rust-tiny-http-0.6)
        ("rust-twoway" ,rust-twoway-0.1))
       #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger-0.5))))))

(define-public rust-nickel-0.11
  (package
    (name "rust-nickel")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nickel" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1na619j2k0hkv5qhws7ccibzhn1v637f1vqwnsn2vnr84y1il1p5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-compiletest-rs" ,rust-compiletest-rs-0.3)
        ("rust-groupable" ,rust-groupable-0.2)
        ("rust-hyper" ,rust-hyper-0.10)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.3)
        ("rust-modifier" ,rust-modifier-0.1)
        ("rust-mustache" ,rust-mustache-0.9)
        ("rust-plugin" ,rust-plugin-0.2)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-time" ,rust-time-0.1)
        ("rust-typemap" ,rust-typemap-0.3)
        ("rust-url" ,rust-url-1))))
    (home-page "https://nickel-org.github.io/")
    (synopsis "Web application framework for Rust")
    (description
     "@code{nickel.rs} is a simple and lightweight foundation for web
applications written in Rust.  Its API is inspired by the popular
@code{express} framework for JavaScript.")
    (license license:expat)))

(define-public rust-opentelemetry-http-0.10
  (package
    (name "rust-opentelemetry-http")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17irqlgsqr1f0in5rhvgl224x2gdcycy8w3ybydlyrdyx2f1hlbz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-isahc" ,rust-isahc-1)
                       ("rust-opentelemetry" ,rust-opentelemetry-0.21)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-surf" ,rust-surf-2)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/open-telemetry/opentelemetry-rust")
    (synopsis "Helpers for exchange of traces and metrics over HTTP")
    (description
     "This package provides helper implementations for exchange of traces
and metrics over HTTP.")
    (license license:asl2.0)))

(define-public rust-poem-1
  (package
    (name "rust-poem")
    (version "1.3.59")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "poem" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0159agmjig6s45sjf1jcbira8icpbakfadwa23pc2i07gg4p8ish"))
       (patches (search-patches "rust-poem-1-fewer-deps.patch"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; use of undeclared crate or module `futures_util`
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-compression" ,rust-async-compression-0.4)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-cookie" ,rust-cookie-0.17)
                       ("rust-csrf" ,rust-csrf-0.4)
                       ;("rust-eyre" ,rust-eyre-0.6)
                       ;("rust-fluent" ,rust-fluent-0.16)
                       ;("rust-fluent-langneg" ,rust-fluent-langneg-0.13)
                       ;("rust-fluent-syntax" ,rust-fluent-syntax-0.11)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.24)
                       ;("rust-intl-memoizer" ,rust-intl-memoizer-0.5)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-nix" ,rust-nix-0.27)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ;("rust-opentelemetry" ,rust-opentelemetry-0.21)
                       ;("rust-opentelemetry-http" ,rust-opentelemetry-http-0.10)
                       ;("rust-opentelemetry-prometheus" ,rust-opentelemetry-prometheus-0.14)
                       ;("rust-opentelemetry-semantic-conventions" ,rust-opentelemetry-semantic-conventions-0.13)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-poem-derive" ,rust-poem-derive-1)
                       ("rust-priority-queue" ,rust-priority-queue-1)
                       ;("rust-prometheus" ,rust-prometheus-0.13)
                       ("rust-quick-xml" ,rust-quick-xml-0.30)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rcgen" ,rust-rcgen-0.11)
                       ;("rust-redis" ,rust-redis-0.23)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rfc7239" ,rust-rfc7239-0.1)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rust-embed" ,rust-rust-embed-8)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ;("rust-sse-codec" ,rust-sse-codec-0.3)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-metrics" ,rust-tokio-metrics-0.3)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.20)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ;("rust-unic-langid" ,rust-unic-langid-0.9)
                       ("rust-wildmatch" ,rust-wildmatch-2)
                       ("rust-x509-parser" ,rust-x509-parser-0.15))
       #:cargo-development-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/poem-web/poem")
    (synopsis
     "Web framework written in the Rust programming language")
    (description "Poem is a full-featured and easy-to-use web framework written
in the Rust programming language.")
    (license (list license:expat license:asl2.0))))

(define-public rust-poem-derive-1
  (package
    (name "rust-poem-derive")
    (version "1.3.59")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "poem-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cdvid2ryn4h9wj7087shf20ijvahh1n44bmwghngn6qh13czpa2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/poem-web/poem")
    (synopsis "Macros for poem")
    (description "This package provides macros for poem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-reqwest-0.12
  (package
    (name "rust-reqwest")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "reqwest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "047aa0qnngnlnf9i0abrs6pgmz15vk81p5pvscwhk3l6jbfsyv2n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=badssl"
                            "--skip=test_allowed_methods"
                            "--skip=test_tls_info"
                            "--skip=connect_many_timeout"
                            "--skip=connect_timeout")
       #:cargo-inputs
       (("rust-async-compression" ,rust-async-compression-0.4)
        ("rust-base64" ,rust-base64-0.22)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-cookie" ,rust-cookie-0.17)
        ("rust-cookie-store" ,rust-cookie-store-0.20)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-h2" ,rust-h2-0.4)
        ("rust-h3" ,rust-h3-0.0.4)
        ("rust-h3-quinn" ,rust-h3-quinn-0.0.5)
        ("rust-hickory-resolver" ,rust-hickory-resolver-0.24)
        ("rust-http" ,rust-http-1)
        ("rust-http-body" ,rust-http-body-1)
        ("rust-http-body-util" ,rust-http-body-util-0.1)
        ("rust-hyper" ,rust-hyper-1)
        ("rust-hyper-rustls" ,rust-hyper-rustls-0.26)
        ("rust-hyper-tls" ,rust-hyper-tls-0.6)
        ("rust-hyper-util" ,rust-hyper-util-0.1)
        ("rust-ipnet" ,rust-ipnet-2)
        ("rust-js-sys" ,rust-js-sys-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-quinn" ,rust-quinn-0.10)
        ("rust-rustls" ,rust-rustls-0.22)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
        ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
        ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
        ("rust-system-configuration" ,rust-system-configuration-0.5)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
        ("rust-tokio-socks" ,rust-tokio-socks-0.5)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-url" ,rust-url-2)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
        ("rust-wasm-streams" ,rust-wasm-streams-0.4)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.26)
        ("rust-winreg" ,rust-winreg-0.52))
       #:cargo-development-inputs
       (("rust-brotli" ,rust-brotli-3)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-hyper" ,rust-hyper-1)
        ("rust-hyper-util" ,rust-hyper-util-0.1)
        ("rust-libflate" ,rust-libflate-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
        ("rust-zstd" ,rust-zstd-0.13))))
    (native-inputs (list pkg-config))
    (inputs (list openssl (list zstd "lib")))
    (home-page "https://github.com/seanmonstar/reqwest")
    (synopsis "High level HTTP client library")
    (description "This package provides a high level HTTP client library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-reqwest-0.11
  (package
    (inherit rust-reqwest-0.12)
    (name "rust-reqwest")
    (version "0.11.23")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "reqwest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hgvzb7r46656r9vqhl5qk1kbr2xzjb91yr2cb321160ka6sxc9p"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; These tests go to the network
         "--skip=test_badssl_modern"
         "--skip=test_badssl_self_signed"
         "--skip=connect_timeout"
         "--skip=test_allowed_methods"
         "--skip=test_tls_info"
         "--skip=connect_many_timeout")
       #:cargo-inputs
       (("rust-async-compression" ,rust-async-compression-0.4)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-cookie" ,rust-cookie-0.16)
        ("rust-cookie-store" ,rust-cookie-store-0.16)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-h2" ,rust-h2-0.3)
        ("rust-h3" ,rust-h3-0.0.3)
        ("rust-h3-quinn" ,rust-h3-quinn-0.0.4)
        ("rust-http" ,rust-http-0.2)
        ("rust-http-body" ,rust-http-body-0.4)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-hyper-rustls" ,rust-hyper-rustls-0.24)
        ("rust-hyper-tls" ,rust-hyper-tls-0.5)
        ("rust-ipnet" ,rust-ipnet-2)
        ("rust-js-sys" ,rust-js-sys-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-quinn" ,rust-quinn-0.10)
        ("rust-rustls" ,rust-rustls-0.21)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
        ("rust-system-configuration" ,rust-system-configuration-0.5)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
        ("rust-tokio-socks" ,rust-tokio-socks-0.5)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.23)
        ("rust-url" ,rust-url-2)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
        ("rust-wasm-streams" ,rust-wasm-streams-0.3)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.25)
        ("rust-winreg" ,rust-winreg-0.50))
       #:cargo-development-inputs
       (("rust-brotli" ,rust-brotli-3)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-libflate" ,rust-libflate-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))))

(define-public rust-reqwest-0.10
  (package
    (inherit rust-reqwest-0.11)
    (name "rust-reqwest")
    (version "0.10.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "reqwest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0z7l46m1mjnvncscaq61zq6qmazrmb33vwjcnfrxpi0liqdgh607"))))
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            ;; These tests require internet access.
                            "--skip=test_badssl_modern"
                            "--skip=test_badssl_self_signed"
                            ;; XXX: Not sure why these fail.
                            "--skip=test_allowed_methods"
                            "--skip=connect_timeout")
       #:cargo-inputs
       (("rust-async-compression" ,rust-async-compression-0.3)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-cookie" ,rust-cookie-0.14)
        ("rust-cookie-store" ,rust-cookie-store-0.12)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-http-body" ,rust-http-body-0.3)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-hyper-rustls" ,rust-hyper-rustls-0.21)
        ("rust-hyper-tls" ,rust-hyper-tls-0.4)
        ("rust-ipnet" ,rust-ipnet-2)
        ("rust-js-sys" ,rust-js-sys-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
        ("rust-time" ,rust-time-0.2)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.14)
        ("rust-tokio-socks" ,rust-tokio-socks-0.3)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.19)
        ("rust-url" ,rust-url-2)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.20)
        ("rust-winreg" ,rust-winreg-0.7))
       #:cargo-development-inputs
       (("rust-brotli" ,rust-brotli-3)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-libflate" ,rust-libflate-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))))

(define-public rust-rfc7239-0.1
  (package
    (name "rust-rfc7239")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rfc7239" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ixsyn8y2jfhfqnhwivgil3cvdr4jdr5s0nr7gqq3d3yryrifwq8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-uncased" ,rust-uncased-0.9))))
    (home-page "https://github.com/icewind1991/rfc7239")
    (synopsis "Parser for rfc7239 formatted Forwarded headers")
    (description "Parser for rfc7239 formatted Forwarded headers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-0.5
  (package
    (name "rust-rocket")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l4i93dai7pyzlkvdjkqg2g7ni1r6749cwx4nrrhsrr6rdybaywy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atomic" ,rust-atomic-0.5)
                       ("rust-binascii" ,rust-binascii-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-either" ,rust-either-1)
                       ("rust-figment" ,rust-figment-0.10)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ref-cast" ,rust-ref-cast-1)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-rocket-codegen" ,rust-rocket-codegen-0.5)
                       ("rust-rocket-http" ,rust-rocket-http-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-state" ,rust-state-0.6)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-ubyte" ,rust-ubyte-0.10)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-yansi" ,rust-yansi-1))
       #:cargo-development-inputs (("rust-figment" ,rust-figment-0.10)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://rocket.rs")
    (synopsis
     "Web framework with focus on ease-of-use, expressibility, and speed")
    (description
     "Rocket is a web framework with a focus on ease-of-use, expressibility,
and speed.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-0.4
  (package
    (inherit rust-rocket-0.5)
    (name "rust-rocket")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ybnhjw92zaan92lsmx6mkhqc9cpsg3885svb3wzyj39pyzvsvz"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-base64" ,rust-base64-0.12)
        ("rust-log" ,rust-log-0.4)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-pear" ,rust-pear-0.1)
        ("rust-rocket-codegen" ,rust-rocket-codegen-0.4)
        ("rust-rocket-http" ,rust-rocket-http-0.4)
        ("rust-state" ,rust-state-0.4)
        ("rust-time" ,rust-time-0.1)
        ("rust-toml" ,rust-toml-0.4)
        ("rust-version-check" ,rust-version-check-0.9)
        ("rust-yansi" ,rust-yansi-0.5)
        ("rust-yansi" ,rust-yansi-0.5))))))

(define-public rust-rocket-codegen-0.5
  (package
    (name "rust-rocket-codegen")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket_codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k6hdf9s9y73kzj89qs688gnfjj1sl4imp6pdjz22pzpmdk808x2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; can't find crate for `rocket`
       #:cargo-inputs (("rust-devise" ,rust-devise-0.4)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rocket-http" ,rust-rocket-http-0.5)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2)
                       ("rust-version-check" ,rust-version-check-0.9))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-trybuild" ,rust-trybuild-1)
        ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://rocket.rs")
    (synopsis "Procedural macros for the Rocket web framework")
    (description
     "This package provides procedural macros for the Rocket web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-codegen-0.4
  (package
    (inherit rust-rocket-codegen-0.5)
    (name "rust-rocket-codegen")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket_codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18s2dll8c4sd26s8cfr6cizj5z55xwnk6r6x7b2wvcf8n9ajrb6f"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-devise" ,rust-devise-0.2)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-quote" ,rust-quote-0.6)
        ("rust-rocket-http" ,rust-rocket-http-0.4)
        ("rust-version-check" ,rust-version-check-0.9)
        ("rust-yansi" ,rust-yansi-0.5))))))

(define-public rust-rocket-http-0.5
  (package
    (name "rust-rocket-http")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket_http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17iq208zf9rfxdnx8hfjxnn51074cc9li99yjigzwnfhjhv6d89p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; can't find crate for `rocket`
       #:cargo-inputs (("rust-cookie" ,rust-cookie-0.18)
                       ("rust-either" ,rust-either-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pear" ,rust-pear-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-ref-cast" ,rust-ref-cast-1)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-stable-pattern" ,rust-stable-pattern-0.1)
                       ("rust-state" ,rust-state-0.6)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-uncased" ,rust-uncased-0.9)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-x509-parser" ,rust-x509-parser-0.13))))
    (home-page "https://rocket.rs")
    (synopsis "HTTP requests, responses and headers tooling for Rocket")
    (description
     "This package provides types, traits, and parsers for HTTP requests,
responses, and headers for the Rocket web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-http-0.4
  (package
    (inherit rust-rocket-http-0.5)
    (name "rust-rocket-http")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket_http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ga98nbcga8amg4xhrfkn1wljnqx9h0vv7mnay9g66vsxl042dnf"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cookie" ,rust-cookie-0.11)
        ("rust-hyper" ,rust-hyper-0.10)
        ("rust-hyper-sync-rustls" ,rust-hyper-sync-rustls-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-pear" ,rust-pear-0.1)
        ("rust-percent-encoding" ,rust-percent-encoding-1)
        ("rust-rustls" ,rust-rustls-0.14)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-state" ,rust-state-0.4)
        ("rust-time" ,rust-time-0.1)
        ("rust-unicode-xid" ,rust-unicode-xid-0.1))))))

(define-public rust-salvo-0.16
  (package
    (name "rust-salvo")
    (version "0.16.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "salvo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jw9h9aac4ms9shvssc8mw53q9842f5bfqv1a8aqkpcyd2j23n4b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `salvo_extra`
       #:cargo-inputs (("rust-salvo-core" ,rust-salvo-core-0.16)
                       ("rust-salvo-extra" ,rust-salvo-extra-0.16))
       #:cargo-development-inputs
       (("rust-async-stream" ,rust-async-stream-0.3)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-jsonwebtoken" ,rust-jsonwebtoken-7)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-tower" ,rust-tower-0.4)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-log" ,rust-tracing-log-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://salvo.rs")
    (synopsis "Salvo is a web framework")
    (description
     "Salvo is a powerful web framework that can make your work easier.")
    (license (list license:expat license:asl2.0))))

(define-public rust-salvo-core-0.16
  (package
    (name "rust-salvo-core")
    (version "0.16.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "salvo_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01dazprfzmjmvwgcrvqxjd12hgwwlk71mskwyl4cj2y2gm5p80bv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=fs::test::test_named_file_builder"
         "--skip=routing::filter::path::tests::test_parse_multi_regex"
         "--skip=routing::filter::path::tests::test_parse_multi_regex_with_prefix"
         "--skip=routing::filter::path::tests::test_parse_multi_regex_with_prefix_and_suffix"
         "--skip=routing::filter::path::tests::test_parse_multi_regex_with_suffix"
         "--skip=routing::filter::path::tests::test_parse_single_regex"
         "--skip=routing::filter::path::tests::test_parse_single_regex_with_prefix"
         "--skip=routing::filter::path::tests::test_parse_single_regex_with_prefix_and_suffix"
         "--skip=routing::filter::path::tests::test_parse_single_regex_with_suffix"
         "--skip=routing::filter::path::tests::test_parse_wildcard_regex")
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-compression" ,rust-async-compression-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cookie" ,rust-cookie-0.16)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-fastrand" ,rust-fastrand-1)
                       ("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-multimap" ,rust-multimap-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-pin-utils" ,rust-pin-utils-0.1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.2)
                       ("rust-salvo-macros" ,rust-salvo-macros-0.16)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tempdir" ,rust-tempdir-0.3)
                       ("rust-textnonce" ,rust-textnonce-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-reqwest" ,rust-reqwest-0.11))))
    (inputs (list openssl))
    (native-inputs (list pkg-config))
    (home-page "https://salvo.rs")
    (synopsis "Core components of the Salvo web framework")
    (description
     "This package provides the core components of the Salvo web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-salvo-extra-0.16
  (package
    (name "rust-salvo-extra")
    (version "0.16.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "salvo_extra" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "023wagm5mpkp1jnpggllbddqigsy5h4qnw2lk8m3j25fj61fl3iy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=proxy::tests::test_proxy"
                            "--skip=serve::tests::test_serve_static_files"
                            "--skip=sse::tests::test_sse_retry")
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.3)
                       ("rust-async-session" ,rust-async-session-3)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-cookie" ,rust-cookie-0.16)
                       ("rust-csrf" ,rust-csrf-0.4)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.23)
                       ("rust-jsonwebtoken" ,rust-jsonwebtoken-7)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-salvo-core" ,rust-salvo-core-0.16)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.16)
                       ("rust-tokio-util" ,rust-tokio-util-0.6)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-reqwest" ,rust-reqwest-0.11))))
    (inputs (list openssl))
    (native-inputs (list pkg-config))
    (home-page "https://salvo.rs")
    (synopsis "Extra components of the Salvo web framework")
    (description
     "This package provides the extra components of the Salvo web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-salvo-macros-0.16
  (package
    (name "rust-salvo-macros")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "salvo_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hdlzvcv2vvbr60w1kmfr9bx8glx4xs9g0ry1pwa7yf7ig987z90"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-proc-quote" ,rust-proc-quote-0.4)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://salvo.rs")
    (synopsis "Salvo proc macros")
    (description "This package provides proc macros for salvo.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stdweb-0.4
  (package
    (name "rust-stdweb")
    (version "0.4.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stdweb" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1md14n9rzxzdskz3hpgln8vxfwqsw2cswc0f5nslh4r82rmlj8nh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-discard" ,rust-discard-1)
        ("rust-futures-channel-preview" ,rust-futures-channel-preview-0.3)
        ("rust-futures-core-preview" ,rust-futures-core-preview-0.3)
        ("rust-futures-executor-preview" ,rust-futures-executor-preview-0.3)
        ("rust-futures-util-preview" ,rust-futures-util-preview-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-stdweb-derive" ,rust-stdweb-derive-0.5)
        ("rust-stdweb-internal-macros" ,rust-stdweb-internal-macros-0.2)
        ("rust-stdweb-internal-runtime" ,rust-stdweb-internal-runtime-0.1)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-rustc-version" ,rust-rustc-version-0.2))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-stdweb-internal-test-macro" ,rust-stdweb-internal-test-macro-0.1)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.2))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Standard library for the client-side Web")
    (description
     "This package provides a standard library for the client-side
Web.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stdweb-0.1
  (package
    (inherit rust-stdweb-0.4)
    (name "rust-stdweb")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "stdweb" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gjk7ch31a3kgdc39kj4zqinf10yqaf717wanh9kwwbbwg430m7g"))))
    (arguments
     (list #:skip-build? #t
           #:cargo-inputs `(("rust-clippy" ,rust-clippy-0.0)
                            ("rust-serde" ,rust-serde-1)
                            ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-stdweb-derive-0.5
  (package
    (name "rust-stdweb-derive")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stdweb-derive" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vsh7g0gaxn4kxqq3knhymdn02p2pfxmnd2j0vplpj6c1yj60yn8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Derive macros for the stdweb crate")
    (description
     "This crate currently defines a derive macro for @code{stdweb} which allows
you to define custom reference types outside of the @code{stdweb} library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stdweb-internal-macros-0.2
  (package
    (name "rust-stdweb-internal-macros")
    (version "0.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stdweb-internal-macros" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "049fq8fl5ny9l5if2qv7kxwng7g6ns95h4fbm3zx360dmpv5zyjq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base-x" ,rust-base-x-0.2)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha1" ,rust-sha1-0.6)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal procedural macros for the stdweb crate")
    (description
     "Internal procedural macros for the @code{stdweb} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stdweb-internal-runtime-0.1
  (package
    (name "rust-stdweb-internal-runtime")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-runtime" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h0nkppb4r8dbrbms2hw9n5xdcs392m0r5hj3b6lsx3h6fx02dr1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal runtime for the @code{stdweb} crate")
    (description "This crate provides internal runtime for the @code{stdweb}
crate.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-stdweb-internal-test-macro-0.1
  (package
    (name "rust-stdweb-internal-test-macro")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-test-macro" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0wx3jlm98qrg1pdw149fprzs9x3x3igqkm5ll23jv2v62yddfrjf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal crate of the `stdweb` crate")
    (description
     "Internal crate of the @code{stdweb} crate.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-surf-2
  (package
    (name "rust-surf")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "surf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mwd0fj0pcdd1q3qp4r045znf0gnvsq1s0pzxlnrhl83npk1m2vi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-async-native-tls" ,rust-async-native-tls-0.3)
        ("rust-async-std" ,rust-async-std-1)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-http-client" ,rust-http-client-6)
        ("rust-http-types" ,rust-http-types-2)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/http-rs/surf")
    (synopsis "HTTP client framework")
    (description
     "Surf is a friendly HTTP client built for casual Rustaceans and veterans
alike.  It's completely modular, and built directly for @code{async/await}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tiny-http-0.12
  (package
    (name "rust-tiny-http")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiny_http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10nw9kk2i2aq4l4csy0825qkq0l66f9mz2c1n57yg8hkckgib69q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ascii" ,rust-ascii-1)
        ("rust-chunked-transfer" ,rust-chunked-transfer-1)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-httpdate" ,rust-httpdate-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rustls" ,rust-rustls-0.20)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.2))
       #:cargo-development-inputs
       (("rust-fdlimit" ,rust-fdlimit-0.1)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-sha1" ,rust-sha1-0.6))))
    (home-page "https://github.com/tiny-http/tiny-http")
    (synopsis "Low level HTTP server library")
    (description "This package provides a low level HTTP server library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tiny-http-0.11
  (package
    (inherit rust-tiny-http-0.12)
    (name "rust-tiny-http")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiny_http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jkpniq72ppbhppyxd2q36kjji05qljsvv1fhvxiwg6j217fzmp0"))))
    (arguments
     `(#:cargo-inputs (("rust-ascii" ,rust-ascii-1)
                       ("rust-chunked-transfer" ,rust-chunked-transfer-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.2)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-fdlimit" ,rust-fdlimit-0.1)
                                   ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
                                   ("rust-sha1" ,rust-sha1-0.6))))))

(define-public rust-tiny-http-0.6
  (package
    (inherit rust-tiny-http-0.12)
    (name "rust-tiny-http")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiny_http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0la95daknfh8g951ddb2zdz6av2459rncp6h9dh02pf98h5glq8n"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ascii" ,rust-ascii-0.8)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-chunked-transfer" ,rust-chunked-transfer-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-url" ,rust-url-1))))))

(define-public rust-tonic-0.10
  (package
    (name "rust-tonic")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tonic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03hx1b2810p4jmsphbql8cn3r22c9n1ar73bj8azf7761lx96q6m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum" ,rust-axum-0.6)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-timeout" ,rust-hyper-timeout-0.4)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-prost" ,rust-prost-0.12)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-static-assertions" ,rust-static-assertions-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower" ,rust-tower-0.4))))
    (home-page "https://github.com/hyperium/tonic")
    (synopsis "gRPC over HTTP/2 implementation for Rust")
    (description
     "This package provides a @code{gRPC} over HTTP/2 implementation focused
on high performance, interoperability, and flexibility.")
    (license license:expat)))

(define-public rust-tonic-0.8
  (package
    (inherit rust-tonic-0.10)
    (name "rust-tonic")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tonic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yymp2xi1p60g81p5jfaybcawpfkb01vqvzqn4cyz6wj7fnry8cg"))))
    (arguments
     `(#:cargo-test-flags '("--release" "--lib" "--bins" "--tests")
       #:cargo-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum" ,rust-axum-0.6)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-timeout" ,rust-hyper-timeout-0.4)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-prost-derive" ,rust-prost-derive-0.11)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-futures" ,rust-tracing-futures-0.2)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-static-assertions" ,rust-static-assertions-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower" ,rust-tower-0.4))))))

(define-public rust-tonic-0.6
  (package
    (inherit rust-tonic-0.10)
    (name "rust-tonic")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tonic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02jxiy0n2mw2c1fchykj3m18wp986685bji26px0z9qhkmjg827z"))))
    (arguments
     `(#:tests? #f     ; unresolved import `crate::codec::compression`
       #:cargo-inputs
       (("rust-async-stream" ,rust-async-stream-0.3)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-h2" ,rust-h2-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-http-body" ,rust-http-body-0.4)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-hyper-timeout" ,rust-hyper-timeout-0.4)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pin-project" ,rust-pin-project-1)
        ("rust-prost" ,rust-prost-0.9)
        ("rust-prost-derive" ,rust-prost-derive-0.9)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.5)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-tokio-util" ,rust-tokio-util-0.6)
        ("rust-tower" ,rust-tower-0.4)
        ("rust-tower-layer" ,rust-tower-layer-0.3)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-futures" ,rust-tracing-futures-0.2)
        ("rust-webpki-roots" ,rust-webpki-roots-0.21))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-static-assertions" ,rust-static-assertions-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower" ,rust-tower-0.4))))))

(define-public rust-tonic-build-0.10
  (package
    (name "rust-tonic-build")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tonic-build" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "129qd12ka65h5f1dzi5mrlz6wndi0pfx1320lawq51f18k01y0lx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-prost-build" ,rust-prost-build-0.12)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/hyperium/tonic")
    (synopsis "Codegen module of @code{tonic} gRPC implementation")
    (description "Codegen module of @code{tonic} @code{gRPC} implementation.")
    (license license:expat)))

(define-public rust-tower-0.4
  (package
    (name "rust-tower")
    (version "0.4.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "073wncyqav4sak1p755hf6vl66njgfc1z1g1di9rxx3cvvh9pymq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; no method named `map_request` found for struct `ServiceBuilder`
       #:cargo-inputs
       (("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-hdrhistogram" ,rust-hdrhistogram-7)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-pin-project" ,rust-pin-project-1)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-tower-layer" ,rust-tower-layer-0.3)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-hdrhistogram" ,rust-hdrhistogram-7)
        ("rust-http" ,rust-http-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-tokio-test" ,rust-tokio-test-0.4)
        ("rust-tower-test" ,rust-tower-test-0.4)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Library for building clients and servers")
    (description
     "Tower is a library of modular and reusable components for building
robust clients and servers.")
    (license license:expat)))

(define-public rust-tower-http-0.5
  (package
    (name "rust-tower-http")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bmgfh1hr92blw7kv2636ykd0gf3h0aqdd89kvjk6b2fg8kr788d"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Remove disabled doctests with unresolved imports.
                   (substitute* "src/lib.rs" (("//!.*") ""))
                   (substitute* "src/classify/status_in_range_is_error.rs"
                                (("///.*") ""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.4)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-http-range-header" ,rust-http-range-header-0.4)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-iri-string" ,rust-iri-string-0.7)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-brotli" ,rust-brotli-3)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-hyper-util" ,rust-hyper-util-0.1)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower" ,rust-tower-0.4)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-uuid" ,rust-uuid-1)
        ("rust-zstd" ,rust-zstd-0.12))))
    (native-inputs (list pkg-config))
    (inputs (list (list zstd "lib")))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description
      "This package provides Tower middleware and utilities for HTTP
clients and servers.")
    (license license:expat)))

(define-public rust-tower-http-0.4
  (package
    (inherit rust-tower-http-0.5)
    (name "rust-tower-http")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h0i2flrw25zwxv72sifq4v5mwcb030spksy7r2a4xl2d4fvpib1"))))
    (arguments
     `(#:cargo-test-flags (list "--release"
                                ;; Not the doc tests.
                                "--lib" "--bins" "--tests")
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.4)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-http-range-header" ,rust-http-range-header-0.3)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-iri-string" ,rust-iri-string-0.7)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs
       (("rust-brotli" ,rust-brotli-3)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower" ,rust-tower-0.4)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-uuid" ,rust-uuid-1)
        ("rust-zstd" ,rust-zstd-0.12))))))

(define-public rust-tower-layer-0.3
  (package
    (name "rust-tower-layer")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-layer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l7i17k9vlssrdg4s3b0ia5jjkmmxsvv8s9y9ih0jfi8ssz8s362"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=layer_fn::layer_fn")
       #:cargo-development-inputs
       (("rust-tower" ,rust-tower-0.4)
        ("rust-tower-service" ,rust-tower-service-0.3))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Easy composition between @code{Service}s")
    (description "This package decorates a @code{Service} to allow easy
composition between @code{Service}s.")
    (license license:expat)))

(define-public rust-tower-service-0.3
  (package
    (name "rust-tower-service")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-service" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0lmfzmmvid2yp2l36mbavhmqgsvzqf7r2wiwz73ml4xmwaf1rg5n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower-layer" ,rust-tower-layer-0.3))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Asynchronous, request / response based, client or server")
    (description "This package provides a trait representing an asynchronous,
request/response based, client or server.")
    (license license:expat)))

(define-public rust-tower-test-0.4
  (package
    (name "rust-tower-test")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19zgjwzr9216yg1ayrnsly06lqdv96m2z1xq0bmf9fgazxrnfm54"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-pin-project" ,rust-pin-project-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-test" ,rust-tokio-test-0.4)
        ("rust-tower-layer" ,rust-tower-layer-0.3)
        ("rust-tower-service" ,rust-tower-service-0.3))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Utilities for writing client and server @code{Service} tests")
    (description "This package provides utilities for writing client and
server @code{Service} tests.")
    (license license:expat)))

(define-public rust-tower-test-0.3
  (package
    (inherit rust-tower-test-0.4)
    (name "rust-tower-test")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1j2k07g3z8ascq7r30bmw3b75v8lhd63mhfl60y59a74q71bp94v"))))
    (arguments
     `(#:cargo-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-test" ,rust-tokio-test-0.2)
        ("rust-tower-layer" ,rust-tower-layer-0.3)
        ("rust-tower-service" ,rust-tower-service-0.3))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.2))))))

(define-public rust-tower-util-0.3
  (package
    (name "rust-tower-util")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x4np2s7h891spvxaarcyainj12a7gvnh7jif9y80cvdh8ckq2fi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-tower-service" ,rust-tower-service-0.3))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-test" ,rust-tokio-test-0.2)
        ("rust-tower-test" ,rust-tower-test-0.3))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Utilities for working with @code{Service}")
    (description "This package provides utilities for working with
@code{Service}.")
    (license license:expat)))

(define-public rust-trust-dns-client-0.22
  (package
    (name "rust-trust-dns-client")
    (version "0.22.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "trust-dns-client" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1znkfhzwikii6v9k98ccbn3krwic1xs3bknf6y0b7nx9wqr8qh3c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Tests require network access.
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-radix-trie" ,rust-radix-trie-0.2)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-rustls" ,rust-rustls-0.20)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.22)
        ("rust-webpki" ,rust-webpki-0.22))
       #:cargo-development-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://trust-dns.org/")
    (synopsis "DNS library with DNSSEC support")
    (description
     "Trust-DNS is a DNS library.  This is the Client library with DNSSEC
support.  DNSSEC with NSEC validation for negative records, is complete.  The
client supports dynamic DNS with SIG0 authenticated requests, implementing
easy to use high level funtions.  Trust-DNS is based on the Tokio and Futures
libraries, which means it should be easily integrated into other software that
also use those libraries.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trust-dns-https-0.20
  (package
    (name "rust-trust-dns-https")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-https" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19f0l1illl69ycb97652rjrjppilz2pz7l9572lrjpkasffgcqr6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-h2" ,rust-h2-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.19)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.20)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.20)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.21))))
    (home-page "https://www.trust-dns.org/index.html")
    (synopsis "DNS over HTTPS extension for the Trust-DNS client")
    (description
     "Trust-DNS is a safe and secure DNS library.  This is an extension for
the Trust-DNS client to use DNS over HTTPS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trust-dns-https-0.19
  (package
    (inherit rust-trust-dns-https-0.20)
    (name "rust-trust-dns-https")
    (version "0.19.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-https" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zfzykz7x6x7g90jl8f1l4w7qj3qxqr1r2w16j5qh2c409lsnqhz"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Requires the internet.
         "--skip=https_client_stream::tests::test_https_google")
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-h2" ,rust-h2-0.2)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.17)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.13)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.19)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.19)
        ("rust-typed-headers" ,rust-typed-headers-0.2)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.19))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-futures" ,rust-futures-0.3))))))

(define-public rust-trust-dns-https-0.18.0-alpha.2
  (package
    (inherit rust-trust-dns-https-0.19)
    (name "rust-trust-dns-https")
    (version "0.18.0-alpha.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-https" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10mad0ys35mp0w91pydk0pzxqd0q2yqvwcf4ppfww7s0l7m8dyid"))))
    (arguments
     `(#:tests? #false                  ;network unreachable
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-h2" ,rust-h2-0.2)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18.0-alpha.2)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.18.0-alpha.2)
        ("rust-typed-headers" ,rust-typed-headers-0.2)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.18))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-futures" ,rust-futures-0.3))))))

(define-public rust-trust-dns-https-0.3
  (package
    (inherit rust-trust-dns-https-0.19)
    (name "rust-trust-dns-https")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-https" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14ps1fxngm8d3ynp9jf86zrqbyzjzh62v5grwrqb1q0xhbz98vv1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #false                  ;network unreachable
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-h2" ,rust-h2-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.9)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.7)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.6)
        ("rust-typed-headers" ,rust-typed-headers-0.1)
        ("rust-webpki" ,rust-webpki-0.19)
        ("rust-webpki-roots" ,rust-webpki-roots-0.16))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.1))))))

(define-public rust-trust-dns-native-tls-0.20
  (package
    (name "rust-trust-dns-native-tls")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "129map2cvy9xcdjg6927xyzic48mq6hqmils0qrmigbr61djxkna"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.20))))
    (home-page "https://www.trust-dns.org/index.html")
    (synopsis "native-tls extension for the Trust-DNS client")
    (description "Trust-DNS is a safe and secure DNS library.  This is an
extension for the Trust-DNS client to use native-tls for TLS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trust-dns-native-tls-0.19
  (package
    (inherit rust-trust-dns-native-tls-0.20)
    (name "rust-trust-dns-native-tls")
    (version "0.19.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "12rh378g144cmw8lpjnivndknlf56i4lzfwnhigp1zviyw5jb7lj"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=tests::test_tls_client_stream_ipv4")
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.19))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))))

(define-public rust-trust-dns-native-tls-0.18.0-alpha.2
  (package
    (inherit rust-trust-dns-native-tls-0.19)
    (name "rust-trust-dns-native-tls")
    (version "0.18.0-alpha.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z99xb24g75lpfg57k91bf4s6niw9aq2dd4bwzlgbfm97byi7akb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=tests::test_tls_client_stream_ipv4")
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18.0-alpha.2))))))

(define-public rust-trust-dns-native-tls-0.6
  (package
    (inherit rust-trust-dns-native-tls-0.19)
    (name "rust-trust-dns-native-tls")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v18xwcy2vz57gnp1a6wx52c4zpwlakpr75ydmai8gc0h2kfzd7l"))))
    (arguments
     `(#:tests? #false
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-tls" ,rust-tokio-tls-0.2)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.7))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.1))))))

(define-public rust-trust-dns-openssl-0.20
  (package
    (name "rust-trust-dns-openssl")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-openssl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zd10g824qrs0yw2bmxphw43iylxlpgvnwb3l3hnwblp2ffhcx50"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.20))))
    (home-page "https://www.trust-dns.org/index.html")
    (synopsis "tokio-openssl extension for the Trust-DNS client")
    (description "Trust-DNS is a safe and secure DNS library.  This is an
extension for the Trust-DNS client to use tokio-openssl for TLS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trust-dns-openssl-0.19
  (package
    (inherit rust-trust-dns-openssl-0.20)
    (name "rust-trust-dns-openssl")
    (version "0.19.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-openssl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pn6q6ipgmjp35gbjbly3hc8lqz3f359wcwlj603f9nwahfnahif"))))
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.4)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.19))
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-0.2))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))))

(define-public rust-trust-dns-openssl-0.18.0-alpha.2
  (package
    (inherit rust-trust-dns-openssl-0.19)
    (name "rust-trust-dns-openssl")
    (version "0.18.0-alpha.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-openssl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16h58wlwgm4jhadi0vxnppdbxlhnxnfid9jxwgnv2fs4d8q9dhg9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.4)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18.0-alpha.2))
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-0.2))))))

(define-public rust-trust-dns-openssl-0.6
  (package
    (inherit rust-trust-dns-openssl-0.19)
    (name "rust-trust-dns-openssl")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-openssl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zwx2bsf1rbyjr6l2c3vi24z7414n4b5qiymva9dmbvwxnqqyk1j"))))
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.3)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.7))
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-0.1))))))

(define-public rust-trust-dns-proto-0.23
  (package
    (name "rust-trust-dns-proto")
    (version "0.23.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x6kaa9vdzq5j6yx6ik0kmp76nd4d9c1x81ii54g8my1a4k1269i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-enum-as-inner" ,rust-enum-as-inner-0.6)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-idna" ,rust-idna-0.4)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-quinn" ,rust-quinn-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tinyvec" ,rust-tinyvec-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25))
       #:cargo-development-inputs
       (("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://trust-dns.org/")
    (synopsis "Safe and secure DNS library")
    (description
     "Trust-DNS is a safe and secure DNS library.  This is the foundational
DNS protocol library for all Trust-DNS projects.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trust-dns-proto-0.22
  (package
    (inherit rust-trust-dns-proto-0.23)
    (name "rust-trust-dns-proto")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09mwv7fnjrkdpf82qqvsbby5xnnpwn0kcp2cqn53br50wk8q6zsg"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-enum-as-inner" ,rust-enum-as-inner-0.5)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-h2" ,rust-h2-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-ipnet" ,rust-ipnet-2)
        ("rust-js-sys" ,rust-js-sys-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-quinn" ,rust-quinn-0.8)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-rustls" ,rust-rustls-0.20)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-socket2" ,rust-socket2-0.4)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tinyvec" ,rust-tinyvec-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-url" ,rust-url-2)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-webpki" ,rust-webpki-0.22)
        ("rust-webpki-roots" ,rust-webpki-roots-0.22))))))

(define-public rust-trust-dns-proto-0.20
  (package
    (inherit rust-trust-dns-proto-0.22)
    (name "rust-trust-dns-proto")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gdsxjl628h02dp0fhcjz6js79fc4dxprqgqny6rghk450dki84q"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-enum-as-inner" ,rust-enum-as-inner-0.3)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-ipnet" ,rust-ipnet-2)
        ("rust-js-sys" ,rust-js-sys-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-socket2" ,rust-socket2-0.3)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-url" ,rust-url-2)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))))

(define-public rust-trust-dns-proto-0.19
  (package
    (inherit rust-trust-dns-proto-0.20)
    (name "rust-trust-dns-proto")
    (version "0.19.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1a8mhawa5bhavmhickmr8ncnvs5jiwpyidig3nabk2nnq2h73b8w"))))
    (arguments
     `(#:cargo-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-enum-as-inner" ,rust-enum-as-inner-0.3)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-js-sys" ,rust-js-sys-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-socket2" ,rust-socket2-0.3)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-url" ,rust-url-2)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-tokio" ,rust-tokio-0.2))))))

(define-public rust-trust-dns-proto-0.18.0-alpha.2
  (package
    (inherit rust-trust-dns-proto-0.19)
    (name "rust-trust-dns-proto")
    (version "0.18.0-alpha.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gs15ckm4d4s59jqmm35lbpx7mvylrk8hiialpnga6d9p0m3lzra"))))
    (arguments
     `(#:cargo-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-enum-as-inner" ,rust-enum-as-inner-0.3)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-socket2" ,rust-socket2-0.3)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-tokio" ,rust-tokio-0.2))))))

(define-public rust-trust-dns-proto-0.7
  (package
    (inherit rust-trust-dns-proto-0.19)
    (name "rust-trust-dns-proto")
    (version "0.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-proto" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0099dm57nnizx4apik9sh3mnvr7rp9mivc903v8xss13dkgynnam"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-enum-as-inner" ,rust-enum-as-inner-0.2)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-idna" ,rust-idna-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-ring" ,rust-ring-0.14)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-0.6)
        ("rust-socket2" ,rust-socket2-0.3)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2)
        ("rust-tokio-udp" ,rust-tokio-udp-0.1)
        ("rust-untrusted" ,rust-untrusted-0.6)
        ("rust-url" ,rust-url-1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-tokio" ,rust-tokio-0.1))))))

(define-public rust-trust-dns-resolver-0.23
  (package
    (name "rust-trust-dns-resolver")
    (version "0.23.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rhbwg7v93yvl3p64skwhkx2zfh2abrx35g3fcy8nwgimz1yd8qh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Tests require network access
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-ipconfig" ,rust-ipconfig-0.3)
                       ("rust-lru-cache" ,rust-lru-cache-0.1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-resolv-conf" ,rust-resolv-conf-0.7)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.23)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25))
       #:cargo-development-inputs
       (("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://trust-dns.org/")
    (synopsis "Safe and secure DNS library")
    (description
     "Trust-DNS is a safe and secure DNS library.  This Resolver library uses
the Client library to perform all DNS queries.  The Resolver is intended to be
a high-level library for any DNS record resolution see Resolver and
AsyncResolver for supported resolution types.  The Client can be used for
other queries.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trust-dns-resolver-0.22
  (package
    (inherit rust-trust-dns-resolver-0.23)
    (name "rust-trust-dns-resolver")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zkw5fx7dwiy4ymn7ywmsb3qhf69mnqdw9mcpyps3c7gvjj1mwmg"))))
    (arguments
     `(#:tests? #f              ; Not all files included.
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-ipconfig" ,rust-ipconfig-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-lru-cache" ,rust-lru-cache-0.1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-resolv-conf" ,rust-resolv-conf-0.7)
        ("rust-rustls" ,rust-rustls-0.20)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.22)
        ("rust-webpki-roots" ,rust-webpki-roots-0.22))
       #:cargo-development-inputs
       (("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))))

(define-public rust-trust-dns-resolver-0.20
  (package
    (inherit rust-trust-dns-resolver-0.22)
    (name "rust-trust-dns-resolver")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r2n933z9yrpdqv60c9mbhl64y2inpx9rm870nq1qqmx226d2wih"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-ipconfig" ,rust-ipconfig-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-lru-cache" ,rust-lru-cache-0.1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-resolv-conf" ,rust-resolv-conf-0.7)
        ("rust-rustls" ,rust-rustls-0.19)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
        ("rust-trust-dns-https" ,rust-trust-dns-https-0.20)
        ("rust-trust-dns-native-tls" ,rust-trust-dns-native-tls-0.20)
        ("rust-trust-dns-openssl" ,rust-trust-dns-openssl-0.20)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.20)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.20)
        ("rust-webpki-roots" ,rust-webpki-roots-0.21))))))

(define-public rust-trust-dns-resolver-0.19
  (package
    (inherit rust-trust-dns-resolver-0.20)
    (name "rust-trust-dns-resolver")
    (version "0.19.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1sqcvwcfllypmjqnhf4dksggpykq57nkh2vdl99xnx8i6wxmj3vi"))))
    (arguments
     `(#:tests? #false                  ;network unreachable
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-ipconfig" ,rust-ipconfig-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-lru-cache" ,rust-lru-cache-0.1)
        ("rust-resolv-conf" ,rust-resolv-conf-0.7)
        ("rust-rustls" ,rust-rustls-0.17)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.4)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.13)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3)
        ("rust-trust-dns-https" ,rust-trust-dns-https-0.19)
        ("rust-trust-dns-native-tls" ,rust-trust-dns-native-tls-0.19)
        ("rust-trust-dns-openssl" ,rust-trust-dns-openssl-0.19)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.19)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.19)
        ("rust-webpki-roots" ,rust-webpki-roots-0.19))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-futures" ,rust-futures-0.3))))))

(define-public rust-trust-dns-resolver-0.18.0-alpha.2
  (package
    (inherit rust-trust-dns-resolver-0.19)
    (name "rust-trust-dns-resolver")
    (version "0.18.0-alpha.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bsal2vz7q3fqdyxa0j1rbbh1hm8mxxv7mf62hjqnvr25d8b343g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--lib" "--bins" "--tests" "--"
         "--skip=async_resolver::tests::test_domain_search"
         "--skip=async_resolver::tests::test_fqdn"
         "--skip=async_resolver::tests::test_idna"
         "--skip=async_resolver::tests::test_large_ndots"
         "--skip=async_resolver::tests::test_lookup_cloudflare"
         "--skip=async_resolver::tests::test_lookup_google"
         "--skip=async_resolver::tests::test_lookup_quad9"
         "--skip=async_resolver::tests::test_ndots"
         "--skip=async_resolver::tests::test_search_list"
         "--skip=hosts::tests::test_read_hosts_conf"
         "--skip=name_server::name_server::tests::test_name_server"
         "--skip=resolver::tests::test_lookup"
         "--skip=system_conf::unix::tests::test_read_resolv_conf")
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-ipconfig" ,rust-ipconfig-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-lru-cache" ,rust-lru-cache-0.1)
        ("rust-resolv-conf" ,rust-resolv-conf-0.6)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-trust-dns-https" ,rust-trust-dns-https-0.18.0-alpha.2)
        ("rust-trust-dns-native-tls" ,rust-trust-dns-native-tls-0.18.0-alpha.2)
        ("rust-trust-dns-openssl" ,rust-trust-dns-openssl-0.18.0-alpha.2)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18.0-alpha.2)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.18.0-alpha.2)
        ("rust-webpki-roots" ,rust-webpki-roots-0.18))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-futures" ,rust-futures-0.3))))))

(define-public rust-trust-dns-resolver-0.11
  (package
    (inherit rust-trust-dns-resolver-0.19)
    (name "rust-trust-dns-resolver")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fd0w2zsdwlsag27fsg0fzyd7j7niw0r22rwh2c5fdmsipjr56bc"))))
    (arguments
     `(#:tests? #false                  ;networking failures
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-ipconfig" ,rust-ipconfig-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-lru-cache" ,rust-lru-cache-0.1)
        ("rust-resolv-conf" ,rust-resolv-conf-0.6)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-0.6)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-trust-dns-https" ,rust-trust-dns-https-0.3)
        ("rust-trust-dns-native-tls" ,rust-trust-dns-native-tls-0.6)
        ("rust-trust-dns-openssl" ,rust-trust-dns-openssl-0.6)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.7)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.6)
        ("rust-webpki-roots" ,rust-webpki-roots-0.16))))))

(define-public rust-trust-dns-rustls-0.20
  (package
    (name "rust-trust-dns-rustls")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00i5jf6bkfxikna0093swl0yz246nabpm0xngdxb94wkr3rz0kq9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.19)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.20)
        ("rust-webpki" ,rust-webpki-0.21))))
    (home-page "https://www.trust-dns.org/index.html")
    (synopsis "rustls extension for the Trust-DNS client")
    (description
     "Trust-DNS is a safe and secure DNS library.  This is an extension for
the Trust-DNS client to use rustls for TLS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trust-dns-rustls-0.19
  (package
    (inherit rust-trust-dns-rustls-0.20)
    (name "rust-trust-dns-rustls")
    (version "0.19.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0d113r4j2821wzxl440bac1xk4c6s5qyx4va0srs6gjvbzhv143h"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=tests::test_tls_client_stream_ipv4")
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.17)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.13)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.19)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.10))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))))

(define-public rust-trust-dns-rustls-0.18.0-alpha.2
  (package
    (inherit rust-trust-dns-rustls-0.19)
    (name "rust-trust-dns-rustls")
    (version "0.18.0-alpha.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qbqn9isrn9awbbhfd72nfqx529idzwdc025ga85vqpxkpryadhc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=tests::test_tls_client_stream_ipv4")
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18.0-alpha.2)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.10))))))

(define-public rust-trust-dns-rustls-0.6
  (package
    (inherit rust-trust-dns-rustls-0.19)
    (name "rust-trust-dns-rustls")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0vbh2y7w2s5gcw33fn4hb5f927kgjm6603vw63slg9riikmsiq43"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--" "--skip=tests::test_tls_client_stream_ipv4")
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.15)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.9)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.7)
        ("rust-webpki" ,rust-webpki-0.19))
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-0.1))))))

(define-public rust-tungstenite-0.21
  (package
    (name "rust-tungstenite")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qaphb5kgwgid19p64grhv2b9kxy7f1059yy92l9kwrlx90sdwcy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-http" ,rust-http-1)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.22)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-input-buffer" ,rust-input-buffer-0.5)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-socket2" ,rust-socket2-0.5))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description
     "This library provides an implementation of WebSockets, RFC6455.  It
allows for both synchronous (like TcpStream) and asynchronous usage and is
easy to integrate into any third-party event loops including MIO.  The API
design abstracts away all the internals of the WebSocket protocol but still
makes them accessible for those who wants full control over the network.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tungstenite-0.20
  (package
    (inherit rust-tungstenite-0.21)
    (name "rust-tungstenite")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fbgcv3h4h1bhhf5sqbwqsp7jnc44bi4m41sgmhzdsk2zl8aqgcy"))))
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.24))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-input-buffer" ,rust-input-buffer-0.5)
                                   ("rust-net2" ,rust-net2-0.2)
                                   ("rust-rand" ,rust-rand-0.8))))))

(define-public rust-tungstenite-0.19
  (package
    (inherit rust-tungstenite-0.20)
    (name "rust-tungstenite")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rxzxg4y22rsvdvs4la7igy9117yidc2m6lsfm2hf0xvsska3yqm"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-http" ,rust-http-0.2)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rustls" ,rust-rustls-0.21)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-url" ,rust-url-2)
        ("rust-utf-8" ,rust-utf-8-0.7)
        ("rust-webpki" ,rust-webpki-0.22)
        ("rust-webpki-roots" ,rust-webpki-roots-0.23))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.4)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-input-buffer" ,rust-input-buffer-0.5)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-rand" ,rust-rand-0.8))))))

(define-public rust-tungstenite-0.16
  (package
    (inherit rust-tungstenite-0.21)
    (name "rust-tungstenite")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l9s7gi9kgl4zynhbyb7737lmwaxaim4b818lwi7y95f2hx73lva"))))
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-env-logger" ,rust-env-logger-0.9)
                                   ("rust-input-buffer" ,rust-input-buffer-0.5)
                                   ("rust-net2" ,rust-net2-0.2)
                                   ("rust-rand" ,rust-rand-0.8))))))

(define-public rust-tungstenite-0.11
  (package
    (inherit rust-tungstenite-0.19)
    (name "rust-tungstenite")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08ra94x3zqkmbsrcmwszknxv2a8g08gk5xlyif3wa037v208sc7h"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.12)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-http" ,rust-http-0.2)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-input-buffer" ,rust-input-buffer-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-url" ,rust-url-2)
        ("rust-utf-8" ,rust-utf-8-0.7))))))

(define-public rust-typed-headers-0.2
  (package
    (name "rust-typed-headers")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typed-headers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jm2xzvvml3a9hhvzf9q4v22l5ifrxrx2kspy7aymknckqgacy9i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.11)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-http" ,rust-http-0.2)
        ("rust-mime" ,rust-mime-0.3))))
    (home-page "https://github.com/sfackler/typed-headers")
    (synopsis "Typed HTTP header serialization and deserialization")
    (description "This package provides typed HTTP header serialization and
deserialization.")
    (license (list license:expat license:asl2.0))))

(define-public rust-typed-headers-0.1
  (package
    (inherit rust-typed-headers-0.2)
    (name "rust-typed-headers")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typed-headers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g40nlq5iw0zxhwb7nfmfbr9m86abgwwhxwhzrm10nfq6bsmlvxx"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-http" ,rust-http-0.1)
        ("rust-mime" ,rust-mime-0.3))))))

(define-public rust-warp-0.3
  (package
    (name "rust-warp")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "warp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sfimrpxkyka1mavfhg5wa4x977qs8vyxa510c627w9zw0i2xsf1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-compression" ,rust-async-compression-0.3)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-headers" ,rust-headers-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-multer" ,rust-multer-2)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pin-project" ,rust-pin-project-1)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.20)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-handlebars" ,rust-handlebars-4)
        ("rust-listenfd" ,rust-listenfd-1)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.5)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-tracing-log" ,rust-tracing-log-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/seanmonstar/warp")
    (synopsis "Serve the web at warp speeds")
    (description "Warp is a composable, web server framework.")
    (license license:expat)))

(define-public rust-warp-0.2
  (package
    (inherit rust-warp-0.3)
    (name "rust-warp")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "warp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01wl8kv5hh1dd7gcwdrmn9xfs7jjsh9yc8xa06ph8yf9akgyc6zl"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-async-compression" ,rust-async-compression-0.3)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-headers" ,rust-headers-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-multipart" ,rust-multipart-0.17)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.14)
        ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.11)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-futures" ,rust-tracing-futures-0.2)
        ("rust-urlencoding" ,rust-urlencoding-1))))))

(define-public rust-web-view-0.7
  (package
    (name "rust-web-view")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "web-view" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1smgmc00nk2wn8kpagp0mpsd0d9f5mvljidf2x7plbi3bymac7gf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;missing files
       #:cargo-inputs
       (("rust-boxfnonce" ,rust-boxfnonce-0.1)
        ("rust-tinyfiledialogs" ,rust-tinyfiledialogs-3)
        ("rust-urlencoding" ,rust-urlencoding-1)
        ("rust-webview-sys" ,rust-webview-sys-0.6))
       #:cargo-development-inputs
       (("rust-actix-rt" ,rust-actix-rt-0.2)
        ("rust-actix-web" ,rust-actix-web-1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-grep" ,rust-grep-0.2)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-rust-embed" ,rust-rust-embed-5)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-walkdir" ,rust-walkdir-2))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+
           webkitgtk-with-libsoup2))
    (home-page "https://github.com/Boscop/web-view")
    (synopsis "Rust bindings for webview")
    (description
     "This library provides a Rust binding to the original implementation of
webview, a tiny cross-platform library to render web-based GUIs as desktop
applications.")
    (license license:expat)))

(define-public rust-webpki-0.22
  (package
    (name "rust-webpki")
    (version "0.22.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lwv7jdlcqjjqqhxcrapnyk5bz4lvr12q444b50gzl3krsjswqzd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ring" ,rust-ring-0.17)
        ("rust-untrusted" ,rust-untrusted-0.9))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/briansmith/webpki")
    (synopsis "Web PKI X.509 Certificate Verification")
    (description "This package provides Web PKI X.509 Certificate
Verification.")
    (license license:isc)))

(define-public rust-webpki-0.21
  (package
    (inherit rust-webpki-0.22)
    (name "rust-webpki")
    (version "0.21.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sm4i8c5bw3bdhi7mjk0wpvwx55hvsmyn0k2lpa4cb161038rqxq"))))
    (arguments
     `(#:tests? #f ;; tests fail to build "missing file tests/ed25519/ee.der"
       #:cargo-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-untrusted" ,rust-untrusted-0.7))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.9))))))

(define-public rust-webpki-0.19
  (package
    (inherit rust-webpki-0.21)
    (name "rust-webpki")
    (version "0.19.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "webpki" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "10nhyxlqsa4caxlxrijm5h79rdg6ld8hqy78ldjnnfhaj3biqzjg"))))
    (arguments
     `(#:tests? #f  ; tests fail to build "missing file tests/ed25519/ee.der"
       #:cargo-inputs
       (("rust-ring" ,rust-ring-0.14)
        ("rust-untrusted" ,rust-untrusted-0.6))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.9))))))

(define-public rust-webpki-0.18
  (package
    (inherit rust-webpki-0.19)
    (name "rust-webpki")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zx1v8afa4ig97dyqfrnlj5i7pib6dnfw88qn2iiqhfq2rrrdmqp"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Fix doctest errors
                 ;; `...` range patterns are deprecated
                 (substitute* "src/name.rs"
                   (("'\\.\\.\\.") "'..="))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ring" ,rust-ring-0.13)
        ("rust-untrusted" ,rust-untrusted-0.6))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.9))))))

(define-public rust-webpki-roots-0.26
  (package
    (name "rust-webpki-roots")
    (version "0.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "029006qfs61q75gl60aap25m0gdqmvd1pcpljid9b0q44yp39pmk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `webpki_ccadb`
       #:cargo-inputs (("rust-rustls-pki-types" ,rust-rustls-pki-types-1))
       #:cargo-development-inputs (("rust-hex" ,rust-hex-0.4)
                                   ("rust-percent-encoding" ,rust-percent-encoding-2)
                                   ("rust-rcgen" ,rust-rcgen-0.12)
                                   ("rust-ring" ,rust-ring-0.17)
                                   ("rust-rustls" ,rust-rustls-0.22)
                                   ("rust-rustls-webpki" ,rust-rustls-webpki-0.102)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-x509-parser" ,rust-x509-parser-0.15)
                                   ("rust-yasna" ,rust-yasna-0.5))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/rustls/webpki-roots")
    (synopsis "Mozilla's CA root certificates for use with webpki")
    (description "This package provides Mozilla's CA root certificates for use
with webpki.")
    (license license:mpl2.0)))

(define-public rust-webpki-roots-0.25
  (package
    (inherit rust-webpki-roots-0.26)
    (name "rust-webpki-roots")
    (version "0.25.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "webpki-roots" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qgqa615gc1cgklls4bkjp9jv9pvv3jnl82lc6wd7dkximywa82z"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             ;; This test wants network access.
             "--skip=generated_code_is_fresh")
       #:cargo-development-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-csv" ,rust-csv-1)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rcgen" ,rust-rcgen-0.11)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
        ("rust-serde" ,rust-serde-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-x509-parser" ,rust-x509-parser-0.15)
        ("rust-yasna" ,rust-yasna-0.5))))))

(define-public rust-webpki-roots-0.24
  (package
    (inherit rust-webpki-roots-0.25)
    (name "rust-webpki-roots")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "120q85pvzpckvvrg085a5jhh91fby94pgiv9y1san7lxbmnm94dj"))))
    (arguments
     `(#:cargo-inputs (("rust-rustls-webpki" ,rust-rustls-webpki-0.101))))))

(define-public rust-webpki-roots-0.23
  (package
    (inherit rust-webpki-roots-0.25)
    (name "rust-webpki-roots")
    (version "0.23.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "webpki-roots" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0f4k8nng542iilxbibh1nhrdf5wbyi9is4fr219zzrc6hgw5hc5h"))))
    (arguments
     `(#:cargo-inputs (("rust-rustls-webpki" ,rust-rustls-webpki-0.100))))))

(define-public rust-webpki-roots-0.22
  (package
    (inherit rust-webpki-roots-0.25)
    (name "rust-webpki-roots")
    (version "0.22.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11rd1aj73qzcvdj3x78crm1758sc4wrbc7rh0r8lmhyjsx01xixn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-webpki" ,rust-webpki-0.22))))))

(define-public rust-webpki-roots-0.21
  (package
    (inherit rust-webpki-roots-0.22)
    (name "rust-webpki-roots")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h49lkr7hrxpyr0xg1nph4m3v1l6rhg8ax9n8msvfwz48hsibgma"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-webpki" ,rust-webpki-0.21))))))

(define-public rust-webpki-roots-0.20
  (package
    (inherit rust-webpki-roots-0.21)
    (name "rust-webpki-roots")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17qpmyym1lsi967b4nc3112nb13ism8731bhjqd9hlajafkxw80g"))))
    (arguments
     `(#:cargo-inputs
       (("rust-webpki" ,rust-webpki-0.21))))))

(define-public rust-webpki-roots-0.19
  (package
    (inherit rust-webpki-roots-0.20)
    (name "rust-webpki-roots")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fapdqwbfv0kncplpvbgnr0bjd5a9krlpij9jdzk0mvaa6vz9vzq"))))))

(define-public rust-webpki-roots-0.18
  (package
    (inherit rust-webpki-roots-0.19)
    (name "rust-webpki-roots")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d4ss607rgi9pj01zzqa13c1p3m35z314yh6lmjaj4kzvwv5gkci"))))))

(define-public rust-webpki-roots-0.17
  (package
    (inherit rust-webpki-roots-0.18)
    (name "rust-webpki-roots")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "12vi8dh0yik0h4f0b9dnlw5i3gxyky7iblbksh6zcq4xvlvswqm2"))))))

(define-public rust-webpki-roots-0.16
  (package
    (inherit rust-webpki-roots-0.17)
    (name "rust-webpki-roots")
    (version "0.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "webpki-roots" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "03ny02mwqdgd2ff23k03kbwr2rrcaymxhp7jcjjikfh340hs83y1"))))
    (arguments
     `(#:cargo-inputs
       (("rust-untrusted" ,rust-untrusted-0.6)
        ("rust-webpki" ,rust-webpki-0.19))))))

(define-public rust-webpki-roots-0.15
  (package
    (inherit rust-webpki-roots-0.20)
    (name "rust-webpki-roots")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1gya8j75jnvf9lz36w0l4bf2xnw8qdx6plvhia891mcgj44g9lc5"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-untrusted" ,rust-untrusted-0.6)
        ("rust-webpki" ,rust-webpki-0.18))))))

(define-public rust-webview-sys-0.6
  (package
    (name "rust-webview-sys")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webview-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jb6h261lzp4b9rp7iwssbc7vs5d3q4wp08a4wvgwps2q4jz0080"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-gdk-sys" ,rust-gdk-sys-0.10)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-sys" ,rust-gtk-sys-0.10)
        ("rust-javascriptcore-rs-sys" ,rust-javascriptcore-rs-sys-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-webkit2gtk-sys" ,rust-webkit2gtk-sys-0.12))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+ webkitgtk-with-libsoup2))
    (home-page "https://github.com/Boscop/web-view")
    (synopsis "Rust native ffi bindings for webview")
    (description "This library provides a Rust binding to the original
implementation of webview, a tiny cross-platform library to render web-based
GUIs as desktop applications.")
    (license license:expat)))

(define-public rust-wiremock-0.5
  (package
    (name "rust-wiremock")
    (version "0.5.22")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wiremock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sf2adr5q3xqrj4sa89fmbr5vl3x51wb1cfp63fr1wrlmwzab8qk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-assert-json-diff" ,rust-assert-json-diff-2)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-deadpool" ,rust-deadpool-0.9)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-timer" ,rust-futures-timer-3)
                       ("rust-http-types" ,rust-http-types-2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-actix-rt" ,rust-actix-rt-2)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-isahc" ,rust-isahc-1)
                                   ("rust-reqwest" ,rust-reqwest-0.11)
                                   ("rust-surf" ,rust-surf-2)
                                   ("rust-tokio" ,rust-tokio-1))))
    (native-inputs (list pkg-config))
    (inputs (list curl openssl zlib))
    (home-page "https://github.com/LukeMathWalker/wiremock-rs")
    (synopsis "HTTP mocking to test Rust applications.")
    (description "This package provides HTTP mocking to test Rust
applications.")
    (license (list license:expat license:asl2.0))))
