;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020-2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2023 VÖRÖSKŐI András <voroskoi@gmail.com>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
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
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-windows)
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

(define-public rust-actix-codec-0.3
  (package
    (name "rust-actix-codec")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-codec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w7506qd2f8q83z6l5lqx1363ks0ysx8f7qgvy8fknrq70xq7lbq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-sink" ,rust-futures-sink-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-pin-project" ,rust-pin-project-0.4)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-util" ,rust-tokio-util-0.3))))
    (home-page "https://actix.rs")
    (synopsis "Codec utilities for working with framed protocols")
    (description
     "This package provides codec utilities for working with framed
protocols.")
    (license (list license:expat license:asl2.0))))

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
     ;; XXX: The crate fails to't build with: "error[E0432]: unresolved import
     ;; `trust_dns_resolver::Background`".  I assume it really expects
     ;; trust-dns-resolver at version 0.18-alpha.2, which we do not provide.
     `(#:skip-build? #true
       #:cargo-inputs
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
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.18)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-actix-testing" ,rust-actix-testing-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-version-requirements
           (lambda _
             (substitute* "Cargo.toml"
               (("0.18.0-alpha.2")
                ,(package-version rust-trust-dns-proto-0.18)))
             #t)))))))

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

(define-public rust-actix-http-2
  (package
    (name "rust-actix-http")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x78h9lzqdhp06v1kf4dhbiqp8sc911w4lqfj5rmdbhpg3l9j8j5"))))
    (build-system cargo-build-system)
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
        ("rust-time" ,rust-time-0.2))))
    (home-page "https://actix.rs")
    (synopsis "HTTP primitives for the Actix ecosystem")
    (description
     "This package provides HTTP primitives for the Actix ecosystem.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-actix-router-0.2
  (package
    (name "rust-actix-router")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-router" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0df2626hk4n4yki6j88v3k0gcm8pi5hdnm1mldyvyi8nvbdzgldv"))))
    (build-system cargo-build-system)
    (arguments
     ;; Tests fail with "error[E0432]: unresolved import `serde_derive`".
     `(#:tests? #false
       #:cargo-inputs
       (("rust-bytestring" ,rust-bytestring-0.1)
        ("rust-http" ,rust-http-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1))))
    (home-page "https://actix.rs")
    (synopsis "Resource path matching library")
    (description "This package provides resource path matching library.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-actix-server-1
  (package
    (name "rust-actix-server")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13khzd6pz9pqksxmw2syipfwq2gi5v9warx6pa24g8iccxp7wh25"))))
    (build-system cargo-build-system)
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
        ("rust-socket2" ,rust-socket2-0.3))))
    (home-page "https://actix.rs")
    (synopsis "General purpose TCP server built for the Actix ecosystem")
    (description
     "This package provides a general purpose TCP server built for the Actix
ecosystem.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-actix-service-1
  (package
    (name "rust-actix-service")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-service" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fw2b1cpxrpqk778mpvxv0cazj0pwjyb6khzs4syhnqvb1fl6lh0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-pin-project" ,rust-pin-project-0.4))
       #:cargo-development-inputs
       (("rust-actix-rt" ,rust-actix-rt-1)
        ("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://actix.rs")
    (synopsis
     "Service trait and combinators for asynchronous request/response")
    (description
     "This package provides a service trait and combinators for representing
asynchronous request/response operations.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-actix-tls-2
  (package
    (name "rust-actix-tls")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yqmlyn02c72a1rrmjkfx5hnz286130y3sq4ll1mbkv1fdyrny14"))))
    (build-system cargo-build-system)
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
        ("rust-webpki-roots" ,rust-webpki-roots-0.20))))
    (home-page "https://actix.rs")
    (synopsis "TLS acceptor services for Actix ecosystem")
    (description
     "This package provides TLS acceptor services for Actix ecosystem.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-actix-utils-2
  (package
    (name "rust-actix-utils")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nkby6wpwcmjr3zcghd962l2hyjry0aayncyjzbx2ck6qpg2541f"))))
    (build-system cargo-build-system)
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
        ("rust-slab" ,rust-slab-0.4))))
    (home-page "https://actix.rs")
    (synopsis "Network related services and utilities for the Actix ecosystem")
    (description
     "This package provides various network related services and utilities for
the Actix ecosystem.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-actix-web-3
  (package
    (name "rust-actix-web")
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11kv8y1p9dw78lnhrw3rqavhmazmy7s0z8j14a3a1yp7fahx8hg6"))))
    (build-system cargo-build-system)
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
        ("rust-url" ,rust-url-2))))
    (home-page "https://actix.rs")
    (synopsis "Powerful, pragmatic, and fast web framework for Rust")
    (description
     "Actix Web is a powerful, pragmatic, and fast web framework for
Rust.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-actix-web-codegen-0.4
  (package
    (name "rust-actix-web-codegen")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ys3f6q0hgflqvp271s49q88m41db3iynm7ydxy0wgikjdqgf9md"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://actix.rs")
    (synopsis "Actix web proc macros")
    (description "This package provides Actix web proc macros.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-h2-0.3
  (package
    (name "rust-h2")
    (version "0.3.21")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cq8g5bgk3fihnqicy3g8gc3dpsalzqjg4bjyip9g4my26m27z4i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
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
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.9)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/hyperium/h2")
    (synopsis "HTTP/2.0 client and server")
    (description "This package provides an HTTP/2.0 client and server.")
    (license license:expat)))

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

(define-public rust-h3-0.0.3
  (package
    (name "rust-h3")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "101vg73galsyk5gnjb49cjb6q40c9z2npcdxpfsj99ky2waijgmq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included
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
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "Async HTTP/3 implementation")
    (description "This package provides an async HTTP/3 implementation.")
    (license license:expat)))

(define-public rust-h3-quinn-0.0.4
  (package
    (name "rust-h3-quinn")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3-quinn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r0sm0j51crlfpy2j1wfhgpg2lrfq2xmf5qjd98ksg3h9l0pb5mc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-h3" ,rust-h3-0.0.3)
                       ("rust-quinn" ,rust-quinn-0.10)
                       ("rust-quinn-proto" ,rust-quinn-proto-0.10)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "QUIC transport implementation based on Quinn")
    (description
     "This package provides QUIC transport implementation based on Quinn.")
    (license license:expat)))

(define-public rust-hyper-0.14
  (package
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
        ("rust-url" ,rust-url-2))))
    (home-page "https://hyper.rs")
    (synopsis "Fast and correct HTTP library")
    (description "This package provides a fast and correct HTTP library.")
    (license license:expat)))

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

(define-public rust-hyper-rustls-0.24
  (package
    (name "rust-hyper-rustls")
    (version "0.24.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1475j4a2nczz4aajzzsq3hpwg1zacmzbqg393a14j80ff8izsgpc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f              ; Not all files included.
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
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/rustls/hyper-rustls")
    (synopsis "Rustls+Hyper integration for pure Rust HTTPS")
    (description
     "This package provides Rustls+Hyper integration for pure Rust HTTPS.")
    (license (list license:asl2.0 license:isc license:expat))))

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

(define-public rust-hyper-tls-0.5
  (package
    (name "rust-hyper-tls")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01crgy13102iagakf6q4mb75dprzr7ps1gj0l5hxm1cvm7gks66n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3))))
    (home-page "https://hyper.rs")
    (synopsis "Default TLS implementation for use with hyper")
    (description "This package provides the default TLS implementation for use
with hyper.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-rocket-0.4
  (package
    (name "rust-rocket")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ybnhjw92zaan92lsmx6mkhqc9cpsg3885svb3wzyj39pyzvsvz"))))
    (build-system cargo-build-system)
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
        ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://rocket.rs")
    (synopsis
     "Web framework with focus on ease-of-use, expressibility, and speed")
    (description
     "Rocket is a web framework with a focus on ease-of-use, expressibility,
and speed.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-codegen-0.4
  (package
    (name "rust-rocket-codegen")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket_codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18s2dll8c4sd26s8cfr6cizj5z55xwnk6r6x7b2wvcf8n9ajrb6f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-devise" ,rust-devise-0.2)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-quote" ,rust-quote-0.6)
        ("rust-rocket-http" ,rust-rocket-http-0.4)
        ("rust-version-check" ,rust-version-check-0.9)
        ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://rocket.rs")
    (synopsis "Procedural macros for the Rocket web framework")
    (description
     "This package provides procedural macros for the Rocket web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-http-0.4
  (package
    (name "rust-rocket-http")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket_http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ga98nbcga8amg4xhrfkn1wljnqx9h0vv7mnay9g66vsxl042dnf"))))
    (build-system cargo-build-system)
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
        ("rust-unicode-xid" ,rust-unicode-xid-0.1))))
    (home-page "https://rocket.rs")
    (synopsis "HTTP requests, responses and headers tooling for Rocket")
    (description
     "This package provides types, traits, and parsers for HTTP requests,
responses, and headers for the Rocket web framework.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-trust-dns-https-0.18
  (package
    (inherit rust-trust-dns-https-0.19)
    (name "rust-trust-dns-https")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-https" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03dapd5larsjlpk6mr4xnm2sb0h7l6dg988wjnaxd8zfi5swq5nl"))))
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
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.18)
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

(define-public rust-trust-dns-native-tls-0.18
  (package
    (inherit rust-trust-dns-native-tls-0.19)
    (name "rust-trust-dns-native-tls")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rcg018vdd5chd4hcmjp753qjlf4k311nmrxa5ay2hxjllzmqd1y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #false                  ;missing files
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18))))))

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

(define-public rust-trust-dns-openssl-0.18
  (package
    (inherit rust-trust-dns-openssl-0.19)
    (name "rust-trust-dns-openssl")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-openssl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1870s27ifsdh9plgcwwbxzvlw17r3dn9v6s0zfryf6kfp9hzpfz2"))))
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.4)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18))
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
    (version "0.19.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0a4zlv60kkbg1nvy3zh18fdg681z83yzppzy39rdkm7llqdhdmyd"))))
    (arguments
     `(#:cargo-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-backtrace" ,rust-backtrace-0.3)
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

(define-public rust-trust-dns-proto-0.18
  (package
    (inherit rust-trust-dns-proto-0.19)
    (name "rust-trust-dns-proto")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vmhw7vdaa6b7wfv438f272ijjl2qlpcp6b1myvif4iay8pp4fi5"))))
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
    (version "0.19.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0xqv31gndybcrr5gi6jjp47qcvdxsc147s69a0y0nc6qqgyws8qg"))))
    (arguments
     `(#:tests? #false                  ;network unreachable
       #:cargo-inputs
       (("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-ipconfig" ,rust-ipconfig-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-lru-cache" ,rust-lru-cache-0.1)
        ("rust-resolv-conf" ,rust-resolv-conf-0.6)
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

(define-public rust-trust-dns-resolver-0.18
  (package
    (inherit rust-trust-dns-resolver-0.19)
    (name "rust-trust-dns-resolver")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cldg6y937il4kjk7rirgfhmk0chz41w7qys9h96skaznh4dzmvj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #false                  ;network unreachable
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
        ("rust-trust-dns-https" ,rust-trust-dns-https-0.18)
        ("rust-trust-dns-native-tls" ,rust-trust-dns-native-tls-0.18)
        ("rust-trust-dns-openssl" ,rust-trust-dns-openssl-0.18)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls-0.18)
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
    (version "0.19.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hj4fx2x4ncj7v8pf6bbn7634zq76hjigm1s2h6b6yjzzmz4yprn"))))
    (arguments
     `(#:tests? #false                  ;missing file
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

(define-public rust-trust-dns-rustls-0.18
  (package
    (inherit rust-trust-dns-rustls-0.19)
    (name "rust-trust-dns-rustls")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19vhb0xsyr0wy4p0liwhv4rqmwv6szfmmid6439gq7wah1x1hzp4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #false                  ;missing file
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.18)
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

(define-public rust-warp-0.2
  (package
    (name "rust-warp")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "warp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01wl8kv5hh1dd7gcwdrmn9xfs7jjsh9yc8xa06ph8yf9akgyc6zl"))))
    (build-system cargo-build-system)
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
        ("rust-urlencoding" ,rust-urlencoding-1))))
    (home-page "https://github.com/seanmonstar/warp")
    (synopsis "Composable web server framework")
    (description "Warp is a composable, web server framework.")
    (license license:expat)))

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
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gd1gxip5kgdwmrvhj5gjxij2mgg2mavq1ych4q1h272ja0xg5gh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-untrusted" ,rust-untrusted-0.7))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.9))))
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

(define-public rust-webpki-roots-0.25
  (package
    (name "rust-webpki-roots")
    (version "0.25.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "webpki-roots" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15piy0vccppqb74li32gnn9l5a4ysxzwh8bp3qv6z8rhr2hyvin9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             ;; This test wants network access.
             "--skip=generated_code_is_fresh")
       #:cargo-development-inputs
       (("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
        ("rust-tokio" ,rust-tokio-1))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/rustls/webpki-roots")
    (synopsis "Mozilla's CA root certificates for use with webpki")
    (description "This package provides Mozilla's CA root certificates for use
with webpki.")
    (license license:mpl2.0)))

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
    (version "0.22.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jbll0ys9jakrvv3l1i216bbgj7jbxr7ad2dihw28xcm7s8fnb2m"))))
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
