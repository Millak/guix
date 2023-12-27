;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020-2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 VÖRÖSKŐI András <voroskoi@gmail.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-tls)
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
        (base32 "0zx1v8afa4ig97dyqfrnlj5i7pib6dnfw88qn2iiqhfq2rrrdmqp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; TODO: Fix building rust-ring-0.13
       #:cargo-inputs
       (("rust-ring" ,rust-ring-0.13)
        ("rust-untrusted" ,rust-untrusted-0.6))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.9))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-curve25519-tables
           (lambda* (#:key vendor-dir #:allow-other-keys)
             (with-directory-excursion
               (dirname (car (find-files vendor-dir "make_curve25519_tables.py")))
               (with-output-to-file "curve25519_tables.h"
                 (lambda _
                   (invoke "python" "make_curve25519_tables.py")))))))))))

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
