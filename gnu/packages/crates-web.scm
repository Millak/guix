;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
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
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit))

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
