;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2022-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Domagoj Stolfa <domagoj.stolfa@gmail.com>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Attila Lendvai <attila@lendvai.name>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2023 VÖRÖSKŐI András <voroskoi@gmail.com>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2025 Jordan Moore <lockbox@struct.foo>
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

(define-module (gnu packages crates-crypto)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-aead-0.5
  (package
    (name "rust-aead")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aead" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c32aviraqag7926xcb9sybdm36v5vh9gnxpn4pxdwjc50zl28ni"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-arrayvec" ,rust-arrayvec-0.7)
        ("rust-blobby" ,rust-blobby-0.3)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-crypto-common" ,rust-crypto-common-0.1)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-heapless" ,rust-heapless-0.7))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis
     "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms")
    (description
     "This package provides traits for Authenticated Encryption with Associated
Data (AEAD) algorithms, such as AES-GCM as ChaCha20Poly1305, which provide a
high-level API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-aead-0.4
  (package
    (inherit rust-aead-0.5)
    (name "rust-aead")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aead" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xw8kp9j1whfdxhgmr2qf9xgslkg52zh6gzmhsh13y9w3s73nq8b"))))
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-heapless" ,rust-heapless-0.7)
        ("rust-rand-core" ,rust-rand-core-0.6))))))

(define-public rust-aead-0.3
  (package
    (name "rust-aead")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aead" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0c8388alvivcj4qkxgh4s4l6fbczn3p8wc0pnar6crlfvcdmvjbz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-heapless" ,rust-heapless-0.5))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for Authenticated Encryption with Associated Data (AEAD)
algorithms")
    (description "This package provides traits for Authenticated Encryption
with Associated Data (AEAD) algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-aes-0.8
  (package
    (name "rust-aes")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1853796anlwp4kqim0s6wm1srl4ib621nm0cl2h3c8klsjkgfsdi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cipher" ,rust-cipher-0.4)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-cipher" ,rust-cipher-0.4)
        ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Facade for AES (Rijndael) block ciphers implementations")
    (description "This package provides a facade for AES (Rijndael) block
ciphers implementations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-aes-0.7
  (package
    (inherit rust-aes-0.8)
    (name "rust-aes")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f0sdx2fsa8w3l7xzsyi9ry3shvnnsgc0znh50if9fm95vslg2wy"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cipher" ,rust-cipher-0.3)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-ctr" ,rust-ctr-0.8)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3))
       #:cargo-development-inputs (("rust-cipher" ,rust-cipher-0.3)
                                   ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-aes-0.6
  (package
    (inherit rust-aes-0.7)
    (name "rust-aes")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q85mw70mgr4glza9y9lrs9nxfa1cdcqzfk6wx0smb3623pr2hw8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-aes-soft" ,rust-aes-soft-0.6)
        ("rust-aesni" ,rust-aesni-0.10)
        ("rust-cipher" ,rust-cipher-0.2))
       #:cargo-development-inputs
       (("rust-cipher" ,rust-cipher-0.2))))))

(define-public rust-aes-0.4
  (package
    (inherit rust-aes-0.6)
    (name "rust-aes")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xgsp2bn5llsppald60iw4497gaspslg0a8hknhniiz4zmki607p"))))
    (arguments
     `(#:cargo-inputs
       (("rust-aes-soft" ,rust-aes-soft-0.4)
        ("rust-aesni" ,rust-aesni-0.7)
        ("rust-block-cipher" ,rust-block-cipher-0.7))
       #:cargo-development-inputs
       (("rust-block-cipher" ,rust-block-cipher-0.7))))))

(define-public rust-aes-0.3.2-yanked
  (package
    (inherit rust-aes-0.4)
    (name "rust-aes")
    (version "0.3.2") ; This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32 "1j90iwpax0y1dqq14i8y9xgpcnnlgnljwkxg3mhzrralwf7ivssl"))))
    (arguments
     `(#:cargo-inputs
       (("rust-aes-soft" ,rust-aes-soft-0.3)
        ("rust-aesni" ,rust-aesni-0.6)
        ("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6))
       #:cargo-development-inputs
       (("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-aes-0.3
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-aes" rust-aes-0.3.2-yanked))

(define-public rust-aes-ctr-0.6
  (package
    (name "rust-aes-ctr")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-ctr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qspjxzrclnb83501595y01dhc0km1ssrbjnwlxhcrsdwp6w6abp"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-aesni" ,rust-aesni-0.10)
             ("rust-aes-soft" ,rust-aes-soft-0.6)
             ("rust-cipher" ,rust-cipher-0.2)
             ("rust-ctr" ,rust-ctr-0.6))
      #:cargo-development-inputs `(("rust-cipher" ,rust-cipher-0.2))))
    (home-page "https://github.com/RustCrypto/block-ciphers/tree/master/aes")
    (synopsis "Pure Rust implementation of AES")
    (description
     "A pure Rust implementation of the @acronym{AES, Advanced Encryption
Standard}.  Use the AES crate if possible, as the aes-ctr has been into it.")
    (license (list license:expat license:asl2.0))))

(define-public rust-aes-gcm-0.10
  (package
    (name "rust-aes-gcm")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-gcm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lgaqgg1gh9crg435509lqdhajg1m2vgma6f7fdj1qa2yyh10443"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aead" ,rust-aead-0.5)
        ("rust-aes" ,rust-aes-0.8)
        ("rust-cipher" ,rust-cipher-0.4)
        ("rust-ctr" ,rust-ctr-0.9)
        ("rust-ghash" ,rust-ghash-0.5)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-aead" ,rust-aead-0.5)
        ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/AEADs")
    (synopsis "AES-GCM (Galois/Counter Mode) Authenticated Encryption")
    (description "This package provides a pure Rust implementation of the
AES-GCM (Galois/Counter Mode) Authenticated Encryption with Associated
Data (AEAD) Cipher with optional architecture-specific hardware
acceleration.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-aes-gcm-0.9
  (package
    (inherit rust-aes-gcm-0.10)
    (name "rust-aes-gcm")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-gcm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xndncn1phjb7pjam63vl0yp7h8jh95m0yxanr1092vx7al8apyz"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (((string-append ">=([[:digit:]]+(\\.[[:digit:]]+)*),"
                                   " <([[:digit:]]+(\\.[[:digit:]]+)*)")
                    _ version _)
                   (string-append ">=" version)))))))
    (arguments
     `(#:cargo-inputs (("rust-aead" ,rust-aead-0.4)
                       ("rust-aes" ,rust-aes-0.7)
                       ("rust-cipher" ,rust-cipher-0.3)
                       ("rust-ctr" ,rust-ctr-0.8)
                       ("rust-ghash" ,rust-ghash-0.4)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-aead" ,rust-aead-0.4)
                                   ("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-aes-gcm-0.8
  (package
    (inherit rust-aes-gcm-0.10)
    (name "rust-aes-gcm")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-gcm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nl8iwlh209y1vj9n2lm1a70i69clvg2z6x69bi4dgdrpgxbay2j"))))
    (arguments
     `(#:cargo-inputs
       (("rust-aead" ,rust-aead-0.3)
        ("rust-aes" ,rust-aes-0.6)
        ("rust-cipher" ,rust-cipher-0.2)
        ("rust-ctr" ,rust-ctr-0.6)
        ("rust-ghash" ,rust-ghash-0.3)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-aes-soft-0.6
  (package
    (name "rust-aes-soft")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-soft" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wj0fi2pvmlw09yvb1aqf0mfkzrfxmjsf90finijh255ir4wf55y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-block-cipher" ,rust-block-cipher-0.7)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2))
       #:cargo-development-inputs
       (("rust-block-cipher" ,rust-block-cipher-0.7))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Bit-sliced implementation of AES (Rijndael) block ciphers")
    (description "This package provides a bit-sliced implementation of
AES (Rijndael) block ciphers.

This package is deprecated and was replaced by the @code{aes} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-aes-soft-0.4.0-yanked
  (package
    (inherit rust-aes-soft-0.6)
    (name "rust-aes-soft")
    (version "0.4.0") ; This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-soft" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32
         "19szsg0qqxq42k7bj5p3svb147n8wxy9a20n4g7mcl2fwrz689a9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-block-cipher" ,rust-block-cipher-0.7)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2))
       #:cargo-development-inputs
       (("rust-block-cipher" ,rust-block-cipher-0.7))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-aes-soft-0.4
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-aes-soft" rust-aes-soft-0.4.0-yanked))

(define-public rust-aes-soft-0.3.3-yanked
  (package
    (inherit rust-aes-soft-0.4)
    (name "rust-aes-soft")
    (version "0.3.3") ; This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-soft" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32 "039si7yjp0wcd750sgq52c60sh2ikaxwd7rq7g0ba7ws7ypfgmyg"))))
    (arguments
     `(#:cargo-inputs
       (("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2))
       #:cargo-development-inputs
       (("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-aes-soft-0.3
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-aes-soft" rust-aes-soft-0.3.3-yanked))

(define-public rust-aesni-0.10
  (package
    (name "rust-aesni")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aesni" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kmh07fp9hbi1aa8dr2rybbgw8vqz6hjmk34c4w7sbscx7si2bpa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.2)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "AES (Rijndael) block ciphers implementation using AES-NI")
    (description "This package provides an implementation of AES (Rijndael)
block ciphers using AES-NI.

This package is deprecated and was replaced by the @code{aes} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-aesni-0.7.0-yanked
  (package
    (inherit rust-aesni-0.10)
    (name "rust-aesni")
    (version "0.7.0") ; This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aesni" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32
         "0r6j0mjkyqnwvgib01cvrwfw8rlx1biw75234niv723n1fdx6l6h"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-block-cipher" ,rust-block-cipher-0.7)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2)
        ("rust-stream-cipher" ,rust-stream-cipher-0.4))
       #:cargo-development-inputs
       (("rust-block-cipher" ,rust-block-cipher-0.7)
        ("rust-stream-cipher" ,rust-stream-cipher-0.4))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-aesni-0.7
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-aesni" rust-aesni-0.7.0-yanked))

(define-public rust-aesni-0.6.0-yanked
  (package
    (inherit rust-aesni-0.7)
    (name "rust-aesni")
    (version "0.6.0") ; This version is yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aesni" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32 "007imgcfl82nilfpamj5dik83pkcmkzvbkxp384p7r3iz6sscw1g"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2)
        ("rust-stream-cipher" ,rust-stream-cipher-0.3))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-aesni-0.6
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-aesni" rust-aesni-0.6.0-yanked))

(define-public rust-argon2-0.5
  (package
    (name "rust-argon2")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "argon2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wn0kk97k49wxidfigmz1pdqmygqzi4h6w72ib7cpq765s4i0diw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64ct" ,rust-base64ct-1)
        ("rust-blake2" ,rust-blake2-0.10)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-password-hash" ,rust-password-hash-0.5)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-password-hash" ,rust-password-hash-0.5))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/argon2")
    (synopsis "Rust argon2 library")
    (description
     "Pure Rust implementation of the Argon2 password hashing function with support
for the Argon2d, Argon2i, and Argon2id algorithmic variants.")
    (license (list license:expat license:asl2.0))))

(define-public rust-base16ct-0.2
  (package
    (name "rust-base16ct")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "base16ct" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kylrjhdzk7qpknrvlphw8ywdnvvg39dizw9622w3wk5xba04zsc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustCrypto/formats/tree/master/base16ct")
    (synopsis "Rust implementation of Base16 a.k.a hexadecimal")
    (description
     "This package provides a pure Rust implementation of Base16 a.k.a
hexadecimal (RFC 4648) which avoids any usages of data-dependent branches/LUTs
and thereby provides portable \"best effort\" constant-time operation and
embedded-friendly no_std support.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-base16ct-0.1
  (package
    (inherit rust-base16ct-0.2)
    (name "rust-base16ct")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base16ct" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1klccxr7igf73wpi0x3asjd8n0xjg0v6a7vxgvfk5ybvgh1hd6il"))))))

(define-public rust-base64ct-1
  (package
    (name "rust-base64ct")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base64ct" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nvdba4jb8aikv60az40x2w1y96sjdq8z3yp09rwzmkhiwv1lg4c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.21)
        ("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/base64ct")
    (synopsis "Implementation of Base64 (RFC 4648)")
    (description
     "This package is a pure Rust implementation of Base64 (RFC 4648) which
avoids any usages of data-dependent branches/LUTs and thereby provides
portable \"best effort\" constant-time operation and embedded-friendly
@code{no_std} support.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-bcrypt-pbkdf-0.10
  (package
    (name "rust-bcrypt-pbkdf")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bcrypt-pbkdf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18pjhsy3m2v0silsp4mjzz8i92zrpqxk9b059zrnk1w8zvhw5ska"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-blowfish" ,rust-blowfish-0.9)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.12)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/bcrypt-pbkdf")
    (synopsis "Bcrypt-pbkdf password-based key derivation function")
    (description
     "This package provides bcrypt-pbkdf password-based key derivation function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bcrypt-pbkdf-0.6
  (package
    (inherit rust-bcrypt-pbkdf-0.10)
    (name "rust-bcrypt-pbkdf")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bcrypt-pbkdf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ms9c5z90n5szx5nbxrqaihny5fs3sl6a1pm3szr5g86jlxw0f3w"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "Cargo.toml"
                   (("\"= ?([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                    (string-append "\"^" version)))))))
    (arguments
     `(#:cargo-inputs (("rust-blowfish" ,rust-blowfish-0.8)
                       ("rust-crypto-mac" ,rust-crypto-mac-0.11)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.8)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-zeroize" ,rust-zeroize-1))))))

(define-public rust-blake2-0.10
  (package
    (name "rust-blake2")
    (version "0.10.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blake2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zlf7w7gql12v61d9jcbbswa3dw8qxsjglylsiljp9f9b3a2ll26"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.10))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "BLAKE2 hash functions")
    (description "This package provides BLAKE2 hash functions in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-blake2-0.9
  (package
    (inherit rust-blake2-0.10)
    (name "rust-blake2")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blake2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x7i67c0hn3bzcwny08rgjrrnarqnqw10qpmh2blbx9hd78kfkha"))))
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.8)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3))
       #:cargo-development-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.8)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-blake2-0.8
  (package
    (inherit rust-blake2-0.10)
    (name "rust-blake2")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blake2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0c4k11j04kqhkci6i9b7lz6p13kpcv228pdqixgrawvavaq0gjwl"))))
    (arguments
     `(#:tests? #f      ; Not all tests compile.
       #:cargo-inputs
       (("rust-byte-tools" ,rust-byte-tools-0.3)
        ("rust-crypto-mac" ,rust-crypto-mac-0.7)
        ("rust-digest" ,rust-digest-0.8)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2))
       #:cargo-development-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.7)
        ("rust-digest" ,rust-digest-0.8)
        ("rust-hex-literal" ,rust-hex-literal-0.1))))))

(define-public rust-blake2b-simd-1
  (package
    (name "rust-blake2b-simd")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "blake2b_simd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "102pfciq6g59hf47gv6kix42cgpqw8pjyf9hx0r3jyb94b9mla13"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-arrayref" ,rust-arrayref-0.3)
        ("rust-arrayvec" ,rust-arrayvec-0.7)
        ("rust-constant-time-eq" ,rust-constant-time-eq-0.3))))
    (home-page "https://github.com/oconnor663/blake2_simd")
    (synopsis "Pure Rust BLAKE2b implementation with dynamic SIMD")
    (description
     "This package provides a pure Rust implementation of the BLAKE2b and
BLAKE2bp hash functions.")
    (license license:expat)))

(define-public rust-blake2b-simd-0.5
  (package
    (inherit rust-blake2b-simd-1)
    (name "rust-blake2b-simd")
    (version "0.5.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blake2b_simd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11y5nm06lpypz65dbxgncs12ckx24i5i4a777ckfhfxd93ili9xg"))))
    (arguments
     `(#:cargo-inputs
       (("rust-arrayref" ,rust-arrayref-0.3)
        ("rust-arrayvec" ,rust-arrayvec-0.5)
        ("rust-constant-time-eq" ,rust-constant-time-eq-0.1))))))

(define-public rust-blakeout-0.3
  (package
    (name "rust-blakeout")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blakeout" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dxcg3sjxd82mn7mq4ammrfpidqf9zsagvhfzgblsi8g4b2fgvw1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-blake2" ,rust-blake2-0.9)
        ("rust-digest" ,rust-digest-0.9))))
    (home-page "https://github.com/Revertron/Blakeout")
    (synopsis "Memory hard hashing algorithm based on Blake2s")
    (description "This package provides memory hard hashing algorithm
based on Blake2s.")
    (license (list license:expat license:asl2.0))))

(define-public rust-block-cipher-0.7
  (package
    (name "rust-block-cipher")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block-cipher" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "043zgfz1x4sxkdcsyabrcr440fcwhfpcqqa54jm7zp35wx4n84zs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for description of block ciphers")
    (description "This package provides traits for description of block
ciphers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-block-cipher-trait-0.6.2-yanked
  (package
    (name "rust-block-cipher-trait")
    (version "0.6.2") ; This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block-cipher-trait" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32 "0x273w6fwka0i48nrv428birdrs2jz6jdnmc0dhc1rq9pm4lv4hw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-generic-array" ,rust-generic-array-0.12))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Block cipher algorithms")
    (description "This package provides a collection of block cipher
algorithms.  This package is deprecated.  Please use block-cipher instead.")
    (license (list license:expat license:asl2.0))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-block-cipher-trait-0.6
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-block-cipher-trait"
                      rust-block-cipher-trait-0.6.2-yanked))

(define-public rust-block-cipher-trait-0.4.2-yanked
  (package
    (inherit rust-block-cipher-trait-0.6)
    (name "rust-block-cipher-trait")
    (version "0.4.2") ; This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block-cipher-trait" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32
         "10qmg8vphqmfllb9a2yx6s7r66jh1wh33clhsawq7ikg2wgz2p6q"))))
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array-0.8))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-block-cipher-trait-0.4
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-block-cipher-trait"
                      rust-block-cipher-trait-0.4.2-yanked))

(define-public rust-block-modes-0.8
  (package
    (name "rust-block-modes")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block-modes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13id7rw1lhi83i701za8w5is3a8qkf4vfigqw3f8jp8mxldkvc1c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-block-padding" ,rust-block-padding-0.2)
        ("rust-cipher" ,rust-cipher-0.3))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Block cipher modes of operation")
    (description "This package provides a collection of block ciphers
and block modes.")
    (license (list license:expat license:asl2.0))))

(define-public rust-blowfish-0.9
  (package
    (name "rust-blowfish")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "blowfish" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mw7bvj3bg5w8vh9xw9xawqh7ixk2xwsxkj34ph96b9b1z6y44p4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs
       (("rust-cipher" ,rust-cipher-0.4))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Blowfish block cipher")
    (description "Blowfish block cipher")
    (license (list license:expat license:asl2.0))))

(define-public rust-blowfish-0.8
  (package
    (inherit rust-blowfish-0.9)
    (name "rust-blowfish")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blowfish" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ax736islxcbghc2lqq4vy7zn6qdigrls71lwg11m3743pyg6gzy"))))
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cipher" ,rust-cipher-0.3)
                       ("rust-opaque-debug" ,rust-opaque-debug-0.3))
       #:cargo-development-inputs (("rust-cipher" ,rust-cipher-0.3))))))

(define-public rust-botan-0.10
  (package
    (name "rust-botan")
    (version "0.10.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "botan" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gn5aznnaxwlf2500q5dk9c24sgy7dasqqzql7w86s1w3apq201m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-botan-sys" ,rust-botan-sys-0.10))
       #:cargo-development-inputs
       (("rust-hex" ,rust-hex-0.4)
        ("rust-wycheproof" ,rust-wycheproof-0.5))))
    (inputs (list botan))
    (home-page "https://botan.randombit.net/")
    (synopsis "Rust wrapper for Botan cryptography library")
    (description "Rust wrapper for Botan cryptography library")
    (license license:expat)))

(define-public rust-botan-0.8
  (package
    (inherit rust-botan-0.10)
    (name "rust-botan")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "botan" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "08bmiyn7c3b0dgx20w6hr28d9jcq7cj78cchr84pc686sb2s41ik"))))
    (arguments
     `(#:cargo-inputs
       (("rust-botan-sys" ,rust-botan-sys-0.8)
        ("rust-cstr-core" ,rust-cstr-core-0.2)
        ("rust-cty" ,rust-cty-0.2))))))

(define-public rust-botan-src-0.30101
  (package
    (name "rust-botan-src")
    (version "0.30101.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "botan-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17xwnan8r21hzbxdailnidp9q54g787s9njhy63yqw83q0k09bxa"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "botan")))))
    (build-system cargo-build-system)
    (arguments '(#:skip-build? #t))
    (home-page "https://botan.randombit.net/")
    (synopsis "Sources of Botan cryptography library")
    (description "Sources of Botan cryptography library")
    (license license:expat)))

(define-public rust-botan-src-0.21703
  (package
    (inherit rust-botan-src-0.30101)
    (name "rust-botan-src")
    (version "0.21703.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "botan-src" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s2ad9q84qsrllfsbj7hjhn7gr3hab9ng6lwzwqmimia6yvja8y8"))
              (modules '((guix build utils)))
              (snippet
               '(begin (delete-file-recursively "botan")))))))

(define-public rust-botan-sys-0.10
  (package
    (name "rust-botan-sys")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "botan-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ji12rxvi4h7pap772cd2hw4xdgqdsgw6m8wqin9klpbp3hxsjcz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-botan-src" ,rust-botan-src-0.30101))))
    (inputs (list botan))
    (home-page "https://botan.randombit.net/")
    (synopsis "FFI wrapper for Botan cryptography library")
    (description "FFI wrapper for Botan cryptography library")
    (license license:expat)))

(define-public rust-botan-sys-0.8
  (package
    (inherit rust-botan-sys-0.10)
    (name "rust-botan-sys")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "botan-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1m11zblxfanrhl97j7z3ap7n17rr8j0rg91sr7f9j6y2bsniaz1x"))))
    (arguments
     `(#:cargo-inputs
       (("rust-botan-src" ,rust-botan-src-0.21703)
        ("rust-cty" ,rust-cty-0.2))))))

(define-public rust-c2-chacha-0.2
  (package
    (name "rust-c2-chacha")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2-chacha" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16mxizk6a1lq6fv1m9zmlc3s9s4wvx8iv2n3p89qp38h8g4r4w91"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-ppv-lite86" ,rust-ppv-lite86-0.2)
        ("rust-stream-cipher" ,rust-stream-cipher-0.3))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/cryptocorrosion/cryptocorrosion")
    (synopsis "The ChaCha family of stream ciphers")
    (description
     "The ChaCha family of stream ciphers.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-camellia-0.1
  (package
    (name "rust-camellia")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "camellia" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c6f61rf0gzq7x9d2qmp0330pb397aldwdpmwqybbwly9rby4r1j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs (("rust-cipher" ,rust-cipher-0.4))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Camellia block cipher")
    (description "This package provides the camellia block cipher.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cast5-0.11
  (package
    (name "rust-cast5")
    (version "0.11.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cast5" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04crg8dj6lxbp3lmdc3filsahxcyvccvhm0gx40g1k5i7mkpvc16"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs
       (("rust-cipher" ,rust-cipher-0.4)
        ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "CAST5 block cipher")
    (description "CAST5 block cipher")
    (license (list license:expat license:asl2.0))))

(define-public rust-cbc-0.1
  (package
    (name "rust-cbc")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cbc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19l9y9ccv1ffg6876hshd123f2f8v7zbkc4nkckqycxf8fajmd96"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-cipher" ,rust-cipher-0.4)
        ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/block-modes")
    (synopsis "Cipher Block Chaining (CBC) block cipher mode of operation")
    (description "Cipher Block Chaining (CBC) block cipher mode of operation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cfb-mode-0.8
  (package
    (name "rust-cfb-mode")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cfb-mode" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c6kd34jk4p52vr0qgn9slj6zdgmc42gfcqr6mqhmy37g138v2vk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-cipher" ,rust-cipher-0.4)
        ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/block-modes")
    (synopsis "Cipher Feedback (CFB) block cipher mode of operation")
    (description "Cipher Feedback (CFB) block cipher mode of operation")
    (license (list license:expat license:asl2.0))))

(define-public rust-chacha20-0.9
  (package
    (name "rust-chacha20")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chacha20" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0678wipx6kghp71hpzhl2qvx80q7caz3vm8vsvd07b1fpms3yqf3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2))
       #:cargo-development-inputs (("rust-cipher" ,rust-cipher-0.4)
                                   ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/stream-ciphers")
    (synopsis "ChaCha20 stream cipher implemented in pure Rust")
    (description
     "The ChaCha20 stream cipher (RFC 8439) implemented in pure Rust using traits
from the RustCrypto @code{cipher} crate, with optional architecture-specific
hardware acceleration (AVX2, SSE2).  Additionally provides the ChaCha8, ChaCha12,
XChaCha20, XChaCha12 and XChaCha8 stream ciphers, and also optional
@code{rand_core-compatible} RNGs based on those ciphers.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-chacha20-0.8
  (package
    (inherit rust-chacha20-0.9)
    (name "rust-chacha20")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chacha20" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19l0nrizh0v9mj2dcd1y0mh7nn9sjnmvvg203nwy6vx6193fb02w"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "Cargo.toml"
             (("version = \">=1, <1.5\"") "version = \"^1\""))))))
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cipher" ,rust-cipher-0.3)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-cipher" ,rust-cipher-0.3)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-chacha20-0.7
  (package
    (inherit rust-chacha20-0.9)
    (name "rust-chacha20")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chacha20" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c8h4sp9zh13v8p9arydjcj92xc6j3mccrjc4mizrvq7fzx9717h"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "Cargo.toml"
             (("version = \">=1, <1.4\"") "version = \"^1\""))))))
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cipher" ,rust-cipher-0.3)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-cipher" ,rust-cipher-0.3)
                                   ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-chacha20poly1305-0.10
  (package
    (name "rust-chacha20poly1305")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chacha20poly1305" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dfwq9ag7x7lnd0znafpcn8h7k4nfr9gkzm0w7sc1lcj451pkk8h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aead" ,rust-aead-0.5)
                       ("rust-chacha20" ,rust-chacha20-0.9)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-poly1305" ,rust-poly1305-0.8)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-aead" ,rust-aead-0.5))))
    (home-page "https://github.com/RustCrypto/AEADs/tree/master/chacha20poly1305")
    (synopsis "Rust implementation of ChaCha20Poly1305 Authenticated Encryption")
    (description
     "Pure Rust implementation of the ChaCha20Poly1305 Authenticated Encryption
with Additional Data Cipher (RFC 8439) with optional architecture-specific
hardware acceleration.  Also contains implementations of the XChaCha20Poly1305
extended nonce variant of ChaCha20Poly1305, and the reduced-round
ChaCha8Poly1305 and ChaCha12Poly1305 lightweight variants.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-chacha20poly1305-0.9
  (package
    (inherit rust-chacha20poly1305-0.10)
    (name "rust-chacha20poly1305")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chacha20poly1305" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xfgn306nfch4a4wwddha8lz6qpnhng50iy4prxlagg6kfq4d151"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "Cargo.toml"
             (("version = \">=1, <1.5\"") "version = \"^1\""))))))
    (arguments
     `(#:cargo-inputs
       (("rust-aead" ,rust-aead-0.4)
        ("rust-chacha20" ,rust-chacha20-0.8)
        ("rust-cipher" ,rust-cipher-0.3)
        ("rust-poly1305" ,rust-poly1305-0.7)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-aead" ,rust-aead-0.4))))))

(define-public rust-chacha20poly1305-0.8
  (package
    (inherit rust-chacha20poly1305-0.10)
    (name "rust-chacha20poly1305")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chacha20poly1305" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18mb6k1w71dqv5q50an4rvp19l6yg8ssmvfrmknjfh2z0az7lm5n"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "Cargo.toml"
             (("version = \">=1, <1.4\"") "version = \"^1\""))))))
    (arguments
     `(#:cargo-inputs (("rust-aead" ,rust-aead-0.4)
                       ("rust-chacha20" ,rust-chacha20-0.7)
                       ("rust-cipher" ,rust-cipher-0.3)
                       ("rust-poly1305" ,rust-poly1305-0.7)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-aead" ,rust-aead-0.4))))))

(define-public rust-cipher-0.4
  (package
    (name "rust-cipher")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cipher" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-crypto-common" ,rust-crypto-common-0.1)
        ("rust-inout" ,rust-inout-0.1)
        ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://docs.rs/cipher/")
    (synopsis "Traits for describing block ciphers and stream ciphers")
    (description "This package provides traits which define the functionality
of block ciphers and stream ciphers.  See RustCrypto/block-ciphers and
RustCrypto/stream-ciphers for algorithm implementations which use these
traits.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cipher-0.3
  (package
    (inherit rust-cipher-0.4)
    (name "rust-cipher")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cipher" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1dyzsv0c84rgz98d5glnhsz4320wl24x3bq511vnyf0mxir21rby"))))
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-generic-array" ,rust-generic-array-0.14))))))

(define-public rust-cipher-0.2
  (package
    (inherit rust-cipher-0.3)
    (name "rust-cipher")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cipher" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "00b8imbmdg7zdrbaczlivmdfdy09xldg95wl4iijl15xgjcfgy0j"))))))

(define-public rust-cmac-0.7
  (package
    (name "rust-cmac")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cmac" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1an1vcala24grlyhvk71ikxk2kmgcbal9kgrzzpjcl9z7i74ahw5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.4)
        ("rust-dbl" ,rust-dbl-0.3)
        ("rust-digest" ,rust-digest-0.10))
       #:cargo-development-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-des" ,rust-des-0.8)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-kuznyechik" ,rust-kuznyechik-0.8)
        ("rust-magma" ,rust-magma-0.8))))
    (home-page "https://github.com/RustCrypto/MACs")
    (synopsis "Generic implementation of Cipher-based Message Authentication Code")
    (description "This package provides a pure Rust implementation of the
Cipher-based Message Authentication Code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crypto-bigint-0.5
  (package
    (name "rust-crypto-bigint")
    (version "0.5.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crypto-bigint" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xmbdff3g6ii5sbxjxc31xfkv9lrmyril4arh3dzckd4gjsjzj8d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-der" ,rust-der-0.7)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-rlp" ,rust-rlp-0.5)
        ("rust-serdect" ,rust-serdect-0.2)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-criterion" ,rust-criterion-0.5)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-num-integer" ,rust-num-integer-0.1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3)
        ("rust-rand-core" ,rust-rand-core-0.6))))
    (home-page "https://github.com/RustCrypto/crypto-bigint")
    (synopsis "Big integer library designed for use in cryptography")
    (description
     "This crate is a pure Rust implementation of a big integer library which
has been designed from the ground-up for use in cryptographic applications.
Provides constant-time, no_std-friendly implementations of modern formulas
using const generics.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-crypto-bigint-0.4
  (package
    (inherit rust-crypto-bigint-0.5)
    (name "rust-crypto-bigint")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-bigint" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vqprgj0aj1340w186zyspi58397ih78jsc0iydvhs6zrlilnazg"))))
    (arguments
     `(#:cargo-inputs (("rust-der" ,rust-der-0.6)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rlp" ,rust-rlp-0.5)
                       ("rust-serdect" ,rust-serdect-0.1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
        #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                    ("rust-hex-literal" ,rust-hex-literal-0.3)
                                    ("rust-num-bigint" ,rust-num-bigint-0.4)
                                    ("rust-num-traits" ,rust-num-traits-0.2)
                                    ("rust-proptest" ,rust-proptest-1)
                                    ("rust-rand-chacha" ,rust-rand-chacha-0.3)
                                    ("rust-rand-core" ,rust-rand-core-0.6))))))

(define-public rust-crypto-bigint-0.3
  (package
    (inherit rust-crypto-bigint-0.5)
    (name "rust-crypto-bigint")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-bigint" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08gx92sj93hk2smqy4nvk8lmpjjjqm7a9ps22q3pxqqxzbas3ih3"))))
    (arguments
     `(#:cargo-inputs (("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rlp" ,rust-rlp-0.5)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3)
                                   ("rust-num-bigint" ,rust-num-bigint-0.4)
                                   ("rust-num-traits" ,rust-num-traits-0.2)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-rand-chacha" ,rust-rand-chacha-0.3)
                                   ("rust-rand-core" ,rust-rand-core-0.6))))))

(define-public rust-crypto-bigint-0.2
  (package
    (inherit rust-crypto-bigint-0.5)
    (name "rust-crypto-bigint")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-bigint" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00qckh65nzb7s7vd60wylw6alxf9g37xh31lirb1qw0l8fxx6fzq"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-rlp" ,rust-rlp-0.5)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))))))

(define-public rust-crypto-box-0.8
  (package
    (name "rust-crypto-box")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto_box" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g4fhdsx1g1d0algpkfwsmclbcbj6k27anj4mj5d0zrhwlnw69px"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aead" ,rust-aead-0.5)
                       ("rust-blake2" ,rust-blake2-0.10)
                       ("rust-chacha20" ,rust-chacha20-0.9)
                       ("rust-chacha20poly1305" ,rust-chacha20poly1305-0.10)
                       ("rust-salsa20" ,rust-salsa20-0.10)
                       ("rust-serdect" ,rust-serdect-0.1)
                       ("rust-x25519-dalek" ,rust-x25519-dalek-1)
                       ("rust-xsalsa20poly1305" ,rust-xsalsa20poly1305-0.9)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rmp-serde" ,rust-rmp-serde-1))))
    (home-page "https://github.com/RustCrypto/nacl-compat")
    (synopsis "Pure Rust implementation of crypto_box")
    (description
     "This package provides a pure Rust implementation of @code{NaCl's}
crypto_box public-key authenticated encryption primitive, which combines the
X25519 Elliptic Curve Diffie-Hellman function and the XSalsa20Poly1305
authenticated encryption cipher.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-crypto-common-0.1
  (package
    (name "rust-crypto-common")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crypto-common" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-typenum" ,rust-typenum-1))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Common cryptographic traits")
    (description
     "This package contains a collection of traits which describe functionality
of cryptographic primitives.")
    ;; The user can choose either license.
    (license (list license:expat license:asl2.0))))

(define-public rust-crypto-mac-0.11
  (package
    (name "rust-crypto-mac")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-mac" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05672ncc54h66vph42s0a42ljl69bwnqjh0x4xgj2v1395psildi"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "Cargo.toml"
                   (("\"= ?([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                    (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-cipher" ,rust-cipher-0.3)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Trait for Message Authentication Code (MAC) algorithms")
    (description "This package provides trait for @dfn{Message Authentication
Code} (MAC) algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crypto-mac-0.10
  (package
    (inherit rust-crypto-mac-0.11)
    (name "rust-crypto-mac")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto-mac" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "06h84hcaksgjzzzc9g9dpmifwx221qzzif6fw8l807khxh471w5z"))
        (snippet
         #~(begin (use-modules (guix build utils))
                  (substitute* "Cargo.toml"
                    (("\"= ?([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                     (string-append "\"^" version)))))))
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-cipher" ,rust-cipher-0.2)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-subtle" ,rust-subtle-2))))))

(define-public rust-crypto-mac-0.8
  (package
    (inherit rust-crypto-mac-0.10)
    (name "rust-crypto-mac")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-mac" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1axfs4zmy74rn9666p92j7nmcv11zdp2d51yrppc2dv26cqa715m"))))
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-subtle" ,rust-subtle-2))))))

(define-public rust-crypto-mac-0.7
  (package
    (inherit rust-crypto-mac-0.10)
    (name "rust-crypto-mac")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-mac" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rbrq6qy9dl0pj4ym2zy33miaaa8vpzdss60p9bdb58xy46l0d24"))))
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-generic-array" ,rust-generic-array-0.12)
        ("rust-subtle" ,rust-subtle-1))))))

(define-public rust-crypto-mac-0.4.0-yanked
  (package
    (inherit rust-crypto-mac-0.11)
    (name "rust-crypto-mac")
    (version "0.4.0") ; This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-mac" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32 "160ixpghhz5kz16f38kzcyv6lx8wmi4cgbhlhq4nazf678iib43p"))))
    (arguments
     `(#:cargo-inputs
       (("rust-constant-time-eq" ,rust-constant-time-eq-0.1)
        ("rust-generic-array" ,rust-generic-array-0.8))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-crypto-mac-0.4
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-crypto-mac" rust-crypto-mac-0.4.0-yanked))

(define-public rust-crypto-secretbox-0.1
  (package
    (name "rust-crypto-secretbox")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto_secretbox" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qa1w5s8dbyb88269zrmvbnillqahz394pl07bsds6gpmn3wzmmr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aead" ,rust-aead-0.5)
                       ("rust-chacha20" ,rust-chacha20-0.9)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-poly1305" ,rust-poly1305-0.8)
                       ("rust-salsa20" ,rust-salsa20-0.10)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.4))))
    (home-page
     "https://github.com/RustCrypto/nacl-compat/tree/master/crypto_secretbox")
    (synopsis
     "Pure Rust implementation of the XSalsa20Poly1305")
    (description
     "Pure Rust implementation of the XSalsa20Poly1305 (a.k.a. @code{NaCl}
crypto_secretbox) authenticated encryption cipher as well as the libsodium
variant of X@code{ChaCha20Poly1305}.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-crypto-tests-0.5
  (package
    (name "rust-crypto-tests")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-tests" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08yrh40a9ll4k29ppizg2yjf96i6s3i9pbkhxp60y8arar93134v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-cipher-trait" ,rust-block-cipher-trait-0.4)
        ("rust-crypto-mac" ,rust-crypto-mac-0.4)
        ("rust-digest" ,rust-digest-0.6)
        ("rust-generic-array" ,rust-generic-array-0.8))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis "Test helpers for cryptographic algorithms")
    (description "This package provides test helpers for cryptographic
algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cryptovec-0.6
  (package
    (name "rust-cryptovec")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cryptovec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pqb2g1n3sx0d2cjiy06amcr2wlf9izwb4jj68nk5cmvlq9zmiyc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--"
                            "--skip=CryptoVec::from_slice (line 406)")
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://pijul.org/cryptovec")
    (synopsis "Vector which zeroes its memory on clears and reallocations")
    (description
     "This package provides a vector which zeroes its memory on clears and
reallocations.")
    (license license:asl2.0)))

(define-public rust-csrf-0.4
  (package
    (name "rust-csrf")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "csrf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q7ixhshj6a7x2vgsr4d4iqa5mgp4fwkr4lx2hgvnj9xcy1py9dh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aead" ,rust-aead-0.4)
                       ("rust-aes-gcm" ,rust-aes-gcm-0.9)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-chacha20poly1305" ,rust-chacha20poly1305-0.8)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-typemap" ,rust-typemap-0.3))))
    (home-page "https://github.com/heartsucker/rust-csrf")
    (synopsis "CSRF protection primitives")
    (description "This package provides CSRF protection primitives.")
    (license license:expat)))

(define-public rust-ctr-0.9
  (package
    (name "rust-ctr")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ctr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d88b73waamgpfjdml78icxz45d95q7vi2aqa604b0visqdfws83"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-cipher" ,rust-cipher-0.4)
        ("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-kuznyechik" ,rust-kuznyechik-0.8)
        ("rust-magma" ,rust-magma-0.8))))
    (home-page "https://docs.rs/ctr/")
    (synopsis "CTR block mode of operation")
    (description "This package provides a generic implementations of CTR mode
for block ciphers. Mode functionality is accessed using traits from
re-exported cipher crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ctr-0.8
  (package
    (inherit rust-ctr-0.9)
    (name "rust-ctr")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ctr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sk1aykwhkny92cnvl6s75dx3fyvfzw5xkd6xz3y7w5anhgvk6q4"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cipher" ,rust-cipher-0.3))))))

(define-public rust-ctr-0.6
  (package
    (inherit rust-ctr-0.8)
    (name "rust-ctr")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ctr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zvyf13675hrlc37myj97k5ng7m1mj3d9p4ic4yvyhvl9zak0jpv"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cipher" ,rust-cipher-0.2))))))

(define-public rust-curve25519-dalek-4
  (package
    (name "rust-curve25519-dalek")
    (version "4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "curve25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gmjb9dsknrr8lypmhkyjd67p1arb8mbfamlwxm7vph38my8pywp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-curve25519-dalek-derive" ,rust-curve25519-dalek-derive-0.1)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-ff" ,rust-ff-0.13)
        ("rust-fiat-crypto" ,rust-fiat-crypto-0.2)
        ("rust-group" ,rust-group-0.13)
        ("rust-platforms" ,rust-platforms-3)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-rustc-version" ,rust-rustc-version-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-core" ,rust-rand-core-0.6)
                                   ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://doc.dalek.rs/curve25519_dalek")
    (synopsis "Group operations on ristretto255 and Curve25519")
    (description
     "This package provides a pure-Rust implementation of group operations
on ristretto255 and Curve25519.")
    (license license:bsd-3)))

(define-public rust-curve25519-dalek-3
  (package
    (inherit rust-curve25519-dalek-4)
    (name "rust-curve25519-dalek")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "curve25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h0vcl8p4syvci9zxkn3h80h06xv1fyqgcrfwrv0lnbzjr9d1ych"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (((string-append ">=([[:digit:]]+(\\.[[:digit:]]+)*),"
                                   " <([[:digit:]]+(\\.[[:digit:]]+)*)")
                    _ version _)
                   (string-append ">=" version)))))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-fiat-crypto" ,rust-fiat-crypto-0.1)
        ("rust-packed-simd-2" ,rust-packed-simd-2-0.3)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-sha2" ,rust-sha2-0.9))))))

(define-public rust-curve25519-dalek-derive-0.1
  (package
    (name "rust-curve25519-dalek-derive")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "curve25519-dalek-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cry71xxrr0mcy5my3fb502cwfxy6822k4pm19cwrilrg7hq4s7l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://doc.dalek.rs/curve25519_dalek")
    (synopsis "Derives of curve25519-dalek")
    (description "This package provides curve25519-dalek Derives.")
    (license (list license:expat license:asl2.0))))

(define-public rust-curve25519-dalek-ng-4
  (package
    (name "rust-curve25519-dalek-ng")
    (version "4.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "curve25519-dalek-ng" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j6y6dsqdfp26ifyygibsrm1a8f9f7870i4053xlczil95r9nd8w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-packed-simd-2" ,rust-packed-simd-2-0.3)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-serde" ,rust-serde-1)
        ("rust-subtle-ng" ,rust-subtle-ng-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-sha2" ,rust-sha2-0.9))))
    (home-page "https://github.com/zkcrypto/curve25519-dalek-ng")
    (synopsis "Implementation of group operations on ristretto255 and Curve25519")
    (description
     "This package provides a pure-Rust implementation of group operations on
ristretto255 and Curve25519.")
    (license license:bsd-3)))

(define-public rust-des-0.8
  (package
    (name "rust-des")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "des" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07kshslxanmg0g6007scvglfhg6mli2a8qzhx4kxx4z9ik781pgz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs (("rust-cipher" ,rust-cipher-0.4))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "DES and Triple DES block ciphers implementation")
    (description "This package provides DES and Triple DES (3DES, TDES) block
ciphers implementations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-des-0.7
  (package
    (inherit rust-des-0.8)
    (name "rust-des")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "des" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0pbsfkkwfqnd4nsv3ik4z09h248f57y7bj2j1l134i2mzd4xshdc"))))
    (arguments
     `(#:skip-build?  #t
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-cipher" ,rust-cipher-0.3)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3))))))

(define-public rust-digest-0.10
  (package
    (name "rust-digest")
    (version "0.10.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "digest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-block-buffer" ,rust-block-buffer-0.10)
        ("rust-const-oid" ,rust-const-oid-0.9)
        ("rust-crypto-common" ,rust-crypto-common-0.1)
        ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for cryptographic hash functions")
    (description
     "Traits for cryptographic hash functions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-digest-0.9
  (package
    (inherit rust-digest-0.10)
    (name "rust-digest")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "digest" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rmhvk33rgvd6ll71z8sng91a52rw14p0drjn1da0mqa138n1pfk"))))
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-generic-array" ,rust-generic-array-0.14))))))

(define-public rust-digest-0.8
  (package
    (inherit rust-digest-0.9)
    (name "rust-digest")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "digest" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1madjl27f3kj5ql7kwgvb9c8b7yb7bv7yfgx7rqzj4i3fp4cil7k"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-generic-array" ,rust-generic-array-0.12))))))

(define-public rust-digest-0.6
  (package
    (inherit rust-digest-0.10)
    (name "rust-digest")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "digest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lgv5rs7i903zvmkdbil0bcjx9vxvi9rx4z9qavapz199q31rbpc"))))
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array-0.8))))))

(define-public rust-dsa-0.6
  (package
    (name "rust-dsa")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12bn5d0nnni4mlla24m3bwi4mhy2nfmyak2qjl0pdbc4j1525g28"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-digest" ,rust-digest-0.10)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pkcs8" ,rust-pkcs8-0.10)
                       ("rust-rfc6979" ,rust-rfc6979-0.4)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-pkcs8" ,rust-pkcs8-0.10)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-chacha" ,rust-rand-chacha-0.3)
                                   ("rust-sha1" ,rust-sha1-0.10))))
    (home-page "https://github.com/RustCrypto/signatures/tree/master/dsa")
    (synopsis
     "Rust implementation of the Digital Signature Algorithm (DSA)")
    (description
     "This package provides a pure Rust implementation of the @acronym{Digital
Signature Algorithm, DSA} as specified in FIPS 186-4 (Digital Signature
Standard), providing RFC6979 deterministic signatures as well as support for
added entropy.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-eax-0.5
  (package
    (name "rust-eax")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "eax" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0a5cpzk577f1lw3wkk20iqvavnbdr5yzjrcglvbvk0ivj2yzlm4r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aead" ,rust-aead-0.5)
        ("rust-cipher" ,rust-cipher-0.4)
        ("rust-cmac" ,rust-cmac-0.7)
        ("rust-ctr" ,rust-ctr-0.9)
        ("rust-subtle" ,rust-subtle-2))
       #:cargo-development-inputs
       (("rust-aead" ,rust-aead-0.5)
        ("rust-aes" ,rust-aes-0.8))))
    (home-page "https://github.com/RustCrypto/AEADs")
    (synopsis "Pure Rust implementation of the EAX Authenticated Encryption with
Associated Data (AEAD)")
    (description
     "Pure Rust implementation of the EAX Authenticated Encryption with Associated
Data (AEAD) Cipher with optional architecture-specific hardware acceleration
This scheme is only based on a block cipher.  It uses counter mode (CTR) for
encryption and CBC mode for generating a OMAC/CMAC/CBCMAC (all names for the
same thing).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ecdsa-0.16
  (package
    (name "rust-ecdsa")
    (version "0.16.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ecdsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jhb0bcbkaz4001sdmfyv8ajrv8a1cg7z7aa5myrd4jjbhmz69zf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-der" ,rust-der-0.7)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
        ("rust-rfc6979" ,rust-rfc6979-0.4)
        ("rust-serdect" ,rust-serdect-0.2)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-signature" ,rust-signature-2)
        ("rust-spki" ,rust-spki-0.7))
       #:cargo-development-inputs
       (("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://github.com/RustCrypto/signatures/tree/master/ecdsa")
    (synopsis "Pure Rust implementation of the ECDSA algorithm")
    (description
     "This package provides a pure Rust implementation of the @dfn{Elliptic
Curve Digital Signature Algorithm} (ECDSA) as specified in FIPS 186-4 (Digital
Signature Standard), providing RFC6979 deterministic signatures as well as
support for added entropy.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ecies-ed25519-0.5
  (package
    (name "rust-ecies-ed25519")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ecies-ed25519" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nrmam79mn2y6b235rpq6lhlsfl63275j2yxps86424gh99j720a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-aes-gcm" ,rust-aes-gcm-0.8)
        ("rust-curve25519-dalek" ,rust-curve25519-dalek-3)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-hkdf" ,rust-hkdf-0.10)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-serde" ,rust-serde-1)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/phayes/ecies-ed25519")
    (synopsis
     "Integrated encryption scheme on Twisted Edwards Curve25519")
    (description
     "ECIES on Twisted Edwards Curve25519 using AES-GCM and HKDF-SHA256.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ecies-ed25519-ng-0.5
  (package
    (name "rust-ecies-ed25519-ng")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Revertron/ecies-ed25519-ng")
             ;; Version bump without a git tag.
             (commit "554ca29a1bbd55f0c7e2f75cb3c7e0e3030afc15")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04s0ycvnz1wbccf46a63w6zxiqm9yszw71q6fk1ssdc64qj7k5mh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aes-gcm" ,rust-aes-gcm-0.10)
        ("rust-curve25519-dalek" ,rust-curve25519-dalek-4)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-hkdf" ,rust-hkdf-0.12)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-ring" ,rust-ring-0.17)
        ("rust-serde" ,rust-serde-1)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-serde-cbor" ,rust-serde-cbor-0.11)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/Revertron/ecies-ed25519-ng")
    (synopsis "Integrated encryption scheme on Twisted Edwards Curve25519")
    (description
     "ECIES on Twisted Edwards Curve25519 using AES-GCM and HKDF-SHA256.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ed25519-2
  (package
    (name "rust-ed25519")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ed25519" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lydzdf26zbn82g7xfczcac9d7mzm3qgx934ijjrd5hjpjx32m8i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-pkcs8" ,rust-pkcs8-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-ed25519-dalek" ,rust-ed25519-dalek-2)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-ring-compat" ,rust-ring-compat-0.8))))
    (home-page "https://github.com/RustCrypto/signatures/tree/master/ed25519")
    (synopsis "Edwards Digital Signature Algorithm over Curve25519")
    (description
      "EdDSA over Curve25519 is specified in RFC 8032.  This package
contains an ed25519::Signature type which other packages can use in
conjunction with the signature::Signer and signature::Verifier traits.
It doesn't contain an implementation of Ed25519.

These traits allow packages which produce and consume Ed25519 signatures
to be written abstractly in such a way that different signer/verifier
providers can be plugged in, enabling support for using different Ed25519
implementations, including HSMs or Cloud KMS services.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ed25519-1
  (package
    (inherit rust-ed25519-2)
    (name "rust-ed25519")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ed25519" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rzydm5wd8szkddx3g55w4vm86y1ika8qp8qwckada5vf1fg7kwi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pkcs8" ,rust-pkcs8-0.9)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-signature" ,rust-signature-1)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-ed25519-dalek" ,rust-ed25519-dalek-1)
        ("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-rand-core" ,rust-rand-core-0.5))))))

(define-public rust-ed25519-compact-2
  (package
    (name "rust-ed25519-compact")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ed25519-compact" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1431kxw67xkk5y5kamfdjxnqbzqy5y4p032syi3wva5y8h7ldcz9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ct-codecs" ,rust-ct-codecs-1)
        ("rust-ed25519" ,rust-ed25519-2)
        ("rust-getrandom" ,rust-getrandom-0.2))
       #:cargo-development-inputs
       (("rust-ct-codecs" ,rust-ct-codecs-1)
        ("rust-getrandom" ,rust-getrandom-0.2))))
    (home-page "https://github.com/jedisct1/rust-ed25519-compact")
    (synopsis "Wasm-friendly Ed25519 implementation")
    (description
     "This package provides a small, self-contained, wasm-friendly Ed25519
implementation.")
    (license license:expat)))

(define-public rust-ed25519-dalek-2
  (package
    (name "rust-ed25519-dalek")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ed25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w88cafwglg9hjizldbmlza0ns3hls81zk1bcih3m5m3h67algaa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            ;; Some tests aren't shipped in the crate.
                            "--skip=vectors::against_reference_implementation"
                            "--skip=check_validation_criteria"
                            "--skip=find_validation_criteria")
       #:cargo-inputs (("rust-curve25519-dalek" ,rust-curve25519-dalek-4)
                       ("rust-ed25519" ,rust-ed25519-2)
                       ("rust-merlin" ,rust-merlin-3)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-blake2" ,rust-blake2-0.10)
        ("rust-criterion" ,rust-criterion-0.5)
        ("rust-curve25519-dalek" ,rust-curve25519-dalek-4)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha3" ,rust-sha3-0.10)
        ("rust-toml" ,rust-toml-0.7)
        ("rust-x25519-dalek" ,rust-x25519-dalek-2))))
    (home-page "https:///doc.dalek.rs/ed25519_dalek")
    (synopsis "Ed25519 EdDSA key generations, signing, and verification")
    (description
     "This package provides fast and efficient ed25519 @code{EdDSA} key
generations, signing, and verification in pure Rust.")
    (license license:bsd-3)))

(define-public rust-ed25519-dalek-1
  (package
    (inherit rust-ed25519-dalek-2)
    (name "rust-ed25519-dalek")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ed25519-dalek" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "17bsriciv93nkm39z22w7mr0h2a3hnbmgf378v4c895gvkkblqn7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t
        #:cargo-inputs
        (("rust-curve25519-dalek" ,rust-curve25519-dalek-3)
         ("rust-ed25519" ,rust-ed25519-1)
         ("rust-merlin" ,rust-merlin-2)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-rand-core" ,rust-rand-core-0.5)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-bytes" ,rust-serde-bytes-0.11)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-zeroize" ,rust-zeroize-1))))))

(define-public rust-ed25519-zebra-2
  (package
    (name "rust-ed25519-zebra")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ed25519-zebra" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0284mnhc2514b9hzyhgcf8vfggwdqwyx8vsawckv9m3dmxv8n4ha"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-curve25519-dalek" ,rust-curve25519-dalek-3)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-rand-core" ,rust-rand-core-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-color-eyre" ,rust-color-eyre-0.5)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-ed25519-zebra" ,rust-ed25519-zebra-1)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-rand" ,rust-rand-0.7))))
    (home-page "https://github.com/ZcashFoundation/ed25519-zebra")
    (synopsis "Zcash-flavored Ed25519 for use in Zebra")
    (description
     "This package provides Zcash-flavored Ed25519 for use in Zebra.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ed25519-zebra-1
  (package
    (inherit rust-ed25519-zebra-2)
    (name "rust-ed25519-zebra")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ed25519-zebra" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zyi37p8p1qqbbkd27w52zvx5cj9b56rvc17jiw9d71j3gziynn8"))))
    (arguments
     `(#:cargo-inputs (("rust-curve25519-dalek" ,rust-curve25519-dalek-3)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-rand-core" ,rust-rand-core-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-rand" ,rust-rand-0.7))))))

(define-public rust-elliptic-curve-0.13
  (package
    (name "rust-elliptic-curve")
    (version "0.13.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "elliptic-curve" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ixx4brgnzi61z29r3g1606nh2za88hzyz8c5r3p6ydzhqq09rmm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base16ct" ,rust-base16ct-0.2)
        ("rust-base64ct" ,rust-base64ct-1)
        ("rust-crypto-bigint" ,rust-crypto-bigint-0.5)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-ff" ,rust-ff-0.13)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-group" ,rust-group-0.13)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-hkdf" ,rust-hkdf-0.12)
        ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.7)
        ("rust-pkcs8" ,rust-pkcs8-0.10)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-sec1" ,rust-sec1-0.7)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serdect" ,rust-serdect-0.2)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-tap" ,rust-tap-1)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-sha3" ,rust-sha3-0.10))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/elliptic-curve")
    (synopsis "General purpose Elliptic Curve Cryptography (ECC) support")
    (description
     "This package provides general purpose @dfn{Elliptic Curve Cryptography}
(ECC) support, including types and traits for representing various elliptic
curve forms, scalars, points, and public/secret keys composed thereof.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-fiat-crypto-0.2
  (package
    (name "rust-fiat-crypto")
    (version "0.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fiat-crypto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07c1vknddv3ak7w89n85ik0g34nzzpms6yb845vrjnv9m4csbpi8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mit-plv/fiat-crypto")
    (synopsis "Fiat-crypto generated Rust")
    (description "This crate provides the extracted Rust code from the Coq
@code{fiat-crypto} libraries.")
    (license (list license:expat license:asl2.0 license:bsd-1))))

(define-public rust-fiat-crypto-0.1
  (package
    (inherit rust-fiat-crypto-0.2)
    (name "rust-fiat-crypto")
    (version "0.1.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fiat-crypto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0xvbcg6wh42q3n7294mzq5xxw8fpqsgc0d69dvm5srh1f6cgc9g8"))))))

(define-public rust-ghash-0.5
  (package
    (name "rust-ghash")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ghash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wbg4vdgzwhkpkclz1g6bs4r5x984w5gnlsj4q5wnafb5hva9n7h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-opaque-debug" ,rust-opaque-debug-0.3)
        ("rust-polyval" ,rust-polyval-0.6)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis "Universal hash over GF(2^128)")
    (description "This package provides a universal hash over GF(2^128) useful
for constructing a Message Authentication Code (MAC), as in the AES-GCM
authenticated encryption cipher.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ghash-0.4
  (package
    (inherit rust-ghash-0.5)
    (name "rust-ghash")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ghash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "169wvrc2k9lw776x3pmqp76kc0w5717wz01bfg9rz0ypaqbcr0qm"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (((string-append ">=([[:digit:]]+(\\.[[:digit:]]+)*),"
                                   " <([[:digit:]]+(\\.[[:digit:]]+)*)")
                    _ version _)
                   (string-append ">=" version)))))))
    (arguments
     `(#:cargo-inputs (("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-polyval" ,rust-polyval-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-ghash-0.3
  (package
    (inherit rust-ghash-0.5)
    (name "rust-ghash")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ghash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0xd362xh17hadc2194dd6kjjq0ak1j4x7kkmfmpq9hw2s564wc4p"))))
    (arguments
     `(#:cargo-inputs
       (("rust-opaque-debug" ,rust-opaque-debug-0.3)
        ("rust-polyval" ,rust-polyval-0.4)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-hkdf-0.12
  (package
    (name "rust-hkdf")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hkdf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xxxzcarz151p1b858yn5skmhyrvn8fs4ivx5km3i1kjmnr8wpvv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-hmac" ,rust-hmac-0.12))
       #:cargo-development-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-hex-literal" ,rust-hex-literal-0.2)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://github.com/RustCrypto/KDFs/")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description "This package provides a HMAC-based Extract-and-Expand Key
Derivation Function (HKDF).")
    (license (list license:expat license:asl2.0))))

(define-public rust-hkdf-0.11
  (package
    (inherit rust-hkdf-0.12)
    (name "rust-hkdf")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hkdf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sw8bz79xqq3bc5dh6nzv084g7va13j3lrqf91c10a2wimbnsw01"))))
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hmac" ,rust-hmac-0.11))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-blobby" ,rust-blobby-0.3)
        ("rust-crypto-tests" ,rust-crypto-tests-0.5)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sha2" ,rust-sha2-0.9))))))

(define-public rust-hkdf-0.10
  (package
    (inherit rust-hkdf-0.11)
    (name "rust-hkdf")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hkdf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0kwn3scjvv2x8zc6nz3wrnzxp9shpsdxnjqiyv2r65r3kiijzasi"))))
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hmac" ,rust-hmac-0.10))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-crypto-tests" ,rust-crypto-tests-0.5)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sha2" ,rust-sha2-0.9))))))

(define-public rust-hmac-0.12
  (package
    (name "rust-hmac")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hmac" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10))))
    (home-page "https://github.com/RustCrypto/MACs")
    (synopsis "Generic implementation of Hash-based Message Authentication Code")
    (description
     "This package provides a generic implementation of @acronym{HMAC,
Hash-based Message Authentication Code}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hmac-0.11
  (package
    (inherit rust-hmac-0.12)
    (name "rust-hmac")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16z61aibdg4di40sqi4ks2s4rz6r29w4sx4gvblfph3yxch26aia"))))
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.11)
        ("rust-digest" ,rust-digest-0.9))
       #:cargo-development-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.11)
        ("rust-md-5" ,rust-md-5-0.9)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-streebog" ,rust-streebog-0.9))))))

(define-public rust-hmac-0.10
  (package
    (inherit rust-hmac-0.11)
    (name "rust-hmac")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "058yxq54x7xn0gk2vy9bl51r32c9z7qlcl2b80bjh3lk3rmiqi61"))))
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.10)
        ("rust-digest" ,rust-digest-0.9))
       #:cargo-development-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.10)
        ("rust-md-5" ,rust-md-5-0.9)
        ("rust-sha2" ,rust-sha2-0.9))))))

(define-public rust-hmac-sha1-0.1
  (package
    (name "rust-hmac-sha1")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hmac-sha1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08k7aylc0v8x3abmxn3h73dkad3anfq2i94xk2mjrf4linnkycz1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-sha1" ,rust-sha1-0.2))))
    (home-page "https://github.com/pantsman0/rust-hmac-sha1")
    (synopsis "Minimal implementation of HMAC-SHA1 in Rust")
    (description
     "This package is a pure Rust implementation of the @acronym{HMAC,
Hash-based Message Authentication Code algorithm} for SHA1.")
    (license license:bsd-3)))

(define-public rust-k256-0.13
  (package
    (name "rust-k256")
    (version "0.13.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "k256" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06s1lxjp49zgmbxnfdy2kajyklbkl4s3jvdvy0amg552padr3qzn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-ecdsa" ,rust-ecdsa-0.16)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
                       ("rust-hex-literal" ,rust-hex-literal-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serdect" ,rust-serdect-0.2)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-signature" ,rust-signature-2))
       #:cargo-development-inputs (("rust-blobby" ,rust-blobby-0.3)
                                   ("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-ecdsa" ,rust-ecdsa-0.16)
                                   ("rust-hex-literal" ,rust-hex-literal-0.4)
                                   ("rust-num-bigint" ,rust-num-bigint-0.4)
                                   ("rust-num-traits" ,rust-num-traits-0.2)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-rand-core" ,rust-rand-core-0.6)
                                   ("rust-sha3" ,rust-sha3-0.10))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/k256")
    (synopsis
     "Library supporting general-purpose elliptic curve group operations")
    (description
     "This package provides a secp256k1 elliptic curve library written in pure
Rust with support for ECDSA signing/verification/public-key recovery,
@dfn{Taproot Schnorr signatures} (BIP340), @dfn{Elliptic Curve Diffie-Hellman}
(ECDH), and general-purpose secp256k1 elliptic curve group operations which can
be used to implement arbitrary protocols.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-kuznyechik-0.8
  (package
    (name "rust-kuznyechik")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "kuznyechik" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0by12awlby61ihp097gz193h8any0dkq5z46svg6130r4jjrjy6a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs
       (("rust-cipher" ,rust-cipher-0.4)
        ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Kuznyechik (GOST R 34.12-2015) block cipher")
    (description "Kuznyechik (GOST R 34.12-2015) block cipher")
    (license (list license:expat license:asl2.0))))

(define-public rust-libsodium-sys-0.2
  (package
    (name "rust-libsodium-sys")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsodium-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zcjka23grayr8kjrgbada6vwagp0kkni9m45v0gpbanrn3r6xvb"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (for-each delete-file-recursively
                           (list "libsodium"
                                 "mingw"
                                 "msvc"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (native-inputs (list pkg-config))
    (inputs (list libsodium))
    (home-page "https://github.com/sodiumoxide/sodiumoxide.git")
    (synopsis "FFI binding to libsodium")
    (description "This package provides FFI bindings to libsodium.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mas-jose-0.7
  (package
    (name "rust-mas-jose")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mas-jose" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x1cikddf2z3994374ql0qs02l9mxrlb74cy4pbq3yrlzcfjb6mk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-ecdsa" ,rust-ecdsa-0.16)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-k256" ,rust-k256-0.13)
                       ("rust-mas-iana" ,rust-mas-iana-0.7)
                       ("rust-p256" ,rust-p256-0.13)
                       ("rust-p384" ,rust-p384-0.13)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rsa" ,rust-rsa-0.9)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-sec1" ,rust-sec1-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-with" ,rust-serde-with-3)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-insta" ,rust-insta-1)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3))))
    (home-page "https://matrix-org.github.io/matrix-authentication-service/")
    (synopsis "JSON Object Signing and Encryption (JWT & co.) utilities")
    (description "This package provides JSON Object Signing and Encryption
(JWT & co.) utilities.")
    (license license:asl2.0)))

(define-public rust-md-5-0.10
  (package
    (name "rust-md-5")
    (version "0.10.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "md-5" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-md5-asm" ,rust-md5-asm-0.5))
       #:cargo-development-inputs (("rust-digest" ,rust-digest-0.10)
                                   ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "MD5 hash function")
    (description
     "This library provides a MD5 hash function for Rust.")
    ;; The user can choose either license.
    (license (list license:expat license:asl2.0))))

(define-public rust-md-5-0.9
  (package
    (inherit rust-md-5-0.10)
    (name "rust-md-5")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md-5" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "059ajjacz1q3cms7vl6cvhdqs4qdw2nnwj9dq99ryzv0p6djfnkv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.9)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-md5-asm" ,rust-md5-asm-0.4)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-md5-0.7
  (package
    (name "rust-md5")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "md5" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wcps37hrhz59fkhf8di1ppdnqld6l1w5sdy7jp7p51z0i4c8329"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/stainless-steel/md5")
    (synopsis "MD5 hash function in Rust")
    (description "The package provides the MD5 hash function.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-md5-0.6
  (package
    (inherit rust-md5-0.7)
    (name "rust-md5")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md5" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "17b2xm4h4cvxsdjsf3kdrzqv2za60kak961xzi5kmw6g6djcssvy"))))))

(define-public rust-md5-0.3
  (package
    (inherit rust-md5-0.6)
    (name "rust-md5")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "md5" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0j2s8aqdkhwhy7awga2bmv5n8qq8bgy8672iha9f3y871dm6vibr"))))))

(define-public rust-md5-asm-0.5
  (package
    (name "rust-md5-asm")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "md5-asm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pz217kwlvrw4bj4hil5acyp3l7g37vwf25psdc210bxzkkqx6yi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/RustCrypto/asm-hashes")
    (synopsis "Assembly implementation of MD5 compression function")
    (description
     "This package contains an assembly implementation of the MD5
compression function.")
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:expat)))

(define-public rust-md5-asm-0.4
  (package
    (inherit rust-md5-asm-0.5)
    (name "rust-md5-asm")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md5-asm" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0gpk5647js1k084jc7pg2gji0cvl6hjkkbfia6lnpk8y4shyairv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1))))))

(define-public rust-nettle-7
  (package
    (name "rust-nettle")
    (version "7.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nettle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dk9rlpz4c0kf2c7298vllpnwr3lh10kkgdbslglmlz5ji5gzrj4"))))
    (build-system cargo-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list clang gmp nettle))
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nettle-sys" ,rust-nettle-sys-2)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-typenum" ,rust-typenum-1))))
    (home-page "https://gitlab.com/sequoia-pgp/nettle-rs")
  (synopsis "Rust bindings for the Nettle cryptographic library")
  (description "This package provides Rust bindings for the Nettle
cryptographic library.")
  (license (list license:lgpl3 license:gpl2 license:gpl3))))

(define-public rust-nettle-sys-2
  (package
    (name "rust-nettle-sys")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nettle-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v36is0kygwc4rcdqnszgdwm14z2j8p23wbblbiq16m120x0b5dl"))))
    (build-system cargo-build-system)
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list nettle))
    (arguments
     `(#:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.68)
        ("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://gitlab.com/sequoia-pgp/nettle-sys")
    (synopsis "Low-level Rust bindings for the Nettle cryptographic library")
    (description "This package provides low-level Rust bindings for the Nettle
cryptographic library.")
    (license ;; licensed under either of these, at your option
     (list license:lgpl3 license:gpl2 license:gpl3))))

(define-public rust-oo7-0.2
  (package
    (name "rust-oo7")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oo7" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13cpaq7f51gqcspd4097vjr7r2cjpxpn6c02x67dsdizk0xaiv5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--"
         "--skip=dbus::collection::tests::create_encrypted_item"
         "--skip=dbus::collection::tests::create_plain_item"
         "--skip=dbus::service::tests::create_collection")
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-async-fs" ,rust-async-fs-2)
                       ("rust-async-io" ,rust-async-io-2)
                       ("rust-async-lock" ,rust-async-lock-3)
                       ("rust-blocking" ,rust-blocking-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cbc" ,rust-cbc-0.1)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-num" ,rust-num-0.4)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.12)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-zbus" ,rust-zbus-3)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/bilelmoussaoui/oo7")
    (synopsis "Secret Service provider")
    (description
     "This package provides oo7, a Rust Secret Service provider.")
    (license license:expat)))

(define-public rust-orion-0.17
  (package
    (name "rust-orion")
    (version "0.17.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "orion" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lzs8dlpdbq19hi3b4358bnrypvsxvfz4xp5b492gkb0rwam9awp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ct-codecs" ,rust-ct-codecs-1)
        ("rust-fiat-crypto" ,rust-fiat-crypto-0.2)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/orion-rs/orion")
    (synopsis "Pure-Rust crypto")
    (description
     "Orion is a cryptography library written in pure Rust.  It aims to provide
easy and usable crypto while trying to minimize the use of unsafe code.")
    (license license:expat)))

(define-public rust-p256-0.13
  (package
    (name "rust-p256")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "p256" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jyd3c3k239ybs59ixpnl7dqkmm072fr1js8kh7ldx58bzc3m1n9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ecdsa" ,rust-ecdsa-0.16)
        ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-primeorder" ,rust-primeorder-0.13)
        ("rust-serdect" ,rust-serdect-0.2)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-criterion" ,rust-criterion-0.4)
        ("rust-ecdsa" ,rust-ecdsa-0.16)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-primeorder" ,rust-primeorder-0.13)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-rand-core" ,rust-rand-core-0.6))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/p256")
    (synopsis "Pure Rust implementation of the NIST P-256")
    (description
     "This package provides a pure Rust implementation of the NIST P-256 (a.k.a.
secp256r1, prime256v1) elliptic curve as defined in SP 800-186, with support for
ECDH, ECDSA signing/verification, and general purpose curve arithmetic.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-p384-0.13
  (package
    (name "rust-p384")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "p384" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02cjlxdvxwvhmnckqnydqpvrwhf5raj67q300d66m7y6pi8nyy3h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ecdsa" ,rust-ecdsa-0.16)
        ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
        ("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-primeorder" ,rust-primeorder-0.13)
        ("rust-serdect" ,rust-serdect-0.2)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-blobby" ,rust-blobby-0.3)
        ("rust-criterion" ,rust-criterion-0.4)
        ("rust-ecdsa" ,rust-ecdsa-0.16)
        ("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-rand-core" ,rust-rand-core-0.6))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/p384")
    (synopsis "Pure Rust implementation of the NIST P-384 elliptic curve")
    (description
     "This package provides a pure Rust implementation of the NIST P-384 (a.k.a.
secp384r1) elliptic curve as defined in SP 800-186 with support for ECDH, ECDSA
signing/verification, and general purpose curve arithmetic support.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-p521-0.13
  (package
    (name "rust-p521")
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "p521" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cl5y2aypa1vxg181a0na3abndz1981pfdp2zkyml88z3wbf5j8g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base16ct" ,rust-base16ct-0.2)
                       ("rust-ecdsa" ,rust-ecdsa-0.16)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
                       ("rust-hex-literal" ,rust-hex-literal-0.4)
                       ("rust-primeorder" ,rust-primeorder-0.13)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serdect" ,rust-serdect-0.2)
                       ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs (("rust-blobby" ,rust-blobby-0.3)
                                   ("rust-ecdsa" ,rust-ecdsa-0.16)
                                   ("rust-hex-literal" ,rust-hex-literal-0.4)
                                   ("rust-primeorder" ,rust-primeorder-0.13)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-rand-core" ,rust-rand-core-0.6))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/p521")
    (synopsis
     "Pure Rust implementation of the NIST P-521 (a.k.a. secp521r1) elliptic curve")
    (description
     "This package provides Pure Rust implementation of the NIST P-521 (a.k.a.
secp521r1) elliptic curve as defined in SP 800-186.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pbkdf2-0.12
  (package
    (name "rust-pbkdf2")
    (version "0.12.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pbkdf2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wms79jh4flpy1zi8xdp4h8ccxv4d85adc6zjagknvppc5vnmvgq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-password-hash" ,rust-password-hash-0.5)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-streebog" ,rust-streebog-0.10))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/pbkdf2")
    (synopsis "Generic implementation of PBKDF2")
    (description "This package contains a collection of password hashing
algorithms, otherwise known as password-based key derivation functions, written
in pure Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pbkdf2-0.11
  (package
    (inherit rust-pbkdf2-0.12)
    (name "rust-pbkdf2")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pbkdf2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05q9wqjvfrs4dvw03yn3bvcs4zghz0a7ycfa53pz2k2fqhp6k843"))))
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-password-hash" ,rust-password-hash-0.4)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-sha-1" ,rust-sha-1-0.10)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-sha-1" ,rust-sha-1-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-streebog" ,rust-streebog-0.10))))
    (home-page "https://github.com/RustCrypto/password-hashing")))

(define-public rust-pbkdf2-0.10
  (package
    (inherit rust-pbkdf2-0.11)
    (name "rust-pbkdf2")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pbkdf2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1myz799hi58qxdxc9cch3q2sl0vs68vmgrd3j7dmc6aqbgrpj5r7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-password-hash" ,rust-password-hash-0.3)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-sha-1" ,rust-sha-1-0.10)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-sha-1" ,rust-sha-1-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-streebog" ,rust-streebog-0.10))))))

(define-public rust-pbkdf2-0.9
  (package
    (inherit rust-pbkdf2-0.10)
    (name "rust-pbkdf2")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pbkdf2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fa7j0gdgghk64qlhzdv32yg52p0cfaz5ifhk7i4pfm1wsy98n7h"))))
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.11)
        ("rust-hmac" ,rust-hmac-0.11)
        ("rust-password-hash" ,rust-password-hash-0.3)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sha2" ,rust-sha2-0.9))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-hmac" ,rust-hmac-0.11)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-streebog" ,rust-streebog-0.9))))))

(define-public rust-pbkdf2-0.8
  (package
    (inherit rust-pbkdf2-0.10)
    (name "rust-pbkdf2")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pbkdf2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ykgicvyjm41701mzqhrfmiz5sm5y0zwfg6csaapaqaf49a54pyr"))))
    (arguments
     (list #:cargo-inputs
           `(("rust-crypto-mac" ,rust-crypto-mac-0.11)
             ("rust-base64ct" ,rust-base64ct-1)
             ("rust-hmac" ,rust-hmac-0.11)
             ("rust-password-hash" ,rust-password-hash-0.2)
             ("rust-rayon" ,rust-rayon-1)
             ("rust-sha-1" ,rust-sha-1-0.9)
             ("rust-sha2" ,rust-sha2-0.9))
           #:cargo-development-inputs
           `(("rust-hex-literal" ,rust-hex-literal-0.3)
             ("rust-hmac" ,rust-hmac-0.11)
             ("rust-rand-core" ,rust-rand-core-0.6)
             ("rust-sha-1" ,rust-sha-1-0.9)
             ("rust-sha2" ,rust-sha2-0.9))))))

(define-public rust-pem-rfc7468-0.7
  (package
    (name "rust-pem-rfc7468")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pem-rfc7468" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04l4852scl4zdva31c1z6jafbak0ni5pi0j38ml108zwzjdrrcw8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pem-rfc7468")
    (synopsis
     "PEM Encoding implementing a subset of Privacy-Enhanced Mail encoding")
    (description
     "This package provides PEM Encoding (RFC 7468) for PKIX, PKCS, and CMS
Structures, implementing a strict subset of the original Privacy-Enhanced Mail
encoding intended specifically for use with cryptographic keys, certificates,
and other messages.  It provides a no_std-friendly, constant-time
implementation suitable for use with cryptographic private keys.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pem-rfc7468-0.6
  (package
    (inherit rust-pem-rfc7468-0.7)
    (name "rust-pem-rfc7468")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pem-rfc7468" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b5d8rvc4lgwxhs72m99fnrg0wq7bqh4x4wq0c7501ci7a1mkl94"))))
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1))))))

(define-public rust-pem-rfc7468-0.3
  (package
    (inherit rust-pem-rfc7468-0.7)
    (name "rust-pem-rfc7468")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pem-rfc7468" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c7vrrksg8fqzxb7q4clzl14f0qnqky7jqspjqi4pailiybmvph1"))))
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1))))))

(define-public rust-pem-rfc7468-0.2
  (package
    (inherit rust-pem-rfc7468-0.7)
    (name "rust-pem-rfc7468")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pem-rfc7468" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m1c9jypydzabg4yscplmvff7pdcc8gg4cqg081hnlf03hxkmsc4"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (((string-append ">=([[:digit:]]+(\\.[[:digit:]]+)*),"
                                   " <([[:digit:]]+(\\.[[:digit:]]+)*)")
                    _ version _)
                   (string-append ">=" version)))))))
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1))))))

(define-public rust-pkcs1-0.7
  (package
    (name "rust-pkcs1")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pkcs1" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zz4mil3nchnxljdfs2k5ab1cjqn7kq5lqp62n9qfix01zqvkzy8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-der" ,rust-der-0.7)
        ("rust-pkcs8" ,rust-pkcs8-0.10)
        ("rust-spki" ,rust-spki-0.7))
       #:cargo-development-inputs
       (("rust-const-oid" ,rust-const-oid-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs1")
    (synopsis "Implementation of Public-Key Cryptography Standards (PKCS) #1")
    (description
     "This package provides a pure Rust implementation of Public-Key
Cryptography Standards (PKCS) #1: RSA Cryptography Specifications Version 2.2
(RFC 8017).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs1-0.3
  (package
    (inherit rust-pkcs1-0.7)
    (name "rust-pkcs1")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkcs1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0813szfx13n4xl6l19m3lwj7pqgljqwc6ipxhr2dv0yc9k06d3x7"))))
    (arguments
     `(#:cargo-inputs (("rust-der" ,rust-der-0.5)
                       ("rust-pkcs8" ,rust-pkcs8-0.8)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-pkcs1-0.2
  (package
    (inherit rust-pkcs1-0.7)
    (name "rust-pkcs1")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkcs1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b2f1a0lf5h53zrjvcqbxzjhh89gcfa1myhf6z7w10ypg61fwsqi"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-der" ,rust-der-0.4)
        ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.2)
        ("rust-zeroize" ,rust-zeroize-1))))))

(define-public rust-pkcs5-0.7
  (package
    (name "rust-pkcs5")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pkcs5" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19k9igzay529fqj90qdkgnvmvwp65wzw73h2vn3sigqq3b4y4iz8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-cbc" ,rust-cbc-0.1)
        ("rust-der" ,rust-der-0.7)
        ("rust-des" ,rust-des-0.8)
        ("rust-pbkdf2" ,rust-pbkdf2-0.12)
        ("rust-scrypt" ,rust-scrypt-0.11)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-spki" ,rust-spki-0.7))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs5")
    (synopsis "Implementation of Public-Key Cryptography Standards (PKCS) #5")
    (description
     "This package is a pure Rust implementation of Public-Key Cryptography
Standards (PKCS) #5: Password-Based Cryptography Specification Version
2.1 (RFC 8018).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs5-0.5
  (package
    (inherit rust-pkcs5-0.7)
    (name "rust-pkcs5")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pkcs5" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0x81m285ijqi0fqkgym6a6ax02mfzdx87zfvqgrjsc2w3wn8c3fi"))))
    (arguments
     `(#:cargo-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-cbc" ,rust-cbc-0.1)
        ("rust-der" ,rust-der-0.6)
        ("rust-des" ,rust-des-0.8)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-pbkdf2" ,rust-pbkdf2-0.11)
        ("rust-scrypt" ,rust-scrypt-0.10)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-spki" ,rust-spki-0.6))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-pkcs5-0.4
  (package
    (inherit rust-pkcs5-0.7)
    (name "rust-pkcs5")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkcs5" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xhyi3k5p6lxb28ivcd1f3skdbmhzk0gamfry7q56pifx9xi8g6n"))))
    (arguments
     `(#:cargo-inputs (("rust-aes" ,rust-aes-0.7)
                       ("rust-block-modes" ,rust-block-modes-0.8)
                       ("rust-der" ,rust-der-0.5)
                       ("rust-des" ,rust-des-0.7)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.9)
                       ("rust-scrypt" ,rust-scrypt-0.8)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-spki" ,rust-spki-0.5))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-pkcs5-0.3
  (package
    (inherit rust-pkcs5-0.7)
    (name "rust-pkcs5")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkcs5" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m3xrrwwbn9883bylgjzssfh3w1lbl7fhkb3ndz721rf27pca8sl"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-aes" ,rust-aes-0.7)
        ("rust-block-modes" ,rust-block-modes-0.8)
        ("rust-der" ,rust-der-0.4)
        ("rust-des" ,rust-des-0.7)
        ("rust-hmac" ,rust-hmac-0.11)
        ("rust-pbkdf2" ,rust-pbkdf2-0.9)
        ("rust-scrypt" ,rust-scrypt-0.8)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-spki" ,rust-spki-0.4))))))

(define-public rust-pkcs7-0.4
  (package
    (name "rust-pkcs7")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkcs7" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rvp9gm7vzcbbzz6vr6xz6ri2szgxm35j0zk5dhf01b40sz7i4fp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-der" ,rust-der-0.7)
                       ("rust-spki" ,rust-spki-0.7)
                       ("rust-x509-cert" ,rust-x509-cert-0.2))
       #:cargo-development-inputs
       (("rust-der" ,rust-der-0.7)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-x509-cert" ,rust-x509-cert-0.2))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs7")
    (synopsis "Implementation of Public-Key Cryptography Standards (PKCS) #7")
    (description
     "This package is a pure Rust implementation of Public-Key
Cryptography Standards (PKCS) #7: Cryptographic Message Syntax
Specification (RFC 5652).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs8-0.10
  (package
    (name "rust-pkcs8")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pkcs8" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dx7w21gvn07azszgqd3ryjhyphsrjrmq5mmz1fbxkj5g0vv4l7r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-der" ,rust-der-0.7)
        ("rust-pkcs5" ,rust-pkcs5-0.7)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-spki" ,rust-spki-0.7)
        ("rust-subtle" ,rust-subtle-2))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs8")
    (synopsis "Implementation of Public-Key Cryptography Standards (PKCS) #8")
    (description
     "This package is a pure Rust implementation of Public-Key Cryptography
Standards (PKCS) #8: Private-Key Information Syntax Specification (RFC 5208),
with additional support for PKCS#8v2 asymmetric key packages (RFC 5958).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs8-0.9
  (package
    (inherit rust-pkcs8-0.10)
    (name "rust-pkcs8")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pkcs8" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fm4sigvcd0zpzg9jcp862a8p272kk08b9lgcs1dm1az19cjrjly"))))
    (arguments
     `(#:cargo-inputs
       (("rust-der" ,rust-der-0.6)
        ("rust-pkcs5" ,rust-pkcs5-0.5)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-spki" ,rust-spki-0.6)
        ("rust-subtle" ,rust-subtle-2))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-pkcs8-0.8
  (package
    (inherit rust-pkcs8-0.10)
    (name "rust-pkcs8")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkcs8" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l29h4mrgi2kpsl98jzky3ni5by3xa1sc6db9yd8l1i1p0zxmavw"))))
    (arguments
     `(#:cargo-inputs (("rust-der" ,rust-der-0.5)
                       ("rust-pkcs5" ,rust-pkcs5-0.4)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-spki" ,rust-spki-0.5)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-pkcs8-0.7
  (package
    (inherit rust-pkcs8-0.10)
    (name "rust-pkcs8")
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkcs8" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0iq46p6fa2b8xy6pj52zpmdy8ya3fg31dj4rc19x1fi69nvgjgpf"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-der" ,rust-der-0.4)
        ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.2)
        ("rust-pkcs1" ,rust-pkcs1-0.2)
        ("rust-pkcs5" ,rust-pkcs5-0.3)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-spki" ,rust-spki-0.4)
        ("rust-zeroize" ,rust-zeroize-1))))))

(define-public rust-poly1305-0.8
  (package
    (name "rust-poly1305")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "poly1305" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1grs77skh7d8vi61ji44i8gpzs3r9x7vay50i6cg8baxfa8bsnc1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-universal-hash" ,rust-universal-hash-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis "Poly1305 universal hash")
    (description
     "Poly1305 is a universal hash function which, when combined with
a cipher, can be used as a Message Authentication Code (MAC).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-poly1305-0.7
  (package
    (inherit rust-poly1305-0.8)
    (name "rust-poly1305")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "poly1305" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pkf4jlriskq9rvz8y5fjj9dw42q6yg5djijlin4n6p1dd3yp2h4"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("version = \">=1, <1\\.4\"") "version = \"^1\""))))))
    (arguments
     `(#:cargo-inputs
       (("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3)
        ("rust-universal-hash" ,rust-universal-hash-0.4)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-polyval-0.6
  (package
    (name "rust-polyval")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "polyval" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09gs56vm36ls6pyxgh06gw2875z2x77r8b2km8q28fql0q6yc7wx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3)
        ("rust-universal-hash" ,rust-universal-hash-0.5)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis "GHASH-like universal hash")
    (description "POLYVAL is a GHASH-like universal hash over GF(2^128) useful
for constructing a Message Authentication Code (MAC).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-polyval-0.5
  (package
    (inherit rust-polyval-0.6)
    (name "rust-polyval")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "polyval" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1890wqvc0csc9y9k9k4gsbz91rgdnhn6xnfmy9pqkh674fvd46c4"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (((string-append ">=([[:digit:]]+(\\.[[:digit:]]+)*),"
                                   " <([[:digit:]]+(\\.[[:digit:]]+)*)")
                    _ version _)
                   (string-append ">=" version)))))))
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-universal-hash" ,rust-universal-hash-0.4)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-polyval-0.4
  (package
    (inherit rust-polyval-0.6)
    (name "rust-polyval")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "polyval" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1kdpcjhc3666g8xaqichsyf6fhn8rry3z70dqhmvv6hb2jmc9g7f"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cpuid-bool" ,rust-cpuid-bool-0.2)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3)
        ("rust-universal-hash" ,rust-universal-hash-0.4)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-ppv-lite86-0.2
  (package
    (name "rust-ppv-lite86")
    (version "0.2.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ppv-lite86" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "017ax9ssdnpww7nrl1hvqh2lzncpv04nnsibmnw9nxjnaqlpp5bp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-zerocopy" ,rust-zerocopy-0.7))))
    (home-page "https://github.com/cryptocorrosion/cryptocorrosion")
    (synopsis "Implementation of the crypto-simd API for x86")
    (description "This crate provides an implementation of the crypto-simd API
for x86.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-primeorder-0.13
  (package
    (name "rust-primeorder")
    (version "0.13.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "primeorder" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rp16710mxksagcjnxqjjq9r9wf5vf72fs8wxffnvhb6i6hiqgim"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
        ("rust-serdect" ,rust-serdect-0.2))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/primeorder")
    (synopsis
     "Rust implementation of addition formulas for prime order elliptic curves")
    (description
     "This package contains a pure Rust implementation of complete addition
formulas for prime order elliptic curves (Renes-Costello-Batina 2015).  It
provides a generic over field elements and curve equation coefficients.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-rfc6979-0.4
  (package
    (name "rust-rfc6979")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rfc6979" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1chw95jgcfrysyzsq6a10b1j5qb7bagkx8h0wda4lv25in02mpgq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-hmac" ,rust-hmac-0.12)
        ("rust-subtle" ,rust-subtle-2))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://github.com/RustCrypto/signatures/tree/master/rfc6979")
    (synopsis "Pure Rust implementation of RFC6979")
    (description
     "This package provides a pure Rust implementation of RFC6979: Deterministic
Usage of the @dfn{Digital Signature Algorithm} (DSA) and @dfn{Elliptic Curve
Digital Signature Algorithm} (ECDSA).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-rfc6979-0.3
  (package
    (inherit rust-rfc6979-0.4)
    (name "rust-rfc6979")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rfc6979" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fzsp705b5lhwd2r9il9grc3lj6rm3b2r89vh0xv181gy5xg2hvp"))))
    (arguments
     `(#:cargo-inputs (("rust-crypto-bigint" ,rust-crypto-bigint-0.4)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-sha2" ,rust-sha2-0.10))))))

(define rust-ring-0.17-sources
  (package
    (name "rust-ring")
    (version "0.17.8.tar.gz")   ; Hack to adjust the output name.
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/briansmith/ring")
                    (commit "fa98b490bcbc99a01ff150896ec74c1813242d7f")))
             (file-name (git-file-name "rust-ring" version))
             (sha256
              (base32 "0rqfal81bf4l3dja98cajfjq2jbz1rcx7xdp2r33cxrm5y5psr28"))
             (patches (search-patches "rust-ring-0.17-ring-core.patch"))
             (snippet
              #~(begin (use-modules (guix build utils))
                       ;; It turns out Guix's nasm works just fine here.
                       (substitute* "build.rs"
                         (("./target/tools/windows/nasm/nasm") "nasm"))
                       ;; These files are pregenerated:
                       (delete-file "crypto/curve25519/curve25519_tables.h")
                       (delete-file "crypto/fipsmodule/ec/p256-nistz-table.h")
                       (delete-file "crypto/fipsmodule/ec/p256_table.h")
                       ;; As seen in git between 0.17.0 and 0.17.1.
                       (substitute* "crypto/curve25519/make_curve25519_tables.py"
                         (("static const uint8_t k25519Precomp")
                          "const uint8_t k25519Precomp"))
                       ;; This file causes problems during the 'package phase and
                       ;; is not distributed with the packaged crate.
                       (substitute* "Cargo.toml"
                         (("\"bench\",") ""))
                       (delete-file "bench/Cargo.toml")))))
    (build-system trivial-build-system)
    (arguments
     (list
       #:modules '((guix build utils))
       #:builder
       #~(begin
           (use-modules (guix build utils))
           (setenv "PATH"
                   (string-join
                     (list #+(this-package-native-input "clang") ; for clang-format
                           #+(this-package-native-input "go")
                           #+(this-package-native-input "gzip")
                           #+(this-package-native-input "nasm")
                           #+(this-package-native-input "perl")
                           #+(this-package-native-input "python-minimal")
                           #+(this-package-native-input "tar"))
                     "/bin:" 'suffix))

           (setenv "HOME" (getcwd))
           (copy-recursively #+source (string-append "ring-" #$version))
           (with-directory-excursion (string-append "ring-" #$version)
             (begin
               (with-directory-excursion "crypto/curve25519"
                 (with-output-to-file "curve25519_tables.h"
                   (lambda _ (invoke "python3" "make_curve25519_tables.py"))))
               (with-directory-excursion "crypto/fipsmodule/ec"
                 (invoke "go" "run" "make_tables.go")
                 (invoke "go" "run" "make_ec_scalar_base_mult_tests.go"))
               (format #t "Generating the pregenerated files ...~%")
               (force-output)
               (mkdir-p "pregenerated/tmp/ring_core_generated")

               ;; We generate all the files which upstream would normally be
               ;; generate by using 'RING_PREGENERATE_ASM=1 cargo build
               ;; --target-dir=target/pregenerate_asm' in order to not include
               ;; a dependency on cargo when generating the sources.
               (define (prefix script)
                 (string-append
                   "pregenerated/"
                   (string-drop-right
                     (string-drop script
                                  (string-index-right script #\/)) 3)))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "ios64"
                           (string-append (prefix script) "-ios64.S"))
                   (invoke "perl" script "linux64"
                           (string-append (prefix script) "-linux64.S"))
                   (invoke "perl" script "win64"
                           (string-append (prefix script) "-win64.S")))
                 '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                   "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                   "crypto/chacha/asm/chacha-armv8.pl"
                   "crypto/cipher_extra/asm/chacha20_poly1305_armv8.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-armv8.pl"
                   "crypto/fipsmodule/bn/asm/armv8-mont.pl"
                   "crypto/fipsmodule/ec/asm/p256-armv8-asm.pl"
                   "crypto/fipsmodule/modes/asm/ghash-neon-armv8.pl"
                   "crypto/fipsmodule/modes/asm/aesv8-gcm-armv8.pl"
                   "crypto/fipsmodule/sha/asm/sha512-armv8.pl"))

               (for-each
                 (lambda (arch)
                   (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-armv8.pl"
                           arch (string-append
                                  "pregenerated/sha256-armv8-" arch ".S")))
                 '("ios64" "linux64" "win64"))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "linux32"
                           (string-append (prefix script) "-linux32.S")))
                 '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                   "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                   "crypto/fipsmodule/aes/asm/bsaes-armv7.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-armv7.pl"
                   "crypto/fipsmodule/bn/asm/armv4-mont.pl"
                   "crypto/chacha/asm/chacha-armv4.pl"
                   "crypto/fipsmodule/modes/asm/ghash-armv4.pl"
                   "crypto/fipsmodule/sha/asm/sha256-armv4.pl"
                   "crypto/fipsmodule/sha/asm/sha512-armv4.pl"))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "elf"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append (prefix script) "-elf.S"))
                   (invoke "perl" script "win32n"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append
                             "pregenerated/tmp/"
                             (string-drop (prefix script) 13) "-win32n.asm")))
                 '("crypto/fipsmodule/aes/asm/aesni-x86.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-x86.pl"
                   "crypto/fipsmodule/bn/asm/x86-mont.pl"
                   "crypto/chacha/asm/chacha-x86.pl"
                   "crypto/fipsmodule/modes/asm/ghash-x86.pl"))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "elf"
                           (string-append (prefix script) "-elf.S"))
                   (invoke "perl" script "macosx"
                           (string-append (prefix script) "-macosx.S"))
                   (invoke "perl" script "nasm"
                           (string-append
                             "pregenerated/tmp/"
                             (string-drop (prefix script) 13) "-nasm.asm")))
                 '("crypto/chacha/asm/chacha-x86_64.pl"
                   "crypto/fipsmodule/aes/asm/aesni-x86_64.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-x86_64.pl"
                   "crypto/fipsmodule/bn/asm/x86_64-mont.pl"
                   "crypto/fipsmodule/bn/asm/x86_64-mont5.pl"
                   "crypto/fipsmodule/ec/asm/p256-x86_64-asm.pl"
                   "crypto/fipsmodule/modes/asm/aesni-gcm-x86_64.pl"
                   "crypto/fipsmodule/modes/asm/ghash-x86_64.pl"
                   "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                   "crypto/cipher_extra/asm/chacha20_poly1305_x86_64.pl"))

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "elf" "pregenerated/sha256-x86_64-elf.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "macosx" "pregenerated/sha256-x86_64-macosx.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "nasm" "pregenerated/tmp/sha256-x86_64-nasm.asm")

               ;; TODO: Extract ring_core_generated/prefix_symbols_nasm.inc
               ;; and ring_core_generated/prefix_symbols_asm.h from build.rs.

               (for-each
                 (lambda (script)
                   (invoke "nasm" "-o" (string-append (prefix script) "o")
                           "-f" "win32" "-i" "include/" "-i" "pregenerated/tmp/"
                           "-Xgnu" "-gcv8" script))
                 (find-files "pregenerated/tmp" "win32n\\.asm"))

               (for-each
                 (lambda (script)
                   (invoke "nasm" "-o" (string-append (prefix script) "o")
                           "-f" "win64" "-i" "include/" "-i" "pregenerated/tmp/"
                           "-Xgnu" "-gcv8" script))
                 (find-files "pregenerated/tmp" "nasm\\.asm"))

               (format #t "Creating the tarball ...~%")
               (force-output)
               (with-directory-excursion "../"
                 (invoke "tar" "czf" #$output
                         ;; avoid non-determinism in the archive
                         "--sort=name" "--mtime=@0"
                         "--owner=root:0" "--group=root:0"
                         (string-append "ring-" #$version))))))))
    (native-inputs
     (list clang go gzip nasm perl python-minimal tar))
    (home-page "https://github.com/briansmith/ring")
    (synopsis "Safe, fast, small crypto using Rust")
    (description "This package provided safe, fast, small crypto using Rust.")
    (license (list license:isc license:openssl))))

(define-public rust-ring-0.17
  (package
    (name "rust-ring")
    (version "0.17.8")
    (source rust-ring-0.17-sources)
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-spin" ,rust-spin-0.9)
                       ("rust-untrusted" ,rust-untrusted-0.9)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/briansmith/ring")
    (synopsis "Safe, fast, small crypto using Rust")
    (description "This package provided safe, fast, small crypto using Rust.")
    (license (list license:isc license:openssl))))

(define rust-ring-0.16-sources
  (package
    (inherit rust-ring-0.17-sources)
    (name "rust-ring")
    (version "0.16.20.tar.gz")  ; Hack to adjust the output name.
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/briansmith/ring")
                    (commit "9cc0d45f4d8521f467bb3a621e74b1535e118188")))
             (file-name (git-file-name "rust-ring" version))
             (sha256
              (base32 "1aps05i5308ka03968glnnqr4kdkk2x4ghlg5vrqhl78jm6ivvby"))
             (snippet
              #~(begin (use-modules (guix build utils))
                       ;; It turns out Guix's nasm works just fine here.
                       (substitute* "build.rs"
                         (("./target/tools/nasm") "nasm"))
                       ;; These files are pregenerated:
                       (delete-file "crypto/curve25519/curve25519_tables.h")
                       (delete-file "crypto/fipsmodule/ec/ecp_nistz256_table.inl")))))
    (arguments
     (list
       #:modules '((guix build utils))
       #:builder
       #~(begin
           (use-modules (guix build utils))
           (setenv "PATH"
                   (string-join
                     (list #+(this-package-native-input "clang") ; for clang-format
                           #+(this-package-native-input "go")
                           #+(this-package-native-input "gzip")
                           #+(this-package-native-input "nasm")
                           #+(this-package-native-input "perl")
                           #+(this-package-native-input "python2-minimal")
                           #+(this-package-native-input "tar"))
                     "/bin:" 'suffix))

           (setenv "HOME" (getcwd))
           (copy-recursively #+source (string-append "ring-" #$version))
           (with-directory-excursion (string-append "ring-" #$version)
             (begin
               (with-directory-excursion "crypto/curve25519"
                 (with-output-to-file "curve25519_tables.h"
                   (lambda _ (invoke "python" "make_curve25519_tables.py"))))
               (with-directory-excursion "crypto/fipsmodule/ec"
                 (with-output-to-file "ecp_nistz256_table.inl"
                   (lambda _ (invoke "go" "run" "make_p256-x86_64-table.go"))))
               (format #t "Generating the pregenerated files ...~%")
               (force-output)
               (mkdir-p "pregenerated/tmp")

               ;; We generate all the files which upstream would normally be
               ;; generate by using '(cd pregenerate_asm && cargo clean &&
               ;; cargo build) ./pregenerate_asm/target/debug/pregenerate_asm'
               ;; in order to not include a dependency on cargo when
               ;; generating the sources.
               (define (prefix script)
                 (string-append
                   "pregenerated/"
                   (string-drop-right
                     (string-drop script
                                  (string-index-right script #\/)) 3)))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "elf"
                           (string-append (prefix script) "-elf.S"))
                   (invoke "perl" script "macosx"
                           (string-append (prefix script) "-macosx.S"))
                   (invoke "perl" script "nasm"
                           (string-append
                             "pregenerated/tmp/"
                             (string-drop (prefix script) 13) "-nasm.asm")))
                 '("crypto/fipsmodule/aes/asm/aesni-x86_64.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-x86_64.pl"
                   "crypto/fipsmodule/bn/asm/x86_64-mont.pl"
                   "crypto/fipsmodule/bn/asm/x86_64-mont5.pl"
                   "crypto/chacha/asm/chacha-x86_64.pl"
                   "crypto/fipsmodule/ec/asm/p256-x86_64-asm.pl"
                   "crypto/fipsmodule/modes/asm/aesni-gcm-x86_64.pl"
                   "crypto/fipsmodule/modes/asm/ghash-x86_64.pl"
                   "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                   "crypto/cipher_extra/asm/chacha20_poly1305_x86_64.pl"))

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "elf" "pregenerated/sha256-x86_64-elf.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "macosx" "pregenerated/sha256-x86_64-macosx.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "nasm" "pregenerated/tmp/sha256-x86_64-nasm.asm")

               (for-each
                 (lambda (script)
                   (invoke "nasm" "-o" (string-append (prefix script) "obj")
                           "-f" "win64" "-Xgnu" "-gcv8" script))
               (find-files "pregenerated/tmp" "\\.asm"))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "ios64"
                           (string-append (prefix script) "-ios64.S"))
                   (invoke "perl" script "linux64"
                           (string-append (prefix script) "-linux64.S")))
                 '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                   "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-armv8.pl"
                   "crypto/fipsmodule/bn/asm/armv8-mont.pl"
                   "crypto/chacha/asm/chacha-armv8.pl"
                   "crypto/fipsmodule/ec/asm/ecp_nistz256-armv8.pl"
                   "crypto/fipsmodule/modes/asm/ghash-neon-armv8.pl"
                   "crypto/fipsmodule/sha/asm/sha512-armv8.pl"))

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-armv8.pl"
                       "ios64" "pregenerated/sha256-armv8-ios64.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-armv8.pl"
                       "linux64" "pregenerated/sha256-armv8-linux64.S")

               (for-each
                 (lambda (script)
                   (invoke "perl" script "elf"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append (prefix script) "-elf.S"))
                   (invoke "perl" script "macosx"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append (prefix script) "-macosx.S"))
                   (invoke "perl" script "win32n"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append
                             "pregenerated/tmp/"
                             (string-drop (prefix script) 13) "-win32n.asm")))
                 '("crypto/fipsmodule/aes/asm/aesni-x86.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-x86.pl"
                   "crypto/fipsmodule/bn/asm/x86-mont.pl"
                   "crypto/chacha/asm/chacha-x86.pl"
                   "crypto/fipsmodule/ec/asm/ecp_nistz256-x86.pl"
                   "crypto/fipsmodule/modes/asm/ghash-x86.pl"))

               (for-each
                 (lambda (script)
                   (invoke "nasm" "-o" (string-append (prefix script) "obj")
                           "-f" "win32" "-Xgnu" "-gcv8" script))
               (find-files "pregenerated/tmp" "-win32n\\.asm"))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "ios32"
                           (string-append (prefix script) "-ios32.S"))
                   (invoke "perl" script "linux32"
                           (string-append (prefix script) "-linux32.S")))
                 '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                   "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                   "crypto/fipsmodule/aes/asm/bsaes-armv7.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-armv7.pl"
                   "crypto/fipsmodule/bn/asm/armv4-mont.pl"
                   "crypto/chacha/asm/chacha-armv4.pl"
                   "crypto/fipsmodule/ec/asm/ecp_nistz256-armv4.pl"
                   "crypto/fipsmodule/modes/asm/ghash-armv4.pl"
                   "crypto/fipsmodule/sha/asm/sha256-armv4.pl"
                   "crypto/fipsmodule/sha/asm/sha512-armv4.pl"))

               (format #t "Creating the tarball ...~%")
               (force-output)
               ;; The other option is to use cargo package --allow-dirty
               (with-directory-excursion "../"
                 (invoke "tar" "czf" #$output
                         ;; avoid non-determinism in the archive
                         "--sort=name" "--mtime=@0"
                         "--owner=root:0" "--group=root:0"
                         (string-append "ring-" #$version))))))))
    (native-inputs
     (list clang go gzip nasm perl python2-minimal tar))))

(define-public rust-ring-0.16
  (package
    (inherit rust-ring-0.17)
    (name "rust-ring")
    (version "0.16.20")
    (source rust-ring-0.16-sources)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-spin" ,rust-spin-0.5)
        ("rust-untrusted" ,rust-untrusted-0.7)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-winapi" ,rust-winapi-0.3)
        ;; build dependencies
        ("rust-cc" ,rust-cc-1))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    ;; For a mostly complete list of supported systems see:
    ;; https://github.com/briansmith/ring/blob/main/.github/workflows/ci.yml#L170
    (supported-systems (list "aarch64-linux" "armhf-linux"
                             "i686-linux" "x86_64-linux"))))

(define rust-ring-0.14-sources
  (package
    (inherit rust-ring-0.17-sources)
    (name "rust-ring")
    (version "0.14.6.tar.gz")   ; Hack to adjust the output name.
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/briansmith/ring")
                    (commit "ef85df478152aa3fe06c811309379efa08f8a529")))
             (file-name (git-file-name "rust-ring" version))
             (sha256
              (base32 "12dgw2spvmkdypgzymw3bxpv4bbpnlq8s10sdggral31x597n6xx"))
             (snippet
              #~(begin (use-modules (guix build utils))
                       ;; It turns out Guix's yasm works just fine here.
                       (substitute* "build.rs"
                         (("yasm.exe") "yasm"))
                       ;; These files are pregenerated:
                       (delete-file "third_party/fiat/curve25519_tables.h")
                       (delete-file "crypto/fipsmodule/ec/ecp_nistz256_table.inl")
                       (delete-file "util/ar/testdata/linux/libsample.a")
                       (delete-file "util/ar/testdata/mac/libsample.a")
                       (delete-file "util/ar/testdata/windows/sample.lib")
                       ;; Fix the doc tests.
                       (substitute* "src/ec/curve25519/ed25519/verification.rs"
                         ((";;") ";"))))))
    (arguments
     (list
       #:modules '((guix build utils))
       #:builder
       #~(begin
           (use-modules (guix build utils))
           (setenv "PATH"
                   (string-join
                     (list #+(this-package-native-input "clang") ; for clang-format
                           #+(this-package-native-input "go")
                           #+(this-package-native-input "gzip")
                           #+(this-package-native-input "perl")
                           #+(this-package-native-input "python2-minimal")
                           #+(this-package-native-input "tar")
                           #+(this-package-native-input "yasm"))
                     "/bin:" 'suffix))

           (setenv "HOME" (getcwd))
           (copy-recursively #+source (string-append "ring-" #$version))
           (with-directory-excursion (string-append "ring-" #$version)
             (begin
               (with-directory-excursion "third_party/fiat"
                 (with-output-to-file "curve25519_tables.h"
                   (lambda _ (invoke "python" "make_curve25519_tables.py"))))
               (with-directory-excursion "crypto/fipsmodule/ec"
                 ;; This one seems to have been changed elsewhere in the
                 ;; sources but not in the script generating the definition.
                 (substitute* "make_p256-x86_64-table.go"
                   (("ecp_nistz256_precomputed") "GFp_nistz256_precomputed"))
                 (with-output-to-file "ecp_nistz256_table.inl"
                   (lambda _ (invoke "go" "run" "make_p256-x86_64-table.go"))))
               (format #t "Generating the pregenerated files ...~%")
               (force-output)
               (mkdir-p "pregenerated/tmp")

               ;; We generate all the files which upstream would normally be
               ;; generate by using '(cd pregenerate_asm && cargo clean &&
               ;; cargo build) ./pregenerate_asm/target/debug/pregenerate_asm'
               ;; in order to not include a dependency on cargo when
               ;; generating the sources.
               (define (prefix script)
                 (string-append
                   "pregenerated/"
                   (string-drop-right
                     (string-drop script (string-index-right script #\/)) 3)))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "elf"
                           (string-append (prefix script) "-elf.S"))
                   (invoke "perl" script "macosx"
                           (string-append (prefix script) "-macosx.S"))
                   (invoke "perl" script "nasm"
                           (string-append
                             "pregenerated/tmp/"
                             (string-drop (prefix script) 13) "-nasm.asm")))
                 '("crypto/fipsmodule/aes/asm/aes-x86_64.pl"
                   "crypto/fipsmodule/aes/asm/aesni-x86_64.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-x86_64.pl"
                   "crypto/fipsmodule/bn/asm/x86_64-mont.pl"
                   "crypto/fipsmodule/bn/asm/x86_64-mont5.pl"
                   "crypto/chacha/asm/chacha-x86_64.pl"
                   "crypto/fipsmodule/ec/asm/p256-x86_64-asm.pl"
                   "crypto/fipsmodule/modes/asm/aesni-gcm-x86_64.pl"
                   "crypto/fipsmodule/modes/asm/ghash-x86_64.pl"
                   "crypto/poly1305/asm/poly1305-x86_64.pl"
                   "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"))

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "elf" "pregenerated/sha256-x86_64-elf.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "macosx" "pregenerated/sha256-x86_64-macosx.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "nasm" "pregenerated/tmp/sha256-x86_64-nasm.asm")

               (for-each
                 (lambda (script)
                   (invoke "yasm" "-X" "vc" "--dformat=cv8"
                           "--oformat=win64" "--machine=amd64" "-o"
                           (string-append (prefix script) "obj") script))
                 (find-files "pregenerated/tmp" "\\.asm"))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "ios64"
                           (string-append (prefix script) "-ios64.S"))
                   (invoke "perl" script "linux64"
                           (string-append (prefix script) "-linux64.S")))
                 '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                   "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                   "crypto/fipsmodule/bn/asm/armv8-mont.pl"
                   "crypto/chacha/asm/chacha-armv8.pl"
                   "crypto/fipsmodule/ec/asm/ecp_nistz256-armv8.pl"
                   "crypto/poly1305/asm/poly1305-armv8.pl"
                   "crypto/fipsmodule/sha/asm/sha512-armv8.pl"))

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-armv8.pl"
                       "ios64" "pregenerated/sha256-armv8-ios64.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-armv8.pl"
                       "linux64" "pregenerated/sha256-armv8-linux64.S")

               (for-each
                 (lambda (script)
                   (invoke "perl" script "elf"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append (prefix script) "-elf.S"))
                   (invoke "perl" script "macosx"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append (prefix script) "-macosx.S"))
                   (invoke "perl" script "win32n"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append
                             "pregenerated/tmp/"
                             (string-drop (prefix script) 13) "-win32n.asm")))
                 '("crypto/fipsmodule/aes/asm/aes-586.pl"
                   "crypto/fipsmodule/aes/asm/aesni-x86.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-x86.pl"
                   "crypto/fipsmodule/bn/asm/x86-mont.pl"
                   "crypto/chacha/asm/chacha-x86.pl"
                   "crypto/fipsmodule/ec/asm/ecp_nistz256-x86.pl"
                   "crypto/fipsmodule/modes/asm/ghash-x86.pl"
                   "crypto/poly1305/asm/poly1305-x86.pl"
                   "crypto/fipsmodule/sha/asm/sha256-586.pl"
                   "crypto/fipsmodule/sha/asm/sha512-586.pl"))

               (for-each
                 (lambda (script)
                   (invoke "yasm" "-X" "vc" "--dformat=cv8"
                           "--oformat=win32" "--machine=x86" "-o"
                           (string-append (prefix script) "obj") script))
                 (find-files "pregenerated/tmp" "-win32n\\.asm"))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "ios32"
                           (string-append (prefix script) "-ios32.S"))
                   (invoke "perl" script "linux32"
                           (string-append (prefix script) "-linux32.S")))
                 '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                   "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                   "crypto/fipsmodule/aes/asm/aes-armv4.pl"
                   "crypto/fipsmodule/aes/asm/bsaes-armv7.pl"
                   "crypto/fipsmodule/bn/asm/armv4-mont.pl"
                   "crypto/chacha/asm/chacha-armv4.pl"
                   "crypto/fipsmodule/ec/asm/ecp_nistz256-armv4.pl"
                   "crypto/fipsmodule/modes/asm/ghash-armv4.pl"
                   "crypto/poly1305/asm/poly1305-armv4.pl"
                   "crypto/fipsmodule/sha/asm/sha256-armv4.pl"
                   "crypto/fipsmodule/sha/asm/sha512-armv4.pl"))

               (format #t "Creating the tarball ...~%")
               (force-output)
               ;; The other option is to use cargo package --allow-dirty
               (with-directory-excursion "../"
                 (invoke "tar" "czf" #$output
                         ;; avoid non-determinism in the archive
                         "--sort=name" "--mtime=@0"
                         "--owner=root:0" "--group=root:0"
                         (string-append "ring-" #$version))))))))
    (native-inputs
     (list clang go gzip perl python2-minimal tar yasm))))

(define-public rust-ring-0.14
  (package
    (inherit rust-ring-0.16)
    (name "rust-ring")
    (version "0.14.6")
    (source rust-ring-0.14-sources)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-spin" ,rust-spin-0.5)
        ("rust-untrusted" ,rust-untrusted-0.6)
        ("rust-winapi" ,rust-winapi-0.3))))))

(define rust-ring-0.13-sources
  (package
    (inherit rust-ring-0.17-sources)
    (name "rust-ring")
    (version "0.13.5.tar.gz")   ; Hack to adjust the output name
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/briansmith/ring")
                    (commit "704e4216a397bd830479bcd6d7dd67fc62cdbe67")))
             (file-name (git-file-name "rust-ring" version))
             (sha256
              (base32 "0iqwf8i2i0a46ymrqss1ngbd2lqphk0mw74c65pxb8skyn2n7csi"))
             (snippet
              #~(begin (use-modules (guix build utils))
                       ;; Make some adjustments for newer versions of rust
                       ;; error: `...` range patterns are deprecated
                       (substitute* "src/digest/sha1.rs"
                         (("0\\.\\.\\.") "0..="))
                       (substitute* "build.rs"
                         (("out_dir\\.clone\\(\\)") "out_dir")
                         (("libs\\.into_iter\\(\\)") "libs.iter()"))
                       ;; It turns out Guix's yasm works just fine here.
                       (substitute* "build.rs"
                         (("yasm.exe") "yasm"))
                       ;; Files which would be deleted in a snippet:
                       (delete-file "third_party/fiat/curve25519_tables.h")
                       (delete-file "crypto/fipsmodule/ec/ecp_nistz256_table.inl")))))
    (arguments
     (list
       #:modules '((guix build utils))
       #:builder
       #~(begin
           (use-modules (guix build utils))
           (setenv "PATH"
                   (string-join
                     (list #+(this-package-native-input "clang") ; for clang-format
                           #+(this-package-native-input "go")
                           #+(this-package-native-input "gzip")
                           #+(this-package-native-input "perl")
                           #+(this-package-native-input "python2-minimal")
                           #+(this-package-native-input "tar")
                           #+(this-package-native-input "yasm"))
                     "/bin:" 'suffix))

           (setenv "HOME" (getcwd))
           (copy-recursively #+source (string-append "ring-" #$version))
           (with-directory-excursion (string-append "ring-" #$version)
             (begin
               (with-directory-excursion "third_party/fiat"
                 (with-output-to-file "curve25519_tables.h"
                   (lambda _ (invoke "python" "make_curve25519_tables.py"))))
               (with-directory-excursion "crypto/fipsmodule/ec"
                 ;; This one seems to have been changed elsewhere in the
                 ;; sources but not in the script generating the definition.
                 (substitute* "make_p256-x86_64-table.go"
                   (("ecp_nistz256_precomputed") "GFp_nistz256_precomputed"))
                 (with-output-to-file "ecp_nistz256_table.inl"
                   (lambda _ (invoke "go" "run" "make_p256-x86_64-table.go"))))
               (format #t "Generating the pregenerated files ...~%")
               (force-output)
               (mkdir-p "pregenerated/tmp")

               ;; We generate all the files which upstream would normally be
               ;; generate by using '(cd pregenerate_asm && cargo clean &&
               ;; cargo build) ./pregenerate_asm/target/debug/pregenerate_asm'
               ;; in order to not include a dependency on cargo when
               ;; generating the sources.
               (define (prefix script)
                 (string-append
                   "pregenerated/"
                   (string-drop-right
                     (string-drop script
                                  (string-index-right script #\/)) 3)))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "elf"
                           (string-append (prefix script) "-elf.S"))
                   (invoke "perl" script "macosx"
                           (string-append (prefix script) "-macosx.S"))
                   (invoke "perl" script "nasm"
                           (string-append
                             "pregenerated/tmp/"
                             (string-drop (prefix script) 13) "-nasm.asm")))
                 '("crypto/fipsmodule/aes/asm/aes-x86_64.pl"
                   "crypto/fipsmodule/aes/asm/aesni-x86_64.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-x86_64.pl"
                   "crypto/fipsmodule/bn/asm/x86_64-mont.pl"
                   "crypto/fipsmodule/bn/asm/x86_64-mont5.pl"
                   "crypto/chacha/asm/chacha-x86_64.pl"
                   "crypto/fipsmodule/ec/asm/p256-x86_64-asm.pl"
                   "crypto/fipsmodule/modes/asm/aesni-gcm-x86_64.pl"
                   "crypto/fipsmodule/modes/asm/ghash-x86_64.pl"
                   "crypto/poly1305/asm/poly1305-x86_64.pl"
                   "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"))

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "elf" "pregenerated/sha256-x86_64-elf.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "macosx" "pregenerated/sha256-x86_64-macosx.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "nasm" "pregenerated/tmp/sha256-x86_64-nasm.asm")

               (for-each
                 (lambda (script)
                   (invoke "yasm" "-X" "vc" "--dformat=cv8"
                           "--oformat=win64" "--machine=amd64" "-o"
                           (string-append (prefix script) "obj") script))
                 (find-files "pregenerated/tmp" "\\.asm"))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "ios64"
                           (string-append (prefix script) "-ios64.S"))
                   (invoke "perl" script "linux64"
                           (string-append (prefix script) "-linux64.S")))
                 '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                   "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                   "crypto/fipsmodule/bn/asm/armv8-mont.pl"
                   "crypto/chacha/asm/chacha-armv8.pl"
                   "crypto/fipsmodule/ec/asm/ecp_nistz256-armv8.pl"
                   "crypto/poly1305/asm/poly1305-armv8.pl"
                   "crypto/fipsmodule/sha/asm/sha512-armv8.pl"))

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-armv8.pl"
                       "ios64" "pregenerated/sha256-armv8-ios64.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-armv8.pl"
                       "linux64" "pregenerated/sha256-armv8-linux64.S")

               (for-each
                 (lambda (script)
                   (invoke "perl" script "elf"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append (prefix script) "-elf.S"))
                   (invoke "perl" script "macosx"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append (prefix script) "-macosx.S"))
                   (invoke "perl" script "win32n"
                           "-fPIC" "-DOPENSSL_IA32_SSE2"
                           (string-append
                             "pregenerated/tmp/"
                             (string-drop (prefix script) 13) "-win32n.asm")))
                 '("crypto/fipsmodule/aes/asm/aes-586.pl"
                   "crypto/fipsmodule/aes/asm/aesni-x86.pl"
                   "crypto/fipsmodule/aes/asm/vpaes-x86.pl"
                   "crypto/fipsmodule/bn/asm/x86-mont.pl"
                   "crypto/chacha/asm/chacha-x86.pl"
                   "crypto/fipsmodule/ec/asm/ecp_nistz256-x86.pl"
                   "crypto/fipsmodule/modes/asm/ghash-x86.pl"
                   "crypto/poly1305/asm/poly1305-x86.pl"
                   "crypto/fipsmodule/sha/asm/sha256-586.pl"
                   "crypto/fipsmodule/sha/asm/sha512-586.pl"))

               (for-each
                 (lambda (script)
                   (invoke "yasm" "-X" "vc" "--dformat=cv8"
                           "--oformat=win32" "--machine=x86" "-o"
                           (string-append (prefix script) "obj") script))
                 (find-files "pregenerated/tmp" "-win32n\\.asm"))

               (for-each
                 (lambda (script)
                   (invoke "perl" script "ios32"
                           (string-append (prefix script) "-ios32.S"))
                   (invoke "perl" script "linux32"
                           (string-append (prefix script) "-linux32.S")))
                 '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                   "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                   "crypto/fipsmodule/aes/asm/aes-armv4.pl"
                   "crypto/fipsmodule/aes/asm/bsaes-armv7.pl"
                   "crypto/fipsmodule/bn/asm/armv4-mont.pl"
                   "crypto/chacha/asm/chacha-armv4.pl"
                   "crypto/fipsmodule/ec/asm/ecp_nistz256-armv4.pl"
                   "crypto/fipsmodule/modes/asm/ghash-armv4.pl"
                   "crypto/poly1305/asm/poly1305-armv4.pl"
                   "crypto/fipsmodule/sha/asm/sha256-armv4.pl"
                   "crypto/fipsmodule/sha/asm/sha512-armv4.pl"))

               (format #t "Creating the tarball ...~%")
               (force-output)
               ;; The other option is to use cargo package --allow-dirty
               (with-directory-excursion "../"
                 (invoke "tar" "czf" #$output
                         ;; avoid non-determinism in the archive
                         "--sort=name" "--mtime=@0"
                         "--owner=root:0" "--group=root:0"
                         (string-append "ring-" #$version))))))))
    (native-inputs
     (list clang go gzip perl python2-minimal tar yasm))))

(define-public rust-ring-0.13
  (package
    (inherit rust-ring-0.14)
    (name "rust-ring")
    (version "0.13.5")
    (source rust-ring-0.13-sources)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-untrusted" ,rust-untrusted-0.6)
        ;; build dependencies
        ("rust-cc" ,rust-cc-1))))))

(define-public rust-ring-compat-0.8
  (package
    (name "rust-ring-compat")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ring-compat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m2vvmbg607f55afx75b9kxbyx6b5wqvhhfv2445z08b2np7pknc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aead" ,rust-aead-0.5)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-ecdsa" ,rust-ecdsa-0.16)
                       ("rust-ed25519" ,rust-ed25519-2)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-p256" ,rust-p256-0.13)
                       ("rust-p384" ,rust-p384-0.13)
                       ("rust-pkcs8" ,rust-pkcs8-0.10)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-ring" ,rust-ring-0.17)
                       ("rust-signature" ,rust-signature-2))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hex-literal" ,rust-hex-literal-0.4))))
    (home-page "https://github.com/RustCrypto/ring-compat")
    (synopsis "RustCrypto trait and ring crypto algorithm compatibility")
    (description
     "This package provides compatibility for using @code{RustCrypto}
traits with cryptographic algorithm implementations from @code{ring}.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ripemd-0.1
  (package
    (name "rust-ripemd")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ripemd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17xh5yl9wjjj2v18rh3m8ajlmdjg1yj13l6r9rj3mnbss4i444mx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.10))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "Pure Rust implementation of the RIPEMD hash functions")
    (description "Pure Rust implementation of the RIPEMD hash functions")
    (license (list license:expat license:asl2.0))))

(define-public rust-rsa-0.9
  (package
    (name "rust-rsa")
    (version "0.9.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06amqm85raq26v6zg00fbf93lbj3kx559n2lpxc3wrvbbiy5vis7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-const-oid" ,rust-const-oid-0.9)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
        ("rust-num-integer" ,rust-num-integer-0.1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-pkcs1" ,rust-pkcs1-0.7)
        ("rust-pkcs8" ,rust-pkcs8-0.10)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-serde" ,rust-serde-1)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-signature" ,rust-signature-2)
        ("rust-spki" ,rust-spki-0.7)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-base64ct" ,rust-base64ct-1)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
        ("rust-serde-test" ,rust-serde-test-1)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-sha3" ,rust-sha3-0.10))))
    (home-page "https://github.com/RustCrypto/RSA")
    (synopsis "Pure Rust RSA implementation")
    (description "This package provides a pure Rust RSA implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rsa-0.6
  (package
    (inherit rust-rsa-0.9)
    (name "rust-rsa")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02viiiylxpk2hx5h5qrpm4lcd8ildvafbw0rn6rx44wnqia2gwjc"))))
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-iter" ,rust-num-iter-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pkcs1" ,rust-pkcs1-0.3)
                       ("rust-pkcs8" ,rust-pkcs8-0.8)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-base64ct" ,rust-base64ct-1)
                                   ("rust-hex-literal" ,rust-hex-literal-0.3)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-chacha" ,rust-rand-chacha-0.3)
                                   ("rust-rand-core" ,rust-rand-core-0.6)
                                   ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-sha1" ,rust-sha1-0.10)
                                   ("rust-sha2" ,rust-sha2-0.10)
                                   ("rust-sha3" ,rust-sha3-0.10))))))

(define-public rust-rsa-0.5
  (package
    (inherit rust-rsa-0.9)
    (name "rust-rsa")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "039676a4mj0875phdi7vc0bd37hv84dh0dql6fmk8dl2w81jcp70"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "Cargo.toml"
                   (("version = \">=1, <1.5\"") "version = \"^1\""))))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.7)
        ("rust-num-integer" ,rust-num-integer-0.1)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-pkcs1" ,rust-pkcs1-0.2)
        ("rust-pkcs8" ,rust-pkcs8-0.7)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-hex" ,rust-hex-0.4)
         ("rust-hex-literal" ,rust-hex-literal-0.3)
         ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
         ("rust-serde-test" ,rust-serde-test-1)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-sha3" ,rust-sha3-0.9))))))

(define-public rust-rust-argon2-2
  (package
    (name "rust-rust-argon2")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rust-argon2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s66kgbvnv5vaq4vlglx587bq93c662whrniz6ycpjb03m9li64x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.21)
        ("rust-blake2b-simd" ,rust-blake2b-simd-1)
        ("rust-constant-time-eq" ,rust-constant-time-eq-0.3)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-hex" ,rust-hex-0.4))))
    (home-page "https://github.com/sru-systems/rust-argon2")
    (synopsis "Argon2 password hashing function in Rust")
    (description
     "This package provides a Rust implementation of the Argon2 password
hashing function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-argon2-0.8
  (package
    (inherit rust-rust-argon2-2)
    (name "rust-rust-argon2")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rust-argon2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yvqkv04fqk3cbvyasibr4bqbxa6mij8jdvibakwlcsbjh6q462b"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-blake2b-simd" ,rust-blake2b-simd-0.5)
        ("rust-constant-time-eq" ,rust-constant-time-eq-0.1)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-serde" ,rust-serde-1))))))

(define-public rust-salsa20-0.10
  (package
    (name "rust-salsa20")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "salsa20" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04w211x17xzny53f83p8f7cj7k2hi8zck282q5aajwqzydd2z8lp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs
       (("rust-cipher" ,rust-cipher-0.4)
        ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/stream-ciphers")
    (synopsis "Salsa20 Stream Cipher")
    (description "Salsa20 is a collection of stream cipher algorithms written
in pure Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-salsa20-0.9
  (package
    (inherit rust-salsa20-0.10)
    (name "rust-salsa20")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "salsa20" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11i646kpgimimqiq8hyi0b7ngp588f7nl9xsc317d9kdcxgvn3qc"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.3)
        ("rust-zeroize" ,rust-zeroize-1))))))

(define-public rust-scrypt-0.11
  (package
    (name "rust-scrypt")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "scrypt" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07zxfaqpns9jn0mnxm7wj3ksqsinyfpirkav1f7kc2bchs2s65h5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-password-hash" ,rust-password-hash-0.5)
        ("rust-pbkdf2" ,rust-pbkdf2-0.12)
        ("rust-salsa20" ,rust-salsa20-0.10)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-password-hash" ,rust-password-hash-0.5))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/scrypt")
    (synopsis "Scrypt password-based key derivation function")
    (description
     "This package provides a Scrypt password-based key derivation
function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-scrypt-0.10
  (package
    (inherit rust-scrypt-0.11)
    (name "rust-scrypt")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "scrypt" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pglmppcl8mdzfxdv2x9dsjrwxhc1bm9zvxjibnlv59jnv9297lz"))))
    (arguments
     `(#:cargo-inputs
       (("rust-hmac" ,rust-hmac-0.12)
        ("rust-password-hash" ,rust-password-hash-0.4)
        ("rust-pbkdf2" ,rust-pbkdf2-0.11)
        ("rust-salsa20" ,rust-salsa20-0.10)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-password-hash" ,rust-password-hash-0.4))))))

(define-public rust-scrypt-0.8
  (package
    (inherit rust-scrypt-0.11)
    (name "rust-scrypt")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "scrypt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09fkz5sc7qx97dyi1nkv69z36diggd2c9mja33cxpsqicdy6sgg7"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-hmac" ,rust-hmac-0.12)
        ("rust-password-hash" ,rust-password-hash-0.3)
        ("rust-pbkdf2" ,rust-pbkdf2-0.10)
        ("rust-salsa20" ,rust-salsa20-0.9)
        ("rust-sha2" ,rust-sha2-0.10))))))

(define-public rust-sec1-0.7
  (package
    (name "rust-sec1")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sec1" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1p273j8c87pid6a1iyyc7vxbvifrw55wbxgr0dh3l8vnbxb7msfk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base16ct" ,rust-base16ct-0.2)
        ("rust-der" ,rust-der-0.7)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-pkcs8" ,rust-pkcs8-0.10)
        ("rust-serdect" ,rust-serdect-0.2)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/sec1")
    (synopsis
     "Rust implementation of SEC1: Elliptic Curve Cryptography encoding formats")
    (description
     "This package provides a pure Rust implementation of SEC1: Elliptic Curve
Cryptography encoding formats including ASN.1 DER-serialized private keys as
well as the Elliptic-Curve-Point-to-Octet-String encoding.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-secp256k1-sys-0.4
  (package
    (name "rust-secp256k1-sys")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "secp256k1-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0dk0as7qdlvg5vkcsihndzg1jgqb9amhwmz3xiip94fy7ibs4zcm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cc" ,rust-cc-1))
        #:cargo-development-inputs
        (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/rust-bitcoin/rust-secp256k1/")
    (synopsis "FFI for Pieter Wuille's @code{libsecp256k1} library")
    (description "This package is a Rust FFI for Pieter Wuille's
@code{libsecp256k1} library.")
    (license license:cc0)))

(define-public rust-serdect-0.2
  (package
    (name "rust-serdect")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serdect" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xw1b6acw6nd0jchzyxzr97f0s4shbcqh92iyjwln0cskshi8kx8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base16ct" ,rust-base16ct-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-ciborium" ,rust-ciborium-0.2)
        ("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json-core" ,rust-serde-json-core-0.5)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-toml" ,rust-toml-0.7))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/serdect")
    (synopsis "Constant-time serde serializer/deserializer helpers")
    (description
     "This package provides constant-time serde serializer/deserializer helpers
for data that potentially contains secrets (e.g. cryptographic keys).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-serdect-0.1
  (package
    (inherit rust-serdect-0.2)
    (name "rust-serdect")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serdect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b6krqs77vzwzdjcrcywlmlwd3msfpgmkkbxx8q9njypyhdwx3q3"))))
    (arguments
     `(#:cargo-inputs (("rust-base16ct" ,rust-base16ct-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-ciborium" ,rust-ciborium-0.2)
                                   ("rust-hex-literal" ,rust-hex-literal-0.3)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json-core" ,rust-serde-json-core-0.4)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-toml" ,rust-toml-0.5))))))

(define-public rust-sha-1-0.10
  (package
    (name "rust-sha-1")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha-1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1700fs5aiiailpd5h0ax4sgs2ngys0mqf3p4j0ry6j2p2zd8l1gm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-sha1-asm" ,rust-sha1-asm-0.5))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "SHA-1 hash function")
    (description "This crate provides a SHA-1 hash function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sha-1-0.9
  (package
    (inherit rust-sha-1-0.10)
    (name "rust-sha-1")
    (version "0.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha-1" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "19jibp8l9k5v4dnhj5kfhaczdfd997h22qz0hin6pw9wvc9ngkcr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.9)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3)
        ("rust-sha1-asm" ,rust-sha1-asm-0.5))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-sha1-0.10
  (package
    (name "rust-sha1")
    (version "0.10.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-sha1-asm" ,rust-sha1-asm-0.5))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/mitsuhiko/rust-sha1")
    (synopsis "Minimal implementation of SHA1 for Rust")
    (description
     "This package provides a minimal implementation of SHA1 for Rust.")
    (license license:bsd-3)))

(define-public rust-sha1-0.6
  (package
    (inherit rust-sha1-0.10)
    (name "rust-sha1")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w1p0s9060cv1vlgfa5c93kjksmvzjjc8j780lns3jj5fk4hbnn1"))))
    (arguments
     `(#:cargo-inputs (("rust-sha1-smol" ,rust-sha1-smol-1))))))

(define-public rust-sha1-0.2
  (package
    (inherit rust-sha1-0.6)
    (name "rust-sha1")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p09zfhd27z6yr5in07gfjcx345010rw51ivlcf14364x3hv2c6c"))
       (modules '((guix build utils)))
       (snippet #~(substitute* "Cargo.toml"
                    ((", path =.*}") "}")))))
    (arguments
     `(#:tests? #f  ; Tests require openssl-1.0
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.7)
        ("rust-rand" ,rust-rand-0.3))))))

(define-public rust-sha1-asm-0.5
  (package
    (name "rust-sha1-asm")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha1-asm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0asqxlxf5li7ac9mi49qj890rzsfb5px5ynzmqq12z5nz2xcwsi8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/RustCrypto/asm-hashes")
    (synopsis "Assembly implementation of SHA-1 compression function")
    (description
     "Assembly implementation of SHA-1 compression function.")
    (license license:expat)))

(define-public rust-sha1-smol-1
  (package
    (name "rust-sha1-smol")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha1_smol" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pbh2xjfnzgblws3hims0ib5bphv7r5rfdpizyh51vnzvnribymv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-openssl" ,rust-openssl-0.10)
                                   ("rust-rand" ,rust-rand-0.4)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/mitsuhiko/sha1-smol")
    (synopsis "Dependency free SHA1 implementation")
    (description
     "This package provides a minimal dependency free implementation of
SHA1 for Rust.")
    (license license:bsd-3)))

(define-public rust-sha1collisiondetection-0.3
  (package
    (name "rust-sha1collisiondetection")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha1collisiondetection" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jwnwrk1x78v9r16jnac2s4v1m2f5a19khwkx1vjh0d6whhn8q0z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-mangen" ,rust-clap-mangen-0.2)
                       ("rust-clap-mangen" ,rust-clap-mangen-0.2)
                       ("rust-const-oid" ,rust-const-oid-0.9)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-generic-array" ,rust-generic-array-0.12))
       #:cargo-development-inputs (("rust-getrandom" ,rust-getrandom-0.2)
                                   ("rust-hex-literal" ,rust-hex-literal-0.4)
                                   ("rust-sha1" ,rust-sha1-0.10))))
    (home-page "https://gitlab.com/sequoia-pgp/sha1collisiondetection")
    (synopsis "SHA-1 hash function with collision detection and mitigation")
    (description
      "This package implementation of the SHA-1 cryptographic hash algorithm.

This is a port of Marc Stevens' sha1collisiondetection algorithm to Rust.  The
code is translated from C to Rust using c2rust.")
    (license license:expat)))

(define-public rust-sha1collisiondetection-0.2
  (package
    (inherit rust-sha1collisiondetection-0.3)
    (name "rust-sha1collisiondetection")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha1collisiondetection" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "13mkl29ma6sqybzvr8dqpj0f697d1xk1d0a39kdcgcihhg7r61xj"))))
    (arguments
     `(#:cargo-inputs (("rust-digest" ,rust-digest-0.9)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-structopt" ,rust-structopt-0.3))
       #:cargo-development-inputs (("rust-getrandom" ,rust-getrandom-0.2)
                                   ("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-sha2-0.10
  (package
    (name "rust-sha2")
    (version "0.10.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-sha2-asm" ,rust-sha2-asm-0.6))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "SHA-2 hash functions")
    (description
     "This package provides a pure Rust implementation of the SHA-2 hash
function family including SHA-224, SHA-256, SHA-384, and SHA-512.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sha2-0.9
  (package
    (inherit rust-sha2-0.10)
    (name "rust-sha2")
    (version "0.9.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "006q2f0ar26xcjxqz8zsncfgz86zqa5dkwlwv03rhx1rpzhs2n2d"))))
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.9)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3)
        ("rust-sha2-asm" ,rust-sha2-asm-0.6))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-sha2-0.8
  (package
    (inherit rust-sha2-0.9)
    (name "rust-sha2")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s9yddvyg6anaikdl86wmwfim25c0d4m0xq0y2ghs34alxpg8mm2"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--lib" "--bins" "--tests")
       #:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.7)
        ("rust-digest" ,rust-digest-0.8)
        ("rust-fake-simd" ,rust-fake-simd-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2)
        ("rust-sha2-asm" ,rust-sha2-asm-0.5))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.8)
        ("rust-hex-literal" ,rust-hex-literal-0.1))))))

(define-public rust-sha2-asm-0.6
  (package
    (name "rust-sha2-asm")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha2-asm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ay1vai08d802avl41r0s6r1nrcnzv7jnj5xna34d03mc56j2idq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1))))       ;build dependency
    (home-page "https://github.com/RustCrypto/asm-hashes")
    (synopsis "Assembly implementation of SHA-2")
    (description "This package provides an assembly implementations of hash
functions core functionality.")
    (license license:expat)))

(define-public rust-sha2-asm-0.5
  (package
    (inherit rust-sha2-asm-0.6)
    (name "rust-sha2-asm")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha2-asm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "192xi35b9qp2ph4fv584z8gy8mr9bsxkbfvb9q9z40k5pqjz5hm7"))))
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1))))))

(define-public rust-sha256-1
  (package
    (name "rust-sha256")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha256" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c35j1z4inpz7fwa6vy0xf3arffz5mykyj8nlc50g8sgj5m8y9qq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://github.com/baoyachi/sha256-rs")
    (synopsis "Sha256 crypto digest")
    (description "This package provides the sha256 crypto digest in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sha3-0.10
  (package
    (name "rust-sha3")
    (version "0.10.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha3" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q5s3qlwnk8d5j34jya98j1v2p3009wdmnqdza3yydwgi8kjv1vm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-keccak" ,rust-keccak-0.1))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "SHA-3 (Keccak) hash function")
    (description "This package provides a pure Rust implementation of the SHA-3
(Keccak) hash function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sha3-0.9
  (package
    (inherit rust-sha3-0.10)
    (name "rust-sha3")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha3" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "02d85wpvz75a0n7r2da15ikqjwzamhii11qy9gqf6pafgm0rj4gq"))))
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.9)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-keccak" ,rust-keccak-0.1)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-signature-2
  (package
    (name "rust-signature")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "signature" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-signature-derive" ,rust-signature-derive-2))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://github.com/RustCrypto/traits/tree/master/signature")
    (synopsis
     "Traits for cryptographic signature algorithms (e.g. ECDSA, Ed25519)")
    (description
     "This package contains traits which provide generic, object-safe APIs
for generating and verifying digital signatures.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-signature-1
  (package
    (inherit rust-signature-2)
    (name "rust-signature")
    (version "1.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "signature" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z3xg405pg827g6hfdprnszsdqkkbrsfx7f1dl04nv9g7cxks8vl"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-signature-derive" ,rust-signature-derive-1))))))

(define-public rust-signature-derive-2
  (package
    (name "rust-signature-derive")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "signature_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11h4z3bql9pzj0mf7bv30q9c3rldk9n03520pk3z9siyj78q20xb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/signature/derive")
    (synopsis "Custom derive support for the 'signature' crate")
    (description "This package provides proc macros used by the signature
crate.

It's not intended to be used directly.  See the signature crate's documentation
for additional details.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-signature-derive-1
  (package
    (inherit rust-signature-derive-2)
    (name "rust-signature-derive")
    (version "1.0.0-pre.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "signature_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03wj342zvljknqwg3qbc9acrcsrzhdp1d2d6pfrh4p1b087k3rln"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-synstructure" ,rust-synstructure-0.12))))))

(define-public rust-simple-asn1-0.6
  (package
    (name "rust-simple-asn1")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "simple_asn1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11d0l3l7lppzr1wxhvsbmjmw6s2vy3v7b8ygz500z4di9qhfbi5d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/acw/simple_asn1")
    (synopsis "Simple DER/ASN.1 encoding/decoding library")
    (description
     "This package provides a simple DER/ASN.1 encoding/decoding library.")
    (license license:isc)))

(define-public rust-simple-asn1-0.4
  (package
    (inherit rust-simple-asn1-0.6)
    (name "rust-simple-asn1")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "simple_asn1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jxy9as8nj65c2n27j843g4fpb95x4fjz31w6qx63q3wwlys2b39"))))
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.2)
                       ("rust-num-traits" ,rust-num-traits-0.2))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-0.7)
                                   ("rust-rand" ,rust-rand-0.5))))))

(define-public rust-sm3-0.4
  (package
    (name "rust-sm3")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sm3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q2qn4d7qhd8v5grr0xdq9jv3likcr2i8nnqqhxy79yh0avs7fgb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-digest" ,rust-digest-0.10))
       #:cargo-development-inputs (("rust-digest" ,rust-digest-0.10)
                                   ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "SM3 (OSCCA GM/T 0004-2012) hash function")
    (description
     "This package provides the SM3 (OSCCA GM/T 0004-2012) hash function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-spki-0.7
  (package
    (name "rust-spki")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "spki" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17fj8k5fmx4w9mp27l970clrh5qa7r5sjdvbsln987xhb34dc7nr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-arbitrary" ,rust-arbitrary-1)
        ("rust-base64ct" ,rust-base64ct-1)
        ("rust-der" ,rust-der-0.7)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/spki")
    (synopsis
     "X.509 Subject Public Key Info (RFC5280) describing public keys")
    (description
     "This package provides X.509 Subject Public Key Info (RFC5280)
describing public keys as well as their associated AlgorithmIdentifiers (i.e.
OIDs)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-spki-0.6
  (package
    (inherit rust-spki-0.7)
    (name "rust-spki")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "spki" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ar1ldkl7svp8l3gfw2hyiiph7n2nqynjnjgdv1pscvsmjxh5kv7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64ct" ,rust-base64ct-1)
        ("rust-der" ,rust-der-0.6)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-spki-0.5
  (package
    (inherit rust-spki-0.7)
    (name "rust-spki")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "spki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09qaddm4kw01xm9638910bm4yqnshzh2p38lvc3kxkvc5b01ml24"))))
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-der" ,rust-der-0.5)
                       ("rust-sha2" ,rust-sha2-0.9))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3))))))

(define-public rust-spki-0.4
  (package
    (inherit rust-spki-0.7)
    (name "rust-spki")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "spki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ckgkcg6db5y94dqhmyikgn8yrsah6pyf4j197hv1c51bp0s00aw"))))
    (arguments `(#:skip-build? #t #:cargo-inputs (("rust-der" ,rust-der-0.4))))))

(define-public rust-ssh-cipher-0.2
  (package
    (name "rust-ssh-cipher")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ssh-cipher" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kvq113x9fcy2jcxp00xk472zxm8d9zxxz2vyqx3rlzh88ki7b6a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-aes-gcm" ,rust-aes-gcm-0.10)
                       ("rust-cbc" ,rust-cbc-0.1)
                       ("rust-chacha20" ,rust-chacha20-0.9)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-ctr" ,rust-ctr-0.9)
                       ("rust-des" ,rust-des-0.8)
                       ("rust-poly1305" ,rust-poly1305-0.8)
                       ("rust-ssh-encoding" ,rust-ssh-encoding-0.2)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/SSH/tree/master/ssh-cipher")
    (synopsis "Pure Rust implementation of SSH symmetric encryption")
    (description
     "This package provides Pure Rust implementation of SSH symmetric encryption
including support for the modern aes128-gcm@@openssh.com/aes256-gcm@@openssh.com
and chacha20-poly1305@@openssh.com algorithms as well as legacy support for
older ciphers.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ssh-encoding-0.2
  (package
    (name "rust-ssh-encoding")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ssh-encoding" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05aavlhk68vm60vbw8lcgx1p5wry367ck8niij7af221xywl54pb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.7)
                       ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.4))))
    (home-page "https://github.com/RustCrypto/SSH/tree/master/ssh-encoding")
    (synopsis "Pure Rust implementation of SSH data type decoders/encoders")
    (description
     "This package provides Pure Rust implementation of SSH data type
decoders/encoders as described in RFC4251.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ssh-key-0.6
  (package
    (name "rust-ssh-key")
    (version "0.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ssh-key" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hx8as8rvnk31ncqg7dlqgcw9bmngkznn3xamf6d010ggwlzb1iv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bcrypt-pbkdf" ,rust-bcrypt-pbkdf-0.10)
                       ("rust-dsa" ,rust-dsa-0.6)
                       ("rust-ed25519-dalek" ,rust-ed25519-dalek-2)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-p256" ,rust-p256-0.13)
                       ("rust-p384" ,rust-p384-0.13)
                       ("rust-p521" ,rust-p521-0.13)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rsa" ,rust-rsa-0.9)
                       ("rust-sec1" ,rust-sec1-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-ssh-cipher" ,rust-ssh-cipher-0.2)
                       ("rust-ssh-encoding" ,rust-ssh-encoding-0.2)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.4)
                                   ("rust-rand-chacha" ,rust-rand-chacha-0.3))))
    (home-page "https://github.com/RustCrypto/SSH/tree/master/ssh-key")
    (synopsis "Pure Rust implementation of SSH key file format decoders/encoders")
    (description
     "This package provides Pure Rust implementation of SSH key file format
decoders/encoders as described in RFC4251/RFC4253 and @code{OpenSSH} key
formats, as well as \"sshsig\" signatures and certificates (including
certificate validation and certificate authority support), with further support
for the `authorized_keys` and `known_hosts` file formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-stream-cipher-0.4
  (package
    (name "rust-stream-cipher")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stream-cipher" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "120y04k3d2jyfnvyrlf38x6bf0yckyk30c7zf8v8qaq4fjcyvy09"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-block-cipher" ,rust-block-cipher-0.7)
        ("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Stream cipher traits")
    (description "This package provides stream cipher traits.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stream-cipher-0.3.2-yanked
  (package
    (inherit rust-stream-cipher-0.4)
    (name "rust-stream-cipher")
    (version "0.3.2") ; This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stream-cipher" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32 "1333qng84n6b15p8kndhajlgvbp1rgdddx04xgsvrjlnb1m2acc1"))))
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-generic-array" ,rust-generic-array-0.14))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-stream-cipher-0.3
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-stream-cipher" rust-stream-cipher-0.3.2-yanked))

(define-public rust-streebog-0.10
  (package
    (name "rust-streebog")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "streebog" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dnm1f3bkm8rvskvl3cvhh1f2nbrpckr8c3hw1hc7kj2ibnyczwy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.10))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "Streebog (GOST R 34.11-2012) hash function")
    (description
     "This package provides a streebog (GOST R 34.11-2012) hash function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-streebog-0.9
  (package
    (inherit rust-streebog-0.10)
    (name "rust-streebog")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "streebog" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0lz7ajfqdqbrnj01m1xc01ch1g0s9391ma36qqkiyf1074d1r8nr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.9)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-subtle-2
  (package
    (name "rust-subtle")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "subtle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://dalek.rs/")
    (synopsis
     "Pure-Rust traits and utilities for cryptographic implementations")
    (description
     "This package provides Pure-Rust traits and utilities for constant-time
cryptographic implementations.")
    (license license:bsd-3)))

(define-public rust-subtle-1
  (package
    (inherit rust-subtle-2)
    (name "rust-subtle")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "subtle" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vm80mxbwfj334izwm8x8l65v1xl9hr0kwrg36r1rq565fkaarrd"))))))

(define-public rust-subtle-ng-2
  (package
    (name "rust-subtle-ng")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "subtle-ng" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hj1wp8xl64bjhbvlfffmllqy7wdw2b505f32gn3qqic4vmpcikk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-rand" ,rust-rand-0.7))))
    (home-page "https://dalek.rs/")
    (synopsis "Pure-Rust for constant-time cryptographic implementations")
    (description
     "This package provides pure-Rust traits and utilities for constant-time
cryptographic implementations.")
    (license license:bsd-3)))

(define-public rust-tiger-0.1
  (package
    (name "rust-tiger")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiger" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01bhc7h8kxc5kjqx9sqrb3g8h4f9av6hpxzyihjq7pprphf56gj4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.9)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-digest" ,rust-digest-0.9))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "Tiger hash function")
    (description
     "This package provides the Tiger cryptographic hash function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tls-codec-0.4
  (package
    (name "rust-tls-codec")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tls_codec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0spv5d8gjpmil4x14d8jk6wps59r4y7kdj77par8b30g6ff8rrxm"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("\"= ?([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tls-codec-derive" ,rust-tls-codec-derive-0.4)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-anstyle" ,rust-anstyle-1)
        ("rust-anstyle-parse" ,rust-anstyle-parse-0.2)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-lex" ,rust-clap-lex-0.5)
        ("rust-criterion" ,rust-criterion-0.5)
        ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/tls_codec")
    (synopsis "Rust implementation of TLS (de)serialization")
    (description
     "This package provides a Rust implementation of TLS serialization
and deserialization.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-tls-codec-derive-0.4
  (package
    (name "rust-tls-codec-derive")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tls_codec_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1704w8zpgpj40yjgq9dddnnfzmq44p63n0606c1g6y8fcm2zb7ld"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `tls_codec`
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-trybuild" ,rust-trybuild-1))))
    (home-page
     "https://github.com/RustCrypto/formats/tree/master/tls_codec/derive")
    (synopsis "Derive macros for the tls_codec trait")
    (description
      "This package provides Derive macros for the tls_codec trait.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-totp-lite-2
  (package
    (name "rust-totp-lite")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "totp-lite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hvnpv7nl79jp96w6g2j7l6xskl5qlx3h0qqf9zry68pvcs33r7q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.10)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs
       (("rust-koibumi-base32" ,rust-koibumi-base32-0.0.2)
        ("rust-version-sync" ,rust-version-sync-0.9))))
    (home-page "https://github.com/fosskers/totp-lite")
    (synopsis "Simple, correct TOTP library")
    (description "Rust-totp-lite provides a simple, correct time-based
One-Time Password library.")
    (license license:expat)))

(define-public rust-twofish-0.7
  (package
    (name "rust-twofish")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "twofish" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04w0ii2c0c9ws08aw6c7illh9zql22il9lbwjk1mgir30aiq73m7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs
       (("rust-cipher" ,rust-cipher-0.4)
        ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Twofish block cipher")
    (description "Twofish block cipher")
    (license (list license:expat license:asl2.0))))

(define-public rust-universal-hash-0.5
  (package
    (name "rust-universal-hash")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "universal-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sh79x677zkncasa95wz05b36134822w6qxmi1ck05fwi33f47gw"))
       (snippet
        #~(begin (use-modules (guix build utils))
           (substitute* "Cargo.toml"
             (("=2\\.4\\.1") "^2.4.1"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-common" ,rust-crypto-common-0.1)
        ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Trait for universal hash functions")
    (description "This package provides traits for universal hash functions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-universal-hash-0.4
  (package
    (inherit rust-universal-hash-0.5)
    (name "rust-universal-hash")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "universal-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01av09i0rqcl8f0xgvn2g07kzyafgbiwdhkfwq0m14kyd67lw8cz"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "Cargo.toml"
                   (("\"= ?([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                    (string-append "\"^" version)))))))
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-subtle" ,rust-subtle-2))))))

(define-public rust-x25519-dalek-2
  (package
    (name "rust-x25519-dalek")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xyjgqpsa0q6pprakdp58q1hy45rf8wnqqscgzx0gyw13hr6ir67"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-curve25519-dalek" ,rust-curve25519-dalek-4)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-rand-core" ,rust-rand-core-0.6))))
    (home-page "https://doc.dalek.rs/x25519_dalek")
    (synopsis "X25519 elliptic curve Diffie-Hellman key exchange")
    (description
     "This crate provides a Rust implementation of x25519 elliptic curve
Diffie-Hellman key exchange, with curve operations provided by
@code{curve25519-dalek}.")
    (license license:bsd-3)))

(define-public rust-x25519-dalek-1
  (package
    (inherit rust-x25519-dalek-2)
    (name "rust-x25519-dalek")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xz0m1pczss9r25d1r52420dl2picdypbcn5ycmlwssp9awvd4i3"))
       (modules '((guix build utils)))
       (snippet '(substitute* "Cargo.toml"
                   (("version = \"=1.3\"") "version = \"^1.3\"")))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-curve25519-dalek" ,rust-curve25519-dalek-3)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-zeroize" ,rust-zeroize-1))))))

(define-public rust-x25519-dalek-ng-1
  (package
    (name "rust-x25519-dalek-ng")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x25519-dalek-ng" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09n35vgrryjy0m6ascfaykc8s0i517rzgj64qdq2jrlri7g78w5z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-curve25519-dalek-ng" ,rust-curve25519-dalek-ng-4)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-serde" ,rust-serde-1)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://dalek.rs/")
    (synopsis "Fork of x25519-dalek")
    (description "This package provides a fork x25519-dalek, with an updated
rand_core.")
    (license license:bsd-3)))

(define-public rust-xsalsa20poly1305-0.9
  (package
    (name "rust-xsalsa20poly1305")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xsalsa20poly1305" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ixnzy6srqk9gkxyh2rrwhpvnc0v3z3gfxgfg36q2zsnaz9xm9h2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aead" ,rust-aead-0.5)
                       ("rust-poly1305" ,rust-poly1305-0.8)
                       ("rust-salsa20" ,rust-salsa20-0.10)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/AEADs")
    (synopsis "DEPRECATED: please use the `crypto_secretbox` crate")
    (description
     "DEPRECATED: please use the `crypto_secretbox` crate.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-z85-3
  (package
    (name "rust-z85")
    (version "3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "z85" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z10407jwvjfzpzaxwxgqsm9vcbyldzzh2qz2b0ijy2h3fprsn9a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/decafbad/z85")
    (synopsis
     "Rust implementation of ZeroMQ's Z85 encoding mechanism with padding")
    (description
     "This package provides a Rust implementation of @code{ZeroMQ's} Z85
encoding mechanism with padding.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zipsign-api-0.1
  (package
    (name "rust-zipsign-api")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zipsign-api" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z30vzhhhd1va9z7ksdw8x8f6y8jb200h2ryk85wvnx9mm3aa4v4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.22)
                       ("rust-ed25519-dalek" ,rust-ed25519-dalek-2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-zip" ,rust-zip-2))))
    (home-page "https://github.com/Kijewski/zipsign")
    (synopsis "Sign and verify zip files with an ed25519 signing key")
    (description
     "This package lets you sign and verify `.zip` and `.tar.gz` files
using an ed25519 signing key.")
    (license (list license:asl2.0))))
