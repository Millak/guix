;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-aes-0.8
  (package
    (name "rust-aes")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qi7z96wf3zd6alg116nh2myp34bw2574jwly4zrhpz9k19887xc"))))
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
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cipher" ,rust-cipher-0.3)
        ("rust-cpufeatures" ,rust-cpufeatures-0.2)
        ("rust-ctr" ,rust-ctr-0.8)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3))))))

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

(define-public rust-aes-0.3
  (package
    (inherit rust-aes-0.4)
    (name "rust-aes")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j90iwpax0y1dqq14i8y9xgpcnnlgnljwkxg3mhzrralwf7ivssl"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-aes-soft" ,rust-aes-soft-0.3)
        ("rust-aesni" ,rust-aesni-0.6)
        ("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6))))))

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
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-gcm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z2429v2d2wyf809h2wc4vwwibwypz3y4p7sn4kzkjb91ip3dqc2"))))
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

(define-public rust-aes-gcm-0.6
  (package
    (inherit rust-aes-gcm-0.8)
    (name "rust-aes-gcm")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-gcm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lga8my3zlc0b1nhcpc1hrbykfm014fqs6d64bwrjqii05w01xc6"))))
    (arguments
     `(#:cargo-inputs
       (("rust-aead" ,rust-aead-0.3)
        ("rust-aes" ,rust-aes-0.4)
        ("rust-block-cipher" ,rust-block-cipher-0.7)
        ("rust-ghash" ,rust-ghash-0.3)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-criterion-cycles-per-byte"
         ,rust-criterion-cycles-per-byte-0.1)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))))

(define-public rust-aes-gcm-0.5
  (package
    (inherit rust-aes-gcm-0.6)
    (name "rust-aes-gcm")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-gcm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f66b5bmyj38r1hj55wzamlzw3y1aql34lgwr2vxn93073d6njl3"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-aead" ,rust-aead-0.2)
        ("rust-aes" ,rust-aes-0.3)
        ("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6)
        ("rust-ghash" ,rust-ghash-0.2)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))))))

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

(define-public rust-aes-soft-0.4
  (package
    (inherit rust-aes-soft-0.6)
    (name "rust-aes-soft")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-soft" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "19szsg0qqxq42k7bj5p3svb147n8wxy9a20n4g7mcl2fwrz689a9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-block-cipher" ,rust-block-cipher-0.7)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2))
       #:cargo-development-inputs
       (("rust-block-cipher" ,rust-block-cipher-0.7))))))

(define-public rust-aes-soft-0.3
  (package
    (inherit rust-aes-soft-0.4)
    (name "rust-aes-soft")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-soft" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "039si7yjp0wcd750sgq52c60sh2ikaxwd7rq7g0ba7ws7ypfgmyg"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2))))))

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

(define-public rust-aesni-0.7
  (package
    (inherit rust-aesni-0.10)
    (name "rust-aesni")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aesni" version))
       (file-name (string-append name "-" version ".tar.gz"))
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
        ("rust-stream-cipher" ,rust-stream-cipher-0.4))))))

(define-public rust-aesni-0.6
  (package
    (inherit rust-aesni-0.7)
    (name "rust-aesni")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aesni" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "007imgcfl82nilfpamj5dik83pkcmkzvbkxp384p7r3iz6sscw1g"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2)
        ("rust-stream-cipher" ,rust-stream-cipher-0.3))))))

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
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "blake2b_simd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1g04mc4gf6jyymyj41749jhhplm3ymnc6z7rhkc1fqwclv4hsbrw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-arrayref" ,rust-arrayref-0.3)
        ("rust-arrayvec" ,rust-arrayvec-0.7)
        ("rust-constant-time-eq" ,rust-constant-time-eq-0.2))))
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
    (version "0.5.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blake2b_simd" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "12icvk8ixlivv3jv5nyrg01sajp4s279zb1kmif0nfja4ms2vyyq"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
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

(define-public rust-botan-0.10
  (package
    (name "rust-botan")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "botan" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vzl5pdysh848zpphsgvj9c40zdi3ynl32zzixsd8vg4vaflhb49"))))
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

(define-public rust-botan-sys-0.10
  (package
    (name "rust-botan-sys")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "botan-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cbjr44gc5dhmgl43sfiqzbsma4anfi3h26m4yzsli23yd1lmyf8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-botan-src" ,rust-botan-src-0.21903))))
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

(define-public rust-botan-src-0.21903
  (package
    (name "rust-botan-src")
    (version "0.21903.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "botan-src" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19fhll4g0v8hbyjxg8c790l9ln5xgf4r6xdcnw438mpy81hvrdxy"))
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
    (inherit rust-botan-src-0.21903)
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

(define-public rust-c2-chacha-0.2
  (package
    (name "rust-c2-chacha")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2-chacha" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "00a11qdc8mg3z0k613rhprkc9p6xz0y7b1681x32ixg0hr3x0r3x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-ppv-lite86" ,rust-ppv-lite86-0.2)
        ("rust-stream-cipher" ,rust-stream-cipher-0.3))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/cryptocorrosion/cryptocorrosion")
    (synopsis "The ChaCha family of stream ciphers")
    (description
     "The ChaCha family of stream ciphers.")
    (license (list license:asl2.0 license:expat))))

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

(define-public rust-crypto-bigint-0.5
  (package
    (name "rust-crypto-bigint")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crypto-bigint" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05gy7sqkxg65bj1wrgq1pbh8iwn1kmfysvzx1g22p4gx3972yk6g"))))
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
        ("rust-criterion" ,rust-criterion-0.4)
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
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-mac" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ghh3qmjf7hv580zqdk4yrbg99v57jx773zb7lzi7j4hj24bdyi5"))))
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
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto-mac" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "19iyh7h9qaqrv29dhbd31rm6pq023ry78nw7jwr3qjy3l22zsms8"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
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

(define-public rust-crypto-mac-0.5
  (package
    (inherit rust-crypto-mac-0.10)
    (name "rust-crypto-mac")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-mac" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n6r10zlnfv9gbjj0380sxfffxhq1khfjqwsn7fx8iil9pzv9689"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-constant-time-eq" ,rust-constant-time-eq-0.1)
        ("rust-generic-array" ,rust-generic-array-0.9))))))

(define-public rust-crypto-mac-0.4
  (package
    (name "rust-crypto-mac")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-mac" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "160ixpghhz5kz16f38kzcyv6lx8wmi4cgbhlhq4nazf678iib43p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-constant-time-eq" ,rust-constant-time-eq-0.1)
        ("rust-generic-array" ,rust-generic-array-0.8))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Trait for Message Authentication Code (MAC) algorithms")
    (description "This package provides traits for Message Authentication
Code (MAC) algorithms.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-crypto-hash-0.3
  (package
    (name "rust-crypto-hash")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-hash" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jnxgpk0j29hzcv42viq5dckyfjnxdjsar55366j95zx80i1cxwa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-commoncrypto" ,rust-commoncrypto-0.2)
        ("rust-hex" ,rust-hex-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-winapi" ,rust-winapi-0.3))))
    (inputs
     (list openssl))
    (home-page "https://github.com/malept/crypto-hash")
    (synopsis "Wrapper for OS-level cryptographic hash functions")
    (description "This package provides a wrapper for OS-level cryptographic
hash functions.")
    (license license:expat)))

(define-public rust-curve25519-dalek-3
  (package
    (name "rust-curve25519-dalek")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "curve25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q8v97275cy6v4ly6y2qwv9a8phnpjg9sy8kv7r6mgdjfacxz7qb"))))
    (build-system cargo-build-system)
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
        ("rust-sha2" ,rust-sha2-0.9))))
    (home-page "https://dalek.rs/curve25519-dalek")
    (synopsis "Group operations on ristretto255 and Curve25519")
    (description
     "This package provides a pure-Rust implementation of group operations on
ristretto255 and Curve25519.")
    (license license:bsd-3)))

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

(define-public rust-digest-0.7
  (package
    (inherit rust-digest-0.9)
    (name "rust-digest")
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "digest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "142vdpvkqlqk9s1dcgpqb2wz76n5a39gjnk68p0zkflc58j75c03"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-generic-array" ,rust-generic-array-0.9))))))

(define-public rust-digest-0.6
  (package
    (name "rust-digest")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "digest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "02mgf8z4hi96w9nl2zb5w3k6lqbhjgv5z8hhyv2b7x7kavqrpcp5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array-0.8))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for cryptographic hash functions")
    (description "This package provides traits for cryptographic hash
functions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-elliptic-curve-0.13
  (package
    (name "rust-elliptic-curve")
    (version "0.13.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "elliptic-curve" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rqn7yq9rgfs7r0dcj4phxf9hqmw2alfxa0lciamsbkz6sm1xivm"))))
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

(define-public rust-elliptic-curve-0.12
  (package
    (inherit rust-elliptic-curve-0.13)
    (name "rust-elliptic-curve")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "elliptic-curve" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lwi108mh6drw5nzqzlz7ighdba5qxdg5vmwwnw1j2ihnn58ifz7"))))
    (arguments
     `(#:cargo-inputs (("rust-base16ct" ,rust-base16ct-0.1)
                       ("rust-base64ct" ,rust-base64ct-1)
                       ("rust-crypto-bigint" ,rust-crypto-bigint-0.4)
                       ("rust-der" ,rust-der-0.6)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-ff" ,rust-ff-0.12)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-group" ,rust-group-0.12)
                       ("rust-hex-literal" ,rust-hex-literal-0.3)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.6)
                       ("rust-pkcs8" ,rust-pkcs8-0.9)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-sec1" ,rust-sec1-0.3)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serdect" ,rust-serdect-0.1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3)
                                   ("rust-sha2" ,rust-sha2-0.10)
                                   ("rust-sha3" ,rust-sha3-0.10))))))

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

(define-public rust-hkdf-0.9
  (package
    (inherit rust-hkdf-0.11)
    (name "rust-hkdf")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hkdf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jdvmf8aadk3s0kn9kk3dj00nprjk9glks5f8dm55r43af34j4gy"))))
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hmac" ,rust-hmac-0.8))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-crypto-tests" ,rust-crypto-tests-0.5)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sha2" ,rust-sha2-0.9))))))

(define-public rust-hkdf-0.8
  (package
    (inherit rust-hkdf-0.9)
    (name "rust-hkdf")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hkdf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qzsmqrvcmgnrb109qr2mvsmr5c4psm1702vrpcqnj02c408m81z"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-digest" ,rust-digest-0.8)
        ("rust-hmac" ,rust-hmac-0.7))))))

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

(define-public rust-hmac-0.8
  (package
    (inherit rust-hmac-0.11)
    (name "rust-hmac")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0h48wc7iysh4xd6ci4prh8bb7nszijrh9w3blaaq8a6cilk8hs0j"))))
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.8)
        ("rust-digest" ,rust-digest-0.9))
       #:cargo-development-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.8)
        ("rust-md-5" ,rust-md-5-0.9)
        ("rust-sha2" ,rust-sha2-0.9))))))

(define-public rust-hmac-0.7
  (package
    (inherit rust-hmac-0.8)
    (name "rust-hmac")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15cnwpssp2n1kdm9x7abir67f2hp3q6rdfj1mcck3hm4rmj5xjsx"))))
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.7)
        ("rust-digest" ,rust-digest-0.8))
       #:cargo-development-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.7)
        ("rust-md-5" ,rust-md-5-0.8)
        ("rust-sha2" ,rust-sha2-0.8))))))

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

(define-public rust-nettle-7
  (package
    (name "rust-nettle")
    (version "7.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nettle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dk36l90p79c3xgmrzp8489h8dfaal0jzaid1n8n3cg7xbrwrzdr"))))
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

(define-public rust-nettle-5
  (package
    (inherit rust-nettle-7)
    (version "5.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nettle" version))
       (file-name
        (string-append (package-name rust-nettle-7) "-" version ".tar.gz"))
       (sha256
        (base32 "0zfplqdf3mag8r7lc124hl24vri8yg711jmm8gl1mpwnlhass2n4"))
       (patches (search-patches "rust-nettle-disable-vendor.patch"))))
    (arguments
     `(#:cargo-inputs
       (("rust-failure" ,rust-failure-0.1)
        ("rust-getrandom" ,rust-getrandom-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nettle-sys" ,rust-nettle-sys-2))))))

(define-public rust-nettle-sys-2
  (package
    (name "rust-nettle-sys")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nettle-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wwa7pmxdz7yl9jwybml2kmrj3i87jcn0h0cdc5xl0lhgcs1rs5m"))))
    (build-system cargo-build-system)
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list nettle))
    (arguments
     `(#:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.63)
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

(define-public rust-orion-0.17
  (package
    (name "rust-orion")
    (version "0.17.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "orion" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ri0b0vyd9vqwlzlcv0q4i7r9pga23q7nnnvd5z4zycjc9v4mryb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ct-codecs" ,rust-ct-codecs-1)
        ("rust-fiat-crypto" ,rust-fiat-crypto-0.1)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-subtle" ,rust-subtle-2)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.4)
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

(define-public rust-pbkdf2-0.6
  (package
    (inherit rust-pbkdf2-0.10)
    (name "rust-pbkdf2")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pbkdf2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0jjaapyawm5iqn97mmfj40dvipsy78cm80qcva28009l2zbw1f5k"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-crypto-mac" ,rust-crypto-mac-0.10)
        ("rust-hmac" ,rust-hmac-0.10)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-subtle" ,rust-subtle-2))
       #:cargo-development-inputs
       (("rust-hmac" ,rust-hmac-0.10)
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
        (base32 "1m1c9jypydzabg4yscplmvff7pdcc8gg4cqg081hnlf03hxkmsc4"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1))))))

(define-public rust-ppv-lite86-0.2
  (package
    (name "rust-ppv-lite86")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ppv-lite86" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1shj4q7jwj0azssr8cg51dk3kh7d4lg9rmbbz1kbqk971vc5wyi3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cryptocorrosion/cryptocorrosion")
    (synopsis "Implementation of the crypto-simd API for x86")
    (description "This crate provides an implementation of the crypto-simd API
for x86.")
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
