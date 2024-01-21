;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Steve George <steve@futurile.net>
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

(define-module (gnu packages crates-crypto)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crypto))

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
