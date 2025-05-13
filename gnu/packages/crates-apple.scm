;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021, 2022 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Jordan Moore <lockbox@struct.foo>
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

(define-module (gnu packages crates-apple)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls))

(define-public rust-block-0.1
  (package
    (name "rust-block")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-development-inputs
       (("rust-objc-test-utils" ,rust-objc-test-utils-0.0.2))))
    (home-page "https://github.com/SSheldon/rust-block")
    (synopsis "Rust interface for Apple's C language extension of blocks")
    (description "This package provides a rust interface for Apple's C language
extension of blocks.")
    (license license:expat)))

(define-public rust-block-sys-0.2
  (package
    (name "rust-block-sys")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rzp0218mwigdmfd5rhmj5h7c1vp0bq0nxaklhsvi8vydrls11df"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f          ; Needs to bind to MacOS libraries.
       #:cargo-inputs (("rust-objc-sys" ,rust-objc-sys-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Raw bindings to Apple's C language extension of blocks")
    (description "This package contains raw bindings to Apple's C language
extension of blocks.")
    (license license:expat)))

(define-public rust-block-sys-0.1
  (package
    (inherit rust-block-sys-0.2)
    (name "rust-block-sys")
    (version "0.1.0-beta.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "block-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ihiar08hk0das4q0ii1gsmql975z3rslli1h13jb44hxr0mg98g"))))
    (arguments
     `(#:tests? #f  ; Tests require gcc-objc.
       #:cargo-inputs
       (("rust-objc-sys" ,rust-objc-sys-0.2))))))

(define-public rust-block2-0.5
  (package
    (name "rust-block2")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-objc2" ,rust-objc2-0.5))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Apple's C language extension of blocks")
    (description "This package contains Apple's C language extension of blocks.")
    (license license:expat)))

(define-public rust-block2-0.3
  (package
    (inherit rust-block2-0.5)
    (name "rust-block2")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2ywcis2xf9444vmdgzr7ankrrkpchn8zimaw950cszm1imdd8m"))))
    (arguments
     `(#:skip-build? #t ; link kind `framework` is only supported on Apple targets
       #:cargo-inputs (("rust-block-sys" ,rust-block-sys-0.2)
                       ("rust-objc2" ,rust-objc2-0.4))))))

(define-public rust-block2-0.2
  (package
    (inherit rust-block2-0.3)
    (name "rust-block2")
    (version "0.2.0-alpha.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "block2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hpcdl81rmwvhfni9413hrg1wd4xwf6vhch3yv15bxs42wyfdncd"))))
    (arguments
     `(#:tests? #f  ; Tests require gcc-objc.
       #:cargo-inputs
       (("rust-block-sys" ,rust-block-sys-0.1)
        ("rust-objc2-encode" ,rust-objc2-encode-2))))))

(define-public rust-cargo-credential-macos-keychain-0.4
  (package
    (name "rust-cargo-credential-macos-keychain")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-credential-macos-keychain" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10djgy45fsirckyjjk7m63vfd6fkp9wrjfxznb5rplz1p4y0acfk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cargo-credential" ,rust-cargo-credential-0.4)
                       ("rust-security-framework" ,rust-security-framework-2))))
    (home-page "https://github.com/rust-lang/cargo")
    (synopsis "Cargo credential process that stores tokens in a macOS keychain")
    (description
     "This package provides a Cargo credential process that stores tokens in a
@code{macOS} keychain.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cocoa-0.25
  (package
    (name "rust-cocoa")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g1pl5hq28arqmvsswf2ib7smj445miwa58qa7wrfvksz54h857n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-block" ,rust-block-0.1)
                       ("rust-cocoa-foundation" ,rust-cocoa-foundation-0.1)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-core-graphics" ,rust-core-graphics-0.23)
                       ("rust-foreign-types" ,rust-foreign-types-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Cocoa for macOS")
    (description "This package provides bindings to Cocoa for macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cocoa-0.24
  (package
    (inherit rust-cocoa-0.25)
    (name "rust-cocoa")
    (version "0.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0flg2cwpqxyvsr1v3f54vi3d3qmbr1sn7gf3mr6nhb056xwxn9gl"))))
    (arguments
     `(#:skip-build? #t ; link kind `framework` is only supported on Apple targets
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-block" ,rust-block-0.1)
        ("rust-cocoa-foundation" ,rust-cocoa-foundation-0.1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.22)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-objc" ,rust-objc-0.2))))))

(define-public rust-cocoa-0.23
  (package
    (inherit rust-cocoa-0.24)
    (name "rust-cocoa")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cj4c2axmg7aiid2786mpzj7wxpd582biv7c7yimqfnggp002hn5"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-block" ,rust-block-0.1)
        ("rust-cocoa-foundation" ,rust-cocoa-foundation-0.1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.22)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-objc" ,rust-objc-0.2))))))

(define-public rust-cocoa-0.22
  (package
    (inherit rust-cocoa-0.23)
    (name "rust-cocoa")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19qyyv01yzrm6aahn6cdxvb4jhl6v4fj0cgqkxmq38i7hq3dqzv6"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-block" ,rust-block-0.1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.21)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-objc" ,rust-objc-0.2))))))

(define-public rust-cocoa-0.20
  (package
    (inherit rust-cocoa-0.25)
    (name "rust-cocoa")
    (version "0.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y0wd1lyiz8cgbsf0fwyw06gb1akg6rvg5jr3wah8mvdqdpyhj8c"))))
    (arguments
     `(#:skip-build? #t     ; only for macOS
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-block" ,rust-block-0.1)
                       ("rust-core-foundation" ,rust-core-foundation-0.7)
                       ("rust-core-graphics" ,rust-core-graphics-0.19)
                       ("rust-foreign-types" ,rust-foreign-types-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc" ,rust-objc-0.2))))))

(define-public rust-cocoa-0.19
  (package
    (inherit rust-cocoa-0.22)
    (name "rust-cocoa")
    (version "0.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0034vahbfv574q4b63rj241b8rnka5cjiqsqc6wiggnin9l7g7zj"))))
    (arguments
     `(#:skip-build? #t     ; only for macOS
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-block" ,rust-block-0.1)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-objc" ,rust-objc-0.2))))))

(define-public rust-cocoa-0.18
  (package
    (inherit rust-cocoa-0.19)
    (name "rust-cocoa")
    (version "0.18.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0m6fgdr4d2fp8jhkqvwr23hrqqqjv72g0j9vdgijc58k05j9j1hp"))))))

(define-public rust-cocoa-foundation-0.1
  (package
    (name "rust-cocoa-foundation")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xwk1khdyqw3dwsl15vr8p86shdcn544fr60ass8biz4nb5k8qlc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; link kind `framework` is only supported on Apple targets
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-block" ,rust-block-0.1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics-types" ,rust-core-graphics-types-0.1)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Cocoa Foundation for macOS")
    (description
     "This package provides bindings to Cocoa Foundation for macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-commoncrypto-0.2
  (package
    (name "rust-commoncrypto")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "commoncrypto" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01whnqcziclsj1gwavvqhrw2r5cmwh00j2fbc56iwnm2ddcahmnh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t
        #:cargo-inputs
        (("rust-clippy" ,rust-clippy-0.0.302)
         ("rust-commoncrypto-sys" ,rust-commoncrypto-sys-0.2))))
    (home-page "https://github.com/malept/rust-commoncrypto")
    (synopsis "Idiomatic Rust wrappers for Mac OS X's CommonCrypto library")
    (description "The @code{commoncrypto} library provides Rust FFI bindings and
idiomatic wrappers for Mac OS X's CommonCrypto library.")
    (license license:expat)))

(define-public rust-commoncrypto-sys-0.2
  (package
    (name "rust-commoncrypto-sys")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "commoncrypto-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ql381ziqh594a7z6m9bvs583lkrhbynk02pmbgp7aj7czs39v8z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t                ;requires the Mac OS library
        #:cargo-inputs
        (("rust-clippy" ,rust-clippy-0.0.302)
         ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/malept/rust-commoncrypto")
    (synopsis "FFI bindings to Mac OS X's CommonCrypto library")
    (description "This package is a component of the @code{commoncrypto}
library which provides Rust FFI bindings and idiomatic wrappers for Mac OS X's
CommonCrypto library.")
    (license license:expat)))

(define-public rust-core-foundation-0.10
  (package
    (name "rust-core-foundation")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qscay14s2rwkg8nd8ljhiaf149hj8sfy95d70zssy64r3jp2lmm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; link kind `framework` is only supported on Apple targets
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "This package provides bindings to Core Foundation for macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-foundation-0.9
  (package
    (inherit rust-core-foundation-0.10)
    (name "rust-core-foundation")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))))
    (arguments
     `(#:tests? #f             ;tests fail with a lot of "undefined reference"
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-uuid" ,rust-uuid-0.5))))))

(define-public rust-core-foundation-0.7
  (package
    (inherit rust-core-foundation-0.9)
    (name "rust-core-foundation")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wbias8f0m5kyn2pcksi0h58fdslams6nmf16w78fgn42dx4rljp"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-uuid" ,rust-uuid-0.5))))))

(define-public rust-core-foundation-0.6
  (package
    (inherit rust-core-foundation-0.7)
    (name "rust-core-foundation")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0va97wf49c8dzm9c8pgyk1jn7z21rl0bj1syf2zz5m2z2hzy1f95"))))
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.6)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-uuid" ,rust-uuid-0.5))))))

(define-public rust-core-foundation-sys-0.8
  (package
    (name "rust-core-foundation-sys")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "This package provides bindings to Core Foundation for macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-foundation-sys-0.7
  (package
    (inherit rust-core-foundation-sys-0.8)
    (name "rust-core-foundation-sys")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b5qfnnmg49sawwfsb0c0wbj81bqi7h7lh68pmhbidf0jjs1m9xk"))))))

(define-public rust-core-foundation-sys-0.6
  (package
    (inherit rust-core-foundation-sys-0.7)
    (name "rust-core-foundation-sys")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fzsw1j9g1x598yhwklg59l15hwzc0pyvs01w9fg2kin4598mjp7"))))))

(define-public rust-core-text-20
  (package
    (name "rust-core-text")
    (version "20.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-text" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mffma8w0ic11ydv6zclamw4dslzmsych1fwz14msih8bh5pkln9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; link kind `framework` is only supported on Apple targets
       #:cargo-inputs (("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-core-graphics" ,rust-core-graphics-0.23)
                       ("rust-foreign-types" ,rust-foreign-types-0.5)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to the Core Text framework")
    (description "This package provides bindings to the Core Text framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-text-19
  (package
    (inherit rust-core-text-20)
    (name "rust-core-text")
    (version "19.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-text" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09aa9gfw0zvjwncskr721ljnyj2vfsxbz2lgl7piqz70cvd4mmwr"))))
    (arguments
     `(#:skip-build? #t ; link kind `framework` is only supported on Apple targets
       #:cargo-inputs
       (("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.22)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-coreaudio-rs-0.10
  (package
    (name "rust-coreaudio-rs")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "coreaudio-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "125d4zr3n363ybga4629p41ym7iqjfb2alnwrc1zj7zyxch4p28i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; Only builds for macos or ios.
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-coreaudio-sys" ,rust-coreaudio-sys-0.2))))
    (home-page "https://github.com/RustAudio/coreaudio-rs")
    (synopsis "Rust interface for Apple's CoreAudio API")
    (description
     "This package provides a rust interface for Apple's CoreAudio API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-coreaudio-sys-0.2
  (package
    (name "rust-coreaudio-sys")
    (version "0.2.16")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "coreaudio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0asnnypbsqzj2lxx4irnmyi9b32sl0vkxb61mj3p5mvp1fm5gs1c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; Only builds for macos or ios.
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.70))))
    (home-page "https://github.com/RustAudio/coreaudio-sys")
    (synopsis
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (description
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen.")
    (license license:expat)))

(define-public rust-dispatch-0.2
  (package
    (name "rust-dispatch")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dispatch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fwjr9b7582ic5689zxj8lf7zl94iklhlns3yivrnv8c9fxr635x"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/SSheldon/rust-dispatch")
    (synopsis "Rust wrapper for Apple's Grand Central Dispatch")
    (description "This package provides a Rust wrapper for Apple's Grand
Central Dispatch.")
    (license license:expat)))

(define-public rust-dispatch-0.1
  (package
    (inherit rust-dispatch-0.2)
    (name "rust-dispatch")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dispatch" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "019nzy993hxaiazcdnayx3csv2iki34i535asw11ki96hakkrs84"))))
    (arguments '(#:tests? #f))))  ; Tests only run on Mac.

(define-public rust-fat-macho-0.4
  (package
    (name "rust-fat-macho")
    (version "0.4.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fat-macho" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0idkn366wipv2l757yqfgzgibqc6jvm89gdk9kpgmvf6lv54b72c"))
              (snippet
               #~(begin (use-modules (guix build utils))
                        (delete-file-recursively "tests/fixtures")))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Test files removed.
       #:cargo-inputs
       (("rust-goblin" ,rust-goblin-0.9)
        ("rust-llvm-bitcode" ,rust-llvm-bitcode-0.1))))
    (home-page "https://github.com/messense/fat-macho-rs.git")
    (synopsis "Mach-O Fat Binary Reader and Writer")
    (description "This package provides a Mach-O Fat Binary Reader and Writer.")
    (license license:expat)))

(define-public rust-fsevent-2
  (package
    (name "rust-fsevent")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fsevent" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pvpz0n4yl64yvx3acchxnfd28vhx88x4pvsa6zrb8d08zqx2dl8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-fsevent-sys" ,rust-fsevent-sys-4))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3)
        ("rust-time" ,rust-time-0.2))))
    (home-page "https://github.com/octplane/fsevent-rust")
    (synopsis "Rust bindings to the fsevent-sys macOS API")
    (description
     "This package provides Rust bindings to the @code{fsevent-sys} macOS API
for file changes notifications")
    (license license:expat)))

(define-public rust-fsevent-0.4
  (package
    (inherit rust-fsevent-2)
    (name "rust-fsevent")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fsevent" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1djxnc2fmv265xqf1iyfz56smh13v9r1p0w9125wjg6k3fyx3dss"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; only available on macOS
       #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-fsevent-sys" ,rust-fsevent-sys-2))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir-0.3)
         ("rust-time" ,rust-time-0.1))))))

(define-public rust-fsevent-sys-4
  (package
    (name "rust-fsevent-sys")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fsevent-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1liz67v8b0gcs8r31vxkvm2jzgl9p14i78yfqx81c8sdv817mvkn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/octplane/fsevent-rust/tree/master/fsevent-sys")
    (synopsis "Rust bindings to the fsevent macOS API")
    (description "This package provides Rust bindings to the @code{fsevent}
macOS API for file changes notifications")
    (license license:expat)))

(define-public rust-fsevent-sys-3
  (package
    (inherit rust-fsevent-sys-4)
    (name "rust-fsevent-sys")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fsevent-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mav57d1zcp4x17h0wprcr188d8yvxfz1c0f1z0p31q52xl5wvya"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2))))))

(define-public rust-fsevent-sys-2
  (package
    (inherit rust-fsevent-sys-3)
    (name "rust-fsevent-sys")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fsevent-sys" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "18246vxk7rqn52m0sfrhivxq802i34p2wqqx5zsa0pamjj5086zl"))))
    (arguments
     `(#:skip-build? #t     ; only available on macOS
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))))

(define-public rust-icrate-0.0.4
  (package
    (name "rust-icrate")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "icrate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06d3g8n6xw3f0ai84mya0dlknp2n33zqdxpxp8272mslibzsmlwr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-block2" ,rust-block2-0.3)
                       ("rust-dispatch" ,rust-dispatch-0.2)
                       ("rust-objc2" ,rust-objc2-0.4))
       #:cargo-development-inputs
       (("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to Apple's frameworks")
    (description "This package provides bindings to Apple's frameworks.")
    (license license:expat)))

(define-public rust-mac-notification-sys-0.6
  (package
    (name "rust-mac-notification-sys")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mac-notification-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14cgvhb2790fzsilwdw720m2pc2zzk0zcgbjgqbkgahp6x7z7s6w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ;Requires OSX to build framework "Foundation"
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-dirs-next" ,rust-dirs-next-2)
                       ("rust-objc-foundation" ,rust-objc-foundation-0.1)
                       ("rust-objc-id" ,rust-objc-id-0.1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/h4llow3En/mac-notification-sys")
    (synopsis "Thin wrapper around macOS Notifications")
    (description
     "This package provides Thin wrapper around @code{macOS} Notifications.")
    (license license:expat)))

(define-public rust-mach-0.3
  (package
    (name "rust-mach")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mach" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yksa8lwzqh150gr4417rls1wk20asy9vhp8kq5g9n7z58xyh8xq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/fitzgen/mach")
    (synopsis "Rust interface to the Mach 3.0 kernel that underlies OSX")
    (description
     "This package provides a Rust interface to the user-space API of the
Mach 3.0 kernel that underlies OSX.")
    (license (list license:asl2.0 license:expat license:bsd-2))))

(define-public rust-mach-0.2
  (package
    (inherit rust-mach-0.3)
    (name "rust-mach")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mach" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qdhs16cl1j3w7kvy6ak7h8lbyqmr6i3i15qfzpnv9gyrn3j9pc6"))))))

(define-public rust-mach-o-sys-0.1
  (package
    (name "rust-mach-o-sys")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mach_o_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09l8p7nmzq37450x2h6nb7dzg1sk6dk36a5rkcrcy81zm21lb19y"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/fitzgen/mach_o_sys")
    (synopsis "Bindings to the OSX mach-o system library")
    (description "This package provides bindings to the OSX mach-o system
library")
  (license (list license:asl2.0 license:expat))))

(define-public rust-mach2-0.4
  (package
    (name "rust-mach2")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mach2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02gpyq89rcrqdbz4hgp5bpjas21dllxfc70jgw8vj0iaxg6mbf8r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; compile_error!("mach requires macOS or iOS");
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/JohnTitor/mach2")
    (synopsis "Rust interface to the user-space API of the Mach 3.0 kernel")
    (description
     "This package provides a Rust interface to the user-space API of the Mach
3.0 kernel that underlies OSX.")
    (license (list license:bsd-2 license:expat license:asl2.0))))

(define-public rust-metal-0.18
  (package
    (name "rust-metal")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h1dx42sdkn4jl1rnjisv687vd5jyck982wxq3r9xgmx8bpa1671"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-block" ,rust-block-0.1)
        ("rust-cocoa" ,rust-cocoa-0.20)
        ("rust-core-graphics" ,rust-core-graphics-0.19)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/gfx-rs/metal-rs")
    (synopsis "Rust bindings for Metal")
    (description "This package provides Rust bindings for Metal.")
    (license (list license:expat license:asl2.0))))

(define-public rust-objc-0.2
  (package
    (name "rust-objc")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1cbpf6kz8a244nn1qzl3xyhmp05gsg4n313c9m3567625d3innwi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Tests require gcc-objc.
       #:cargo-inputs
       (("rust-malloc-buf" ,rust-malloc-buf-0.0)
        ("rust-objc-exception" ,rust-objc-exception-0.1))))
    (home-page "https://github.com/SSheldon/rust-objc")
    (synopsis "Objective-C Runtime bindings and wrapper for Rust")
    (description "This package provides an Objective-C Runtime bindings and
wrapper for Rust.")
    (license license:expat)))

(define-public rust-objc-exception-0.1
  (package
    (name "rust-objc-exception")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc_exception" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "191cmdmlypp6piw67y4m8y5swlxf5w0ss8n1lk5xd2l1ans0z5xd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/SSheldon/rust-objc-exception")
    (synopsis "Rust interface for Objective-C's throw and try/catch statements")
    (description
     "This package provides a Rust interface for Objective-C's throw and
try/catch statements.")
    (license license:expat)))

(define-public rust-objc-foundation-0.1
  (package
    (name "rust-objc-foundation")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc-foundation" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1y9bwb3m5fdq7w7i4bnds067dhm4qxv4m1mbg9y61j9nkrjipp8s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Only available on macOS.
       #:cargo-inputs
       (("rust-block" ,rust-block-0.1)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-objc-id" ,rust-objc-id-0.1))))
    (home-page "https://github.com/SSheldon/rust-objc-foundation")
    (synopsis "Rust wrapper for Objective-C's Foundation framework")
    (description "This package provides a rust wrapper for Objective-C's
Foundation framework.")
    (license license:expat)))

(define-public rust-objc-id-0.1
  (package
    (name "rust-objc-id")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc_id" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fq71hnp2sdblaighjc82yrac3adfmqzhpr11irhvdfp9gdlsbf9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Tests require gcc-objc.
       #:cargo-inputs (("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/SSheldon/rust-objc-id")
    (synopsis "Rust smart pointers for Objective-C reference counting")
    (description
     "This package provides Rust smart pointers for Objective-C reference counting.")
    (license license:expat)))

(define-public rust-objc-sys-0.3
  (package
    (name "rust-objc-sys")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "objc-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0423gry7s3rmz8s3pzzm1zy5mdjif75g6dbzc2lf2z0c77fipffd"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Raw bindings to the Objective-C runtime and ABI")
    (description
     "This package provides raw bindings to the Objective-C runtime and ABI.")
    (license license:expat)))

(define-public rust-objc-sys-0.2
  (package
    (inherit rust-objc-sys-0.3)
    (name "rust-objc-sys")
    (version "0.2.0-beta.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "objc-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1msm1bwv69k12ikxm71mi1ifrbx2bzsmk2w2bah98mp9q4s9hfyz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t         ; Needs gcc-objc
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))))

(define-public rust-objc-test-utils-0.0.2
  (package
    (name "rust-objc-test-utils")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc_test_utils" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09rckmp5h9bbns08xzicdlk7y5lxj2ygbg3yqk1cszfnzd5n8kzx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gcc" ,rust-gcc-0.3))))
    (home-page "https://github.com/SSheldon/rust-objc")
    (synopsis "Utilities for testing Objective-C interop")
    (description
     "This package provides utilities for testing Objective-C interop.")
    (license license:expat)))

(define-public rust-objc2-0.5
  (package
    (name "rust-objc2")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "015qa2d3vh7c1j2736h5wjrznri7x5ic35vl916c22gzxva8b9s6"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-malloc-buf" ,rust-malloc-buf-1)
                       ("rust-objc-sys" ,rust-objc-sys-0.3)
                       ("rust-objc2-encode" ,rust-objc2-encode-4)
                       ("rust-objc2-proc-macros" ,rust-objc2-proc-macros-0.1))
       #:cargo-development-inputs
       (("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-iai" ,rust-iai-0.1)
        ("rust-memoffset" ,rust-memoffset-0.9)
        ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis
     "Objective-C interface and bindings to the Cocoa Foundation framework")
    (description "This package provides Objective-C interface and bindings to
the Cocoa Foundation framework.")
    (license license:expat)))

(define-public rust-objc2-0.4
  (package
    (inherit rust-objc2-0.5)
    (name "rust-objc2")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13gr3zqv8gzlylff5d4za91f50asb7vsrkpv8kiva3nkzm05m72m"))))
    (arguments
     `(#:skip-build? #t ; link kind `framework` is only supported on Apple targets
       #:cargo-inputs (("rust-malloc-buf" ,rust-malloc-buf-1)
                       ("rust-objc-sys" ,rust-objc-sys-0.3)
                       ("rust-objc2-encode" ,rust-objc2-encode-3)
                       ("rust-objc2-proc-macros" ,rust-objc2-proc-macros-0.1))
       #:cargo-development-inputs
       (("rust-iai" ,rust-iai-0.1)
        ("rust-static-assertions" ,rust-static-assertions-1))))))

(define-public rust-objc2-0.3
  (package
    (inherit rust-objc2-0.4)
    (name "rust-objc2")
    (version "0.3.0-beta.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "objc2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jfnrwmp6n2d6snlpcyfk9l41nhm50wj9h42b6hqj2rxbm1facgy"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-block2" ,rust-block2-0.2)            ; 0.2.0-alpha.6
        ("rust-objc-sys" ,rust-objc-sys-0.2)        ; 0.2.0-beta.2
        ("rust-objc2-encode" ,rust-objc2-encode-2)  ; 2.0.0-pre.2
        ("rust-objc2-proc-macros" ,rust-objc2-proc-macros-0.1)
        ("rust-uuid" ,rust-uuid-1))))))

(define-public rust-objc2-app-kit-0.2
  (package
    (name "rust-objc2-app-kit")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-app-kit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zqyi5l1bm26j1bgmac9783ah36m5kcrxlqp5carglnpwgcrms74"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-core-data" ,rust-objc2-core-data-0.2)
                       ("rust-objc2-core-image" ,rust-objc2-core-image-0.2)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2)
                       ("rust-objc2-quartz-core" ,rust-objc2-quartz-core-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the AppKit framework")
    (description
     "This package provides bindings to the @code{AppKit} framework.")
    (license license:expat)))

(define-public rust-objc2-cloud-kit-0.2
  (package
    (name "rust-objc2-cloud-kit")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-cloud-kit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02dhjvmcq8c2bwj31jx423jygif1scs9f0lmlab0ayhw75b3ppbl"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-core-location" ,rust-objc2-core-location-0.2)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CloudKit framework")
    (description
     "This package provides bindings to the @code{CloudKit} framework.")
    (license license:expat)))

(define-public rust-objc2-contacts-0.2
  (package
    (name "rust-objc2-contacts")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-contacts" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12a8m927xrrxa54xhqhqnkkl1a6l07pyrpnqfk9jz09kkh755zx5"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Contacts framework")
    (description "This package provides bindings to the Contacts framework.")
    (license license:expat)))

(define-public rust-objc2-core-data-0.2
  (package
    (name "rust-objc2-core-data")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-data" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vvk8zjylfjjj04dzawydmqqz5ajvdkhf22cnb07ihbiw14vyzv1"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreData framework")
    (description
     "This package provides bindings to the @code{CoreData} framework.")
    (license license:expat)))

(define-public rust-objc2-core-image-0.2
  (package
    (name "rust-objc2-core-image")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-image" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "102csfb82zi2sbzliwsfd589ckz0gysf7y6434c9zj97lmihj9jm"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreImage framework")
    (description
     "This package provides bindings to the @code{CoreImage} framework.")
    (license license:expat)))

(define-public rust-objc2-core-location-0.2
  (package
    (name "rust-objc2-core-location")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-location" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10apgsrigqryvi4rcc0f6yfjflvrl83f4bi5hkr48ck89vizw300"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-contacts" ,rust-objc2-contacts-0.2)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreLocation framework")
    (description
     "This package provides bindings to the @code{CoreLocation} framework.")
    (license license:expat)))

(define-public rust-objc2-encode-4
  (package
    (name "rust-objc2-encode")
    (version "4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-encode" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y7hjg4k828zhn4fjnbidrz3vzw4llk9ldy92drj47ydjc9yg4bq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Objective-C type-encoding representation and parsing")
    (description
     "This package provides Objective-C type-encoding representation and parsing.")
    (license license:expat)))

(define-public rust-objc2-encode-3
  (package
    (inherit rust-objc2-encode-4)
    (name "rust-objc2-encode")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-encode" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rknhkcnyj4qv1pzqp5j8l80726phz8fcxpsbpz9nhmg6xdq8yfh"))))))

(define-public rust-objc2-encode-2
  (package
    (inherit rust-objc2-encode-3)
    (name "rust-objc2-encode")
    (version "2.0.0-pre.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "objc2-encode" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04h5wns3hxmc9g652hr9xqzrijs4ij9sdnlgc0ha202v050srz5b"))))
    (arguments
     `(#:tests? #f      ; Test suite wants gcc-objc
       #:cargo-inputs (("rust-objc-sys" ,rust-objc-sys-0.2))))))

(define-public rust-objc2-foundation-0.2
  (package
    (name "rust-objc2-foundation")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a6mi77jsig7950vmx9ydvsxaighzdiglk5d229k569pvajkirhf"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.5)
                       ("rust-dispatch" ,rust-dispatch-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.5))
       #:cargo-development-inputs
       (("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Foundation framework")
    (description "This package provides bindings to the Foundation framework.")
    (license license:expat)))

(define-public rust-objc2-link-presentation-0.2
  (package
    (name "rust-objc2-link-presentation")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-link-presentation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "160k4qh00yrx57dabn3hzas4r98kmk9bc0qsy1jvwday3irax8d1"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-app-kit" ,rust-objc2-app-kit-0.2)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the LinkPresentation framework")
    (description
     "This package provides bindings to the @code{LinkPresentation} framework.")
    (license license:expat)))

(define-public rust-objc2-metal-0.2
  (package
    (name "rust-objc2-metal")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-metal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mmdga66qpxrcfq3gxxhysfx3zg1hpx4z886liv3j0pnfq9bl36x"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Metal framework")
    (description "This package provides bindings to the Metal framework.")
    (license license:expat)))

(define-public rust-objc2-proc-macros-0.1
  (package
    (name "rust-objc2-proc-macros")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "objc2-proc-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w335fj58k76z94d242xq18qkj7iw082lpy3kxnisaa5r7q4aaa6"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Procedural macros for the objc2 project")
    (description "This package provides procedural macros for the objc2 project.")
    (license license:expat)))

(define-public rust-objc2-quartz-core-0.2
  (package
    (name "rust-objc2-quartz-core")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-quartz-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ynw8819c36l11rim8n0yzk0fskbzrgaqayscyqi8swhzxxywaz4"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the QuartzCore/CoreAnimation framework")
    (description
     "This package provides bindings to the @code{QuartzCore/CoreAnimation}
framework.")
    (license license:expat)))

(define-public rust-objc2-symbols-0.2
  (package
    (name "rust-objc2-symbols")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-symbols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p04hjkxan18g2b7h9n2n8xxsvazapv2h6mfmmdk06zc7pz4ws0a"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Symbols framework")
    (description "This package Provides Bindings to the Symbols framework.")
    (license license:expat)))

(define-public rust-objc2-ui-kit-0.2
  (package
    (name "rust-objc2-ui-kit")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-ui-kit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vrb5r8z658l8c19bx78qks8c5hg956544yirf8npk90idwldfxq"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-block2" ,rust-block2-0.5)
        ("rust-objc2" ,rust-objc2-0.5)
        ("rust-objc2-cloud-kit" ,rust-objc2-cloud-kit-0.2)
        ("rust-objc2-core-data" ,rust-objc2-core-data-0.2)
        ("rust-objc2-core-image" ,rust-objc2-core-image-0.2)
        ("rust-objc2-core-location" ,rust-objc2-core-location-0.2)
        ("rust-objc2-foundation" ,rust-objc2-foundation-0.2)
        ("rust-objc2-link-presentation" ,rust-objc2-link-presentation-0.2)
        ("rust-objc2-quartz-core" ,rust-objc2-quartz-core-0.2)
        ("rust-objc2-symbols" ,rust-objc2-symbols-0.2)
        ("rust-objc2-uniform-type-identifiers" ,rust-objc2-uniform-type-identifiers-0.2)
        ("rust-objc2-user-notifications" ,rust-objc2-user-notifications-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the UIKit framework")
    (description "This package provides bindings to the UIKit framework.")
    (license license:expat)))

(define-public rust-objc2-uniform-type-identifiers-0.2
  (package
    (name "rust-objc2-uniform-type-identifiers")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-uniform-type-identifiers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ziv4wkbxcaw015ypg0q49ycl7m14l3x56mpq2k1rznv92bmzyj4"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the UniformTypeIdentifiers framework")
    (description
     "This package provides bindings to the @code{UniformTypeIdentifiers} framework.")
    (license license:expat)))

(define-public rust-objc2-user-notifications-0.2
  (package
    (name "rust-objc2-user-notifications")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-user-notifications" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cscv2w3vxzaslz101ddv0z9ycrrs4ayikk4my4qd3im8bvcpkvn"))))
    (build-system cargo-build-system)
    (arguments
     ;; Must specify the desired runtime using Cargo features on non-Apple platforms
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.5)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-core-location" ,rust-objc2-core-location-0.2)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the UserNotifications framework")
    (description
     "This package provides bindings to the @code{UserNotifications} framework.")
    (license license:expat)))

(define-public rust-security-framework-3
  (package
    (name "rust-security-framework")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g1wq04rb6gsyfawphv5vhmmicbm5l25gsvr05mvng6cpz4zilw1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; unresolved import `security_framework::secure_transport`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-core-foundation" ,rust-core-foundation-0.10)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-security-framework-sys" ,rust-security-framework-sys-2))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-time" ,rust-time-0.3)
                                   ("rust-x509-parser" ,rust-x509-parser-0.16))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "@code{Security.framework} bindings for macOS and iOS")
    (description "This package provides @code{Security.framework} bindings for
macOS and iOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-2
  (package
    (inherit rust-security-framework-3)
    (name "rust-security-framework")
    (version "2.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))))
    (arguments
     `(#:tests? #f      ; unresolved import `security_framework::secure_transport`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-security-framework-sys" ,rust-security-framework-sys-2))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-time" ,rust-time-0.3)
        ("rust-x509-parser" ,rust-x509-parser-0.16))))))

(define-public rust-security-framework-1
  (package
    (inherit rust-security-framework-2)
    (name "rust-security-framework")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0axwlax65j1f79rsm4ylc8rc6p2knbi3dgnpbdq7a1bzh5k2hl5d"))))
    (arguments
     `(#:tests? #f      ; Not all files included
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.7)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-security-framework-sys" ,rust-security-framework-sys-1))
       #:cargo-development-inputs
       (("rust-hex" ,rust-hex-0.4)
        ("rust-tempdir" ,rust-tempdir-0.3))))))

(define-public rust-security-framework-0.3
  (package
    (inherit rust-security-framework-1)
    (name "rust-security-framework")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pqn79cl9njnnhsmjvvphkzx8is5jhfd8bhxpllgvrgggjfl5wlf"))))
    (arguments
     `(#:tests? #f      ; Some test files not included in release.
       #:cargo-inputs
       (("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.6)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-security-framework-sys" ,rust-security-framework-sys-0.3))
       #:cargo-development-inputs
       (("rust-hex" ,rust-hex-0.4)
        ("rust-tempdir" ,rust-tempdir-0.3))))))

(define-public rust-security-framework-0.2.4-yanked
  (package
    (inherit rust-security-framework-0.3)
    (name "rust-security-framework")
    (version "0.2.4") ;This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32 "0gw3xxg8yzbjb4ny5cy07gky177c1nbgpxqjsw3hfzpfgrxji9bz"))))
    (arguments
     `(#:skip-build? #t ;MacOS specific
       #:cargo-inputs
       (("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.6)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-security-framework-sys" ,rust-security-framework-sys-0.2))
       #:cargo-development-inputs (("rust-hex" ,rust-hex-0.3)
                                   ("rust-tempdir" ,rust-tempdir-0.3))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-security-framework-0.2
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-security-framework"
                      rust-security-framework-0.2.4-yanked))

(define-public rust-security-framework-sys-2
  (package
    (name "rust-security-framework-sys")
    (version "2.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mbhagj98y2byhjkr353y1nings01pfa9yk0gxmcb0ydd0vzsqqq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
        ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Low-level FFI bindings to Apple @code{Security.framework}")
    (description "This package provides low level FFI bindings to Apple
@code{Security.framework}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-sys-1
  (package
    (inherit rust-security-framework-sys-2)
    (name "rust-security-framework-sys")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1iynsjz53lqkkw4zbq8l99xn799chbx90lsmrlfnsyxii14v1kji"))))
    (arguments
     `(#:cargo-inputs
       (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-security-framework-sys-0.3
  (package
    (inherit rust-security-framework-sys-1)
    (name "rust-security-framework-sys")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15gqhhi206lzynd0pcbswxhvqc4p9bmpl2h9qnwfnpb16zy96573"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.6))))))

(define-public rust-security-framework-sys-0.2.4-yanked
  (package
    (inherit rust-security-framework-sys-0.3)
    (name "rust-security-framework-sys")
    (version "0.2.4") ;This version was yanked!
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework-sys" version))
       (file-name (string-append name "-" version "-yanked.tar.gz"))
       (sha256
        (base32 "07zv0szz2kfy1hn251h0qsq0q9i1zia768d8vzril1g6xarj7mcj"))))
    (arguments
     `(#:skip-build? #t ;MacOS specific
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.6)
                       ("rust-libc" ,rust-libc-0.2))))
    (properties '((crate-version-yanked? . #t)))))

(define-public rust-security-framework-sys-0.2
  ;; There are no non-yanked versions of this semver.
  (deprecated-package "rust-security-framework-sys"
                      rust-security-framework-sys-0.2.4-yanked))

(define-public rust-system-configuration-0.6
  (package
    (name "rust-system-configuration")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "system-configuration" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sxslml567zm0v8g732314vd2gk9sd3k4xj22xk6p64xir29v1rw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; struct `sockaddr_in` has no field named `sin_len`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-system-configuration-sys" ,rust-system-configuration-sys-0.6))))
    (home-page "https://github.com/mullvad/system-configuration-rs")
    (synopsis "Bindings to SystemConfiguration framework for macOS")
    (description
     "This package provides bindings to the @code{SystemConfiguration} framework
for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-system-configuration-0.5
  (package
    (inherit rust-system-configuration-0.6)
    (name "rust-system-configuration")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "system-configuration" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rz0r30xn7fiyqay2dvzfy56cvaa3km74hnbz2d72p97bkf3lfms"))))
    (arguments
     `(#:skip-build? #t ; struct `sockaddr_in` has no field named `sin_len`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-system-configuration-sys" ,rust-system-configuration-sys-0.5))))))

(define-public rust-system-configuration-sys-0.6
  (package
    (name "rust-system-configuration-sys")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "system-configuration-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i5sqrmgy58l4704hibjbl36hclddglh73fb3wx95jnmrq81n7cf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/mullvad/system-configuration-rs")
    (synopsis "Low level bindings to SystemConfiguration framework for macOS")
    (description
     "Low level bindings to @code{SystemConfiguration} framework for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-system-configuration-sys-0.5
  (package
    (inherit rust-system-configuration-sys-0.6)
    (name "rust-system-configuration-sys")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "system-configuration-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jckxvdr37bay3i9v52izgy52dg690x5xfg3hd394sv2xf4b2px7"))))
    (arguments
     `(#:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2))))))
