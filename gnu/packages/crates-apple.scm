;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2023 Steve George <steve@futurile.net>
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
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io))

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
       (("rust-objc-test-utils" ,rust-objc-test-utils-0.0))))
    (home-page "https://github.com/SSheldon/rust-block")
    (synopsis "Rust interface for Apple's C language extension of blocks")
    (description "This package provides a rust interface for Apple's C language
extension of blocks.")
    (license license:expat)))

(define-public rust-block2-0.3
  (package
    (name "rust-block2")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2ywcis2xf9444vmdgzr7ankrrkpchn8zimaw950cszm1imdd8m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; link kind `framework` is only supported on Apple targets
       #:cargo-inputs (("rust-block-sys" ,rust-block-sys-0.2)
                       ("rust-objc2" ,rust-objc2-0.4))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Apple's C language extension of blocks")
    (description "This package contains Apple's C language extension of blocks.")
    (license license:expat)))

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

(define-public rust-block-sys-0.2
  (package
    (name "rust-block-sys")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14pddxc4rma833prvlbh5a22q6qwx32hhz7aqmnw1p9cj58czmrd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Needs to bind to MacOS libraries.
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
    (version "0.2.12")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "coreaudio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "091b4sq3kl8n4dy86l4mxq9vjzsn8w8b51xzfcpxwjkciqjv4d7h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; Only builds for macos or ios.
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.64))))
    (home-page "https://github.com/RustAudio/coreaudio-sys")
    (synopsis
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (description
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen.")
    (license license:expat)))

(define-public rust-core-foundation-0.9
  (package
    (name "rust-core-foundation")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ii1ihpjb30fk38gdikm5wqlkmyr8k46fh4k2r8sagz5dng7ljhr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f             ;tests fail with a lot of "undefined reference"
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-uuid" ,rust-uuid-0.5))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "This package provides bindings to Core Foundation for macOS.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-core-foundation-0.2
  (package
    (inherit rust-core-foundation-0.6)
    (name "rust-core-foundation")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rvcn7ab5r69wvn7gby745jlpy8pirfywcdxbiypy083s93dggr5"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.2)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-core-foundation-sys-0.8
  (package
    (name "rust-core-foundation-sys")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13w6sdf06r0hn7bx2b45zxsg1mm2phz34jikm6xc5qrbr6djpsh6"))))
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

(define-public rust-core-foundation-sys-0.2
  (package
    (inherit rust-core-foundation-sys-0.6)
    (name "rust-core-foundation-sys")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13f7f3kblyj6yxcxm74yg84vj9ahaprlc1vgblagmj6bzmzmsnh6"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2))))))
