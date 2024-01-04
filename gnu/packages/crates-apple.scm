;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
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
