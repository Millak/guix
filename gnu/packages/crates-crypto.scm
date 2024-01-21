;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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
