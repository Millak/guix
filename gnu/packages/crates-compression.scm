;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
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

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-module (gnu packages crates-compression)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-brotli-7
  (package
    (name "rust-brotli")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g99xay61mds9d23fnfj5gfbd6g11gihfgs3y1abljwldzqvi5yc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
                       ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2)
                       ("rust-brotli-decompressor" ,rust-brotli-decompressor-4)
                       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://github.com/dropbox/rust-brotli")
    (synopsis "Brotli compressor and decompressor")
    (description
     "This package provides a brotli compressor and decompressor that with an
interface avoiding the rust stdlib.  This makes it suitable for embedded devices
and kernels.  It is designed with a pluggable allocator so that the standard
lib's allocator may be employed.  The default build also includes a stdlib
allocator and stream interface.  Disable this with --features=no-stdlib.  All
included code is safe.")
    (license (list license:bsd-3 license:expat))))

(define-public rust-brotli-6
  (package
    (inherit rust-brotli-7)
    (name "rust-brotli")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0swvf6cgbwhwrpd5y23vq6wipb6q2wqvi2j0hy0xa9lkplfrgxvl"))))
    (arguments
     `(#:cargo-inputs (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
                       ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2)
                       ("rust-brotli-decompressor" ,rust-brotli-decompressor-4)
                       ("rust-sha2" ,rust-sha2-0.10))))))

(define-public rust-brotli-3
  (package
    (inherit rust-brotli-7)
    (name "rust-brotli")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "14f34ml3i8qbnh4hhlv5r6j10bkx420gspsl1cgznl1wqrdx4h6n"))))
    (arguments
     `(#:cargo-inputs
       (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
        ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2)
        ("rust-brotli-decompressor" ,rust-brotli-decompressor-2)
        ("rust-sha2" ,rust-sha2-0.10))))))

(define-public rust-brotli-decompressor-4
  (package
    (name "rust-brotli-decompressor")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli-decompressor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qn39c7n6wm40i2bm0d3q2qslmaavlh804iv0ccbba4m80pbsics"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; not all test files included
       #:cargo-inputs (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
                       ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2))))
    (home-page "https://github.com/dropbox/rust-brotli-decompressor")
    (synopsis "Brotli decompressor")
    (description "This package provides a brotli decompressor with no
dependency on the rust stdlib.  This makes it suitable for embedded devices
and kernels.")
    (license (list license:bsd-3 license:expat))))

(define-public rust-brotli-decompressor-2
  (package
    (inherit rust-brotli-decompressor-4)
    (name "rust-brotli-decompressor")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli-decompressor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0kyyh9701dwqzwvn2frff4ww0zibikqd1s1xvl7n1pfpc3z4lbjf"))))
    (arguments
     `(#:tests? #f      ; not all test files included
       #:cargo-inputs
       (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
        ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2))))))

(define-public rust-brotli-sys-0.3
  (package
    (name "rust-brotli-sys")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kdfdbcba6zwa13xpjwgiplblkdf6vigxjbwwp6l2ascbylxwia4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/alexcrichton/brotli2-rs")
    (synopsis "Raw bindings to libbrotli")
    (description "This package provides raw bindings to libbrotli.")
    (license (list license:expat license:asl2.0))))

(define-public rust-brotli2-0.3
  (package
    (name "rust-brotli2")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13jnhpmfkqy2xar4lxrsk3rx3i12bgnarnsxq4arhc6yxb1kdc0c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-brotli-sys" ,rust-brotli-sys-0.3)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.4))))
    (home-page "https://github.com/alexcrichton/brotli2-rs")
    (synopsis "Rust bindings to compression library libbrotli")
    (description
     "This package provides bindings to libbrotli to provide brotli
decompression and compression to Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bzip2-0.4
  (package
    (name "rust-bzip2")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bzip2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y27wgqkx3k2jmh4k26vra2kqjq1qc1asww8hac3cv1zxyk1dcdx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bzip2-sys" ,rust-bzip2-sys-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-partial-io" ,rust-partial-io-0.3)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-tokio-core" ,rust-tokio-core-0.1))))
    (home-page "https://github.com/alexcrichton/bzip2-rs")
    (synopsis
     "Rust bindings to libbzip2 for bzip2 compression and decompression")
    (description
     "Bindings to @code{libbzip2} for @code{bzip2} compression and decompression
exposed as Reader/Writer streams.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bzip2-0.3
  (package
    (inherit rust-bzip2-0.4)
    (name "rust-bzip2")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bzip2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fvfwanp42j1zpig880jhb5mc0na50bijmwd6211p77sy35w7ds2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bzip2-sys" ,rust-bzip2-sys-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-partial-io" ,rust-partial-io-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.4)
        ("rust-rand" ,rust-rand-0.3)
        ("rust-tokio-core" ,rust-tokio-core-0.1))))))

(define-public rust-bzip2-rs-0.1
  (package
    (name "rust-bzip2-rs")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bzip2-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dgp83kixqrqj6q6574qr5zsfpbsiiwhqs3krhvsn4f8wkkmksxy"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Don't suggest nightly features of tinyvec.
                 (substitute* "Cargo.toml"
                   (("\"tinyvec/nightly_const_generics\", ") ""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-tinyvec" ,rust-tinyvec-1))
       #:cargo-development-inputs
       (("rust-bzip2" ,rust-bzip2-0.4)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.6))))
    (home-page "https://github.com/paolobarbolini/bzip2-rs")
    (synopsis "Pure Rust bzip2 decompressor")
    (description "Pure Rust bzip2 decompressor.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bzip2-sys-0.1
  (package
    (name "rust-bzip2-sys")
    (version "0.1.11+1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bzip2-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1p2crnv8d8gpz5c2vlvzl0j55i3yqg5bi0kwsl1531x77xgraskk"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "bzip2-1.0.8")
            (delete-file "build.rs")
            ;; Inspired by Debian's patch.
            (with-output-to-file "build.rs"
              (lambda _
                (format #t "fn main() {~@
                        println!(\"cargo:rustc-link-lib=bz2\");~@
                        }~%")))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-cc" ,rust-cc-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/alexcrichton/bzip2-rs")
    (synopsis "Rust bindings to libbzip2")
    (description
     "Bindings to @code{libbzip2} for bzip2 compression and decompression
exposed as Reader/Writer streams.")
    (license (list license:expat license:asl2.0))))

