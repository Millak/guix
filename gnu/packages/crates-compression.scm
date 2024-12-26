;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Steve George <steve@futurile.net>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages pkg-config))

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

(define-public rust-deflate-1
  (package
    (name "rust-deflate")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "deflate" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0bs319wa9wl7pn9j6jrrxg1gaqbak581rkx210cbix0qyljpwvy8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; not all test files included
       #:cargo-inputs
       (("rust-adler32" ,rust-adler32-1)
        ("rust-gzip-header" ,rust-gzip-header-1))
       #:cargo-development-inputs
       (("rust-miniz-oxide" ,rust-miniz-oxide-0.5))))
    (home-page "https://github.com/image-rs/deflate-rs")
    (synopsis "DEFLATE, zlib and gzip encoder written in rust")
    (description "This package provides a DEFLATE, zlib and gzip encoder
written in rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-deflate-0.9
  (package
    (inherit rust-deflate-1)
    (name "rust-deflate")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "deflate" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0w0ww0hrq4bjnihxgbnrri4lj5c8yzg31fyzx36fd9pvvw2vz5az"))))
    (arguments
     `(#:tests? #f      ; not all test files included
       #:cargo-inputs
       (("rust-adler32" ,rust-adler32-1)
        ("rust-gzip-header" ,rust-gzip-header-0.3))
       #:cargo-development-inputs
       (("rust-miniz-oxide" ,rust-miniz-oxide-0.3))))))

(define-public rust-deflate-0.8
  (package
    (inherit rust-deflate-0.9)
    (name "rust-deflate")
    (version "0.8.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "deflate" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0x6iqlayg129w63999kz97m279m0jj4x4sm6gkqlvmp73y70yxvk"))))
    (arguments
     `(#:tests? #f      ; not all test files included
       #:cargo-inputs
         (("rust-adler32" ,rust-adler32-1)
          ("rust-byteorder" ,rust-byteorder-1)
          ("rust-gzip-header" ,rust-gzip-header-0.3))
       #:cargo-development-inputs
         (("rust-miniz-oxide" ,rust-miniz-oxide-0.3))))))

(define-public rust-deflate-0.7
  (package
    (inherit rust-deflate-0.8)
    (name "rust-deflate")
    (version "0.7.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "deflate" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1d7d9fpmgjnznrksmd3vlv3dyw01wsrm11ifil6ag22871xnlyvh"))))
    (arguments
     `(#:cargo-inputs
       (("rust-adler32" ,rust-adler32-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-gzip-header" ,rust-gzip-header-0.3)
        ("rust-flate2" ,rust-flate2-1))))))

(define-public rust-deflate64-0.1
  (package
    (name "rust-deflate64")
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "deflate64" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
          "06scix17pa7wzzfsnhkycpcc6s04shs49cdaxx2k1sl0226jnsfs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-bytemuck" ,rust-bytemuck-1)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/anatawa12/deflate64-rs#readme")
    (synopsis "Deflate64 implementation based on .NET's implementation")
    (description "Deflate64 implementation based on .NET's implementation.")
    (license license:expat)))

(define-public rust-flate2-1
  (package
    (name "rust-flate2")
    (version "1.0.34")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "flate2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w1nf2ap4q1sq1v6v951011wcvljk449ap7q7jnnjf8hvjs8kdd1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cloudflare-zlib-sys" ,rust-cloudflare-zlib-sys-0.3)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-libz-ng-sys" ,rust-libz-ng-sys-1)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-libz-rs-sys" ,rust-libz-rs-sys-0.3)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.7))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/rust-lang/flate2-rs")
    (synopsis "Bindings to miniz.c for DEFLATE compression and decompression")
    (description
     "Bindings to miniz.c for DEFLATE compression and decompression exposed as
Reader/Writer streams.  Contains bindings for zlib, deflate, and gzip-based
streams.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gzip-header-1
  (package
    (name "rust-gzip-header")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gzip-header" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18lm2y96mahkmcd76pzyam2sl3v6lsl9mn8ajri9l0p6j9xm5k4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crc32fast" ,rust-crc32fast-1))))
    (home-page "https://github.com/oyvindln/gzip-header")
    (synopsis "Decoding and encoding the header part of gzip files")
    (description
     "This package provides a crate for decoding and encoding the header part
of gzip files based on the gzip header implementation in the @code{flate2} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gzip-header-0.3
  (package
    (inherit rust-gzip-header-1)
    (name "rust-gzip-header")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gzip-header" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fg6vm8sgsm69szwqyz7abfbyziv6pv0jkcailimlamvsfrzwc81"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crc32fast" ,rust-crc32fast-1))))))

(define-public rust-gzp-0.11
  (package
    (name "rust-gzp")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gzp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bvvz969c9kpyp7h6ry9mzhk7lb4hj4hpd810n0i26jjk4c5vip7"))
       (snippet
        #~(begin (use-modules ((guix build utils)))
                 ;; Switch the default from zlib-ng to zlib.
                 (substitute* "Cargo.toml"
                   (("\"deflate_zlib_ng\"") "\"deflate_zlib\""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-core-affinity" ,rust-core-affinity-0.8)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-flume" ,rust-flume-0.10)
                       ("rust-libdeflater" ,rust-libdeflater-0.12)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-snap" ,rust-snap-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list zlib))
    (home-page "https://github.com/sstadick/gzp")
    (synopsis "Parallel compression library")
    (description "This package provides a library for parallel compression.")
    (license (list license:unlicense license:expat))))

(define-public rust-libdeflate-sys-1
  (package
    (name "rust-libdeflate-sys")
    (version "1.23.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdeflate-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0hbrhz3jxjs4a3lnyw9c2wlfknmijb7ahxbs53kcnpvri9y6cfs1"))
              (snippet
               #~(begin (use-modules (guix build utils))
                        ;; Prefer the system libdeflate.
                        (substitute* "Cargo.toml"
                          (("^dynamic") "default = [\"dynamic\"]\ndynamic"))
                        ;; Accept any version of libdeflate.
                        (substitute* "build.rs"
                          ((".*exactly_version.*") ""))
                        (delete-file-recursively "libdeflate")))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list libdeflate))
    (home-page "https://github.com/adamkewley/libdeflater")
    (synopsis "Bindings to libdeflate")
    (description
     "This package provides bindings to libdeflate for DEFLATE (de)compression
exposed as non-streaming buffer operations.  It contains bindings for raw
deflate, zlib, and gzip data.")
    (license license:asl2.0)))

;; TODO: Unbundle libdeflate
(define-public rust-libdeflate-sys-0.12
  (package
    (inherit rust-libdeflate-sys-1)
    (name "rust-libdeflate-sys")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdeflate-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "11a7nn3p73vw3cnra36vz7mz60wx9jzhzwwji1hbmql5gy0v1xz1"))))
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1))))))

(define-public rust-libdeflate-sys-0.11
  (package
    (inherit rust-libdeflate-sys-1)
    (name "rust-libdeflate-sys")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdeflate-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0xs2wdly73ar7r9qyrbwnkwjjz4wd92kv5h78cdxfrsbp2v88ryb"))))
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1))))))

(define-public rust-libdeflate-sys-0.7
  (package
    (inherit rust-libdeflate-sys-0.11)
    (name "rust-libdeflate-sys")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdeflate-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0a5pkrxa7zcb0nab2j48dn6j8d8g1pjvz196c308wrax6dpazkjc"))))
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1))))))

