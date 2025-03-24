;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020, 2023-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023, 2024 Steve George <steve@futurile.net>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
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
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages pkg-config))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-autocompress-0.2
  (package
    (name "rust-autocompress")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "autocompress" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16pjdfr5b2ixs2xk3h6mvxprxr84rpaips624d6vbap5vsdkvzx4"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:tests? #false ;The crate does not include test files
      #:skip-build? #t  ; could not find `block` in `zstd`
      #:cargo-inputs
      `(("rust-brotli" ,rust-brotli-3)
        ("rust-bzip2" ,rust-bzip2-0.4)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-lz4" ,rust-lz4-1)
        ("rust-snap" ,rust-snap-1)
        ("rust-xz2" ,rust-xz2-0.1)
        ("rust-zstd" ,rust-zstd-0.9))
      #:cargo-development-inputs
      `(("rust-clap" ,rust-clap-2)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.4)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-temp-testdir" ,rust-temp-testdir-0.2))))
    (inputs (list xz))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/informationsea/autocompress-rs")
    (synopsis "Select decoder from magic bytes or encoder from file extension")
    (description
     "This crate lets you automatically select a suitable decoder from magic
bytes or encoder from file extension.")
    (license license:asl2.0)))

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

(define-public rust-cloudflare-zlib-sys-0.3
  (package
    (name "rust-cloudflare-zlib-sys")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cloudflare-zlib-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0vyd0l0vprvh9hc1ikllybrk8xc0lz9f509d2xgxgrpyxp8vch10"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/cloudflare/zlib")
    (synopsis
     "Cloudflare fork of zlib with massive performance improvements")
    (description
     "Cloudflare fork of zlib with massive performance improvements.")
    (license
     (list license:expat
           license:asl2.0
           license:zlib))))

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

(define-public rust-delharc-0.6
  (package
    (name "rust-delharc")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "delharc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18g5haj6bj92azif4jifhdy9vrv6blg3wyvpmxslh2gm2wkbm4qw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Test files are missing.
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-memchr" ,rust-memchr-2))
       #:cargo-development-inputs (("rust-crc-any" ,rust-crc-any-2)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/royaltm/rust-delharc")
    (synopsis "Parse and extract files from LHA/LZH archives")
    (description
     "This package provides a Rust library for parsing and extracting files
from LHA/LZH archives, which are often suffixed @code{.lha} or @code{.lzh}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fdeflate-0.3
  (package
    (name "rust-fdeflate")
    (version "0.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fdeflate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-simd-adler32" ,rust-simd-adler32-0.3))
       #:cargo-development-inputs (("rust-miniz-oxide" ,rust-miniz-oxide-0.7)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/image-rs/fdeflate")
    (synopsis "Fast specialized deflate implementation")
    (description
     "This package provides a fast, specialized deflate implementation.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-inflate-0.4
  (package
    (name "rust-inflate")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "inflate" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zxjdn8iwa0ssxrnjmywm3r1v284wryvzrf8vkc7nyf5ijbjknqw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-adler32" ,rust-adler32-1))))
    (home-page "https://github.com/PistonDevelopers/inflate.git")
    (synopsis "DEFLATE decoding")
    (description "This package provides DEFLATE decoding.")
    (license license:expat)))

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

(define-public rust-libdeflater-1
  (package
    (name "rust-libdeflater")
    (version "1.23.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdeflater" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0fj5a7766kk25rb8fr0i0grypdz1a3g5ij2nkh5mbh7f2z4pd0yp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libdeflate-sys" ,rust-libdeflate-sys-1))
       #:cargo-development-inputs
       (("rust-adler32" ,rust-adler32-1)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-flate2" ,rust-flate2-1))))
    (native-inputs (list pkg-config))
    (inputs (list libdeflate))
    (home-page "https://github.com/adamkewley/libdeflater")
    (synopsis "Bindings to libdeflate for DEFLATE")
    (description "This package provides bindings to libdeflate for DEFLATE
(de)compression exposed as non-streaming buffer operations.  Contains bindings
for raw deflate, zlib, and gzip data.")
    (license license:asl2.0)))

(define-public rust-libdeflater-0.12
  (package
    (inherit rust-libdeflater-1)
    (name "rust-libdeflater")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdeflater" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0cbrdvwhilvmk919izkp5bqgwfa7b8nj2ar9gp67nb345wl667k7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libdeflate-sys" ,rust-libdeflate-sys-0.12))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-flate2" ,rust-flate2-1))))))

(define-public rust-libflate-2
  (package
    (name "rust-libflate")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libflate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07mj9z89vbhq837q58m4v2nblgsmrn6vrp8w1j8g0kpa2kfdzna5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-adler32" ,rust-adler32-1)
                       ("rust-core2" ,rust-core2-0.4)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-dary-heap" ,rust-dary-heap-0.3)
                       ("rust-libflate-lz77" ,rust-libflate-lz77-2))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-4))))
    (home-page "https://github.com/sile/libflate")
    (synopsis "DEFLATE algorithm and related formats (ZLIB, GZIP)")
    (description "This package provides a Rust implementation of DEFLATE
algorithm and related formats (ZLIB, GZIP).")
    (license license:expat)))

(define-public rust-libflate-1
  (package
    (inherit rust-libflate-2)
    (name "rust-libflate")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libflate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "063xw2z477h3vh7j32y0f54a6nbndd7yf7rr5wpsvfw5nrqsxx2z"))))
    (arguments
     `(#:cargo-inputs
       (("rust-adler32" ,rust-adler32-1)
        ("rust-core2" ,rust-core2-0.4)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-libflate-lz77" ,rust-libflate-lz77-1))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2))))))

(define-public rust-libflate-lz77-2
  (package
    (name "rust-libflate-lz77")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libflate_lz77" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gc6h98jwigscasz8vw1vv65b3rismqcbndb8hf6yf4z6qxxgq76"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-core2" ,rust-core2-0.4)
        ("rust-hashbrown" ,rust-hashbrown-0.14)
        ("rust-rle-decode-fast" ,rust-rle-decode-fast-1))
       #:cargo-development-inputs (("rust-libflate" ,rust-libflate-2))))
    (home-page "https://github.com/sile/libflate")
    (synopsis "LZ77 encoder for libflate crate")
    (description "This package provides a LZ77 encoder for libflate crate.")
    (license license:expat)))

(define-public rust-libflate-lz77-1
  (package
    (inherit rust-libflate-lz77-2)
    (name "rust-libflate-lz77")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libflate_lz77" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gxc75fb2sk0xgrh3qxvxcx1l93yhmyxn9241r251wl5zj5klbd5"))))
    (arguments
     `(#:cargo-inputs
       (("rust-core2" ,rust-core2-0.4)
        ("rust-hashbrown" ,rust-hashbrown-0.13)
        ("rust-rle-decode-fast" ,rust-rle-decode-fast-1))
       #:cargo-development-inputs
       (("rust-libflate" ,rust-libflate-1))))))

(define-public rust-libz-ng-sys-1
  ;; TODO: Unbundle zlib-ng.
  (package
    (name "rust-libz-ng-sys")
    (version "1.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libz-ng-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0f54ffm7bzqdvmcxkv2as6ir9bgzhkaq0g1jgwkz2mns04d7adj4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cmake" ,rust-cmake-0.1)
        ("rust-libc" ,rust-libc-0.2))))
    (native-inputs
     (list cmake-minimal pkg-config zlib))
    (home-page "https://github.com/rust-lang/libz-sys")
    (synopsis "Low-level bindings to zlib-ng (libz-ng)")
    (description
     "This package provides low-level bindings to zlib-ng (libz-ng), a
high-performance zlib library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libz-rs-sys-0.3
  (package
    (name "rust-libz-rs-sys")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libz-rs-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vsnvkff9i4qxnid1xl7wrmhz8alvqw9z5lnpimpzzgrxr4r56q0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-zlib-rs" ,rust-zlib-rs-0.3))))
    (home-page "https://github.com/trifectatechfoundation/zlib-rs")
    (synopsis "Memory-safe zlib implementation written in Rust")
    (description
     "This package provides a memory-safe zlib implementation written in Rust.")
    (license license:zlib)))

(define-public rust-libz-sys-1
  (package
    (name "rust-libz-sys")
    (version "1.1.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libz-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
           "0wp4i6zl385ilmcqafv61jwsk1mpk6yb8gpws9nwza00x19n9lfj"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "src/zlib")
                 (delete-file-recursively "src/zlib-ng")))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ;; Build dependencies:
        ("rust-cc" ,rust-cc-1)
        ("rust-cmake" ,rust-cmake-0.1)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (native-inputs
     (list pkg-config zlib))
    (home-page "https://github.com/rust-lang/libz-sys")
    (synopsis "Bindings to the system libz library")
    (description
     "This package provides bindings to the system @code{libz} library (also
known as zlib).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-lz4-1
  (package
    (name "rust-lz4")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lz4" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wad97k0asgvaj16ydd09gqs2yvgaanzcvqglrhffv7kdpc2v7ky"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-lz4-sys" ,rust-lz4-sys-1))
       #:cargo-development-inputs
       (("rust-docmatic" ,rust-docmatic-0.1)
        ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/10xGenomics/lz4-rs")
    (synopsis "Rust LZ4 bindings library")
    (description "This crate provides Rust LZ4 bindings.")
    (license license:expat)))

(define-public rust-lz4-compress-0.1
  (package
    (name "rust-lz4-compress")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lz4-compress" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14cb8rpdfk6q3bjkf7mirpyzb6rvvcglqnayx6lvpa92m4rnb5hg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-0.5)
                       ("rust-quick-error" ,rust-quick-error-1))))
    (home-page "https://github.com/ticki/tfs")
    (synopsis "Pure Rust implementation of raw LZ4 compression/decompression")
    (description
     "Pure Rust implementation of raw LZ4 compression/decompression.")
    (license license:expat)))

(define-public rust-lz4-flex-0.11
  (package
    (name "rust-lz4-flex")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lz4_flex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n290fjvfi8jg20n6i0q77g8pqi5srnpgg7zhw1ppnlyd5bb5a9y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-twox-hash" ,rust-twox-hash-1))
       #:cargo-development-inputs (("rust-lz4-compress" ,rust-lz4-compress-0.1)
                                   ("rust-lzzzz" ,rust-lzzzz-1)
                                   ("rust-more-asserts" ,rust-more-asserts-0.3)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-snap" ,rust-snap-1))))
    (home-page "https://github.com/pseitz/lz4_flex")
    (synopsis "LZ4 implementation in Rust, no unsafe by default")
    (description "LZ4 implementation in Rust, no unsafe by default.")
    (license license:expat)))

(define-public rust-lz4-flex-0.9
  (package
    (inherit rust-lz4-flex-0.11)
    (name "rust-lz4-flex")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lz4_flex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18xm7s81bpfgvvrra2kknrbgfbi295diz90mkhxvr00phfrbp30s"))))
    (arguments
     `(#:cargo-inputs (("rust-twox-hash" ,rust-twox-hash-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-lz4-compress" ,rust-lz4-compress-0.1)
                                   ("rust-lzzzz" ,rust-lzzzz-1)
                                   ("rust-more-asserts" ,rust-more-asserts-0.2)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-snap" ,rust-snap-1))))))

(define-public rust-lz4-sys-1
  (package
    (name "rust-lz4-sys")
    (version "1.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lz4-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0059ik4xlvnss5qfh6l691psk4g3350ljxaykzv10yr0gqqppljp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/bozaro/lz4-rs")
    (synopsis "Rust LZ4 sys package")
    (description "This is the Rust LZ4 sys package.")
    (license license:expat)))

(define-public rust-lzma-rs-0.3
  (package
    (name "rust-lzma-rs")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lzma-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0phif4pnjrn28zcxgz3a7z86hhx5gdajmkrndfw4vrkahd682zi9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-crc" ,rust-crc-3)
        ("rust-env-logger" ,rust-env-logger-0.9)
        ("rust-log" ,rust-log-0.4))
       #:cargo-development-inputs
       (("rust-rust-lzma" ,rust-rust-lzma-0.5)))) ;; called rust-lzma in crates-io
    (native-inputs
      (list pkg-config xz))
    (home-page "https://github.com/gendx/lzma-rs")
    (synopsis "Codec for LZMA, LZMA2 and XZ written in pure Rust")
    (description
     "This package provides a codec for LZMA, LZMA2 and XZ written in pure Rust.")
    (license license:expat)))

(define-public rust-lzma-rust-0.1
  (package
    (name "rust-lzma-rust")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lzma-rust" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12dj3w2pnvx014pzjl8pc115rldgk6cbc7w6lwg24y2d3xfwzvfm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Only doc tests, which all fail.
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1))))
    (home-page "https://github.com/dyz1990/sevenz-rust/tree/main/lzma-rust")
    (synopsis "Codec LZMA/LZMA2")
    (description
     "This package provides LZMA/LZMA2 codec ported from tukaani xz for java'.")
    (license license:asl2.0)))

(define-public rust-lzma-sys-0.1
  (package
    (name "rust-lzma-sys")
    (version "0.1.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lzma-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09sxp20waxyglgn3cjz8qjkspb3ryz2fwx4rigkwvrk46ymh9njz"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "xz-5.2")))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-cc" ,rust-cc-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs
     (list pkg-config xz))
    (home-page "https://github.com/alexcrichton/xz2-rs")
    (synopsis "Bindings to liblzma for lzma and xz stream encoding/decoding")
    (description
     "This package contains the raw bindings to liblzma which contains an
implementation of LZMA and xz stream encoding/decoding.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-lzw-0.10
  (package
    (name "rust-lzw")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lzw" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1170dfskhzlh8h2bm333811hykjvpypgnvxyhhm1rllyi2xpr53x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nwin/lzw.git")
    (synopsis "LZW compression and decompression")
    (description
     "This package provides LZW compression and decompression.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lzzzz-1
  (package
    (name "rust-lzzzz")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lzzzz" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ggphn3ca5527jjq778z8hslqgmqymykgwcj63307b62r6hcr55c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1))
       #:cargo-development-inputs
       (("rust-assert-fs" ,rust-assert-fs-1)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/picoHz/lzzzz")
    (synopsis "Full-featured liblz4 binding for Rust")
    (description "Full-featured liblz4 binding for Rust.")
    (license license:expat)))

(define-public rust-miniz-oxide-0.8
  (package
    (name "rust-miniz-oxide")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miniz_oxide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wadxkg6a6z4lr7kskapj5d8pxlx7cp1ifw4daqnkzqjxych5n72"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-adler2" ,rust-adler2-2)
        ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
        ("rust-simd-adler32" ,rust-simd-adler32-0.3))))
    (home-page "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis "Pure rust replacement for the miniz DEFLATE/zlib encoder/decoder")
    (description
     "A pure rust replacement for the miniz DEFLATE/zlib encoder/decoder.  Using
@code{flate2} with the @code{rust_backend} feature provides an easy to use
streaming API for miniz_oxide.")
    (license (list license:expat license:zlib license:asl2.0))))

(define-public rust-miniz-oxide-0.7
  (package
    (inherit rust-miniz-oxide-0.8)
    (name "rust-miniz-oxide")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miniz_oxide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "024wv14aa75cvik7005s5y2nfc8zfidddbd7g55g7sjgnzfl18mq"))))
    (arguments
     `(#:cargo-inputs
       (("rust-adler" ,rust-adler-1)
        ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
        ("rust-simd-adler32" ,rust-simd-adler32-0.3))))))

(define-public rust-miniz-oxide-0.6
  (package
    (inherit rust-miniz-oxide-0.7)
    (name "rust-miniz-oxide")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "miniz_oxide" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1yp8z6yll5ypz1ldmgnv7zi0r78kbvmqmn2mii77jzmk5069axdj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-adler" ,rust-adler-1)
        ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
        ("rust-simd-adler32" ,rust-simd-adler32-0.3))))))

(define-public rust-miniz-oxide-0.5
  (package
    (inherit rust-miniz-oxide-0.6)
    (name "rust-miniz-oxide")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miniz_oxide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d2xcypr8s0skd81dhlrylas1j794qyz74snm11jc8kmy6l0nncn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-adler" ,rust-adler-1)
        ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
        ("rust-simd-adler32" ,rust-simd-adler32-0.3))))))

(define-public rust-miniz-oxide-0.4
  (package
    (inherit rust-miniz-oxide-0.5)
    (name "rust-miniz-oxide")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miniz_oxide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jsfv00hl5rmx1nijn59sr9jmjd4rjnjhh4kdjy8d187iklih9d9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-adler" ,rust-adler-1)
        ("rust-autocfg" ,rust-autocfg-1)
        ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))))

(define-public rust-miniz-oxide-0.3
  (package
    (inherit rust-miniz-oxide-0.4)
    (name "rust-miniz-oxide")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz_oxide" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0dblrhgbm0wa8jjl8cjp81akaj36yna92df4z1h9b26n3spal7br"))))
    (arguments
     `(#:cargo-inputs (("rust-adler32" ,rust-adler32-1))))))

(define-public rust-rust-lzma-0.5
  (package
    (name "rust-rust-lzma")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rust-lzma" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1njlmh9hq2qg5ssdangwbdkz1lrfj2brf8kfp65k7vmfmr6w0pc9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs
      (list pkg-config xz))
    (home-page "https://github.com/fpgaminer/rust-lzma")
    (synopsis
      "Simple interface for LZMA compression and decompression")
    (description
     "This package provides Simple interface for LZMA compression and decompression.")
    (license license:expat)))

(define-public rust-ruzstd-0.7
  (package
    (name "rust-ruzstd")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ruzstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17z83lw00pd3190yvdk2lksdi7my6wc0psgg07is6gf7pyb2kl7s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;missing data files
       #:cargo-inputs
       (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
        ("rust-twox-hash" ,rust-twox-hash-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/KillingSpark/zstd-rs")
    (synopsis "Decoder for the zstd compression format")
    (description
     "This package provides a decoder for the zstd compression format.")
    (license license:expat)))

(define-public rust-ruzstd-0.6
  (package
    (inherit rust-ruzstd-0.7)
    (name "rust-ruzstd")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ruzstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yygqpar2x910lnii4k5p43aj4943hlnxpczmqhsfddmxrqa8x2i"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; not all files included
         "--skip=tests::decode_corpus::test_decode_corpus_files"
         "--skip=tests::dict_test::test_dict_decoding"
         "--skip=tests::fuzz_regressions::test_all_artifacts"
         "--skip=tests::test_block_header_reading"
         "--skip=tests::test_decode_from_to"
         "--skip=tests::test_frame_decoder"
         "--skip=tests::test_frame_header_reading"
         "--skip=tests::test_specific_file"
         "--skip=tests::test_streaming")
       #:cargo-inputs
         (("rust-byteorder" ,rust-byteorder-1)
          ("rust-derive-more" ,rust-derive-more-0.99)
          ("rust-twox-hash" ,rust-twox-hash-1))
       #:cargo-development-inputs
         (("rust-criterion" ,rust-criterion-0.5)
          ("rust-rand" ,rust-rand-0.8))))))

(define-public rust-ruzstd-0.5
  (package
    (inherit rust-ruzstd-0.7)
    (name "rust-ruzstd")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ruzstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ga8jciw7ka3mxrzl39skmsbdslajghzglcil10g0z4rh65fpi2q"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; not all files included
         "--skip=tests::decode_corpus::test_decode_corpus_files"
         "--skip=tests::dict_test::test_dict_decoding"
         "--skip=tests::fuzz_regressions::test_all_artifacts"
         "--skip=tests::test_block_header_reading"
         "--skip=tests::test_decode_from_to"
         "--skip=tests::test_frame_decoder"
         "--skip=tests::test_frame_header_reading"
         "--skip=tests::test_specific_file"
         "--skip=tests::test_streaming")
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-twox-hash" ,rust-twox-hash-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-rand" ,rust-rand-0.8))))))

(define-public rust-ruzstd-0.4
  (package
    (inherit rust-ruzstd-0.5)
    (name "rust-ruzstd")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ruzstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p4ghqzkq36dy1x1ijnk7jmml4wi3v9bkfzlbm2hsnkiz6wglgxc"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; not all files included
         "--skip=tests::decode_corpus::test_decode_corpus_files"
         "--skip=tests::dict_test::test_dict_decoding"
         "--skip=tests::fuzz_regressions::test_all_artifacts"
         "--skip=tests::test_block_header_reading"
         "--skip=tests::test_decode_from_to"
         "--skip=tests::test_frame_decoder"
         "--skip=tests::test_frame_header_reading"
         "--skip=tests::test_specific_file"
         "--skip=tests::test_streaming")
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-thiserror-core" ,rust-thiserror-core-1)
                       ("rust-twox-hash" ,rust-twox-hash-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-rand" ,rust-rand-0.8))))))

(define-public rust-sevenz-rust-0.5
  (package
    (name "rust-sevenz-rust")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sevenz-rust" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0na67bs0ff01vicwwlc26sbh9jh4wpqn7988k31cixn5s231wfi3"))
       (snippet
        #~(begin (delete-file "tests/resources/decompress_x86.exe")))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            ;; Not all files included.
                            "--skip=decompress_lzma2_bcj_x86_file")
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-bit-set" ,rust-bit-set-0.5)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bzip2" ,rust-bzip2-0.4)
                       ("rust-cbc" ,rust-cbc-0.1)
                       ("rust-crc" ,rust-crc-3)
                       ("rust-filetime-creation" ,rust-filetime-creation-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-lzma-rust" ,rust-lzma-rust-0.1)
                       ("rust-nt-time" ,rust-nt-time-0.6)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-zstd" ,rust-zstd-0.13))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/dyz1990/sevenz-rust")
    (synopsis "7z decompressor/compressor")
    (description
     "This package provides a 7z decompressor/compressor written in pure rust.")
    (license license:asl2.0)))

(define-public rust-tar-0.4
  (package
    (name "rust-tar")
    (version "0.4.43")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tar" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xm1l6gg180wq9xrq9vhyyxxpr4kvyh933yjagax05wf7wqrhnf6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Test tarballs not included in crate.
       #:cargo-inputs
       (("rust-filetime" ,rust-filetime-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-xattr" ,rust-xattr-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/alexcrichton/tar-rs")
    (synopsis "Tar file reading/writing for Rust")
    (description
     "This package provides a Rust implementation of a TAR file reader and
writer.  This library does not currently handle compression, but it is abstract
over all I/O readers and writers.  Additionally, great lengths are taken to
ensure that the entire contents are never required to be entirely resident in
memory all at once.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unrar-0.5
  (package
    (name "rust-unrar")
    (version "0.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unrar" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a8cd2l2hgmc7h5vjkvsl71vas5l7xqjwmx4kh8z48m26mvnm7f9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-unrar-sys" ,rust-unrar-sys-0.5)
                       ("rust-widestring" ,rust-widestring-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/muja/unrar.rs")
    (synopsis "List and extract RAR archives")
    (description
     "This package provides functionality to list and extract RAR archives.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unrar-sys-0.5
  (package
    (name "rust-unrar-sys")
    (version "0.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unrar_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1npkjbqx77rsls2f2nhi6x83ass4cw9d27xl647gmzvr6h82b0rz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/muja/unrar.rs")
    (synopsis "FFI bindings to unrar")
    (description
     "This package provides FFI bindings to unrar (with minimal abstractions).")
    (license license:expat)))

(define-public rust-xz-0.1
  (package
    (name "rust-xz")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xz" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d6sq57g1969hjl5k7gzzdbyr60za9hk8qs9iqz26biazy87d21w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-xz2" ,rust-xz2-0.1))))
    (native-inputs
     (list pkg-config xz))
    (home-page "https://github.com/alexcrichton/xz2-rs")
    (synopsis "Alias of `xz2` crate")
    (description
     "Rust bindings to @code{liblzma} providing Read/Write streams as well as
low-level in-memory encoding/decoding.  Alias of @code{xz2} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-xz2-0.1
  (package
    (name "rust-xz2")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xz2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qk7nzpblizvayyq4xzi4b0zacmmbqr6vb9fc0v1avyp17f4931q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included in the tarball.
         "--skip=standard_files")
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-lzma-sys" ,rust-lzma-sys-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-tokio-core" ,rust-tokio-core-0.1))))
    (native-inputs
     (list pkg-config xz))
    (home-page "https://github.com/alexcrichton/xz2-rs")
    (synopsis "Rust bindings to liblzma")
    (description "This package provides Rust bindings to liblzma providing
Read/Write streams as well as low-level in-memory encoding and decoding.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zip-2
  (package
    (name "rust-zip")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zip" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "151lrzswjkhwzlr6dkmgbi4s51sa8dr496n6mwiswms0xa444pnw"))
       (modules '((guix build utils)))
       (snippet
         ;; loosen version requirement for rust-clap-4
         '(begin (substitute* "Cargo.toml"
                   (("version = \"=") "version = \"^"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;;tests missing
       #:cargo-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-arbitrary" ,rust-arbitrary-1)
        ("rust-bzip2" ,rust-bzip2-0.4)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-constant-time-eq" ,rust-constant-time-eq-0.3)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-deflate64" ,rust-deflate64-0.1)
        ("rust-displaydoc" ,rust-displaydoc-0.2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-indexmap" ,rust-indexmap-2)
        ("rust-lzma-rs" ,rust-lzma-rs-0.3)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-pbkdf2" ,rust-pbkdf2-0.12)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-zeroize" ,rust-zeroize-1)
        ("rust-zopfli" ,rust-zopfli-0.8)
        ("rust-zstd" ,rust-zstd-0.13))
       #:cargo-development-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-bencher" ,rust-bencher-0.1)
        ("rust-clap" ,rust-clap-4)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-time" ,rust-time-0.3)
        ("rust-walkdir" ,rust-walkdir-2))))
    (native-inputs (list pkg-config))
    (inputs (list (list zstd "lib")))
    (home-page "https://github.com/zip-rs/zip2")
    (synopsis "Library to support reading and writing Zip files")
    (description
     "Rust library for reading and writing Zip files.")
    (license license:expat)))

(define-public rust-zip-0.6
  (package
    (inherit rust-zip-2)
    (name "rust-zip")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zip" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qcjbqfvbwxi5g9wbymf2r05cvziic2qqj4xy64q3hp48vi980vn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-bzip2" ,rust-bzip2-0.4)
        ("rust-constant-time-eq" ,rust-constant-time-eq-0.1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-pbkdf2" ,rust-pbkdf2-0.11)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-time" ,rust-time-0.3)
        ("rust-zstd" ,rust-zstd-0.11))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-time" ,rust-time-0.3)
        ("rust-walkdir" ,rust-walkdir-2))))
    (native-inputs (list pkg-config))
    (inputs (list (list zstd "lib")))))

(define-public rust-zlib-rs-0.4
  (package
    (name "rust-zlib-rs")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zlib-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y24i695cwvm4frs98bhjznfa19by6hkb42np7abl4lk79ah3nma"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-quickcheck" ,rust-quickcheck-1))
       #:cargo-development-inputs (("rust-crc32fast" ,rust-crc32fast-1)
                                   ("rust-quickcheck" ,rust-quickcheck-1))))
    (home-page "https://github.com/trifectatechfoundation/zlib-rs")
    (synopsis "Memory-safe zlib implementation written in Rust")
    (description
     "This package provides a memory-safe zlib implementation written in Rust.")
    (license license:zlib)))

(define-public rust-zlib-rs-0.3
  (package
    (inherit rust-zlib-rs-0.4)
    (name "rust-zlib-rs")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zlib-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06kkjpqddvb5n8c24mmd3lmmcsy2yfwfsjyni8dggysayfd7r50b"))))
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-quickcheck" ,rust-quickcheck-1))))))

(define-public rust-zopfli-0.8
  (package
    (name "rust-zopfli")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zopfli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ip9azz9ldk19m0m1hdppz3n5zcz0cywbg1vx59g4p5c3cwry0g5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bumpalo" ,rust-bumpalo-3)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-lockfree-object-pool" ,rust-lockfree-object-pool-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-simd-adler32" ,rust-simd-adler32-0.3))
       #:cargo-development-inputs
       (("rust-miniz-oxide" ,rust-miniz-oxide-0.7)
       ("rust-proptest" ,rust-proptest-1)
       ("rust-proptest-derive", rust-proptest-derive-0.4))))
    (home-page "https://github.com/zopfli-rs/zopfli")
    (synopsis
      "Rust implementation of the Zopfli compression algorithm")
    (description
     "This package provides a Rust implementation of the Zopfli compression algorithm.")
    (license license:asl2.0)))

(define-public rust-zstd-0.13
  (package
    (name "rust-zstd")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
          "1ygkr6wspm9clbp7ykyl0rv69cfsf9q4lic9wcqiwn34lrwbgwpw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-zstd-safe" ,rust-zstd-safe-7))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-4)
                                   ("rust-humansize" ,rust-humansize-2)
                                   ("rust-partial-io" ,rust-partial-io-0.5)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (native-inputs (list pkg-config))
    (inputs (list (list zstd "lib")))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Binding to the zstd compression library")
    (description
     "This package provides a binding to the Zstd compression library.")
    (license license:expat)))

(define-public rust-zstd-0.12
  (package
    (inherit rust-zstd-0.13)
    (name "rust-zstd")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g654jj8z25rvzli2b1231pcp9y7n6vk44jaqwgifh9n2xg5j9qs"))))
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-zstd-safe" ,rust-zstd-safe-6))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-4)
                                   ("rust-humansize" ,rust-humansize-2)
                                   ("rust-partial-io" ,rust-partial-io-0.5)
                                   ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-zstd-0.11
  (package
    (inherit rust-zstd-0.13)
    (name "rust-zstd")
    (version "0.11.2+zstd.1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r7xlmgnifhxbfyid8vkcnd5ip16gx9hf89d1l0lzrpc4q1rdk10"))))
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-zstd-safe" ,rust-zstd-safe-5))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-3)
        ("rust-humansize" ,rust-humansize-1)
        ("rust-partial-io" ,rust-partial-io-0.5)
        ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-zstd-0.9
  (package
    (inherit rust-zstd-0.11)
    (name "rust-zstd")
    (version "0.9.2+zstd.1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0m5aik2jy2w1g68i4isa0c3gq9a7avq9abgjfjbc6f60yqdym413"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("\"=([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-zstd-safe" ,rust-zstd-safe-4))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-3)
        ("rust-humansize" ,rust-humansize-1)
        ("rust-partial-io" ,rust-partial-io-0.5)
        ("rust-walkdir" ,rust-walkdir-2))))
    (native-inputs '())
    (inputs '())))

(define-public rust-zstd-safe-7
  (package
    (name "rust-zstd-safe")
    (version "7.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-safe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
          "0nch85m5cr493y26yvndm6a8j6sd9mxpr2awrim3dslcnr6sp8sl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-zstd-sys" ,rust-zstd-sys-2))))
    (native-inputs (list pkg-config))
    (inputs (list (list zstd "lib")))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Safe low-level bindings to the zstd compression library")
    (description
     "This package provides safe low-level bindings to the zstd compression
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zstd-safe-6
  (package
    (inherit rust-zstd-safe-7)
    (name "rust-zstd-safe")
    (version "6.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-safe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10cm0v8sw3jz3pi0wlwx9mbb2l25lm28w638a5n5xscfnk8gz67f"))))
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-zstd-sys" ,rust-zstd-sys-2))))))

(define-public rust-zstd-safe-5
  (package
    (inherit rust-zstd-safe-7)
    (name "rust-zstd-safe")
    (version "5.0.2+zstd.1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-safe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nzl4q3xl68pq58g9xlym299bvjdii8cl7ix595ym7jgw22maahx"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-zstd-sys" ,rust-zstd-sys-2))))))

(define-public rust-zstd-safe-4
  (package
    (inherit rust-zstd-safe-5)
    (name "rust-zstd-safe")
    (version "4.1.6+zstd.1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-safe" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fz97qn7galkwl31n28cil44nxfj81ryd33v2vb041r7pd8irdll"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-zstd-sys" ,rust-zstd-sys-1))))
    (native-inputs '())
    (inputs '())))

;; TODO: Submit patches to use system zstd-seekable.
(define-public rust-zstd-seekable-0.1
  (package
    (name "rust-zstd-seekable")
    (version "0.1.23")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-seekable" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hn3lry2p1fzjlx6shwq1k5bcpx4lqckmvl16gqx326vbiy12jjp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; TODO: zstd-seekable fails to link during 'check phase.
       #:cargo-inputs (("rust-bincode" ,rust-bincode-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-threadpool" ,rust-threadpool-1))))
    (home-page "https://nest.pijul.com/pmeunier/zstd-seekable")
    (synopsis "Bindings to the seekable version of ZStandard")
    (description
     "This package provides bindings to the seekable version of ZStandard.")
    (license license:bsd-3)))

(define-public rust-zstd-sys-2
  (package
    (name "rust-zstd-sys")
    (version "2.0.13+zstd.1.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1almbackh06am0d2kc4a089n3al91jg3ahgg9kcrg3zfrwhhzzrq"))
       (snippet #~(begin
                    (use-modules (guix build utils))
                    (delete-file-recursively "zstd")))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list (list zstd "lib")))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Low-level bindings to the zstd compression library")
    (description "This package provides low-level Rust bindings to the zstd
compression library.")
    (license (list license:expat license:asl2.0))))

;; TODO: Unbundle zstd.
(define-public rust-zstd-sys-1
  (package
    (inherit rust-zstd-sys-2)
    (name "rust-zstd-sys")
    (version "1.6.3+zstd.1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a1f839k3mxh3qhjga1vd2sfxrsi41cqrnpyc5byfd6nr2jsyjgw"))))
    (arguments
     `(#:install-source? #f     ; invalid inclusion of reserved file name
       #:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.59)
        ("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))))

