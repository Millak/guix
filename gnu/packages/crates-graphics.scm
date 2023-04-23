;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020-2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Antoine Côté <antoine.cote@posteo.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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

(define-module (gnu packages crates-graphics)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video))

;;;
;;; Please: Try to add new module packages in alphabetic order.

(define-public rust-andrew-0.3
  (package
    (name "rust-andrew")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "andrew" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kx79z6mh7wwp4pz683bdya54h7w7wpzjcwf834fwbv4vl4znjlc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-rusttype" ,rust-rusttype-0.9)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-xdg" ,rust-xdg-2)
        ("rust-xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://github.com/trimental/andrew")
    (synopsis "Provides convenient drawing of objects to buffers")
    (description
     "The @code{andrew} crate provides convenient drawing of objects such as
shapes, lines and text to buffers.")
    (license license:expat)))

(define-public rust-andrew-0.2
  (package
    (inherit rust-andrew-0.3)
    (name "rust-andrew")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "andrew" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pmklwcwy8g1jras46fz8xcny779zfqpg4riksrbdhkjk3w0jzwv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-line-drawing" ,rust-line-drawing-0.7)
        ("rust-rusttype" ,rust-rusttype-0.7)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-xdg" ,rust-xdg-2)
        ("rust-xml-rs" ,rust-xml-rs-0.8))
       #:cargo-development-inputs
       (("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.4))))
    (inputs
     (list wayland))))

(define-public rust-ansi-colours-1
  (package
    (name "rust-ansi-colours")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ansi_colours" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03b2365y0ffkvqw61bc4imz6661jvi39vcs4q6q5d43znqrq4rrj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rgb" ,rust-rgb-0.8))
       #:cargo-development-inputs
       (("rust-crc64" ,rust-crc64-1)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-empfindung" ,rust-empfindung-0.2)
        ("rust-lab" ,rust-lab-0.11))))
    (home-page "https://github.com/mina86/ansi_colours")
    (synopsis "Palette converter between true-colour and ANSI terminal")
    (description
     "@code{ansi_colours} is a library which converts between 24-bit sRGB
colours and 8-bit colour palette used by ANSI terminals such as @code{xterm} on
@code{rxvt-unicode} in 256-colour mode.
The most common use case is when using 24-bit colours in a terminal emulator
which only support 8-bit colour palette.  This package allows true-colours to be
approximated by values supported by the terminal.")
    (license license:lgpl3+)))

(define-public rust-ansi-term-0.12
  (package
    (name "rust-ansi-term")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ljmkbilxgmhavxvxqa7qvm6f3fjggi7q2l3a72q9x0cxjvrnanm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles")
    (description
     "This is a library for controlling colours and formatting, such as red bold
text or blue underlined text, on ANSI terminals.")
    (license license:expat)))

(define-public rust-ansi-term-0.11
  (package
    (inherit rust-ansi-term-0.12)
    (name "rust-ansi-term")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-ansi-term-0.9
  (package
    (inherit rust-ansi-term-0.11)
    (name "rust-ansi-term")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ansi_term" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xif1bh938qpfc3d0f9xgidibpm65xix11w9gszwqnia00q7rb13"))))
    (arguments `())))

(define-public rust-aom-sys-0.3
  (package
    (name "rust-aom-sys")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aom-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0swg90iwypakh7vq77zwh34238c1r7vd5smj0vza7dv7xa22wh0g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.61)
         ("rust-system-deps" ,rust-system-deps-6))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list clang libaom llvm))
    (home-page "https://github.com/rust-av/aom-rs")
    (synopsis "FFI bindings to aom")
    (description "This package provides FFI bindings to aom.")
    (license license:expat)))

(define-public rust-ascii-canvas-2
  (package
    (name "rust-ascii-canvas")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ascii-canvas" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a9s8vrbc5jr6ry5ygjyfqmbs9gyya1v6dsxzsczpai8z4nvg3pz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t  ;; TODO: failes due to an unresolved import
       #:cargo-inputs
       (("rust-term" ,rust-term-0.5))))
    (home-page "https://github.com/nikomatsakis/ascii-canvas")
    (synopsis "Simple canvas for drawing lines and styled text and emitting to
the terminal")
    (description "@code{ASCII} canvas is a simple Rust library that allows you
to draw lines and colored text and then write them to the terminal.  It uses
the term library to handle the ANSI nonsense and hence it works on Windows,
Mac, and Unix.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-avif-parse-1
  (package
    (name "rust-avif-parse")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "avif-parse" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1vhd4n06g5mbf4sznz67mk352pw9hh97f4ysafp737xvzfd3zyw7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitreader" ,rust-bitreader-0.3)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-fallible-collections" ,rust-fallible-collections-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-static-assertions" ,rust-static-assertions-1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.9)
        ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/kornelski/avif-parse")
    (synopsis "Parser for AVIF image files")
    (description "This AVIF parser allows extracting the AV1 payload and alpha
channel metadata out of AVIF image files.  The parser is a fork of Mozilla's
MP4 parser used in Firefox, so it's designed to be robust and safely handle
untrusted data.")
    (license license:mpl2.0)))

(define-public rust-avif-parse-0.13
  (package
    (inherit rust-avif-parse-1)
    (name "rust-avif-parse")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "avif-parse" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vylrjq77mpl6flmd85j5f2qimh6vjn03syvq8agb62x56khm0xj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitreader" ,rust-bitreader-0.3)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-fallible-collections" ,rust-fallible-collections-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-static-assertions" ,rust-static-assertions-1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-avif-serialize-0.7
  (package
    (name "rust-avif-serialize")
    (version "0.7.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "avif-serialize" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0d2makdw756978i8s3qhlhh1h91y5maxriay6r4kmsmk8pky2qfc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f              ; Undeclared dependencies
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7))
       #:cargo-development-inputs (("rust-avif-parse" ,rust-avif-parse-1))))
    (home-page "https://lib.rs/avif-serialize")
    (synopsis "Writer for AVIF header structure (MPEG/HEIF/MIAF/ISO-BMFF)")
    (description
     "This package provides a minimal writer for AVIF header structure.  This
is a tiny alternative to @code{libavif}.  It creates the jungle of
MPEG/HEIF/MIAF/ISO-BMFF ``boxes'' as appropriate for AVIF files.  It supports
alpha channel embedding.")
    (license license:bsd-3)))

(define-public rust-avif-serialize-0.6
  (package
    (inherit rust-avif-serialize-0.7)
    (name "rust-avif-serialize")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "avif-serialize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "130wq838lslkcqcp2kjci7q3aq9qpir07pvxndc81xqbn63wvdjg"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-arrayvec" ,rust-arrayvec-0.5))))))

(define-public rust-cgl-0.3
  (package
    (name "rust-cgl")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zs7skrsyrsm759vfy2cygkx52fx91b567a12bpaz1sf4d8hbv8c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; only available on macOS
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/cgl-rs")
    (synopsis "Rust bindings for CGL on Mac")
    (description "Rust bindings for CGL on Mac.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cgl-0.2
  (package
    (inherit rust-cgl-0.3)
    (name "rust-cgl")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0j8ayr8pbwvyv6l8r7m5z197rs3pqn97085w9j4rfn7yfh5yrrsm"))))
    (arguments
     `(#:skip-build? #t     ; only available on macOS
       #:cargo-inputs
       (("rust-gleam" ,rust-gleam-0.6)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-cgmath-0.18
  (package
    (name "rust-cgmath")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgmath" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "05sk7c1c1jg5ygqvc3y77kxddp177gwazfibhd864ag3800x760s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-approx" ,rust-approx-0.4)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/brendanzab/cgmath")
    (synopsis "Linear algebra and mathematics library")
    (description
     "This package provides a linear algebra and mathematics library
for computer graphics.")
    (license license:asl2.0)))

(define-public rust-cgmath-0.17
  (package
    (inherit rust-cgmath-0.18)
    (name "rust-cgmath")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgmath" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rvgila6ivr0dh1bxza450a4yfwdi2pwj3h1vnwg0jy4xk6l8f98"))))
    (arguments
     `(#:skip-build? #t     ; Crate won't build without glium.
       #:cargo-inputs
       (("rust-approx" ,rust-approx-0.3)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-serde" ,rust-serde-1)
        ("rust-simd" ,rust-simd-0.2))
       #:cargo-development-inputs
       (;("rust-glium" ,rust-glium-0.23)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-cgmath-0.16
  (package
    (inherit rust-cgmath-0.17)
    (name "rust-cgmath")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgmath" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07754c03v3srzf64ghsl3fggrdi4kjy6l3vyq2d2wfjfixybb934"))))
    (arguments
     `(#:skip-build? #t     ; Crate won't build without glium.
       #:cargo-inputs
       (("rust-approx" ,rust-approx-0.1)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-num-traits" ,rust-num-traits-0.1)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-simd" ,rust-simd-0.2))
       #:cargo-development-inputs
       (;("rust-glium" ,rust-glium-0.19)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-core-graphics-0.22
  (package
    (name "rust-core-graphics")
    (version "0.22.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11lx6xw8nc9fpd552g60qa0cxh0maah8j2m26vkq0aslkgv3b7r6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics-types" ,rust-core-graphics-types-0.1)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-graphics-rs")
    (synopsis "Bindings to Core Graphics for macOS")
    (description
     "This package provides bindings to Core Graphics for macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-graphics-0.21
  (package
    (inherit rust-core-graphics-0.22)
    (name "rust-core-graphics")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i9gwzkil9k276317by0mi1pxz036h412dmcp1bzmlq4adj5anha"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics-types" ,rust-core-graphics-types-0.1)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-core-graphics-0.19
  (package
    (inherit rust-core-graphics-0.21)
    (name "rust-core-graphics")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08z9pgwfc0wb5v3ns7rnb2010q9g42b5vfwhp9fv4spawrs9725k"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.7)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-core-graphics-0.17
  (package
    (inherit rust-core-graphics-0.21)
    (name "rust-core-graphics")
    (version "0.17.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1acm3vygngnilzlr6klym5ywh7kfzh2xxrh2l41152hwmdl0jyan"))))
    (arguments
     `(#:skip-build? #t     ; only for macOS
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-core-graphics-types-0.1
  (package
    (name "rust-core-graphics-types")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12vqf0n5mjjcqjksdd82n2zh8hfda2zpiiqsr522c2266j5vcs1s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings for some fundamental Core Graphics types")
    (description
     "This package provides bindings for some fundamental Core Graphics
types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-video-sys-0.1
  (package
    (name "rust-core-video-sys")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-video-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a1qbn50jrb5hxrfshyb7y0f3pbf4ily6i6nciv7bn8ac4isvv1l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
        ("rust-core-graphics" ,rust-core-graphics-0.19)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-metal" ,rust-metal-0.18)
        ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/luozijun/rust-core-video-sys")
    (synopsis "Bindings to CoreVideo.framework for macOS and iOS")
    (description
     "This package provides bindings to CoreVideo.framework for macOS
and iOS.")
    (license license:expat)))

(define-public rust-dav1d-0.6
  (package
    (name "rust-dav1d")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dav1d" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qz2lx37pmx798lysgh6k5lk5y20ckr7pp8c1p6v2z0p721i913j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dav1d-sys" ,rust-dav1d-sys-0.3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list dav1d clang llvm))
    (home-page "https://github.com/rust-av/dav1d-rs")
    (synopsis "Libdav1d bindings")
    (description "This package provides libdav1d bindings in rust.")
    (license license:expat)))

(define-public rust-dav1d-sys-0.7
  (package
    (name "rust-dav1d-sys")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dav1d-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "16gzjsfnvfd5zr8mrx5n9mdd4vjvwfwpk9hfscgz7sjyzjdjzcm0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list dav1d))
    (home-page "https://github.com/rust-av/dav1d-rs")
    (synopsis "FFI bindings to dav1d")
    (description "This package provides FFI bindings to dav1d.")
    (license license:expat)))

(define-public rust-dav1d-sys-0.3
  (package
    (inherit rust-dav1d-sys-0.7)
    (name "rust-dav1d-sys")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dav1d-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10y8637snqc3kb9mhs8p9zi8171ba2hlbvhk06vs6hfifx60rr48"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.59)
        ("rust-system-deps" ,rust-system-deps-6))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list dav1d clang llvm))))

(define-public rust-dcv-color-primitives-0.4
  (package
    (name "rust-dcv-color-primitives")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dcv-color-primitives" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "01c0ag8dwzz00hyq9qam9d5j6yzdh8xpidcn37vgkzlmhgfz8mql"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-paste" ,rust-paste-1)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/aws/dcv-color-primitives")
    (synopsis "Library to perform image color model conversion")
    (description
     "This package provides a library to perform image color model conversion.")
    (license license:expat-0)))

(define-public rust-euclid-0.22
  (package
    (name "rust-euclid")
    (version "0.22.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "euclid" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "102blw7ljphi7i2xg435z0bb0a4npmwwbgyfinqxg1m0af2q55ns"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-arbitrary" ,rust-arbitrary-1)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/servo/euclid")
    (synopsis "Geometry primitives")
    (description "Geometry primitives written in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-eui48-0.4
  (package
    (name "rust-eui48")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "eui48" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0sqbmcnvilanzjagknmpf85pnji2b9hn2pqzd5rygrfkwikghk4c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-regex" ,rust-regex-1)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1))))
    (home-page "https://github.com/abaumhauer/eui48")
    (synopsis "Library to generate and parse IEEE EUI-48 and EUI-64")
    (description
     "This package provides a library to generate and parse IEEE EUI-48 and
EUI-64, also known as MAC-48 media access control addresses.")
    (license (list license:expat license:asl2.0))))

(define-public rust-eui48-0.3
  (package
    (inherit rust-eui48-0.4)
    (name "rust-eui48")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "eui48" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mmdhczfdxwv5v5h90ydqkx0mdqiv0h2clshm2cm4qlwp0gacw29"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-serde" ,rust-serde-1))))))

(define-public rust-exr-1
  (package
    (name "rust-exr")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "exr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1ssgq9zkf53mhwvhj9khvrlh0f9h3dl1pg7cs0irvn1fgvs5xbz8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                  ; Not all files included
       #:cargo-inputs
       (("rust-bit-field" ,rust-bit-field-0.10)
        ("rust-flume" ,rust-flume-0.10)
        ("rust-half" ,rust-half-2)
        ("rust-lebe" ,rust-lebe-0.5)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.6)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-threadpool" ,rust-threadpool-1)
        ("rust-zune-inflate" ,rust-zune-inflate-0.2))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-image" ,rust-image-0.24)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/johannesvollmer/exrs")
    (synopsis "Read and write OpenEXR files without any unsafe code")
    (description "Read and write OpenEXR files without any unsafe code")
    (license license:bsd-3)))

(define-public rust-gfx-0.18
  (package
    (name "rust-gfx")
    (version "0.18.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0nqmxqi3x4ni0g78g77a6aldrv8cfvzhnpqhxyd2ap4aa3wldph1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-draw-state" ,rust-draw-state-0.8)
        ("rust-gfx-core" ,rust-gfx-core-0.9))))
    (home-page "https://github.com/gfx-rs/gfx")
    (synopsis "High-performance, bindless graphics API")
    (description
     "This package provides a high-performance, bindless graphics API.")
    (license license:asl2.0)))

(define-public rust-gfx-core-0.9
  (package
    (name "rust-gfx-core")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx_core" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0haldr99n12d90vqgvl77n59hywlklhdff85j2aljaz1yapdvyvm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-draw-state" ,rust-draw-state-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-bitflags" ,rust-bitflags-1))))
    (home-page "https://github.com/gfx-rs/gfx")
    (synopsis "Core library of Gfx-rs")
    (description "This package is a core library of Gfx-rs.")
    (license license:asl2.0)))

(define-public rust-gif-0.11
  (package
    (name "rust-gif")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nsfd5qvp69z8kn17ziiq8zv4mclfycyxppf5k9fm2h8g1z1i9y3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-color-quant" ,rust-color-quant-1)
        ("rust-weezl" ,rust-weezl-0.1))))
    (home-page "https://github.com/image-rs/image-gif")
    (synopsis "GIF decoder and encoder")
    (description "This package provides a GIF decoder and encoder in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gif-0.10
  (package
    (inherit rust-gif-0.11)
    (name "rust-gif")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gif" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bw174f7civdfgryvc8pvyhicpr96hzdajnda4s3y8iv3ch907a7"))))
    (arguments
     `(#:tests? #f      ; tests not included in release
       #:cargo-inputs
       (("rust-color-quant" ,rust-color-quant-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-lzw" ,rust-lzw-0.10))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob-0.3))))))

(define-public rust-gl-0.14
  (package
    (name "rust-gl")
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "015lgy3qpzdw7mnh59a4y4hdjq1fhv7nkqlmh1h6fzc212qxlkm9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-glutin" ,rust-glutin-0.21))
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis "OpenGL bindings for rust")
    (description "This package provides OpenGL bindings for rust.")
    (license license:asl2.0)))

(define-public rust-gl-generator-0.14
  (package
    (name "rust-gl-generator")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gl-generator" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0k8j1hmfnff312gy7x1aqjzcm8zxid7ij7dlb8prljib7b1dz58s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-khronos-api" ,rust-khronos-api-3)
        ("rust-log" ,rust-log-0.4)
        ("rust-xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis "Code generators for bindings to the Khronos OpenGL APIs")
    (description
     "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (license license:asl2.0)))

(define-public rust-gl-generator-0.13
  (package
    (inherit rust-gl-generator-0.14)
    (name "rust-gl-generator")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gl-generator" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jpqjqpyrl73sf8y20p5rv50qz8glnsvv9infg8h4vi52zgbp66a"))))))

(define-public rust-gl-generator-0.11
 (package
   (inherit rust-gl-generator-0.13)
   (name "rust-gl-generator")
   (version "0.11.0")
   (source
    (origin
      (method url-fetch)
      (uri (crate-uri "gl-generator" version))
      (file-name
       (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "1gdchvay0k0g931b2ki33mkfixcw4radk5b8sqsm29rahxg3v8ir"))))))

(define-public rust-gleam-0.6
  (package
    (name "rust-gleam")
    (version "0.6.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gleam" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1iazvk3kvw3620gm6x8hy2x1lz51k04acl78cr3ppryhk5y0vqfa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.13))))
    (home-page "https://github.com/servo/gleam")
    (synopsis "Generated OpenGL bindings and wrapper for Servo")
    (description
     "Generated OpenGL bindings and wrapper for Servo.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-glutin-0.30
  (package
    (name "rust-glutin")
    (version "0.30.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05gzw9icj4s0p9db9srnwrd3m3plcs7260jlblyy2pbiqygap6zq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cfg-aliases" ,rust-cfg-aliases-0.1)
        ("rust-cgl" ,rust-cgl-0.3)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-dispatch" ,rust-dispatch-0.2)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.4)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.4)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.4)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-objc2" ,rust-objc2-0.3)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
        ("rust-wayland-sys" ,rust-wayland-sys-0.30)
        ("rust-windows-sys" ,rust-windows-sys-0.45)
        ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Cross-platform OpenGL context provider")
    (description "This package provides an OpenGL context provider.")
    (license license:asl2.0)))

(define-public rust-glutin-0.28
  (package
    (inherit rust-glutin-0.30)
    (name "rust-glutin")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lpnf61x4jbm55bpdr10k1a1pl3cs719i9y4qibsdj2bajz9vsh0"))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-cgl" ,rust-cgl-0.3)
        ("rust-cocoa" ,rust-cocoa-0.24)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.1)
        ("rust-glutin-emscripten-sys" ,rust-glutin-emscripten-sys-0.1)
        ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys-0.1)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.1)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-wayland-egl" ,rust-wayland-egl-0.29)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winit" ,rust-winit-0.26))))
    (inputs
     (list rust-wayland-client-0.29 rust-wayland-egl-0.29))))

(define-public rust-glutin-0.26
  (package
    (inherit rust-glutin-0.28)
    (name "rust-glutin")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18szbh4dixcr7pmymvbrpv21hv0wrpii5w03rv2534bb2ywwpq8s"))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-cgl" ,rust-cgl-0.3)
        ("rust-cocoa" ,rust-cocoa-0.23)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.1)
        ("rust-glutin-emscripten-sys" ,rust-glutin-emscripten-sys-0.1)
        ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys-0.1)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.1)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libloading" ,rust-libloading-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-wayland-egl" ,rust-wayland-egl-0.28)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winit" ,rust-winit-0.24))))
    (inputs
     (list rust-wayland-client-0.28 rust-wayland-egl-0.28))))

(define-public rust-glutin-0.22
  (package
    (inherit rust-glutin-0.26)
    (name "rust-glutin")
    (version "0.22.0-alpha5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0lilr4f335m1fq1acmshd51zblfaglw1hha6lhalnc1fw3cg0aag"))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-cgl" ,rust-cgl-0.3)
        ("rust-cocoa" ,rust-cocoa-0.19)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.1)
        ("rust-glutin-emscripten-sys" ,rust-glutin-emscripten-sys-0.1)
        ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys-0.1)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.1)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libloading" ,rust-libloading-0.5)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
        ("rust-parking-lot" ,rust-parking-lot-0.9)
        ("rust-wayland-client" ,rust-wayland-client-0.23)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winit" ,rust-winit-0.20))))))

(define-public rust-glutin-0.21
  (package
    (inherit rust-glutin-0.22)
    (name "rust-glutin")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ggyyqn7dvz4yx5ygqfvnxwfb78wvdm5y6xqw5my1b4x61dv6wak"))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-cgl" ,rust-cgl-0.2)
        ("rust-cocoa" ,rust-cocoa-0.18)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.1)
        ("rust-glutin-emscripten-sys" ,rust-glutin-emscripten-sys-0.1)
        ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys-0.1)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.1)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libloading" ,rust-libloading-0.5)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
        ("rust-parking-lot" ,rust-parking-lot-0.9)
        ("rust-wayland-client" ,rust-wayland-client-0.21)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winit" ,rust-winit-0.19))))))

(define-public rust-glutin-egl-sys-0.1
  (package
    (name "rust-glutin-egl-sys")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin-egl-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04f2ci9kb8q4dv4kviigvgfy54lr4jmbnmjsvi50qj13anjnmfra"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3)
        ("rust-gl-generator" ,rust-gl-generator-0.13))))
    (home-page "https://github.com/rust-windowing/glutin")
    (synopsis "Egl bindings for glutin")
    (description "The egl bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-emscripten-sys-0.1
  (package
    (name "rust-glutin-emscripten-sys")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_emscripten_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wb3qfxg3jh6ibb7bxmlmvf4jcpzck3pn0035g1sds3nvx343pl0"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Emscripten bindings for glutin")
    (description "This package provides Emscripten bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-gles2-sys-0.1
  (package
    (name "rust-glutin-gles2-sys")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_gles2_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00wisv3a7818bpw5nnqwibmh1bw032izix2l3657q2kkidq4w2g8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14)
        ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Gles2 bindings for glutin")
    (description "This package provides gles2 bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-glx-sys-0.1
  (package
    (name "rust-glutin-glx-sys")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_glx_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l8kk60kq5v6hl1qr6ym2arzvbsgkh71aa8485cp901bq27kqfby"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14)
        ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Glx bindings for glutin")
    (description "This package provides glx bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-wgl-sys-0.1
  (package
    (name "rust-glutin-wgl-sys")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_wgl_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15hns8b3i7iy366m61dg7jlr7wgzz8z8cakgbj3apnv92ld9b99x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Wgl bindings for glutin")
    (description "This package provides wgl bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-ical-0.7
  (package
    (name "rust-ical")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ical" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kvk1pgas67rnp0n4424lxxs8y3n1h0fw3ap8jbfcxqdmlap57sa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Peltoche/ical-rs")
    (synopsis "Ical/Vcard parser for Rust")
    (description
     "This library parse the ICalendar format defined in RFC5545, as well as
similar formats like VCard.")
    (license license:asl2.0)))

(define-public rust-image-0.24
  (package
    (name "rust-image")
    (version "0.24.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "image" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0iarjrn9rimnri1g0hagdzljq3v9fy6gy7qlmz80yyskkfafmdv9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f              ; Not all files included.
       #:cargo-inputs
       (("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-color-quant" ,rust-color-quant-1)
        ("rust-dav1d" ,rust-dav1d-0.6)
        ("rust-dcv-color-primitives" ,rust-dcv-color-primitives-0.4)
        ("rust-exr" ,rust-exr-1)
        ("rust-gif" ,rust-gif-0.11)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.3)
        ("rust-mp4parse" ,rust-mp4parse-0.12)
        ("rust-num-rational" ,rust-num-rational-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.17)
        ("rust-ravif" ,rust-ravif-0.8)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-tiff" ,rust-tiff-0.8)
        ("rust-webp" ,rust-webp-0.2))
       #:cargo-development-inputs
       (("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.3)
        ("rust-num-complex" ,rust-num-complex-0.4)
        ("rust-quickcheck" ,rust-quickcheck-1))))
    (home-page "https://github.com/image-rs/image")
    (synopsis "Imaging library written in Rust")
    (description
     "This package is an imaging library written in Rust.  It provides basic
filters and decoders for the most common image formats.")
    (license license:expat)))

(define-public rust-image-0.23
  (package
    (inherit rust-image-0.24)
    (name "rust-image")
    (version "0.23.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18gn2f7xp30pf9aqka877knlq308khxqiwjvsccvzaa4f9zcpzr4"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-color-quant" ,rust-color-quant-1)
        ("rust-gif" ,rust-gif-0.11)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-num-rational" ,rust-num-rational-0.3)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.16)
        ("rust-ravif" ,rust-ravif-0.6)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-tiff" ,rust-tiff-0.6))))))

(define-public rust-image-0.22
  (package
    (inherit rust-image-0.23)
    (name "rust-image")
    (version "0.22.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jpbd0p1q7xx6395ba9ikz2k4cfp26qczisa8m2v15w3hzd2mv88"))))
    (arguments
     `(#:tests? #f      ; Some test images are missing from the release.
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-gif" ,rust-gif-0.10)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-num-rational" ,rust-num-rational-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.15)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-tiff" ,rust-tiff-0.3))
       #:cargo-development-inputs
       (("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.9))))))

(define-public rust-image-0.21
  (package
    (inherit rust-image-0.22)
    (name "rust-image")
    (version "0.21.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1sv534xp8yyn7jj0q6yn2bgng1350f962g81sv8v7c6pgi31wdrm"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-gif" ,rust-gif-0.10)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-num-rational" ,rust-num-rational-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.14)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-tiff" ,rust-tiff-0.2))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob-0.3)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.6))))))

(define-public rust-image-0.20
  (package
    (inherit rust-image-0.21)
    (name "rust-image")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01058n0jcw25pq5shn7lkvywv8c28xsxb3nwwyb4r16ijm1mnrj4"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-gif" ,rust-gif-0.10)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-num-rational" ,rust-num-rational-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.12)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-tiff" ,rust-tiff-0.2))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob-0.2)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.6))))))

(define-public rust-imgref-1
  (package
    (name "rust-imgref")
    (version "1.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "imgref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b3czpz206z4nvpq7yq0v58bwjmqjwjmkr302hbzpp4523glkkxj"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/kornelski/imgref")
    (synopsis "2D slice of a @code{Vec}")
    (description
     "This package provides a trivial struct for interchange of 2d-dimensional
pixel buffers with width, height and stride.")
    (license license:cc0)))

(define-public rust-jpeg-decoder-0.3
  (package
    (name "rust-jpeg-decoder")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jpeg-decoder" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0gkv0zx95i4fr40fj1a10d70lqi6lfyia8r5q8qjxj8j4pj0005w"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Cargo.toml"
                    (("=0\\.2\\.83") "^0.2.83"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                  ; Not all files included
       #:cargo-inputs
       (("rust-rayon" ,rust-rayon-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-png" ,rust-png-0.16)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/image-rs/jpeg-decoder")
    (synopsis "JPEG decoder")
    (description "JPEG decoder written in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-jpeg-decoder-0.1
  (package
    (inherit rust-jpeg-decoder-0.3)
    (name "rust-jpeg-decoder")
    (version "0.1.22")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jpeg-decoder" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wnh0bmmswpgwhgmlizz545x8334nlbmkq8imy9k224ri3am7792"))))
    (arguments
     `(#:tests? #f      ; Some test files missing.
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-rayon" ,rust-rayon-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-png" ,rust-png-0.14)
        ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-libwebp-sys-0.4
  (package
    (name "rust-libwebp-sys")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libwebp-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1gvjaqhjpzdskx8x4q1lfgw24jnbjgkx4s6dxpkkg2d2ba4d37s3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=tests::poke")
       #:cargo-inputs
       (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/NoXF/libwebp-sys")
    (synopsis "Bindings to libwebp (bindgen, static linking)")
    (description "Bindings to libwebp (bindgen, static linking)")
    (license license:expat)))

(define-public rust-line-drawing-0.7
  (package
    (name "rust-line-drawing")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "line_drawing" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fcna7hq1g1kkkqy07hydscx5d2zgb6gskz3vnsvsif8h8ysvisw"))))
    (build-system cargo-build-system)
    (arguments
     ;; This version does not specify any versions on dependants.
     `(#:tests? #f      ; Cannot compile line_drawing for the test suite.
       #:cargo-inputs
       (("rust-num-traits" ,rust-num-traits-0.2))
       #:cargo-development-inputs
       (("rust-bresenham" ,rust-bresenham-0.1)
        ("rust-image" ,rust-image-0.22) ; 0.17?
        ("rust-rand" ,rust-rand-0.6))))
    (home-page "https://github.com/expenses/line_drawing")
    (synopsis "Collection of line-drawing algorithms")
    (description
     "This package provides a collection of line-drawing algorithms for use in
graphics and video games.")
    (license license:expat)))

(define-public rust-lyon-geom-0.17
  (package
    (name "rust-lyon-geom")
    (version "0.17.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lyon_geom" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "12al92qsh0f8ci3qf3533r4y5hxzzyfp972vm4wqzz9bq9vwx6ff"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
        (("rust-arrayvec" ,rust-arrayvec-0.5)
         ("rust-euclid" ,rust-euclid-0.22)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/nical/lyon")
    (synopsis "2D graphics rendering on the GPU using tessellation")
    (description
     "This package provides 2D graphics rendering on the GPU using tessellation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lyon-path-0.17
  (package
    (name "rust-lyon-path")
    (version "0.17.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lyon_path" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h7vbfaanf3x7xch6md4q4ja3xmvsi41n7d6ga40vjk7yzymj2jv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-lyon-geom" ,rust-lyon-geom-0.17)
        ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/nical/lyon")
    (synopsis "Types and utilities to store, build and iterate over 2D paths")
    (description
     "Types and utilities to store, build and iterate over 2D paths.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lyon-svg-0.17
  (package
    (name "rust-lyon-svg")
    (version "0.17.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lyon_svg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0fin6h9vwgz7gi2a1b0j9bp8a1aval2lvha1kzy7iijm4kgdcj3d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lyon-path" ,rust-lyon-path-0.17)
         ("rust-svgtypes" ,rust-svgtypes-0.5))))
    (home-page "https://github.com/nical/lyon")
    (synopsis "SVG helpers for the lyon crates")
    (description "This package provides SVG helpers for the lyon crates.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mp4parse-0.12
  (package
    (name "rust-mp4parse")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mp4parse" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1ppqv60qiyrnbb996gb1sik08c0j2i317llv3rrcwb1cjg3bdlk7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f              ; Not all files included.
       #:cargo-inputs
       (("rust-bitreader" ,rust-bitreader-0.3)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-fallible-collections" ,rust-fallible-collections-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-static-assertions" ,rust-static-assertions-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-test-assembler" ,rust-test-assembler-0.1)
        ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/mozilla/mp4parse-rust")
    (synopsis "Parser for ISO base media file format (mp4)")
    (description "Parser for ISO base media file format (mp4)")
    (license license:mpl2.0)))

(define-public rust-osmesa-sys-0.1
  (package
    (name "rust-osmesa-sys")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "osmesa-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fq1q1zcgfb0qydrg9r2738jlwc4hqxgb9vj11z72bjxx7kfrkw8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-shared-library" ,rust-shared-library-0.1))))
    (home-page "https://crates.io/crates/osmesa-sys")
    (synopsis "OSMesa library bindings for Rust")
    (description "This package provides OSMesa library bindings for Rust.")
    (license license:cc0)))

(define-public rust-piston-float-1
  (package
    (name "rust-piston-float")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-float" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0r35aasycms79hf2vf1ap40kkp8ywgl4hmfkf762dq8jwd3vw07r"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/pistondevelopers/float")
    (synopsis
      "Traits for generic floats in game development")
    (description
      "Traits for generic floats in game development")
    (license license:expat)))

(define-public rust-piston-gfx-texture-0.40
  (package
    (name "rust-piston-gfx-texture")
    (version "0.40.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-gfx_texture" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nr5awdgk3njfvfanszrv4gxz93f6skid1c8yijswccygripchqz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gfx" ,rust-gfx-0.18)
        ("rust-image" ,rust-image-0.22)
        ("rust-piston-texture" ,rust-piston-texture-0.8)
        ("rust-gfx-core" ,rust-gfx-core-0.9))))
    (home-page "https://github.com/pistondevelopers/gfx_texture")
    (synopsis
      "Gfx texture representation that works nicely with Piston libraries")
    (description "This package provides a Gfx texture representation that works
nicely with Piston libraries.")
    (license license:expat)))

(define-public rust-piston-graphics-api-version-0.2
  (package
    (name "rust-piston-graphics-api-version")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-graphics_api_version" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b5p6s45jqv057lpbxkiq3yrdjjhvcynmi2vjf8292rf0yh4hky5"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/PistonDevelopers/graphics_api_version")
    (synopsis
      "Library for storing graphics API versions")
    (description
      "This package provides a library for storing graphics API versions")
    (license license:expat)))

(define-public rust-piston-shaders-graphics2d-0.3
  (package
    (name "rust-piston-shaders-graphics2d")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-shaders_graphics2d" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dhh9bv4q19gdnj9d1nqq0yrvzs6gcn0c5j1p1f3xzyzq7d1gg4p"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/PistonDevelopers/shaders")
    (synopsis "Shaders for 2D graphics in Rust")
    (description "Shaders for 2D graphics in Rust")
    (license license:expat)))

(define-public rust-piston-texture-0.8
  (package
    (name "rust-piston-texture")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-texture" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pcv5my49b8xzqcb87wqh2ndgvr4s9ipys96s0h9j2plxrj3bjb2"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/pistondevelopers/texture")
    (synopsis "Generic library for textures")
    (description
      "This package provides a generic library for textures")
    (license license:expat)))

(define-public rust-piston-viewport-1
  (package
    (name "rust-piston-viewport")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-viewport" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16378hcy41b7x3zj2z4har0wq6fl4r62kf9p106jjl8hg2dv3aq1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-piston-float" ,rust-piston-float-1))))
    (home-page "https://github.com/PistonDevelopers/viewport")
    (synopsis "Library for storing viewport information")
    (description
     "This package provides a library for storing viewport information.")
    (license license:expat)))

(define-public rust-pistoncore-event-loop-0.49
  (package
    (name "rust-pistoncore-event-loop")
    (version "0.49.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-event_loop" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h9ij9vx42xg39198yxdlpk842pli5jqm2kwswiv3bqqcji0fwsm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-pistoncore-input" ,rust-pistoncore-input-0.28)
        ("rust-pistoncore-window" ,rust-pistoncore-window-0.44))))
    (home-page "https://github.com/PistonDevelopers/piston")
    (synopsis "Piston event loop for games and interactive applications")
    (description "This package provides a Piston event loop for games and
interactive applications.")
    (license license:expat)))

(define-public rust-pistoncore-input-0.28
  (package
    (name "rust-pistoncore-input")
    (version "0.28.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-input" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1rrcz9px098m3nx98gvrvzirfdp3vg03cblfkcrp4wnvswc0hwq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-piston-viewport" ,rust-piston-viewport-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-bitflags" ,rust-bitflags-1))))
    (home-page "https://github.com/PistonDevelopers/piston")
    (synopsis "Structure for user input")
    (description
     "This package provides a structure for user input.")
    (license license:expat)))

(define-public rust-pistoncore-window-0.44
  (package
    (name "rust-pistoncore-window")
    (version "0.44.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-window" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "18qy3nnpb9jczvkiyzzznamck0pzgiyi6073jrkldnci6b3in10q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-piston-graphics-api-version"
         ,rust-piston-graphics-api-version-0.2)
        ("rust-pistoncore-input" ,rust-pistoncore-input-0.28))))
    (home-page "https://github.com/PistonDevelopers/piston")
    (synopsis "Library for window abstraction")
    (description
     "This package provides a library for window abstraction.")
    (license license:expat)))

(define-public rust-png-0.17
  (package
    (name "rust-png")
    (version "0.17.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f66slx641p7an277xskz8vq7syy9cmhsx1qwnfb268ahspqww2x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #false                  ;XXX missing files in tarball
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-deflate" ,rust-deflate-1)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.6))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-getopts" ,rust-getopts-0.2)
        ("rust-glium" ,rust-glium-0.31)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-term" ,rust-term-0.7))))
    (home-page "https://github.com/image-rs/image-png")
    (synopsis "PNG decoding and encoding library in pure Rust")
    (description
     "This package is a PNG decoding and encoding library in pure Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-png-0.16
  (package
    (inherit rust-png-0.17)
    (name "rust-png")
    (version "0.16.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ipl44q3vy4kvx6j296vk7d4v8gvcg203lrkvvixwixq1j98fciw"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-deflate" ,rust-deflate-0.8)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.3))))))

(define-public rust-png-0.15
  (package
    (inherit rust-png-0.16)
    (name "rust-png")
    (version "0.15.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "10x2qkhyfnm3si5vgx77r2ik811gaap7ahi825wfxgsb0lirm1gg"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-deflate" ,rust-deflate-0.7)
        ("rust-inflate" ,rust-inflate-0.4))
       #:cargo-development-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ;; TODO: glium has many cyclic dependencies with other packages
        ;;("rust-glium" ,rust-glium-0.24)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-term" ,rust-term-0.6))))))

(define-public rust-png-0.14
  (package
    (inherit rust-png-0.15)
    (name "rust-png")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nf3a8r9p9zrj4x30b48f7yv18dz9xkmrq9b3lnzmpnhzn0z9nk3"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-deflate" ,rust-deflate-0.7)
        ("rust-inflate" ,rust-inflate-0.4)
        ("rust-num-iter" ,rust-num-iter-0.1))
       #:cargo-development-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ;; TODO: glium has many cyclic dependencies with other packages
        ;; ("rust-glium" ,rust-glium-0.22)
        ("rust-glob" ,rust-glob-0.2)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-term" ,rust-term-0.4))))))

(define-public rust-png-0.12
  (package
    (inherit rust-png-0.14)
    (name "rust-png")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nqlc8lqf8ncv3kj0gzlxwli61dbbxcjlrp176kvilw4sl09cjzm"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-deflate" ,rust-deflate-0.7)
        ("rust-inflate" ,rust-inflate-0.4)
        ("rust-num-iter" ,rust-num-iter-0.1))
       #:cargo-development-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ;; TODO: gluum has many cyclic dependencies with other packages
        ;; ("rust-glium" ,rust-glium-0.21)
        ("rust-glob" ,rust-glob-0.2)
        ("rust-term" ,rust-term-0.4))))))

(define-public rust-ravif-0.8
  (package
    (name "rust-ravif")
    (version "0.8.10+rust-1.67.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ravif" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1r3s78781kb9lwysdvpdc80gavly33dcs4inhhp2dawml9g3rjss"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-avif-serialize" ,rust-avif-serialize-0.7)
        ("rust-imgref" ,rust-imgref-1)
        ("rust-loop9" ,rust-loop9-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-quick-error" ,rust-quick-error-2)
        ("rust-rav1e" ,rav1e)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rgb" ,rust-rgb-0.8))
       #:cargo-development-inputs
       (("rust-avif-parse" ,rust-avif-parse-1))))
    (native-inputs
     (list nasm pkg-config))
    (inputs
     (list libgit2 zlib))
    (home-page "https://lib.rs/ravif")
    (synopsis "Rust library for encoding images in AVIF format")
    (description
     "This package provides a rav1e-based pure Rust library for encoding images in
AVIF format (powers the `cavif` tool).")
    (license license:bsd-3)))

(define-public rust-ravif-0.6
  (package
    (inherit rust-ravif-0.8)
    (name "rust-ravif")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ravif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gyc7w1fz3qdk95cdpkj185dm6lskxfp329xm69waxc565fcz9rx"))))
    (arguments
     `(#:cargo-inputs
       (("rust-avif-serialize" ,rust-avif-serialize-0.6)
        ("rust-imgref" ,rust-imgref-1)
        ("rust-loop9" ,rust-loop9-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rav1e" ,rav1e)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rgb" ,rust-rgb-0.8))
       #:cargo-development-inputs
       (("rust-avif-parse" ,rust-avif-parse-0.13))))
    (native-inputs
     (list nasm))                 ;for building rav1e
    (inputs '())))

(define-public rust-raw-window-handle-0.4
  (package
    (name "rust-raw-window-handle")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "raw-window-handle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0hgvrqbr2b62zhq4ryv08h92mwis9v8f7j9pwcgxzlp7nswvw05q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cty" ,rust-cty-0.2))))
    (home-page "https://github.com/rust-windowing/raw-window-handle")
    (synopsis "Interoperability library for Rust Windowing applications")
    (description
     "Interoperability library for Rust Windowing applications.")
    (license license:expat)))

(define-public rust-raw-window-handle-0.3
  (package
    (inherit rust-raw-window-handle-0.4)
    (name "rust-raw-window-handle")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "raw-window-handle" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04c2wir7qq3g2b143yav52a1g5ack8ffqx2bpmrn9bc0dix1li0a"))))
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))))

(define-public rust-rgb-0.8
  (package
    (name "rust-rgb")
    (version "0.8.36")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rgb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ncgzkgifbyfx7vpnygfl4mgpdhhbaywxybx6pnjraf77wz2vv10"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://lib.rs/crates/rgb")
    (synopsis "Struct for sharing pixels between crates")
    (description
     "This package provides @code{struct RGB/RGBA/etc.} for sharing pixels
between crates + convenience methods for color manipulation.  It allows no-copy
high-level interoperability.  It also adds common convenience methods and
implements standard Rust traits to make `RGB`/`RGBA` pixels and slices
first-class Rust objects.")
    (license license:expat)))

(define-public rust-smithay-client-toolkit-0.15
  (package
    (name "rust-smithay-client-toolkit")
    (version "0.15.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18wxla80y6m4l3dwawi7bl1d9m9dfcg4sxxjcgjqq3psjxmg2a4a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.9)
        ("rust-dlib" ,rust-dlib-0.5)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-memmap2" ,rust-memmap2-0.3)
        ("rust-nix" ,rust-nix-0.22)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-wayland-client" ,rust-wayland-client-0.29)
        ("rust-wayland-cursor" ,rust-wayland-cursor-0.29)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.29))))
    (home-page "https://github.com/smithay/client-toolkit")
    (synopsis "Toolkit for making client Wayland applications")
    (description
     "This package provides a toolkit for making client Wayland applications.")
    (license license:expat)))

(define-public rust-smithay-client-toolkit-0.12
  (package
    (inherit rust-smithay-client-toolkit-0.15)
    (name "rust-smithay-client-toolkit")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rjdszpf8pns99gyy8f5axf01ckc33f30dddfazyfg45xfii6vii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-andrew" ,rust-andrew-0.3)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.6)
        ("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-memmap2" ,rust-memmap2-0.1)
        ("rust-nix" ,rust-nix-0.18)
        ("rust-wayland-client" ,rust-wayland-client-0.28)
        ("rust-wayland-cursor" ,rust-wayland-cursor-0.28)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.28))))))

(define-public rust-smithay-client-toolkit-0.6
  (package
    (inherit rust-smithay-client-toolkit-0.12)
    (name "rust-smithay-client-toolkit")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0m20687zs36l6xak2s5k9s7qp78ly8xfjpbmrhacp7whfn4hx5lk"))))
    (arguments
     `(#:cargo-inputs
       (("rust-andrew" ,rust-andrew-0.2)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-memmap" ,rust-memmap-0.7)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-client" ,rust-wayland-client-0.23)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.23))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-image" ,rust-image-0.21)
        ("rust-wayland-client" ,rust-wayland-client-0.23))))))

(define-public rust-smithay-client-toolkit-0.4
  (package
    (inherit rust-smithay-client-toolkit-0.6)
    (name "rust-smithay-client-toolkit")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1yj8yzd0lhqpsgq0x4iikl9a02q2hnkky81brk938alv0ibqrjrc"))))
    (arguments
     `(#:cargo-inputs
       (("rust-andrew" ,rust-andrew-0.2)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-memmap" ,rust-memmap-0.7)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-client" ,rust-wayland-client-0.21)
        ("rust-wayland-commons" ,rust-wayland-commons-0.21)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.21))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-image" ,rust-image-0.20)
        ("rust-wayland-client" ,rust-wayland-client-0.21))))))

(define-public rust-smithay-clipboard-0.6
  (package
    (name "rust-smithay-clipboard")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-clipboard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14dwisd56cbr80zf719l3fh0n8pm1fjmvry9lsbhdbccf8cv525b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.12)
        ("rust-wayland-client" ,rust-wayland-client-0.28))))
    (home-page "https://github.com/smithay/smithay-clipboard")
    (synopsis "Access to the Wayland clipboard for client applications")
    (description
     "This package provides access to the Wayland clipboard for client
applications.")
    (license license:expat)))

(define-public rust-tiff-0.8
  (package
    (name "rust-tiff")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tiff" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0wg4a6w8sakyy0mggblg340mx8bgglx9hwsxsn8g5fpjkx7k6jbl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f              ; Not all files included
       #:cargo-inputs
       (("rust-flate2" ,rust-flate2-1)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.3)
        ("rust-weezl" ,rust-weezl-0.1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/image-rs/image-tiff")
    (synopsis "TIFF decoding and encoding library in pure Rust")
    (description
     "This package provides TIFF decoding and encoding library in pure Rust.")
    (license license:expat)))

(define-public rust-tiff-0.6
  (package
    (inherit rust-tiff-0.8)
    (name "rust-tiff")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ds48vs919ccxa3fv1www7788pzkvpg434ilqkq7sjb5dmqg8lws"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.4)
        ("rust-weezl" ,rust-weezl-0.1))))))

(define-public rust-tiff-0.5
  (package
    (inherit rust-tiff-0.6)
    (name "rust-tiff")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tiff" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0bzzvxcx21pzryxgd7x7a1himiqs2y4k55754wzlr56sqj3qlfrz"))))
    (arguments
     `(#:tests? #f      ; not all test files included
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.3))))))

(define-public rust-tiff-0.3
  (package
    (inherit rust-tiff-0.5)
    (name "rust-tiff")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiff" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zgmbny2f8rssqmjdfvnysy0vqwcvlwl6q9f5yixhavlqk7w5dyp"))))
    (arguments
     `(#:tests? #f      ; Tests images not included with release.
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-num-derive" ,rust-num-derive-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-tiff-0.2
  (package
    (inherit rust-tiff-0.3)
    (name "rust-tiff")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiff" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1kn7psgpacns337vvqh272rkqwnakmjd51rc7ygwnc03ibr38j0y"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-num-derive" ,rust-num-derive-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2))))))

(define-public rust-wayland-client-0.29
  (package
    (name "rust-wayland-client")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05b7qikqj22rjy17kqw5ar7j2chpy18dr0gqapvwjfd00n60cfrz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #false               ;circular dependency on wayland-protocols
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nix" ,rust-nix-0.24)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-wayland-commons" ,rust-wayland-commons-0.29)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.29)
        ("rust-wayland-sys" ,rust-wayland-sys-0.29))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     (list rust-bitflags-1
           rust-downcast-rs-1
           rust-libc-0.2
           rust-nix-0.24
           rust-scoped-tls-1
           rust-wayland-commons-0.29
           rust-wayland-scanner-0.29
           rust-wayland-sys-0.29))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Rust bindings to the standard C implementation of the wayland protocol")
    (description
     "This package provides Rust bindings to the standard C implementation of
the wayland protocol, client side.")
    (license license:expat)))

(define-public rust-wayland-client-0.28
  (package
    (inherit rust-wayland-client-0.29)
    (name "rust-wayland-client")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mxnflzv9s3qpcp0z7kqvrzki5bknfar9n9yky06f8ivs00vxgdx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nix" ,rust-nix-0.18)
        ("rust-scoped-tls" ,rust-scoped-tls-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     (list rust-bitflags-1
           rust-downcast-rs-1
           rust-libc-0.2
           rust-nix-0.18
           rust-scoped-tls-1
           rust-wayland-commons-0.28
           rust-wayland-scanner-0.28
           rust-wayland-sys-0.28))))

(define-public rust-wayland-client-0.23
  (package
    (inherit rust-wayland-client-0.28)
    (name "rust-wayland-client")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nmw2kz70llc5mxwzg6bglnqy0qnyr9224zjmq9czazgw3mq045g"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-commons" ,rust-wayland-commons-0.23)
        ("rust-wayland-sys" ,rust-wayland-sys-0.23)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.23))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs `())))

(define-public rust-wayland-client-0.21
  (package
    (inherit rust-wayland-client-0.23)
    (name "rust-wayland-client")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04r7dy074hhdalsi1day482wvmczr40hg7qvrnzkgxpakrgkx5j9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-commons" ,rust-wayland-commons-0.21)
        ("rust-wayland-sys" ,rust-wayland-sys-0.21)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.21))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-wayland-commons-0.29
  (package
    (name "rust-wayland-commons")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-commons" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00m90bnxqy0d6lzqlyazc1jh18jgbjwigmyr0rk3m8w4slsg34c6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.24)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-wayland-sys" ,rust-wayland-sys-0.29))))
    (inputs
     (list rust-nix-0.24 rust-once-cell-1 rust-smallvec-1
           rust-wayland-sys-0.29))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Types and structures used by wayland-client and wayland-server")
    (description
     "This package provides common types and structures used by wayland-client
and wayland-server.")
    (license license:expat)))

(define-public rust-wayland-commons-0.28
  (package
    (inherit rust-wayland-commons-0.29)
    (name "rust-wayland-commons")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-commons" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mid1sgy3bmiywnrhsr31b8w6zvk1ll2ci2as15ddv8pczvm0128"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.18)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-smallvec" ,rust-smallvec-1))))
    (inputs
     (list rust-nix-0.18 rust-once-cell-1 rust-smallvec-1
           rust-wayland-sys-0.28))))

(define-public rust-wayland-commons-0.23
  (package
    (inherit rust-wayland-commons-0.28)
    (name "rust-wayland-commons")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-commons" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nyvcs6xxxzqgh0wvc7z0fgi89bf3h9p4qrbf77bnfbwlb8v0rmv"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-sys" ,rust-wayland-sys-0.23))))
    (inputs `())))

(define-public rust-wayland-commons-0.21
  (package
    (inherit rust-wayland-commons-0.23)
    (name "rust-wayland-commons")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-commons" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1v1jpcsnn6cwwy5ii5pdl58i6b9slmi8mn4my4fpwrlbfsb8ih20"))))
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-sys" ,rust-wayland-sys-0.21))))))

(define-public rust-wayland-cursor-0.29
  (package
    (name "rust-wayland-cursor")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-cursor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qbn6wqmjibkx3lb3ggbp07iabzgx2zhrm0wxxxjbmhkdyvccrb8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.24)
        ("rust-wayland-client" ,rust-wayland-client-0.29)
        ("rust-xcursor" ,rust-xcursor-0.3))))
    (inputs
     (list rust-nix-0.24 rust-wayland-client-0.29 rust-xcursor-0.3))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Bindings to libwayland-cursor")
    (description
     "This crate provides helpers to load the system provided cursor images
and load them into WlBuffers as well as obtain the necessary metadata to
properly display animated cursors.")
    (license license:expat)))

(define-public rust-wayland-cursor-0.28
  (package
    (inherit rust-wayland-cursor-0.29)
    (name "rust-wayland-cursor")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-cursor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pvf96a9hg7b40vyvamcg491sa0006fr9bzf1xkaf8q22qn15syn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.18)
        ("rust-xcursor" ,rust-xcursor-0.3))))
    (inputs
     (list rust-nix-0.18 rust-wayland-client-0.28 rust-xcursor-0.3))))

(define-public rust-wayland-egl-0.29
  (package
    (name "rust-wayland-egl")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-egl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z8hwixv5kj201p2pnbdwxbl4s9hz5cxd8i1v0k2j08sz14yjba0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-wayland-client" ,rust-wayland-client-0.29)
        ("rust-wayland-sys" ,rust-wayland-sys-0.29))))
    (inputs
     (list rust-wayland-client-0.29 rust-wayland-sys-0.29))
    ;; For the PKG_CONFIG_PATH environment variable.
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Bindings to libwayland-egl")
    (description
     "This crate provides bindings for OpenGL/Vulkan support for
Wayland client apps.  It creates an EGLSurface from any
WlSurface, which can then play the role of the base surface for
initializing an OpenGL or Vulkan context.")
    (license license:expat)))

(define-public rust-wayland-egl-0.28
  (package
    (inherit rust-wayland-egl-0.29)
    (name "rust-wayland-egl")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-egl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xd7iap0x4sidmy9dv02cdnxjhnbk9li7r7f39x9cg0i8xs50ly6"))))
    (build-system cargo-build-system)
    (inputs
     (list rust-wayland-client-0.28 rust-wayland-sys-0.28))
    (native-inputs
     (list pkg-config))))

(define-public rust-wayland-protocols-0.29
  (package
    (name "rust-wayland-protocols")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ihbjyd0w460gd7w22g9qabbwd4v8x74f8vsh7p25csljcgn4l5r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-wayland-client" ,rust-wayland-client-0.29)
        ("rust-wayland-commons" ,rust-wayland-commons-0.29)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.29)
        ("rust-wayland-server" ,rust-wayland-server-0.29))))
    (inputs
     (list rust-bitflags-1 rust-wayland-client-0.29
           rust-wayland-commons-0.29 rust-wayland-scanner-0.29
           rust-wayland-server-0.29))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generated API for the officials Wayland protocol extensions")
    (description
     "This package provides a generated API for the officials Wayland protocol
extensions.")
    (license license:expat)))

(define-public rust-wayland-protocols-0.28
  (package
    (inherit rust-wayland-protocols-0.29)
    (name "rust-wayland-protocols")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c0sw13qssrvf3jgygwqpiimpaagz3haxn9jridd4k85sfs856ii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1))))
    (inputs
     (list rust-bitflags-1 rust-wayland-client-0.28
           rust-wayland-commons-0.28 rust-wayland-scanner-0.28
           rust-wayland-server-0.28))))

(define-public rust-wayland-protocols-0.23
  (package
    (inherit rust-wayland-protocols-0.28)
    (name "rust-wayland-protocols")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ygwbzqlnks5xzafka3c8ag6k92g2h6ygj2xsmvjfx2n6rj8dhkc"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-wayland-client" ,rust-wayland-client-0.23)
        ("rust-wayland-commons" ,rust-wayland-commons-0.23)
        ("rust-wayland-server" ,rust-wayland-server-0.23)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.23))))
    (inputs `())))

(define-public rust-wayland-protocols-0.21
  (package
    (inherit rust-wayland-protocols-0.23)
    (name "rust-wayland-protocols")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0i91yh3nxk9llhly2ly3nvlfx0lbpvyq919cgmnyx3j25bmf5zaa"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-wayland-client" ,rust-wayland-client-0.21)
        ("rust-wayland-commons" ,rust-wayland-commons-0.21)
        ("rust-wayland-server" ,rust-wayland-server-0.21)
        ("rust-wayland-sys" ,rust-wayland-sys-0.21)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.21))))))

(define-public rust-wayland-scanner-0.29
  (package
    (name "rust-wayland-scanner")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lxx3i2kxnmsk421qx87lqqc9kd2y1ksjxcyg0pqbar2zbc06hwg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-xml-rs" ,rust-xml-rs-0.8))))
    (inputs
     (list rust-proc-macro2-1 rust-quote-1 rust-xml-rs-0.8))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generate Rust APIs from XML Wayland protocol files")
    (description
     "Wayland Scanner generates Rust APIs from XML Wayland protocol files.
It is intended for use with wayland-sys.  You should only need this crate if
you are working on custom Wayland protocol extensions.  Look at the
wayland-client crate for usable bindings.")
    (license license:expat)))

(define-public rust-wayland-scanner-0.28
  (package
    (inherit rust-wayland-scanner-0.29)
    (name "rust-wayland-scanner")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g8ky63qk27in7zajycj3fyydsxlj19hanfcvr8d7z5kcxbvl43h"))))
    (inputs
     (list rust-proc-macro2-1 rust-quote-1 rust-xml-rs-0.8))))

(define-public rust-wayland-scanner-0.23
  (package
    (inherit rust-wayland-scanner-0.28)
    (name "rust-wayland-scanner")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0g8wcphykjrcpslznyi3qccx1pckw97rckq5b295nfbg6r3j5c4k"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-0.6)
        ("rust-xml-rs" ,rust-xml-rs-0.8))))))

(define-public rust-wayland-scanner-0.21
  (package
    (inherit rust-wayland-scanner-0.23)
    (name "rust-wayland-scanner")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17mp49v7w0p0x5ry628lj2llljnwkr9aj9g4bqqhfibid32jhf5z"))))))

(define-public rust-wayland-server-0.29
  (package
    (name "rust-wayland-server")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11ickjbqpfs19wakf56l3j9asjcfw00d7wj48s3i99yvv1nq8q2k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nix" ,rust-nix-0.24)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-wayland-commons" ,rust-wayland-commons-0.29)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.29)
        ("rust-wayland-sys" ,rust-wayland-sys-0.29))))
    (inputs
     (list rust-bitflags-1
           rust-downcast-rs-1
           rust-lazy-static-1
           rust-libc-0.2
           rust-nix-0.24
           rust-parking-lot-0.11
           rust-scoped-tls-1
           rust-wayland-commons-0.29
           rust-wayland-scanner-0.29
           rust-wayland-sys-0.29))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Bindings to the standard C implementation of the wayland protocol")
    (description
     "This package provides Rust bindings to the standard C implementation of
the wayland protocol, server side.")
    (license license:expat)))

(define-public rust-wayland-server-0.28
  (package
    (inherit rust-wayland-server-0.29)
    (name "rust-wayland-server")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09jfdjfqhjfcpiz4csgh60ymfkmz1cl3jmxyzq9hzcp0kyyxix93"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nix" ,rust-nix-0.18)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-scoped-tls" ,rust-scoped-tls-1))))
    (inputs
     (list rust-bitflags-1
           rust-downcast-rs-1
           rust-lazy-static-1
           rust-libc-0.2
           rust-nix-0.18
           rust-parking-lot-0.11
           rust-scoped-tls-1
           rust-wayland-commons-0.28
           rust-wayland-scanner-0.28
           rust-wayland-sys-0.28))))

(define-public rust-wayland-server-0.23
  (package
    (inherit rust-wayland-server-0.28)
    (name "rust-wayland-server")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ccsalq6gnf07klnbjx2dxcbibhw03rqsgi578p913s3zsjlcg8a"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-commons" ,rust-wayland-commons-0.23)
        ("rust-wayland-sys" ,rust-wayland-sys-0.23)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.23))))
    (inputs `())))

(define-public rust-wayland-server-0.21
  (package
    (inherit rust-wayland-server-0.23)
    (name "rust-wayland-server")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ayn4wlrpg0fw04prri9awpkjvbzjil0d3l3a8zs9pdbnspvw6ah"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-commons" ,rust-wayland-commons-0.21)
        ("rust-wayland-sys" ,rust-wayland-sys-0.21)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.21))))))

(define-public rust-wayland-sys-0.29
  (package
    (name "rust-wayland-sys")
    (version "0.29.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m2jwk5q36jidwbdmdicmi27r9dzi4wanzg3i28nfxc9kbvisd6r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dlib" ,rust-dlib-0.5)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-memoffset" ,rust-memoffset-0.6)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libwayland (dirname (search-input-file
                                         inputs "lib/libwayland-client.so"))))
               (substitute* (find-files "src" "\\.rs$")
                 (("libwayland.*\\.so" shared-lib)
                  (string-append libwayland "/" shared-lib)))))))))
    (inputs
     (list rust-dlib-0.5 rust-lazy-static-1 rust-libc-0.2
           rust-pkg-config-0.3))
    (propagated-inputs
     (list wayland))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "FFI bindings to the various @file{libwayland-*.so} libraries")
    (description
     "This package provides FFI bindings to the various
@file{libwayland-*.so} libraries.  You should only need this crate if
you are working on custom Wayland protocol extensions.  Look at the
crate @code{rust-wayland-client} for usable bindings.")
    (license license:expat)))

(define-public rust-wayland-sys-0.28
  (package
    (inherit rust-wayland-sys-0.29)
    (name "rust-wayland-sys")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16f03jsy7q6p2wpaazc4w4kycyyk0fz7lacpdbcizl9m1i7874v7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libwayland (assoc-ref inputs "wayland")))
               (substitute* (find-files "src" "\\.rs$")
                 (("libwayland.*\\.so" shared-lib)
                  (string-append libwayland "/lib/" shared-lib)))
               #t))))))
    (inputs
     (list rust-dlib-0.4 rust-lazy-static-1 rust-libc-0.2
           rust-pkg-config-0.3))
    (propagated-inputs
     (list wayland))))

(define-public rust-wayland-sys-0.23
  (package
    (inherit rust-wayland-sys-0.28)
    (name "rust-wayland-sys")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1x2qafvj8hd2x5qfaan2dfpw9amg0f5g9sqrkdy7qvbddsl8jknr"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2))))
    (inputs `())
    (propagated-inputs `())))

(define-public rust-wayland-sys-0.21
  (package
    (inherit rust-wayland-sys-0.23)
    (name "rust-wayland-sys")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0a0ndgkg98pvmkv44yya4f7mxzjaxylknqh64bpa05w0azyv02jj"))))))

(define-public rust-webp-0.2
  (package
    (name "rust-webp")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "webp" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1bhw6xp7vg4rx7flxgzvdzk21q2dx1bsn06h0yj7jq0n3y12y0ng"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-image" ,rust-image-0.24)
        ("rust-libwebp-sys" ,rust-libwebp-sys-0.4))
       #:cargo-development-inputs
       (("rust-image" ,rust-image-0.24))))
    (home-page "https://github.com/jaredforth/webp")
    (synopsis "WebP conversion library")
    (description "This package procides a WebP conversion library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-winit-0.26
  (package
    (name "rust-winit")
    (version "0.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fp7cdh7llbqmm6ga8f6bzk9785jmkbyy1w631hr9faq3n9wqhwv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cocoa" ,rust-cocoa-0.24)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.22)
        ("rust-core-video-sys" ,rust-core-video-sys-0.1)
        ("rust-dispatch" ,rust-dispatch-0.2)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-mio" ,rust-mio-0.8)
        ("rust-ndk" ,rust-ndk-0.5)
        ("rust-ndk-glue" ,rust-ndk-glue-0.5)
        ("rust-ndk-sys" ,rust-ndk-sys-0.2)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.15)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11-dl" ,rust-x11-dl-2))
       #:cargo-development-inputs
       (("rust-console-log" ,rust-console-log-0.2)
        ("rust-simple-logger" ,rust-simple-logger-1))))
    (inputs
     (list rust-wayland-client-0.29 rust-wayland-protocols-0.29))
    (home-page "https://github.com/rust-windowing/winit")
    (synopsis "Window creation library")
    (description
     "Winit is a window creation and management library. It can create
windows and lets you handle events (for example: the window being
resized, a key being pressed, a mouse movement, etc.) produced by
window.

Winit is designed to be a low-level brick in a hierarchy of libraries.
Consequently, in order to show something on the window you need to use
the platform-specific getters provided by winit, or another library.")
    (license license:asl2.0)))

(define-public rust-winit-0.24
  (package
    (inherit rust-winit-0.26)
    (name "rust-winit")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15zmpx5ip6ziqhds7md1s0ri0blhxfa8fg1ylg84pf0frrpxlkns"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cocoa" ,rust-cocoa-0.24)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.22)
        ("rust-core-video-sys" ,rust-core-video-sys-0.1)
        ("rust-dispatch" ,rust-dispatch-0.2)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-mio-extras" ,rust-mio-extras-2)
        ("rust-ndk" ,rust-ndk-0.2)
        ("rust-ndk-glue" ,rust-ndk-glue-0.2)
        ("rust-ndk-sys" ,rust-ndk-sys-0.2)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.12)
        ("rust-stdweb" ,rust-stdweb-0.4)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11-dl" ,rust-x11-dl-2))
       #:cargo-development-inputs
       (("rust-console-log" ,rust-console-log-0.2)
        ("rust-simple-logger" ,rust-simple-logger-1))))
    (inputs
     (list rust-wayland-client-0.28))))

(define-public rust-winit-0.20
  (package
    (inherit rust-winit-0.24)
    (name "rust-winit")
    (version "0.20.0-alpha6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1g5cchl97zcg525j6jdr77yby8cmhwv1qqwcd3sf4l4zl263195z"))
       (patches
         (list
           (origin
             (method url-fetch)
             (uri "https://github.com/rust-windowing/winit/commit/d1c6506865c7bddbb5fb4d80a613e43ddc1370b5.patch")
             (file-name (string-append name "-fix-bindings.patch"))
             (sha256
              (base32
               "03q4bvdq86kii53d0vsywv08g8vqirf9h1lz2cl6rcc7gjfynpds")))))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-cocoa" ,rust-cocoa-0.19)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-core-video-sys" ,rust-core-video-sys-0.1)
        ("rust-dispatch" ,rust-dispatch-0.1)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.10)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.6)
        ("rust-stdweb" ,rust-stdweb-0.4)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wayland-client" ,rust-wayland-client-0.23)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11-dl" ,rust-x11-dl-2))
       #:cargo-development-inputs
       (("rust-console-log" ,rust-console-log-0.1)
        ("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-image" ,rust-image-0.21))))))

(define-public rust-winit-0.19
  (package
    (inherit rust-winit-0.20)
    (name "rust-winit")
    (version "0.19.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1a4lnfyvlc4jabhs30wlmkgdjv7qhbplmyp833kl7ykjni5yp5hy"))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-cocoa" ,rust-cocoa-0.18)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-image" ,rust-image-0.21)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.9)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.4)
        ("rust-wayland-client" ,rust-wayland-client-0.21)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11-dl" ,rust-x11-dl-2))))))

(define-public rust-x11-2
  (package
    (name "rust-x11")
    (version "2.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10pbvmcyqm6j6zr4zk7znk8silmilihv8jxmbxbl1b0pkidqsqy2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (propagated-inputs
     (list mesa))
    (home-page "https://github.com/erlepereira/x11-rs")
    (synopsis "X11 library bindings for Rust")
    (description "This crate provides X11 library bindings for Rust.")
    (license license:expat)))

(define-public rust-x11rb-0.8
  (package
    (name "rust-x11rb")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "x11rb" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "068g5ll4l5f35c2v098hj0kj2c9ma0r7v3pbli164q9g7w5hiyvg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t
        #:cargo-inputs
        (("rust-gethostname" ,rust-gethostname-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libloading" ,rust-libloading-0.7)
         ("rust-nix" ,rust-nix-0.20)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-winapi" ,rust-winapi-0.3)
         ("rust-winapi-wsapoll" ,rust-winapi-wsapoll-0.1))))
    (home-page "https://github.com/psychon/x11rb")
    (synopsis "Rust bindings to X11")
    (description "This package provides Rust bindings to X11")
    (license (list license:expat license:asl2.0))))

(define-public rust-x11-clipboard-0.5
  (package
    (name "rust-x11-clipboard")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11-clipboard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17c5yxxhknrp7y9mc7mp85ra8q4jw12c174m9yzbfr1vs2pkgsg5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-xcb" ,rust-xcb-0.9))))
    (home-page "https://github.com/quininer/x11-clipboard")
    (synopsis "x11 clipboard support for Rust")
    (description "This package provides x11 clipboard support for Rust.")
    (license license:expat)))

(define-public rust-x11-dl-2
  (package
    (name "rust-x11-dl")
    (version "2.18.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11-dl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y7yq4sfvv56shk4v3s7gvlrwk9d0migj622fl4i4c5klpiq3y9b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-maybe-uninit" ,rust-maybe-uninit-2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/erlepereira/x11-rs.git")
    (synopsis "X11 library bindings for Rust")
    (description "This package provides X11 library bindings for Rust.")
    (license license:cc0)))

(define-public rust-y4m-0.7
  (package
    (name "rust-y4m")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "y4m" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bhdgb7hgx7j92nm6ij5n8wisp50j8ff66ks14jzwdw2mwhrjam7"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/image-rs/y4m")
    (synopsis "YUV4MPEG2 (@file{.y4m}) encoder and decoder")
    (description
     "This package provides a YUV4MPEG2 (@file{.y4m}) encoder and decoder.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
