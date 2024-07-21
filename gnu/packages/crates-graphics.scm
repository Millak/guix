;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
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
;;; Copyright © 2023, 2024 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

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
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ansi_colours" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "104aj4fi8nxdb9c5ahpwn53afmfcdzmwi3k9rawl3lvm42ymh5ba"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (((string-append ">= ([[:digit:]]+\\.[[:digit:]]+),"
                                   " <= ([[:digit:]]+\\.[[:digit:]]+)")
                    _ version _)
                   (string-append ">=" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-termcolor" ,rust-termcolor-1))
       #:cargo-development-inputs
       (("rust-crc64" ,rust-crc64-2)
        ("rust-criterion" ,rust-criterion-0.5)
        ("rust-empfindung" ,rust-empfindung-0.2)
        ("rust-lab" ,rust-lab-0.11)
        ("rust-rgb" ,rust-rgb-0.8))))
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

(define-public rust-ansi-to-tui-2
  (package
    (name "rust-ansi-to-tui")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ansi-to-tui" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l43nyj2difngwjbiy6vd5p8bw96w06swgw5hx6vi9zvqzs8wyqm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nom" ,rust-nom-7)
                       ("rust-simdutf8" ,rust-simdutf8-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tui" ,rust-tui-0.16))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1))))
    (home-page "https://github.com/uttarayan21/ansi-to-tui")
    (synopsis
     "Library to convert ansi color coded text into @code{ratatui::text::Text}")
    (description
     "This package provides a library to convert ansi color coded text into
@code{ratatui::text::Text} type from the ratatui library.")
    (license license:expat)))

(define-public rust-ansiterm-0.12
  (package
    (name "rust-ansiterm")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ansiterm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k14pywvgd829swxzji0nchk4n6yvr9xz6lkwv96v8ax77sqgdaa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ansi-colours" ,rust-ansi-colours-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/rustadopt/ansiterm-rs")
    (synopsis "Library for ANSI terminal colours and styles (bold, underline)")
    (description
     "This package provides a library for ANSI terminal colours and
styles (bold, underline).")
    (license license:expat)))

(define-public rust-aom-sys-0.3
  (package
    (name "rust-aom-sys")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aom-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bc1dzl3c95s44q7c1i0vnj7fhiqf44in8w22nw5vmp1vgbpadk2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (native-inputs (list pkg-config))
    (inputs (list clang libaom llvm))
    (home-page "https://github.com/rust-av/aom-rs")
    (synopsis "FFI bindings to aom")
    (description "This package provides FFI bindings to aom.")
    (license license:expat)))

(define-public rust-ascii-canvas-3
  (package
    (name "rust-ascii-canvas")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ascii-canvas" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1in38ziqn4kh9sw89ys4naaqzvvjscfs0m4djqbfq7455v5fq948"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-term" ,rust-term-0.7))
       #:cargo-development-inputs (("rust-diff" ,rust-diff-0.1))))
    (home-page "https://github.com/nikomatsakis/ascii-canvas")
    (synopsis "Simple canvas for drawing lines and styled text and emitting to
the terminal")
    (description "@code{ASCII} canvas is a simple Rust library that allows you
to draw lines and colored text and then write them to the terminal.  It uses
the term library to handle the ANSI nonsense and hence it works on Windows,
Mac, and Unix.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ascii-canvas-2
  (package
    (inherit rust-ascii-canvas-3)
    (name "rust-ascii-canvas")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ascii-canvas" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a9s8vrbc5jr6ry5ygjyfqmbs9gyya1v6dsxzsczpai8z4nvg3pz"))))
    (arguments
     `(#:skip-build? #t  ;; TODO: failes due to an unresolved import
       #:cargo-inputs (("rust-term" ,rust-term-0.5))))))

(define-public rust-ash-0.37
  (package
    (name "rust-ash")
    (version "0.37.3+1.3.251")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jndbsi5c8xifh4fdp378xpbyzdhs7y38hmbhih0lsv8bn1w7s9r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=entry::Entry::try_enumerate_instance_version"
         "--skip=src/lib.rs")
       #:cargo-inputs (("rust-libloading" ,rust-libloading-0.7))))
    (home-page "https://github.com/MaikKlein/ash")
    (synopsis "Vulkan bindings for Rust")
    (description "Vulkan bindings for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ash-window-0.12
  (package
    (name "rust-ash-window")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ash-window" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1armbqzr0x905yypvh9ywgjj91kn93y5mxd6gkwaiwr9gid2h4mr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ash" ,rust-ash-0.37)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
                       ("rust-raw-window-metal" ,rust-raw-window-metal-0.3))
       #:cargo-development-inputs (("rust-ash" ,rust-ash-0.37)
                                   ("rust-winit" ,rust-winit-0.27))))
    (native-inputs (list pkg-config vulkan-loader))
    (inputs (list expat fontconfig freetype))
    (home-page "https://github.com/MaikKlein/ash")
    (synopsis "Interop library between ash and raw-window-handle")
    (description "Interop library between ash and raw-window-handle.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-avif-serialize-0.8
  (package
    (name "rust-avif-serialize")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "avif-serialize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1llnwlj11wcifdlny8x8yksl3zmz8i6a35il0cd4ar335yj7av47"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `mp4parse`
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

(define-public rust-avif-serialize-0.7
  (package
    (inherit rust-avif-serialize-0.8)
    (name "rust-avif-serialize")
    (version "0.7.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "avif-serialize" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0d2makdw756978i8s3qhlhh1h91y5maxriay6r4kmsmk8pky2qfc"))))
    (arguments
     `(#:tests? #f              ; Undeclared dependencies
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7))
       #:cargo-development-inputs (("rust-avif-parse" ,rust-avif-parse-1))))))

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

(define-public rust-box-drawing-0.1
  (package
    (name "rust-box-drawing")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "box_drawing" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jx4rrxy4xmgmplmgl398vrng67sfl8qny7n7d91fyw6zpaxh9za"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.com/chronos.alfa/box_drawing.git")
    (synopsis "Simple library containing constants for UTF-8 box drawing")
    (description
     "This package provides a simple library containing constants for UTF-8 box
drawing.")
    (license license:expat)))

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

(define-public rust-color-to-tui-0.2
  (package
    (name "rust-color-to-tui")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "color-to-tui" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k3vyp2fl0lcqs8iwssv56562kag6ljqaixirrci77ydmcq3zi0s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-tui" ,rust-tui-0.16))
       #:cargo-development-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://git.uttarayan.me/uttarayan/color-to-tui")
    (synopsis
     "Parse colors and convert them to @code{ratatui::style::Colors}")
    (description
     "This package provides parsing colors and converting them to
@code{ratatui::style::Colors}.")
    (license license:expat)))

(define-public rust-colorous-1
  (package
    (name "rust-colorous")
    (version "1.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "colorous" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1crqxkhpvwjcnjggp2qjs2mzyd1xrv3drgqq4bzlhi9ggj687c3y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-dejavu" ,rust-dejavu-2)
                                   ("rust-image" ,rust-image-0.24)
                                   ("rust-imageproc" ,rust-imageproc-0.23)
                                   ("rust-rusttype" ,rust-rusttype-0.9))))
    (home-page "https://github.com/dtolnay/colorous")
    (synopsis "Professional color schemes ported from d3-scale-chromatic")
    (description "Professional color schemes ported from d3-scale-chromatic.")
    (license license:asl2.0)))

(define-public rust-core-graphics-0.23
  (package
    (name "rust-core-graphics")
    (version "0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04m2hpzrkzkkxmvlak20ivlgf8rcsh3j3y67vgz2c30iyjx2j2lp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-core-graphics-types" ,rust-core-graphics-types-0.1)
                       ("rust-foreign-types" ,rust-foreign-types-0.5)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Graphics for macOS")
    (description
     "This package provides bindings to Core Graphics for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-graphics-0.22
  (package
    (inherit rust-core-graphics-0.23)
    (name "rust-core-graphics")
    (version "0.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yz4xzbz36vbmlra0viazzlicp8kap1ldgshsp5nzz4g7fmvp095"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics-types" ,rust-core-graphics-types-0.1)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))))

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

(define-public rust-cursor-icon-1
  (package
    (name "rust-cursor-icon")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cursor-icon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14brf4vd6az9hnszwzqj7xyfaymqx9806d4i7xmwlaja3wjsr9ln"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-wayland-client" ,rust-wayland-client-0.31)
        ("rust-wayland-cursor" ,rust-wayland-cursor-0.31))))
    (home-page "https://github.com/rust-windowing/cursor-icon")
    (synopsis "Cross platform cursor icon type")
    (description "This package provides a cross platform cursor icon type.")
    (license (list license:expat license:asl2.0 license:zlib))))

(define-public rust-d3d12-0.7
  (package
    (name "rust-d3d12")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "d3d12" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "084z4nz0ddmsjn6qbrgxygr55pvpi3yjrrkvmzyxs79b56ml8vp1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; unresolved imports `winapi::shared`, `winapi::um`
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libloading" ,rust-libloading-0.8)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/gfx-rs/d3d12-rs")
    (synopsis "Low level D3D12 API wrapper")
    (description "Low level D3D12 API wrapper.")
    (license (list license:expat license:asl2.0))))

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
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dav1d-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "13z5qvf35lkda67l6l1bkdp1gmqg75cqfblldxh4n8rbmn4zsj9s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Expects 1.0.0 <= dav1d < 1.3.0
       #:cargo-inputs
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

(define-public rust-deltae-0.3
  (package
    (name "rust-deltae")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "deltae" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d3hw9hpvicl9x0x34jr2ybjk5g5ym1lhbyz6zj31110gq8zaaap"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-clap" ,rust-clap-4))))
    (home-page "https://gitlab.com/ryanobeirne/deltae")
    (synopsis "Calculate delta-e between two colors")
    (description "Calculate Delta E between two colors in CIE Lab space.")
    (license license:expat)))

(define-public rust-drm-0.10
  (package
    (name "rust-drm")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "drm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11xyv3l03a3zxsrfr02mwnn5d6h4100919zb2v9fpizv7xq1pywp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-drm-ffi" ,rust-drm-ffi-0.6)
                       ("rust-drm-fourcc" ,rust-drm-fourcc-2)
                       ("rust-nix" ,rust-nix-0.27))
       #:cargo-development-inputs (("rust-image" ,rust-image-0.24)
                                   ("rust-nix" ,rust-nix-0.27)
                                   ("rust-rustyline" ,rust-rustyline-12))))
    (home-page "https://github.com/Smithay/drm-rs")
    (synopsis "Safe, low-level bindings to the Direct Rendering Manager API")
    (description
     "Safe, low-level bindings to the Direct Rendering Manager API.")
    (license license:expat)))

(define-public rust-drm-ffi-0.6
  (package
    (name "rust-drm-ffi")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "drm-ffi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0par8xcrpnz1h53yam4ai9jpqc9as337vclzsn4hw9xnqhciqzds"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-drm-sys" ,rust-drm-sys-0.5)
                       ("rust-nix" ,rust-nix-0.27))))
    (home-page "https://github.com/Smithay/drm-rs")
    (synopsis "Safe, low-level bindings to the Direct Rendering Manager API")
    (description
     "Safe, low-level bindings to the Direct Rendering Manager API.")
    (license license:expat)))

(define-public rust-drm-fourcc-2
  (package
    (name "rust-drm-fourcc")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "drm-fourcc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x76v9a0pkgym4n6cah4barnai9gsssm7gjzxskw2agwibdvrbqa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.57)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/danielzfranklin/drm-fourcc-rs")
    (synopsis "Enum with every valid Direct Rendering Manager format fourcc")
    (description "This package provides an enum with every valid @acronym{DRM,
Direct Rendering Manager} format fourcc.")
    (license license:expat)))

(define-public rust-drm-sys-0.5
  (package
    (name "rust-drm-sys")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "drm-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bhmwzbraxclivn2h83ab7aqdcly82sy7w85az6mcah6d021qkrs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.66)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/Smithay/drm-rs")
    (synopsis "Bindings to the Direct Rendering Manager API")
    (description "Bindings to the Direct Rendering Manager API.")
    (license license:expat)))

(define-public rust-enterpolation-0.2
  (package
    (name "rust-enterpolation")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "enterpolation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ah41msghasm0i97awa67rv3mg6p3j0xijswy1gpdipprg4gbb8z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-assert-float-eq" ,rust-assert-float-eq-1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-topology-traits" ,rust-topology-traits-0.1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-image" ,rust-image-0.24)
                                   ("rust-palette" ,rust-palette-0.7))))
    (home-page "https://github.com/NicolasKlenert/enterpolation")
    (synopsis "Library for create and compute interpolations and extrapolations")
    (description
     "This package provides a library for creating and computing interpolations,
extrapolations and smoothing of generic data points.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-evdev-0.11
  (package
    (name "rust-evdev")
    (version "0.11.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "evdev" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zknxkgkyh9fx3mq4div9kcgvgsiy91vzd5sq7bdinsn467sfx65"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitvec" ,rust-bitvec-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.23)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-itertools" ,rust-itertools-0.10)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/cmr/evdev")
    (synopsis "Evdev interface for Linux")
    (description "This package provides an evdev interface for Linux.")
    (license (list license:asl2.0 license:expat))))

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

(define-public rust-fast-image-resize-2
  (package
    (name "rust-fast-image-resize")
    (version "2.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fast_image_resize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nnm59h7dl2bpi5k2wcd7zz14nl00sa33jiipbjbn48f0i09ly6c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; Unresolved import `testing'
       #:cargo-inputs
       (("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.4)
        ("rust-image" ,rust-image-0.24)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-nix" ,rust-nix-0.26)
        ("rust-png" ,rust-png-0.17)
        ("rust-resize" ,rust-resize-0.7)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/cykooz/fast_image_resize")
    (synopsis
     "Fast image resizing with using of SIMD instructions")
    (description
     "This package provides fast image resizing with using of SIMD instructions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fast-srgb8-1
  (package
    (name "rust-fast-srgb8")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fast-srgb8" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18g6xwwh4gnkyx1352hnvwagpv0n4y98yp2llm8vyvwxh487abnx"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/thomcc/fast-srgb8")
    (synopsis "Conversions between linear float and 8-bit @code{sRGB}")
    (description
     "This package provides very fast conversions between linear float and
8-bit @code{sRGB} (with @code{no_std} support).")
    (license (list license:expat license:asl2.0 license:cc0))))

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

(define-public rust-gif-0.12
  (package
    (name "rust-gif")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ibhjyrslfv9qm400gp4hd50v9ibva01j4ab9bwiq1aycy9jayc0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-color-quant" ,rust-color-quant-1)
                       ("rust-weezl" ,rust-weezl-0.1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-glob" ,rust-glob-0.3)
                                   ("rust-png" ,rust-png-0.17))))
    (home-page "https://github.com/image-rs/image-gif")
    (synopsis "GIF decoder and encoder")
    (description "This package provides a GIF decoder and encoder in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gif-0.11
  (package
    (inherit rust-gif-0.12)
    (name "rust-gif")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nsfd5qvp69z8kn17ziiq8zv4mclfycyxppf5k9fm2h8g1z1i9y3"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-color-quant" ,rust-color-quant-1)
        ("rust-weezl" ,rust-weezl-0.1))))))

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
       (uri (crate-uri "gl_generator" version))
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
       (uri (crate-uri "gl_generator" version))
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
      (uri (crate-uri "gl_generator" version))
      (file-name
       (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "1gdchvay0k0g931b2ki33mkfixcw4radk5b8sqsm29rahxg3v8ir"))))))

(define-public rust-gl-loader-0.1
  (package
    (name "rust-gl-loader")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gl_loader" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lwr1gd7hrb2nk67zw4pc04vl4h868r5a7846zjr0548bzfrcbg3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; missing `gl` crate
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/maeln/gl_loader")
    (synopsis "Simple OpenGL function pointer loader based on Glad")
    (description
     "Simple @code{OpenGL} function pointer loader based on Glad.")
    (license license:cecill)))

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

(define-public rust-glow-0.12
  (package
    (name "rust-glow")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a1p6c9nff09m4gn0xnnschcpjq35y7c12w69ar8l2mnwj0fa3ya"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-slotmap" ,rust-slotmap-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/grovesNL/glow.git")
    (synopsis "Bindings to run GL anywhere")
    (description
     "GL on Whatever: a set of bindings to run GL (Open GL, @code{OpenGL} ES, and
@code{WebGL}) anywhere, and avoid target-specific code.")
    (license (list license:expat license:asl2.0 license:zlib))))

(define-public rust-glutin-0.31
  (package
    (name "rust-glutin")
    (version "0.31.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04mjvgh2dipwa8wdy8gc70k8w48104v8vmr2cmqdqspq5ai5jm00"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cfg-aliases" ,rust-cfg-aliases-0.1)
                       ("rust-cgl" ,rust-cgl-0.3)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-dispatch" ,rust-dispatch-0.2)
                       ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.6)
                       ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.5)
                       ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.5)
                       ("rust-icrate" ,rust-icrate-0.0.4)
                       ("rust-libloading" ,rust-libloading-0.8)
                       ("rust-objc2" ,rust-objc2-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
                       ("rust-wayland-sys" ,rust-wayland-sys-0.31)
                       ("rust-windows-sys" ,rust-windows-sys-0.48)
                       ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/rust-windowing/glutin")
    (synopsis "Cross-platform OpenGL context provider")
    (description "This package provides an OpenGL context provider.")
    (license license:asl2.0)))

(define-public rust-glutin-0.30
  (package
    (inherit rust-glutin-0.31)
    (name "rust-glutin")
    (version "0.30.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zk0cxchdy6fmnnwggylgg748wbk04mys9pv76nvh6974h1kpjcg"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cfg-aliases" ,rust-cfg-aliases-0.1)
        ("rust-cgl" ,rust-cgl-0.3)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-dispatch" ,rust-dispatch-0.2)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.5)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.4)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.4)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-objc2" ,rust-objc2-0.3)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
        ("rust-wayland-sys" ,rust-wayland-sys-0.30)
        ("rust-windows-sys" ,rust-windows-sys-0.45)
        ("rust-x11-dl" ,rust-x11-dl-2))))))

(define-public rust-glutin-0.29
  (package
    (inherit rust-glutin-0.31)
    (name "rust-glutin")
    (version "0.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04y7s5m74j9gns5bdja0alkm0m0b727vf9k7rw6g5jpxjk99lk24"))))
    (arguments
     `(#:cargo-inputs (("rust-cgl" ,rust-cgl-0.3)
                       ("rust-cocoa" ,rust-cocoa-0.24)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.1)
                       ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys-0.1)
                       ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.1)
                       ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.1)
                       ("rust-libloading" ,rust-libloading-0.7)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
                       ("rust-wayland-client" ,rust-wayland-client-0.29)
                       ("rust-wayland-egl" ,rust-wayland-egl-0.29)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-winit" ,rust-winit-0.27))))
    (native-inputs (list pkg-config))
    (inputs (list expat fontconfig freetype))))

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
        ("rust-winit" ,rust-winit-0.26))))))

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
        ("rust-winit" ,rust-winit-0.24))))))

(define-public rust-glutin-0.21
  (package
    (inherit rust-glutin-0.26)
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
     `(#:cargo-test-flags
       '("--release" "--lib" "--bins" "--tests")
       #:cargo-inputs
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

(define-public rust-glutin-egl-sys-0.6
  (package
    (name "rust-glutin-egl-sys")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_egl_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kcv5pdpdsyhzpiahga15kk7yd4m64ia2k6xqcrz97ihylimdk3p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gl-generator" ,rust-gl-generator-0.14)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/rust-windowing/glutin")
    (synopsis "Egl bindings for glutin")
    (description "The egl bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-egl-sys-0.5
  (package
    (inherit rust-glutin-egl-sys-0.6)
    (name "rust-glutin-egl-sys")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_egl_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1iapzqscy4891crxdddddq4qqqday1sf0s0j762yqs2sdjr4wy5g"))))
    (arguments
     `(#:cargo-inputs (("rust-gl-generator" ,rust-gl-generator-0.14)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))))

(define-public rust-glutin-egl-sys-0.4
  (package
    (inherit rust-glutin-egl-sys-0.6)
    (name "rust-glutin-egl-sys")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_egl_sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0z9nm3d6qcgqg7f6qkbnsfs4cy90d8raw09inf2qc564nnmz1ap5"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14)
        ("rust-windows-sys" ,rust-windows-sys-0.45))))))

(define-public rust-glutin-egl-sys-0.1
  (package
    (inherit rust-glutin-egl-sys-0.4)
    (name "rust-glutin-egl-sys")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_egl_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g81bz7ppvaksvwcw1jg553g8b2shvmnfm9ms6hixwvinj20z438"))))
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3)
        ("rust-gl-generator" ,rust-gl-generator-0.14))))))

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

(define-public rust-glutin-glx-sys-0.5
  (package
    (name "rust-glutin-glx-sys")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_glx_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0krv3chf5sy83rsfwq267paczskpwnb5gcw0agac5p0hdilgsrd1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gl-generator" ,rust-gl-generator-0.14)
                       ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/rust-windowing/glutin")
    (synopsis "Glx bindings for glutin")
    (description "This package provides glx bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-glx-sys-0.4
  (package
    (inherit rust-glutin-glx-sys-0.5)
    (name "rust-glutin-glx-sys")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_glx_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "150l397l64p4r46wshh8zdlwifpcqpm93fm3csh4m5k8wmgwnlqv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14)
        ("rust-x11-dl" ,rust-x11-dl-2))))))

(define-public rust-glutin-glx-sys-0.1
  (package
    (inherit rust-glutin-glx-sys-0.4)
    (name "rust-glutin-glx-sys")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_glx_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s14s3v2dddkx141w2x65s8ik54mrn432hisbc65i62hhrshagfr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14)
        ("rust-x11-dl" ,rust-x11-dl-2))))))

(define-public rust-glutin-wgl-sys-0.5
  (package
    (name "rust-glutin-wgl-sys")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_wgl_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b9f6qjc8gwhfxac4fpxkvv524l493f6b6q764nslpwmmjnri03c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gl-generator" ,rust-gl-generator-0.14))))
    (home-page "https://github.com/rust-windowing/glutin")
    (synopsis "Wgl bindings for glutin")
    (description "This package provides wgl bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-wgl-sys-0.4
  (package
    (inherit rust-glutin-wgl-sys-0.5)
    (name "rust-glutin-wgl-sys")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_wgl_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rc1c585ai9gav2nvdd5pn1x9gxv57yl5gg9cnyccgq3j273k2gg"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14))))))

(define-public rust-glutin-wgl-sys-0.1
  (package
    (inherit rust-glutin-wgl-sys-0.4)
    (name "rust-glutin-wgl-sys")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_wgl_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15hns8b3i7iy366m61dg7jlr7wgzz8z8cakgbj3apnv92ld9b99x"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14))))))

(define-public rust-gpu-alloc-0.6
  (package
    (name "rust-gpu-alloc")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gpu-alloc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wd1wq7qs8ja0cp37ajm9p1r526sp6w0kvjp3xx24jsrjfx2vkgv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gpu-alloc-types" ,rust-gpu-alloc-types-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/zakarumych/gpu-alloc")
    (synopsis "Implementation agnostic memory allocator for Vulkan like APIs")
    (description
     "Implementation agnostic memory allocator for Vulkan like APIs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gpu-alloc-types-0.3
  (package
    (name "rust-gpu-alloc-types")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gpu-alloc-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "190wxsp9q8c59xybkfrlzqqyrxj6z39zamadk1q7v0xad2s07zwq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2))))
    (home-page "https://github.com/zakarumych/gpu-alloc")
    (synopsis "Core types of gpu-alloc crate")
    (description "Core types of gpu-alloc crate.")
    (license (list license:expat license:asl2.0))))

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
    (version "0.24.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "image" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "04d7f25b8nlszfv9a474n4a0al4m2sv9gqj3yiphhqr0syyzsgbg"))))
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
        ("rust-gif" ,rust-gif-0.12)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.3)
        ("rust-mp4parse" ,rust-mp4parse-0.17)
        ("rust-num-rational" ,rust-num-rational-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.17)
        ("rust-qoi" ,rust-qoi-0.4)
        ("rust-ravif" ,rust-ravif-0.11)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-tiff" ,rust-tiff-0.9)
        ("rust-webp" ,rust-webp-0.2))
       #:cargo-development-inputs
       (("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-criterion" ,rust-criterion-0.4)
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

(define-public rust-imageproc-0.23
  (package
    (name "rust-imageproc")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "imageproc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mszh0jz8208r9h62aq61mda7xf6pwldcmcnl80n6ihx6n9ykbmn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included
       #:cargo-inputs (("rust-approx" ,rust-approx-0.5)
                       ("rust-conv" ,rust-conv-0.3)
                       ("rust-image" ,rust-image-0.24)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-nalgebra" ,rust-nalgebra-0.30)
                       ("rust-num" ,rust-num-0.4)
                       ("rust-quickcheck" ,rust-quickcheck-0.9)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-rand-distr" ,rust-rand-distr-0.2)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rusttype" ,rust-rusttype-0.9)
                       ("rust-sdl2" ,rust-sdl2-0.35))
       #:cargo-development-inputs
       (("rust-assert-approx-eq" ,rust-assert-approx-eq-1)
        ("rust-image" ,rust-image-0.24)
        ("rust-quickcheck" ,rust-quickcheck-0.9)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/image-rs/imageproc")
    (synopsis "Image processing operations")
    (description "Image processing operations.")
    (license license:expat)))

(define-public rust-imgref-1
  (package
    (name "rust-imgref")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "imgref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09l18s80crfn7g8ank3v44g43xns4pg7f6hpaz3sfna1bwsxmzj4"))))
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
        ("rust-png" ,rust-png-0.16)
        ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-keyframe-1
  (package
    (name "rust-keyframe")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "keyframe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1afr5ffns3k79xaqnw6rw3qn8sngwly6gxfnjn8d060mk3vqnw30"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; Done to avoid dev dependency on ggez, a game engine.
       #:cargo-inputs (("rust-mint" ,rust-mint-0.5)
                       ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/HannesMann/keyframe")
    (synopsis "Simple library for animation in Rust")
    (description
     "This package provides a simple library for animation in Rust.
It's features include:

@enumerate
@item Several easing functions, including user-defined Bézier curves and keyframable curves.
@item Animation sequences (like CSS keyframes).
@item @code{mint} integration for 2D/3D/4D support (points, rectangles, colors, etc).
@end enumerate")
    (license license:expat)))

(define-public rust-libdav1d-sys-0.6
  (package
    (name "rust-libdav1d-sys")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libdav1d-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wh5jgdm33ld6djxsc7cmwd1ifqys145zlbsf8516n625lscrj8j"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")
                 ;; Force linking to our packaged dav1d.
                 (delete-file "build.rs")
                 (with-output-to-file "build.rs"
                   (lambda _
                     (format #t "fn main() {~@
                             println!(\"cargo:rustc-link-lib=dav1d\");~@
                             }~%")))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (inputs (list dav1d))
    (home-page "https://github.com/njaard/libavif-rs")
    (synopsis "Link to dav1d AV1 decoder")
    (description
     "This package builds and links to the dav1d AV1 decoder.")
    (license license:bsd-2)))

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

(define-public rust-lyon-geom-1
  (package
    (name "rust-lyon-geom")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lyon_geom" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ydzjz8lh2jlg9gjcpmkix0yxfgkv76yn0mb67h0nasa4f6zpv7d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-euclid" ,rust-euclid-0.22)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/nical/lyon")
    (synopsis "2D graphics rendering on the GPU using tessellation")
    (description
     "This package provides 2D graphics rendering on the GPU using tessellation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lyon-geom-0.17
  (package
    (inherit rust-lyon-geom-1)
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
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
        (("rust-arrayvec" ,rust-arrayvec-0.5)
         ("rust-euclid" ,rust-euclid-0.22)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-serde" ,rust-serde-1))))))

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

(define-public rust-mp4parse-0.17
  (package
    (name "rust-mp4parse")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mp4parse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w654hv04w1zi2m9b1kji2610mrfrc554xqw4par5kn6sc1m58v3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=overflow_protection"
                            "--skip=public")
       #:cargo-inputs (("rust-bitreader" ,rust-bitreader-0.3)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-fallible-collections" ,rust-fallible-collections-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-static-assertions" ,rust-static-assertions-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-test-assembler" ,rust-test-assembler-0.1)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/mozilla/mp4parse-rust")
    (synopsis "Parser for ISO base media file format (mp4)")
    (description "Parser for ISO base media file format (mp4).")
    (license license:mpl2.0)))

(define-public rust-mp4parse-0.12
  (package
    (inherit rust-mp4parse-0.17)
    (name "rust-mp4parse")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mp4parse" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1scynvlmiy6xv2rrzzpijd812amh6a863na8i0xrcw5d9d08kl8h"))))
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
        ("rust-walkdir" ,rust-walkdir-2))))))

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

(define-public rust-palette-0.7
  (package
    (name "rust-palette")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "palette" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ynr6q7629gpw6pg8w1wrsl31sq48nxh1bg4awyrhhk6nyj27z7b"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("\"=([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Regression tests not included.
       #:cargo-inputs
       (("rust-approx" ,rust-approx-0.5)
        ("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-fast-srgb8" ,rust-fast-srgb8-1)
        ("rust-libm" ,rust-libm-0.2)
        ("rust-palette-derive" ,rust-palette-derive-0.7)
        ("rust-phf" ,rust-phf-0.11)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-wide" ,rust-wide-0.7))
       #:cargo-development-inputs
       (("rust-enterpolation" ,rust-enterpolation-0.2)
        ("rust-image" ,rust-image-0.23)
        ("rust-rand-mt" ,rust-rand-mt-4)
        ("rust-ron" ,rust-ron-0.8)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/Ogeon/palette")
    (synopsis "Convert and manage colors")
    (description
     "This package provides converting and managing colors in Rust with a focus
on correctness, flexibility and ease of use.")
    (license (list license:expat license:asl2.0))))

(define-public rust-palette-0.6
  (package
    (inherit rust-palette-0.7)
    (name "rust-palette")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "palette" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jay4zbxfnz6hj9f78inb3n3hmaamivnrrapy4ri0n0jf67xd74g"))))
    (arguments
     `(#:cargo-inputs
       (("rust-approx" ,rust-approx-0.5)
        ("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-palette-derive" ,rust-palette-derive-0.6)
        ("rust-phf" ,rust-phf-0.11)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-csv" ,rust-csv-1)
        ("rust-image" ,rust-image-0.23)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rand-mt" ,rust-rand-mt-4)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-palette-derive-0.7
  (package
    (name "rust-palette-derive")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "palette_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vcxjslri6f24zgv3n6ixhzb21a8z23fa6h42s8ss2zcvc10g2g8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-find-crate" ,rust-find-crate-0.6)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/Ogeon/palette")
    (synopsis "Automatically implement traits from the @code{palette} crate")
    (description "This package allows automatically implements traits from the
@code{palette} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-palette-derive-0.6
  (package
    (inherit rust-palette-derive-0.7)
    (name "rust-palette-derive")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "palette_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09z4nd4sbmzqd1pqr48vrdca3v2c03dzr70cmxs7zhp7m13dzvh5"))))
    (arguments
     `(#:cargo-inputs
       (("rust-find-crate" ,rust-find-crate-0.6)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-pbr-1
  (package
    (name "rust-pbr")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pbr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "056mqvw168ziig1dgl2kq4vmkamv6gk3hv1x9696r6ynl3gjfn7d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=tty::unix::compare_with_stty")
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/a8m/pb")
    (synopsis "Console progress bar for Rust")
    (description "This package provides a console progress bar for Rust.")
    (license license:expat)))

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
    (version "0.17.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qdmajjzkdbmk5zk7qb5pc6927xa26hr2v68hbkpa9ris79v1r06"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files incuded.
         "--skip=decoder::stream::tests::image_gamma"
         "--skip=decoder::stream::tests::image_source_chromaticities"
         "--skip=decoder::stream::tests::test_two_iccp_chunks"
         "--skip=encoder::tests::image_palette"
         "--skip=src/decoder/mod.rs - decoder::Decoder<R>::set_ignore_text_chunk (line 269)"
         "--skip=src/decoder/mod.rs - decoder::Decoder<R>::set_limits (line 182)"
         "--skip=src/lib.rs - (line 13)"
         "--skip=src/text_metadata.rs - text_metadata (line 25)")
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-fdeflate" ,rust-fdeflate-0.3)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.7))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-clap" ,rust-clap-3)
        ("rust-criterion" ,rust-criterion-0.4)
        ("rust-getopts" ,rust-getopts-0.2)
        ("rust-glium" ,rust-glium-0.32)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-term" ,rust-term-0.7))))
    (native-inputs (list pkg-config))
    (inputs (list expat fontconfig freetype))
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

(define-public rust-qoi-0.4
  (package
    (name "rust-qoi")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "qoi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00c0wkb112annn2wl72ixyd78mf56p4lxkhlmsggx65l3v3n8vbz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; unresolved import `libqoi`
       #:cargo-inputs (("rust-bytemuck" ,rust-bytemuck-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-cfg-if" ,rust-cfg-if-1)
                                   ("rust-png" ,rust-png-0.17)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/aldanor/qoi-rust")
    (synopsis "Encoder/decoder for QOI (Quite Okay Image) format")
    (description
     "VERY fast encoder/decoder for the @acronym{QOI, Quite Okay Image} format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ravif-0.11
  (package
    (name "rust-ravif")
    (version "0.11.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ravif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1315abwxldavjxdl8dphab16fh8akw000hn406dbjnsi53b4kgl5"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("\"=([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-avif-serialize" ,rust-avif-serialize-0.8)
                       ("rust-bitstream-io" ,rust-bitstream-io-2)
                       ("rust-imgref" ,rust-imgref-1)
                       ("rust-loop9" ,rust-loop9-0.1)
                       ("rust-quick-error" ,rust-quick-error-2)
                       ("rust-rav1e" ,rav1e)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rgb" ,rust-rgb-0.8))
       #:cargo-development-inputs (("rust-avif-parse" ,rust-avif-parse-1))))
    (native-inputs
     (list nasm pkg-config))
    (inputs
     (list libgit2 zlib))
    (home-page "https://lib.rs/ravif")
    (synopsis "Rust library for encoding images in AVIF format")
    (description
     "This package provides a rav1e-based pure Rust library for encoding images
in AVIF format (powers the @code{cavif} tool).")
    (license license:bsd-3)))

(define-public rust-ravif-0.8
  (package
    (inherit rust-ravif-0.11)
    (name "rust-ravif")
    (version "0.8.10+rust-1.67.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ravif" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1r3s78781kb9lwysdvpdc80gavly33dcs4inhhp2dawml9g3rjss"))))
    (arguments
     `(#:skip-build? #t     ; Needs rav1e-0.6
       #:cargo-inputs
       (("rust-avif-serialize" ,rust-avif-serialize-0.7)
        ("rust-imgref" ,rust-imgref-1)
        ("rust-loop9" ,rust-loop9-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-quick-error" ,rust-quick-error-2)
        ("rust-rav1e" ,rav1e)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rgb" ,rust-rgb-0.8))
       #:cargo-development-inputs
       (("rust-avif-parse" ,rust-avif-parse-1))))))

(define-public rust-ravif-0.6
  (package
    (inherit rust-ravif-0.8)
    (name "rust-ravif")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ravif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rfm63bv0q2rjyivmzlk4wma8xff6jralh7dr1bjz2aw4knm7cw1"))))
    (arguments
     `(#:skip-build? #t     ; Not packaging older versions of rav1e.
       #:cargo-inputs
       (("rust-avif-serialize" ,rust-avif-serialize-0.6)
        ("rust-imgref" ,rust-imgref-1)
        ("rust-loop9" ,rust-loop9-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rav1e" ,rav1e)    ; 0.5
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rgb" ,rust-rgb-0.8))
       #:cargo-development-inputs
       (("rust-avif-parse" ,rust-avif-parse-0.13))))
    (native-inputs
     (list nasm))                 ;for building rav1e
    (inputs '())))

(define-public rust-raw-window-handle-0.6
  (package
    (name "rust-raw-window-handle")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "raw-window-handle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i5mxxfcgqmvmzg4f0lcz68g4xfd9jybhrdkxd2v37qv1q587aa2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/rust-windowing/raw-window-handle")
    (synopsis "Interoperability library for Rust Windowing applications")
    (description
     "Interoperability library for Rust Windowing applications.")
    (license (list license:expat license:asl2.0 license:zlib))))

(define-public rust-raw-window-handle-0.5
  (package
    (inherit rust-raw-window-handle-0.6)
    (name "rust-raw-window-handle")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "raw-window-handle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1f9k10fgda464ia1b2hni8f0sa8i0bphdsbs3di032x80qgrmzzj"))))
    (arguments '())))

(define-public rust-raw-window-handle-0.4
  (package
    (inherit rust-raw-window-handle-0.5)
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
    (arguments
     `(#:cargo-inputs
       (("rust-cty" ,rust-cty-0.2))))))

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

(define-public rust-raw-window-metal-0.3
  (package
    (name "rust-raw-window-metal")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "raw-window-metal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xlfy69chky5gxk67p9h7zmf4n4y15fk9abani6c4m4d4n9s8kmc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cocoa" ,rust-cocoa-0.25)
                       ("rust-core-graphics" ,rust-core-graphics-0.23)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.5))))
    (home-page "https://github.com/norse-rs/raw-window-metal")
    (synopsis "Interop library between Metal and raw-window-handle")
    (description "Interop library between Metal and raw-window-handle.")
    (license (list license:expat license:asl2.0))))

(define-public rust-resize-0.7
  (package
    (name "rust-resize")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "resize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hdd5r2m1700y6r88v5hq3q28xixrsbfhbzqz26409jyy3zvvrw7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fallible-collections" ,rust-fallible-collections-0.4)
        ("rust-rgb" ,rust-rgb-0.8))
       #:cargo-development-inputs
       (("rust-png" ,rust-png-0.17))))
    (home-page "https://github.com/PistonDevelopers/resize")
    (synopsis "Image resampling library in pure Rust")
    (description "This package provides an image resampling library in pure Rust.")
    (license license:expat)))

(define-public rust-resize-0.4
  (package
    (inherit rust-resize-0.7)
    (name "rust-resize")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "resize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bamrw2m37l8q46mcy6snp6106d93dq7x67hbbj32w88pjdhxn84"))))
    (arguments
     `(#:cargo-development-inputs (("rust-png" ,rust-png-0.16))))))

(define-public rust-rgb-0.8
  (package
    (name "rust-rgb")
    (version "0.8.37")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rgb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n275hng7hmnzjavmdf24vqd86nm6bkg80nhr4zmgzb49c0aiah5"))))
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

(define-public rust-scad-1
  (package
    (name "rust-scad")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "scad" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yvy7ckfd7r261iywm75na1ykd9cl8h0q8ajb1iwg1jmnbs6vry6"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "docs")
           ;; The very next commit in the repository updates nalgebra
           (substitute* "Cargo.toml"
             (("0\\.16\\.8") "0.27.1"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=common_objects::tests::cube_center_x"
         "--skip=common_objects::tests::cube_center_yz")
       #:cargo-inputs
       (("rust-nalgebra" ,rust-nalgebra-0.27))))
    (home-page "https://github.com/thezoq2/Rust-Scad")
    (synopsis "Crate for generating OpenSCAD models using Rust")
    (description
     "This package provides a crate for generating @code{OpenSCAD} models
using Rust.")
    (license license:lgpl2.0+)))

(define-public rust-sdl2-0.35
  (package
    (name "rust-sdl2")
    (version "0.35.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sdl2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "06ivcavxhc7zyhbfmy2544dz0lnaqf33d9xf0jggpw93nrvr55gp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Building requires several SDL2 inputs.
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-c-vec" ,rust-c-vec-2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.4)
        ("rust-sdl2-sys" ,rust-sdl2-sys-0.35))))
    (home-page "https://github.com/Rust-SDL2/rust-sdl2")
    (synopsis "SDL2 bindings for Rust")
    (description "This package provides SDL2 bindings for Rust.")
    (license license:expat)))

(define-public rust-sdl2-sys-0.35
  (package
    (name "rust-sdl2-sys")
    (version "0.35.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sdl2-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1w7ranfpmbvsnviq0y8d1cz9pajp8c4b84lslycq02kcrzi6nn73"))
              (modules '((guix build utils)))
              (snippet
               '(begin (delete-file-recursively "SDL")))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Building requires several SDL2 inputs.
       #:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.53)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cmake" ,rust-cmake-0.1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-vcpkg" ,rust-vcpkg-0.2)
        ("rust-version-compare" ,rust-version-compare-0.1))))
    (home-page "https://github.com/rust-sdl2/rust-sdl2")
    (synopsis "Raw SDL2 bindings for Rust, used internally rust-sdl2")
    (description "This package provides raw SDL2 bindings for Rust, used
internally rust-sdl2.")
    (license license:expat)))

(define-public rust-smithay-client-toolkit-0.18
  (package
    (name "rust-smithay-client-toolkit")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03v0h71qzg5iw5nd2k15a50ic55a9wq6bc7l5dyczfm33yadkqv0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Cut the dependency chain
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-calloop" ,rust-calloop-0.12)
        ("rust-calloop-wayland-source" ,rust-calloop-wayland-source-0.2)
        ("rust-cursor-icon" ,rust-cursor-icon-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-memmap2" ,rust-memmap2-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-rustix" ,rust-rustix-0.38)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-wayland-backend" ,rust-wayland-backend-0.3)
        ("rust-wayland-client" ,rust-wayland-client-0.31)
        ("rust-wayland-csd-frame" ,rust-wayland-csd-frame-0.3)
        ("rust-wayland-cursor" ,rust-wayland-cursor-0.31)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.31)
        ("rust-wayland-protocols-wlr" ,rust-wayland-protocols-wlr-0.2)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.31)
        ("rust-xkbcommon" ,rust-xkbcommon-0.7)
        ("rust-xkeysym" ,rust-xkeysym-0.2))))
    (home-page "https://github.com/smithay/client-toolkit")
    (synopsis "Toolkit for making client Wayland applications")
    (description
     "This package provides a toolkit for making client Wayland applications.")
    (license license:expat)))

(define-public rust-smithay-client-toolkit-0.16
  (package
    (inherit rust-smithay-client-toolkit-0.18)
    (name "rust-smithay-client-toolkit")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "smithay-client-toolkit" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0m7l0zhl9s3321yj8z6hf1g0w3l2ay85irgcw2r5wwfj69yw81zk"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.10)
        ("rust-dlib" ,rust-dlib-0.5)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-memmap2" ,rust-memmap2-0.5)
        ("rust-nix" ,rust-nix-0.24)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-wayland-client" ,rust-wayland-client-0.29)
        ("rust-wayland-cursor" ,rust-wayland-cursor-0.29)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.29))
       #:cargo-development-inputs (("rust-image" ,rust-image-0.24))))))

(define-public rust-smithay-client-toolkit-0.15
  (package
    (inherit rust-smithay-client-toolkit-0.16)
    (name "rust-smithay-client-toolkit")
    (version "0.15.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18wxla80y6m4l3dwawi7bl1d9m9dfcg4sxxjcgjqq3psjxmg2a4a"))))
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
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.29))))))

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

(define-public rust-smithay-client-toolkit-0.4
  (package
    (inherit rust-smithay-client-toolkit-0.12)
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

(define-public rust-smithay-clipboard-0.7
  (package
    (name "rust-smithay-clipboard")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-clipboard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19m1rqw4fsp9x92cji9qz169004djjh376b68ylcp9g51hl2pdhb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.18)
        ("rust-wayland-backend" ,rust-wayland-backend-0.3))
       #:cargo-development-inputs
       (("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.18))))
    (native-inputs (list pkg-config))
    (inputs (list libxkbcommon))
    (home-page "https://github.com/smithay/smithay-clipboard")
    (synopsis "Access to the Wayland clipboard for client applications")
    (description
     "This package provides access to the Wayland clipboard for client
applications.")
    (license license:expat)))

(define-public rust-smithay-clipboard-0.6
  (package
    (inherit rust-smithay-clipboard-0.7)
    (name "rust-smithay-clipboard")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-clipboard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s5hyhbmnk75i0sm14wy4dy7c576a4dyi1chfwdhpbhz1a3mqd0a"))))
    (arguments
     `(#:cargo-inputs
       (("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.16)
        ("rust-wayland-client" ,rust-wayland-client-0.29))
       #:cargo-development-inputs
       (("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.16))))))

(define-public rust-softbuffer-0.3
  (package
    (name "rust-softbuffer")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "softbuffer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j199d8zg964324sppk1gnkq2361ivay7ykrlm71npg8v3ma4vc2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-as-raw-xcb-connection" ,rust-as-raw-xcb-connection-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-cfg-aliases" ,rust-cfg-aliases-0.1)
                       ("rust-cocoa" ,rust-cocoa-0.25)
                       ("rust-core-graphics" ,rust-core-graphics-0.23)
                       ("rust-drm" ,rust-drm-0.10)
                       ("rust-fastrand" ,rust-fastrand-2)
                       ("rust-foreign-types" ,rust-foreign-types-0.5)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
                       ("rust-redox-syscall" ,rust-redox-syscall-0.4)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-tiny-xlib" ,rust-tiny-xlib-0.2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-sys" ,rust-wayland-sys-0.31)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-windows-sys" ,rust-windows-sys-0.48)
                       ("rust-x11rb" ,rust-x11rb-0.12))
       #:cargo-development-inputs
       (("rust-colorous" ,rust-colorous-1)
        ("rust-criterion" ,rust-criterion-0.4)
        ("rust-image" ,rust-image-0.24)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rustix" ,rust-rustix-0.38)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
        ("rust-winit" ,rust-winit-0.28)
        ("rust-winit-test" ,rust-winit-test-0.1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'add-absolute-library-references
           (lambda* (#:key inputs vendor-dir #:allow-other-keys)
             (substitute* (find-files vendor-dir "\\.rs$")
               (("libX11\\.so")
                (search-input-file inputs "lib/libX11.so"))
               (("libX11-xcb\\.so")
                (search-input-file inputs "lib/libX11-xcb.so"))
               ;; Lots of libraries from rust-x11-dl and others.
               (("libX[[:alpha:]]*\\.so" all)
                (search-input-file inputs (string-append "lib/" all))))))
         (add-before 'check 'pre-check
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             ;; Most tests require an X server.
             (let ((xvfb (search-input-file (or native-inputs inputs)
                                            "bin/Xvfb"))
                   (display ":1"))
               (setenv "DISPLAY" display)
               (system (string-append xvfb " " display " &"))))))))
    (inputs (list libx11
                  libxcursor
                  libxext
                  libxft
                  libxi
                  libxinerama
                  libxmu
                  libxpresent
                  libxrandr
                  libxscrnsaver
                  libxt
                  libxtst))
    (native-inputs (list xorg-server-for-tests))
    (home-page "https://github.com/rust-windowing/softbuffer")
    (synopsis "Cross-platform software buffer")
    (description "Cross-platform software buffer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tiff-0.9
  (package
    (name "rust-tiff")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04b2fd3clxm0pmdlfip8xj594zyrsfwmh641i6x1gfiz9l7jn5vd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-flate2" ,rust-flate2-1)
                       ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.3)
                       ("rust-weezl" ,rust-weezl-0.1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/image-rs/image-tiff")
    (synopsis "TIFF decoding and encoding library in pure Rust")
    (description
     "This package provides TIFF decoding and encoding library in pure Rust.")
    (license license:expat)))

(define-public rust-tiff-0.8
  (package
    (inherit rust-tiff-0.9)
    (name "rust-tiff")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tiff" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0wg4a6w8sakyy0mggblg340mx8bgglx9hwsxsn8g5fpjkx7k6jbl"))))
    (arguments
     `(#:tests? #f              ; Not all files included
       #:cargo-inputs
       (("rust-flate2" ,rust-flate2-1)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.3)
        ("rust-weezl" ,rust-weezl-0.1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3))))))

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

(define-public rust-tiny-xlib-0.2
  (package
    (name "rust-tiny-xlib")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiny-xlib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vgbk1n6mk9vrvd228bpila359cz7vb9pbhyim507alv4r4qs2fl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=error_handling"
                            "--skip=remove_and_re_insert"
                            "--skip=smoke"
                            "--skip=replace_old_handler")
       #:cargo-inputs (("rust-as-raw-xcb-connection" ,rust-as-raw-xcb-connection-1)
                       ("rust-ctor" ,rust-ctor-0.2)
                       ("rust-libloading" ,rust-libloading-0.8)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-x11-dl" ,rust-x11-dl-2)
        ("rust-x11rb" ,rust-x11rb-0.12))))
    (inputs
     (list libx11))
    (home-page "https://github.com/notgull/tiny-xlib")
    (synopsis "Tiny Xlib wrapper for Rust")
    (description "This package provides a tiny Xlib wrapper for Rust.")
    (license (list license:expat license:asl2.0 license:zlib))))

(define-public rust-wayland-backend-0.3
  (package
    (name "rust-wayland-backend")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-backend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h4s8nfrl1q8xys1409lfwkb70cdh81c0pvzr1s69mwhrrhzll4x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `wayland_scanner`
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-downcast-rs" ,rust-downcast-rs-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-scoped-tls" ,rust-scoped-tls-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-wayland-sys" ,rust-wayland-sys-0.31))
       #:cargo-development-inputs (("rust-concat-idents" ,rust-concat-idents-1)
                                   ("rust-env-logger" ,rust-env-logger-0.10))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Low-level bindings to the Wayland protocol")
    (description "This package provides low-level bindings to the Wayland
protocol.")
    (license license:expat)))

(define-public rust-wayland-backend-0.1
  (package
    (inherit rust-wayland-backend-0.3)
    (name "rust-wayland-backend")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-backend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n1yi6vna23wfkrpk1j46sx5qbsijh50viha4sra73by8lkqxd21"))))
    (arguments
     `(#:tests? #f      ; Use of undeclared dependencies
       #:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-io-lifetimes" ,rust-io-lifetimes-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-nix" ,rust-nix-0.26)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-wayland-sys" ,rust-wayland-sys-0.30))
       #:cargo-development-inputs
       (("rust-concat-idents" ,rust-concat-idents-1)
        ("rust-env-logger" ,rust-env-logger-0.10))))))

(define-public rust-wayland-client-0.31
  (package
    (name "rust-wayland-client")
    (version "0.31.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07rzml07li3bi4nnqx4i2rfj3xkifzxp1d6cd1kflb2wjgp9dyw2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `wayland_protocols`
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31))
       #:cargo-development-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Rust bindings to the standard C implementation of the wayland protocol")
    (description
     "This package provides Rust bindings to the standard C implementation of
the wayland protocol, client side.")
    (license license:expat)))

(define-public rust-wayland-client-0.30
  (package
    (inherit rust-wayland-client-0.31)
    (name "rust-wayland-client")
    (version "0.30.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1j3as2g1znrs2lpkksqcvx8pag85yiwwbcv6wb3lyrqgfxa9d728"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `wayland_protocols`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-nix" ,rust-nix-0.26)
        ("rust-wayland-backend" ,rust-wayland-backend-0.1)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.30))
       #:cargo-development-inputs
       (("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-wayland-client-0.29
  (package
    (inherit rust-wayland-client-0.30)
    (name "rust-wayland-client")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05b7qikqj22rjy17kqw5ar7j2chpy18dr0gqapvwjfd00n60cfrz"))))
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
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-wayland-client-0.28
  (package
    (inherit rust-wayland-client-0.29)
    (name "rust-wayland-client")
    (version "0.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m831sj4w5k0j9167f2dy3815k73g153j09271cz20p5a0ik7az3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `wayland_protocols`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nix" ,rust-nix-0.20)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-wayland-commons" ,rust-wayland-commons-0.28)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.28)
        ("rust-wayland-sys" ,rust-wayland-sys-0.28))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

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
        ("rust-tempfile" ,rust-tempfile-3))))))

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
    (version "0.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-commons" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1npvcrwh8chjcji73c24hlp05zbv6dxv24bylb8bn4bhgja1f652"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.20)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-wayland-sys" ,rust-wayland-sys-0.28))))))

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
        ("rust-wayland-sys" ,rust-wayland-sys-0.23))))))

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

(define-public rust-wayland-csd-frame-0.3
  (package
    (name "rust-wayland-csd-frame")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-csd-frame" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zjcmcqprfzx57hlm741n89ssp4sha5yh5cnmbk2agflvclm0p32"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cursor-icon" ,rust-cursor-icon-1)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3))))
    (home-page "https://github.com/rust-windowing/wayland-csd-frame")
    (synopsis "Common trait and types for wayland CSD interop")
    (description "This package provides common trait and types for wayland
@acronym{CSD, Client Side Decorations} interop.")
    (license license:expat)))

(define-public rust-wayland-cursor-0.31
  (package
    (name "rust-wayland-cursor")
    (version "0.31.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-cursor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fii68l6x235b867q96yx1xqkl16azkf5i841ldd24yxd2l5zkki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustix" ,rust-rustix-0.38)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-xcursor" ,rust-xcursor-0.3))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Bindings to libwayland-cursor")
    (description
     "This crate provides helpers to load the system provided cursor images
and load them into WlBuffers as well as obtain the necessary metadata to
properly display animated cursors.")
    (license license:expat)))

(define-public rust-wayland-cursor-0.29
  (package
    (inherit rust-wayland-cursor-0.31)
    (name "rust-wayland-cursor")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-cursor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qbn6wqmjibkx3lb3ggbp07iabzgx2zhrm0wxxxjbmhkdyvccrb8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.24)
        ("rust-wayland-client" ,rust-wayland-client-0.29)
        ("rust-xcursor" ,rust-xcursor-0.3))))))

(define-public rust-wayland-cursor-0.28
  (package
    (inherit rust-wayland-cursor-0.29)
    (name "rust-wayland-cursor")
    (version "0.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-cursor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nm61zkxwddq9x64dalcb5rihz2w6kz7blmxwx2nsn6ixn200qdy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.20)
        ("rust-wayland-client" ,rust-wayland-client-0.28)
        ("rust-xcursor" ,rust-xcursor-0.3))))))

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
    (native-inputs (list pkg-config))
    (inputs (list wayland))
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
    (version "0.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-egl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mk9yv9b5w64syi09x0ma3s7s7ajdn2hhvykh8wv4ml7w6qimflr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-wayland-client" ,rust-wayland-client-0.28)
                       ("rust-wayland-sys" ,rust-wayland-sys-0.28))))))

(define-public rust-wayland-protocols-0.31
  (package
    (name "rust-wayland-protocols")
    (version "0.31.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x310l1p6p3p3l76nl1l2yava9408dy77s605917zadlp1jz70cg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31)
                       ("rust-wayland-server" ,rust-wayland-server-0.31))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generated API for the officials Wayland protocol extensions")
    (description
     "This package provides a generated API for the officials Wayland protocol
extensions.")
    (license license:expat)))

(define-public rust-wayland-protocols-0.30
  (package
    (inherit rust-wayland-protocols-0.31)
    (name "rust-wayland-protocols")
    (version "0.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0kcvvli38gdjb9c7dpa2s0ix4nnqfq7n2bbc39370kx9bhg10a1v"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-wayland-backend" ,rust-wayland-backend-0.1)
        ("rust-wayland-client" ,rust-wayland-client-0.30)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.30)
        ("rust-wayland-server" ,rust-wayland-server-0.30))))))

(define-public rust-wayland-protocols-0.29
  (package
    (inherit rust-wayland-protocols-0.30)
    (name "rust-wayland-protocols")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ihbjyd0w460gd7w22g9qabbwd4v8x74f8vsh7p25csljcgn4l5r"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-wayland-client" ,rust-wayland-client-0.29)
        ("rust-wayland-commons" ,rust-wayland-commons-0.29)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.29)
        ("rust-wayland-server" ,rust-wayland-server-0.29))))))

(define-public rust-wayland-protocols-0.28
  (package
    (inherit rust-wayland-protocols-0.29)
    (name "rust-wayland-protocols")
    (version "0.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07w6kyr05fmajq3i8npfk4q6j5p35qja91x03zvaqfw09pm20ri8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-wayland-client" ,rust-wayland-client-0.28)
        ("rust-wayland-commons" ,rust-wayland-commons-0.28)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.28)
        ("rust-wayland-server" ,rust-wayland-server-0.28))))))

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
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.23))))))

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

(define-public rust-wayland-protocols-plasma-0.2
  (package
    (name "rust-wayland-protocols-plasma")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols-plasma" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yglbxd4ka8284k0j0ssh9hf6wd9qp2n0s2qrsdymyaz258kb013"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-protocols" ,rust-wayland-protocols-0.31)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31)
                       ("rust-wayland-server" ,rust-wayland-server-0.31))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generated API for the Plasma wayland protocol extensions")
    (description "Generated API for the Plasma wayland protocol extensions.")
    (license license:expat)))

(define-public rust-wayland-protocols-wlr-0.2
  (package
    (name "rust-wayland-protocols-wlr")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols-wlr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mjww9psk2nc5hm2q4s3qas30rbzfg1sb6qgw518fbbcdfvn27xd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-protocols" ,rust-wayland-protocols-0.31)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31)
                       ("rust-wayland-server" ,rust-wayland-server-0.31))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generated API for the WLR wayland protocol extensions")
    (description "This package provides generated API for the WLR wayland
protocol extensions.")
    (license license:expat)))

(define-public rust-wayland-protocols-wlr-0.1
  (package
    (inherit rust-wayland-protocols-wlr-0.2)
    (name "rust-wayland-protocols-wlr")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols-wlr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "12jqi7n77l8a13hc5w5fkdgs4kdjk9i6nvl74njsdr106c4r3sgw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-wayland-backend" ,rust-wayland-backend-0.1)
        ("rust-wayland-client" ,rust-wayland-client-0.30)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.30)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.30)
        ("rust-wayland-server" ,rust-wayland-server-0.30))))))

(define-public rust-wayland-scanner-0.31
  (package
    (name "rust-wayland-scanner")
    (version "0.31.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10y2nq076x4zml8wc5bw75560rwvrsfpi35mdyc02w1854lsdcv3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=client_gen::tests::client_gen"
                            "--skip=interfaces::tests::interface_gen"
                            "--skip=server_gen::tests::server_gen")
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quick-xml" ,rust-quick-xml-0.31)
                       ("rust-quote" ,rust-quote-1))
       #:cargo-development-inputs (("rust-similar" ,rust-similar-2))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generate Rust APIs from XML Wayland protocol files")
    (description
     "Wayland Scanner generates Rust APIs from XML Wayland protocol files.
It is intended for use with wayland-sys.  You should only need this crate if
you are working on custom Wayland protocol extensions.  Look at the
wayland-client crate for usable bindings.")
    (license license:expat)))

(define-public rust-wayland-scanner-0.30
  (package
    (inherit rust-wayland-scanner-0.31)
    (name "rust-wayland-scanner")
    (version "0.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03ikmfwacsgbym2y4raf05knl1qjlgg81sy0174jxhzvayr77f5r"))))
    (arguments
     `(#:tests? #f      ; Tests expect running wayland instance.
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quick-xml" ,rust-quick-xml-0.28)
        ("rust-quote" ,rust-quote-1))
       #:cargo-development-inputs
       (("rust-similar" ,rust-similar-2))))))

(define-public rust-wayland-scanner-0.29
  (package
    (inherit rust-wayland-scanner-0.30)
    (name "rust-wayland-scanner")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lxx3i2kxnmsk421qx87lqqc9kd2y1ksjxcyg0pqbar2zbc06hwg"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-xml-rs" ,rust-xml-rs-0.8))))))

(define-public rust-wayland-scanner-0.28
  (package
    (inherit rust-wayland-scanner-0.29)
    (name "rust-wayland-scanner")
    (version "0.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w839jsh7nrni4f2x5bkapf98w7kddxyqmpks4rf67dnvsr3x4nf"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-xml-rs" ,rust-xml-rs-0.8))))))

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

(define-public rust-wayland-server-0.31
  (package
    (name "rust-wayland-server")
    (version "0.31.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dzxfabykj156abnbx2i8j8gvmgb9yys9mfj9sx29g45qbay9rh0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-downcast-rs" ,rust-downcast-rs-1)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Bindings to the standard C implementation of the wayland protocol")
    (description
     "This package provides Rust bindings to the standard C implementation of
the wayland protocol, server side.")
    (license license:expat)))

(define-public rust-wayland-server-0.30
  (package
    (inherit rust-wayland-server-0.31)
    (name "rust-wayland-server")
    (version "0.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fnjhhcbnwgyplawc02v3b6nkxnhzl2981yiyzzlj7gyjs0c4hww"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-io-lifetimes" ,rust-io-lifetimes-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-nix" ,rust-nix-0.26)
        ("rust-wayland-backend" ,rust-wayland-backend-0.1)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.30))))))

(define-public rust-wayland-server-0.29
  (package
    (inherit rust-wayland-server-0.30)
    (name "rust-wayland-server")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11ickjbqpfs19wakf56l3j9asjcfw00d7wj48s3i99yvv1nq8q2k"))))
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
        ("rust-wayland-sys" ,rust-wayland-sys-0.29))))))

(define-public rust-wayland-server-0.28
  (package
    (inherit rust-wayland-server-0.29)
    (name "rust-wayland-server")
    (version "0.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f2lclv6x794a48m3anqgx7qzz5s5dvbsj3xahnmz5izk1bhq7qb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nix" ,rust-nix-0.20)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-wayland-commons" ,rust-wayland-commons-0.28)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.28)
        ("rust-wayland-sys" ,rust-wayland-sys-0.28))))))

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
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.23))))))

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

(define-public rust-wayland-sys-0.31
  (package
    (name "rust-wayland-sys")
    (version "0.31.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bxpwamgagpxa8p9m798gd3g6rwj2m4sbdvc49zx05jjzzmci80m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dlib" ,rust-dlib-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memoffset" ,rust-memoffset-0.9)
                       ("rust-once-cell" ,rust-once-cell-1)
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
    (inputs (list wayland))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "FFI bindings to the various @file{libwayland-*.so} libraries")
    (description
     "This package provides FFI bindings to the various
@file{libwayland-*.so} libraries.  You should only need this crate if
you are working on custom Wayland protocol extensions.  Look at the
crate @code{rust-wayland-client} for usable bindings.")
    (license license:expat)))

(define-public rust-wayland-sys-0.30
  (package
    (inherit rust-wayland-sys-0.31)
    (name "rust-wayland-sys")
    (version "0.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01man4ll2kyxp9x2934rhnf98522pzwsd2c6jwr73q08qqma1cln"))))
    (arguments
     `(#:cargo-inputs
       (("rust-dlib" ,rust-dlib-0.5)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-memoffset" ,rust-memoffset-0.7)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libwayland (dirname (search-input-file
                                         inputs "lib/libwayland-client.so"))))
               (substitute* (find-files "src" "\\.rs$")
                 (("libwayland.*\\.so" shared-lib)
                  (string-append libwayland "/" shared-lib)))))))))))

(define-public rust-wayland-sys-0.29
  (package
    (inherit rust-wayland-sys-0.30)
    (name "rust-wayland-sys")
    (version "0.29.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m79qqmr1hx7jlyrvnrxjma5s6dk5js9fjsr4nx7vv1r7hdcw4my"))))
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
    (inputs (list wayland))))

(define-public rust-wayland-sys-0.28
  (package
    (inherit rust-wayland-sys-0.29)
    (name "rust-wayland-sys")
    (version "0.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f7yy3c6h270xd4wk2nsrr9433gmkg29d5rfxndvzznpmslzqhfq"))))
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
             (let ((libwayland (assoc-ref inputs "wayland")))
               (substitute* (find-files "src" "\\.rs$")
                 (("libwayland.*\\.so" shared-lib)
                  (string-append libwayland "/lib/" shared-lib)))))))))
    (inputs (list wayland))))

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
        ("rust-libc" ,rust-libc-0.2))))))

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

(define-public rust-wayrs-client-1
  (package
    (name "rust-wayrs-client")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayrs-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18i19b7z4wp0mjwn256ps3pdfk47lx45h0cx8wkjlv7akkgzxnj7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nix" ,rust-nix-0.27)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-wayrs-scanner" ,rust-wayrs-scanner-0.13))))
    (home-page "https://github.com/MaxVerevkin/wayrs")
    (synopsis "Wayland client library")
    (description "This package provides a wayland client library.")
    (license license:expat)))

(define-public rust-wayrs-proto-parser-2
  (package
    (name "rust-wayrs-proto-parser")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayrs-proto-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a9gb0iv6rm1z2n4isfb9x05pv6wlgn23hljb9s038m4dgmyn1hl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-quick-xml" ,rust-quick-xml-0.31)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/MaxVerevkin/wayrs")
    (synopsis "Parser for wayland protocol xml files")
    (description "This package provides a parser for wayland protocol
xml files.")
    (license license:expat)))

(define-public rust-wayrs-protocols-0.13
  (package
    (name "rust-wayrs-protocols")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayrs-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1888h4wh3knjwh19v4y2gjpsv5j78c4vk08gls5f3aa1iryfpd7x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-wayrs-client" ,rust-wayrs-client-1))))
    (home-page "https://github.com/MaxVerevkin/wayrs")
    (synopsis "Collection of Wayland protocols to use with wayrs-client")
    (description
     "This package provides a collection of Wayland protocols to use
with wayrs-client.")
    (license license:expat)))

(define-public rust-wayrs-scanner-0.13
  (package
    (name "rust-wayrs-scanner")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayrs-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07xzg36rnnsb4z4rd82r2mk3y05vg1ssfwrry2kd4yz395sx91z3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-wayrs-proto-parser" ,rust-wayrs-proto-parser-2))))
    (home-page "https://github.com/MaxVerevkin/wayrs")
    (synopsis "Generates code for wayrs-client from xml files")
    (description "This package provides code generation for wayrs-client
from xml files.")
    (license license:expat)))

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

(define-public rust-winit-0.29
  (package
    (name "rust-winit")
    (version "0.29.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kzzl8m68sq6njdr6n1b90ppfg1pfhkcq48iqxpfdshyjh8lz0jc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ahash" ,rust-ahash-0.8)
        ("rust-android-activity" ,rust-android-activity-0.5)
        ("rust-atomic-waker" ,rust-atomic-waker-1)
        ("rust-bitflags" ,rust-bitflags-2)
        ("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-calloop" ,rust-calloop-0.12)
        ("rust-cfg-aliases" ,rust-cfg-aliases-0.1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.23)
        ("rust-cursor-icon" ,rust-cursor-icon-1)
        ("rust-icrate" ,rust-icrate-0.0.4)
        ("rust-js-sys" ,rust-js-sys-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-memmap2" ,rust-memmap2-0.9)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-ndk" ,rust-ndk-0.8)
        ("rust-ndk-sys" ,rust-ndk-sys-0.5)
        ("rust-objc2" ,rust-objc2-0.4)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-orbclient" ,rust-orbclient-0.3)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.6)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.4)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
        ("rust-redox-syscall" ,rust-redox-syscall-0.3)
        ("rust-rustix" ,rust-rustix-0.38)
        ("rust-sctk-adwaita" ,rust-sctk-adwaita-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.18)
        ("rust-smol-str" ,rust-smol-str-0.2)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
        ("rust-wayland-backend" ,rust-wayland-backend-0.3)
        ("rust-wayland-client" ,rust-wayland-client-0.31)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.31)
        ("rust-wayland-protocols-plasma" ,rust-wayland-protocols-plasma-0.2)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-web-time" ,rust-web-time-0.2)
        ("rust-windows-sys" ,rust-windows-sys-0.48)
        ("rust-x11-dl" ,rust-x11-dl-2)
        ("rust-x11rb" ,rust-x11rb-0.13)
        ("rust-xkbcommon-dl" ,rust-xkbcommon-dl-0.4))
       #:cargo-development-inputs
       (("rust-console-log" ,rust-console-log-1)
        ("rust-image" ,rust-image-0.24)
        ("rust-simple-logger" ,rust-simple-logger-4)
        ("rust-softbuffer" ,rust-softbuffer-0.3)
        ("rust-web-sys" ,rust-web-sys-0.3))))
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

(define-public rust-winit-0.28
  (package
    (inherit rust-winit-0.29)
    (name "rust-winit")
    (version "0.28.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "152bi6lrmnasg6dnsdjqgnzyis3n90i09cja720m4krq8l5xk5lm"))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-activity" ,rust-android-activity-0.4)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-cfg-aliases" ,rust-cfg-aliases-0.1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.22)
        ("rust-dispatch" ,rust-dispatch-0.2)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-mio" ,rust-mio-0.8)
        ("rust-ndk" ,rust-ndk-0.7)
        ("rust-objc2" ,rust-objc2-0.3)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-orbclient" ,rust-orbclient-0.3)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
        ("rust-redox-syscall" ,rust-redox-syscall-0.3)
        ("rust-sctk-adwaita" ,rust-sctk-adwaita-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.16)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wayland-client" ,rust-wayland-client-0.29)
        ("rust-wayland-commons" ,rust-wayland-commons-0.29)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.29)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.29)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-windows-sys" ,rust-windows-sys-0.45)
        ("rust-x11-dl" ,rust-x11-dl-2))
       #:cargo-development-inputs
       (("rust-console-log" ,rust-console-log-0.2)
        ("rust-image" ,rust-image-0.24)
        ("rust-simple-logger" ,rust-simple-logger-2)
        ("rust-web-sys" ,rust-web-sys-0.3))))))

(define-public rust-winit-0.27
  (package
    (inherit rust-winit-0.28)
    (name "rust-winit")
    (version "0.27.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z35jymza04gjniq0mmydq3m1mrr9pqfcwcldj4zvcl6pmpnsydv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cocoa" ,rust-cocoa-0.24)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.22)
        ("rust-dispatch" ,rust-dispatch-0.2)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-mio" ,rust-mio-0.8)
        ("rust-ndk" ,rust-ndk-0.7)
        ("rust-ndk-glue" ,rust-ndk-glue-0.7)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.4)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
        ("rust-sctk-adwaita" ,rust-sctk-adwaita-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.16)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wayland-client" ,rust-wayland-client-0.29)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.29)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-windows-sys" ,rust-windows-sys-0.36)
        ("rust-x11-dl" ,rust-x11-dl-2))
       #:cargo-development-inputs (("rust-console-log" ,rust-console-log-0.2)
                                   ("rust-image" ,rust-image-0.24)
                                   ("rust-simple-logger" ,rust-simple-logger-2))))
    (native-inputs (list pkg-config))
    (inputs (list expat fontconfig freetype))))

(define-public rust-winit-0.26
  (package
    (inherit rust-winit-0.28)
    (name "rust-winit")
    (version "0.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fp7cdh7llbqmm6ga8f6bzk9785jmkbyy1w631hr9faq3n9wqhwv"))))
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
        ("rust-simple-logger" ,rust-simple-logger-1))))))

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
        ("rust-simple-logger" ,rust-simple-logger-1))))))

(define-public rust-winit-0.19
  (package
    (inherit rust-winit-0.24)
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

(define-public rust-winit-test-0.1
  (package
    (name "rust-winit-test")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1czdg3zvwazng2gwlda1nb26hklk1qizz84h97bk9mv2jf52yjx3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-owo-colors" ,rust-owo-colors-3)
                       ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                       ("rust-web-time" ,rust-web-time-0.2)
                       ("rust-winit" ,rust-winit-0.28))
       #:cargo-development-inputs (("rust-winit" ,rust-winit-0.28))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'add-absolute-library-references
           (lambda* (#:key inputs vendor-dir #:allow-other-keys)
             (substitute* (find-files vendor-dir "\\.rs$")
               (("libX11\\.so")
                (search-input-file inputs "lib/libX11.so"))
               (("libX11-xcb\\.so")
                (search-input-file inputs "lib/libX11-xcb.so"))
               ;; Lots of libraries from rust-x11-dl and others.
               (("libX[[:alpha:]]*\\.so" all)
                (search-input-file inputs (string-append "lib/" all))))))
         (add-before 'check 'pre-check
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             ;; Tests do not expect XDG_RUNTIME_DIR to be empty.
             (setenv "XDG_RUNTIME_DIR" "/tmp")

             ;; Most tests require an X server.
             (let ((xvfb (search-input-file (or native-inputs inputs)
                                            "bin/Xvfb"))
                   (display ":1"))
               (setenv "DISPLAY" display)
               (system (string-append xvfb " " display " &"))))))))
    (inputs (list libx11
                  libxcursor
                  libxext
                  libxft
                  libxi
                  libxinerama
                  libxmu
                  libxpresent
                  libxrandr
                  libxscrnsaver
                  libxt
                  libxtst))
    (native-inputs (list xorg-server-for-tests))
    (home-page "https://github.com/notgull/winit-test")
    (synopsis "Run tests using the winit event loop")
    (description "Run tests using the winit event loop.")
    (license (list license:expat license:asl2.0 license:zlib))))

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

(define-public rust-x11rb-0.13
  (package
    (name "rust-x11rb")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11rb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06lzpmb67sfw37m0i9zz786hx6fklmykd9j3689blk3yijnmxwpq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-as-raw-xcb-connection" ,rust-as-raw-xcb-connection-1)
                       ("rust-gethostname" ,rust-gethostname-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libloading" ,rust-libloading-0.8)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-x11rb-protocol" ,rust-x11rb-protocol-0.13))
       #:cargo-development-inputs
       (("rust-gethostname" ,rust-gethostname-0.4)
        ("rust-polling" ,rust-polling-3)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/psychon/x11rb")
    (synopsis "Rust bindings to X11")
    (description "This package provides Rust bindings to X11.")
    (license (list license:expat license:asl2.0))))

(define-public rust-x11rb-0.12
  (package
    (inherit rust-x11rb-0.13)
    (name "rust-x11rb")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11rb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02h492k920mb1733cdmly138zfiwkspil6ssqcvi7inyshk1nr5i"))))
    (arguments
     `(#:cargo-inputs (("rust-as-raw-xcb-connection" ,rust-as-raw-xcb-connection-1)
                       ("rust-gethostname" ,rust-gethostname-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libloading" ,rust-libloading-0.7)
                       ("rust-nix" ,rust-nix-0.26)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-winapi-wsapoll" ,rust-winapi-wsapoll-0.1)
                       ("rust-x11rb-protocol" ,rust-x11rb-protocol-0.12))
       #:cargo-development-inputs
       (("rust-polling" ,rust-polling-2)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))))

(define-public rust-x11rb-0.11
  (package
    (inherit rust-x11rb-0.13)
    (name "rust-x11rb")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11rb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05wn86brwm1cd8xgrbs9s1fy71asav8qlsrnlzkvm4fx2aacgwyd"))))
    (arguments
     `(#:cargo-inputs (("rust-gethostname" ,rust-gethostname-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libloading" ,rust-libloading-0.7)
                       ("rust-nix" ,rust-nix-0.25)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-winapi-wsapoll" ,rust-winapi-wsapoll-0.1)
                       ("rust-x11rb-protocol" ,rust-x11rb-protocol-0.11))))))

(define-public rust-x11rb-0.10
  (package
    (inherit rust-x11rb-0.13)
    (name "rust-x11rb")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x11rb" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01ympxagdl0qs35k1ww712shpnpbahkcc29j5dqmwd4z461lhasr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gethostname" ,rust-gethostname-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-nix" ,rust-nix-0.24)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winapi-wsapoll" ,rust-winapi-wsapoll-0.1)
        ("rust-x11rb-protocol" ,rust-x11rb-protocol-0.10))))))

(define-public rust-x11rb-0.8
  (package
    (inherit rust-x11rb-0.10)
    (name "rust-x11rb")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "x11rb" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "068g5ll4l5f35c2v098hj0kj2c9ma0r7v3pbli164q9g7w5hiyvg"))))
    (arguments
      `(#:skip-build? #t
        #:cargo-inputs
        (("rust-gethostname" ,rust-gethostname-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libloading" ,rust-libloading-0.7)
         ("rust-nix" ,rust-nix-0.20)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-winapi" ,rust-winapi-0.3)
         ("rust-winapi-wsapoll" ,rust-winapi-wsapoll-0.1))))))

(define-public rust-x11rb-protocol-0.13
  (package
    (name "rust-x11rb-protocol")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11rb-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d3cc2dr5fcx8asgrm31d7lrxpnbqi6kl5v3r71gx7xxp3272gp6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))
    (home-page "https://github.com/psychon/x11rb")
    (synopsis "Rust bindings to X11")
    (description "Rust bindings to X11.")
    (license (list license:expat license:asl2.0))))

(define-public rust-x11rb-protocol-0.12
  (package
    (inherit rust-x11rb-protocol-0.13)
    (name "rust-x11rb-protocol")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11rb-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g24qdvq0mbyl2npz7zqy5v6hpdxq2qakkpnp3x02rzvl3ww7ml2"))))
    (arguments
     `(#:cargo-inputs (("rust-nix" ,rust-nix-0.26)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4))))))

(define-public rust-x11rb-protocol-0.11
  (package
    (inherit rust-x11rb-protocol-0.13)
    (name "rust-x11rb-protocol")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11rb-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mlwsfb4w8dic0hf0qdxix81f7w07z9v2nsdwly0f8qi2hxm3cg0"))))
    (arguments
     `(#:cargo-inputs (("rust-nix" ,rust-nix-0.25)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3))))))

(define-public rust-x11rb-protocol-0.10
  (package
    (inherit rust-x11rb-protocol-0.13)
    (name "rust-x11rb-protocol")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x11rb-protocol" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rxcpsmgrziwqy9ml81h8r1lwy422h1i5j3d007dpj8a3islbcjn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.24)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3))))))

(define-public rust-x11-clipboard-0.8
  (package
    (name "rust-x11-clipboard")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11-clipboard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ps0fk1912vzy382fc8l926q8w1l8bxmw72l3kr9bwdi2l8wl6ml"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-x11rb" ,rust-x11rb-0.12))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             ;; Most tests require an X server.
             (let ((xvfb (search-input-file (or native-inputs inputs)
                                            "bin/Xvfb"))
                   (display ":1"))
               (setenv "DISPLAY" display)
               (system (string-append xvfb " " display " &"))))))))
    (native-inputs (list xorg-server-for-tests))
    (home-page "https://github.com/quininer/x11-clipboard")
    (synopsis "x11 clipboard support for Rust")
    (description "This package provides x11 clipboard support for Rust.")
    (license license:expat)))

(define-public rust-x11-clipboard-0.7
  (package
    (inherit rust-x11-clipboard-0.8)
    (name "rust-x11-clipboard")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x11-clipboard" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0r3lgslbbdf0mb914n0f9q2pqci407r1pcddwbl7sfvc4alrl2wq"))))
    (arguments
     `(#:tests? #f      ; Wants a running X server.
       #:cargo-inputs (("rust-x11rb" ,rust-x11rb-0.10))))))

(define-public rust-x11-dl-2
  (package
    (name "rust-x11-dl")
    (version "2.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11-dl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vsiq62xpcfm0kn9zjw5c9iycvccxl22jya8wnk18lyxzqj5jwrq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/erlepereira/x11-rs.git")
    (synopsis "X11 library bindings for Rust")
    (description "This package provides X11 library bindings for Rust.")
    (license license:expat)))

(define-public rust-xkbcommon-0.7
  (package
    (name "rust-xkbcommon")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xkbcommon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07n9shhcls66wjvmk5pzqql46ipfdv7b8hbc384wgv9hk4jpv1hk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-as-raw-xcb-connection" ,rust-as-raw-xcb-connection-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memmap2" ,rust-memmap2-0.8)
                       ("rust-xkeysym" ,rust-xkeysym-0.2))
       #:cargo-development-inputs (("rust-evdev" ,rust-evdev-0.11))))
    (inputs (list libxkbcommon))
    (home-page "https://github.com/rust-x-bindings/xkbcommon-rs")
    (synopsis "Rust bindings and wrappers for libxkbcommon")
    (description "This package provides a set of bindings and safe wrappers
for @code{libxkbcommon}.")
    (license license:expat)))

(define-public rust-xkbcommon-0.5
  (package
    (inherit rust-xkbcommon-0.7)
    (name "rust-xkbcommon")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xkbcommon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "123b96fbp693z43z2f2gbadb9dzf523n2d21j3izcaz9k6sjbnsj"))))
    (arguments
     `(#:cargo-inputs (("rust-as-raw-xcb-connection" ,rust-as-raw-xcb-connection-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memmap2" ,rust-memmap2-0.7))
       #:cargo-development-inputs (("rust-evdev" ,rust-evdev-0.11))))))

(define-public rust-xkbcommon-dl-0.4
  (package
    (name "rust-xkbcommon-dl")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xkbcommon-dl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16c6kmgqbffdsnw5w9q97p55d824ss3fqzif2lrh33648j2nc939"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-dlib" ,rust-dlib-0.5)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-xkeysym" ,rust-xkeysym-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'add-absolute-library-references
           (lambda* (#:key inputs vendor-dir #:allow-other-keys)
             (substitute* (find-files vendor-dir "\\.rs$")
               (("libxkbcommon-x11\\.so")
                (search-input-file inputs "lib/libxkbcommon-x11.so"))
               (("libxkbcommon\\.so")
                (search-input-file inputs "lib/libxkbcommon.so"))))))))
    (inputs
     (list libxkbcommon))
    (home-page "https://github.com/rust-windowing/xkbcommon-dl")
    (synopsis "Dynamically loaded xkbcommon and xkbcommon-x11 Rust bindings")
    (description
     "Dynamically loaded xkbcommon and xkbcommon-x11 Rust bindings.")
    (license license:expat)))

(define-public rust-xkeysym-0.2
  (package
    (name "rust-xkeysym")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xkeysym" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0886dn1rlkiazcp5n6ayqfg0ibpiny62dlbiyr9v4l32nxl8wjh5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bytemuck" ,rust-bytemuck-1)
                                   ("rust-x11rb" ,rust-x11rb-0.11))))
    (home-page "https://github.com/notgull/xkeysym")
    (synopsis "Rust library for working with X11 keysyms")
    (description
     "This package provides a library for working with X11 keysyms.")
    (license (list license:expat license:asl2.0 license:zlib))))

(define-public rust-y4m-0.8
  (package
    (name "rust-y4m")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "y4m" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j24y2zf60lpxwd7kyg737hqfyqx16y32s0fjyi6fax6w4hlnnks"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-resize" ,rust-resize-0.4))))
    (home-page "https://github.com/image-rs/y4m")
    (synopsis "YUV4MPEG2 (@file{.y4m}) encoder and decoder")
    (description
     "This package provides a YUV4MPEG2 (@file{.y4m}) encoder and decoder.")
    (license license:expat)))

(define-public rust-y4m-0.7
  (package
    (inherit rust-y4m-0.8)
    (name "rust-y4m")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "y4m" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bhdgb7hgx7j92nm6ij5n8wisp50j8ff66ks14jzwdw2mwhrjam7"))))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
