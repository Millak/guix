;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (gnu packages kanata)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows))

(define-public rust-kanata-interception-0.3
  (package
    (name "rust-kanata-interception")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-interception" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01mn1dskhm124x0nxfcw5cyb07h0i256x9bfj23aq6adjsdpprg2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-interception-sys" ,rust-interception-sys-0.1)
                       ("rust-num-enum" ,rust-num-enum-0.6)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/jtroo/kanata")
    (synopsis "Safe wrapper for Interception, forked for use with Kanata")
    (description
     "This package provides a safe wrapper for Interception, forked for use
with Kanata.")
    (license (list license:expat license:asl2.0))))

(define-public rust-kanata-keyberon-0.180
  (package
    (name "rust-kanata-keyberon")
    (version "0.180.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-keyberon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0iaz1nxsf523ajm8rm3im9q08jkswmv2b83cq6vfgy5mdzhi8zj3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-arraydeque" ,rust-arraydeque-0.5)
        ("rust-heapless" ,rust-heapless-0.7)
        ("rust-kanata-keyberon-macros" ,rust-kanata-keyberon-macros-0.2)
        ("rust-rustc-hash" ,rust-rustc-hash-1))))
    (home-page "https://github.com/TeXitoi/keyberon")
    (synopsis "Pure Rust keyboard firmware, forked for use with Kanata")
    (description
     "This package provides Pure Rust keyboard firmware, forked for use with
Kanata.")
    (license license:expat)))

(define-public rust-kanata-keyberon-macros-0.2
  (package
    (name "rust-kanata-keyberon-macros")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-keyberon-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lj7ldiazmszh0k01h7mjzhjg59bdakvx2pnpc9mq2ir0czzixkk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1))))
    (home-page "")
    (synopsis "Macros for Keyberon, forked for Kanata project")
    (description
     "This package provides macros for Keyberon, forked for Kanata project.")
    (license license:expat)))

(define-public rust-kanata-parser-0.180
  (package
    (name "rust-kanata-parser")
    (version "0.180.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02vwi5xj38yympavc7mygxiyky3153xnmwnpw4mdwvjwfsppxp1c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-kanata-keyberon" ,rust-kanata-keyberon-0.180)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-miette" ,rust-miette-5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-patricia-tree" ,rust-patricia-tree-0.8)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/jtroo/kanata")
    (synopsis "Parser for configuration language of Kanata")
    (description
     "This package provides a parser for configuration language of Kanata,
a keyboard remapper.")
    (license license:lgpl3)))

(define-public rust-kanata-tcp-protocol-0.180
  (package
    (name "rust-kanata-tcp-protocol")
    (version "0.180.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-tcp-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nlilmw5x73xp1say99x1avrfkcski5fi9blnh3qilbxbv4350y7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/jtroo/kanata")
    (synopsis "TCP protocol for Kanata")
    (description "This package provides TCP protocol for Kanata.")
    (license license:lgpl3)))

(define-public rust-karabiner-driverkit-0.1
  (package
    (name "rust-karabiner-driverkit")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "karabiner-driverkit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pqnh9n3a8wxqzdj7d30f99g322da8zpnixsq5gfs9n1klccj380"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-os-info" ,rust-os-info-3))))
    (home-page "https://github.com/Psych3r/driverkit")
    (synopsis "Karabiner-DriverKit-VirtualHIDDevice wrapper for Kanata")
    (description
     "This package provides a minimal Karabiner-DriverKit-VirtualHIDDevice
wrapper for Kanata.")
    (license license:lgpl3)))

(define-public kanata
  (package
    (name "kanata")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1b37kx0g9l1j3jbg59vcp3fv9c57ykgww9vigmc9gmf2bisrkgzh"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-inputs
      `(("rust-anyhow" ,rust-anyhow-1)
        ("rust-arboard" ,rust-arboard-3)
        ("rust-clap" ,rust-clap-4)
        ("rust-core-graphics" ,rust-core-graphics-0.24)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-embed-resource" ,rust-embed-resource-2)
        ("rust-encode-unicode" ,rust-encode-unicode-0.3)
        ("rust-evdev" ,rust-evdev-0.12)
        ("rust-indoc" ,rust-indoc-2)
        ("rust-inotify" ,rust-inotify-0.10)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-kanata-interception" ,rust-kanata-interception-0.3)
        ("rust-kanata-keyberon" ,rust-kanata-keyberon-0.180)
        ("rust-kanata-parser" ,rust-kanata-parser-0.180)
        ("rust-kanata-tcp-protocol" ,rust-kanata-tcp-protocol-0.180)
        ("rust-karabiner-driverkit" ,rust-karabiner-driverkit-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-miette" ,rust-miette-5)
        ("rust-mio" ,rust-mio-0.8)
        ("rust-muldiv" ,rust-muldiv-1)
        ("rust-native-windows-gui" ,rust-native-windows-gui-1)
        ("rust-nix" ,rust-nix-0.26)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-open" ,rust-open-5)
        ("rust-os-pipe" ,rust-os-pipe-1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-radix-trie" ,rust-radix-trie-0.2)
        ("rust-regex" ,rust-regex-1)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-sd-notify" ,rust-sd-notify-0.4)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-signal-hook" ,rust-signal-hook-0.3)
        ("rust-simplelog" ,rust-simplelog-0.12)
        ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
        ("rust-time" ,rust-time-0.3)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/jtroo/kanata")
    (synopsis "Multi-layer keyboard customization")
    (description
     "Kanata is a keyboard re-mapper.  It supports multiple layers of key,
and advanced key behavior customization, such as tap-hold, macros and
Unicode.")
    (license license:lgpl3)))
