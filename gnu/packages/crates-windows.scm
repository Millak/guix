;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 Daniel Ziltener <dziltener@lyrion.ch>
;;; Copyright © 2024 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2024 Jordan Moore <lockbox@struct.foo>
;;; Copyright © 2024 Murilo <murilo@disroot.org>
;;; Copyright © 2024 normally_js <normally_js@posteo.net>
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

(define-module (gnu packages crates-windows)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io))

(define-public rust-anstyle-wincon-3
  (package
    (name "rust-anstyle-wincon")
    (version "3.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle-wincon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "099ir0w3lbpsp1nxdzbf4anq98ww8ykyc9pd1g03xgkj1v7dn291"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstyle" ,rust-anstyle-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))
       #:cargo-development-inputs (("rust-lexopt" ,rust-lexopt-0.3))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Styling legacy Windows terminals")
    (description "Styling legacy Windows terminals.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-wincon-1
  (package
    (inherit rust-anstyle-wincon-3)
    (name "rust-anstyle-wincon")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "anstyle-wincon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k6gcx8qih4gzb5glrl37rqvj5qc893cvkkssdnjjm4iw02snxy6"))))
    (arguments
     `(#:cargo-inputs (("rust-anstyle" ,rust-anstyle-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-lexopt" ,rust-lexopt-0.3))))))

(define-public rust-anstyle-wincon-0.2
  (package
    (inherit rust-anstyle-wincon-3)
    (name "rust-anstyle-wincon")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle-wincon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yil703c9gp5hn1d8fn5m3dr4mlwml80v6mvhwr9y52v2kv7l4n3"))))
    (arguments
     `(#:cargo-inputs (("rust-anstyle" ,rust-anstyle-0.3)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))
       #:cargo-development-inputs (("rust-lexopt" ,rust-lexopt-0.3))))))

(define-public rust-cargo-credential-wincred-0.4
  (package
    (name "rust-cargo-credential-wincred")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-credential-wincred" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1amc3s0ym8y6pipiis3cc84m2bd3nazx93b7m77waazrr9disx4p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cargo-credential" ,rust-cargo-credential-0.4)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/rust-lang/cargo")
    (synopsis
     "Cargo credential process that stores tokens with Windows Credential Manager")
    (description
     "This package provides a Cargo credential process that stores tokens with
Windows Credential Manager.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cargo-credential-wincred-0.3
  (package
    (inherit rust-cargo-credential-wincred-0.4)
    (name "rust-cargo-credential-wincred")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-credential-wincred" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w8ciyyrq0vp25bdpsmj8221xh09x4np80wrhc53br8gkldljdv6"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cargo-credential" ,rust-cargo-credential-0.3)
        ("rust-windows-sys" ,rust-windows-sys-0.48))))))

(define-public rust-cfb-0.7
  (package
    (name "rust-cfb")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cfb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03y6p3dlm7gfds19bq4ba971za16rjbn7q2v0vqcri52l2kjv3yk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-2)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-pcg" ,rust-rand-pcg-0.3)
                                   ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/mdsteele/rust-cfb")
    (synopsis "Read/write Compound File Binary files")
    (description
     "This package provides Read/write functionality for Compound File Binary
(structured storage) files.")
    (license license:expat)))

(define-public rust-clipboard-win-5
  (package
    (name "rust-clipboard-win")
    (version "5.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clipboard-win" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14n87fc0vzbd0wdhqzvcs1lqgafsncplzcanhpik93xhhalfgvqm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; unresolved import `clipboard_win::raw`
       #:cargo-inputs (("rust-error-code" ,rust-error-code-3)
                       ("rust-windows-win" ,rust-windows-win-3))))
    (home-page "https://github.com/DoumanAsh/clipboard-win")
    (synopsis "Simple way to interact with Windows clipboard")
    (description
     "This package provides simple way to interact with Windows clipboard.")
    (license license:boost1.0)))

(define-public rust-clipboard-win-4
  (package
    (inherit rust-clipboard-win-5)
    (name "rust-clipboard-win")
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clipboard-win" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qh3rypkf1lazniq4nr04hxsck0d55rigb5sjvpvgnap4dyc54bi"))))
    (arguments
     `(#:tests? #f  ; unresolved import `clipboard_win::raw`
       #:cargo-inputs
       (("rust-error-code" ,rust-error-code-2)
        ("rust-str-buf" ,rust-str-buf-1)
        ("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-clipboard-win-3
  (package
    (inherit rust-clipboard-win-4)
    (name "rust-clipboard-win")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clipboard-win" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hh3npqfa1lfn62fwvkmjlpfnizq343a994b898ffsvb100mxpwz"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-lazy-bytes-cast" ,rust-lazy-bytes-cast-5)
        ("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-codepage-0.1
  (package
    (name "rust-codepage")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "codepage" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d0qr4wqc4yrab7halsa3r6akb2i2bk2cqr04vl8m0n23c38vxj8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-encoding-rs" ,rust-encoding-rs-0.8))))
    (home-page "https://docs.rs/codepage/")
    (synopsis "Mapping between Windows code page and @code{encoding_rs}")
    (description
     "This package provides mapping between Windows code page numbers and
@code{encoding_rs} character encodings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-conpty-0.5
  (package
    (name "rust-conpty")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "conpty" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nmkhiysnssvbi4kqaq8cybb0ffngbl64kfpk8s86ihdg940caxp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Specifically for Windows.
       #:cargo-inputs
       (("rust-windows" ,rust-windows-0.44))
       #:cargo-development-inputs
       (("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.1))))
    (home-page "https://github.com/zhiburt/conpty")
    (synopsis "Library which provides an interface for ConPTY")
    (description
     "This package provides a library which provides an interface for @code{ConPTY}.")
    (license license:expat)))

(define-public rust-deelevate-0.2
  (package
    (name "rust-deelevate")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "deelevate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kj2kcqv47l3iacpamgzbn742yf9d09h0xgwbadxs1l9qkw9fwqw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ;; Depends on Windows
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-pathsearch" ,rust-pathsearch-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-shared-library" ,rust-shared-library-0.1)
                       ("rust-termwiz" ,rust-termwiz-0.15)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://crates.io/crates/deelevate")
    (synopsis "Drop privileges on Windows")
    (description "This package provides Drop privileges on Windows.")
    (license license:expat)))

(define-public rust-dunce-1
  (package
    (name "rust-dunce")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dunce" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04y8wwv3vvcqaqmqzssi6k0ii9gs6fpz96j5w9nky2ccsl23axwj"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.com/kornelski/dunce")
    (synopsis "Normalize Windows paths to the most compatible format")
    (description
     "This crate converts Windows UNC paths to the MS-DOS-compatible format
whenever possible, but leaves UNC paths as-is when they can't be unambiguously
expressed in a simpler way.  This allows legacy programs to access all paths
they can possibly access, and doesn't break any paths for UNC-aware
programs.")
    (license (list license:cc0 license:expat-0 license:asl2.0))))

(define-public rust-embed-resource-2
  (package
    (name "rust-embed-resource")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "embed-resource" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yb3kbw3xpghiwf69769jpng725kwa2cxm27qj5s7dm0cfgnz2xn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-vswhom" ,rust-vswhom-0.1)
                       ("rust-winreg" ,rust-winreg-0.52))))
    (home-page "https://github.com/nabijaczleweli/rust-embed-resource")
    (synopsis
     "Cargo library to handle compilation and inclusion of Windows resources")
    (description
     "This package provides a Cargo library to handle compilation and
inclusion of Windows resources in the most resilient fashion imaginable.")
    (license license:expat)))

(define-public rust-embed-resource-1
  (package
    (inherit rust-embed-resource-2)
    (name "rust-embed-resource")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "embed-resource" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0piayd947h4dswbpwqs02zq42y4kfzxcl52wmr7pbr07dj3vnap6"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-rustc-version" ,rust-rustc-version-0.4)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-vswhom" ,rust-vswhom-0.1)
        ("rust-winreg" ,rust-winreg-0.10))))))

(define-public rust-fs-at-0.2
  (package
    (name "rust-fs-at")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fs_at" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dn0hi9inmppk3mypvnaimjcdrxr0f3pi8d2p8jxn9gajjb6rbql"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aligned" ,rust-aligned-0.4)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cvt" ,rust-cvt-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.11)
                                   ("rust-fs-set-times" ,rust-fs-set-times-0.20)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-test-log" ,rust-test-log-0.2))))
    (home-page "https://github.com/rbtcollins/fs_at.git")
    (synopsis "Implementation of 'at' functions for various platforms")
    (description
     "This package provides an implementation of at functions for various platforms.")
    (license license:asl2.0)))

(define-public rust-fs-set-times-0.20
  (package
    (name "rust-fs-set-times")
    (version "0.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fs-set-times" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d4ww0f4143nda9cq0s9fl3986c0f09njs663k2z5w16mwin2bjy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-io-lifetimes" ,rust-io-lifetimes-2)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/bytecodealliance/fs-set-times")
    (synopsis "Set filesystem timestamps")
    (description "This package provides set filesystem timestamps.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-gdi32-sys-0.2
  (package
    (name "rust-gdi32-sys")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdi32-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0605d4ngjsspghwjv4jicajich1gnl0aik9f880ajjzjixd524h9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.2)
        ("rust-winapi-build" ,rust-winapi-build-0.1))))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Function definitions for the Windows API library gdi32")
    (description "This package contains function definitions for the Windows
API library @code{gdi32}.")
    (license license:expat)))

(define-public rust-implib-0.3
  (package
    (name "rust-implib")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "implib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n1bcfcsnng54ssf4hjknx87amz61j0kdis94f9kk7gnva07v9r7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-memoffset" ,rust-memoffset-0.9)
                       ("rust-object" ,rust-object-0.36))))
    (home-page "https://github.com/messense/implib-rs")
    (synopsis "Generate Windows import library from module definition file")
    (description
     "This package provides a way to generate a Windows import library from a
module definition file.")
    (license license:expat)))

(define-public rust-ipconfig-0.3
  (package
    (name "rust-ipconfig")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ipconfig" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zwr0x3jnqmjdqqbzhb0nid011qyhcyfdfqv32cdw85pjqpvk3dm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; failed to resolve: could not find `computer` in `ipconfig`
       #:cargo-inputs (("rust-socket2" ,rust-socket2-0.5)
                       ("rust-widestring" ,rust-widestring-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48)
                       ("rust-winreg" ,rust-winreg-0.50))))
    (home-page "https://github.com/liranringel/ipconfig")
    (synopsis "Get network adapters and configuration information for Windows")
    (description "This package lets you get network adapters information and
network configuration for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ipconfig-0.2
  (package
    (inherit rust-ipconfig-0.3)
    (name "rust-ipconfig")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ipconfig" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mzsagc6bk3i3fpggqlq8am5rxn4hgs297rsaya90w79xj5g3qpp"))))
    (arguments
     `(#:tests? #f  ; failed to resolve: could not find `computer` in `ipconfig`
       #:cargo-inputs
       (("rust-socket2" ,rust-socket2-0.3)
        ("rust-widestring" ,rust-widestring-0.4)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winreg" ,rust-winreg-0.6))))))

(define-public rust-kernel32-sys-0.2
  (package
    (name "rust-kernel32-sys")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "kernel32-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1389av0601a9yz8dvx5zha9vmkd6ik7ax0idpb032d28555n41vm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.2)
        ("rust-winapi-build" ,rust-winapi-build-0.1))))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Function definitions for the Windows API library kernel32")
    (description "Contains function definitions for the Windows API library
kernel32.")
    (license license:expat)))

(define-public rust-known-folders-1
  (package
    (name "rust-known-folders")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "known-folders" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z4jlxfqv1jqp9g5m1dr23zpjpl5kpbqgdqfk8jnxd681isa3ndp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/artichoke/known-folders-rs")
    (synopsis "Safe wrapper around the Known Folders API on Windows")
    (description
     "This package provides a safe wrapper around the Known Folders API on Windows.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-miow-0.6
  (package
    (name "rust-miow")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i307jyhxnhgzj148cdb9zq59rhlhr1b65g142g9z9r01d1pd7rm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.8)
        ("rust-socket2" ,rust-socket2-0.5))))
    (home-page "https://github.com/yoshuawuyts/miow")
    (synopsis "Rust I/O library for Windows")
    (description
     "This package provides a zero overhead I/O library for Windows, focusing on
IOCP and Async I/O abstractions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-miow-0.5
  (package
    (inherit rust-miow-0.6)
    (name "rust-miow")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08qi8xm2zf8dqacdbnrp19aqk2xiwmw75n1mpq43rqsmysibrzsj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-windows-sys" ,rust-windows-sys-0.42))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.8)
        ("rust-socket2" ,rust-socket2-0.4))))))

(define-public rust-miow-0.4
  (package
    (inherit rust-miow-0.5)
    (name "rust-miow")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03p0dgbahlysgxk0sihhf09k5h13r9aam3d6rfivdbxkj9vpydx7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-windows-sys" ,rust-windows-sys-0.28))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.8)
        ("rust-socket2" ,rust-socket2-0.4))))))

(define-public rust-miow-0.3
  (package
    (inherit rust-miow-0.4)
    (name "rust-miow")
    (version "0.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08afp2xfpxmdw003111lxz6g9jgbj4zi2fpldvv7da6d4nqcbwdr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.8)
        ("rust-socket2" ,rust-socket2-0.4))))))

(define-public rust-miow-0.2
  (package
    (inherit rust-miow-0.3)
    (name "rust-miow")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miow" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0kcl8rnv0bhiarcdakik670w8fnxzlxhi1ys7152sck68510in7b"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-winapi" ,rust-winapi-0.2)
        ("rust-ws2-32-sys" ,rust-ws2-32-sys-0.2))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.3))))))

(define-public rust-named-pipe-0.4
  (package
    (name "rust-named-pipe")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "named_pipe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0azby10wzmsrf66m1bysbil0sjfybnvhsa8py093xz4irqy4975d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; failed to resolve: could not find `shared` in `winapi`
       #:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/blackbeam/named_pipe")
    (synopsis "Wrapper for overlapped (asynchronous) IO of Windows's named pipes")
    (description
     "This package provides a wrapper for overlapped (asynchronous) IO of
Windows's named pipes.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nt-time-0.6
  (package
    (name "rust-nt-time")
    (version "0.6.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nt-time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02sikab4va5gv2ja5rjd7fp1g1dh6l1pryjlsprxh0hcq247fmkf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Not all files included.
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zip" ,rust-zip-2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-clap" ,rust-clap-4)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/sorairolake/nt-time")
    (synopsis "Windows file time library")
    (description "This package provides a Windows file time library.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-output-vt100-0.1
  (package
    (name "rust-output-vt100")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "output_vt100" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0rpvpiq7gkyvvwyp9sk0zxhbk99ldlrv5q3ycr03wkmbxgx270k2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Phundrak/output-vt100-rs")
    (synopsis "Utility to activate escape codes in Windows' CMD and PowerShell")
    (description
     "Utility to activate escape codes in Windows' CMD and PowerShell.")
    (license license:expat)))

(define-public rust-python3-dll-a-0.2
  (package
    (name "rust-python3-dll-a")
    (version "0.2.12")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "python3-dll-a" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a57r12py9zy7hw48j63wl4a3k5k3ghmcd5d9gk79rjh34bzjrlv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;Windows-like targets only
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/pyo3/python3-dll-a")
    (synopsis "Python import library generator")
    (description
     "This crate generates import libraries for the Python shared library
for MinGW-w64 and MSVC (cross-)compile targets.")
    (license license:expat)))

(define-public rust-remove-dir-all-1
  (package
    (name "rust-remove-dir-all")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "remove_dir_all" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "026xl6wlkjxksm1n3dcccygssami56aa937h6vgnmxxcfnsc1340"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-cvt" ,rust-cvt-0.1)
        ("rust-env-logger" ,rust-env-logger-0.11)
        ("rust-fs-at" ,rust-fs-at-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-normpath" ,rust-normpath-1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-windows-sys" ,rust-windows-sys-0.59))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-env-logger" ,rust-env-logger-0.11)
        ("rust-log" ,rust-log-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-test-log" ,rust-test-log-0.2))))
    (home-page "https://github.com/XAMPPRocky/remove_dir_all.git")
    (synopsis "Implementation of remove_dir_all for Windows")
    (description
     "This package provides a safe, reliable implementation of
@code{remove_dir_all} for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-remove-dir-all-0.5
  (package
    (inherit rust-remove-dir-all-1)
    (name "rust-remove-dir-all")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "remove_dir_all" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1rzqbsgkmr053bxxl04vmvsd1njyz0nxvly97aip6aa2cmb15k9s"))))
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3))))))

(define-public rust-schannel-0.1
  (package
    (name "rust-schannel")
    (version "0.1.27")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "schannel" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-sys" ,rust-windows-sys-0.59))
       #:cargo-development-inputs (("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/steffengy/schannel-rs")
    (synopsis "Rust bindings to the Windows SChannel APIs")
    (description
     "Rust bindings to the Windows SChannel APIs providing TLS client and
server functionality.")
    (license license:expat)))

(define-public rust-serde-ini-0.2
  (package
    (name "rust-serde-ini")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_ini" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f8ir1bbcdyad50aj1c53dkiwr24x6dr88f045skl1xvwa3nc8zb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-result" ,rust-result-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-void" ,rust-void-1))))
    (home-page "https://github.com/arcnmx/serde-ini")
    (synopsis "Windows INI file {de,}serialization")
    (description
     "@code{serde_ini} provides a serde @code{Serializer} and
@code{Deserializer} for the INI format.")
    (license license:expat)))

(define-public rust-tauri-winrt-notification-0.2
  (package
    (name "rust-tauri-winrt-notification")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tauri-winrt-notification" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wb8d0fdzvgs4kmnhc2znx00n5cnh40ajawvblgkhqkg1nvmz7zq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ;Requires Windows
       #:cargo-inputs (("rust-quick-xml" ,rust-quick-xml-0.31)
                       ("rust-windows" ,rust-windows-0.56)
                       ("rust-windows-version" ,rust-windows-version-0.1))))
    (home-page "https://github.com/tauri-apps/winrt-notification")
    (synopsis "Incomplete wrapper over the WinRT toast api")
    (description
     "This package provides An incomplete wrapper over the @code{WinRT} toast api.")
    (license (list license:expat license:asl2.0))))

(define-public rust-uds-windows-1
  (package
    (name "rust-uds-windows")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "uds_windows" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memoffset" ,rust-memoffset-0.9)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/haraldh/rust_uds_windows")
    (synopsis "Unix Domain Sockets for Windows")
    (description "This library integrates Unix Domain Sockets on Windows.")
    (license license:expat)))

(define-public rust-user32-sys-0.2
  (package
    (name "rust-user32-sys")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "user32-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ivxc7hmsxax9crdhxdd1nqwik4s9lhb2x59lc8b88bv20fp3x2f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi-0.2))
       #:cargo-development-inputs (("rust-winapi-build" ,rust-winapi-build-0.1))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-cargo-toml
                    (lambda _
                      (substitute* "Cargo.toml"
                        ((", path =.*}")
                         "}")) #t)))))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Function definitions for the Windows API library user32")
    (description
     "Contains function definitions for the Windows API library user32.
See winapi for types and constants.")
    (license license:expat)))

(define-public rust-win-crypto-ng-0.5
  (package
    (name "rust-win-crypto-ng")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "win-crypto-ng" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14mv2wsvji8x7ds72zsjpz6hdq57y4r8r38xjyr4mrbib91zpawr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Windows library
       #:cargo-inputs
       (("rust-cipher" ,rust-cipher-0.4)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/emgre/win-crypto-ng")
    (synopsis "Safe bindings to Windows Cryptography API: Next Generation")
    (description "Safe bindings to Windows Cryptography API: Next Generation")
    (license license:bsd-3)))

(define-public rust-win32job-2
  (package
    (name "rust-win32job")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "win32job" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g1blsb7ixrqjicykx82rvrymcydlsdgfwzb61x88iyrazsinasv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rusty-fork" ,rust-rusty-fork-0.3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-windows" ,rust-windows-0.52))))
    (home-page "https://github.com/ohadravid/win32job-rs")
    (synopsis "Safe API for Windows job objects")
    (description
     "This package provides a safe API for Windows job objects.")
    (license (list license:expat license:asl2.0))))

(define-public rust-winapi-0.3
  (package
    (name "rust-winapi")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))))
    (build-system cargo-build-system)
    ;; This package depends unconditionally on these two crates.
    (arguments
     (list #:cargo-inputs
           (list rust-winapi-i686-pc-windows-gnu-0.4
                 rust-winapi-x86-64-pc-windows-gnu-0.4)))
    (inputs
     (list rust-winapi-i686-pc-windows-gnu-0.4
           rust-winapi-x86-64-pc-windows-gnu-0.4))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Raw FFI bindings for all of Windows API")
    (description
     "Raw FFI bindings for all of Windows API.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-0.2
  (package
    (inherit rust-winapi-0.3)
    (name "rust-winapi")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0yh816lh6lf56dpsgxy189c2ai1z3j8mw9si6izqb6wsjkbcjz8n"))))
    (arguments '(#:skip-build? #t))))

(define-public rust-winapi-build-0.1
  (package
    (name "rust-winapi-build")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-build" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1g4rqsgjky0a7530qajn2bbfcrl2v0zb39idgdws9b1l7gp5wc9d"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Common code for build.rs in WinAPI -sys crates")
    (description
     "Common code for build.rs in WinAPI -sys crates.")
    (license license:expat)))

(define-public rust-winapi-i686-pc-windows-gnu-0.4
  (package
    (name "rust-winapi-i686-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-i686-pc-windows-gnu" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (for-each delete-file (find-files "." "\\.a$"))))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Import libraries for the i686-pc-windows-gnu target")
    (description "This crate provides import libraries for the
i686-pc-windows-gnu target.  Please don't use this crate directly, depend on
@code{winapi} instead.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-util-0.1
  (package
    (name "rust-winapi-util")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/BurntSushi/winapi-util")
    (synopsis "Dumping ground for high level safe wrappers over winapi")
    (description
     "This package provides a dumping ground for high level safe wrappers over
winapi.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-winapi-wsapoll-0.1
  (package
    (name "rust-winapi-wsapoll")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winapi-wsapoll" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a1zxmpvxaw75y4lwavi6qbq95cnrz83a5p84rarjxn5g7vcbbqy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/psychon/winapi-wsapoll")
    (synopsis "Safe wrapper around WSAPoll")
    (description "This package provides safe wrapper around WSAPoll.")
    (license (list license:expat license:asl2.0))))

(define-public rust-winapi-x86-64-pc-windows-gnu-0.4
  (package
    (name "rust-winapi-x86-64-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-x86_64-pc-windows-gnu" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (for-each delete-file (find-files "." "\\.a$"))))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Import libraries for the x86_64-pc-windows-gnu target")
    (description "This package provides import libraries for the
x86_64-pc-windows-gnu target.  Please don't use this crate directly, depend on
@code{winapi} instead.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wincolor-1
  (package
    (name "rust-wincolor")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wincolor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "017x33ljndwc76cp5z9llgndn0nh7v8jcjaykbizkawmwy9n3pyp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3)
        ("rust-winapi-util" ,rust-winapi-util-0.1))))
    (home-page "https://github.com/BurntSushi/termcolor/tree/master/wincolor")
    (synopsis "Windows API for controlling text color in a Windows console")
    (description
     "This package provides a simple Windows specific API for controlling text
color in a Windows console.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-wincolor-0.1
  (package
    (inherit rust-wincolor-1)
    (name "rust-wincolor")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wincolor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rvpvv26a8c4dla5i5hsxlkvjcjjbl0dylhhg4147m54lfcn9c7f"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-windows-collections-0.2
  (package
    (name "rust-windows-collections")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-collections" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s65anr609qvsjga7w971p6iq964h87670dkfqfypnfgwnswxviv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-core" ,rust-windows-core-0.61))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows collection types")
    (description "This package provides Windows collection types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-0.61
  (package
    (name "rust-windows")
    (version "0.61.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06d4ahj0lns53cgza2w73r82fqwabyxqp1npp81cnf2p08yqzvn5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-collections" ,rust-windows-collections-0.2)
                       ("rust-windows-core" ,rust-windows-core-0.61)
                       ("rust-windows-future" ,rust-windows-future-0.2)
                       ("rust-windows-link" ,rust-windows-link-0.1)
                       ("rust-windows-numerics" ,rust-windows-numerics-0.2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-0.58
  (package
    (name "rust-windows")
    (version "0.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dkjj94b0gn91nn1n22cvm4afsj98f5qrhcl3112v6f4jcfx816x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-core" ,rust-windows-core-0.58)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "The windows crate lets you call any Windows API past,
present, and future using code generated on the fly directly from the metadata
describing the API and right into your Rust package where you can call them as
if they were just another Rust module.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-0.57
  (package
    (inherit rust-windows-0.58)
    (name "rust-windows")
    (version "0.57.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hqid10bqvxa3pbpgvrh2cilf950lxsd9zqfv3rldc73v2s2qd0j"))))
    (arguments
     `(#:cargo-inputs (("rust-windows-core" ,rust-windows-core-0.57)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))))

(define-public rust-windows-0.56
  (package
    (inherit rust-windows-0.57)
    (name "rust-windows")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cp10nzrqgrlk91dpwxjcpzyy6imr5vxr5f898pss7nz3gq9vrhx"))))
    (arguments
     `(#:cargo-inputs (("rust-windows-core" ,rust-windows-core-0.56)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))))

(define-public rust-windows-0.52
  (package
    (inherit rust-windows-0.58)
    (name "rust-windows")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gnh210qjlprpd1szaq04rjm1zqgdm9j7l9absg0kawi2rwm72p4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-core" ,rust-windows-core-0.52)
                       ("rust-windows-implement" ,rust-windows-implement-0.52)
                       ("rust-windows-interface" ,rust-windows-interface-0.52)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))))

(define-public rust-windows-0.48
  (package
    (inherit rust-windows-0.52)
    (name "rust-windows")
    (version "0.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03vh89ilnxdxdh0n9np4ns4m10fvm93h3b0cc05ipg3qq1mqi1p6"))))
    (arguments
     `(#:cargo-inputs
       (("rust-windows-implement" ,rust-windows-implement-0.48)
        ("rust-windows-interface" ,rust-windows-interface-0.48)
        ("rust-windows-targets" ,rust-windows-targets-0.48))))))

(define-public rust-windows-0.46
  (package
    (inherit rust-windows-0.48)
    (name "rust-windows")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "098crdz9gxgy3614ygznqqhn28q097r498b3rg35584nd8gb9b6d"))))
    (arguments
     `(#:cargo-inputs
       (("rust-windows-implement" ,rust-windows-implement-0.46)
        ("rust-windows-interface" ,rust-windows-interface-0.46)
        ("rust-windows-targets" ,rust-windows-targets-0.42))))))

(define-public rust-windows-0.44
  (package
    (inherit rust-windows-0.48)
    (name "rust-windows")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ax1ip82dhszxz4hhsrdj3b0681xw6axahnfldxcgi506nmmsx4y"))))
    (arguments
     `(#:tests? #f      ; Test suite only runs on Windows.
       #:cargo-inputs
       (("rust-windows-implement" ,rust-windows-implement-0.44)
        ("rust-windows-interface" ,rust-windows-interface-0.44)
        ("rust-windows-targets" ,rust-windows-targets-0.42))))))

(define-public rust-windows-0.43
  (package
    (inherit rust-windows-0.44)
    (name "rust-windows")
    (version "0.43.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i32alvc4n4l7abmv1fsvnd1lzw17f1cpr16kgx0sqz5wg82wrh4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-implement" ,rust-windows-implement-0.43)
                       ("rust-windows-interface" ,rust-windows-interface-0.43)
                       ("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.42)
                       ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.42)
                       ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.42)
                       ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.42)
                       ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.42)
                       ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.42)
                       ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.42))))))

(define-public rust-windows-0.39
  (package
    (inherit rust-windows-0.46)
    (name "rust-windows")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jkr4rxj6jn7brqxycr1pjgvnixsimsiywl4a7v20q5ca05bvi7i"))))
    (arguments
     `(#:cargo-inputs (("rust-windows-implement" ,rust-windows-implement-0.39)
                       ("rust-windows-interface" ,rust-windows-interface-0.39)
                       ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.39)
                       ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.39)
                       ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.39)
                       ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.39)
                       ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.39))))))

(define-public rust-windows-0.9
  (package
    (inherit rust-windows-0.46)
    (name "rust-windows")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zy9jmhkhmsng7l9qiznxpdh5ns303s875p5kf6a5q9ym0rka7rn"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-const-sha1" ,rust-const-sha1-0.2)
        ("rust-windows-gen" ,rust-windows-gen-0.9)
        ("rust-windows-macros" ,rust-windows-macros-0.9))))))

(define-public rust-windows-aarch64-gnullvm-0.52
  (package
    (name "rust-windows-aarch64-gnullvm")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))
       (snippet
        '(delete-file "lib/libwindows.0.52.0.a"))))
    (build-system cargo-build-system)
    (arguments (list #:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description
     "This package provides code gen support for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-gnullvm-0.48
  (package
    (inherit rust-windows-aarch64-gnullvm-0.52)
    (name "rust-windows-aarch64-gnullvm")
    (version "0.48.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))
              (snippet
               '(delete-file "lib/libwindows.0.48.5.a"))))
    (arguments (list #:skip-build? #t))))

(define-public rust-windows-aarch64-gnullvm-0.42
  (package
    (inherit rust-windows-aarch64-gnullvm-0.48)
    (name "rust-windows-aarch64-gnullvm")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))
              (snippet
               '(delete-file "lib/libwindows.a"))))
    (arguments
     (list #:skip-build? #t))))

(define-public rust-windows-aarch64-msvc-0.52
  (package
    (name "rust-windows-aarch64-msvc")
    (version "0.52.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))
              (snippet
               '(delete-file "lib/windows.0.52.0.lib"))))
    (build-system cargo-build-system)
    (arguments (list #:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "This package provides code gen support for the windows
crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-msvc-0.48
  (package
    (inherit rust-windows-aarch64-msvc-0.52)
    (name "rust-windows-aarch64-msvc")
    (version "0.48.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))
              (snippet
               #~(delete-file "lib/windows.0.48.5.lib"))))
    (arguments (list #:skip-build? #t))))

(define-public rust-windows-aarch64-msvc-0.42
  (package
    (inherit rust-windows-aarch64-msvc-0.48)
    (name "rust-windows-aarch64-msvc")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))
              (snippet
               #~(delete-file "lib/windows.lib"))))
    (arguments
     (list #:skip-build? #t))))

(define-public rust-windows-aarch64-msvc-0.39
  (package
    (inherit rust-windows-aarch64-msvc-0.42)
    (name "rust-windows-aarch64-msvc")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wj0nfmyli4bn5af243r4s3zncxv0n4j6dd8zyb41gcnc1k12xzc"))
       (snippet
        #~(delete-file "lib/windows.lib"))))))

(define-public rust-windows-aarch64-msvc-0.36
  (package
    (inherit rust-windows-aarch64-msvc-0.42)
    (name "rust-windows-aarch64-msvc")
    (version "0.36.1")
    (source
     (origin
       (inherit (package-source rust-windows-aarch64-msvc-0.42))
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ixaxs2c37ll2smprzh0xq5p238zn8ylzb3lk1zddqmd77yw7f4v"))))))

(define-public rust-windows-aarch64-msvc-0.32
  (package
    (inherit rust-windows-aarch64-msvc-0.36)
    (name "rust-windows-aarch64-msvc")
    (version "0.32.0")
    (source (origin
              (inherit (package-source rust-windows-aarch64-msvc-0.36))
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1x8bnafz15ksgpbjbgk1l1j2jx4rq4a2ylzcahb1jhy4n59jgsfq"))))))

(define-public rust-windows-aarch64-msvc-0.28
  (package
    (inherit rust-windows-aarch64-msvc-0.32)
    (name "rust-windows-aarch64-msvc")
    (version "0.28.0")
    (source (origin
              (inherit (package-source rust-windows-aarch64-msvc-0.32))
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hpk0n2z0jzzvwlvs98b75sa4q920953nqfc119rv19nwm0mlsaj"))))))

(define-public rust-windows-acl-0.3
  (package
    (name "rust-windows-acl")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-acl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hyfng4dagfndxpwxynjk9zird8lhrp7zrsqc1h4rjvbk0iifyqp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-field-offset" ,rust-field-offset-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-widestring" ,rust-widestring-0.4)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://trailofbits.github.io/windows-acl")
    (synopsis "Rust crate to simplify Windows ACL operations")
    (description
     "This package provides a rust crate to simplify Windows ACL operations.")
    (license license:expat)))

(define-public rust-windows-bindgen-0.58
  (package
    (name "rust-windows-bindgen")
    (version "0.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12gm2nmwbgspvmmrrjg1pqqf6mk7qmkmaqg5lvrm28v97kcjikci"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-windows-metadata" ,rust-windows-metadata-0.58))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows metadata compiler")
    (description "This package provides Windows metadata compiler.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-bindgen-0.57
  (package
    (inherit rust-windows-bindgen-0.58)
    (name "rust-windows-bindgen")
    (version "0.57.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d67wwhbdwf3rmdbiyfsz55hky2a972y2xqg7iablxv27l8rdjqw"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-windows-metadata" ,rust-windows-metadata-0.57))))))

(define-public rust-windows-bindgen-0.56
  (package
    (inherit rust-windows-bindgen-0.57)
    (name "rust-windows-bindgen")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0inv2w78qv6375ndrgm9vilkgscwak80igz8vkf7zw8c6fk3x3m2"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-windows-metadata" ,rust-windows-metadata-0.56))))))

(define-public rust-windows-bindgen-0.54
  (package
    (inherit rust-windows-bindgen-0.57)
    (name "rust-windows-bindgen")
    (version "0.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hid039rnygimc2kxkzfc892j6hcdjpza2490ggz35r8fjs7csfq"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-windows-metadata" ,rust-windows-metadata-0.54))))))

(define-public rust-windows-bindgen-0.52
  (package
    (inherit rust-windows-bindgen-0.56)
    (name "rust-windows-bindgen")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "071lrbhbvh0l8m1wf5000xxmcry1gjpqdxcqm23qmss9d05zn3lp"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-windows-metadata" ,rust-windows-metadata-0.52))))))

(define-public rust-windows-bindgen-0.51
  (package
    (inherit rust-windows-bindgen-0.52)
    (name "rust-windows-bindgen")
    (version "0.51.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xfdq4q958qal5iks8xkaanf7w3akzfxc58dxvz7amhjg2vic7xw"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-windows-metadata" ,rust-windows-metadata-0.51))))))

(define-public rust-windows-interface-0.59
  (package
    (name "rust-windows-interface")
    (version "0.59.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a4zr8740gyzzhq02xgl6vx8l669jwfby57xgf0zmkcdkyv134mx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The interface macro for the windows crate")
    (description
     "This package provides The interface macro for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-implement-0.60
  (package
    (name "rust-windows-implement")
    (version "0.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dm88k3hlaax85xkls4gf597ar4z8m5vzjjagzk910ph7b8xszx4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The implement macro for the windows crate")
    (description
     "This package provides The implement macro for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-core-0.61
  (package
    (name "rust-windows-core")
    (version "0.61.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qsa3iw14wk4ngfl7ipcvdf9xyq456ms7cx2i9iwf406p7fx7zf0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-implement" ,rust-windows-implement-0.60)
                       ("rust-windows-interface" ,rust-windows-interface-0.59)
                       ("rust-windows-link" ,rust-windows-link-0.1)
                       ("rust-windows-result" ,rust-windows-result-0.3)
                       ("rust-windows-strings" ,rust-windows-strings-0.4))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Core type support for COM and Windows")
    (description
     "This package provides Core type support for COM and Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-core-0.58
  (package
    (name "rust-windows-core")
    (version "0.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16czypy425jzmiys4yb3pwsh7cm6grxn9kjp889iqnf2r17d99kb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-implement" ,rust-windows-implement-0.58)
                       ("rust-windows-interface" ,rust-windows-interface-0.58)
                       ("rust-windows-result" ,rust-windows-result-0.2)
                       ("rust-windows-strings" ,rust-windows-strings-0.1)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-core-0.57
  (package
    (inherit rust-windows-core-0.58)
    (name "rust-windows-core")
    (version "0.57.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bc3jxw2jw76xkk3ddvnp5b2m76qmbzv1qncgvb6qrlhl8wj9vfj"))))
    (arguments
     `(#:cargo-inputs (("rust-windows-implement" ,rust-windows-implement-0.57)
                       ("rust-windows-interface" ,rust-windows-interface-0.57)
                       ("rust-windows-result" ,rust-windows-result-0.1)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))
       #:cargo-development-inputs
       (("rust-windows-bindgen" ,rust-windows-bindgen-0.57))))))

(define-public rust-windows-core-0.56
  (package
    (inherit rust-windows-core-0.57)
    (name "rust-windows-core")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19pj57bm0rzhlk0ghrccd3i5zvh0ghm52f8cmdc8d3yhs8pfb626"))))
    (arguments
     `(#:cargo-inputs (("rust-windows-implement" ,rust-windows-implement-0.56)
                       ("rust-windows-interface" ,rust-windows-interface-0.56)
                       ("rust-windows-result" ,rust-windows-result-0.1)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))
       #:cargo-development-inputs
       (("rust-windows-bindgen" ,rust-windows-bindgen-0.56))))))

(define-public rust-windows-core-0.52
  (package
    (inherit rust-windows-core-0.56)
    (name "rust-windows-core")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))))
    (arguments
     `(#:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))))))

(define-public rust-windows-gen-0.9
  (package
    (name "rust-windows-gen")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_gen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lh492px26rrna0harikyy4p7nk520pw2lv0dczp4n2xa6y4s5al"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t #:cargo-inputs (("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "Code gen support for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnu-0.52
  (package
    (name "rust-windows-i686-gnu")
    (version "0.52.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))
              (snippet
               '(delete-file "lib/libwindows.0.52.0.a"))))
    (build-system cargo-build-system)
    (arguments (list #:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description
     "This package provides code gen support for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnu-0.48
  (package
    (inherit rust-windows-i686-gnu-0.52)
    (name "rust-windows-i686-gnu")
    (version "0.48.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))
              (snippet
               #~(delete-file "lib/libwindows.0.48.5.a"))))
    (arguments (list #:skip-build? #t))))

(define-public rust-windows-i686-gnu-0.42
  (package
    (inherit rust-windows-i686-gnu-0.48)
    (name "rust-windows-i686-gnu")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))
              (snippet
               #~(delete-file "lib/libwindows.a"))))
    (arguments
     (list #:skip-build? #t))))

(define-public rust-windows-i686-gnu-0.39
  (package
    (inherit rust-windows-i686-gnu-0.42)
    (name "rust-windows-i686-gnu")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_gnu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06wynhxklmh3s1ril9bh00rhv1npppcyirsp60p09xx501qwagvn"))
       (snippet
        #~(delete-file "lib/libwindows.a"))))))

(define-public rust-windows-i686-gnu-0.36
  (package
    (inherit rust-windows-i686-gnu-0.42)
    (name "rust-windows-i686-gnu")
    (version "0.36.1")
    (source (origin
              (inherit (package-source rust-windows-i686-gnu-0.42))
              (method url-fetch)
              (uri (crate-uri "windows_i686_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dm3svxfzamrv6kklyda9c3qylgwn5nwdps6p0kc9x6s077nq3hq"))))))

(define-public rust-windows-i686-gnu-0.32
  (package
    (inherit rust-windows-i686-gnu-0.36)
    (name "rust-windows-i686-gnu")
    (version "0.32.0")
    (source (origin
              (inherit (package-source rust-windows-i686-gnu-0.36))
              (method url-fetch)
              (uri (crate-uri "windows_i686_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05g6kpdfxwxnw2gn1nrd7bsf5997rci0k3h3nqby168ph5l1qwba"))))))

(define-public rust-windows-i686-gnu-0.28
  (package
    (inherit rust-windows-i686-gnu-0.32)
    (name "rust-windows-i686-gnu")
    (version "0.28.0")
    (source (origin
              (inherit (package-source rust-windows-i686-gnu-0.32))
              (method url-fetch)
              (uri (crate-uri "windows_i686_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12hx7qpsjg9p7jggfcplqa3mf1mzr7k7s5ybzqwg1zmg4fn2aizm"))))))

(define-public rust-windows-i686-gnullvm-0.52
  (package
    (name "rust-windows-i686-gnullvm")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))
       (snippet
        '(delete-file "lib/libwindows.0.52.0.a"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-msvc-0.52
  (package
    (name "rust-windows-i686-msvc")
    (version "0.52.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))
              (snippet
               '(delete-file "lib/windows.0.52.0.lib"))))
    (build-system cargo-build-system)
    (arguments (list #:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description
     "This package provides code gen support for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-msvc-0.48
  (package
    (inherit rust-windows-i686-msvc-0.52)
    (name "rust-windows-i686-msvc")
    (version "0.48.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))
              (snippet
               #~(delete-file "lib/windows.0.48.5.lib"))))
    (arguments (list #:skip-build? #t))))

(define-public rust-windows-i686-msvc-0.42
  (package
    (inherit rust-windows-i686-msvc-0.48)
    (name "rust-windows-i686-msvc")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))
              (snippet
               #~(delete-file "lib/windows.lib"))))
    (arguments
     (list #:skip-build? #t))))

(define-public rust-windows-i686-msvc-0.39
  (package
    (inherit rust-windows-i686-msvc-0.42)
    (name "rust-windows-i686-msvc")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01hiv9msxssy5iqxs7bczvf094k4mz56yi4z1bhj32c2b3zcpivv"))
       (snippet
        #~(delete-file "lib/windows.lib"))))))

(define-public rust-windows-i686-msvc-0.36
  (package
    (inherit rust-windows-i686-msvc-0.42)
    (name "rust-windows-i686-msvc")
    (version "0.36.1")
    (source (origin
              (inherit (package-source rust-windows-i686-msvc-0.42))
              (method url-fetch)
              (uri (crate-uri "windows_i686_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "097h2a7wig04wbmpi3rz1akdy4s8gslj5szsx8g2v0dj91qr3rz2"))))))

(define-public rust-windows-i686-msvc-0.32
  (package
    (inherit rust-windows-i686-msvc-0.36)
    (name "rust-windows-i686-msvc")
    (version "0.32.0")
    (source (origin
              (inherit (package-source rust-windows-i686-msvc-0.36))
              (method url-fetch)
              (uri (crate-uri "windows_i686_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wj1wi01fc8hrasbakjcq8y5a7ynw9l2mcw08svmsq823axi2v0l"))))))

(define-public rust-windows-i686-msvc-0.28
  (package
    (inherit rust-windows-i686-msvc-0.32)
    (name "rust-windows-i686-msvc")
    (version "0.28.0")
    (source (origin
              (inherit (package-source rust-windows-i686-msvc-0.32))
              (method url-fetch)
              (uri (crate-uri "windows_i686_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0r0z8s1wcdwd20azsdfilf2a6bz68xkavl990wy64hyc8f51bmai"))))))

(define-public rust-windows-implement-0.58
  (package
    (name "rust-windows-implement")
    (version "0.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16spr5z65z21qyv379rv2mb1s5q2i9ibd1p2pkn0dr9qr535pg9b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The implement macro for the windows crate")
    (description
     "This package provides The @code{implement} macro for the
Windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-implement-0.57
  (package
    (inherit rust-windows-implement-0.58)
    (name "rust-windows-implement")
    (version "0.57.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mqs7qypclnmx5r8yq5jy3g2d8i27vzag9yzzzxzpdnmb70ds1wi"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))))

(define-public rust-windows-implement-0.56
  (package
    (inherit rust-windows-implement-0.57)
    (name "rust-windows-implement")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16rgkvlx4syqmajfdwmkcvn6nvh126wjj8sg3jvsk5fdivskbz7n"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))))

(define-public rust-windows-implement-0.52
  (package
    (inherit rust-windows-implement-0.58)
    (name "rust-windows-implement")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0il91jkdgnwl20gm8dwbjswsmiq7paif49dyk5kvhwv72wrqq5hj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))))

(define-public rust-windows-implement-0.48
  (package
    (inherit rust-windows-implement-0.52)
    (name "rust-windows-implement")
    (version "0.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1764n853zd7bb0wn94i0qxfs6kdy7wrz7v9qhdn7x7hvk64fabjy"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-windows-implement-0.46
  (package
    (inherit rust-windows-implement-0.48)
    (name "rust-windows-implement")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-implement" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09kw706qcms5yy34lf714zspj34v8jirfxv7yycavpcsa9czpd69"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-windows-implement-0.44
  (package
    (inherit rust-windows-implement-0.48)
    (name "rust-windows-implement")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ij5q9khlcfn43a1p3ypjbn711k50s9pc8la5bf04ys1wfl7rs3c"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-windows-implement-0.43
  (package
    (inherit rust-windows-implement-0.48)
    (name "rust-windows-implement")
    (version "0.43.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01zqmrdnz2j5qd4ahqjsz724mdabi577f400yywcahy7dl2rpqmp"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-windows-implement-0.39
  (package
    (inherit rust-windows-implement-0.46)
    (name "rust-windows-implement")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ryfy5sgf26xmflf33zabzqn10pp6pjrbz75yh2xrdcwa27zj0ds"))))
    (arguments
     `(#:cargo-inputs (("rust-syn" ,rust-syn-1)
                       ("rust-windows-tokens" ,rust-windows-tokens-0.39))))))

(define-public rust-windows-interface-0.58
  (package
    (name "rust-windows-interface")
    (version "0.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "059mxmfvx3x88q74ms0qlxmj2pnidmr5mzn60hakn7f95m34qg05"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The interface macro for the Windows crate")
    (description
     "This package provides the interface macro for the Windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-interface-0.57
  (package
    (inherit rust-windows-interface-0.58)
    (name "rust-windows-interface")
    (version "0.57.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19zwlzr0q1z9s692681yb5w2lhvwcyx4v95s25hfdkd3isry9gi9"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))))

(define-public rust-windows-interface-0.56
  (package
    (inherit rust-windows-interface-0.57)
    (name "rust-windows-interface")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k2prfxna0mw47f8gi8qhw9jfpw66bh2cqzs67sgipjfpx30b688"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))))

(define-public rust-windows-interface-0.52
  (package
    (inherit rust-windows-interface-0.58)
    (name "rust-windows-interface")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1la254wzd8qlbxplvb667z5mwdh9jngg1qyhxg6fx9wm00pc73cx"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))))

(define-public rust-windows-interface-0.48
  (package
    (inherit rust-windows-interface-0.52)
    (name "rust-windows-interface")
    (version "0.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1iqcilw0hfyzwhk12xfmcy40r10406sgf4xmdansijlv1kr8vyz6"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-windows-interface-0.46
  (package
    (inherit rust-windows-interface-0.48)
    (name "rust-windows-interface")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-interface" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0a87zxh3wq5ng1vvgqf7jhydsshrpc5w39pyvr0l1vyv3q5k67xc"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-windows-interface-0.44
  (package
    (inherit rust-windows-interface-0.48)
    (name "rust-windows-interface")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zwwwfzjdf087gvgy48bbfq9yd0fsh1fj5wzs88gim7cj6jnjgw5"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-windows-interface-0.43
  (package
    (inherit rust-windows-interface-0.44)
    (name "rust-windows-interface")
    (version "0.43.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06drakcacg4d680qa2sk62kqn7ini00xw3zz0hwqwx1my2z4z3dw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))))

(define-public rust-windows-interface-0.39
  (package
    (inherit rust-windows-interface-0.46)
    (name "rust-windows-interface")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00h56znmak3p8bh28y3s48m5zv6q7dn40vnvf3dzf0sz5rszrym2"))))))

(define-public rust-windows-macros-0.9
  (package
    (name "rust-windows-macros")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xivsg3lf023hs83xiab2k40fmrl11nbihcdrdkc8pc4ab398xqg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-syn" ,rust-syn-1) ("rust-windows-gen" ,rust-windows-gen-0.9))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Macros for the windows crate")
    (description "Macros for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-metadata-0.58
  (package
    (name "rust-windows-metadata")
    (version "0.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "028qjb9gdljm1m9lrbg1kd1rjy8s8hzsj0k313kykkqj60y7z0rf"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows metadata reader")
    (description "This package provides Windows metadata reader.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-metadata-0.57
  (package
    (inherit rust-windows-metadata-0.58)
    (name "rust-windows-metadata")
    (version "0.57.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z1xl76p0zjwsvyj16w6pbp2layhjqz13y34pid9x7avh9vd0243"))))))

(define-public rust-windows-metadata-0.56
  (package
    (inherit rust-windows-metadata-0.57)
    (name "rust-windows-metadata")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d1vizbp6b1wjh3qnjrh120w1iwqal3lfj52wdac847zgy1gg4rr"))))))

(define-public rust-windows-metadata-0.54
  (package
    (inherit rust-windows-metadata-0.57)
    (name "rust-windows-metadata")
    (version "0.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hh4bpima19p18kr5a2ss46hgmgafjkqzyfzhm0dazvx6sw70hz4"))))))

(define-public rust-windows-metadata-0.52
  (package
    (inherit rust-windows-metadata-0.56)
    (name "rust-windows-metadata")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vz49s2mm74fmjabh3kxxhzbz16ys41b78jgi6xwssp2069db3r1"))))))

(define-public rust-windows-metadata-0.51
  (package
    (inherit rust-windows-metadata-0.52)
    (name "rust-windows-metadata")
    (version "0.51.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03h0c6qs1yyl0z69p4k1hdq636j868qdxnri1dy47nprjvckacbm"))))))

(define-public rust-windows-registry-0.2
  (package
    (name "rust-windows-registry")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-registry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c04923fq0rbvl3z0h67xr6rh2fgwkizhclhqv0j79i0nwdh0074"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-result" ,rust-windows-result-0.2)
                       ("rust-windows-strings" ,rust-windows-strings-0.1)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows registry")
    (description "This package provides Windows registry.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-link-0.1
  (package
    (name "rust-windows-link")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-link" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f2cq7imbrppsmmnz8899hfhg07cp5gq6rh0bjhb1qb6nwshk13n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Linking for Windows")
    (description "This package provides Linking for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-result-0.3
  (package
    (name "rust-windows-result")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-result" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-link" ,rust-windows-link-0.1))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows error handling")
    (description "This package provides Windows error handling.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-result-0.2
  (package
    (name "rust-windows-result")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-result" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03mf2z1xcy2slhhsm15z24p76qxgm2m74xdjp8bihyag47c4640x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows error handling")
    (description "This package provides Windows error handling.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-result-0.1
  (package
    (inherit rust-windows-result-0.2)
    (name "rust-windows-result")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-result" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y274q1v0vy21lhkgslpxpq1m08hvr1mcs2l88h1b1gcx0136f2y"))))
    (arguments
     `(#:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))
       #:cargo-development-inputs
       (("rust-windows-bindgen" ,rust-windows-bindgen-0.57))))))

(define-public rust-windows-service-0.7
  (package
    (name "rust-windows-service")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-service" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12lwc04rji06p8g8dhc5aps6cw3xbx6admzqxj8l0jkkgz66nkfj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; unresolved import `windows_service::service`
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-widestring" ,rust-widestring-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/mullvad/windows-service-rs")
    (synopsis
     "Facilities for management and implementation of Windows services")
    (description
     "This package provides a crate that provides facilities for management and
implementation of Windows services.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-strings-0.1
  (package
    (name "rust-windows-strings")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-strings" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "042dxvi3133f7dyi2pgcvknwkikk47k8bddwxbq5s0l6qhjv3nac"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-result" ,rust-windows-result-0.2)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows strings provides common string types")
    (description "This package provides common string types when using
Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-sys-0.59
  (package
    (name "rust-windows-sys")
    (version "0.59.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "The windows crate lets you call any Windows API past,
present, and future using code generated on the fly directly from the metadata
describing the API and right into your Rust package where you can call them as
if they were just another Rust module.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-sys-0.52
  (package
    (inherit rust-windows-sys-0.59)
    (name "rust-windows-sys")
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))))))

(define-public rust-windows-sys-0.48
  (package
    (inherit rust-windows-sys-0.52)
    (name "rust-windows-sys")
    (version "0.48.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.48))))))

(define-public rust-windows-sys-0.45
  (package
    (inherit rust-windows-sys-0.48)
    (name "rust-windows-sys")
    (version "0.45.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))))
    (arguments
     (list #:skip-build? #t
           #:cargo-inputs
           `(("rust-windows-targets" ,rust-windows-targets-0.42))))))

(define-public rust-windows-sys-0.42
  (package
    (inherit rust-windows-sys-0.45)
    (name "rust-windows-sys")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19waf8aryvyq9pzk0gamgfwjycgzk4gnrazpfvv171cby0h1hgjs"))))
    (arguments
     (list #:skip-build? #t
           #:cargo-inputs
           `(("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.42)
             ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.42)
             ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.42)
             ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.42)
             ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.42)
             ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.42)
             ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.42))))))

(define-public rust-windows-sys-0.36
  (package
    (inherit rust-windows-sys-0.42)
    (name "rust-windows-sys")
    (version "0.36.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lmqangv0zg1l46xiq7rfnqwsx8f8m52mqbgg2mrx7x52rd1a17a"))))
    (arguments
     (list #:skip-build? #t
           #:cargo-inputs
           `(("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.36)
             ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.36)
             ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.36)
             ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.36)
             ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.36))))))

(define-public rust-windows-sys-0.28
  (package
    (inherit rust-windows-sys-0.36)
    (name "rust-windows-sys")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xkghf343nll9i1yvha1a4spf53mnb5knzmnqj9adgsw5mh3kjl2"))))
    (arguments
     (list #:skip-build? #t
           #:cargo-inputs
           `(("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.28)
             ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.28)
             ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.28)
             ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.28)
             ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.28))))))

(define-public rust-windows-targets-0.52
  (package
    (name "rust-windows-targets")
    (version "0.52.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-targets" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.52)
        ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.52)
        ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.52)
        ("rust-windows-i686-gnullvm" ,rust-windows-i686-gnullvm-0.52)
        ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.52)
        ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.52)
        ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.52)
        ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description
     "This package provides code gen support for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-targets-0.48
  (package
    (inherit rust-windows-targets-0.52)
    (name "rust-windows-targets")
    (version "0.48.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-targets" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))))
    (arguments
     `(#:cargo-inputs
       (("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.48)
        ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.48)
        ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.48)
        ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.48)
        ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.48)
        ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.48)
        ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.48))))))

(define-public rust-windows-targets-0.42
  (package
    (inherit rust-windows-targets-0.48)
    (name "rust-windows-targets")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-targets" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))))
    (arguments
     `(#:cargo-inputs
       (("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.42)
        ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.42)
        ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.42)
        ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.42)
        ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.42)
        ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.42)
        ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.42))))))

(define-public rust-windows-tokens-0.39
  (package
    (name "rust-windows-tokens")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-tokens" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15zmsz8ji6z7471xwznrm4hqp6j94s7pjjz7i34vmrjzw4pxwf7q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "This package provides code generation support for the
windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-version-0.1
  (package
    (name "rust-windows-version")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-version" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05a5hia3d2vxd16vj2hxiyicxaqdjcm9sgpip4pzza4vgi2sm639"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))
       #:cargo-development-inputs
       (("rust-windows-bindgen" ,rust-windows-bindgen-0.56))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows version information")
    (description "This package provides Windows version information.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-win-3
  (package
    (name "rust-windows-win")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-win" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p7jbk3i7wj1i6w7chfp4rpbyd6ckgncp6h493wm4frbc8rkxqjq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; unresolved import `windows_win::sys`
       #:cargo-inputs (("rust-error-code" ,rust-error-code-3))
       #:cargo-development-inputs (("rust-clipboard-win" ,rust-clipboard-win-5))))
    (home-page "https://github.com/DoumanAsh/windows-win-rs")
    (synopsis "Windows hacking library to find windows and access them")
    (description
     "Some windows hacking library with utilities to find windows and access them.")
    (license license:boost1.0)))

(define-public rust-windows-x86-64-gnu-0.52
  (package
    (name "rust-windows-x86-64-gnu")
    (version "0.52.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))
              (snippet
               '(delete-file "lib/libwindows.0.52.0.a"))))
    (build-system cargo-build-system)
    (arguments (list #:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description
     "This package provides code gen support for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnu-0.48
  (package
    (inherit rust-windows-x86-64-gnu-0.52)
    (name "rust-windows-x86-64-gnu")
    (version "0.48.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))
              (snippet
               #~(delete-file "lib/libwindows.0.48.5.a"))))
    (arguments (list #:skip-build? #t))))

(define-public rust-windows-x86-64-gnu-0.42
  (package
    (inherit rust-windows-x86-64-gnu-0.48)
    (name "rust-windows-x86-64-gnu")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))
              (snippet
               #~(delete-file "lib/libwindows.a"))))
    (arguments
     (list #:skip-build? #t))))

(define-public rust-windows-x86-64-gnu-0.39
  (package
    (inherit rust-windows-x86-64-gnu-0.42)
    (name "rust-windows-x86-64-gnu")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_gnu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r9b4lmapq66nn2dga7a0mkdv5sgbp184kfwx3hklrbxcdjw2s38"))
       (snippet
        #~(delete-file "lib/libwindows.a"))))))

(define-public rust-windows-x86-64-gnu-0.36
  (package
    (inherit rust-windows-x86-64-gnu-0.42)
    (name "rust-windows-x86-64-gnu")
    (version "0.36.1")
    (source (origin
              (inherit (package-source rust-windows-x86-64-gnu-0.42))
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qfrck3jnihymfrd01s8260d4snql8ks2p8yaabipi3nhwdigkad"))))))

(define-public rust-windows-x86-64-gnu-0.32
  (package
    (inherit rust-windows-x86-64-gnu-0.36)
    (name "rust-windows-x86-64-gnu")
    (version "0.32.0")
    (source (origin
              (inherit (package-source rust-windows-x86-64-gnu-0.36))
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1g34xhcayig9sndq3555w95q6lr7jr839zxv6l365ijlfhpv24n9"))))))

(define-public rust-windows-x86-64-gnu-0.28
  (package
    (inherit rust-windows-x86-64-gnu-0.32)
    (name "rust-windows-x86-64-gnu")
    (version "0.28.0")
    (source (origin
              (inherit (package-source rust-windows-x86-64-gnu-0.32))
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0m79bhdr54g4h4wh2q8wkjlkypb5wvl7xzhc2csiaqb5yl4z8cdw"))))))

(define-public rust-windows-x86-64-gnullvm-0.52
  (package
    (name "rust-windows-x86-64-gnullvm")
    (version "0.52.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))
              (snippet
               '(delete-file "lib/libwindows.0.52.0.a"))))
    (build-system cargo-build-system)
    (arguments (list #:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description
     "This package provides code gen support for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnullvm-0.48
  (package
    (inherit rust-windows-x86-64-gnullvm-0.52)
    (name "rust-windows-x86-64-gnullvm")
    (version "0.48.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))
              (snippet
               '(delete-file "lib/libwindows.0.48.5.a"))))
    (arguments (list #:skip-build? #t))))

(define-public rust-windows-x86-64-gnullvm-0.42
  (package
    (inherit rust-windows-x86-64-gnullvm-0.48)
    (name "rust-windows-x86-64-gnullvm")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))
              (snippet
               '(delete-file "lib/libwindows.a"))))
    (arguments (list #:skip-build? #t))))

(define-public rust-windows-x86-64-msvc-0.52
  (package
    (name "rust-windows-x86-64-msvc")
    (version "0.52.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))
              (snippet
               '(delete-file "lib/windows.0.52.0.lib"))))
    (build-system cargo-build-system)
    (arguments (list #:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description
     "This package provides code gen support for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-msvc-0.48
  (package
    (inherit rust-windows-x86-64-msvc-0.52)
    (name "rust-windows-x86-64-msvc")
    (version "0.48.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))
              (snippet
               #~(delete-file "lib/windows.0.48.5.lib"))))
    (arguments (list #:skip-build? #t))))

(define-public rust-windows-x86-64-msvc-0.42
  (package
    (inherit rust-windows-x86-64-msvc-0.48)
    (name "rust-windows-x86-64-msvc")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))
              (snippet
               #~(delete-file "lib/windows.lib"))))
    (arguments (list #:skip-build? #t))))

(define-public rust-windows-x86-64-msvc-0.39
  (package
    (inherit rust-windows-x86-64-msvc-0.42)
    (name "rust-windows-x86-64-msvc")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02g8fy1sv8g0p4fi2yk62j9a5zwhcfknp9vwg1ifkjp97a440kay"))
        (snippet
         #~(delete-file "lib/windows.lib"))))))

(define-public rust-windows-x86-64-msvc-0.36
  (package
    (inherit rust-windows-x86-64-msvc-0.42)
    (name "rust-windows-x86-64-msvc")
    (version "0.36.1")
    (source (origin
              (inherit (package-source rust-windows-x86-64-msvc-0.42))
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "103n3xijm5vr7qxr1dps202ckfnv7njjnnfqmchg8gl5ii5cl4f8"))))))

(define-public rust-windows-x86-64-msvc-0.32
  (package
    (inherit rust-windows-x86-64-msvc-0.36)
    (name "rust-windows-x86-64-msvc")
    (version "0.32.0")
    (source (origin
              (inherit (package-source rust-windows-x86-64-msvc-0.36))
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05l392h518dxn808dc1zkv6d0r9z38q68qqc0ix9fs9741v28jjh"))))))

(define-public rust-windows-x86-64-msvc-0.28
  (package
    (inherit rust-windows-x86-64-msvc-0.32)
    (name "rust-windows-x86-64-msvc")
    (version "0.28.0")
    (source (origin
              (inherit (package-source rust-windows-x86-64-msvc-0.32))
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17z8q25pd3dp6b84qm9nlayd3ym78sbryxlqmgcxvz9vpmy8qarz"))))))

(define-public rust-winreg-0.52
  (package
    (name "rust-winreg")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19gh9vp7mp1ab84kc3ag48nm9y7xgjhh3xa4vxss1gylk1rsaxx2"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "Cargo.toml"
                   (("\"~([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                    (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; OS not supported
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.3)
                                   ("rust-serde-transcode" ,rust-serde-transcode-1)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/gentoo90/winreg-rs")
    (synopsis "Rust bindings to the MS Windows Registry API")
    (description
     "This package provides Rust bindings to MS Windows Registry API.")
    (license license:expat)))

(define-public rust-winreg-0.51
  (package
    (inherit rust-winreg-0.52)
    (name "rust-winreg")
    (version "0.51.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z48nmaskwsiyyq9576sgf8ya3fvf1xg3kma8q7n8ml1jkvkszwk"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "Cargo.toml"
                   (("\"~([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                    (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t ; OS not supported
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.3)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-winreg-0.50
  (package
    (inherit rust-winreg-0.51)
    (name "rust-winreg")
    (version "0.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cddmp929k882mdh6i9f2as848f13qqna6czwsqzkh1pqnr5fkjj"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "Cargo.toml"
                   (("\"~([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                    (string-append "\"^" version)))))))
    (arguments
     `(#:skip-build? #t ; OS not supported
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.3)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-winreg-0.10
  (package
    (inherit rust-winreg-0.50)
    (name "rust-winreg")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17c6h02z88ijjba02bnxi5k94q5cz490nf3njh9yypf8fbig9l40"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-winreg-0.8
  (package
    (inherit rust-winreg-0.10)
    (name "rust-winreg")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1364vyx4kh170pxfg8iwlvv8xskvry53xfya0565q8qnx73gh1yi"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-winreg-0.7
  (package
    (inherit rust-winreg-0.8)
    (name "rust-winreg")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0sdxcyvda4v1v6a0k1j2v1400z3ng323k9a56gxvkq51x21dn801"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.3)
        ("rust-serde-derive" ,rust-serde-derive-1))))))

(define-public rust-winreg-0.6
  (package
    (name "rust-winreg")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jdcqr6zmvwyrp87h48miasfdvv16gjsb60rc8dy2kqwb3mnv65j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.3)
        ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/gentoo90/winreg-rs")
    (synopsis "Rust bindings to MS Windows Registry API")
    (description
     "This package provides Rust bindings to MS Windows Registry API.")
    (license license:expat)))

(define-public rust-winres-0.1
  (package
    (name "rust-winres")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winres" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v2gvqnd8iwwvb6fs69nv0mmk1z96430527n0qlfbsarxxhv53dn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/mxre/winres")
    (synopsis "Create and set windows icons and metadata for executables")
    (description "A simple library to facilitate adding metainformation and
icons to windows executables and dynamic libraries.")
    (license license:expat)))

(define-public rust-winresource-0.1
  (package
    (name "rust-winresource")
    (version "0.1.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winresource" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fv9xlgg9a6gp1jhrp7zj7kln7ris64889n3z1x59m1s6ldnjxkj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--"
                            "--skip=tests::toolkit_include_win10"
                            "--skip=tests::toolkit_include_win8"
                            "--skip=WindowsResource::set_language"
                            "--skip=WindowsResource::set_manifest")
       #:cargo-inputs (("rust-toml" ,rust-toml-0.8)
                       ("rust-version-check" ,rust-version-check-0.9))
       #:cargo-development-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/BenjaminRi/winresource")
    (synopsis "Create and set windows icons and metadata for executables")
    (description "This package provides functions to create and set windows
icons and metadata for executables.")
    (license license:expat)))

(define-public rust-winsafe-0.0.19
  (package
    (name "rust-winsafe")
    (version "0.0.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winsafe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rodrigocfd/winsafe")
    (synopsis "Windows API and GUI in safe, idiomatic Rust")
    (description
     "This package provides Windows API and GUI in safe, idiomatic Rust.")
    (license license:expat)))

(define-public rust-winutil-0.1
  (package
    (name "rust-winutil")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winutil" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0vkyl3fbbf05n5ph5yz8sfaccrk9x3qsr25560w6w68ldf5i7bvx"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))))
    (build-system cargo-build-system)
    (home-page "https://bitbucket.org/DaveLancaster/winutil")
    (synopsis "Library wrapping a handful of useful winapi functions")
    (description
     "A simple library wrapping a handful of useful winapi functions.")
    (license license:expat)))

(define-public rust-wio-0.2
  (package
    (name "rust-wio")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "199p404fp96w1f1c93bf1jrvaqwypxf3hmmldhww4jk4yhr9j4jx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/retep998/wio-rs")
    (synopsis "Windows IO wrapper")
    (description
     "Wio is a middle-level wrapper around various things in Windows API.  It
is designed to be a very thin layer around Windows API to provide a safe Rusty
API but without hiding any functionality.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wmi-0.12
  (package
    (name "rust-wmi")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wmi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "000sdzx8fnw913ws28ranf8bhm8dvvdpz89s4fhqfbkxpd5b9zys"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-windows" ,rust-windows-0.48))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tempdir" ,rust-tempdir-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/ohadravid/wmi-rs")
    (synopsis "WMI crate for Rust")
    (description
     "This package provides the WMI (Windows Management Instrumentation) crate
for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ws2-32-sys-0.2
  (package
    (name "rust-ws2-32-sys")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ws2_32-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ppscg5qfqaw0gzwv2a4nhn5bn01ff9iwn6ysqnzm4n8s3myz76m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.2)
        ("rust-winapi-build" ,rust-winapi-build-0.1))))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Function definitions for the Windows API library ws2_32")
    (description
     "Contains function definitions for the Windows API library ws2_32.")
    (license license:expat)))
