;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2024 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io))

(define-public rust-anstyle-wincon-3
  (package
    (name "rust-anstyle-wincon")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle-wincon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19v0fv400bmp4niqpzxnhg83vz12mmqv7l2l8vi80qcdxj0lpm8w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstyle" ,rust-anstyle-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-lexopt" ,rust-lexopt-0.3))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Styling legacy Windows terminals")
    (description "Styling legacy Windows terminals.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-wincon-1
  (package
    (inherit rust-anstyle-wincon-3)
    (name "rust-anstyle-wincon")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "anstyle-wincon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12714vwjf4c1wm3qf49m5vmd93qvq2nav6zpjc0bxbh3ayjby2hq"))))
    (arguments
     `(#:cargo-inputs
       (("rust-anstyle" ,rust-anstyle-1)
        ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs
       (("rust-lexopt" ,rust-lexopt-0.3))))))

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
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-credential-wincred" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bb9yczmk3ral2r20v5c4jzf3l9qp0nmm11i20s0w5inprp9b228"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cargo-credential" ,rust-cargo-credential-0.4)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
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

(define-public rust-clipboard-win-5
  (package
    (name "rust-clipboard-win")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clipboard-win" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15x28184cw6r8hx30farqvcir0kz151dfbfms4avl9q11rqa1y8j"))))
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
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "codepage" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sdxp0f8dy2rscln646326lny9i0jm7ncgyp4yncdwndq0i943lb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-encoding-rs" ,rust-encoding-rs-0.8))))
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

(define-public rust-dunce-1
  (package
    (name "rust-dunce")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dunce" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fqcbwfclldbknmawi69l6zyncaiqzxkpbybcb2cc7jmlxnqrkjn"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "embed-resource" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cicrqlblz5xf4ns7yk4aifpskbvvvg436ls4c93h77f4zlc6k7m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-vswhom" ,rust-vswhom-0.1)
                       ("rust-winreg" ,rust-winreg-0.51))))
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

(define-public rust-ipconfig-0.3
  (package
    (name "rust-ipconfig")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ipconfig" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gn5j5sp58mz0630dhs1b8by7j0jqagldbd5iyln690gp7qjlc5x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-socket2" ,rust-socket2-0.4)
        ("rust-widestring" ,rust-widestring-0.5)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winreg" ,rust-winreg-0.10))))
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

(define-public rust-output-vt100-0.1
  (package
    (name "rust-output-vt100")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "output_vt100" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ygqplpxz4gg3i8f3rkan2q69pqll7gv65l2mmd8r9dphnvwbkak"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Phundrak/output-vt100-rs")
    (synopsis
     "Utility to activate escape codes in Windows' CMD and PowerShell")
    (description
     "Utility to activate escape codes in Windows' CMD and PowerShell.")
    (license license:expat)))

(define-public rust-python3-dll-a-0.2
  (package
    (name "rust-python3-dll-a")
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "python3-dll-a" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0grlz344xq435qgs5mic0yz1p643qh3h1m11lw4zms1b87a7rw6m"))))
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

(define-public rust-remove-dir-all-0.8
  (package
    (name "rust-remove-dir-all")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "remove_dir_all" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ay0mi06ak1n5gw3yjgp0hvzl59nj31ahxpdqvczw5qrvkx5r293"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aligned" ,rust-aligned-0.4)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-cvt" ,rust-cvt-0.1)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-fs-at" ,rust-fs-at-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-normpath" ,rust-normpath-1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-windows-sys" ,rust-windows-sys-0.45))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-env-logger" ,rust-env-logger-0.10)
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
    (inherit rust-remove-dir-all-0.8)
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
    (version "0.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "schannel" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1cxivcbczzfv4295pqi5s80kr7nhs7xc0i40zf43b7q5qw3gng3i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-windows-sys" ,rust-windows-sys-0.42))))
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

(define-public rust-uds-windows-0.1
  (package
    (inherit rust-uds-windows-1)
    (name "rust-uds-windows")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uds_windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v7g9hl06s13xc4d2bn6khqn2cz6ha2f2d71j8l6mvlhypsjpch9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-winapi" ,rust-winapi-0.3))))))

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
     `(#:cargo-inputs
       (("winapi-i686-pc-windows-gnu" ,rust-winapi-i686-pc-windows-gnu-0.4)
        ("winapi-x86-64-pc-windows-gnu" ,rust-winapi-x86-64-pc-windows-gnu-0.4))))
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
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15i5lm39wd44004i9d5qspry2cynkrpvwzghr6s2c3dsk28nz7pj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
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
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winapi-wsapoll" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vnzlcm6yrlx0xdx4g7zr41n84aj73h0p8fwh0m60mbiyl873ha4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))))
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

(define-public rust-winreg-0.5
  (package
    (inherit rust-winreg-0.7)
    (name "rust-winreg")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jkh4jj2g8g0bl7r1xvq9vv9hr4gdzphg9ndqm65q6f1jn9paym2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-clippy" ,rust-clippy-0.0)
        ("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))))))

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
    (version "0.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winresource" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aakwh8llq2zvm7qihkrg7sz50hzccyl4x831j60g4psijpsmqkp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; failed to resolve: could not find `um` in `winapi`
       #:cargo-inputs (("rust-toml" ,rust-toml-0.7)
                       ("rust-version-check" ,rust-version-check-0.9))
       #:cargo-development-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/BenjaminRi/winresource")
    (synopsis "Create and set windows icons and metadata for executables")
    (description "This package provides functions to create and set windows
icons and metadata for executables.")
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

(define-public rust-windows-0.52
  (package
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
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "The windows crate lets you call any Windows API past,
present, and future using code generated on the fly directly from the metadata
describing the API and right into your Rust package where you can call them as
if they were just another Rust module.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-windows-0.32
  (package
    (inherit rust-windows-0.46)
    (name "rust-windows")
    (version "0.32.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1v0h5b5g1ak2f2a6gkgjqgrqkkbdcfmf02nfmmj27g4nj3dzdvgv"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-windows-implement" ,rust-windows-implement-0.32)
        ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.32)
        ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.32)
        ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.32)
        ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.32)
        ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.32))))))

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
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1shmn1kbdc0bpphcxz0vlph96bxz0h1jlmh93s9agf2dbpin8xyb"))
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
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vvmy1ypvzdvxn9yf0b8ygfl85gl2gpcyvsvqppsmlpisil07amv"))
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

(define-public rust-windows-bindgen-0.52
  (package
    (name "rust-windows-bindgen")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "071lrbhbvh0l8m1wf5000xxmcry1gjpqdxcqm23qmss9d05zn3lp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-windows-metadata" ,rust-windows-metadata-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows metadata compiler")
    (description "Windows metadata compiler.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-windows-core-0.52
  (package
    (name "rust-windows-core")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides the core of Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnu-0.52
  (package
    (name "rust-windows-i686-gnu")
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04zkglz4p3pjsns5gbz85v4s5aw102raz4spj4b0lmm33z5kg1m2"))
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

(define-public rust-windows-i686-msvc-0.52
  (package
    (name "rust-windows-i686-msvc")
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16kvmbvx0vr0zbgnaz6nsks9ycvfh5xp05bjrhq65kj623iyirgz"))
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

(define-public rust-windows-implement-0.52
  (package
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
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The implement macro for the windows crate")
    (description "This package provides the @code{implement} macro for the
windows crate.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-windows-implement-0.32
  (package
    (inherit rust-windows-implement-0.46)
    (name "rust-windows-implement")
    (version "0.32.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-implement" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0f3lnjs9rlihin9cjf9y7np1x15c0v09v0cwlw1n7c30145xmciz"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-syn" ,rust-syn-1)
        ("rust-windows-tokens" ,rust-windows-tokens-0.32))))))

(define-public rust-windows-interface-0.52
  (package
    (name "rust-windows-interface")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1la254wzd8qlbxplvb667z5mwdh9jngg1qyhxg6fx9wm00pc73cx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The interface macro for the windows crate")
    (description "This package provides the interface macro for the windows
crate.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-windows-metadata-0.52
  (package
    (name "rust-windows-metadata")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vz49s2mm74fmjabh3kxxhzbz16ys41b78jgi6xwssp2069db3r1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows metadata reader")
    (description "Windows metadata reader.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-windows-sys-0.52
  (package
    (name "rust-windows-sys")
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "The windows crate lets you call any Windows API past,
present, and future using code generated on the fly directly from the metadata
describing the API and right into your Rust package where you can call them as
if they were just another Rust module.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-windows-targets-0.52
  (package
    (name "rust-windows-targets")
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-targets" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kg7a27ynzw8zz3krdgy6w5gbqcji27j1sz4p7xk2j5j8082064a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.52)
        ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.52)
        ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.52)
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

(define-public rust-windows-tokens-0.32
  (package
    (inherit rust-windows-tokens-0.39)
    (name "rust-windows-tokens")
    (version "0.32.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-tokens" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rrqbxjkyk6h6p6jjzbcxr0mhqbz0yfndd2s2dsgmbl75f4yy7gn"))))))

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
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zdy4qn178sil5sdm63lm7f0kkcjg6gvdwmcprd2yjmwn8ns6vrx"))
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
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17lllq4l2k1lqgcnw1cccphxp9vs7inq99kjlm2lfl9zklg7wr8s"))
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
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "012wfq37f18c09ij5m6rniw7xxn5fcvrxbqd0wd8vgnl3hfn9yfz"))
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

