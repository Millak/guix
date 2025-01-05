;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (gnu packages crates-shell)
  #:use-module (gnu packages c)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-database)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))


(define-public rust-nu-ansi-term-0.50
  (package
    (name "rust-nu-ansi-term")
    (version "0.50.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-ansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16a3isvbxx8pa3lk71h3cq2fsx2d17zzq42j4mhpxy81gl2qx8nl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/nushell/nu-ansi-term")
    (synopsis "Library for ANSI terminal colors and styles (bold, underline)")
    (description
     "This package is a library for ANSI terminal colors and styles (bold,
underline).")
    (license license:expat)))

(define-public rust-nu-ansi-term-0.49
  (package
    (inherit rust-nu-ansi-term-0.50)
    (name "rust-nu-ansi-term")
    (version "0.49.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-ansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2svfnircd9jp06wk55qcbb9v5cadkfcjfg99vm21qdjg0x6wy0"))))
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1)
        ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-nu-ansi-term-0.46
  (package
    (inherit rust-nu-ansi-term-0.49)
    (name "rust-nu-ansi-term")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nu-ansi-term" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))))
    (arguments
     `(#:cargo-inputs
       (("rust-overload" ,rust-overload-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-nu-cli-0.101
  (package
    (name "rust-nu-cli")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nfyfbgjdgkn1vs1qi4xfyc8b7czsprv5bdpn2fvr44ynqw8njiw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--"
                            "--skip=repl::are_session_ids_in_sync"
                            "--skip=repl::test_auto_cd::auto_cd_tilde"
                            "--skip=commands"
                            "--skip=completions")
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crossterm" ,rust-crossterm-0.28)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.14)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-is-executable" ,rust-is-executable-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.101)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.101)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-path" ,rust-nu-path-0.101)
                       ("rust-nu-plugin-engine" ,rust-nu-plugin-engine-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-reedline" ,rust-reedline-0.38)
                       ("rust-sysinfo" ,rust-sysinfo-0.32)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-which" ,rust-which-7))
       #:cargo-development-inputs
       (("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.101)
        ("rust-nu-command" ,rust-nu-command-0.101)
        ("rust-nu-test-support" ,rust-nu-test-support-0.101)
        ("rust-rstest" ,rust-rstest-0.23)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-cli")
    (synopsis "CLI-related functionality for Nushell")
    (description "This package provides CLI-related functionality for Nushell.")
    (license license:expat)))

(define-public rust-nu-cmd-base-0.101
  (package
    (name "rust-nu-cmd-base")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-base" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06x1rz652jwkvf2nf2r9q7pawm7nv2sdz17b6ihhlai3l3mjr896"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-path" ,rust-nu-path-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-base")
    (synopsis "Foundation tools to build Nushell commands")
    (description
     "This package provides the foundation tools to build Nushell commands.")
    (license license:expat)))

(define-public rust-nu-cmd-extra-0.101
  (package
    (name "rust-nu-cmd-extra")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-extra" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1salamvy8cckv600w8jyr7i47g7hg51dybk5gbjw73bzlwfn1v8m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--"
                            "--skip=commands::bits"
                            "--skip=commands::bytes")
       #:cargo-inputs (("rust-fancy-regex" ,rust-fancy-regex-0.14)
                       ("rust-heck" ,rust-heck-0.5)
                       ("rust-itertools" ,rust-itertools-0.13)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.101)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-json" ,rust-nu-json-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-pretty-hex" ,rust-nu-pretty-hex-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rust-embed" ,rust-rust-embed-8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-v-htmlescape" ,rust-v-htmlescape-0.15))
       #:cargo-development-inputs
       (("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.101)
        ("rust-nu-command" ,rust-nu-command-0.101)
        ("rust-nu-test-support" ,rust-nu-test-support-0.101))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-extra")
    (synopsis "Nushell's extra commands")
    (description "This package contains nushell's extra commands that are not
part of the 1.0 api standard.")
    (license license:expat)))

(define-public rust-nu-cmd-lang-0.101
  (package
    (name "rust-nu-cmd-lang")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-lang" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f1lpk4c93ijw9hfkb2p9izxs1c0zcndbq6iy7ir30p8n8m645k1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-itertools" ,rust-itertools-0.13)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-shadow-rs" ,rust-shadow-rs-0.37))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-lang")
    (synopsis "Nushell's core language commands")
    (description "This package provides nushell's core language commands.")
    (license license:expat)))

(define-public rust-nu-cmd-plugin-0.101
  (package
    (name "rust-nu-cmd-plugin")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-plugin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fy7nl176ms5l7wshad2wky7f0afzz9ay2p15l0dr88g39mm03mh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-itertools" ,rust-itertools-0.13)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-path" ,rust-nu-path-0.101)
                       ("rust-nu-plugin-engine" ,rust-nu-plugin-engine-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-plugin")
    (synopsis "Commands for managing Nushell plugins")
    (description
     "This package provides commands for managing Nushell plugins.")
    (license license:expat)))

(define-public rust-nu-color-config-0.101
  (package
    (name "rust-nu-color-config")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-color-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ral2glvvqfrvmhgh8v13bdv3lix017hgc9ka1gsxaf2fnacrg0b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--"
         "--skip=style_computer::test_computable_style_closure_basic"
         "--skip=style_computer::test_computable_style_closure_errors")
       #:cargo-inputs (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-json" ,rust-nu-json-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-nu-test-support" ,rust-nu-test-support-0.101))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-color-config")
    (synopsis "Color configuration code used by Nushell")
    (description "This package contains color configuration code used by Nushell.")
    (license license:expat)))

(define-public rust-nu-command-0.101
  (package
    (name "rust-nu-command")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10j6krmkjcj8avylf8yq81ja8dhg9d496nnb345l1nj70m330g63"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-alphanumeric-sort" ,rust-alphanumeric-sort-1)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bracoxide" ,rust-bracoxide-0.1)
                       ("rust-brotli" ,rust-brotli-6)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-calamine" ,rust-calamine-0.26)
                       ("rust-chardetng" ,rust-chardetng-0.1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-humanize" ,rust-chrono-humanize-0.2)
                       ("rust-chrono-tz" ,rust-chrono-tz-0.10)
                       ("rust-crossterm" ,rust-crossterm-0.28)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-dialoguer" ,rust-dialoguer-0.11)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-dtparse" ,rust-dtparse-2)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.14)
                       ("rust-filesize" ,rust-filesize-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-human-date-parser" ,rust-human-date-parser-0.2)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-itertools" ,rust-itertools-0.13)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-multipart-rs" ,rust-multipart-rs-0.1)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-notify-debouncer-full" ,rust-notify-debouncer-full-0.3)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.101)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.101)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-glob" ,rust-nu-glob-0.101)
                       ("rust-nu-json" ,rust-nu-json-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-path" ,rust-nu-path-0.101)
                       ("rust-nu-pretty-hex" ,rust-nu-pretty-hex-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-system" ,rust-nu-system-0.101)
                       ("rust-nu-table" ,rust-nu-table-0.101)
                       ("rust-nu-term-grid" ,rust-nu-term-grid-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-nuon" ,rust-nuon-0.101)
                       ("rust-oem-cp" ,rust-oem-cp-2)
                       ("rust-open" ,rust-open-5)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-print-positions" ,rust-print-positions-0.6)
                       ("rust-procfs" ,rust-procfs-0.17)
                       ("rust-quick-xml" ,rust-quick-xml-0.37)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rmp" ,rust-rmp-0.8)
                       ("rust-roxmltree" ,rust-roxmltree-0.20)
                       ("rust-rusqlite" ,rust-rusqlite-0.31)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-sysinfo" ,rust-sysinfo-0.32)
                       ("rust-tabled" ,rust-tabled-0.16)
                       ("rust-titlecase" ,rust-titlecase-3)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-trash" ,rust-trash-5)
                       ("rust-umask" ,rust-umask-2)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.2)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-url" ,rust-url-2)
                       ("rust-uu-cp" ,rust-uu-cp-0.0.28)
                       ("rust-uu-mkdir" ,rust-uu-mkdir-0.0.28)
                       ("rust-uu-mktemp" ,rust-uu-mktemp-0.0.28)
                       ("rust-uu-mv" ,rust-uu-mv-0.0.28)
                       ("rust-uu-touch" ,rust-uu-touch-0.0.28)
                       ("rust-uu-uname" ,rust-uu-uname-0.0.28)
                       ("rust-uu-whoami" ,rust-uu-whoami-0.0.28)
                       ("rust-uucore" ,rust-uucore-0.0.28)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-v-htmlescape" ,rust-v-htmlescape-0.15)
                       ("rust-wax" ,rust-wax-0.6)
                       ("rust-which" ,rust-which-7)
                       ("rust-windows" ,rust-windows-0.56)
                       ("rust-winreg" ,rust-winreg-0.52))
       #:cargo-development-inputs
       (("rust-dirs" ,rust-dirs-5)
        ("rust-mockito" ,rust-mockito-1)
        ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.101)
        ("rust-nu-test-support" ,rust-nu-test-support-0.101)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3)
        ("rust-rstest" ,rust-rstest-0.23)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-command")
    (synopsis "Nushell's built-in commands")
    (description "This package contains nushell's built-in commands.")
    (license license:expat)))

(define-public rust-nu-derive-value-0.101
  (package
    (name "rust-nu-derive-value")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-derive-value" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07rd7jl4f8y77nfzinlc7fwmlim8lzacmi7pqrz5d2m8dgnwixvi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-heck" ,rust-heck-0.5)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-derive-value")
    (synopsis "Macros implementation of #[derive(FromValue, IntoValue)]")
    (description
     "This package provides macros with an implementation of
#[derive(@code{FromValue}, @code{IntoValue})].")
    (license license:expat)))

(define-public rust-nu-engine-0.101
  (package
    (name "rust-nu-engine")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-engine" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "054vlid5xwv56283nw2fr5q874ga5r9z98x0kq50d0fj7rc1jrjw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-nu-glob" ,rust-nu-glob-0.101)
                       ("rust-nu-path" ,rust-nu-path-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-terminal-size" ,rust-terminal-size-0.4))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-engine")
    (synopsis "Nushell's evaluation engine")
    (description "This package provides nushell's evaluation engine.")
    (license license:expat)))

(define-public rust-nu-explore-0.101
  (package
    (name "rust-nu-explore")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-explore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ah81jh3zrcfffjbc0wyncscpvh650nl9sq2aj2jnmngwwir47nc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ansi-str" ,rust-ansi-str-0.8)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-crossterm" ,rust-crossterm-0.28)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.101)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-json" ,rust-nu-json-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-path" ,rust-nu-path-0.101)
                       ("rust-nu-pretty-hex" ,rust-nu-pretty-hex-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-table" ,rust-nu-table-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-ratatui" ,rust-ratatui-0.26)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-unicode-width" ,rust-unicode-width-0.2))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-explore")
    (synopsis "Nushell table pager")
    (description "This package contains the nushell table pager.")
    (license license:expat)))

(define-public rust-nu-glob-0.101
  (package
    (name "rust-nu-glob")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q3np4fgxp8lgfawn8rqv4cw3jpr668g2phza34csaj1d7zakl5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-glob")
    (synopsis "Support for matching file paths against Unix shell style patterns")
    (description
     "This package provides support for matching file paths against Unix shell
style patterns.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nu-json-0.101
  (package
    (name "rust-nu-json")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l0kj7dyg1kpy4qrl5hvmkp0lxm4w206mgg8227z0gsvl2936zd8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--" "--skip=test_hjson")
       #:cargo-inputs (("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs
       (("rust-fancy-regex" ,rust-fancy-regex-0.14)
        ("rust-nu-path" ,rust-nu-path-0.101)
        ("rust-nu-test-support" ,rust-nu-test-support-0.101)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-json")
    (synopsis "Human JSON (Hjson) serialization file format")
    (description "This crate is a Rust library for parsing and generating Human
JSON Hjson.  It is built upon Serde, a high performance generic serialization
framework.")
    (license license:expat)))

(define-public rust-nu-lsp-0.101
  (package
    (name "rust-nu-lsp")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-lsp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c4wp44251qczc0w2dbn55q2i0j0n4xrfjaxh5rzj451vzxjc7yd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-lsp-server" ,rust-lsp-server-0.7)
                       ("rust-lsp-types" ,rust-lsp-types-0.95)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-cli" ,rust-nu-cli-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-reedline" ,rust-reedline-0.38)
                       ("rust-ropey" ,rust-ropey-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs
       (("rust-assert-json-diff" ,rust-assert-json-diff-2)
        ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.101)
        ("rust-nu-command" ,rust-nu-command-0.101)
        ("rust-nu-test-support" ,rust-nu-test-support-0.101))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-lsp")
    (synopsis "Nushell's integrated LSP server")
    (description "This package contains nushell'e integrated LSP server.")
    (license license:expat)))

(define-public rust-nu-parser-0.101
  (package
    (name "rust-nu-parser")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13dmw30vw71zkdbdzf541zbsk0can0ndzkl6azhkpj3vpmv2ip1a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytesize" ,rust-bytesize-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-itertools" ,rust-itertools-0.13)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-path" ,rust-nu-path-0.101)
                       ("rust-nu-plugin-engine" ,rust-nu-plugin-engine-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs (("rust-rstest" ,rust-rstest-0.23))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-parser")
    (synopsis "Nushell's parser")
    (description "This package contains nushell's parser")
    (license license:expat)))

(define-public rust-nu-path-0.101
  (package
    (name "rust-nu-path")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-path" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gsjnp81padgp9k95bnha5kkilfq0yxpm00splc93mvhyfx1pk9w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dirs" ,rust-dirs-5)
                       ("rust-omnipath" ,rust-omnipath-0.1)
                       ("rust-pwd" ,rust-pwd-1))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-path")
    (synopsis "Path handling library for Nushell")
    (description "This package provides path handling library for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-0.101
  (package
    (name "rust-nu-plugin")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-plugin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g2bpmibd0k125vnicfr10ndpyms3f3rdplk5wjh6a2l3450yq7b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-plugin-core" ,rust-nu-plugin-core-0.101)
                       ("rust-nu-plugin-protocol" ,rust-nu-plugin-protocol-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-thiserror" ,rust-thiserror-2))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1)
                                   ("rust-typetag" ,rust-typetag-0.2))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-plugin")
    (synopsis "Functionality for building Nushell plugins")
    (description
     "This package contains functionality for building Nushell plugins.")
    (license license:expat)))

(define-public rust-nu-plugin-core-0.101
  (package
    (name "rust-nu-plugin-core")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-plugin-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0axa72lfpdsci3f0yfd52cf1xdgrz3b0rb219495ifyc3h5xc7xh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-interprocess" ,rust-interprocess-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nu-plugin-protocol" ,rust-nu-plugin-protocol-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-windows" ,rust-windows-0.56))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-plugin-core")
    (synopsis "Shared internal functionality to support Nushell plugins")
    (description
     "This package provides shared internal functionality to support
Nushell plugins.")
    (license license:expat)))

(define-public rust-nu-plugin-engine-0.101
  (package
    (name "rust-nu-plugin-engine")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-plugin-engine" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14anf95z6fwyajnr0blp7hd1ah05vdjq0ypvrhfvi9qk07f6yxyg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-plugin-core" ,rust-nu-plugin-core-0.101)
                       ("rust-nu-plugin-protocol" ,rust-nu-plugin-protocol-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-system" ,rust-nu-system-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows" ,rust-windows-0.56))
       #:cargo-development-inputs (("rust-typetag" ,rust-typetag-0.2))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-plugin-engine")
    (synopsis "Functionality for running Nushell plugins from a Nushell engine")
    (description
     "This package provides functionality for running Nushell plugins from a
Nushell engine.")
    (license license:expat)))

(define-public rust-nu-plugin-protocol-0.101
  (package
    (name "rust-nu-plugin-protocol")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-plugin-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s18gqkjjxc9jq9ivrqrp9cdq9a4kngd6ki87iv5r6bgwg1xbjqz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-typetag" ,rust-typetag-0.2))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-plugin-protocol")
    (synopsis "Protocol type definitions for Nushell plugins")
    (description
     "This package provides protocol type definitions for Nushell plugins.")
    (license license:expat)))

(define-public rust-nu-plugin-test-support-0.101
  (package
    (name "rust-nu-plugin-test-support")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-plugin-test-support" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17r036lxhscs05hpc4q0bz3prcqq3k5wz9xqmrca44cwyb0wilcm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.101)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-plugin" ,rust-nu-plugin-0.101)
                       ("rust-nu-plugin-core" ,rust-nu-plugin-core-0.101)
                       ("rust-nu-plugin-engine" ,rust-nu-plugin-engine-0.101)
                       ("rust-nu-plugin-protocol" ,rust-nu-plugin-protocol-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-similar" ,rust-similar-2))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1)
                                   ("rust-typetag" ,rust-typetag-0.2))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-plugin-test-support")
    (synopsis "Testing support for Nushell plugins")
    (description "This package provides Testing support for Nushell plugins.")
    (license license:expat)))

(define-public rust-nu-pretty-hex-0.101
  (package
    (name "rust-nu-pretty-hex")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-pretty-hex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00bxf27y31kr5w8hh2v65hrs09b5r53yilwjjwbzxn7hpmpra5fg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50))
       #:cargo-development-inputs
       (("rust-heapless" ,rust-heapless-0.8)
        ("rust-rand" ,rust-rand-0.8))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-pretty-hex")
    (synopsis "Pretty hex dump of bytes slice in the common style")
    (description
     "This package provides pretty hex dump of bytes slice in the common style.")
    (license license:expat)))

(define-public rust-nu-protocol-0.101
  (package
    (name "rust-nu-protocol")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fr7jbzapqr68z5v7bvhgxkj697lqy1l3lkgykbk09b3bcwrmx3j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--"
         ;; Not all files included.
         "--skip=config_add_unsupported_key"
         "--skip=config_add_unsupported_type"
         "--skip=config_add_unsupported_value"
         "--skip=config_affected_when_deep_mutated"
         "--skip=config_affected_when_mutated"
         "--skip=config_is_mutable"
         "--skip=config_preserved_after_do"
         "--skip=fancy_default_errors"
         "--skip=filesize_format_auto_metric_false"
         "--skip=filesize_format_auto_metric_true"
         "--skip=filesize_metric_false"
         "--skip=filesize_metric_overrides_format"
         "--skip=filesize_metric_true"
         "--skip=narratable_errors"
         "--skip=plugins")
       #:cargo-inputs (("rust-brotli" ,rust-brotli-6)
                       ("rust-byte-unit" ,rust-byte-unit-5)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-humanize" ,rust-chrono-humanize-0.2)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-dirs-sys" ,rust-dirs-sys-0.4)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.14)
                       ("rust-heck" ,rust-heck-0.5)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lru" ,rust-lru-0.12)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-nu-derive-value" ,rust-nu-derive-value-0.101)
                       ("rust-nu-path" ,rust-nu-path-0.101)
                       ("rust-nu-system" ,rust-nu-system-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-typetag" ,rust-typetag-0.2)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs
       (("rust-nu-test-support" ,rust-nu-test-support-0.101)
        ("rust-nu-utils" ,rust-nu-utils-0.101)
        ("rust-os-pipe" ,rust-os-pipe-1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-rstest" ,rust-rstest-0.23)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-strum" ,rust-strum-0.26)
        ("rust-strum-macros" ,rust-strum-macros-0.26)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-protocol")
    (synopsis "Nushell's internal protocols, including its abstract syntax tree")
    (description
     "This package provides nushell's internal protocols, including its abstract
syntax tree.")
    (license license:expat)))

(define-public rust-nu-std-0.101
  (package
    (name "rust-nu-std")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-std" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "078bald5fn2np7hj9vd8xyi02al8z79qiapj6jdfnicaf7jlqzb8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-std")
    (synopsis "Standard library of Nushell")
    (description "This package provides the standard library of Nushell.")
    (license license:expat)))

(define-public rust-nu-system-0.101
  (package
    (name "rust-nu-system")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-system" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xrh2vybh7vhyhcw987mjmnx12g3g07y9n24p1md2pdxciz2y641"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-itertools" ,rust-itertools-0.13)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libproc" ,rust-libproc-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-ntapi" ,rust-ntapi-0.4)
                       ("rust-procfs" ,rust-procfs-0.17)
                       ("rust-sysinfo" ,rust-sysinfo-0.32)
                       ("rust-windows" ,rust-windows-0.56))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-system")
    (synopsis "Nushell system querying")
    (description "This package provides Nushell system querying.")
    (license license:expat)))

(define-public rust-nu-table-0.101
  (package
    (name "rust-nu-table")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-table" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pf6l401inns1w9x8wd3s3c7ymqkd1qrh94b0cp8q5hi431vr7my"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fancy-regex" ,rust-fancy-regex-0.14)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.101)
                       ("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-tabled" ,rust-tabled-0.16)
                       ("rust-terminal-size" ,rust-terminal-size-0.4))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-table")
    (synopsis "Nushell table printing")
    (description "This package provides nushell table printing.")
    (license license:expat)))

(define-public rust-nu-term-grid-0.101
  (package
    (name "rust-nu-term-grid")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-term-grid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q60ginwdpqqd862nb407km3yx7x4ql4lhb8y6ly1ldwzj1x9lbn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-unicode-width" ,rust-unicode-width-0.2))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-term-grid")
    (synopsis "Nushell grid printing")
    (description "This package provides nushell grid printing.")
    (license license:expat)))

(define-public rust-nu-test-support-0.101
  (package
    (name "rust-nu-test-support")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-test-support" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jb0nr252q5pl7jkkvhpzj06j22r059mda5mid9fqm3rhka4dyky"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--"
         ;; Not all files included.
         "--skip=playground::tests::current_working_directory_back_to_root_from_anywhere"
         "--skip=playground::tests::current_working_directory_in_sandbox_directory_created")
       #:cargo-inputs (("rust-nu-glob" ,rust-nu-glob-0.101)
                       ("rust-nu-path" ,rust-nu-path-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-which" ,rust-which-7))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-test-support")
    (synopsis "Support for writing Nushell tests")
    (description "This package contains support for writing Nushell tests.")
    (license license:expat)))

(define-public rust-nu-utils-0.101
  (package
    (name "rust-nu-utils")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i9png2l28qjbwa0v8yp09sxhdqss7rl82y9affjvqg6m27ldlak"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossterm" ,rust-crossterm-0.28)
                       ("rust-crossterm-winapi" ,rust-crossterm-winapi-0.9)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-sys-locale" ,rust-sys-locale-0.3)
                       ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-utils")
    (synopsis "Nushell utility functions")
    (description "This package contains utility functions for nushell.")
    (license license:expat)))

(define-public rust-nuon-0.101
  (package
    (name "rust-nuon")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nuon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j3pd7nwk150krakqjh5jbyqsnzd4ywsyjm50kgrg0mf9flihz28"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-nu-engine" ,rust-nu-engine-0.101)
                       ("rust-nu-parser" ,rust-nu-parser-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-nu-utils" ,rust-nu-utils-0.101))
       #:cargo-development-inputs (("rust-chrono" ,rust-chrono-0.4))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nuon")
    (synopsis "Support for the NUON format")
    (description "This package provides support for the NUON format.")
    (license license:expat)))

(define-public rust-reedline-0.38
  (package
    (name "rust-reedline")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "reedline" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wq9j74bs306qyh7bnnj6fj44w1xf0abiayq6v4rdhw4mnq8rylv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arboard" ,rust-arboard-3)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crossbeam" ,rust-crossbeam-0.8)
                       ("rust-crossterm" ,rust-crossterm-0.28)
                       ("rust-fd-lock" ,rust-fd-lock-4)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-rusqlite" ,rust-rusqlite-0.31)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-strum" ,rust-strum-0.26)
                       ("rust-strum-macros" ,rust-strum-macros-0.26)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))
       #:cargo-development-inputs
       (("rust-gethostname" ,rust-gethostname-0.4)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-rstest" ,rust-rstest-0.23)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/nushell/reedline")
    (synopsis "Readline-like crate for CLI text input")
    (description
     "This package provides a readline-like crate for CLI text input.")
    (license license:expat)))

(define-public rust-spinners-4
  (package
    (name "rust-spinners")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "spinners" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10jgzdy6x79ipnfhavn46zbg4hlx98mcfr7p4f4j774b6mzr9vx0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-maplit" ,rust-maplit-1)
                       ("rust-strum" ,rust-strum-0.24))))
    (home-page "https://github.com/fgribreau/spinners")
    (synopsis "60+ Elegant terminal spinners for Rust")
    (description "This package provides 60+ Elegant terminal spinners for Rust.")
    (license license:expat)))
