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

(define-public rust-nu-ansi-term-0.44
  (package
    (inherit rust-nu-ansi-term-0.49)
    (name "rust-nu-ansi-term")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-ansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lmc9rdqnw586gv4a0c2gbg3x4a04fy65xk3fczby8lq84rz41i3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-overload" ,rust-overload-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-nu-cli-0.91
  (package
    (name "rust-nu-cli")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v7xz13n5gzhwkn4d8wq1v22zbdvfqd2zxnqf2q3k8cnnj51ql63"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; 42 of 45 of the completions tests fail.
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crossterm" ,rust-crossterm-0.27)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-is-executable" ,rust-is-executable-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.91)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-reedline" ,rust-reedline-0.30)
                       ("rust-sysinfo" ,rust-sysinfo-0.30)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-which" ,rust-which-6))
       #:cargo-development-inputs
       (("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-command" ,rust-nu-command-0.91)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91)
        ("rust-rstest" ,rust-rstest-0.18))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-cli")
    (synopsis "CLI-related functionality for Nushell")
    (description "This package provides CLI-related functionality for Nushell.")
    (license license:expat)))

(define-public rust-nu-cmd-base-0.91
  (package
    (name "rust-nu-cmd-base")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-base" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i2bdvhl1qmpzrip4b45xr4vg0himfsi120xq5al9vs5y80x2lla"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-base")
    (synopsis "Foundation tools to build Nushell commands")
    (description
     "This package provides the foundation tools to build Nushell commands.")
    (license license:expat)))

(define-public rust-nu-cmd-dataframe-0.91
  (package
    (name "rust-nu-cmd-dataframe")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-dataframe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1avsx50kr0snbm62l91v7a7wfq05igv5aagwhczm1g4xdpl448x9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-tz" ,rust-chrono-tz-0.8)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-num" ,rust-num-0.4)
                       ("rust-polars" ,rust-polars-0.37)
                       ("rust-polars-arrow" ,rust-polars-arrow-0.37)
                       ("rust-polars-io" ,rust-polars-io-0.37)
                       ("rust-polars-ops" ,rust-polars-ops-0.37)
                       ("rust-polars-plan" ,rust-polars-plan-0.37)
                       ("rust-polars-utils" ,rust-polars-utils-0.37)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sqlparser" ,rust-sqlparser-0.43))
       #:cargo-development-inputs (("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-dataframe")
    (synopsis "Nushell's dataframe commands based on polars")
    (description
     "This package contains nushell's dataframe commands based on polars.")
    (license license:expat)))

(define-public rust-nu-cmd-extra-0.91
  (package
    (name "rust-nu-cmd-extra")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-extra" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x905m6yci5n3ir89arq7vdvx0czqpjvr3j8i32f7bqh0z3jisc3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-heck" ,rust-heck-0.4)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-json" ,rust-nu-json-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-pretty-hex" ,rust-nu-pretty-hex-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rust-embed" ,rust-rust-embed-8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-v-htmlescape" ,rust-v-htmlescape-0.15))
       #:cargo-development-inputs
       (("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-command" ,rust-nu-command-0.91)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-extra")
    (synopsis "Nushell's extra commands")
    (description "This package contains nushell's extra commands that are not
part of the 1.0 api standard.")
    (license license:expat)))

(define-public rust-nu-cmd-lang-0.91
  (package
    (name "rust-nu-cmd-lang")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-lang" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zwdw3y4jn6s6h41jnwljpj9cfyhr68av61idikjkhi7l6hygy5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-itertools" ,rust-itertools-0.12)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-shadow-rs" ,rust-shadow-rs-0.26))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-lang")
    (synopsis "Nushell's core language commands")
    (description "This package provides nushell's core language commands.")
    (license license:expat)))

(define-public rust-nu-color-config-0.91
  (package
    (name "rust-nu-color-config")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-color-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "115bha7r4sy19w80vbbfc3av9g0pa1fcksdaqznm0yjlykv49czy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=style_computer::test_computable_style_closure_basic"
         "--skip=style_computer::test_computable_style_closure_errors")
       #:cargo-inputs (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-json" ,rust-nu-json-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-nu-test-support" ,rust-nu-test-support-0.91))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-color-config")
    (synopsis "Color configuration code used by Nushell")
    (description "This package contains color configuration code used by Nushell.")
    (license license:expat)))

(define-public rust-nu-command-0.91
  (package
    (name "rust-nu-command")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jiz6bndkwfnhs4cc74am8krnhyb5kyq310nf7ma5038q6vqs8q9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-alphanumeric-sort" ,rust-alphanumeric-sort-1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bracoxide" ,rust-bracoxide-0.1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-calamine" ,rust-calamine-0.24)
                       ("rust-chardetng" ,rust-chardetng-0.1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-humanize" ,rust-chrono-humanize-0.2)
                       ("rust-chrono-tz" ,rust-chrono-tz-0.8)
                       ("rust-crossterm" ,rust-crossterm-0.27)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-dialoguer" ,rust-dialoguer-0.11)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-dtparse" ,rust-dtparse-2)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-filesize" ,rust-filesize-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-human-date-parser" ,rust-human-date-parser-0.1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-nix" ,rust-nix-0.27)
                       ("rust-notify-debouncer-full" ,rust-notify-debouncer-full-0.3)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.91)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-glob" ,rust-nu-glob-0.91)
                       ("rust-nu-json" ,rust-nu-json-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-pretty-hex" ,rust-nu-pretty-hex-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-system" ,rust-nu-system-0.91)
                       ("rust-nu-table" ,rust-nu-table-0.91)
                       ("rust-nu-term-grid" ,rust-nu-term-grid-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-open" ,rust-open-5)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-print-positions" ,rust-print-positions-0.6)
                       ("rust-procfs" ,rust-procfs-0.16)
                       ("rust-quick-xml" ,rust-quick-xml-0.31)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-roxmltree" ,rust-roxmltree-0.19)
                       ("rust-rusqlite" ,rust-rusqlite-0.31)
                       ("rust-same-file" ,rust-same-file-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-sysinfo" ,rust-sysinfo-0.30)
                       ("rust-tabled" ,rust-tabled-0.14)
                       ("rust-terminal-size" ,rust-terminal-size-0.3)
                       ("rust-titlecase" ,rust-titlecase-2)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-trash" ,rust-trash-3)
                       ("rust-umask" ,rust-umask-2)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-url" ,rust-url-2)
                       ("rust-uu-cp" ,rust-uu-cp-0.0.23)
                       ("rust-uu-mkdir" ,rust-uu-mkdir-0.0.23)
                       ("rust-uu-mktemp" ,rust-uu-mktemp-0.0.23)
                       ("rust-uu-mv" ,rust-uu-mv-0.0.23)
                       ("rust-uu-whoami" ,rust-uu-whoami-0.0.23)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-v-htmlescape" ,rust-v-htmlescape-0.15)
                       ("rust-wax" ,rust-wax-0.6)
                       ("rust-which" ,rust-which-6)
                       ("rust-windows" ,rust-windows-0.52)
                       ("rust-winreg" ,rust-winreg-0.52))
       #:cargo-development-inputs
       (("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-mockito" ,rust-mockito-1)
        ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-rstest" ,rust-rstest-0.18))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-command")
    (synopsis "Nushell's built-in commands")
    (description "This package contains nushell's built-in commands.")
    (license license:expat)))

(define-public rust-nu-engine-0.91
  (package
    (name "rust-nu-engine")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-engine" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j4g3nhg9yw7nilnf3n1k4yfn5glmd3vbap1zxwzz24xw7ap62c7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-glob" ,rust-nu-glob-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-engine")
    (synopsis "Nushell's evaluation engine")
    (description "This package provides nushell's evaluation engine.")
    (license license:expat)))

(define-public rust-nu-explore-0.91
  (package
    (name "rust-nu-explore")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-explore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j1xry4idjxrnyfz9l227s5hd82635dqc72gyw4zwq35izjrgqmm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ansi-str" ,rust-ansi-str-0.8)
                       ("rust-crossterm" ,rust-crossterm-0.27)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-json" ,rust-nu-json-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-table" ,rust-nu-table-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-ratatui" ,rust-ratatui-0.26)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-terminal-size" ,rust-terminal-size-0.3)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-explore")
    (synopsis "Nushell table pager")
    (description "This package contains the nushell table pager.")
    (license license:expat)))

(define-public rust-nu-glob-0.91
  (package
    (name "rust-nu-glob")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "094jkfb7rlcl0dxs5gnw8x30zv75s372l72zsg1wmv8lblzbfybx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=test::test_iteration_errors")
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-glob")
    (synopsis "Support for matching file paths against Unix shell style patterns")
    (description
     "This package provides support for matching file paths against Unix shell
style patterns.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nu-json-0.91
  (package
    (name "rust-nu-json")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ga6kmmavd3rxjkk3j7jm6kjg2ny066a713ccca9nj0i9gbm6b1h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-json")
    (synopsis "Human JSON (Hjson) serialization file format")
    (description "This crate is a Rust library for parsing and generating Human
JSON Hjson.  It is built upon Serde, a high performance generic serialization
framework.")
    (license license:expat)))

(define-public rust-nu-lsp-0.91
  (package
    (name "rust-nu-lsp")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-lsp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sj61lnw74jrd7az9b5367gk4qry06s783k5vqgjznx4nqvr80xj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=diagnostics::tests::publish_diagnostics_fixed_unknown_variable"
         "--skip=diagnostics::tests::publish_diagnostics_variable_does_not_exists"
         "--skip=notification::tests::hover_correct_documentation_on_let"
         "--skip=notification::tests::hover_on_command_after_full_content_change"
         "--skip=notification::tests::hover_on_command_after_partial_content_change"
         "--skip=notification::tests::open_document_with_utf_char"
         "--skip=tests::complete_command_with_space"
         "--skip=tests::complete_command_with_utf_line"
         "--skip=tests::complete_on_variable"
         "--skip=tests::goto_definition_of_command"
         "--skip=tests::goto_definition_of_command_parameter"
         "--skip=tests::goto_definition_of_variable"
         "--skip=tests::hover_on_command"
         "--skip=tests::hover_on_variable")
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-lsp-server" ,rust-lsp-server-0.7)
                       ("rust-lsp-types" ,rust-lsp-types-0.95)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-cli" ,rust-nu-cli-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-reedline" ,rust-reedline-0.30)
                       ("rust-ropey" ,rust-ropey-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs
       (("rust-assert-json-diff" ,rust-assert-json-diff-2)
        ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-command" ,rust-nu-command-0.91)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-lsp")
    (synopsis "Nushell's integrated LSP server")
    (description "This package contains nushell'e integrated LSP server.")
    (license license:expat)))

(define-public rust-nu-parser-0.91
  (package
    (name "rust-nu-parser")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "110jgz6lr0bi6904k63yqbsrcgfrpn044j2xz9if8akprk4p5w4z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytesize" ,rust-bytesize-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-plugin" ,rust-nu-plugin-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs (("rust-rstest" ,rust-rstest-0.18))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-parser")
    (synopsis "Nushell's parser")
    (description "This package contains nushell's parser")
    (license license:expat)))

(define-public rust-nu-path-0.91
  (package
    (name "rust-nu-path")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-path" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "146lm48vna9w5kr46dclqmzl1cbz5k7j1zz6jl8i6d83np4nn1sa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dirs-next" ,rust-dirs-next-2)
                       ("rust-omnipath" ,rust-omnipath-0.1)
                       ("rust-pwd" ,rust-pwd-1))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-path")
    (synopsis "Path handling library for Nushell")
    (description "This package provides path handling library for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-0.91
  (package
    (name "rust-nu-plugin")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-plugin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "054hmmz78njl6qhpcbbifip5r879ipa2j3y5ndlj588b9qaijvva"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bincode" ,rust-bincode-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-typetag" ,rust-typetag-0.2))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-plugin")
    (synopsis "Functionality for building Nushell plugins")
    (description
     "This package contains functionality for building Nushell plugins.")
    (license license:expat)))

(define-public rust-nu-pretty-hex-0.91
  (package
    (name "rust-nu-pretty-hex")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-pretty-hex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1iq8amp5hqf2xxp5n74l5sgqv2bj204zwbjcnarhy88ijzjicrl6"))))
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

(define-public rust-nu-protocol-0.91
  (package
    (name "rust-nu-protocol")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h6ikglmx79w5izcb7jv66s7cdsq6302pgwzyjyaxyw8jyprvx2g"))))
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
       #:cargo-inputs (("rust-byte-unit" ,rust-byte-unit-5)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-humanize" ,rust-chrono-humanize-0.2)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-lru" ,rust-lru-0.12)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-system" ,rust-nu-system-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-typetag" ,rust-typetag-0.2))
       #:cargo-development-inputs
       (("rust-nu-test-support" ,rust-nu-test-support-0.91)
        ("rust-rstest" ,rust-rstest-0.18)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-strum" ,rust-strum-0.25)
        ("rust-strum-macros" ,rust-strum-macros-0.26))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-protocol")
    (synopsis "Nushell's internal protocols, including its abstract syntax tree")
    (description
     "This package provides nushell's internal protocols, including its abstract
syntax tree.")
    (license license:expat)))

(define-public rust-nu-std-0.91
  (package
    (name "rust-nu-std")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-std" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1igdid80qbfgqdmcg6szq2rsi7i5qlyhplw74byh81vkqsn5z74w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-miette" ,rust-miette-7)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-std")
    (synopsis "Standard library of Nushell")
    (description "This package provides the standard library of Nushell.")
    (license license:expat)))

(define-public rust-nu-system-0.91
  (package
    (name "rust-nu-system")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-system" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jq87rjbmgpkf2cr2ajfs12f3wzpsh43m0drmrgj7b8lk2g6q9by"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libproc" ,rust-libproc-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-nix" ,rust-nix-0.27)
                       ("rust-ntapi" ,rust-ntapi-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-procfs" ,rust-procfs-0.16)
                       ("rust-sysinfo" ,rust-sysinfo-0.30)
                       ("rust-windows" ,rust-windows-0.52))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-system")
    (synopsis "Nushell system querying")
    (description "Nushell system querying")
    (license license:expat)))

(define-public rust-nu-table-0.91
  (package
    (name "rust-nu-table")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-table" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r3b0lvkmjfkkcpxq6pls0sc5jp08a25ykfi0hifn0lsb9nady9m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-tabled" ,rust-tabled-0.14))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-table")
    (synopsis "Nushell table printing")
    (description "This package provides nushell table printing.")
    (license license:expat)))

(define-public rust-nu-term-grid-0.91
  (package
    (name "rust-nu-term-grid")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-term-grid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12f0i7m6flpkf1valkjfg6chalifpb65cknq91p22sii4dx0x89r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-term-grid")
    (synopsis "Nushell grid printing")
    (description "This package provides nushell grid printing.")
    (license license:expat)))

(define-public rust-nu-test-support-0.91
  (package
    (name "rust-nu-test-support")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-test-support" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "146ncw3318hcbhb7cpz3bdz0ypd8x4cpzhhl0q55r2mxxci7ik38"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=playground::tests::current_working_directory_back_to_root_from_anywhere"
         "--skip=playground::tests::current_working_directory_in_sandbox_directory_created")
       #:cargo-inputs (("rust-hamcrest2" ,rust-hamcrest2-0.3)
                       ("rust-nu-glob" ,rust-nu-glob-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-which" ,rust-which-6))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-test-support")
    (synopsis "Support for writing Nushell tests")
    (description "This package contains support for writing Nushell tests.")
    (license license:expat)))

(define-public rust-nu-utils-0.91
  (package
    (name "rust-nu-utils")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xlnhli0zmv4nxznmjb2azq62ywq252zqvpx4668xv70japd74ag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossterm-winapi" ,rust-crossterm-winapi-0.9)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-sys-locale" ,rust-sys-locale-0.3)
                       ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-utils")
    (synopsis "Nushell utility functions")
    (description "This package contains utility functions for nushell.")
    (license license:expat)))
