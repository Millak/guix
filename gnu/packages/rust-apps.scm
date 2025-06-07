;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020-2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.ccom>
;;; Copyright © 2021, 2022 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2021, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021, 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Mathieu Laparie <mlaparie@disr.it>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2023 Arnav Andrew Jose <arnav.jose@gmail.com>
;;; Copyright © 2023 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2023, 2024 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2024 Suhail Singh <suhail@bayesians.ca>
;;; Copyright © 2024 Jordan Moore <lockbox@struct.foo>
;;; Copyright © 2024 muradm <mail@muradm.net>
;;; Copyright © 2024 normally_js <normally_js@posteo.net>
;;; Copyright © 2025 Divya Ranjan Pattanaik <divya@subvertising.org>
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.com>
;;; Copyright © 2024 Danny Milosavljevic <dannym@friendly-machines.com>
;;; Copyright © 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2025 Gabriel Santos <gabrielsantosdesouza@disroot.org>
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

(define-module (gnu packages rust-apps)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages c)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg))

(define-public aardvark-dns
  (package
    (name "aardvark-dns")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aardvark-dns" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d2bs5lmijv6s3n71gqc986n1wy7ny9w74741njjix7932a7yd5f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'aardvark-dns))
    (home-page "https://github.com/containers/aardvark-dns")
    (synopsis "Container-focused DNS A/AAAA record server")
    (description
     "Aardvark-dns is an authoritative DNS server for A/AAAA container
records.  It can forward other requests to configured resolvers.")
    (license license:asl2.0)))

(define-public agate
  (package
    (name "agate")
    (version "3.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "agate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g1zrk3zmyckafcy8rjqjpk9hmas8wgxydhgm70cirsxhz661as6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (native-inputs (list pkg-config))
    (inputs (cons openssl (cargo-inputs 'agate)))
    (home-page "https://github.com/mbrubeck/agate")
    (synopsis "Very simple server for the Gemini hypertext protocol")
    (description
     "Agate is a server for the Gemini network protocol, built with the Rust
programming language.  It has very few features, and can only serve static
files.  It uses async I/O, and should be quite efficient even when running on
low-end hardware and serving many concurrent requests.")
    (license (list license:expat license:asl2.0))))

(define-public alfis
  (package
    (name "alfis")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Revertron/Alfis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "189dqgcnl11fdmd6242h1pbawlq7jdm22zykc1kkcj1dv6s55nvs"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Use a packaged version of web-view.
                 (substitute* "Cargo.toml"
                   (("git = .*web-view\",") "version = \"*\",")
                   ((", git = .*ureq\"") "")
                   (("git = .*ecies-ed25519-ng.*version") "version"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags
       '("--release" "--"
         "--skip=dns::client::tests::test_tcp_client"
         "--skip=dns::client::tests::test_udp_client")))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons* at-spi2-core
            gtk
            glib
            pango
            sqlite
            webkitgtk-with-libsoup2
            (cargo-inputs 'alfis)))
    (home-page "https://github.com/Revertron/Alfis")
    (synopsis "Alternative Free Identity System")
    (description
     "This project represents a minimal blockchain without cryptocurrency,
capable of sustaining any number of domain names in a bunch of original
alternative zones.")
    (license license:agpl3+)))

(define-public bat
  (package
    (name "bat")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11nc2iv2qhd1bs16yijqq934864ybnmg485rny70scy26xb9xk4x"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("\"~([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pre-build
            (lambda _
              (setenv "BAT_ASSETS_GEN_DIR" "target")))
          (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share"))
                     (bash-completions-dir
                      (string-append share "/bash-completion/completions"))
                     (zsh-completions-dir
                      (string-append share "/zsh/site-functions"))
                     (fish-completions-dir
                      (string-append share "/fish/vendor_completions.d"))
                     (man1 (string-append share "/man/man1")))
                (mkdir-p bash-completions-dir)
                (mkdir-p zsh-completions-dir)
                (mkdir-p fish-completions-dir)
                (copy-file "target/assets/completions/bat.bash"
                           (string-append bash-completions-dir "/bat"))
                (copy-file "target/assets/completions/bat.zsh"
                           (string-append zsh-completions-dir "/_bat"))
                (install-file "target/assets/completions/bat.fish"
                              fish-completions-dir)
                (install-file "target/assets/manual/bat.1" man1)))))))
    (native-inputs (list pkg-config))
    (inputs (cons* libgit2-1.7 oniguruma zlib (cargo-inputs 'bat)))
    (home-page "https://github.com/sharkdp/bat")
    (synopsis "@command{cat} clone with syntax highlighting and git integration")
    (description
     "@command{bat} is a drop-in @command{cat} replacement featuring syntax
highlighting for a large number of languages, git integration, and automatic
paging.")
    (license (list license:expat license:asl2.0))))

(define-public bottom
  (package
    (name "bottom")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bottom" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y9wjbzrmcvh0fvfr5cizzwzy6f18hagk970mljwhccrwdsbaapg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags
       '("--"
         "--skip=valid_config_tests::test_all_proc"
         "--skip=valid_config_tests::test_basic"
         "--skip=valid_config_tests::test_cpu_doughnut"
         "--skip=valid_config_tests::test_empty"
         "--skip=valid_config_tests::test_filtering"
         "--skip=valid_config_tests::test_many_proc"
         "--skip=valid_config_tests::test_styling_sanity_check"
         "--skip=valid_config_tests::test_styling_sanity_check_2"
         "--skip=valid_config_tests::test_theme")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enable-building-completions
           (lambda _
             (setenv "BTM_GENERATE" "true")))
         (add-after 'install 'install-extras
           (lambda* (#:key native-inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (man1 (string-append share "/man/man1"))
                    (bash-completions-dir
                     (string-append out "/etc/bash_completion.d/"))
                    (zsh-completions-dir
                     (string-append share "/zsh/site-functions"))
                    (fish-completions-dir
                     (string-append share "/fish/vendor_completions.d"))
                    (elvish-completions-dir
                     (string-append share "/elvish/lib"))
                    (nu-completions-dir
                     (string-append share "/nushell/vendor/autoload")))
               (install-file "target/tmp/bottom/manpage/btm.1" man1)
               (install-file "target/tmp/bottom/completion/_btm"
                             zsh-completions-dir)
               (install-file "target/tmp/bottom/completion/btm.fish"
                             fish-completions-dir)
               (mkdir-p bash-completions-dir)
               (copy-file "target/tmp/bottom/completion/btm.bash"
                          (string-append bash-completions-dir "/btm"))
               (mkdir-p elvish-completions-dir)
               (copy-file "target/tmp/bottom/completion/btm.elv"
                          (string-append elvish-completions-dir "/btm"))
               (mkdir-p nu-completions-dir)
               (copy-file "target/tmp/bottom/completion/btm.nu"
                          (string-append nu-completions-dir "/btm"))))))))
    (inputs (cargo-inputs 'bottom))
    (home-page "https://github.com/ClementTsang/bottom")
    (synopsis "Customizable graphical process/system monitor for the terminal")
    (description
     "This package provides a customizable graphical process/system monitor for
the terminal.")
    (license license:expat)))

;; Note: It has expat license.
;; Note: That is supposedly the (unreleased) version 0.6.3.
(define %tinycbor-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/intel/tinycbor")
          (commit "d393c16f3eb30d0c47e6f9d92db62272f0ec4dc7")))
    (file-name "tinycbor-src")
    (sha256
     (base32
      "0w38lzj0rz36skc1cn3shllc82c7nn32h88frb8f164a8haq3hkw"))))

(define-public c2rust
  (package
    (name "c2rust")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05cm423m7v30b6gwgfzizhyqn3ncnfndin5zbkhyg9ah3pqccgps"))))
    (build-system cargo-build-system)
    (native-inputs (list clang cmake-minimal %tinycbor-source))
    (inputs (cons llvm (cargo-inputs 'c2rust)))
    (arguments
     (list #:install-source? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'patch
                 (lambda _
                   ;; The build process will slightly patch the sources.
                   (copy-recursively
                    #+(this-package-native-input "tinycbor-src")
                    "/tmp/tinycbor")
                   (substitute*
                       (string-append "guix-vendor/rust-c2rust-ast-exporter-"
                                      #$(package-version this-package)
                                      ".tar.gz/src/CMakeLists.txt")
                     (("GIT_TAG .*") "")
                     (("GIT_REPOSITORY .*")
                      "SOURCE_DIR \"/tmp/tinycbor\"\n")))))))
    (home-page "https://c2rust.com/")
    (synopsis "C to Rust translation, refactoring, and cross-checking")
    (description
     "This package provides C to Rust translation, refactoring, and cross-checking.")
    (license license:bsd-3)))

(define-public cargo-audit
  (package
    (name "cargo-audit")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-audit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a00yqpckkw86zh2hg7ra82c5fx0ird5766dyynimbvqiwg2ps0n"))))
    (build-system cargo-build-system)
    (arguments (list #:install-source? #f))
    (inputs (cargo-inputs 'cargo-audit))
    (home-page "https://rustsec.org/")
    (synopsis "Audit Cargo.lock for crates with security vulnerabilities")
    (description
     "This package provides a Cargo subcommand, @command{cargo audit}, to
audit @file{Cargo.lock} for crates with security vulnerabilities.")
    (license (list license:asl2.0 license:expat))))

(define-public cargo-bloat
  (package
    (name "cargo-bloat")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-bloat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zhimclamvy4dggwnciras6w5ilc0wg0c0f7q8hq1qsmmf1w9qjn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'cargo-bloat))
    (home-page "https://github.com/RazrFalcon/cargo-bloat")
    (synopsis "Find out what takes most of the space in your executable")
    (description
     "This package provides a way to find out what takes most of the space
in your executable.")
    (license license:expat)))

(define-public cargo-license
  (package
    (name "cargo-license")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-license" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jw9sn91a23ry6sx3337gi6d56vykgar0i9rrrxgxh93mvdw0qgh"))))
    (build-system cargo-build-system)
    (arguments (list #:install-source? #f))
    (inputs (cargo-inputs 'cargo-license))
    (home-page "https://github.com/onur/cargo-license")
    (synopsis "Cargo subcommand to see license of dependencies")
    (description
     "This package provides a Cargo subcommand, @command{cargo license}, to see
license of dependencies.")
    (license license:expat)))

(define-public cargo-machete
  (package
    (name "cargo-machete")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f9dlc2db5kak85fpq4m31ca0jcb66v3vdjfkwj96h9q3q2hphn1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--"
         ;; Not all files are included.
         "--skip=search_unused::test_crate_renaming_works"
         "--skip=search_unused::test_false_positive_macro_use"
         "--skip=search_unused::test_ignore_deps_works"
         "--skip=search_unused::test_ignore_deps_workspace_works"
         "--skip=search_unused::test_just_unused"
         "--skip=search_unused::test_just_unused_with_manifest"
         "--skip=search_unused::test_unused_kebab_spec"
         "--skip=search_unused::test_unused_renamed_in_registry"
         "--skip=search_unused::test_unused_renamed_in_spec"
         "--skip=search_unused::test_unused_transitive"
         "--skip=search_unused::test_with_bench"
         "--skip=search_unused::test_workspace_from_relative_path"
         "--skip=test_ignore_target")
       #:install-source? #f))
    (inputs (cargo-inputs 'cargo-machete))
    (home-page "https://github.com/bnjbvr/cargo-machete")
    (synopsis "Find unused dependencies in Cargo.toml")
    (description "@code{cargo-machete} finds unused dependencies in Cargo.toml.")
    (license (list license:expat license:asl2.0))))

(define-public cargo-readme
  (package
    (name "cargo-readme")
    (version "3.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/webern/cargo-readme.git")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jwh2j4lw1hk08aflgk7pamnhdbrzr47dc0ipzczn48k6008fm8l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-test-warnings
           (lambda _
             ;; Otherwise the test case will see the warning being emitted
             ;; that "config" is deprecated.
             (when (file-exists? ".cargo/config")
               (rename-file ".cargo/config"
                            ".cargo/config.toml")))))))
    (inputs (cargo-inputs 'cargo-readme))
    (home-page "https://github.com/webern/cargo-readme")
    (synopsis
     "Cargo subcommand to generate README.md content from doc comments")
    (description
     "This package provides a Cargo subcommand to generate README.md content from doc
comments.")
    (license (list license:expat license:asl2.0))))

(define-public cargo-remark
  (package
    (name "cargo-remark")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-remark" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hfg3drsmyif7g8sqc40a5nzkzygqr9gqdajhaydh7dah2w8gkyq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs
     (cons mimalloc (cargo-inputs 'cargo-remark)))
    (home-page "https://github.com/kobzol/cargo-remark")
    (synopsis
     "Cargo subcommand for displaying LLVM optimization remarks from compiling Rust programs")
    (description
     "This package provides a Cargo subcommand for displaying LLVM optimization remarks from
compiling Rust programs.")
    (license license:expat)))

(define-public cargo-show-asm
  (package
    (name "cargo-show-asm")
    (version "0.2.49")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-show-asm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01dg77r3jbbbvf5icl46l24vhw2x8q13nqw414aj77p95jk2gf2g"))))
    (build-system cargo-build-system)
    (inputs
     (cons capstone (cargo-inputs 'cargo-show-asm)))
    (arguments
     `(#:install-source? #f))
    (home-page "https://github.com/pacak/cargo-show-asm")
    (synopsis
     "cargo subcommand that displays the generated assembly of Rust source code.")
    (description
     "This package provides a cargo subcommand that displays the generated assembly of
Rust source code.")
    (license (list license:expat license:asl2.0))))

(define-public cargo-with
  (package
    (name "cargo-with")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cbourjau/cargo-with.git")
             (commit "2eb3cbd87f221f24e780b84306574541de38a1e4")))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "127ifblgp7v2vv8iafl88y1cjyskymqdi0nzsavnyab0x9jiskcr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'cargo-with))
    (home-page "https://github.com/cbourjau/cargo-with/")
    (synopsis
     "Cargo extension to run build artifacts through tools like `gdb`.")
    (description
     "This package provides a Cargo extension to run the build artifacts
through tools like `gdb`.")
    (license license:gpl3)))

(define-public codeberg-cli
  (package
    (name "codeberg-cli")
    (version "0.4.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "codeberg-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l0pi9va2jdja3qxzxii0jf0hjph4f0rn04dcb6j2qpbmdmg8jr0"))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:install-source? #f
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'install-extras
             (lambda* (#:key native-inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (share (string-append out "/share"))
                      (bash-completions-dir
                       (string-append out "/etc/bash_completion.d/"))
                      (zsh-completions-dir
                       (string-append share "/zsh/site-functions"))
                      (fish-completions-dir
                       (string-append share "/fish/vendor_completions.d"))
                      (elvish-completions-dir
                       (string-append share "/elvish/lib"))
                      (berg (if #$(%current-target-system)
                                (search-input-file native-inputs "/bin/berg")
                                (string-append out "/bin/berg"))))
                 (for-each mkdir-p
                           (list bash-completions-dir
                                 zsh-completions-dir
                                 fish-completions-dir
                                 elvish-completions-dir))
                 (with-output-to-file
                   (string-append bash-completions-dir "/berg")
                   (lambda _ (invoke berg "completion" "bash")))
                 (with-output-to-file
                   (string-append zsh-completions-dir "/_berg")
                   (lambda _ (invoke berg "completion" "zsh")))
                 (with-output-to-file
                   (string-append fish-completions-dir "/berg.fish")
                   (lambda _ (invoke berg "completion" "fish")))
                 (with-output-to-file
                   (string-append elvish-completions-dir "/berg")
                   (lambda _ (invoke berg "completion" "elvish")))))))))
    (native-inputs
     (append
       (if (%current-target-system)
           (list this-package)
           '())
      (list pkg-config)))
    (inputs
     (cons* libgit2-1.8 libssh2 openssl zlib (cargo-inputs 'codeberg-cli)))
    (home-page "https://codeberg.org/Aviac/codeberg-cli")
    (synopsis "CLI Tool for codeberg similar to gh and glab")
    (description
     "This package provides CLI Tool for codeberg similar to gh and glab.")
    (license license:agpl3+)))

(define-public complgen
  (package
    (name "complgen")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adaszko/complgen")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v94lg7p79j5706d520jfvidpkw5pqv1a8hgg6cy3fpkghgr375j"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f))
    (native-inputs (list git-minimal))
    (inputs (cargo-inputs 'complgen))
    (home-page "https://github.com/adaszko/complgen")
    (synopsis "Declarative bash/fish/zsh completions without writing shell
scripts")
    (description
     "@command{complgen} is a tool that allows you to generate
completion scripts for all major shells (@code{bash}, @code{fish}, @code{zsh})
from a single, concise, @code{EBNF}-like grammar.")
    (license license:asl2.0)))

(define-public cyme
  (package
    (name "cyme")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cyme" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qss8cpsdbxlljscd046a14d624k5kcawwlw9n9r60shk9gljqpj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags '("--release" "--"
                            ;; Disable tests as they try to access host USB.
                            "--skip=test_list"
                            "--skip=test_list_filtering"
                            "--skip=test_run"
                            "--skip=test_tree"
                            "--skip=test_tree_filtering"
                            "--skip=test_lsusb_device"
                            "--skip=test_lsusb_list"
                            "--skip=test_lsusb_show"
                            "--skip=test_lsusb_tree"
                            "--skip=test_lsusb_tree_verbose"
                            "--skip=test_lsusb_vidpid"
                            ;; unable to find hwdb.bin database file
                            "--skip=udev::hwdb::get")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (install-file "doc/cyme.1" (string-append out "/share/man/man1"))
                (mkdir-p (string-append out "/etc/bash_completion.d"))
                (copy-file "doc/cyme.bash"
                           (string-append out "/etc/bash_completion.d/cyme"))
                (install-file "doc/cyme.fish"
                              (string-append out "/share/fish/vendor_completions.d"))
                (install-file "doc/_cyme"
                              (string-append out "/share/zsh/site-functions"))))))))
    (inputs (cons libusb (cargo-inputs 'cyme)))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/tuna-f1sh/cyme")
    (synopsis "List system USB buses and devices")
    (description
     "This package provides a CLI tool to list system USB buses and devices
similar to lsusb.")
    (license license:gpl3+)))

(define-public diffr
  (package
    (name "diffr")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diffr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1kdngd5g1ssdiq7d10jr3jwg0sx740x3vmhq3j594a5kd467ikib"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       ;; https://github.com/mookid/diffr/issues/79
       #:cargo-test-flags
       '("--release" "--"
         "--skip=tests_cli::color_invalid_attribute_name"
         "--skip=tests_cli::color_invalid_color_not_done"
         "--skip=tests_cli::color_invalid_color_value_ansi"
         "--skip=tests_cli::color_invalid_color_value_name"
         "--skip=tests_cli::color_invalid_color_value_rgb"
         "--skip=tests_cli::color_invalid_face_name"
         "--skip=tests_cli::color_ok"
         "--skip=tests_cli::color_ok_multiple"
         "--skip=tests_cli::color_only_face_name"
         "--skip=tests_cli::debug_flag"
         "--skip=tests_cli::line_numbers_style"
         "--skip=tests_cli::test_bad_argument")))
    (inputs (cargo-inputs 'diffr))
    (home-page "https://github.com/mookid/diffr")
    (synopsis "Longest Common Sequence based diff highlighting tool")
    (description
     "This package provides an @acronym{LCS, longest common sequence} based diff
highlighting tool to ease code review from your terminal.")
    (license license:expat)))

(define-public difftastic
  (package
    (name "difftastic")
    (version "0.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "difftastic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0md332fch4b87akdvljzxp4m2k5yri7cpkz3n54jc762j7j9qmrz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags
       `("--release" "--"
         "--skip=display::side_by_side::tests::test_display_hunks"
         "--skip=display::style::tests::split_string_cjk"
         "--skip=display::style::tests::split_string_cjk2"
         "--skip=display::style::tests::split_string_simple"
         "--skip=display::style::tests::split_string_unicode"
         "--skip=display::style::tests::test_combining_char"
         "--skip=display::style::tests::test_split_and_apply"
         ,(string-append "--skip=display::style::tests::"
                         "test_split_and_apply_gap_between_styles_on_wrap_boundary")
         "--skip=display::style::tests::test_split_and_apply_trailing_text"
         "--skip=display::style::tests::test_split_and_apply_trailing_text_newline")))
    (inputs
     (cons mimalloc (cargo-inputs 'difftastic)))
    (home-page "https://difftastic.wilfred.me.uk/")
    (synopsis "Structural diff command that understands syntax")
    (description
     "@command{difft} provides a structural diff that understands syntax.  It
compares files using the syntax, not line-by-line providing accurate diffs
that are easier to read.  It works with a variety of languages including
Javascript, Python, Rust and Scheme.")
    (license license:expat)))

(define-public drill
  (package
    (name "drill")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "drill" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0jp9r19zc9m3hgxc7a98fhyi1ga0qwjprxjsqaxiykmjpb86bxf3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:install-source? #f))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons openssl (cargo-inputs 'drill)))
    (home-page "https://github.com/fcsonline/drill")
    (synopsis "HTTP load testing application")
    (description
     "Drill is a HTTP load testing application written in Rust inspired by
Ansible syntax.  Benchmark files can be written in YAML.")
    (license license:gpl3)))

(define-public dutree
  (package
    (name "dutree")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dutree" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1611h27i8fm3jndscd6w65z8z7w09nnrm61vdgs9kb8ln57gqm8x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'dutree))
    (home-page "https://ownyourbits.com/2018/03/25/analyze-disk-usage-with-dutree/")
    (synopsis "Command line tool to analyze disk usage")
    (description
     "@command{dutree} is command line tool to analyze disk usage.
Features include:
@enumerate
@item coloured output, according to the @code{LS_COLORS} environment variable.
@item display the file system tree.
@item ability to aggregate small files.
@item ability to exclude files or directories.
@item ability to compare different directories.
@item fast, written in Rust.
@end enumerate\n")
    (license license:gpl3)))

(define-public emacs-lsp-booster
  (package
    (name "emacs-lsp-booster")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blahgeek/emacs-lsp-booster")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12vrgqyvm1841i9ld9b3axa9ybgqf3kr6nbfd0l4zdnhyljz3zxq"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f))
    (native-inputs (list emacs))    ; Not emacs-minimal
    (inputs (cargo-inputs 'emacs-lsp-booster))
    (home-page "https://github.com/blahgeek/emacs-lsp-booster")
    (synopsis "Emacs LSP performance booster")
    (description
     "@code{emacs-lsp-booster} improves the performance of @code{lsp-mode} and
@code{eglot} Emacs packages using a wrapper executable.  See the home-page for
configuration instructions.")
    (license license:expat)))

(define-public evremap
  (let ((commit "cc618e8b973f5c6f66682d1477b3b868a768c545")) ;version bump
    (package
      (name "evremap")
      (version "0.1.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wez/evremap")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "182ry573q8kjsxx2hvxk9d6clahpll1vh50zvs5g652jd6a2f038"))))
      (build-system cargo-build-system)
      (arguments (list #:install-source? #f))
      (native-inputs (list pkg-config))
      (inputs (cons libevdev (cargo-inputs 'evremap)))
      (home-page "https://github.com/wez/evremap")
      (synopsis "Keyboard input remappper")
      (description
       "Evremap is a keyboard input remapper.  It works by grabbing exclusive
access to an input device and maintaining a model of the keys that are
pressed.  It then applies your remapping configuration to produce the
effective set of pressed keys and emits appropriate changes to a virtual
output device.

Its remapping is effective system-wide: in Wayland, X11 and the Linux
console.")
      (license license:expat))))

(define-public eza
  (package
    (name "eza")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "eza" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "026i75pk4vnx1yz2iggkdin2xwbb58qwqm3rim7f4q905m8ar2jh"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'build-manual
            (lambda* (#:key inputs #:allow-other-keys)
              (when (assoc-ref inputs "pandoc")
                (map (lambda (page)
                       (with-output-to-file page
                         (lambda _
                           (invoke "pandoc" "--standalone"
                                   "-f" "markdown"
                                   "-t" "man"
                                   (string-append "man/" page ".md")))))
                     (list "eza.1"
                           "eza_colors.5"
                           "eza_colors-explanation.5")))))
          (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share"))
                     (bash-completions-dir
                       (string-append share "/bash-completion/completions"))
                     (zsh-completions-dir
                       (string-append share "/zsh/site-functions"))
                     (fish-completions-dir
                       (string-append share "/fish/vendor_completions.d"))
                     (nu-completions-dir
                       (string-append share "/nushell/vendor/autoload"))
                     (man1 (string-append share "/man/man1"))
                     (man5 (string-append share "/man/man5")))
                (when (file-exists? "eza.1")
                  (install-file "eza.1" man1))
                (when (file-exists? "eza_colors.5")
                  (install-file "eza_colors.5" man5))
                (when (file-exists? "eza_colors-explanation.5")
                  (install-file "eza_colors-explanation.5" man5))
                (install-file "completions/bash/eza" bash-completions-dir)
                (install-file "completions/zsh/_eza" zsh-completions-dir)
                (install-file "completions/fish/eza.fish" fish-completions-dir)
                (install-file "completions/nush/eza.nu" nu-completions-dir)))))))
    (native-inputs
     (append (list pkg-config)
             (if (supported-package? pandoc)
                 (list pandoc)
                 '())))
    (inputs (cons* libgit2-1.9 zlib (cargo-inputs 'eza)))
    (home-page "https://github.com/eza-community/eza")
    (synopsis "Modern replacement for ls")
    (description
     "@code{eza} is a modern replacement for the command-line
program @code{ls}.  It uses colours to distinguish file types and
metadata.  It also knows about symlinks, extended attributes, and Git.
This package is the community maintained fork of @code{exa}.")
    (license license:eupl1.2)))

(define-public exa
  (deprecated-package "exa" eza))

(define-public fclones
  (package
    (name "fclones")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fclones" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1danl1sn7l1b5wz27aqbx43nnvsm9nflly8l8xqf41c4ainq5j07"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:modules
      '((guix build cargo-build-system)
        (guix build utils)
        (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-completions
            (lambda* (#:key native-inputs #:allow-other-keys)
              (for-each
               (match-lambda
                 ((shell . path)
                  (mkdir-p (in-vicinity #$output (dirname path)))
                  (let ((binary
                         (if #$(%current-target-system)
                             (search-input-file native-inputs "bin/fclones")
                             (in-vicinity #$output "bin/fclones"))))
                    (with-output-to-file (in-vicinity #$output path)
                      (lambda _
                        (invoke binary "complete" shell))))))
               '(("bash"   . "share/bash-completion/completions/fclones")
                 ("elvish" . "share/elvish/lib/fclones")
                 ("fish"   . "share/fish/vendor_completions.d/fclones.fish")
                 ("zsh"    . "share/zsh/site-functions/_fclones"))))))))
    (native-inputs
     (if (%current-target-system)
         (list this-package)
         '()))
    (inputs (cargo-inputs 'fclones))
    (home-page "https://github.com/pkolaczk/fclones")
    (synopsis "Find and operate on duplicate files")
    (description
     "@command{fclones} is a command line utility that identifies groups of
identical files and gets rid of the file copies you no longer need.  It comes
with plenty of configuration options for controlling the search scope and
offers many ways of removing duplicates.  For maximum flexibility, it
integrates well with other Unix utilities like @command{find} and it speaks
JSON, so you have a lot of control over the search and cleanup process.")
    (license license:expat)))

(define-public fd
  (package
    (name "fd")
    (version "10.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fd-find" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0d5zv6pmxxsblbvk4pzxjbj072d2bg3byhss57699y2s37xdw26y"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-test-flags
      ;; No user 'root' in the build environment.
      '(list "--release" "--"
             "--skip=test_owner_root")
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'override-jemalloc
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((jemalloc (assoc-ref inputs "jemalloc")))
                ;; This flag is needed when not using the bundled jemalloc.
                ;; https://github.com/tikv/jemallocator/issues/19
                (setenv "CARGO_FEATURE_UNPREFIXED_MALLOC_ON_SUPPORTED_PLATFORMS" "1")
                (setenv "JEMALLOC_OVERRIDE"
                        (string-append jemalloc "/lib/libjemalloc.so")))))
          (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                ;; Manpages
                (install-file "doc/fd.1" (string-append out "/share/man/man1"))
                ;; Completions require running the built binary.
                (unless #$(%current-target-system)
                  (invoke "make" "completions")
                  (install-file "autocomplete/fd.bash"
                                (string-append out "/etc/bash_completion.d"))
                  (install-file "autocomplete/fd.fish"
                                (string-append out "/share/fish/vendor_completions.d"))
                  (install-file "autocomplete/_fd"
                                (string-append out "/share/zsh/site-functions"))
                  (rename-file (string-append out "/etc/bash_completion.d/fd.bash")
                               (string-append out "/etc/bash_completion.d/fd")))))))))
     (inputs (cons jemalloc (cargo-inputs 'fd)))
     (home-page "https://github.com/sharkdp/fd")
     (synopsis "Simple, fast and user-friendly alternative to find")
     (description
      "@code{fd} is a simple, fast and user-friendly alternative to @code{find}.
While it does not seek to mirror all of find's powerful functionality, it provides
defaults for 80% of the use cases.")
     (license (list license:expat license:asl2.0))))

(define-public forgejo-cli
  (package
    (name "forgejo-cli")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "forgejo-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a3f10317fv0qmwnv53vzs5dilax0xqhix3idrgjz4rcvjs42d25"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f))
    (native-inputs (list pkg-config))
    (inputs
     (cons* libgit2-1.8
            libssh2
            oniguruma
            openssl
            zlib
            (cargo-inputs 'forgejo-cli)))
    (home-page "https://codeberg.org/Cyborus/forgejo-cli/")
    (synopsis "CLI tool for Forgejo")
    (description "This package provides a CLI tool for Forgejo.")
    (license (list license:asl2.0 license:expat))))

(define-public gitoxide
  (package
    (name "gitoxide")
    (version "0.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gitoxide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19nrari83270csawjiyc88dm6s0h7lk0x9p8clbg7y8wj08g6rag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("gix-features/zlib-stock")
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-extras
           (lambda* (#:key native-inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (man1 (string-append share "/man/man1"))
                    (bash-completions-dir
                     (string-append out "/etc/bash_completion.d/"))
                    (zsh-completions-dir
                     (string-append share "/zsh/site-functions"))
                    (fish-completions-dir
                     (string-append share "/fish/vendor_completions.d"))
                    (elvish-completions-dir
                     (string-append share "/elvish/lib"))
                    (gix (if ,(%current-target-system)
                             (search-input-file native-inputs "/bin/gix")
                             (string-append out "/bin/gix")))
                    (ein (if ,(%current-target-system)
                             (search-input-file native-inputs "/bin/ein")
                             (string-append out "/bin/ein"))))
               (for-each mkdir-p
                         (list bash-completions-dir
                               zsh-completions-dir
                               fish-completions-dir
                               elvish-completions-dir))
               (with-output-to-file
                 (string-append bash-completions-dir "/gix")
                 (lambda _ (invoke gix "completions" "--shell" "bash")))
               (with-output-to-file
                 (string-append bash-completions-dir "/ein")
                 (lambda _ (invoke ein "completions" "--shell" "bash")))
               (with-output-to-file
                 (string-append zsh-completions-dir "/_gix")
                 (lambda _ (invoke gix "completions" "--shell" "zsh")))
               (with-output-to-file
                 (string-append zsh-completions-dir "/_ein")
                 (lambda _ (invoke ein "completions" "--shell" "zsh")))
               (with-output-to-file
                 (string-append fish-completions-dir "/gix.fish")
                 (lambda _ (invoke gix "completions" "--shell" "fish")))
               (with-output-to-file
                 (string-append fish-completions-dir "/ein.fish")
                 (lambda _ (invoke ein "completions" "--shell" "fish")))
               (with-output-to-file
                 (string-append elvish-completions-dir "/gix")
                 (lambda _ (invoke gix "completions" "--shell" "elvish")))
               (with-output-to-file
                 (string-append elvish-completions-dir "/ein")
                 (lambda _ (invoke ein "completions" "--shell" "elvish")))))))))
    (native-inputs
     (append
       (if (%current-target-system)
           (list this-package)
           '())
       (list cmake-minimal pkg-config)))
    (inputs (cons* curl openssl sqlite zlib (cargo-inputs 'gitoxide)))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "command-line application for interacting with git repositories")
    (description
     "This package provides a command-line application for interacting with git
repositories.")
    (license (list license:expat license:asl2.0))))

(define-public gitui
  (package
    (name "gitui")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gitui" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mbf7gqnlkprx6scyghnp0g5cq2ap0j9c48gnpv0kqlhig0c5r07"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "wix")
           (substitute* "Cargo.toml"
             ;; Remove vendor-openssl from the default features.
             ((".*\"vendor-openssl\",.*") ""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; this test fails with permission denied error
         "--skip=test_symbolic_links")
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-release-variable
           (lambda _
             (setenv "GITUI_RELEASE" "true")
             (setenv "BUILD_GIT_COMMIT_ID" "GNUGUIX"))))))
    (native-inputs (list cmake-minimal pkg-config))
    (inputs (cons* libgit2-1.9 libssh2 openssl zlib (cargo-inputs 'gitui)))
    (home-page "https://github.com/extrawurst/gitui")
    (synopsis "Terminal UI for git")
    (description "This package provides a fast Terminal UI for git.")
    (license license:expat)))

(define-public helvum
  (package
    (name "helvum")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/pipewire/helvum")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q8gkx7djrfdl8fykppsqkxiadsq47v0xhj612nxlrvjz8n77ygn"))))
    (build-system meson-build-system)
    (arguments
     `(#:imported-modules (,@%meson-build-system-modules
                           ,@%cargo-build-system-modules)
       #:modules (((guix build cargo-build-system) #:prefix cargo:)
                  (guix build meson-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-for-build
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")
               (("update_desktop_database: true")
                "update_desktop_database: false"))
             (delete-file "Cargo.lock")))
         ;; The meson 'configure phase changes to a different directory and
         ;; we need it created before unpacking the crates.
         (add-after 'configure 'prepare-cargo-build-system
           (lambda args
             (for-each
              (lambda (phase)
                (format #t "Running cargo phase: ~a~%" phase)
                (apply (assoc-ref cargo:%standard-phases phase)
                       #:vendor-dir "vendor"
                       args))
              '(unpack-rust-crates
                configure
                check-for-pregenerated-files
                patch-cargo-checksums)))))))
    (native-inputs (list clang pkg-config rust `(,rust "cargo")))
    (inputs (cons* glib gtk libadwaita pipewire (cargo-inputs 'helvum)))
    (home-page "https://gitlab.freedesktop.org/pipewire/helvum")
    (synopsis "GTK patchbay for pipewire")
    (description "This package provides a GTK patchbay for pipewire.")
    (license license:gpl3)))

(define-public hexyl
  (package
    (name "hexyl")
    (version "0.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hexyl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1y2yrr8nh3idya5wviqqnvz57y4mvw1jx3gi57acddkj9386vma3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'install-manual
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((man1 (string-append (assoc-ref outputs "out")
                                       "/share/man/man1")))
              (when (assoc-ref inputs "pandoc")
                (mkdir-p man1)
                (with-output-to-file (string-append man1 "/hexyl.1")
                  (lambda _
                    (invoke "pandoc" "--standalone"
                            "--from" "markdown"
                            "--to" "man"
                            "doc/hexyl.1.md"))))))))))
    (native-inputs
     (if (supported-package? pandoc)
         (list pandoc)
         '()))
    (inputs (cargo-inputs 'hexyl))
    (home-page "https://github.com/sharkdp/hexyl")
    (synopsis "Command-line hex viewer")
    (description
     "This package provides a command line hex viewer.  It uses a colored output
for distinguishing different kinds of bytes such as NULL bytes, printable ASCII
characters, ASCII whitespace characters, other ASCII characters and non-ASCII.")
    (license (list license:expat license:asl2.0))))

(define-public hyperfine
  (package
    (name "hyperfine")
    (version "1.19.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyperfine" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "07pm8i71l9y50awz0d97zb231lcvp3c2hmdh98znq4m9a02xd7hv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:modules ((guix build cargo-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
        (add-after 'install 'install-more
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out   (assoc-ref outputs "out"))
                   (share (string-append out "/share/"))
                   (man   (string-append share "man/man1"))
                   (bash  (string-append out "/etc/bash_completion.d/"))
                   (fish  (string-append share "fish/vendor_completions.d"))
                   (zsh   (string-append share "zsh/site-functions")))
              (install-file "doc/hyperfine.1" man)
              (for-each (cut install-file <> bash)
                        (find-files "target" "^hyperfine.bash$"))
              (rename-file (string-append bash "/hyperfine.bash")
                           (string-append bash "/hyperfine"))
              (for-each (cut install-file <> fish)
                        (find-files "target" "^hyperfine.fish$"))
              (for-each (cut install-file <> zsh)
                        (find-files "target" "^_hyperfine$"))))))))
    (inputs (cargo-inputs 'hyperfine))
    (home-page "https://github.com/sharkdp/hyperfine")
    (synopsis "Command-line benchmarking tool")
    (description
     "This package provides a command-line benchmarking tool.")
    (license (list license:expat license:asl2.0))))

(define-public i3status-rust
  (package
    (name "i3status-rust")
    (version "0.33.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/greshake/i3status-rust")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17fl0gx17mqc05jvr35g031d8z43cnlvqmjdwdbybl0lq4rbi6f4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-resources-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (substitute* "src/util.rs"
                 (("/usr/share/i3status-rust") share)))))
         (add-after 'unpack 'substitute-package-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (define* (substitute-command-block* file command full-command)
               (substitute* file
                 (((string-append "Command::new\\(\"" command "\"\\)"))
                  (string-append "Command::new(\"" full-command "\")"))))
             (substitute-command-block* "src/blocks/keyboard_layout/set_xkb_map.rs"
               "setxkbmap" (search-input-file inputs "/bin/setxkbmap"))
             (substitute-command-block* "src/blocks/sound/alsa.rs"
               "alsactl" (search-input-file inputs "/sbin/alsactl"))
             (substitute-command-block* "src/blocks/sound/alsa.rs"
               "amixer" (search-input-file inputs "/bin/amixer"))
             (substitute-command-block* "src/blocks/speedtest.rs"
               "speedtest-cli" (search-input-file inputs "/bin/speedtest-cli"))
             (substitute-command-block* "src/blocks/xrandr.rs"
               "xrandr" (search-input-file inputs "/bin/xrandr"))
             (substitute-command-block* "src/util.rs"
               "sh" (search-input-file inputs "/bin/sh"))
             (substitute-command-block* "src/subprocess.rs"
               "sh" (search-input-file inputs "/bin/sh"))))
         (add-after 'install 'install-resources
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "files" (string-append out "/share")))))
         (add-after 'install 'wrap-i3status
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (paths (map
                           (lambda (input)
                             (string-append
                               (assoc-ref inputs input) "/bin"))
                           '("iproute2" "kdeconnect"))))
               (wrap-program (string-append out "/bin/i3status-rs")
                 `("PATH" prefix ,paths))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons* alsa-utils
            bash-minimal
            dbus
            iproute
            kdeconnect
            (list lm-sensors "lib")
            pulseaudio
            openssl
            setxkbmap
            speedtest-cli
            xrandr
            (cargo-inputs 'i3status-rust)))
    (home-page "https://github.com/greshake/i3status-rust/")
    (synopsis "Replacement for i3status, written in Rust")
    (description "@code{i3status-rs} is a feature-rich and resource-friendly
replacement for i3status, written in pure Rust.  It provides a way to display
@code{blocks} of system information (time, battery status, volume, etc) on the i3
bar.  It is also compatible with sway.")
    (license license:gpl3)))

(define-public jless
  (package
    (name "jless")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jless" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mij8c0lp62mnfvcbzrhmf1g70fq29lj2s9l05qx7njsqs64xqkf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (native-inputs (list python pkg-config)) ;needed by rust-xcb
    (inputs (cons* libx11 libxcb (cargo-inputs 'jless)))
    (home-page "https://github.com/PaulJuliusMartinez/jless")
    (synopsis "Command-line JSON viewer")
    (description "This package provides a command-line JSON viewer.")
    (license license:expat)))

(define-public just
  (package
    (name "just")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "just" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "03k9ifgxwxvx41f7xc9hv09h0w6j9k46cazfdxzynq56dly3kl7c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--"
         "--skip=backticks::trailing_newlines_are_stripped"
         "--skip=completions::bash"
         "--skip=functions::env_var_functions"
         "--skip=string::shebang_backtick")
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-hardcoded-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (cons "src/justfile.rs"
                                (find-files "tests/" "\\.rs$"))
               (("/bin/sh")
                (search-input-file inputs "/bin/sh"))
               (("/usr/bin/env sh")
                (search-input-file inputs "/bin/sh"))
               (("/usr/bin/env")
                (search-input-file inputs "/bin/env"))
               (("/bin/echo")
                (search-input-file inputs "/bin/echo")))))
         (add-after 'install 'install-extras
           (lambda* (#:key native-inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (man1 (string-append share "/man/man1"))
                    (bash-completions-dir
                     (string-append out "/etc/bash_completion.d/"))
                    (zsh-completions-dir
                     (string-append share "/zsh/site-functions"))
                    (fish-completions-dir
                     (string-append share "/fish/vendor_completions.d"))
                    (elvish-completions-dir
                     (string-append share "/elvish/lib"))
                    (just (if ,(%current-target-system)
                          (search-input-file native-inputs "/bin/just")
                          (string-append out "/bin/just"))))
               (mkdir "man")
               (with-output-to-file "man/just.1"
                 (lambda _ (invoke just "--man")))
               (install-file "man/just.1" man1)

               (mkdir-p bash-completions-dir)
               (with-output-to-file
                 (string-append bash-completions-dir "/just")
                 (lambda _ (invoke just "--completions" "bash")))
               (mkdir-p zsh-completions-dir)
               (with-output-to-file
                 (string-append zsh-completions-dir "/_just")
                 (lambda _ (invoke just "--completions" "zsh")))
               (mkdir-p fish-completions-dir)
               (with-output-to-file
                 (string-append fish-completions-dir "/just.fish")
                 (lambda _ (invoke just "--completions" "fish")))
               (mkdir-p elvish-completions-dir)
               (with-output-to-file
                 (string-append elvish-completions-dir "/just")
                 (lambda _ (invoke just "--completions" "elvish")))))))))
    (native-inputs (if (%current-target-system)
                       (list this-package)
                       '()))
    (inputs (cons* bash-minimal coreutils-minimal (cargo-inputs 'just)))
    (home-page "https://github.com/casey/just")
    (synopsis "Command runner")
    (description "This package provides @code{just}, a command runner.
@code{just} is a handy way to save and run project-specific commands.")
    (license license:cc0)))

(define-public kanata
  (package
    (name "kanata")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1632iaclw9qy6sswm2wqapa28px7rdxqchk8b1wwp6k2scysr2bs"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f))
    (inputs (cargo-inputs 'kanata))
    (home-page "https://github.com/jtroo/kanata")
    (synopsis "Multi-layer keyboard customization")
    (description
     "Kanata is a keyboard re-mapper.  It supports multiple layers of key,
and advanced key behavior customization, such as tap-hold, macros and
Unicode.")
    (license license:lgpl3)))

(define-public kibi
  (package
    (name "kibi")
    (version "0.2.2")
    (source
     (origin
       ;; crates.io doesn't have the config files
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ilai-deutel/kibi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s9ka3pfhpssms2y5707f33n59ljnqqwp7jarh2l55a9dhlnl7d3"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-test-flags `(list "--release" "--"
                                "--skip=syntax::tests::syntax_d_files")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'install-extras
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (share (string-append out "/share"))
                              (syntax.d (string-append share "/syntax.d"))
                              (etc (string-append out "/etc")))
                         (mkdir-p syntax.d)
                         (copy-recursively "syntax.d" syntax.d)
                         (rename-file "config_example.ini" "config.ini")
                         (install-file "config.ini" etc)))))))
    (inputs (cargo-inputs 'kibi))
    (home-page "https://github.com/ilai-deutel/kibi")
    (synopsis "Featureful text editor in less than 1024 lines of code")
    (description
     "Inspired by the kilo text editor in C, this package provides a text
editor in less than 1024 lines of code with syntax highlighting, search and
more.")
    (license (list license:expat license:asl2.0))))

(define-public lsd
  (package
    (name "lsd")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lsd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06l0ynhny43q74dyb8m4r2j1w9xz29m0xrqmnpysm1f09bx3dzrj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-shell-completion-dir
           (lambda _
             (setenv "SHELL_COMPLETIONS_DIR" "target/assets")))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (bash-completions-dir
                      (string-append out "/etc/bash_completion.d"))
                    (zsh-completions-dir
                      (string-append share "/zsh/site-functions"))
                    (fish-completions-dir
                      (string-append share "/fish/vendor_completions.d")))
               ;; The completions are generated in build.rs.
               (install-file "target/assets/_lsd" zsh-completions-dir)
               (install-file "target/assets/lsd.fish" fish-completions-dir)
               (mkdir-p bash-completions-dir)
               (copy-file "target/assets/lsd.bash"
                          (string-append bash-completions-dir "/lsd"))))))))
    (native-inputs (list libgit2-1.7
                         pkg-config
                         zlib
                         ;; for tests
                         git-minimal))
    (inputs (cargo-inputs 'lsd))
    (home-page "https://github.com/lsd-rs/lsd")
    (synopsis "Mostly ls compatible command with pretty colors")
    (description
     "This package provides An ls command with a lot of pretty colors
and some other stuff.")
    (license license:asl2.0)))

(define-public macchina
  (package
    (name "macchina")
    (version "6.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "macchina" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m1hkf81njdbx69c2k3hp3dslq6xfh14hs8v7iadw3cl44dshb7r"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'install-extras
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (share (string-append out "/share"))
                              (contrib (string-append share "/contrib")))
                         (mkdir-p contrib)
                         (copy-recursively "contrib" contrib)))))))
    (native-inputs (list pkg-config))
    (inputs (cons* libgit2 sqlite zlib (cargo-inputs 'macchina)))
    (home-page "https://github.com/Macchina-CLI/macchina")
    (synopsis "System information fetcher with an emphasis on performance")
    (description
     "This package provides a system information fetcher with an emphasis on
performance.  Similar to neofetch, this package prints out system information
on the terminal in a visually appealing way.")
    (license license:expat)))

(define-public matugen
  (package
    (name "matugen")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "matugen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rqljm689say9f1878x9x9v1ahaji52vqrnnm6nmkkilfsyfx550"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'matugen))
    (home-page "https://github.com/InioX/matugen")
    (synopsis "Generate themes using the 'Material You' palette")
    (description
     "@command{matugen} generates a 'Material You' color palette based on a
specified image or color, easing the process of theme creation.")
    (license license:gpl2)))

(define-public maturin
  (package
    (name "maturin")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "maturin" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mxgal826395cq9klg9h2f4qvqhi4s0v4x54bhvsikq2bjcf934w"))
              (patches (search-patches "maturin-no-cross-compile.patch"))))
    (build-system cargo-build-system)
    (arguments
     `(#:modules ((guix build cargo-build-system)
                  ((guix build pyproject-build-system) #:prefix py:)
                  (guix build utils))
       #:imported-modules ((guix build cargo-build-system)
                           (guix build cargo-utils)
                           ,@%pyproject-build-system-modules)
       #:install-source? #f
       #:cargo-test-flags
       '("--"
         ;; Not all files are included.
         "--skip=build_options::test::test_find_bridge_bin"
         "--skip=build_options::test::test_find_bridge_cffi"
         "--skip=build_options::test::test_find_bridge_pyo3"
         "--skip=build_options::test::test_find_bridge_pyo3_abi3"
         "--skip=build_options::test::test_find_bridge_pyo3_feature"
         "--skip=metadata::test::test_implicit_readme"
         "--skip=metadata::test::test_merge_metadata_from_pyproject_dynamic_license_test"
         "--skip=metadata::test::test_merge_metadata_from_pyproject_toml"
         "--skip=metadata::test::test_merge_metadata_from_pyproject_toml_with_customized_python_source_dir"
         "--skip=pyproject_toml::tests::test_warn_missing_maturin_version")
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-python-module
           (lambda _
             ;; Match the features from the cargo-build-system and Cargo.toml.
             (setenv "MATURIN_SETUP_ARGS" "--features=default")
             ((assoc-ref py:%standard-phases 'build))))

         ;; We can't use the pyproject install phase because maturin is a
         ;; binary, not a python script.
         (add-after 'install 'install-python-module
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (wheel (car (find-files "dist" "\\.whl$")))
                   (site-dir (py:site-packages inputs outputs))
                   (pyversion
                     (string-append "python"
                                    (py:python-version
                                      (assoc-ref inputs "python-wrapper")))))
               (invoke "python" "-m" "zipfile" "-e" wheel site-dir)
               (mkdir-p (string-append out "/bin"))
               (for-each delete-file
                         (find-files (string-append out "/lib/" pyversion)
                                     "^maturin$")))))
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (maturin (string-append out "/bin/maturin")))
               ;; TODO? fig, powershell
               (mkdir-p (string-append out "/etc/bash_completion.d"))
               (with-output-to-file
                 (string-append out "/etc/bash_completion.d/maturin")
                 (lambda _ (invoke maturin "completions" "bash")))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (with-output-to-file
                 (string-append share "/fish/vendor_completions.d/maturin.fish")
                 (lambda _ (invoke maturin "completions" "fish")))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (with-output-to-file
                 (string-append share "/zsh/site-functions/_maturin")
                 (lambda _ (invoke maturin "completions" "zsh")))
               (mkdir-p (string-append share "/elvish/lib"))
               (with-output-to-file
                 (string-append share "/elvish/lib/maturin")
                 (lambda _ (invoke maturin "completions" "elvish")))
               (mkdir-p (string-append share "/nushell/vendor/autoload"))
               (with-output-to-file
                 (string-append share "/nushell/vendor/autoload/maturin")
                 (lambda _ (invoke maturin "completions" "nushell")))))))))
    (propagated-inputs
     (list python-tomli))
    (inputs (cons bzip2 (cargo-inputs 'maturin)))
    (native-inputs
     (list python-wheel
           python-wrapper
           python-setuptools-rust))
    (home-page "https://github.com/pyo3/maturin")
    (synopsis "Build and publish crates and python packages")
    (description
     "Build and publish crates with @code{pyo3}, @code{rust-cpython} and
@code{cffi} bindings as well as rust binaries as python packages.")
    (license (list license:expat license:asl2.0))))

(define-public netavark
  (package
    (name "netavark")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "netavark" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dn8ra8rlq1hwb7pay3qbashrisi23293jflvv9k0zciaibsvfyy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (native-inputs (list protobuf))
    (inputs (cargo-inputs 'netavark))
    (home-page "https://github.com/containers/netavark")
    (synopsis "Container network stack")
    (description "Netavark is a rust based network stack for containers.  It
is being designed to work with Podman but is also applicable for other OCI
container management applications.")
    (license license:asl2.0)))

(define-public ouch
  (package
    (name "ouch")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ouch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gslgyv63jq66w5pymsn7jnkmh2b8s8wzqvhs6k2iywzc4nm3gxd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'pre-build
          (lambda _
            (setenv "OUCH_ARTIFACTS_FOLDER" "target")
            ;; Uses nonfree library.
            (invoke "cargo" "remove" "unrar")))
        (add-after 'install 'install-extras
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (share (string-append out "/share"))
                   (bash-completions-dir
                    (string-append out "/etc/bash-completion.d"))
                   (zsh-completions-dir
                    (string-append share "/zsh/site-functions"))
                   (fish-completions-dir
                    (string-append share "/fish/vendor_completions.d"))
                   (elvish-completions-dir
                    (string-append share "/elvish/lib"))
                   (man1 (string-append share "/man/man1")))
              (mkdir-p bash-completions-dir)
              (mkdir-p elvish-completions-dir)
              (copy-file "target/ouch.bash"
                         (string-append bash-completions-dir "/ouch"))
              (install-file "target/_ouch"
                            (string-append zsh-completions-dir "/_ouch"))
              (install-file "target/ouch.fish"
                            fish-completions-dir)
              (copy-file "target/ouch.elv"
                         (string-append elvish-completions-dir "/ouch"))
              (for-each (lambda (manpage)
                          (install-file manpage man1))
                        (find-files "target" "\\.1$"))))))))
    (native-inputs (list git-minimal/pinned pkg-config))
    (inputs (cons* clang `(,zstd "lib") zlib (cargo-inputs 'ouch)))
    (home-page "https://github.com/ouch-org/ouch")
    (synopsis "Compression and decompression utility")
    (description
     "This package provides a command-line utility for easily compressing and
decompressing files and directories.")
    (license license:expat)))

(define-public py-spy
  (package
    (name "py-spy")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "py-spy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fvdmrqp4qand1zb9cwks8hpkysdqajrdh9y7ks15c78985k1x64"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-shell-completions
            (lambda* (#:key native-inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share"))
                     (bash-completions-dir
                      (string-append out "/etc/bash_completion.d/"))
                     (zsh-completions-dir
                      (string-append share "/zsh/site-functions"))
                     (fish-completions-dir
                      (string-append share "/fish/vendor_completions.d"))
                     (elvish-completions-dir
                      (string-append share "/elvish/lib"))
                     (py-spy (if #$(%current-target-system)
                                (search-input-file native-inputs "/bin/py-spy")
                                (string-append out "/bin/py-spy"))))
                (for-each mkdir-p
                          (list bash-completions-dir
                                zsh-completions-dir
                                fish-completions-dir
                                elvish-completions-dir))
                (with-output-to-file
                  (string-append bash-completions-dir "/py-spy")
                  (lambda _ (invoke py-spy "completions" "bash")))
                (with-output-to-file
                  (string-append zsh-completions-dir "/_py-spy")
                  (lambda _ (invoke py-spy "completions" "zsh")))
                (with-output-to-file
                  (string-append fish-completions-dir "/py-spy.fish")
                  (lambda _ (invoke py-spy "completions" "fish")))
                (with-output-to-file
                  (string-append elvish-completions-dir "/py-spy")
                  (lambda _ (invoke py-spy "completions" "elvish")))))))))
    (native-inputs
     (append
       (if (%current-target-system)
           (list this-package)
           '())
       (list python-minimal-wrapper)))
    (inputs (cons libunwind (cargo-inputs 'py-spy)))
    (home-page "https://github.com/benfred/py-spy")
    (synopsis "Sampling profiler for Python programs")
    (description
     "This package provides a sampling profiler for Python programs.")
    (license license:expat)))

(define-public ripgrep
  (package
    (name "ripgrep")
    (version "14.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ripgrep" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n81lnfzy556f63pgnizay2sgx8fgn4mmailbybjfiaqvhr80yzp"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      ;; Note: the built target 'rg' binary is required for 'install-extras
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'install-extras
                     (lambda* (#:key native-inputs outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (share (string-append out "/share"))
                              (bash-completions-dir
                                (string-append out "/etc/bash_completion.d/"))
                              (zsh-completions-dir
                                (string-append share "/zsh/site-functions"))
                              (fish-completions-dir
                                (string-append share "/fish/vendor_completions.d"))
                              (man1 (string-append share "/man/man1"))
                              (rg (if #$(%current-target-system)
                                    (search-input-file native-inputs "/bin/rg")
                                    (string-append out "/bin/rg"))))
                           (mkdir-p man1)
                           (with-output-to-file (string-append man1 "/rg.1")
                             (lambda _
                               (invoke rg "--generate" "man")))
                           (mkdir-p bash-completions-dir)
                           (with-output-to-file (string-append
                                                  bash-completions-dir "/rg")
                             (lambda _
                               (invoke rg "--generate" "complete-bash")))
                           (mkdir-p zsh-completions-dir)
                           (with-output-to-file (string-append
                                                  zsh-completions-dir "/_rg")
                             (lambda _
                               (invoke rg "--generate" "complete-zsh")))
                           (mkdir-p fish-completions-dir)
                           (with-output-to-file
                             (string-append fish-completions-dir "/rg.fish")
                             (lambda _
                               (invoke rg "--generate" "complete-fish")))))))
      #:features '(list "pcre2")))
    (inputs (cons pcre2 (cargo-inputs 'ripgrep)))
    (native-inputs (cons* pkg-config (if (%current-target-system)
                                         (list this-package)
                                         '())))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Line-oriented search tool and Rust successor to @command{grep}")
    (description
     "@code{ripgrep} (@command{rg}) is a line-oriented search tool that
recursively searches your current directory for a regex pattern while
respecting your gitignore rules. @code{ripgrep} is similar to other popular
search tools like The Silver Searcher, @command{ack} and @command{grep}.")
    (license (list license:unlicense license:expat))))

(define-public rot8
  (package
    (name "rot8")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rot8" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bvb87sr9pkf6sj5ghgmga4nrp5kwiqnllzi672da5vs915xh8li"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'rot8))
    (home-page "https://github.com/efernau/rot8/")
    (synopsis "Automatic display rotation using built-in accelerometer")
    (description "@command{rot8} is a daemon that automates rotating screen and
associated input devices using the built-in accelerometer; handy for convertible
touchscreen devices.")
    (license license:expat)))

(define-public rust-swc
  (package
    (name "rust-swc")
    (version "1.2.129")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swc-project/swc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "06dda65mfm5p819lvzkpqnf7zm3migp5j3584znvq5ickax758br"))
       (modules '((guix build utils)))
       (snippet '(substitute* "Cargo.toml"
                   ((".*git = .*") "")))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-build-flags
       '("--release" "-p" "swc_cli")
       #:cargo-test-flags
       '("--release" "-p" "swc_cli")
       #:cargo-install-paths
       '("crates/swc_cli")))
    (inputs (cargo-inputs 'rust-swc))
    (home-page "https://swc.rs/")
    (synopsis "Typescript/javascript compiler")
    (description "@code{rust-swc} is a typescript/javascript compiler.  It
consumes a javascript or typescript file which uses recently added features
like async-await and emits javascript code which can be executed on old
browsers.")
    (license (list license:expat
                   license:asl2.0))))

(define-deprecated rust-swc-1 rust-swc)

(define-public rust-cargo-edit
  (package
    (name "rust-cargo-edit")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cargo-edit" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1y3pa7qdq2r13ya9qfiw0lb8xm0qbs6q64pj548q6c0hdcxw2mdm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       ;; Not all files included.
       #:cargo-test-flags '("--" "--skip=::case")))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons* curl
            libgit2-1.8
            libssh2
            openssl
            zlib
            (cargo-inputs 'rust-cargo-edit)))
    (home-page "https://github.com/killercup/cargo-edit")
    (synopsis "Add and remove dependencies from the command line")
    (description
     "This package extends Cargo to allow you to add and remove dependencies
by modifying your @file{Cargo.toml} file from the command line.")
    (license (list license:asl2.0 license:expat))))

(define-deprecated rust-cargo-edit-0.8 rust-cargo-edit)

(define-public git-interactive-rebase-tool
  (package
    (name "git-interactive-rebase-tool")
    (version "2.4.1")
    (source
     (origin
       ;; crates.io does not provide the test data.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitmaro/git-interactive-rebase-tool")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1asf1nlnbd915hs288ga67sr6540slgi2a0kmvxy7q4skd4w8n9n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-release-variable
           (lambda _
             (setenv "GIRT_BUILD_GIT_HASH" "GNUGUIX"))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons* libgit2-1.7 zlib (cargo-inputs 'git-interactive-rebase-tool)))
    (home-page "https://gitrebasetool.mitmaro.ca/")
    (synopsis "Terminal based sequence editor for git interactive rebase")
    (description
     "This application is a terminal-based sequence editor for git interactive
rebase.")
    (license license:gpl3+)))

(define-public pastel
  (package
    (name "pastel")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pastel" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mwy4nx3jn74sr1q8ap98faja5wp7hz51yqga8l050xz645kb8wj"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f))
    (inputs (cargo-inputs 'pastel))
    (home-page "https://github.com/sharkdp/pastel")
    (synopsis
     "Command-line tool to generate, analyze, convert and manipulate colors")
    (description
     "Pastel is a command-line tool to generate, analyze, convert and
manipulate colors.  It supports many different color formats and color spaces
like RGB (sRGB), HSL, CIELAB, CIELCh as well as ANSI 8-bit and 24-bit
representations.")
    (license (list license:expat license:asl2.0))))

(define-public prettypst
  (package
    (name "prettypst")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/antonWetzel/prettypst")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0727anhd2wl967m8k5z3bxb37h45nsjbbjz8akjn3mq96cfbfsbw"))))
    (build-system cargo-build-system)
    (arguments (list #:install-source? #f))
    (inputs (cargo-inputs 'prettypst))
    (home-page "https://github.com/antonWetzel/prettypst")
    (synopsis "Configurable formatter for Typst")
    (description
     "Prettypst is a configurable source file formatter for the Typst
typesetting system.")
    (license license:expat)))

(define-public procs
  (package
    (name "procs")
    (version "0.14.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "procs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s2vnfsfrzkqamd0xn7p1gh9m6ja3riksc4waw89hvjph9wng8pn"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'install-manual-page
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (man (string-append out "/share/man/man1")))
                         (mkdir-p man)
                         (invoke "a2x"
                                 "--no-xmllint"
                                 "--doctype=manpage"
                                 "--format=manpage"
                                 "man/procs.1.adoc"
                                 (string-append "--destination-dir=" man)))))
                   (add-after 'install 'install-shell-completions
                     (lambda* (#:key native-inputs outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (share (string-append out "/share"))
                              (bash-completions-dir
                               (string-append out "/etc/bash_completion.d/"))
                              (zsh-completions-dir
                               (string-append share "/zsh/site-functions"))
                              (fish-completions-dir
                               (string-append share "/fish/vendor_completions.d"))
                              (elvish-completions-dir
                               (string-append share "/elvish/lib"))
                              (procs (if #$(%current-target-system)
                                         (search-input-file native-inputs "/bin/procs")
                                         (string-append out "/bin/procs"))))
                         (for-each mkdir-p
                                   (list bash-completions-dir
                                         zsh-completions-dir
                                         fish-completions-dir
                                         elvish-completions-dir))
                         (with-output-to-file
                           (string-append bash-completions-dir "/procs")
                           (lambda _ (invoke procs "--gen-completion-out" "bash")))
                         (with-output-to-file
                           (string-append zsh-completions-dir "/_procs")
                           (lambda _ (invoke procs "--gen-completion-out" "zsh")))
                         (with-output-to-file
                           (string-append fish-completions-dir "/procs.fish")
                           (lambda _ (invoke procs "--gen-completion-out" "fish")))
                         (with-output-to-file
                           (string-append elvish-completions-dir "/procs")
                           (lambda _ (invoke procs "--gen-completion-out" "elvish")))))))))
    (native-inputs
     (append
       (if (%current-target-system)
           (list this-package)
           '())
       (list asciidoc)))
    (inputs (cargo-inputs 'procs))
    (home-page "https://github.com/dalance/procs")
    (synopsis "Modern replacement for @command{ps}")
    (description "This package provides a  modern replacement for @command{ps}
with colored output, multi-column keyword search, additional information, pager
support, watch support (like @command{top}) and a tree view.")
    (license license:expat)))

(define-public rust-cbindgen-0.28
  (package
    (name "rust-cbindgen")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cbindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zyiaifg6mcd4wwhhbxk8adzhph6qz4wxzgagvg3ijp95j58dpga"))))
    (build-system cargo-build-system)
    (arguments (list #:install-source? #f))
    (native-inputs (list python-cython))
    (inputs (cargo-inputs 'rust-cbindgen-0.28))
    (home-page "https://github.com/mozilla/cbindgen")
    (synopsis "Tool for generating C bindings to Rust code")
    (description
     "This package provides a tool for generating C/C++ bindings to Rust code.")
    (license license:mpl2.0)))

(define-public rust-cbindgen-0.27
  (package
    (inherit rust-cbindgen-0.28)
    (name "rust-cbindgen")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cbindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sqm3axr678d72yihgmpr9d17mj99ccibxfqhw53mgzwzkbqvkiz"))))))

(define-public rust-cbindgen-0.26
  (package
    (inherit rust-cbindgen-0.27)
    (name "rust-cbindgen")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cbindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jdbxmn5h5nlr4bifx85gny309djv5djs9q78fa1d7sj0wdw2sys"))))
    (inputs (cargo-inputs 'rust-cbindgen-0.26))))

(define-public rust-cbindgen-0.24
  (package
    (inherit rust-cbindgen-0.26)
    (name "rust-cbindgen")
    (version "0.24.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cbindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13g1k2zljdp326b0cv1nhyh7jsacd364fh0cr2g828hiyfm2z4jb"))))))

(define-public rust-cbindgen-0.23
  (package
    (inherit rust-cbindgen-0.24)
    (name "rust-cbindgen")
    (version "0.23.0")
    (source (origin
             (method url-fetch)
             (uri (crate-uri "cbindgen" version))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "006rn3fn4njayjxr2vd24g1awssr9i3894nbmfzkybx07j728vav"))))))

(define-public rust-cbindgen rust-cbindgen-0.27)

(define-public rust-bindgen-cli
  (package
    (name "rust-bindgen-cli")
    (version "0.71.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p2lmkl7vfhpr8gnav11p1jrwrqsmrqwr2fgwp5x1bsn17511vgx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    (bindgen (string-append bin "/bindgen"))
                    (llvm-dir (string-append
                                (assoc-ref inputs "clang") "/lib")))
               (install-file "target/release/bindgen" bin)
               (wrap-program bindgen
                 `("LIBCLANG_PATH" = (,llvm-dir))))))
         (add-after 'install 'install-completions
           (lambda* (#:key native-inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (bindgen (string-append out "/bin/bindgen")))
               (mkdir-p (string-append out "/etc/bash_completion.d/"))
               (with-output-to-file
                 (string-append out "/etc/bash_completion.d/bindgen")
                 (lambda _ (invoke bindgen "--generate-shell-completions" "bash")))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (with-output-to-file
                 (string-append share "/fish/vendor_completions.d/bindgen.fish")
                 (lambda _ (invoke bindgen "--generate-shell-completions" "fish")))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (with-output-to-file
                 (string-append share "/zsh/site-functions/_bindgen")
                 (lambda _ (invoke bindgen "--generate-shell-completions" "zsh")))
               (mkdir-p (string-append share "/elvish/lib"))
               (with-output-to-file
                 (string-append share "/elvish/lib/bindgen")
                 (lambda _
                   (invoke bindgen "--generate-shell-completions" "elvish")))))))))
    (inputs (cons* bash-minimal clang (cargo-inputs 'rust-bindgen-cli)))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis "Generate Rust FFI bindings to C and C++ libraries")
    (description "This package can be used to automatically generate Rust FFI
bindings to C and C++ libraries.  This package provides the @command{bindgen}
command.")
    (license license:bsd-3)))

(define-public sniffglue
  (package
    (name "sniffglue")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sniffglue" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0dkkw8gwrgawd2s5bg47508i3kjnsv1dwmqa3hlijdvdw4wgm9gz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs
     (cons* libpcap libseccomp (cargo-inputs 'sniffglue)))
    (home-page "https://github.com/kpcyrd/sniffglue")
    (synopsis "Secure multithreaded packet sniffer")
    (description
     "This package provides a network sniffer written in Rust.  Packets
are parsed concurrently using a thread pool to utilize all cpu cores.  A goal
of the project is to be runnable on untrusted networks without crashing.")
    (license license:gpl3)))

(define-public speakersafetyd
  (package
    (name "speakersafetyd")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "speakersafetyd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c4yk8mq8nazshdcasimlgnyhx27wzkad4wzicy5x43grq26b966"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda _
              (substitute* "src/main.rs"
                (("/usr/local") #$output))))
          (add-after 'unpack 'remove-systemd-udev-rules
            (lambda _
              (substitute* "95-speakersafetyd.rules"
                ((".*SYSTEMD_WANTS.*") ""))))
          (add-before 'install 'prepare-to-install
            (lambda _
              (setenv "DESTDIR" #$output)
              (setenv "SHAREDIR" "/share")
              (setenv "SPEAKERSAFETYD_GROUP" "nixbld")
              (setenv "SPEAKERSAFETYD_USER" "nixbld")
              (invoke "make" "install"))))))
    (inputs (cons alsa-lib (cargo-inputs 'speakersafetyd)))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/AsahiLinux/speakersafetyd/")
    (synopsis "Speaker protection daemon")
    (description "Speakersafetyd is a userspace daemon written in Rust that
implements an analogue of the Texas Instruments Smart Amp speaker protection
model.")
    (license license:expat)))

(define-public tectonic
  (package
    (name "tectonic")
    (version "0.15.0")
    (source
     (origin
       ;; Grab all the sources instead of each packaged crate in the workspace.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tectonic-typesetting/tectonic")
             (commit (string-append name "@" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02wchm7kmfsw8y71x84hlk9qf5ldvj2ir7j8pcq2a09wlj4xi4f5"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "crates/bridge_harfbuzz/harfbuzz")))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:install-source? #f
       #:features '(list "external-harfbuzz")
       #:cargo-test-flags '(list "--features" "external-harfbuzz")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'install-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (doc (string-append out "/share/doc/" #$name "-" #$version)))
                 (copy-recursively "docs/src" doc)))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons* fontconfig
            freetype
            graphite2
            harfbuzz
            icu4c
            openssl
            (cargo-inputs 'tectonic)))
    (home-page "https://tectonic-typesetting.github.io/")
    (synopsis "Complete, embeddable TeX/LaTeX engine")
    (description
     "This package provides a modernized, complete, embeddable
TeX/LaTeX engine.  Tectonic is forked from the XeTeX extension to the
classic Web2C implementation of TeX and uses the TeXLive distribution
of support files.")
    (license license:expat)))

(define-public treefmt
  (package
    (name "treefmt")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "treefmt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pfx8kgaf0rc8ijps2fqb61gjnak3sf430hvg52bnby9qqyd51h8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'treefmt))
    (home-page "https://numtide.github.io/treefmt")
    (synopsis "Command-line application to format the code tree")
    (description
     "This application provides a way to unify the formatting process of the
codebase.  It is nice for large code trees where using multiple formatters are
common.  @command{treefmt} comes with the following features.

@itemize
@item Unified CLI and output.
@item Runs formatters in parallel.
@item Cache changed files for performance.
@end itemize

The application does have some design decisions to keep in mind.

@itemize
@item The source code is kept under version control, making it possible to
revert and check changes.
@item Only one formatter per file, making outputs idempotent.
@end itemize")
    (license license:expat)))

(define-public hex
  (package
    (name "hex")
    (version "0.6.0")
    (source
     (origin
       ;; crates.io does not provide the test data.
       ;; Not all releases are pushed to crates.io.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sitkevij/hex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kv07ghibifs6rnskg1na6a0hdb0f8vqfbpv5k8g09lc2075gjv1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags '("--"
                            ;; Not all files included.
                            "--skip=tests::test_cli_arg_order_1"
                            "--skip=tests::test_cli_arg_order_2"
                            "--skip=tests::test_cli_input_directory"
                            "--skip=tests::test_cli_input_missing_file"
                            "--skip=tests::test_cli_input_stdin"
                            "--skip=tests::test_cli_missing_param_value")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "hx.1" (string-append out "/share/man/man1"))))))))
    (inputs (cargo-inputs 'hex))
    (home-page "https://github.com/sitkevij/hex")
    (synopsis "Hexadecimal colorized view of a file")
    (description
     "@command{hx} accepts a file path as input and outputs a hexadecimal
colorized view to stdout.")
    (license license:expat)))

(define-public tokei
  (package
    (name "tokei")
    (version "12.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokei" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "000w549v1bpw7r57xw656p40ywf1gimvxxx5cjnri2js0xg927x4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons* libgit2 openssl zlib (cargo-inputs 'tokei)))
    (home-page "https://tokei.rs")
    (synopsis "Count code, quickly")
    (description
     "Tokei is a program that displays statistics about your code.  Tokei will
show number of files, total lines within those files and code, comments, and
blanks grouped by language.")
    (license (list license:expat license:asl2.0))))

(define-public typst
  (package
    (name "typst")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typst-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10xnxf6z78hcck7647vfq9vigrvvz0a6g4ha4l4vn5zlarrxwd56"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:modules '((guix build cargo-build-system)
                  (guix build utils)
                  (ice-9 match)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version-string
            (lambda _
              (setenv "TYPST_VERSION" #$(package-version this-package))))
          (add-after 'configure 'configure-artifacts
            (lambda _
              (mkdir "artifacts")
              (setenv "GEN_ARTIFACTS" "artifacts")))
          (add-after 'install 'install-artifacts
            (lambda _
              (with-directory-excursion "artifacts"
                (for-each (cut install-file <>
                               (string-append #$output "/share/man/man1"))
                          (find-files "." "\\.1$"))
                (rename-file "typst.bash" "typst")
                (map
                 (match-lambda
                   ((file . loc)
                    (install-file file (string-append #$output "/share" loc))))
                 '(("typst" . "/bash-completion/completions")
                   ("_typst" . "/zsh/site-functions")
                   ("typst.elv" . "/elvish/lib")
                   ("typst.fish" . "/fish/vendor_completions.d")))))))))
    (inputs (cons* openssl (cargo-inputs 'typst)))
    (native-search-paths
     (list (search-path-specification
            (variable "TYPST_PACKAGE_PATH")
            (files '("share/typst/packages"))
            (separator #f))
           (search-path-specification
            (variable "TYPST_FONT_PATHS")
            (files '("share/fonts" "share/texmf-dist/fonts")))))
    (home-page "https://typst.app/")
    (synopsis "LaTeX-like typesetting system")
    (description
     "Typst is a markup-based typesetting system that is designed to be as
powerful as LaTeX while being much easier to learn and use.  Features include
built-in markup for math typesetting, bibliography management and other common
tasks, an extensible scripting system for uncommon tasks, incremental
compilation, and intuitive error messages.")
    (license license:asl2.0)))

(define-public typstyle
  (package
    (name "typstyle")
    (version "0.13.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typstyle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06mg12rkls1hkiz8wxchj1jqf1l1bq963s80mrvjfiajb08zqdx1"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:modules
      '((guix build cargo-build-system)
        (guix build utils)
        (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-completions
            (lambda* (#:key native-inputs #:allow-other-keys)
              (for-each
               (match-lambda
                 ((shell . path)
                  (mkdir-p (in-vicinity #$output (dirname path)))
                  (let ((binary
                         (if #$(%current-target-system)
                             (search-input-file native-inputs "bin/typstyle")
                             (in-vicinity #$output "bin/typstyle"))))
                    (with-output-to-file (in-vicinity #$output path)
                      (lambda _
                        (invoke binary "completions" shell))))))
               '(("bash"   . "share/bash-completion/completions/typstyle")
                 ("elvish" . "share/elvish/lib/typstyle")
                 ("fish"   . "share/fish/vendor_completions.d/typstyle.fish")
                 ("zsh"    . "share/zsh/site-functions/_typstyle"))))))))
    (native-inputs
     (if (%current-target-system)
         (list this-package)
         '()))
    (inputs (cargo-inputs 'typstyle))
    (home-page "https://enter-tainer.github.io/typstyle/")
    (synopsis "Consistent formatter for Typst")
    (description
     "Typstyle is a formatter for the Typst typesetting system designed with
universal consistency and correctness as top priorities.  It is
configuration-free.")
    (license license:asl2.0)))

(define-public vivid
  (package
    (name "vivid")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vivid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xynznf7drvhjhhnwdxrbjgr6qgfa5lzwxxqdclnjvzwkbhl2i2q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'vivid))
    (home-page "https://github.com/sharkdp/vivid")
    (synopsis "LS_COLORS environment variable manager")
    (description
     "vivid is a generator for the @code{LS_COLORS} environment variable that
controls the colorized output of ls, tree, fd, bfs, dust and many other tools.

It uses a YAML configuration format for the filetype-database and the color
themes.  In contrast to @command{dircolors}, the database and the themes are
organized in different files.  This allows users to choose and customize color
themes independent from the collection of file extensions.  Instead of using
cryptic ANSI escape codes, colors can be specified in the RRGGBB format and
will be translated to either truecolor (24-bit) ANSI codes or 8-bit codes for
older terminal emulators.")
    (license (list license:expat license:asl2.0))))

(define-public watchexec
  (package
    (name "watchexec")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "watchexec-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qqbcipx90q4hl1l39ijwqqndbd23kmkqha3wxpqn1b8dylfxgy2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zsh (string-append out "/share/zsh/site-functions/_watchexec"))
                    (doc (string-append out "/share/doc/watchexec-" ,version)))
               (mkdir-p (dirname zsh))
               ;; FIXME: The crates.io source does not provide zsh
               ;; completions.  But the GitHub source does not compile.
               ;;
               ;; (copy-file "completions/zsh" zsh)
               (install-file "README.md" doc)))))))
    (inputs (cargo-inputs 'watchexec))
    (home-page "https://github.com/watchexec/watchexec")
    (synopsis "Executes commands in response to file modifications")
    (description
     "@command{watchexec} is a simple, standalone tool that watches a path and
runs a command whenever it detects modifications.")
    (license license:asl2.0)))

(define-public rbw
  (package
    (name "rbw")
    (version "1.13.2")
    (outputs '("out" "scripts"))
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rbw" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1plwv71iwdcdprknsn32x7wzlg1hnikq3wqbym4yiwpk5kf6anmm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key native-inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (rbw (if ,(%current-target-system)
                          (search-input-file native-inputs "/bin/rbw")
                          (string-append out "/bin/rbw"))))
               (mkdir-p (string-append out "/etc/bash_completion.d"))
               (with-output-to-file
                 (string-append out "/etc/bash_completion.d/rbw")
                 (lambda _ (invoke rbw "gen-completions" "bash")))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (with-output-to-file
                 (string-append share "/fish/vendor_completions.d/rbw.fish")
                 (lambda _ (invoke rbw "gen-completions" "fish")))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (with-output-to-file
                 (string-append share "/zsh/site-functions/_rbw")
                 (lambda _ (invoke rbw "gen-completions" "zsh")))
               (mkdir-p (string-append share "/elvish/lib"))
               (with-output-to-file
                 (string-append share "/elvish/lib/rbw")
                 (lambda _ (invoke rbw "gen-completions" "elvish"))))))
         (add-after 'install 'install-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (scripts (assoc-ref outputs "scripts")))
               (for-each (lambda (file)
                           (install-file file (string-append scripts "/bin")))
                         (find-files "bin"))
               (for-each (lambda (file)
                           (wrap-script file
                             ;; TODO: Do we want to wrap these with more programs?
                             ;; pass git fzf libsecret xclip rofi
                             `("PATH" prefix
                                (,(string-append out "/bin")
                                 ,(dirname (search-input-file inputs "/bin/grep"))
                                 ,(dirname (search-input-file inputs "/bin/sed"))
                                 ,(dirname (search-input-file inputs "/bin/perl"))
                                 ,(dirname (search-input-file inputs "/bin/xargs"))
                                 ,(dirname (search-input-file inputs "/bin/sort"))))))
                         (find-files (string-append scripts "/bin")))))))))
    (native-inputs
     (cons* perl (if (%current-target-system)
                   (list this-package)
                   '())))
    (inputs
     (cons* coreutils-minimal findutils grep perl sed (cargo-inputs 'rbw)))
    (home-page "https://git.tozt.net/rbw")
    (synopsis "Unofficial Bitwarden CLI")
    (description "This package is an unofficial command line client for
Bitwarden.  Although Bitwarden ships with a command line client, but
it's limited by being stateless, which makes it very difficult to use.  This
client avoids that problem by maintaining a background process which is able
to hold the keys in memory, similar to the way that ssh-agent or gpg-agent
work.  This allows the client to be used in a much simpler way, with the
background agent taking care of maintaining the necessary state.")
    (license license:expat)))

;; Note: Keep it in sync with our current rust:cargo version.
(define-public rust-cargo-c
  (package
    (name "rust-cargo-c")
    (version "0.10.9+cargo-0.85.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargo-c" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1cxawccpssmpvv6a0sn8lkms5nd3gyh46g407bk2i8xyzyh87pvq"))))
    (build-system cargo-build-system)
    (arguments (list #:install-source? #f))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons* curl
            libgit2-1.8
            libssh2
            openssl
            sqlite
            zlib
            (cargo-inputs 'rust-cargo-c)))
    (home-page "https://github.com/lu-zero/cargo-c")
    (synopsis "Build and install C-compatible libraries")
    (description
     "This package produces and installs a correct pkg-config file, a static
library and a dynamic library, and a C header to be used by any C (and
C-compatible) software.")
    (license license:expat)))

(define-public rtss
  (package
    (name "rtss")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rtss" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r1b6fynkjnpj5p3k209sa13mjvh4k0ghzwnribm48dh9v7lfnnv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'rtss))
    (home-page "https://github.com/Freaky/rtss")
    (synopsis "Annotate stdout/stderr with elapsed times")
    (description "@code{rtss} annotates its output with relative durations between
consecutive lines and since program start.")
    (license license:expat)))

(define-public sd
  (package
    (name "sd")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a16p1s0j28n3vj006qm7b03k5s9mkr11cbbksvfb88wi10kqqbh"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-extras
            (lambda _
              (let ((share (string-append #$output "/share/"))
                    (bash-dir (string-append #$output "/etc/bash_completion.d/"))
                    (elvish-dir (string-append #$output "/share/elvish/lib/")))
                (install-file "gen/sd.1" (string-append share "/man/man1"))
                (with-directory-excursion "gen/completions"
                  (install-file "_sd" (string-append share "zsh/site-functions"))
                  (install-file "sd.fish"
                                (string-append share "fish/vendor_completions.d"))
                  (mkdir-p bash-dir)
                  (mkdir-p elvish-dir)
                  (copy-file "sd.bash" (string-append bash-dir "sd"))
                  (copy-file "sd.elv" (string-append elvish-dir "sd")))))))))
    (inputs (cargo-inputs 'sd))
    (home-page "https://github.com/chmln/sd")
    (synopsis "Intuitive find & replace CLI")
    (description "@code{sd} is an intuitive find & replace CLI with
JavaScript/Python-style regular expressions, a string-literal mode, and smart,
common-sense defaults.")
    (license license:expat)))

(define-public skim
  (package
    (name "skim")
    (version "0.16.1")
    (source
     (origin
       ;; crates.io doesn't have everything needed.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lotabout/skim")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00k487pqhifcf9jvx7acwyhag34g5zrn49hry7xiwdflpqq4x1cl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases (modify-phases %standard-phases
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (sk  (car (find-files "target" "^sk$"))))
                        (install-file sk bin))))
                  (add-after 'build 'build-extras
                    (lambda _
                      ;; Delete the manpages and completions before rebuilding.
                      (for-each delete-file '("man/man1/sk.1"
                                              "shell/completion.bash"
                                              "shell/completion.zsh"))
                      (invoke "cargo" "run" "--package" "xtask" "mangen")
                      (invoke "cargo" "run" "--package" "xtask" "compgen")))
                  (add-after 'install 'install-extras
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (share (string-append out "/share"))
                             (man (string-append out "/share/man"))
                             (vimfiles (string-append share
                                        "/vim/vimfiles/pack/guix/start/skim/plugin"))
                             (bash-completion (string-append out
                                               "/etc/bash_completion.d"))
                             (zsh-site (string-append share
                                                      "/zsh/site-functions"))
                             (fish-vendor (string-append share
                                           "/fish/vendor-completions.d")))
                        ;; Binaries
                        (for-each (lambda (binary)
                                    (install-file binary bin))
                                  (find-files "bin"))
                        (mkdir-p share)
                        ;; Manpages
                        (copy-recursively "man" man)
                        ;; Vim plugins
                        (mkdir-p vimfiles)
                        (copy-recursively "plugin" vimfiles)
                        ;; Completions
                        (mkdir-p bash-completion)
                        (copy-file "shell/completion.bash"
                                   (string-append bash-completion "/skim"))
                        (copy-file "shell/key-bindings.bash"
                                   (string-append bash-completion
                                                  "/skim-bindings"))
                        (mkdir-p zsh-site)
                        (copy-file "shell/completion.zsh"
                                   (string-append zsh-site "/_skim"))
                        (copy-file "shell/key-bindings.zsh"
                                   (string-append zsh-site "/_skim-bindings"))
                        (mkdir-p fish-vendor)
                        (copy-file "shell/key-bindings.fish"
                                   (string-append fish-vendor
                                                  "/skim-bindings.fish"))))))))
    (inputs (cargo-inputs 'skim))
    (home-page "https://github.com/lotabout/skim")
    (synopsis "Fuzzy Finder in Rust")
    (description "This package provides a fuzzy finder in Rust.")
    (license license:expat)))

(define-public spotifyd
  (package
    (name "spotifyd")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "spotifyd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m0z1q35by9wskjy2imnf8g8liy4g2ljyd69z6fryvvw64j0qzd6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:features (list "alsa_backend"
                        "dbus_mpris"
                        "pulseaudio_backend"
                        "rodio_backend")))
    (native-inputs (list pkg-config))
    (inputs (cons* alsa-lib dbus pulseaudio (cargo-inputs 'spotifyd)))
    (home-page "https://github.com/Spotifyd/spotifyd")
    (synopsis "Spotify streaming daemon with Spotify Connect support")
    (description
     "This package provides a light-weight daemon that connects to the Spotify
music service.  A Spotifyd instance can be controlled by clients that use the
Spotify Connect protocol, which includes the official Spotify mobile apps.")
    (license license:gpl3)))

(define-public svd2rust
  (package
    (name "svd2rust")
    (version "0.36.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "svd2rust" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0pfbvgxp49j04f1zxgq5n5csv7b1ysnnmafnyal08yisrvg476nw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'svd2rust))
    (home-page "https://github.com/rust-embedded/svd2rust/")
    (synopsis "Generate Rust register maps (`struct`s) from SVD files")
    (description
     "This program can be used to generate Rust register maps (`struct`s) from SVD
files.")
    (license (list license:expat license:asl2.0))))

(define-public swayhide
  (package
    (name "swayhide")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "swayhide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0synzfd35494vlp2wnqmqbzgc0vg2ivn90hnxvk6qak0w65xhxcv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completion.d/"))
                    (fish (string-append out "/share/fish/vendor_completions.d/"))
                    (zsh  (string-append out "/share/zsh/site-functions/")))
               (mkdir-p bash)
               (mkdir-p zsh)
               (copy-file "completions/swayhide.bash"
                          (string-append bash "swayhide"))
               (copy-file "completions/swayhide.zsh"
                          (string-append zsh "_swayhide"))
               (install-file "completions/swayhide.fish" fish)))))))
    (inputs (cargo-inputs 'swayhide))
    (home-page "https://github.com/NomisIV/swayhide/")
    (synopsis "Swallow windows on swaywm")
    (description "swayhide hides the currently active terminal (by moving it
to the scratchpad), then it executes the supplied command.  When the child
process has finished, the terminal is moved back.  This is useful if your
workflow includes opening graphical programs from the terminal, as the locked
terminal won't have to take up any space.")
    (license license:gpl3+)))

(define-public swayr
  (package
   (name "swayr")
   (version "0.28.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "swayr" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1q3fim66q74pxsv9vhhcxgdnsjmkz08adsf4ng61sxxvf3hry3lf"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-test-flags
      '("--"
        "--skip=config::test_load_swayr_config")
      #:install-source? #f))
   (inputs (cargo-inputs 'swayr))
   (home-page "https://sr.ht/~tsdh/swayr/")
   (synopsis "Window-switcher for the sway window manager")
   (description
    "This package provides a last-recently-used window-switcher for the sway
window manager.  Swayr consists of a daemon, and a client.  The swayrd daemon
records window/workspace creations, deletions, and focus changes using sway's
JSON IPC interface.  The swayr client offers subcommands, and sends them to the
daemon which executes them.")
   (license license:gpl3+)))

(define-public swayrbar
  (package
    (name "swayrbar")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "swayrbar" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05jpa87i6q1cpikyqqliy3q2ksslj79kgin8jq9ls6073yk5q6z7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=config::test_load_swayrbar_config")
       #:install-source? #f))
    (inputs (cargo-inputs 'swayrbar))
    (home-page "https://sr.ht/~tsdh/swayr/#swayrbar")
    (synopsis "Swaybar-protocol implementation for sway/swaybar")
    (description
     "This package provides a swaybar-protocol implementation for sway/swaybar.")
    (license license:gpl3+)))

(define-public swaysome
  (package
    (name "swaysome")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "swaysome" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12rqvjj9d12nm9zppgp4hvfw5l308gn9ljbbgbhi0cglpg11rnjk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (man1 (string-append out "/share/man/man1")))
               (install-file "swaysome.1" man1)))))))
    (inputs (cargo-inputs 'swaysome))
    (home-page "https://gitlab.com/hyask/swaysome")
    (synopsis "Manage your multiple outputs with the sway window manager")
    (description
     "This package provides a way to manage your multiple outputs with the sway
window manager.")
    (license license:expat)))

(define-public tealdeer
  (package
    (name "tealdeer")
    (version "1.7.2")
    (source
     (origin
       ;; Completions aren't in the release tarball.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dbrgn/tealdeer")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0yy0cvbw3n16whnqqycr54w18kiv6jlc3v0pl0276zvn9mc7p4qr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completion.d/"))
                    (fish (string-append out "/share/fish/vendor_completions.d/"))
                    (zsh  (string-append out "/share/zsh/site-functions/")))
               (mkdir-p bash)
               (mkdir-p fish)
               (mkdir-p zsh)
               (copy-file "completion/bash_tealdeer"
                          (string-append bash "tealdeer"))
               (copy-file "completion/fish_tealdeer"
                          (string-append fish "tealdeer.fish"))
               (copy-file "completion/zsh_tealdeer"
                          (string-append zsh "_tealdeer"))))))
       #:install-source? #f
       #:cargo-test-flags
       '("--"
         ;; These tests go to the network
         "--skip=test_quiet_old_cache"
         "--skip=test_quiet_cache"
         "--skip=test_quiet_failures"
         "--skip=test_pager_flag_enable"
         "--skip=test_markdown_rendering"
         "--skip=test_spaces_find_command"
         "--skip=test_autoupdate_cache"
         "--skip=test_update_cache"
         "--skip=test_create_cache_directory_path")))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons openssl (cargo-inputs 'tealdeer)))
    (home-page "https://github.com/dbrgn/tealdeer/")
    (synopsis "Fetch and show tldr help pages for many CLI commands")
    (description
     "This package fetches and shows tldr help pages for many CLI commands.
Full featured offline client with caching support.")
    (license (list license:expat license:asl2.0))))

(define-public uv
  (package
    (name "uv")
    (version "0.6.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "uv" version))
       (sha256
        (base32 "14ajgsl7zzsrig1vppcgs77q4fqg5w858jxma9hqab4b8nrpzxmn"))
       (modules '((guix build utils)))
       (snippet
        #~(for-each delete-file
                    (find-files "crates/uv-trampoline/trampolines"
                                "\\.exe$")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:imported-modules
      (append %cargo-build-system-modules
              %pyproject-build-system-modules)
      #:modules
      '((srfi srfi-26)
        (ice-9 match)
        ((guix build cargo-build-system) #:prefix cargo:)
        (guix build pyproject-build-system)
        (guix build utils))
      #:tests? #f  ; Tests require multiple python versions and network access.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-guix-vendored-dependencies
            (lambda _
              (substitute* "Cargo.toml"
                (("git.*, rev.*}")
                 "version = \"*\"}"))))
          (add-after 'unpack 'prepare-cargo-build-system
            (lambda args
              (for-each
               (lambda (phase)
                 (format #t "Running cargo phase: ~a~%" phase)
                 (apply (assoc-ref cargo:%standard-phases phase)
                        args))
               '(unpack-rust-crates
                 configure
                 check-for-pregenerated-files
                 patch-cargo-checksums))))
          (add-before 'build 'override-jemalloc
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((jemalloc (assoc-ref inputs "jemalloc")))
                ;; This flag is needed when not using the bundled jemalloc.
                ;; https://github.com/tikv/jemallocator/issues/19
                (setenv "CARGO_FEATURE_UNPREFIXED_MALLOC_ON_SUPPORTED_PLATFORMS" "1")
                (setenv "JEMALLOC_OVERRIDE"
                        (string-append jemalloc "/lib/libjemalloc.so")))))
          (replace 'install
            ;; We can't use the pyproject install phase because uv is a
            ;; binary, not a python script.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (wheel (car (find-files "dist" "\\.whl$")))
                    (site-dir (site-packages inputs outputs))
                    (pyversion
                     (string-append "python"
                                    (python-version
                                     (assoc-ref inputs "python")))))
                (invoke "python" "-m" "zipfile" "-e" wheel site-dir)
                (mkdir-p (string-append out "/bin"))
                (for-each delete-file
                          (find-files (string-append out "/lib/" pyversion)
                                      "^uvx?$"))
                (for-each (cut install-file <> (string-append out "/bin"))
                          (find-files "target" "^uvx?$")))))
          (replace 'check
            (lambda args
              (setenv "HOME" (getcwd))
              ;; NOTE: ‘#:tests?’ is honored here.
              (apply (assoc-ref cargo:%standard-phases 'check) args)))
          (add-after 'install 'install-extras
            (lambda* (#:key native-inputs #:allow-other-keys)
              (let ((uv (if #$(%current-target-system)
                            (search-input-file native-inputs "/bin/uv")
                            (string-append #$output "/bin/uv")))
                    (uvx (if #$(%current-target-system)
                             (search-input-file native-inputs "/bin/uvx")
                             (string-append #$output "/bin/uvx"))))
                (for-each
                 (match-lambda
                   ((shell uv-name uvx-name completions-dir)
                    (mkdir-p completions-dir)
                    (with-output-to-file (in-vicinity completions-dir uv-name)
                      (lambda _
                        (invoke uv "generate-shell-completion" shell)))
                    (with-output-to-file (in-vicinity completions-dir uvx-name)
                      (lambda _
                        (invoke uvx "--generate-shell-completion" shell)))))
                 `(("bash" "uv" "uvx"
                    ,(string-append #$output "/etc/bash_completion.d"))
                   ("zsh" "_uv" "_uvx"
                    ,(string-append #$output "/share/zsh/site-functions"))
                   ("fish" "uv.fish" "uvx.fish"
                    ,(string-append #$output "/share/fish/vendor_completions.d"))
                   ("elvish" "uv" "uvx"
                    ,(string-append #$output "/share/elvish/lib"))
                   ("nushell" "uv" "uvx"
                    ,(string-append #$output "/share/nushell/vendor/autoload"))))))))))
    (native-inputs
     (append
      (list maturin pkg-config rust `(,rust "cargo"))
      (if (%current-target-system)
          (list this-package
                (make-rust-sysroot (%current-target-system)))
          '())))
    (inputs (cons* jemalloc xz `(,zstd "lib") (cargo-inputs 'uv)))
    (home-page "https://docs.astral.sh/uv/")
    (synopsis "Python package and project manager written in Rust")
    (description
     "@command{uv} is a high-performance Python package and project manager
written in Rust, known for its execution speed and compatibility with existing
tools.")
    (license (list license:asl2.0 license:expat))))

(define-public git-absorb
  (package
    (name "git-absorb")
    (version "0.7.0")
    (source
     (origin
       ;; crates.io does not include the manual page.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tummychow/git-absorb")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cqz9m5w7sh7pgfcirvdpv6i7l7nfaqbgs88zx0wr5sqg5wk2zky"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-manual-page
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1")))
               (with-directory-excursion "Documentation"
                 (invoke "a2x"
                         "--no-xmllint"
                         "--doctype=manpage"
                         "--format=manpage"
                         "git-absorb.adoc"))
               (install-file "Documentation/git-absorb.1" man))))
         (add-after 'install 'install-completions
           (lambda* (#:key native-inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (git-absorb
                      (if ,(%current-target-system)
                          (search-input-file native-inputs "/bin/git-absorb")
                          (string-append out "/bin/git-absorb"))))
               (mkdir-p (string-append out "/etc/bash_completion.d"))
               (with-output-to-file
                 (string-append out "/etc/bash_completion.d/git-absorb")
                 (lambda _ (invoke git-absorb "--gen-completions" "bash")))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (with-output-to-file
                 (string-append share "/fish/vendor_completions.d/git-absorb.fish")
                 (lambda _ (invoke git-absorb "--gen-completions" "fish")))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (with-output-to-file
                 (string-append share "/zsh/site-functions/_git-absorb")
                 (lambda _ (invoke git-absorb "--gen-completions" "zsh")))
               (mkdir-p (string-append share "/elvish/lib"))
               (with-output-to-file
                 (string-append share "/elvish/lib/git-absorb")
                 (lambda _ (invoke git-absorb "--gen-completions" "elvish")))))))))
    (native-inputs
     (append
       (if (%current-target-system)
           (list this-package)
           '())
       (list asciidoc git-minimal/pinned pkg-config)))
    (inputs
     (cons* libgit2-1.9 zlib (cargo-inputs 'git-absorb)))
    (home-page "https://github.com/tummychow/git-absorb")
    (synopsis "Git tool for making automatic fixup commits")
    (description
     "@code{git absorb} automatically absorbs staged changes into their
current branch.  @code{git absorb} will automatically identify which commits
are safe to modify, and which staged changes belong to each of those commits.
It will then write @code{fixup!} commits for each of those changes.")
    (license license:bsd-3)))

(define-public git-delta
  (package
    (name "git-delta")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-delta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bmjan13lm1d6vcy8mh0iryl2rnvh39ml5y4alf6s728xdzc2yhj"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-test-flags
      '(list "--release" "--"
             "--skip=ansi::tests::test_measure_text_width"
             "--skip=features::line_numbers::tests::test_line_numbers_continue_correctly_after_wrapping"
             "--skip=features::side_by_side::tests::test_two_plus_lines_exact_fit"
             "--skip=handlers::diff_header::tests::test_diff_header_relative_paths"
             "--skip=tests::test_example_diffs::tests::test_binary_file_added"
             "--skip=tests::test_example_diffs::tests::test_binary_file_removed"
             "--skip=tests::test_example_diffs::tests::test_binary_files_differ"
             "--skip=tests::test_example_diffs::tests::test_binary_files_differ_after_other"
             "--skip=wrapping::tests::test_alignment_1_line_vs_3_lines"
             "--skip=wrapping::tests::test_alignment_2_lines_vs_3_lines"
             "--skip=wrapping::tests::test_wrap_line_newlines")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share"))
                     (bash-completions-dir
                      (string-append out "/etc/bash-completion.d"))
                     (zsh-completions-dir
                      (string-append share "/zsh/site-functions"))
                     (fish-completions-dir
                      (string-append share "/fish/vendor_completions.d")))
                (mkdir-p bash-completions-dir)
                (mkdir-p zsh-completions-dir)
                (mkdir-p fish-completions-dir)
                (copy-file "etc/completion/completion.bash"
                           (string-append bash-completions-dir "/delta"))
                (copy-file "etc/completion/completion.zsh"
                           (string-append zsh-completions-dir "/_delta"))
                (copy-file "etc/completion/completion.fish"
                           (string-append fish-completions-dir "/delta.fish"))))))))
    (native-inputs (list git-minimal pkg-config))
    (inputs
     (cons* libgit2-1.7
            oniguruma
            openssl
            zlib
            (cargo-inputs 'git-delta)))
    (home-page "https://github.com/dandavison/delta")
    (synopsis "Syntax-highlighting pager for git")
    (description
     "This package provides a syntax-highlighting pager for @command{git}.  It
uses @command{bat} for syntax highlighting and provides many features such as
advanced keybindings, word-level diff highlighting, syntax highlighting for
@command{grep} and a stylized box presentation.")
    (license license:expat)))

(define-public wallust
  (package
    (name "wallust")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wallust" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08k593k7ixddf5z98drhzg2a858gal15dc8ih3gpsszz5q73y2ly"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:features '(list "buildgen")
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((share (string-append #$output "/share"))
                     (install-man (lambda (filename)
                                    (install-file filename
                                                  (string-append share
                                                                 "/man/man"
                                                                 (string-take-right
                                                                  filename 1))))))
                (mkdir-p (string-append #$output "/etc/bash_completion.d"))
                (mkdir-p (string-append share "/elvish/lib"))
                (copy-file "completions/wallust.bash"
                           (string-append #$output
                                          "/etc/bash_completion.d/wallust"))
                (copy-file "completions/wallust.elv"
                           (string-append share "/elvish/lib/wallust"))
                (install-file "completions/wallust.fish"
                              (string-append share
                                             "/fish/vendor_completions.d/"))
                (install-file "completions/_wallust"
                              (string-append share "/zsh/site-functions/"))
                (with-directory-excursion "man"
                  (for-each install-man
                            (find-files ".")))))))))
    (native-inputs (list pkg-config))
    (inputs (cons* libgit2-1.9 zlib (cargo-inputs 'wallust)))
    (home-page "https://explosion-mental.codeberg.page/wallust")
    (synopsis "Generate themes from images with advanced templating")
    (description
     "Wallust is a rust-based successor to \"pywal\" which generates color
themes from images with advanced templating and generation options.
Templates are made using either a subset of Jinja2 or pywal syntax.
Color generation may be constrained to obey ANSI color standards, meet
minimum contrast levels, and more.")
    (license license:expat)))

(define-public rust-xremap
  (package
    (name "rust-xremap")
    (version "0.10.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xremap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "00wi8xygkgj7niq4vky7jpg8yr02yrqlkvrhgnc6vbw87n7h5fmi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '()
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (xremap (string-append out "/bin/xremap")))
               (mkdir-p (string-append out "/etc/bash_completion.d"))
               (with-output-to-file
                 (string-append out "/etc/bash_completion.d/xremap")
                 (lambda _ (invoke xremap "--completions" "bash")))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (with-output-to-file
                 (string-append share "/fish/vendor_completions.d/xremap.fish")
                 (lambda _ (invoke xremap "--completions" "fish")))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (with-output-to-file
                 (string-append share "/zsh/site-functions/_xremap")
                 (lambda _ (invoke xremap "--completions" "zsh")))
               (mkdir-p (string-append share "/elvish/lib"))
               (with-output-to-file
                 (string-append share "/elvish/lib/xremap")
                 (lambda _ (invoke xremap "--completions" "elvish")))))))))
    (inputs (cargo-inputs 'rust-xremap))
    (home-page "https://github.com/k0kubun/xremap")
    (synopsis "Dynamic key remapper for X and Wayland")
    (description "This package provides dynamic key remapper for X and Wayland.")
    (license license:expat)))

(define-public xremap-gnome
  (package
    (inherit rust-xremap)
    (name "xremap-gnome")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-xremap)
       ((#:features _) '(list "gnome"))))))

(define-public xremap-kde
  (package
    (inherit rust-xremap)
    (name "xremap-kde")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-xremap)
       ((#:features _) '(list "kde"))))))

(define-public xremap-wlroots
  (package
    (inherit rust-xremap)
    (name "xremap-wlroots")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-xremap)
       ((#:features _) '(list "wlroots"))))))

(define-public xremap-hyprland
  (package
    (inherit rust-xremap)
    (name "xremap-hyprland")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-xremap)
       ((#:features _) '(list "hyprland"))))))

(define-public xremap-sway
  (deprecated-package "xremap-sway" xremap-wlroots))

(define-public xremap-x11
  (package
    (inherit rust-xremap)
    (name "xremap-x11")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-xremap)
       ((#:features _) '(list "x11"))))))

(define-public xsv
  (package
    (name "xsv")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xsv" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pvzr7x5phlya6m5yikvy13vgbazshw0plysckz9zmf2ly5x4jl8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'xsv))
    (home-page "https://github.com/BurntSushi/xsv")
    (synopsis "High performance CSV command line toolkit")
    (description
     "This package provides a high performance CSV command line toolkit.")
    (license (list license:unlicense license:expat))))

(define-public zoxide
  (package
    (name "zoxide")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zoxide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xwbc9zjglgzzxk23qyg2924gkyaclc844jcg1apx0190r4qlc3z"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-references
                 (lambda _
                   (substitute* (find-files "templates")
                     (("zoxide (add|query)" all)
                      (string-append #$output "/bin/" all))
                     (("(zoxide = \")(zoxide)" _ prefix suffix)
                      (string-append prefix #$output "/bin/" suffix)))))
               (add-after 'install 'install-more
                 (lambda _
                   (let* ((out #$output)
                          (share (string-append out "/share"))
                          (man1 (string-append share "/man/man1"))
                          (bash-completions-dir
                            (string-append out "/etc/bash-completion.d"))
                          (zsh-completions-dir
                            (string-append share "/zsh/site-functions"))
                          (fish-completions-dir
                            (string-append share "/fish/vendor_completions.d"))
                          (elvish-completions-dir
                            (string-append share "/elvish/lib")))
                     ;; The completions are generated in build.rs.
                     (mkdir-p man1)
                     (mkdir-p bash-completions-dir)
                     (mkdir-p elvish-completions-dir)
                     (for-each (lambda (file)
                                 (install-file file man1))
                               (find-files "man/man1"))
                     (copy-file "contrib/completions/zoxide.bash"
                                (string-append bash-completions-dir "/zoxide"))
                     (install-file "contrib/completions/zoxide.fish"
                                   fish-completions-dir)
                     (install-file "contrib/completions/_zoxide"
                                   zsh-completions-dir)
                     (copy-file "contrib/completions/zoxide.elv"
                                (string-append elvish-completions-dir
                                               "/zoxide"))))))))
    (inputs (cargo-inputs 'zoxide))
    (home-page "https://github.com/ajeetdsouza/zoxide/")
    (synopsis "Fast way to navigate your file system")
    (description
     "Zoxide is a fast replacement for your @command{cd} command.  It keeps
track of the directories you use most frequently, and uses a ranking algorithm
to navigate to the best match.")
    (license license:expat)))

(define-public htmlq
  (package
    (name "htmlq")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "htmlq" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0912cdkz5xji1hzfj1cf42zh1kd860b52xmwwhb7q2jhp6qk25jh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:install-source? #f))
    (inputs (cargo-inputs 'htmlq))
    (home-page "https://github.com/mgdm/htmlq")
    (synopsis "Like jq, but for HTML")
    (description "Extract content from HTML files using CSS selectors.")
    (license license:expat)))

(define-public podlet
  (package
    (name "podlet")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "podlet" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j394gv9fpl1wii7l0v4y31mdni6r98l223wd6x2v3ia82091xg4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'podlet))
    (home-page "https://github.com/containers/podlet")
    (synopsis
     "Generate Podman Quadlet files from a Podman command, compose file,
or existing object")
    (description
     "This package generates Podman Quadlet files from a Podman command,
compose file, or existing object.")
    (license license:mpl2.0)))

(define-public espflash
  (package
    (name "espflash")
    (version "3.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/esp-rs/espflash.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vmq3b66yinqypgzfpdivli2ipiyzingakxy84j31srzg70m7maz"))))
    (build-system cargo-build-system)
    (inputs
     (cons eudev (cargo-inputs 'espflash)))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (delete-file "Cargo.lock")
             (chdir "espflash"))))))
    (home-page "https://github.com/esp-rs/espflash")
    (synopsis "Command-line tool for flashing Espressif devices")
    (description
     "This package provides a command-line tool for flashing Espressif devices.")
    (license (list license:expat license:asl2.0))))
