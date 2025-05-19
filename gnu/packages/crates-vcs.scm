;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2022-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2024 Murilo <murilo@disroot.org>
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

(define-module (gnu packages crates-vcs)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sequoia)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define-public rust-asyncgit-0.26
  (package
    (name "rust-asyncgit")
    (version "0.26.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asyncgit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zj3mz9g964m4z8zla70k3gicqg6hph801qgbn3fj48j254di4q9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=sync::hooks::tests::test_hooks_commit_msg_reject_in_subfolder"
         "--skip=sync::hooks::tests::test_post_commit_hook_reject_in_subfolder"
         "--skip=sync::hooks::tests::test_pre_commit_workdir"
         "--skip=sync::submodules::tests::test_smoke")  ; network
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-easy-cast" ,rust-easy-cast-0.5)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-git2" ,rust-git2-0.18)
                       ("rust-git2-hooks" ,rust-git2-hooks-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rayon-core" ,rust-rayon-core-1)
                       ("rust-scopetime" ,rust-scopetime-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-ssh-key" ,rust-ssh-key-0.6)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-truncate" ,rust-unicode-truncate-1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.11)
        ("rust-invalidstring" ,rust-invalidstring-0.1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-serial-test" ,rust-serial-test-3)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config git-minimal))
    (inputs (list libgit2-1.7 libssh2 openssl zlib))
    (home-page "https://github.com/extrawurst/gitui")
    (synopsis "Use git2 in an asynchronous context")
    (description
     "This package provides for using git2 in an asynchronous context.")
    (license license:expat)))

(define-public rust-asyncgit-0.25
  (package
    (inherit rust-asyncgit-0.26)
    (name "rust-asyncgit")
    (version "0.25.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asyncgit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05qlwp63k5zd4yd7n18v6bs32fhbx5qlsc98j203maacy0vlm9h7"))))
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip" "reject_in_subfolder"      ; /bin/sh
                            "--skip" "test_pre_commit_workdir"  ; /bin/sh
                            "--skip" "sync::submodules::tests::test_smoke") ; network
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-easy-cast" ,rust-easy-cast-0.5)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-git2" ,rust-git2-0.18)
                       ("rust-git2-hooks" ,rust-git2-hooks-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rayon-core" ,rust-rayon-core-1)
                       ("rust-scopetime" ,rust-scopetime-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-truncate" ,rust-unicode-truncate-0.2)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.11)
        ("rust-invalidstring" ,rust-invalidstring-0.1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-serial-test" ,rust-serial-test-3)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-auth-git2-0.5
  (package
    (name "rust-auth-git2")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "auth-git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "149p4485g7js5a0lnblgw549mhy62rkdh4ib63kky09b46pva41q"))))
    (build-system cargo-build-system)
    (arguments
     (list #:tests? #f  ; use of undeclared crate or module `log`
           #:cargo-inputs
           (list rust-dirs-5
                 rust-git2-0.19
                 rust-log-0.4
                 rust-terminal-prompt-0.2)
           #:cargo-development-inputs
           (list rust-assert2-0.3
                 rust-clap-4
                 rust-env-logger-0.10
                 rust-git2-0.19)))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.8 libssh2 zlib))
    (home-page "https://github.com/de-vri-es/auth-git2-rs")
    (synopsis "Authentication for `git2`")
    (description "This package provides Authentication for `git2`.")
    (license license:bsd-2)))

(define-public rust-forgejo-api-0.5
  (package
    (name "rust-forgejo-api")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "forgejo-api" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "114lhnafx34xak9dx7hcsvgdr48bnzn7k32al276h00jl9wjd3ag"))))
    (build-system cargo-build-system)
    (arguments
     (list #:tests? #f  ; Wants to run against a forgejo instance.
           #:cargo-inputs
           (list rust-base64ct-1
                 rust-bytes-1
                 rust-reqwest-0.11
                 rust-serde-1
                 rust-serde-json-1
                 rust-soft-assert-0.1
                 rust-thiserror-1
                 rust-time-0.3
                 rust-tokio-1
                 rust-url-2
                 rust-zeroize-1)
           #:cargo-development-inputs
           (list rust-eyre-0.6
                 rust-reqwest-0.11
                 rust-tokio-1)))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://codeberg.org/Cyborus/forgejo-api")
    (synopsis "Interface to Forgejo's Web API")
    (description "This package provides an interface to Forgejo's Web API.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-forgejo-api-0.4
  (package
    (inherit rust-forgejo-api-0.5)
    (name "rust-forgejo-api")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "forgejo-api" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bc1m39g3i8mm70cf35hcqh898xpm2y4x819y8qfaaw6pnhg90pn"))))
    (arguments
     (list #:tests? #f  ; Wants to run against a forgejo instance.
           #:cargo-inputs
           (list rust-base64ct-1
                 rust-bytes-1
                 rust-reqwest-0.11
                 rust-serde-1
                 rust-serde-json-1
                 rust-soft-assert-0.1
                 rust-thiserror-1
                 rust-time-0.3
                 rust-tokio-1
                 rust-url-2
                 rust-zeroize-1)
           #:cargo-development-inputs
           (list rust-eyre-0.6
                 rust-reqwest-0.11
                 rust-tokio-1)))))

(define-public rust-git-testament-0.2
  (package
    (name "rust-git-testament")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "git-testament" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l9f4d4p21lfvx25640q7xl1i6m749ljx7bsr0czjy8ljaf9jx2s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; cannot find value `None` in this scope
       #:cargo-inputs
       (("rust-git-testament-derive" ,rust-git-testament-derive-0.2))
       #:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/kinnison/git-testament/")
    (synopsis "Record git working tree status when compiling your crate")
    (description "Record git working tree status when compiling your crate")
    (license license:bsd-3)))

(define-public rust-git-testament-derive-0.2
  (package
    (name "rust-git-testament-derive")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-testament-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h645dqdzzgibgkar3vki1mkpkf73r9cfbvlaqbdrcqywxkwksmv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs (("rust-git-testament" ,rust-git-testament-0.2))))
    (home-page "https://github.com/kinnison/git-testament/")
    (synopsis "Record git working tree status when compiling your crate")
    (description
     "This package provides an inner procedural macro for git-testament.")
    (license license:bsd-3)))

(define-public rust-git-version-0.3
  (package
    (name "rust-git-version")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-version" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06ddi3px6l2ip0srn8512bsh8wrx4rzi65piya0vrz5h7nm6im8s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; not a git repository
       #:cargo-inputs
       (("rust-git-version-macro" ,rust-git-version-macro-0.3))
       #:cargo-development-inputs (("rust-assert2" ,rust-assert2-0.3)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/fusion-engineering/rust-git-version")
    (synopsis "Embed git information in your code at compile-time")
    (description
     "This crates compiles the git version (tag name, or hash otherwise) and
dirty state into your program.")
    (license license:bsd-2)))

(define-public rust-git-version-macro-0.3
  (package
    (name "rust-git-version-macro")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-version-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h1s08fgh9bkwnc2hmjxcldv69hlxpq7a09cqdxsd5hb235hq0ak"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f        ; not a git repository
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-assert2" ,rust-assert2-0.3))))
    (home-page "https://github.com/fusion-engineering/rust-git-version")
    (synopsis "Internal macro crate for git-version")
    (description
     "This is an internal macro crate for git-version.")
    (license license:bsd-2)))

(define-public rust-git2-0.20
  (package
    (name "rust-git2")
    (version "0.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0451zzmvblvlrj6y6pgdsxrqh42hi789n3k9lp0hslmi6fhhgsrd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libgit2-sys" ,rust-libgit2-sys-0.18)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis
     "Bindings to libgit2 for interoperating with git repositories. This library is
both threadsafe and memory safe and allows both reading and writing git
repositories.")
    (description
     "This package provides Bindings to libgit2 for interoperating with git repositories.  This library is
both threadsafe and memory safe and allows both reading and writing git
repositories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-git2-0.19
  (package
    (name "rust-git2")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "091pv7866z1qjq800ys0wjv8n73wrv7fqdrddxcnq36w8lzbf0xr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags (list "--release" "--"
                                "--skip=cred::test::credential_helper5")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libgit2-sys" ,rust-libgit2-sys-0.17)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-4)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-time" ,rust-time-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.8 libssh2 openssl zlib))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Rust bindings to libgit2")
    (description
     "This package provides bindings to libgit2 for interoperating with git
repositories.  This library is both threadsafe and memory safe and allows both
reading and writing git repositories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-git2-0.18
  (package
    (inherit rust-git2-0.19)
    (name "rust-git2")
    (version "0.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w7gcq6v9kdlh0vcv27xrk09c1bhkarqhnp52pvnnximzrxnlbi3"))))
    (arguments
     `(#:cargo-test-flags (list "--release" "--"
                                "--skip=cred::test::credential_helper5")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libgit2-sys" ,rust-libgit2-sys-0.16)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-4)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-time" ,rust-time-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.7 libssh2 openssl zlib))))

(define-public rust-git2-0.17
  (package
    (inherit rust-git2-0.18)
    (name "rust-git2")
    (version "0.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i00kg3yizh7mn6hnj3yz3hpniisidlavifgy8n3cnm9gim9v63v"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.15)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-time" ,rust-time-0.1))))
    (native-inputs
     (list pkg-config
           git-minimal))                ;for a single test
    (inputs (list libgit2-1.6 libssh2 openssl zlib))))

(define-public rust-git2-0.16
  (package
    (inherit rust-git2-0.17)
    (name "rust-git2")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k1aavsfhk0i9jycc9gb61w2jwy8w9dgkd7zkz295wwm566gdxyc"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.14)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-paste" ,rust-paste-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-time" ,rust-time-0.1))))
    (native-inputs
     (list pkg-config
           git-minimal))                ;for a single test
    (inputs
     (list libgit2 libssh2 openssl zlib))))

(define-public rust-git2-0.15
  (package
    (inherit rust-git2-0.16)
    (name "rust-git2")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lf7yxgrbglx5qqvk033n44ymmrg72z271911jwix9d6lgjbx519"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.14)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-paste" ,rust-paste-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-time" ,rust-time-0.1))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("git" ,git-minimal/pinned)))              ;for a single test
    (inputs
     (list libgit2 libssh2 openssl zlib))))

(define-public rust-git2-0.13
  (package
    (inherit rust-git2-0.15)
    (name "rust-git2")
    (version "0.13.25")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mppxyjzi69m879mwpin4d9jljanwaijlx3f5w3fdh143g62k4pj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.12)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-paste" ,rust-paste-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-time" ,rust-time-0.1))))
    (inputs
     (list libgit2-1.3 libssh2 openssl zlib))))

(define-public rust-git2-0.9
  (package
    (inherit rust-git2-0.13)
    (name "rust-git2")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09lj6i26yial0drdbmfh36avz6wizaxqb0k41sqn2kca1qv01d4c"))))
    (arguments
     `(#:tests? #f      ; Needs older version of libgit2.
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.8)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-docopt" ,rust-docopt-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-thread-id" ,rust-thread-id-3)
        ("rust-time" ,rust-time-0.1))))))

(define-public rust-git2-curl-0.20
  (package
    (name "rust-git2-curl")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2-curl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17q7p4xdmvzn8jy75cdpl6bncy70z1v864wv0ch2690wg9919zv8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t                 ;need rust-civet and others
       #:cargo-inputs (("rust-curl" ,rust-curl-0.4)
                       ("rust-git2" ,rust-git2-0.19)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Libgit2 HTTP transport backend powered by @code{libcurl}")
    (description "Backend for an HTTP transport in @code{libgit2}, powered by
libcurl, which is intended to be used with the @code{git2} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-git2-curl-0.19
  (package
    (inherit rust-git2-curl-0.20)
    (name "rust-git2-curl")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2-curl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hzp64h1x8kr2vvf3bx195s1999sh8d0cygw4vykymwcc1hnpqkq"))))
    (arguments
     `(#:skip-build? #t                 ;need rust-civet and others
       #:cargo-inputs (("rust-curl" ,rust-curl-0.4)
                       ("rust-git2" ,rust-git2-0.18)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-url" ,rust-url-2))))))

(define-public rust-git2-hooks-0.3
  (package
    (name "rust-git2-hooks")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2-hooks" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gr08zl1g0ll16smanqq8rxmwn8i5fikk7manch2022bh3lhw09y"))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.7 libssh2 openssl zlib))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; unresolved import `git2_testing`
       #:cargo-inputs (("rust-git2" ,rust-git2-0.17)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-shellexpand" ,rust-shellexpand-3)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/extrawurst/gitui")
    (synopsis "Git hooks")
    (description "This crate adds git hooks support based on git2-rs.")
    (license license:expat)))

(define-public rust-git-conventional-0.12
  (package
    (name "rust-git-conventional")
    (version "0.12.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-conventional" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i3vlmq0b5nrayxfgc27brbwhy3f03dwsb834qjz47f8zjvlkagn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-unicase" ,rust-unicase-2)
                       ("rust-winnow" ,rust-winnow-0.7))))
    (home-page "https://github.com/crate-ci/git-conventional")
    (synopsis "parser library for the Conventional Commit specification.")
    (description
     "This package provides a parser library for the Conventional Commit
specification.")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-info-0.1
  (package
    (name "rust-git-info")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git_info" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jzlqz5q6yl7d3a8nazgn5blvjci6h7mjxqab0cxwq73b51lf9cm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "http://github.com/sagiegurari/git_info")
    (synopsis "Extracts git repository information")
    (description "This package provides Extracts git repository information.")
    (license license:asl2.0)))

(define-public rust-gix-merge-0.3
  (package
    (name "rust-gix-merge")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-merge" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15dbccdjjspg9vvfclwj87qy48k06rnykwhwl1n9aqn8d0fsh6jy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.50)
                       ("rust-gix-filter" ,rust-gix-filter-0.17)
                       ("rust-gix-fs" ,rust-gix-fs-0.13)
                       ("rust-gix-hash" ,rust-gix-hash-0.16)
                       ("rust-gix-index" ,rust-gix-index-0.38)
                       ("rust-gix-object" ,rust-gix-object-0.47)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-revision" ,rust-gix-revision-0.32)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.18)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-16)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.39)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "crate of the gitoxide project implementing merge algorithms")
    (description
     "This package provides a crate of the gitoxide project implementing merge
algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-shallow-0.2
  (package
    (name "rust-gix-shallow")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-shallow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rjhwcjjixfy4fbzciyz5mikkvq38rwfyny86ckya0z324q58wmb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.16)
                       ("rust-gix-lock" ,rust-gix-lock-16)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Handle shallow git files")
    (description
     "This package provides a crate of the gitoxide project dedicated to
handling files specifying the shallow boundary.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-blame-0.0.0
  (package
    (name "rust-gix-blame")
    (version "0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-blame" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11m9524qygdgg5zjfmww8di8ppc2jd98r2yi1sspwd5277i9bixd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gix-diff" ,rust-gix-diff-0.50)
                       ("rust-gix-hash" ,rust-gix-hash-0.16)
                       ("rust-gix-object" ,rust-gix-object-0.47)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.44)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.39)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "Implementing a git 'blame' algorithm")
    (description
     "This package provides a crate of the gitoxide project dedicated to implementing
a blame algorithm.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-state-0.16
  (package
    (name "rust-gix-worktree-state")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qfbcxcka9gx9dyjzdz9v4pr6yn9w1kvyx03myh3h1g5z8b58ak7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-filter" ,rust-gix-filter-0.16)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-glob" ,rust-gix-glob-0.17)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-index" ,rust-gix-index-0.37)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.38)
                       ("rust-io-close" ,rust-io-close-0.3)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "Set the git worktree to a particular state")
    (description
     "This package provides a crate of the gitoxide project implementing setting the
worktree to a particular state.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-submodule-0.16
  (package
    (name "rust-gix-submodule")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p4zx1v997wjglya5xrfxdixf586scigbj43dqmbwvnb1y65yid2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-config" ,rust-gix-config-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.8)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.27)
                       ("rust-gix-url" ,rust-gix-url-0.28)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git submodules")
    (description
     "This package provides a crate of the gitoxide project dealing with
git submodules.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-status-0.16
  (package
    (name "rust-gix-status")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pv4fprwb2iiz737p7kljzxwdzxkdjzsv1mg1rmvs23n4w77fr8n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-diff" ,rust-gix-diff-0.49)
                       ("rust-gix-dir" ,rust-gix-dir-0.11)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-filter" ,rust-gix-filter-0.16)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-index" ,rust-gix-index-0.37)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.38)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "'git status'-like functionality")
    (description
     "This package provides a crate of the gitoxide project dealing with git
status'-like functionality.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-transport-0.44
  (package
    (name "rust-gix-transport")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bybr8741kilfvmwg71p80xfadjvsy2mm39ilb7i71vsa0gdj16x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-gix-command" ,rust-gix-command-0.4)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.26)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-packetline" ,rust-gix-packetline-0.18)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.28)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git transport layer")
    (description
     "This package implements the git transport layer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-shallow-0.1
  (package
    (name "rust-gix-shallow")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-shallow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00ffhjy3lzqr6zqj0h3c635634383z0g0wgndz5r4x7888r6gll8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-lock" ,rust-gix-lock-15)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Handle shallow git files")
    (description
     "This package provides a crate of the gitoxide project dedicated to
handling files specifying the shallow boundary.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-refspec-0.27
  (package
    (name "rust-gix-refspec")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ga1dh6500q88wb0ymd2s2iqraw1j6gjqdgb1bmwfs3qfjxmdh00"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-revision" ,rust-gix-revision-0.31)
                       ("rust-gix-validate" ,rust-gix-validate-0.9)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "parsing and representing git refspecs")
    (description
     "This package provides a crate of the gitoxide project for parsing and
representing git refspecs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-protocol-0.47
  (package
    (name "rust-gix-protocol")
    (version "0.47.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rxhmvm3c1lamghxr9gp3bnb0mcw069rai6cx5f07mzynvl44in8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.26)
                       ("rust-gix-date" ,rust-gix-date-0.9)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-lock" ,rust-gix-lock-15)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.17)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-ref" ,rust-gix-ref-0.49)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.27)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.17)
                       ("rust-gix-shallow" ,rust-gix-shallow-0.1)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.44)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-maybe-async" ,rust-maybe-async-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git protocols")
    (description
     "This package provides a crate of the gitoxide project for implementing git
protocols.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-pack-0.56
  (package
    (name "rust-gix-pack")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v2n5k0q08zdiqwbmn1rgwd0fcm943lcibwpmgkwlady564r4n21"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clru" ,rust-clru-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.49)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.6)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-15)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.43)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-uluru" ,rust-uluru-3))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Implements git packs and related data structures")
    (description
     "This package implements git packs and related data structures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-odb-0.66
  (package
    (name "rust-gix-odb")
    (version "0.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bnhjzg360vy6f303ps7halnnkx3x81dwy4l8q2f4bipng70wy6b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.9)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.6)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-pack" ,rust-gix-pack-0.56)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Implements various git object databases")
    (description "This package implements various git object databases.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-negotiate-0.17
  (package
    (name "rust-gix-negotiate")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a7igqmnnk183fyibbrwdfyjpqqz4vl5p79vr3lqcls02q586zyj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.25)
                       ("rust-gix-date" ,rust-gix-date-0.9)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.17)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "git negotiation algorithms")
    (description
     "This package provides a crate of the gitoxide project implementing
negotiation algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-revision-0.31
  (package
    (name "rust-gix-revision")
    (version "0.31.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rzrr2i5vb8yc0cffprgkyawxrigsxfp11c4rv96hnj0fk2dvqb1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.25)
                       ("rust-gix-date" ,rust-gix-date-0.9)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.6)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.17)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "finding names for git revisions and parsing specifications")
    (description
     "This package provides a crate of the gitoxide project dealing with
finding names for revisions and parsing specifications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-merge-0.2
  (package
    (name "rust-gix-merge")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-merge" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jf22r6bzdbpkizs3syp7fq9zlkf0y0qkl73ykgccpz5z02x5fj8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.49)
                       ("rust-gix-filter" ,rust-gix-filter-0.16)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-index" ,rust-gix-index-0.37)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-revision" ,rust-gix-revision-0.31)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.17)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-15)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.38)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git merge algorithms")
    (description
     "This package provides a crate of the gitoxide project implementing merge
algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-pathspec-0.8
  (package
    (name "rust-gix-pathspec")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pathspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07mqfl6232285yzsmym2vr7vndwh3ivx9p7xgv7nzsd4wkxjsisc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.23)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-glob" ,rust-gix-glob-0.17)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git magical pathspecs")
    (description
     "This package provides a crate of the gitoxide project dealing with
git magical pathspecs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-discover-0.37
  (package
    (name "rust-gix-discover")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vxnzwaz7g5dzn3dscy4qry3yb7r0748zldlxjdllsi69vx6vgw3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.49)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "Discover git repositories and check if a directory is a git repository")
    (description
     "This package provides a way to discover git repositories and check if a
directory is a git repository.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-dir-0.11
  (package
    (name "rust-gix-dir")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-dir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14qjgsbb1dkyyha3l68z7n84j1c7jjycqrw3ia746d5xyjygz8pv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-discover" ,rust-gix-discover-0.37)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.12)
                       ("rust-gix-index" ,rust-gix-index-0.37)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.8)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.38)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git directory walks")
    (description
     "This package provides a crate of the gitoxide project dealing with
git directory walks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-index-0.37
  (package
    (name "rust-gix-index")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yi5g8vfk5vgb5la031s86a6j7i83f90sm51zz468ssm43yla1i7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-lock" ,rust-gix-lock-15)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.43)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.9)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "git index file handler")
    (description
     "This package provides acrate of the gitoxide project dedicated to
implementing the git index file.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ignore-0.12
  (package
    (name "rust-gix-ignore")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12mv0lgq8aviy6fc4mdxr7r0ra0l1kb729wf8fkhmbx4s8jgpcdn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.17)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Handling .gitignore files")
    (description
     "This package provides a crate of the gitoxide project for dealing with
.gitignore files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-0.38
  (package
    (name "rust-gix-worktree")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04za9fgw10w0r0sfqnjd6sji3xcwiwgr9asy1ma25yl8a7hvnvbm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.23)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-glob" ,rust-gix-glob-0.17)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.12)
                       ("rust-gix-index" ,rust-gix-index-0.37)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-validate" ,rust-gix-validate-0.9)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "git shared worktree-related types and utilities")
    (description
     "This package provides a crate of the gitoxide project for shared worktree
related types and utilities.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-diff-0.49
  (package
    (name "rust-gix-diff")
    (version "0.49.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xh4mac785zyvnyap5awpj2iw1kw6aqgz5hg19d21jnbxik2bsd8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.4)
                       ("rust-gix-filter" ,rust-gix-filter-0.16)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-15)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.43)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.38)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Calculate differences between various git objects")
    (description
     "This package provides Calculate differences between various git objects.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-url-0.28
  (package
    (name "rust-gix-url")
    (version "0.28.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ncj6k4lk3qb0i27ida7ngi9z06qpmrbva6v0da3zgd67drzp5nh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "git parsing and serialization of gix-url")
    (description
     "This package provides a crate of the gitoxide project implementing
parsing and serialization of gix-url.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-credentials-0.26
  (package
    (name "rust-gix-credentials")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02iy6pnvmd04jma303fszv922kpyhd5ggk5l38armhl5nxb0r9c2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.4)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.9)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-url" ,rust-gix-url-0.28)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "Interact with git credentials helpers")
    (description
     "This package provides a crate of the gitoxide project to interact with
git credentials helpers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-tempfile-15
  (package
    (name "rust-gix-tempfile")
    (version "15.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10nvk82g7fhljg5y63dxpd8p7296wrfzxyssk957misc17pqdsrg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dashmap" ,rust-dashmap-6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "git tempfile implementation with a global registry to assure cleanup")
    (description
     "This package provides a tempfile implementation with a global registry
to assure cleanup.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-lock-15
  (package
    (name "rust-gix-lock")
    (version "15.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h6r088yv5fk0d14zihssfh1zfhdyc8cpnpbygcn7nsjlilaplqw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gix-tempfile" ,rust-gix-tempfile-15)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git-style lock-file implementation")
    (description "This package provides a git-style lock-file implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ref-0.49
  (package
    (name "rust-gix-ref")
    (version "0.49.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ps8xzgkblsn3k5961ykyjkpm55hmxwi340lf4dhz7c3divn26x9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.33)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-lock" ,rust-gix-lock-15)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-15)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.9)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Handle git references")
    (description "This package provides a crate to handle git references.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-config-0.42
  (package
    (name "rust-gix-config")
    (version "0.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06bgc1isbk831763ii4f1yahf7r3id301ksri4acp68zr83b8jb6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-glob" ,rust-gix-glob-0.17)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.49)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-unicode-bom" ,rust-unicode-bom-2)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git-config file parser and editor")
    (description
     "This package provides a git-config file parser and editor from the gitoxide
project.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-revwalk-0.17
  (package
    (name "rust-gix-revwalk")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "178hrfgigvr3a1m8mn1949ms75wb10s7rwyqczqghmpl6by2c02i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.25)
                       ("rust-gix-date" ,rust-gix-date-0.9)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.6)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "crate providing utilities for walking the git revision graph")
    (description
     "This package provides a crate providing utilities for walking the
git revision graph.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-commitgraph-0.25
  (package
    (name "rust-gix-commitgraph")
    (version "0.25.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11cdlkbkv80imbdkiy8k09gb1c48k6qadpmxvavb53w6ly8nbnm8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Read-only access to the git commitgraph file format")
    (description
     "This package provides read-only access to the git commitgraph file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-traverse-0.43
  (package
    (name "rust-gix-traverse")
    (version "0.43.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12dcmf67m409ss0jjpgr9vjh048cs6hbplkiz69kxqhrhrj7vm3f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.25)
                       ("rust-gix-date" ,rust-gix-date-0.9)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.6)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.17)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git tree traversal")
    (description "This package provides git tree traversal.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-fs-0.12
  (package
    (name "rust-gix-fs")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f8xifs0wkq7lhy3c8091kq2lx15qkynjb6fwnbiyqjsa2n4yg9v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fastrand" ,rust-fastrand-2)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "file system specific utilities for `gitoxide`")
    (description
     "This package provides file system specific utilities to `gitoxide`.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-filter-0.16
  (package
    (name "rust-gix-filter")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02krfk40gvz53wardlmzngkc27cf7zb5dzn742x41y37avpcs3ix"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.23)
                       ("rust-gix-command" ,rust-gix-command-0.4)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-packetline-blocking" ,rust-gix-packetline-blocking-0.18)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git filters")
    (description
     "This package provides a crate of the gitoxide project implementing
git filters.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-glob-0.17
  (package
    (name "rust-gix-glob")
    (version "0.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d9lrxas6zjia91j3m4z8rnazz1s02j9kgw4fib82d8aximrmxma"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "git pattern matching")
    (description
     "This package provides a crate of the gitoxide project dealing with pattern
matching.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-attributes-0.23
  (package
    (name "rust-gix-attributes")
    (version "0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-attributes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p6a6ai3pk8c7xn48vbw7gvjh7rc5m13cbcsd7zfvh4l462vzyfx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.17)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-kstring" ,rust-kstring-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Deal with .gitattributes files")
    (description
     "This package provides a crate of the gitoxide project dealing with
.gitattributes files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-stream-0.18
  (package
    (name "rust-gix-worktree-stream")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kv9vchybbpc1y8qrgd4gq2k8vpygwqxsyz682wfx0h45jp5w01l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gix-attributes" ,rust-gix-attributes-0.23)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-filter" ,rust-gix-filter-0.16)
                       ("rust-gix-fs" ,rust-gix-fs-0.12)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.43)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "generate a byte-stream from a git-tree")
    (description
     "This package provides a way to generate a byte-stream from a git-tree.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-hashtable-0.6
  (package
    (name "rust-gix-hashtable")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hashtable" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zhqgncv6jh3x7a7a2w3qbayghmiwv230mdw6gvqw1ricqjmpxhf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "Provide hashtable based data structures optimized to utilize
git ObjectId keys")
    (description
     "This package provides a crate that provides hashtable based data structures
optimized to utilize git @code{ObjectId} keys.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-hash-0.15
  (package
    (name "rust-gix-hash")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kp4yjlkp8g4qg0r2zs0jmz19r076f2y91cjsikhxvclf70wqphb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-faster-hex" ,rust-faster-hex-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "Borrowed and owned git hash digests used to identify git objects")
    (description
     "This package provides borrowed and owned git hash digests used to
identify git objects.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-features-0.39
  (package
    (name "rust-gix-features")
    (version "0.39.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07yqby9y0icx2l7kwbvxfg6z8b7gfznknwd4vd0a68p0y9rxd1bx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-jwalk" ,rust-jwalk-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-29)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "integrate capabilities using compile-time feature flags for gitoxide")
    (description
     "This package provides a crate to integrate various capabilities using
compile-time feature flags for gitoxide.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-object-0.46
  (package
    (name "rust-gix-object")
    (version "0.46.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1skp7mbqbcdqkv9n4nxk87ilzd4jxfs7k10864zh7vw3040mhbg4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.33)
                       ("rust-gix-date" ,rust-gix-date-0.9)
                       ("rust-gix-features" ,rust-gix-features-0.39)
                       ("rust-gix-hash" ,rust-gix-hash-0.15)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.6)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.9)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "Immutable and mutable git objects with decoding and encoding support")
    (description
     "This package provides immutable and mutable git objects with decoding
and encoding support.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-archive-0.18
  (package
    (name "rust-gix-archive")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-archive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09ls5n0zg5vfhy6xvg1s5lrl1gbb478xxccwrzrf9p23cl4fyqwv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-date" ,rust-gix-date-0.9)
                       ("rust-gix-object" ,rust-gix-object-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.18)
                       ("rust-jiff" ,rust-jiff-0.1)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-zip" ,rust-zip-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "archive generation from a git worktree stream")
    (description
     "This package provides archive generation from a git worktree stream.")
    (license (list license:expat license:asl2.0))))
(define-public rust-gix-0.66
  (package
    (name "rust-gix")
    (version "0.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1swmi6l1ph9rqyzqd4v04apxima9zi8c9r9prd2z0111mv8vhj4h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.32)
                       ("rust-gix-archive" ,rust-gix-archive-0.15)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-config" ,rust-gix-config-0.40)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.9)
                       ("rust-gix-diff" ,rust-gix-diff-0.46)
                       ("rust-gix-dir" ,rust-gix-dir-0.8)
                       ("rust-gix-discover" ,rust-gix-discover-0.35)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.13)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.11)
                       ("rust-gix-index" ,rust-gix-index-0.35)
                       ("rust-gix-lock" ,rust-gix-lock-14)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.24)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.15)
                       ("rust-gix-object" ,rust-gix-object-0.44)
                       ("rust-gix-odb" ,rust-gix-odb-0.63)
                       ("rust-gix-pack" ,rust-gix-pack-0.53)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.45)
                       ("rust-gix-ref" ,rust-gix-ref-0.47)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.25)
                       ("rust-gix-revision" ,rust-gix-revision-0.29)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.15)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.13)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.14)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.42)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.41)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.9)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.36)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.13)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.15)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-29)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-serial-test" ,rust-serial-test-3)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Interact with Git repositories using a pure Rust implementation")
    (description "Gitoxide is a pure Rust implementation of Git that aims to be lean
and correct.  Interact with Git repositories just like Git would.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-0.64
  (package
    (inherit rust-gix-0.66)
    (name "rust-gix")
    (version "0.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kv3w87h8gbwg16k7nqzalb417v8yzh7fq0nh28350nckz91916p"))))
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-archive" ,rust-gix-archive-0.13)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-config" ,rust-gix-config-0.38)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-diff" ,rust-gix-diff-0.44)
                       ("rust-gix-dir" ,rust-gix-dir-0.6)
                       ("rust-gix-discover" ,rust-gix-discover-0.33)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.11)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-lock" ,rust-gix-lock-14)
                       ("rust-gix-macros" ,rust-gix-macros-0.1)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.23)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-odb" ,rust-gix-odb-0.61)
                       ("rust-gix-pack" ,rust-gix-pack-0.51)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.45)
                       ("rust-gix-ref" ,rust-gix-ref-0.45)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.23)
                       ("rust-gix-revision" ,rust-gix-revision-0.27)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.13)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.11)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.12)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.42)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.39)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.11)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.13)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-28)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-async-std" ,rust-async-std-1)
        ("rust-is-ci" ,rust-is-ci-1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-serial-test" ,rust-serial-test-3)
        ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-gix-0.63
  (package
    (inherit rust-gix-0.66)
    (name "rust-gix")
    (version "0.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01jbkflpr570inbdjv1xgfsimqf6xfrr0yg6mlv4aypsmlc50k4q"))))
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-archive" ,rust-gix-archive-0.13)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-config" ,rust-gix-config-0.37)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-diff" ,rust-gix-diff-0.44)
                       ("rust-gix-dir" ,rust-gix-dir-0.5)
                       ("rust-gix-discover" ,rust-gix-discover-0.32)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.11)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-lock" ,rust-gix-lock-14)
                       ("rust-gix-macros" ,rust-gix-macros-0.1)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.23)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-odb" ,rust-gix-odb-0.61)
                       ("rust-gix-pack" ,rust-gix-pack-0.51)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.45)
                       ("rust-gix-ref" ,rust-gix-ref-0.44)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.23)
                       ("rust-gix-revision" ,rust-gix-revision-0.27)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.13)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.10)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.11)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.42)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.39)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.11)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.13)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-28)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-serial-test" ,rust-serial-test-3)
                                   ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-gix-0.57
  (package
    (inherit rust-gix-0.66)
    (name "rust-gix")
    (version "0.57.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pnprvkywvzqqzlm5zsawjzjx71zh36q45ffm40bbiwj50w2bl3d"))))
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.29)
                       ("rust-gix-archive" ,rust-gix-archive-0.8)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.21)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.23)
                       ("rust-gix-config" ,rust-gix-config-0.33)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.23)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-diff" ,rust-gix-diff-0.39)
                       ("rust-gix-discover" ,rust-gix-discover-0.28)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-filter" ,rust-gix-filter-0.8)
                       ("rust-gix-fs" ,rust-gix-fs-0.9)
                       ("rust-gix-glob" ,rust-gix-glob-0.15)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.10)
                       ("rust-gix-index" ,rust-gix-index-0.28)
                       ("rust-gix-lock" ,rust-gix-lock-12)
                       ("rust-gix-macros" ,rust-gix-macros-0.1)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.21)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.11)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-odb" ,rust-gix-odb-0.56)
                       ("rust-gix-pack" ,rust-gix-pack-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.5)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.43)
                       ("rust-gix-ref" ,rust-gix-ref-0.40)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.21)
                       ("rust-gix-revision" ,rust-gix-revision-0.25)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.11)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.4)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.7)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-12)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.40)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.36)
                       ("rust-gix-url" ,rust-gix-url-0.26)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.29)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.6)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.8)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-28)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-gix-0.56
  (package
    (inherit rust-gix-0.66)
    (name "rust-gix")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jqq0vl71qdphspdkcanjkwp4pk35bmafh7sjxc56rhdqv4ws3av"))))
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-archive" ,rust-gix-archive-0.7)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-config" ,rust-gix-config-0.32)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-diff" ,rust-gix-diff-0.38)
                       ("rust-gix-discover" ,rust-gix-discover-0.27)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-filter" ,rust-gix-filter-0.7)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.9)
                       ("rust-gix-index" ,rust-gix-index-0.27)
                       ("rust-gix-lock" ,rust-gix-lock-11)
                       ("rust-gix-macros" ,rust-gix-macros-0.1)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.20)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.10)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-odb" ,rust-gix-odb-0.55)
                       ("rust-gix-pack" ,rust-gix-pack-0.45)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.4)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.42)
                       ("rust-gix-ref" ,rust-gix-ref-0.39)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.20)
                       ("rust-gix-revision" ,rust-gix-revision-0.24)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.10)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.3)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.6)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.39)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.35)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.28)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.5)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.7)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-26)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-gix-0.55
  (package
    (inherit rust-gix-0.56)
    (name "rust-gix")
    (version "0.55.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1spf1hgpbn76y2am0q4i1qxwy8987g9f7byhs09r6y5v3v6nf9h0"))))
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-archive" ,rust-gix-archive-0.6)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-config" ,rust-gix-config-0.31)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.21)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-diff" ,rust-gix-diff-0.37)
                       ("rust-gix-discover" ,rust-gix-discover-0.26)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-filter" ,rust-gix-filter-0.6)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.9)
                       ("rust-gix-index" ,rust-gix-index-0.26)
                       ("rust-gix-lock" ,rust-gix-lock-11)
                       ("rust-gix-macros" ,rust-gix-macros-0.1)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.20)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.9)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-odb" ,rust-gix-odb-0.54)
                       ("rust-gix-pack" ,rust-gix-pack-0.44)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.4)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.7)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.41)
                       ("rust-gix-ref" ,rust-gix-ref-0.38)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.19)
                       ("rust-gix-revision" ,rust-gix-revision-0.23)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.9)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.2)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.5)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.38)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.34)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.27)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.4)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.6)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-26)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-gix-0.54
  (package
    (inherit rust-gix-0.55)
    (name "rust-gix")
    (version "0.54.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ly919iq2jnb28fvac5iwqv9jqn1pr7fl63xsn89cial8kkk4vdd"))))
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.27)
                       ("rust-gix-archive" ,rust-gix-archive-0.5)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.21)
                       ("rust-gix-config" ,rust-gix-config-0.30)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.20)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-diff" ,rust-gix-diff-0.36)
                       ("rust-gix-discover" ,rust-gix-discover-0.25)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-filter" ,rust-gix-filter-0.5)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.8)
                       ("rust-gix-index" ,rust-gix-index-0.25)
                       ("rust-gix-lock" ,rust-gix-lock-10)
                       ("rust-gix-macros" ,rust-gix-macros-0.1)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.19)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.8)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-odb" ,rust-gix-odb-0.53)
                       ("rust-gix-pack" ,rust-gix-pack-0.43)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.3)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.7)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.40)
                       ("rust-gix-ref" ,rust-gix-ref-0.37)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.18)
                       ("rust-gix-revision" ,rust-gix-revision-0.22)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.8)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.1)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.4)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.37)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.33)
                       ("rust-gix-url" ,rust-gix-url-0.24)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.26)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.3)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-26)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-gix-actor-0.32
  (package
    (name "rust-gix-actor")
    (version "0.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rc662dyhxipvz3c181h7rif3y627lb0ky83s1nadi25rl9f66gw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-date" ,rust-gix-date-0.9)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-winnow" ,rust-winnow-0.6))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, a way to identify Git actors")
    (description "Part of Gitoxide, a pure Rust implementation of Git.  This
package provides a way to identify Git actors.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-actor-0.31
  (package
    (inherit rust-gix-actor-0.32)
    (name "rust-gix-actor")
    (version "0.31.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wm3i9g69hkfhwiw1c4z9fr1hkfxsfxyzdh07b637f1lgqsm9r50"))))
    (arguments
     `(#:tests? #f ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.6))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-actor-0.29
  (package
    (inherit rust-gix-actor-0.32)
    (name "rust-gix-actor")
    (version "0.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n6rq4nln61kvppz304llwl3ricckx04ikfwj7w5zixm9amva9ys"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-actor-0.28
  (package
    (inherit rust-gix-actor-0.29)
    (name "rust-gix-actor")
    (version "0.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05xldn3aq58kjx2i87xsb2gdw7qhxvvikyvsiwvv85ppkq1cmb9f"))))
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-actor-0.27
  (package
    (inherit rust-gix-actor-0.28)
    (name "rust-gix-actor")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09sychqipf8bsmrwn62svpda457h8wj65qnl49qqk42j5jc0xih8"))))
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-btoi" ,rust-btoi-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-date" ,rust-gix-date-0.8)
        ("rust-gix-features" ,rust-gix-features-0.35)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-actor-0.21
  (package
    (inherit rust-gix-actor-0.27)
    (name "rust-gix-actor")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f6q1ivdcgj26jln48nl2hphwmps6sb9m4dysldvzbz1dfgkzrwz"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-btoi" ,rust-btoi-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-date" ,rust-gix-date-0.5)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-nom" ,rust-nom-7)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-archive-0.15
  (package
    (name "rust-gix-archive")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-archive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02m6n14jpvz61rwf1rg5lbjgz47n7zbcvqiramsqnff1an5c0iwi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-gix-date" ,rust-gix-date-0.9)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.15)
        ("rust-jiff" ,rust-jiff-0.1)
        ("rust-tar" ,rust-tar-0.4)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-zip" ,rust-zip-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Archive generation from of a worktree stream")
    (description
     "This package provides archive generation from of a worktree stream.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-archive-0.13
  (package
    (inherit rust-gix-archive-0.15)
    (name "rust-gix-archive")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-archive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m4nwxcfrivnf74kx38sxgcalba8nbl2fq4xlvad28q2vzmvpdk3"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.13)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zip" ,rust-zip-2))))))

(define-public rust-gix-archive-0.8
  (package
    (inherit rust-gix-archive-0.15)
    (name "rust-gix-archive")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-archive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04yz6382qx6r6g1x2czvpz80ympa33mg3ihkmdf2mcahmgc1mz8w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.8)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zip" ,rust-zip-0.6))))
    ;(home-page "https://github.com/Byron/gitoxide")
    ;(synopsis "Archive generation of a Git worktree")
    ;(description "Archive generation of a worktree.  Part of Gitoxide a pure
;Rust implementation of Git.")
   ; (license (list license:expat license:asl2.0))
    ))

(define-public rust-gix-archive-0.7
  (package
    (inherit rust-gix-archive-0.8)
    (name "rust-gix-archive")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-archive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f6c5qmf905wm1fp0ra255925avzwgvp7p2rrpqnk4xwiidhklf9"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.7)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zip" ,rust-zip-0.6))))))

(define-public rust-gix-archive-0.6
  (package
    (inherit rust-gix-archive-0.7)
    (name "rust-gix-archive")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-archive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17gh1wzs1jrmywbchrqdmkma2c0saik7k52fralfdfkf6hbq97wh"))))
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `gix_attributes`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.6)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zip" ,rust-zip-0.6))))))

(define-public rust-gix-archive-0.5
  (package
    (inherit rust-gix-archive-0.6)
    (name "rust-gix-archive")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-archive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1izajfmninmjg7mvcp6y0mdy641hkvvqmsd7gp6d92mkgxgn6wdb"))))
    (arguments
     `(#:tests? #f  ; undeclared crate gix_worktree, gix_filter, gix_odb
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.5)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zip" ,rust-zip-0.6))))))

(define-public rust-gix-attributes-0.22
  (package
    (name "rust-gix-attributes")
    (version "0.22.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-attributes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "048k2c9kgh6bv7d1d2iyca8cmvb9mw090jjnab9kv5sam8jvzk7b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-glob" ,rust-gix-glob-0.16)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-gix-trace" ,rust-gix-trace-0.1)
        ("rust-kstring" ,rust-kstring-2)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-bom" ,rust-unicode-bom-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this crate deals with .gitattributes")
    (description
     "This package provides a crate from the Gitoxide project dealing with
@code{.gitattributes} files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-attributes-0.21
  (package
    (inherit rust-gix-attributes-0.22)
    (name "rust-gix-attributes")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-attributes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "066y16rw994wrvvhv1xwf5gnc51bh727jxyql7wzrkkb7mhffvdx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.15)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-kstring" ,rust-kstring-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))))

(define-public rust-gix-attributes-0.20
  (package
    (inherit rust-gix-attributes-0.21)
    (name "rust-gix-attributes")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-attributes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "109kciz3cssfbx9zgslngdrkzwf3zd9mlv0srm3yqxlcsdlm8f8g"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-kstring" ,rust-kstring-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))))

(define-public rust-gix-attributes-0.19
  (package
    (inherit rust-gix-attributes-0.20)
    (name "rust-gix-attributes")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-attributes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "086qgrh8srr5vyswbchn72kw967f25szjgk27dss96vhf1g6cl94"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-byteyarn" ,rust-byteyarn-0.2)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))))

(define-public rust-gix-bitmap-0.2
  (package
    (name "rust-gix-bitmap")
    (version "0.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-bitmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lvbqvxsj12f628rf279gffrf10a6jxmnjjbk7m1m25w9dxqk2yl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implement the standard git bitmap format")
    (description
     "This package provides a crate of the gitoxide project dedicated
implementing the standard git bitmap format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-chunk-0.4
  (package
    (name "rust-gix-chunk")
    (version "0.4.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-chunk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xxl7rfh5vgcc83qvzmf9hprdyl2iipkl4z47x6bif60lnrvxzy6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Interact with the git chunk file format")
    (description
     "Interact with the git chunk file format used in multi-pack index and
commit-graph files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-command-0.3
  (package
    (name "rust-gix-command")
    (version "0.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lzyg587s4rcrlvi42ml744ardqy6l5vh7hrx3bkyib47a7nnzbd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-shell-words" ,rust-shell-words-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Handle internal git command execution")
    (description
     "This package provides a crate of the gitoxide project handling internal git
command execution.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-command-0.2
  (package
    (inherit rust-gix-command-0.3)
    (name "rust-gix-command")
    (version "0.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ix44maislxlranv67yw5fan5k82lpgax22zgc4jrxvpypxnqmrw"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1))))))

(define-public rust-gix-commitgraph-0.24
  (package
    (name "rust-gix-commitgraph")
    (version "0.24.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y7wc0y0xb0kh3c22pj3ns04zdqglqb22gj71kn3cn2ngzv0cfqk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-chunk" ,rust-gix-chunk-0.4)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-memmap2" ,rust-memmap2-0.9)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Read only access to the Git commitgraph file format")
    (description
     "Part of the Gitoxide project, a pure Rust implementation of Git.  This
package provides read only access to Git's commitgraph file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-commitgraph-0.23
  (package
    (inherit rust-gix-commitgraph-0.24)
    (name "rust-gix-commitgraph")
    (version "0.23.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z0hn19phbwcwmvp6mn21pvmlq92jyjzw19y0rvs87wm6kscp3by"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-commitgraph-0.22
  (package
    (inherit rust-gix-commitgraph-0.23)
    (name "rust-gix-commitgraph")
    (version "0.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dpcdj9s5pkdvqpc22jm42y2lhkji2jgixps7a05kw11l1xh19w5"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-commitgraph-0.21
  (package
    (inherit rust-gix-commitgraph-0.22)
    (name "rust-gix-commitgraph")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12nzyh8gyg0k15swsy51m1iy521a7p5xbdg9pw1a1w1cw9g9fnp7"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-commitgraph-0.16
  (package
    (inherit rust-gix-commitgraph-0.21)
    (name "rust-gix-commitgraph")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "028s0ai44gb9ynic2ch3ingzg8h44c47q90xlzk4fp6mnghhljg8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-chunk" ,rust-gix-chunk-0.4)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-memmap2" ,rust-memmap2-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-config-0.40
  (package
    (name "rust-gix-config")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c1bnijxf1j4qfqriw9kzf1g40i2yys324fyj4j5b8vcgr49grvq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.47)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2)
                       ("rust-winnow" ,rust-winnow-0.6))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Git-config file parser and editor from the Gitoxide project")
    (description
     "This package provides a git-config file parser and editor from the
Gitoxide project.  Gitoxide is a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-config-0.38
  (package
    (inherit rust-gix-config-0.40)
    (name "rust-gix-config")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n768fy37w3wy8jsjk34draw8fcljq44yrf8qay9xw0v7p83zx98"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.45)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2)
                       ("rust-winnow" ,rust-winnow-0.6))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))))

(define-public rust-gix-config-0.37
  (package
    (inherit rust-gix-config-0.40)
    (name "rust-gix-config")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h680yjj90hqch8x2bgnybx01smff2yvcrja6n7dj4byjm1gxyjk"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.44)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2)
                       ("rust-winnow" ,rust-winnow-0.6))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))))

(define-public rust-gix-config-0.33
  (package
    (inherit rust-gix-config-0.40)
    (name "rust-gix-config")
    (version "0.33.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y8h4j2rzdvh20a0clchsy6kga8dlaivapzfqk5av71nbf2h8wrn"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-glob" ,rust-gix-glob-0.15)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.40)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))))

(define-public rust-gix-config-0.32
  (package
    (inherit rust-gix-config-0.33)
    (name "rust-gix-config")
    (version "0.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hz0rj2nx16jrmp6sjzamk2wk96zcmq1s4lfp2c6wryqalflfh83"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.39)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))))

(define-public rust-gix-config-0.31
  (package
    (inherit rust-gix-config-0.32)
    (name "rust-gix-config")
    (version "0.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fzraij8rb98j71id939qc56nzaqfaqp8ln3kcvhjv66nk39ibjw"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.38)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))))

(define-public rust-gix-config-0.30
  (package
    (inherit rust-gix-config-0.31)
    (name "rust-gix-config")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dcaz3ylf2x763mjfg7jd4qhx65c8l7zqzp3g9kkyza8815m2wf1"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-config-value" ,rust-gix-config-value-0.14)
        ("rust-gix-features" ,rust-gix-features-0.35)
        ("rust-gix-glob" ,rust-gix-glob-0.13)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-ref" ,rust-gix-ref-0.37)
        ("rust-gix-sec" ,rust-gix-sec-0.10)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-bom" ,rust-unicode-bom-2)
        ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5))))))

(define-public rust-gix-config-value-0.14
  (package
    (name "rust-gix-config-value")
    (version "0.14.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config-value" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13mjjpljpc07qiyk36gdycdksy8ba85l9invz6ya7443v7syxaj9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "This crate implements @code{git-config} value parsing")
    (description
     "This package is a crate for @code{git-config} value parsing.  Part of
Gitoxide a Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-credentials-0.24
  (package
    (name "rust-gix-credentials")
    (version "0.24.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y6f5g8ny3rh80vw12qxzzvisw6588yll71hmvqq51wn0p9r3qwc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Git credentials handlers for Gitoxide")
    (description
     "Gitoxide is a Rust implementation of Git.  This package provides helpers
to interact with Git credentials helpers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-credentials-0.23
  (package
    (inherit rust-gix-credentials-0.24)
    (name "rust-gix-credentials")
    (version "0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07wpl3ahldcaa44iscr8ashbym0x51rw8gj0wqx78qqpqfkz631q"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-url" ,rust-gix-url-0.26)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-credentials-0.22
  (package
    (inherit rust-gix-credentials-0.23)
    (name "rust-gix-credentials")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gcgr1wvgji9l4wk8pifabb77r48b8x5llx0s1mr89qb8m1aqgai"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-credentials-0.21
  (package
    (inherit rust-gix-credentials-0.22)
    (name "rust-gix-credentials")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1956pmz4sj25kydwh4ardzv9zbdpqrx050g5c4c2m14v0rs5sp0w"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.7)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-credentials-0.20
  (package
    (inherit rust-gix-credentials-0.21)
    (name "rust-gix-credentials")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12mwq0fah6wai26lnq9k3m71lr8cgih43rqy2in6mby59j40p426"))))
    (arguments
     `(#:tests? #f ;wants undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.7)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.24)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-date-0.9
  (package
    (name "rust-gix-date")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-date" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gqij6pgbajq3a07a0y528pqfa6m5nspc4dvffqliqjycixlfz65"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                        ("rust-document-features" ,rust-document-features-0.2)
                        ("rust-itoa" ,rust-itoa-1)
                        ("rust-jiff" ,rust-jiff-0.1)
                        ("rust-serde" ,rust-serde-1)
                        ("rust-thiserror" ,rust-thiserror-2))
       #:cargo-development-inputs
       (("rust-once-cell" ,rust-once-cell-1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this crate parses dates the way Git does")
    (description
     "Part of Gitoxide, this crate parses dates the way Git does.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-date-0.8
  (package
    (inherit rust-gix-date-0.9)
    (name "rust-gix-date")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-date" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h5qygjxr8p2g2vdrzpvx0plnyy9gsym29wjxc5fx48ly8qnkvcy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1))))))

(define-public rust-gix-date-0.5
  (package
    (inherit rust-gix-date-0.8)
    (name "rust-gix-date")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-date" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00jrc86398553z2mdljx9vh8skqgdydhsrr11ak3148fcx2l25mw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs
       (("rust-once-cell" ,rust-once-cell-1))))))

(define-public rust-gix-diff-0.46
  (package
    (name "rust-gix-diff")
    (version "0.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v1k7ld1b4xinq3dhvia4g9dck7b5x22h68wifrzh07z1zcazjcj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-gix-command" ,rust-gix-command-0.3)
        ("rust-gix-filter" ,rust-gix-filter-0.13)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-tempfile" ,rust-gix-tempfile-14)
        ("rust-gix-trace" ,rust-gix-trace-0.1)
        ("rust-gix-worktree" ,rust-gix-worktree-0.36)
        ("rust-imara-diff" ,rust-imara-diff-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Calculate differences between various Git objects")
    (description
      "Calculate differences between various Git objects.  This
package is a part of Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-diff-0.44
  (package
    (inherit rust-gix-diff-0.46)
    (name "rust-gix-diff")
    (version "0.44.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17bci0q2d4bhm6g1gx6ibzbxjj6ykxy6303x8q4rgd85lg4db5hr"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-diff-0.39
  (package
    (inherit rust-gix-diff-0.46)
    (name "rust-gix-diff")
    (version "0.39.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fmrkrs2d3sfkrr0k0mvglmhhz3igh2q9w772xpnhbf4z1a08spx"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-filter" ,rust-gix-filter-0.8)
                       ("rust-gix-fs" ,rust-gix-fs-0.9)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-12)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.29)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-diff-0.38
  (package
    (inherit rust-gix-diff-0.39)
    (name "rust-gix-diff")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pf88521djzb0gygr0idi1rqlxdwcjym2bprpps6izkwi22sj6c1"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-filter" ,rust-gix-filter-0.7)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.28)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-diff-0.37
  (package
    (inherit rust-gix-diff-0.38)
    (name "rust-gix-diff")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m055q3sywj4i3c3xhdw75ir77l6pn3k9bhazimfvjdqkzv984wk"))))
    (arguments
     `(#:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-diff-0.36
  (package
    (inherit rust-gix-diff-0.37)
    (name "rust-gix-diff")
    (version "0.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "134jv0rw7v9lgci65ynq4xy85mvy9rbvpg1n3zl0d0iq5haxp3bq"))))
    (arguments
     `(#:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-dir-0.8
  (package
    (name "rust-gix-dir")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-dir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "166xgf5i4z5bi1jazh5kp2bdvqpbqgb2mh97b8f9ldb1cq3sklqf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;looking for undeclared gix-testtools
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-gix-discover" ,rust-gix-discover-0.35)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-ignore" ,rust-gix-ignore-0.11)
        ("rust-gix-index" ,rust-gix-index-0.35)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
        ("rust-gix-trace" ,rust-gix-trace-0.1)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-gix-worktree" ,rust-gix-worktree-0.36)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Gitoxide project crate that deals with directory walks")
    (description
     "This package is part of the Gitoxide project.  This crate deals with
directory walking.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-dir-0.6
  (package
    (inherit rust-gix-dir-0.8)
    (name "rust-gix-dir")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-dir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r8kif2i622saw068c63jh08q64a5qixnggxgdsjvp80m9wmd5qc"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-discover" ,rust-gix-discover-0.33)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.11)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-dir-0.5
  (package
    (inherit rust-gix-dir-0.8)
    (name "rust-gix-dir")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-dir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x29x6qdj4nwma01qgqadi0dwix3rnv0mlj1wnmn7gasaj69zjb0"))))
    (arguments
     `(#:tests? #f ;looking for undeclared gix-testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-discover" ,rust-gix-discover-0.32)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.11)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-discover-0.35
  (package
    (name "rust-gix-discover")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ljnv5c2q1xpwpw45qhli0hydl7ba52dfpw1dv16ndv7jmmkcxq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-ref" ,rust-gix-ref-0.47)
        ("rust-gix-sec" ,rust-gix-sec-0.10)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-defer" ,rust-defer-0.2)
        ("rust-is-ci" ,rust-is-ci-1)
        ("rust-serial-test" ,rust-serial-test-3)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "Discover Git repositories and check if a directory is a Git repository")
    (description
     "Discover Git repositories and check if a directory is a repository.
This package is part of Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-discover-0.33
  (package
    (inherit rust-gix-discover-0.35)
    (name "rust-gix-discover")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03j52f646q05igg4bln6svjxhxiz944khinj7sx33jy3rqqjfrk7"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.45)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-defer" ,rust-defer-0.2)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-3)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-discover-0.32
  (package
    (inherit rust-gix-discover-0.35)
    (name "rust-gix-discover")
    (version "0.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1grxby9mj5c9bs305wrf70x0rsdpq25nc00csm86p9ixnscwc9zw"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.44)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-defer" ,rust-defer-0.2)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-3)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-discover-0.28
  (package
    (inherit rust-gix-discover-0.35)
    (name "rust-gix-discover")
    (version "0.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mj29906cd3qn9q6am0fc2k2g63jvz3cqim652fqjgfwds4v5mxq"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.40)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-defer" ,rust-defer-0.1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-discover-0.27
  (package
    (inherit rust-gix-discover-0.28)
    (name "rust-gix-discover")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01wwjb9g91nzjfnlhgblsdn7nh8259714r44yxykn2xydr0qkbbg"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.39)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-defer" ,rust-defer-0.1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-discover-0.26
  (package
    (inherit rust-gix-discover-0.27)
    (name "rust-gix-discover")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wlhqkrfyln97arr3hyllw4xc9gnk2qb4nkh70z8hy0i6bq5qpd4"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.38)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-defer" ,rust-defer-0.1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-discover-0.25
  (package
    (inherit rust-gix-discover-0.26)
    (name "rust-gix-discover")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cyy5yahngnh16n08n9qjp69aavxa7nkrxzw0ajaj3jssx1pcl39"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-gix-hash" ,rust-gix-hash-0.13)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-ref" ,rust-gix-ref-0.37)
        ("rust-gix-sec" ,rust-gix-sec-0.10)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-defer" ,rust-defer-0.1)
        ("rust-is-ci" ,rust-is-ci-1)
        ("rust-serial-test" ,rust-serial-test-2)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-features-0.38
  (package
    (name "rust-gix-features")
    (version "0.38.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sfw6zs3qgmlqjkj4cvyfz6q6dgdlw1d16c7yckwgyg5kyn4aw5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-jwalk" ,rust-jwalk-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-28)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-bstr" ,rust-bstr-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Crate to integrate various capabilities using compile-time feature flags")
    (description
     "This package provides a crate to integrate various capabilities using
compile-time feature flags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-features-0.37
  (package
    (inherit rust-gix-features-0.38)
    (name "rust-gix-features")
    (version "0.37.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bsp9lb4cy00irinxis6wvqvsdcm9fc1fnvkl05z6rf6vkl700nm"))))
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-jwalk" ,rust-jwalk-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-28)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-bstr" ,rust-bstr-1))))))

(define-public rust-gix-features-0.36
  (package
    (inherit rust-gix-features-0.38)
    (name "rust-gix-features")
    (version "0.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ckilzixrfylgnw5by3wpmym3ri0v9dbc60dkknfnnxvqsjs8ijd"))))
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-jwalk" ,rust-jwalk-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-26)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-bstr" ,rust-bstr-1))))))

(define-public rust-gix-features-0.35
  (package
    (inherit rust-gix-features-0.36)
    (name "rust-gix-features")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k98r3742xrlqwyaq13a9gazppm5swyx2h4hcmigg0s9mqiz97wv"))))
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-jwalk" ,rust-jwalk-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-26)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-bstr" ,rust-bstr-1))))))

(define-public rust-gix-features-0.30
  (package
    (inherit rust-gix-features-0.35)
    (name "rust-gix-features")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0543ggy5vxr2lpi1405mcq93bshj3gfvpjgf13a60q5z14s4k31s"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-bytesize" ,rust-bytesize-1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-jwalk" ,rust-jwalk-0.8)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-prodash" ,rust-prodash-25)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha1-smol" ,rust-sha1-smol-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs
       (("rust-bstr" ,rust-bstr-1))))))

(define-public rust-gix-filter-0.13
  (package
    (name "rust-gix-filter")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17hi8c47n1mmccpbwfcjs4w829wnfhpcgv1vjmdnw1j0w457j8a1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-gix-attributes" ,rust-gix-attributes-0.22)
        ("rust-gix-command" ,rust-gix-command-0.3)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-packetline-blocking" ,rust-gix-packetline-blocking-0.17)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-gix-trace" ,rust-gix-trace-0.1)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-serial-test" ,rust-serial-test-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this package implements Git filters")
    (description
     "This package provides a crate from the Gitoxide project implementing
Git filters in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-filter-0.11
  (package
    (inherit rust-gix-filter-0.13)
    (name "rust-gix-filter")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06m6ph3b67696ckq5gfn9pwm77sh507km7sfzx6my9r8v8w7fm76"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-gix-attributes" ,rust-gix-attributes-0.22)
        ("rust-gix-command" ,rust-gix-command-0.3)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-object" ,rust-gix-object-0.42)
        ("rust-gix-packetline-blocking" ,rust-gix-packetline-blocking-0.17)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-gix-trace" ,rust-gix-trace-0.1)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-serial-test" ,rust-serial-test-3))))))

(define-public rust-gix-filter-0.8
  (package
    (inherit rust-gix-filter-0.13)
    (name "rust-gix-filter")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05p5yr0syd91k0yfn6pdqw66yvbq7q6ffzpd53s5g7dzi3bc367m"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.21)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-packetline-blocking" ,rust-gix-packetline-blocking-0.17)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-serial-test" ,rust-serial-test-2))))))

(define-public rust-gix-filter-0.7
  (package
    (inherit rust-gix-filter-0.8)
    (name "rust-gix-filter")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w1bgvzr9yjrh00ba2325lwy32x2r4crr496qbkn9hsmisfmqskd"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-packetline-blocking" ,rust-gix-packetline-blocking-0.17)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1))))))

(define-public rust-gix-filter-0.6
  (package
    (inherit rust-gix-filter-0.7)
    (name "rust-gix-filter")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zs288v2l7n8qcbvsjrc3xkm11mynyjwj7jj0ixricdnzp9p9xlj"))))
    (arguments
     `(#:tests? #f  ; use of undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-packetline-blocking" ,rust-gix-packetline-blocking-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1))))))

(define-public rust-gix-filter-0.5
  (package
    (inherit rust-gix-filter-0.6)
    (name "rust-gix-filter")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "024bv38s7q59wsp3whzpcra0x40mv53xii2jrnv5ni21rll0vr0v"))))
    (arguments
     `(#:tests? #f  ; use of undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-packetline-blocking" ,rust-gix-packetline-blocking-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1))))))

(define-public rust-gix-fs-0.11
  (package
    (name "rust-gix-fs")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xc5z6w5s9rr0rgf7aab8hqg0dnblhk543crw30d19pykhjfdgzj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fastrand" ,rust-fastrand-2)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "File system specific utilities to Gitoxide`")
    (description
     "Gitoxide is a pure Rust implementation of Git.  This package provides
file system utilities for Gitoxide.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-fs-0.9
  (package
    (inherit rust-gix-fs-0.11)
    (name "rust-gix-fs")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pgcmp17qizq2zk0s7cn08kd9jhq9rlkk2fbpx5l6dsm00xc4mbm"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-fs-0.8
  (package
    (inherit rust-gix-fs-0.9)
    (name "rust-gix-fs")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01z1whm3qn0pinw4inbpvf53kbfw3kjq48h9vrd6lxzm82q6xs10"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-fs-0.7
  (package
    (inherit rust-gix-fs-0.8)
    (name "rust-gix-fs")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0db6bj773ssqvy03mi28glzy963cd1aaaxcbj4nv7s9glsmmz089"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-features" ,rust-gix-features-0.35))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-fsck-0.4
  (package
    (name "rust-gix-fsck")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-fsck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a3ybr321f7p1qv37hvylihcqjxxqlf3jll8wcm0q6nhwpy2i0mv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.42))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Verifies the connectivity and validity of objects in the database")
    (description "This package verifies the connectivity and validity of objects
in the database.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-glob-0.16
  (package
    (name "rust-gix-glob")
    (version "0.16.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04gijbam0k2vdshm2m0d6hg0hvv7ky4dgr9p4y2l02hapi5qp43l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Gitoxide project crate dealing with pattern matching")
    (description
     "This package provides a crate from the Gitoxide project dealing with pattern
matching.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-glob-0.15
  (package
    (inherit rust-gix-glob-0.16)
    (name "rust-gix-glob")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f610aws4ah12phhbj7h4ypbkj8i00f4dpfw8gip09r6igqk4qmf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-glob-0.14
  (package
    (inherit rust-gix-glob-0.15)
    (name "rust-gix-glob")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06gz18spc8p4b3rbbbh2i2dz1ld2cw3ikgxkwmhjkspfqnc95cax"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-glob-0.13
  (package
    (inherit rust-gix-glob-0.14)
    (name "rust-gix-glob")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17dix59mc93m8z97ywkgpssjsmjgl4cfkifja7vxql8jy62nxmx9"))))
    (arguments
     `(#:tests? #f  ; no method named `trim_start` found for reference `&BStr`
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-hash-0.14
  (package
    (name "rust-gix-hash")
    (version "0.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pjdlxbqxd9lbkccryfw2ghifiq3gz9h8ylliw0va8b16vvpsggr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-faster-hex" ,rust-faster-hex-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Borrowed and owned git hash digests used to identify git objects")
    (description
     "Borrowed and owned git hash digests used to identify git objects.  This
package is part of Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-hash-0.13
  (package
    (inherit rust-gix-hash-0.14)
    (name "rust-gix-hash")
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q1xcp8f5prpyr4x62jixrlgm99snscnf87bny1faqvg4v1gi30z"))))
    (arguments
     `(#:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-faster-hex" ,rust-faster-hex-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-hash-0.11
  (package
    (inherit rust-gix-hash-0.13)
    (name "rust-gix-hash")
    (version "0.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bq986grpsfc6ddav5dlb8zvz1aky264dnnnmax2h1lsmpr2yhjb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-document-features" ,rust-document-features-0.2)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-hashtable-0.5
  (package
    (name "rust-gix-hashtable")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hashtable" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hp2m2rvbv0vav5lkq7d7bvx74qrb6w3hnj1rq3aq69wdzhq1pvx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Hashtable based data structures optimized to utilize ObjectId keys")
    (description
     "Hashtable based data structures optimized to utilize @code{ObjectId}
keys.  Part of Gitoxide a Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-hashtable-0.4
  (package
    (inherit rust-gix-hashtable-0.5)
    (name "rust-gix-hashtable")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hashtable" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jnk93sz53b28ajkn4yrvmh0hj3x2jcb81h6nbqc8zkdh601idpy"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12))))))

(define-public rust-gix-hashtable-0.2
  (package
    (inherit rust-gix-hashtable-0.4)
    (name "rust-gix-hashtable")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hashtable" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13f5v6vghfpzxm5xkmk86gjhsjfqng9rpam37hqjssgkxkk4qprq"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-hash" ,rust-gix-hash-0.11)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12))))))

(define-public rust-gix-ignore-0.11
  (package
    (name "rust-gix-ignore")
    (version "0.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dzs6zlwj8105ynmbiszf319i7x3a3lpav0gda8gaq44b6bcsiz4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "This Gitoxide crate handles .gitignore files")
    (description
     "This crate is part of Gitoxide, it handles @code{.gitignore} files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ignore-0.10
  (package
    (inherit rust-gix-ignore-0.11)
    (name "rust-gix-ignore")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hr6zmh9gx1qx0rwzj1m44sn8plw5wspyi7kf9zdpbk01i2cwmpk"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.15)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))))

(define-public rust-gix-ignore-0.9
  (package
    (inherit rust-gix-ignore-0.10)
    (name "rust-gix-ignore")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ydq53isj75vf7gjggnv8yf2jimx7sfk5xpw66hvqi8nya6cq5d2"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))))

(define-public rust-gix-ignore-0.8
  (package
    (inherit rust-gix-ignore-0.9)
    (name "rust-gix-ignore")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qzmpylhwqqnnb7hcbwfbvblbzg3hzid4d2w42j2vc7nl51z8j5h"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))))

(define-public rust-gix-index-0.35
  (package
    (name "rust-gix-index")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bfjbrwmg8w9hi5bv47db272ks8b3385zrkn45l1fh248hr21m0c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=access::tests::entry_by_path_with_conflicting_file")
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-lock" ,rust-gix-lock-14)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-traverse" ,rust-gix-traverse-0.41)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-gix-validate" ,rust-gix-validate-0.9)
        ("rust-hashbrown" ,rust-hashbrown-0.14)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-memmap2" ,rust-memmap2-0.9)
        ("rust-rustix" ,rust-rustix-0.38)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Part of Gitoxide, this crate implements the Git index file")
    (description
     "Part of Gitoxide, a pure Rust implementation of Rust.  This package's
crate implements the Git index file.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-index-0.33
  (package
    (inherit rust-gix-index-0.35)
    (name "rust-gix-index")
    (version "0.33.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gyply1fzp0wbl8jvckiw4hyv34na7lq0jd4z14bp15xapml96ls"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=access::tests::entry_by_path_with_conflicting_file")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-lock" ,rust-gix-lock-14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.39)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-index-0.28
  (package
    (inherit rust-gix-index-0.35)
    (name "rust-gix-index")
    (version "0.28.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y47r8830i6fw7djq6la8rn7badk8dvg50mqgxzi7m68yqyycl4y"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=access::tests::entry_by_path_with_conflicting_file")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-fs" ,rust-gix-fs-0.9)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-lock" ,rust-gix-lock-12)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.36)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-index-0.27
  (package
    (inherit rust-gix-index-0.28)
    (name "rust-gix-index")
    (version "0.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "173yd650vwvd7ynmwsc7pbf4zr50x4qij3ab4xmfk4i9rpshiwzk"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=access::tests::entry_by_path_with_conflicting_file")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-lock" ,rust-gix-lock-11)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.35)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-index-0.26
  (package
    (inherit rust-gix-index-0.27)
    (name "rust-gix-index")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l0n7cld8m5fq1cnd3lyygmsirw5kzw7gxl8j082wbqv2b64yfn8"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=access::tests::entry_by_path_with_conflicting_file")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-lock" ,rust-gix-lock-11)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.34)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-index-0.25
  (package
    (inherit rust-gix-index-0.26)
    (name "rust-gix-index")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dqp5ri3kh87qgy0gxzlr0m4jki8xk5sr8zm867hh4rws6ln6kgm"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=access::tests::entry_by_path_with_conflicting_file")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-lock" ,rust-gix-lock-10)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.33)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-lock-14
  (package
    (name "rust-gix-lock")
    (version "14.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17g1sknpvjqaq2s29c693mbmkp8sign0174qfi3n3x7ijzi7zg73"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Git style lock files implementation in Rust")
    (description "Part of Gitoxide, a pure rust implementation of Git.  This
package provides git style lock files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-lock-12
  (package
    (inherit rust-gix-lock-14)
    (name "rust-gix-lock")
    (version "12.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "001bh0yx1xnkhnbkfj3p3v649sp5hypm4pgq9jsk1qpijy9l62pl"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-tempfile" ,rust-gix-tempfile-12)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-lock-11
  (package
    (inherit rust-gix-lock-12)
    (name "rust-gix-lock")
    (version "11.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0drgl9qhkvlhjl0jc0lh2h7h3by1yg9wx4a8cqss8c4qlbk6ap3y"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-lock-10
  (package
    (inherit rust-gix-lock-11)
    (name "rust-gix-lock")
    (version "10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15dazvw49bdx60366vngmrfn69rvxf0pr411a1ak6vbbigx9dz27"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-tempfile" ,rust-gix-tempfile-10)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-macros-0.1
  (package
    (name "rust-gix-macros")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05ycxnh5sxjsn4lvay309n6knr8ksvkb6zx6f50ik24zc4iyk74r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Proc-macro utilities for gix")
    (description "Proc-macro utilities for Gitoxide.  Gitoxide is a pure Rust
implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-mailmap-0.24
  (package
    (name "rust-gix-mailmap")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xsj6w3azbcd3qx3pln9f4jabndm7kl59jxlnajy2095xk425mfp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-actor" ,rust-gix-actor-0.32)
        ("rust-gix-date" ,rust-gix-date-0.9)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Functions for parsing mailmap files in Gitoxide")
    (description
     "This package contains functions for parsing mailmap files.  It's part of
Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-mailmap-0.23
  (package
    (inherit rust-gix-mailmap-0.24)
    (name "rust-gix-mailmap")
    (version "0.23.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "024jy339znwi11pz826favmn6in5fn73ygskq33r19dnxnkaqvgg"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-mailmap-0.21
  (package
    (inherit rust-gix-mailmap-0.24)
    (name "rust-gix-mailmap")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05sscbs0qzn40syp0v7s2xrjx10pdfqqg53axr8xrggcmv1mm1dv"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.29)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-mailmap-0.20
  (package
    (inherit rust-gix-mailmap-0.21)
    (name "rust-gix-mailmap")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01wzzs8gifl6i4vzwbx1ywzwgazy1db6yfh8b3bjsssy1pn5ycp2"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-mailmap-0.19
  (package
    (inherit rust-gix-mailmap-0.20)
    (name "rust-gix-mailmap")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nx713bx8bi76h14zgg4786afpzryph16pcg43pndq19dslhzw20"))))
    (arguments
     `(#:tests? #f      ; undeclared crate `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.27)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-negotiate-0.15
  (package
    (name "rust-gix-negotiate")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ns7p4m1skzfkhb5wzr9h01qqsgnrhbqm53g9giak4d157rkn1ml"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
        ("rust-gix-date" ,rust-gix-date-0.9)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-revwalk" ,rust-gix-revwalk-0.15)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Implements Git's negotiation algorithms as part of Gixoxide")
    (description
     "Gitoxide is a pure Rust implementation of Git.  This package consists of
Git's negotiation algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-negotiate-0.13
  (package
    (inherit rust-gix-negotiate-0.15)
    (name "rust-gix-negotiate")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0962x6gxhwp55kyr26hzsswv8qbg9h1f16x8kd8n7fq7cgxpkj4y"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.13)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-negotiate-0.11
  (package
    (inherit rust-gix-negotiate-0.15)
    (name "rust-gix-negotiate")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10nmbml8jz86rq6p2pcc4i7ks0naa9s069i80nnzcng2x6shp0p6"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.23)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.11)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-negotiate-0.10
  (package
    (inherit rust-gix-negotiate-0.11)
    (name "rust-gix-negotiate")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0540dz6pcybn1g3mq47nvpnmj90akkrasl4b07fv6lf0v766m7wp"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-negotiate-0.9
  (package
    (inherit rust-gix-negotiate-0.10)
    (name "rust-gix-negotiate")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zxnxfjjqxap8plkhz5f4h0gwm83ain229y2vhwwxjgcj7sdqp1a"))))
    (arguments
     `(#:tests? #f      ; user of undeclared crate gix_testtools
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.9)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-negotiate-0.8
  (package
    (inherit rust-gix-negotiate-0.9)
    (name "rust-gix-negotiate")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01408hs82nhj40arkdx145cfmdccf7pydf89sywd3ihik6zrf5kg"))))
    (arguments
     `(#:tests? #f      ; user of undeclared crate gix_testtools
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.21)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-object-0.44
  (package
    (name "rust-gix-object")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ylfp09gzy42b1m1day41m49b3d8dggw480qfd07dppi6hc80nrg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-actor" ,rust-gix-actor-0.32)
        ("rust-gix-date" ,rust-gix-date-0.9)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-gix-validate" ,rust-gix-validate-0.9)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-winnow" ,rust-winnow-0.6))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Immutable and mutable Git objects with decoding and encoding support")
    (description
     "Part of Gitoxide, a pure Rust Git implementation.  This package contains
functions to handle immutable and mutable git objects with decoding and encoding
support.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-object-0.42
  (package
    (inherit rust-gix-object-0.44)
    (name "rust-gix-object")
    (version "0.42.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11p3pynmriglj5j3hzh5qa4sz7pnhzxxzr1w85xzmhp7ni32zni5"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.6))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-object-0.40
  (package
    (inherit rust-gix-object-0.44)
    (name "rust-gix-object")
    (version "0.40.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18hklfk1a1mpnr1syxb40dhy92c5yfl6b1ilvsgv8hdaiwp4128c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.29)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-object-0.39
  (package
    (inherit rust-gix-object-0.40)
    (name "rust-gix-object")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cqya07cwq0wcx2kzmxsm9acpl4mwsxwfx797z3c282phb2pkgzy"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-object-0.38
  (package
    (inherit rust-gix-object-0.39)
    (name "rust-gix-object")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lbaz0mzsg5vvm5qvi1nf6f0hyz62hfx18xk3h57fn3z4r22l3vl"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-object-0.37
  (package
    (inherit rust-gix-object-0.38)
    (name "rust-gix-object")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vzxayiss5k7pmlm0q4axaliidg9a3lq7bkv2ds775k7dihijzhy"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.27)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-object-0.30
  (package
    (inherit rust-gix-object-0.37)
    (name "rust-gix-object")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1is6hkzcv38m9k2yzja39h3d9s5rvs9vqpfb17kw7pj43kswh9l9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-btoi" ,rust-btoi-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-actor" ,rust-gix-actor-0.21)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-validate" ,rust-gix-validate-0.7)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-nom" ,rust-nom-7)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-odb-0.63
  (package
    (name "rust-gix-odb")
    (version "0.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0iy7grqs5h73nz781zfmiyisr9pm4yjxsamby1ady5qwf1l805d3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-arc-swap" ,rust-arc-swap-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-date" ,rust-gix-date-0.9)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-pack" ,rust-gix-pack-0.53)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-serde" ,rust-serde-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implements various Git object databases")
    (description
      "Implements various Git object databases for Gitoxide.
Gitoxide is a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-odb-0.61
  (package
    (inherit rust-gix-odb-0.63)
    (name "rust-gix-odb")
    (version "0.61.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16xwwfjna9m3rd66hkhcyps6s3bq23r5wpbxpfixi4qxakz89lr0"))))
    (arguments
     `(#:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-pack" ,rust-gix-pack-0.51)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-odb-0.56
  (package
    (inherit rust-gix-odb-0.63)
    (name "rust-gix-odb")
    (version "0.56.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c9kgiv8frpwz9nz9n6wai8xys8vawnfhw1mnz1cchfyffl6vbj6"))))
    (arguments
     `(#:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-pack" ,rust-gix-pack-0.46)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-odb-0.55
  (package
    (inherit rust-gix-odb-0.56)
    (name "rust-gix-odb")
    (version "0.55.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b1vr5yp96a3ik32a41rp749v2nir7n6hhwdrip9rja02nbmzbhz"))))
    (arguments
     `(#:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-pack" ,rust-gix-pack-0.45)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-odb-0.54
  (package
    (inherit rust-gix-odb-0.55)
    (name "rust-gix-odb")
    (version "0.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fql2p0xinkdaq7bybz12j1yw0b4lq3d1nl3sf2ad3qdp1nbac46"))))
    (arguments
     `(#:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-pack" ,rust-gix-pack-0.44)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-odb-0.53
  (package
    (inherit rust-gix-odb-0.54)
    (name "rust-gix-odb")
    (version "0.53.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gv4zvmizqvxh4n3gnv3yzp83v3spklj0cf6rlrz38m3dcn3jsld"))))
    (arguments
     `(#:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-pack" ,rust-gix-pack-0.43)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-pack-0.53
  (package
    (name "rust-gix-pack")
    (version "0.53.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m6rdxkq21ni71vk8k7qbsjxr7mgkjpdijh3wkhf28gf5qsal8rj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;use of undeclared crate gix_testtools
       #:cargo-inputs
       (("rust-clru" ,rust-clru-0.6)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-chunk" ,rust-gix-chunk-0.4)
        ("rust-gix-diff" ,rust-gix-diff-0.46)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-tempfile" ,rust-gix-tempfile-14)
        ("rust-gix-traverse" ,rust-gix-traverse-0.41)
        ("rust-memmap2" ,rust-memmap2-0.9)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-uluru" ,rust-uluru-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implements Git packs and related data structures")
    (description
     "Git packs and related data structures.  Gitoxide is a pure
Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-pack-0.51
  (package
    (inherit rust-gix-pack-0.53)
    (name "rust-gix-pack")
    (version "0.51.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aajbwpmfzg7a33yzqfjz2rmczvbask13hd19gwmvrgz3x4r819y"))))
    (arguments
     `(#:tests? #f  ; use of undeclared crate gix_testtools
       #:cargo-inputs (("rust-clru" ,rust-clru-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.44)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.39)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uluru" ,rust-uluru-3))))))

(define-public rust-gix-pack-0.46
  (package
    (inherit rust-gix-pack-0.53)
    (name "rust-gix-pack")
    (version "0.46.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16jsy3b1rnp0xinwlaz71zsnamqzhnlys3a0bhnhf50ag514savq"))))
    (arguments
     `(#:tests? #f      ; use of undeclared create gix_testtools
       #:cargo-inputs (("rust-clru" ,rust-clru-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.39)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-12)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.36)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uluru" ,rust-uluru-3))))))

(define-public rust-gix-pack-0.45
  (package
    (inherit rust-gix-pack-0.46)
    (name "rust-gix-pack")
    (version "0.45.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "019jbbbxq53r5l7sfh3vcbyyzagrmxhg6vs5fgrxsvs4j8f4jsa5"))))
    (arguments
     `(#:tests? #f      ; use of undeclared create gix_testtools
       #:cargo-inputs (("rust-clru" ,rust-clru-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.38)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.35)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uluru" ,rust-uluru-3))))))

(define-public rust-gix-pack-0.44
  (package
    (inherit rust-gix-pack-0.45)
    (name "rust-gix-pack")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hwphs7ks8pf6v4wrmhd4iy8vj1in95db4q6j82i9zyy60pblc8l"))))
    (arguments
     `(#:tests? #f      ; use of undeclared create gix_testtools
       #:cargo-inputs (("rust-clru" ,rust-clru-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.37)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.34)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uluru" ,rust-uluru-3))))))

(define-public rust-gix-pack-0.43
  (package
    (inherit rust-gix-pack-0.44)
    (name "rust-gix-pack")
    (version "0.43.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cqsxdqz6sdk1m96mpa0f33kddx1inx91gsbd72in7mk8lx20dkm"))))
    (arguments
     `(#:tests? #f      ; use of undeclared create gix_testtools
       #:cargo-inputs (("rust-clru" ,rust-clru-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.36)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.33)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uluru" ,rust-uluru-3))))))

(define-public rust-gix-packetline-0.17
  (package
    (name "rust-gix-packetline")
    (version "0.17.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-packetline" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
          "0jay9kgy8fgc809xcipgwhz430a4pyywhcb7c0n25yp2bx6yyhwc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-faster-hex" ,rust-faster-hex-0.9)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-maybe-async" ,rust-maybe-async-0.2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Crate of the gitoxide project implementing the pkt-line serialization format")
    (description
     "This package provides a crate of the gitoxide project implementing the
pkt-line serialization format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-packetline-0.16
  (package
    (inherit rust-gix-packetline-0.17)
    (name "rust-gix-packetline")
    (version "0.16.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-packetline" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ayp00k4a6qgjjyrd2rmzxyx0n8h36dxscjn1pzil5b4x6qq90wa"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-faster-hex" ,rust-faster-hex-0.8)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-lite" ,rust-futures-lite-1)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-maybe-async" ,rust-maybe-async-0.2))))))

(define-public rust-gix-packetline-blocking-0.17
  (package
    (name "rust-gix-packetline-blocking")
    (version "0.17.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-packetline-blocking" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h3bansw7mp2p6id78lwlrs4pm8nclmql07qbxpxv657p822705r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-faster-hex" ,rust-faster-hex-0.9)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Duplicate of @code{gix-packetline} with @code{blocking-io} selected")
    (description "Part of Gitoxide, a pure Rust implementation of Git.  This
package is a duplicate of @code{gix-packetline} with the @code{blocking-io}
feature pre-selected.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-packetline-blocking-0.16
  (package
    (inherit rust-gix-packetline-blocking-0.17)
    (name "rust-gix-packetline-blocking")
    (version "0.16.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-packetline-blocking" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f9dr79jz9y11qhf0syxwa4nvn4czpyka84hzshxd10wa3vrb0vx"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-faster-hex" ,rust-faster-hex-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-path-0.10
  (package
    (name "rust-gix-path")
    (version "0.10.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-path" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ds5hvyjwic564jbvpy14ibrf1f8701q0877n2p41qsi3bpr5hmg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-thiserror" ,rust-thiserror-2))
       #:cargo-development-inputs
       (("rust-known-folders" ,rust-known-folders-1)
        ("rust-serial-test" ,rust-serial-test-3)
        ("rust-windows" ,rust-windows-0.58)
        ("rust-winreg" ,rust-winreg-0.52))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Part of the Gitoxide project, this crate deals with paths and their conversions")
    (description
     "Part of the Gitoxide project, a lean and safe Rust implementation of
Git.  This crate deals with paths and their conversions")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-pathspec-0.7
  (package
    (name "rust-gix-pathspec")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pathspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ssw9k2kq7hcn5xn9nby4gvq2628clxapf331r6l3d1jjlivy8sx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-gix-attributes" ,rust-gix-attributes-0.22)
        ("rust-gix-config-value" ,rust-gix-config-value-0.14)
        ("rust-gix-glob" ,rust-gix-glob-0.16)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-serial-test" ,rust-serial-test-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Capabilities to handle Git's pathspecs")
    (description
     "This package provides capabilities for handling Git's pathspecs.  It is
part of Gitoxide, a Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-pathspec-0.5
  (package
    (inherit rust-gix-pathspec-0.7)
    (name "rust-gix-pathspec")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pathspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0iqj3l4z29sg2z4bb21dn3n58wd1jgl6ydpnradzh13wa7lhxnqc"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.21)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-glob" ,rust-gix-glob-0.15)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-serial-test" ,rust-serial-test-2))))))

(define-public rust-gix-pathspec-0.4
  (package
    (inherit rust-gix-pathspec-0.5)
    (name "rust-gix-pathspec")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pathspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ka9h2lfgbfbby5rciipgy6nkl1qkcrhp0xvr11z13m3flpvkfqx"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-serial-test" ,rust-serial-test-2))))))

(define-public rust-gix-pathspec-0.3
  (package
    (inherit rust-gix-pathspec-0.4)
    (name "rust-gix-pathspec")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pathspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zg5m586s0sxxwadlvsx9hrrkyym9d4q936kk0zyf6y58ydnrqn3"))))
    (arguments
     `(#:tests? #f ;undeclared crate gix_testtools
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-serial-test" ,rust-serial-test-2))))))

(define-public rust-gix-prompt-0.8
  (package
    (name "rust-gix-prompt")
    (version "0.8.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-prompt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1505js24g8dziljc7jl5frmk0af1847v106fqsxmz75wqjpj4y3s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-thiserror" ,rust-thiserror-2))
       #:cargo-development-inputs (("rust-expectrl" ,rust-expectrl-0.7)
                                   ("rust-serial-test" ,rust-serial-test-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Handles prompts in the terminal as part of Gitoxide")
    (description
     "Gitoxide is a Rust implementation of Git.  This crate handles the
terminals prompt.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-prompt-0.7
  (package
    (inherit rust-gix-prompt-0.8)
    (name "rust-gix-prompt")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-prompt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y26b3d7z222b223ir9qf8yqwhknzc3c5yksjffmwvsid4vr36jw"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-expectrl" ,rust-expectrl-0.7)
                                   ("rust-serial-test" ,rust-serial-test-2))))))

(define-public rust-gix-protocol-0.45
  (package
    (name "rust-gix-protocol")
    (version "0.45.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gjf2s9ssch79jfyv7bpa8pxwgdqks6940x04bpfzd81dw0a2hyc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-lite" ,rust-futures-lite-2)
        ("rust-gix-credentials" ,rust-gix-credentials-0.24)
        ("rust-gix-date" ,rust-gix-date-0.9)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-transport" ,rust-gix-transport-0.42)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-maybe-async" ,rust-maybe-async-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implementation of Git protocols that's part of Gitoxide")
    (description
     "This package implements Git protocols for Gitoxide.  Gitoxide is a pure
Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-protocol-0.43
  (package
    (inherit rust-gix-protocol-0.45)
    (name "rust-gix-protocol")
    (version "0.43.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pfg4b0b6a753mxrp2x7jaclry6d88x3lzxx7dgi14ar8cw2g9gc"))))
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.23)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-transport" ,rust-gix-transport-0.40)
                       ("rust-maybe-async" ,rust-maybe-async-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-gix-packetline" ,rust-gix-packetline-0.17))))))

(define-public rust-gix-protocol-0.42
  (package
    (inherit rust-gix-protocol-0.43)
    (name "rust-gix-protocol")
    (version "0.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05zq1vpak1nd83155j4n7gqvj8zmzkxr39yybddd22yv0zs6wwwm"))))
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-transport" ,rust-gix-transport-0.39)
                       ("rust-maybe-async" ,rust-maybe-async-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-gix-packetline" ,rust-gix-packetline-0.17))))))

(define-public rust-gix-protocol-0.41
  (package
    (inherit rust-gix-protocol-0.42)
    (name "rust-gix-protocol")
    (version "0.41.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03hy77hbszssdkc4iwig3f82ib4i6agfag37svd90pzsppm3y7ir"))))
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.21)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-transport" ,rust-gix-transport-0.38)
                       ("rust-maybe-async" ,rust-maybe-async-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-gix-packetline" ,rust-gix-packetline-0.16))))))

(define-public rust-gix-protocol-0.40
  (package
    (inherit rust-gix-protocol-0.41)
    (name "rust-gix-protocol")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16qc5q53z5jdqb433106lzzifhf321zgv89ha65bxj8cq86p0yyc"))))
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.20)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-transport" ,rust-gix-transport-0.37)
                       ("rust-maybe-async" ,rust-maybe-async-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-gix-packetline" ,rust-gix-packetline-0.16))))))

(define-public rust-gix-quote-0.4
  (package
    (name "rust-gix-quote")
    (version "0.4.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-quote" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qws02kwhnn0hcl5cizc99irw07qi3nycmyd2qlaphkf461f58b4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Various quotations used by git")
    (description
     "This package provides a crate of the gitoxide project dealing with various
quotations used by git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ref-0.47
  (package
    (name "rust-gix-ref")
    (version "0.47.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r8i8fj7xgmp88qk1w7xc2gs7l8sb8y0azx5algskaprxc3883df"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;use of undeclared crate gix_testtools
       #:cargo-inputs
       (("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-actor" ,rust-gix-actor-0.32)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-lock" ,rust-gix-lock-14)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-tempfile" ,rust-gix-tempfile-14)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-gix-validate" ,rust-gix-validate-0.9)
        ("rust-memmap2" ,rust-memmap2-0.9)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis "Part of Gitoxide, this crate handles Git references")
    (description "This package provides a crate to handle git references.
Part of Gitoxide, a project to create a pure Rust Git implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ref-0.45
  (package
    (inherit rust-gix-ref-0.47)
    (name "rust-gix-ref")
    (version "0.45.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dcv2pgaz6gzhx38zw0qh9pkmhqhf4hqq2gf7waia9snlnh9cvk3"))))
    (arguments
     `(#:tests? #f  ; use of undeclared crate gix_testtools
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-lock" ,rust-gix-lock-14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.6))
       #:cargo-development-inputs (("rust-gix-date" ,rust-gix-date-0.8))))))

(define-public rust-gix-ref-0.44
  (package
    (inherit rust-gix-ref-0.47)
    (name "rust-gix-ref")
    (version "0.44.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0biy3y7src1wsx5prynvyg7wzyzf3ss8f6hyphpb5ijvgscs551k"))))
    (arguments
     `(#:tests? #f ;use of undeclared crate gix_testtools
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-lock" ,rust-gix-lock-14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.6))))))

(define-public rust-gix-ref-0.40
  (package
    (inherit rust-gix-ref-0.47)
    (name "rust-gix-ref")
    (version "0.40.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j96yj1j6mr06qspgxjdssssbf7dzj2bxz5224sqz3b3hhcvvnb4"))))
    (arguments
     `(#:tests? #f      ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.29)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-fs" ,rust-gix-fs-0.9)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-lock" ,rust-gix-lock-12)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-12)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))))))

(define-public rust-gix-ref-0.39
  (package
    (inherit rust-gix-ref-0.40)
    (name "rust-gix-ref")
    (version "0.39.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zmvbjqg7b46banbp7a7507wa1mx992gcmgg2wrpzkqjqannj81v"))))
    (arguments
     `(#:tests? #f      ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-lock" ,rust-gix-lock-11)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))))))

(define-public rust-gix-ref-0.38
  (package
    (inherit rust-gix-ref-0.39)
    (name "rust-gix-ref")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ljasz4v4bikrb06wdp7hafznmhqh0zgmqvy02w2z3f8gb8gdhhf"))))
    (arguments
     `(#:tests? #f      ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-lock" ,rust-gix-lock-11)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))))))

(define-public rust-gix-ref-0.37
  (package
    (inherit rust-gix-ref-0.38)
    (name "rust-gix-ref")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r4d0zpin2c62s4j88n32cd0gf1f2da1kp4yfr0kcq8bcr4vgri2"))))
    (arguments
     `(#:tests? #f      ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.27)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-lock" ,rust-gix-lock-10)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-10)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))))))

(define-public rust-gix-refspec-0.25
  (package
    (name "rust-gix-refspec")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19lwhs10cg9rg040k20w8i27ay475isggngxbxhngfj14gw0bc7b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-revision" ,rust-gix-revision-0.29)
        ("rust-gix-validate" ,rust-gix-validate-0.9)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Parsing and representing refspecs for Gitoxide")
    (description
     "This package parses and represents Git refspecs.  It's part of Gitoxide
a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-refspec-0.23
  (package
    (inherit rust-gix-refspec-0.25)
    (name "rust-gix-refspec")
    (version "0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0drzc1p43mc8nprxlqm4s86y9b23rsz89dvqghgmsmb25v6zhs38"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-revision" ,rust-gix-revision-0.27)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-refspec-0.21
  (package
    (inherit rust-gix-refspec-0.25)
    (name "rust-gix-refspec")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01771g6dr5jqg9p1pvl6d7m5x52yfzgwqgm5namka5rc17srs8dy"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-revision" ,rust-gix-revision-0.25)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-refspec-0.20
  (package
    (inherit rust-gix-refspec-0.21)
    (name "rust-gix-refspec")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vivb1g2ispw1mgibnq7yirvqbbclxgfldqwvk08zrqy5swd7nbn"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-revision" ,rust-gix-revision-0.24)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-refspec-0.19
  (package
    (inherit rust-gix-refspec-0.20)
    (name "rust-gix-refspec")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rx6q4k13zciaajz9a6g1wb1w70y92m6fzqc30xb9g8xqi69gc6c"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-revision" ,rust-gix-revision-0.23)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-refspec-0.18
  (package
    (inherit rust-gix-refspec-0.19)
    (name "rust-gix-refspec")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07pniqh74kkm0n727m7wjxgrgwnaypljkhsh8nyw7wvh3rxwp588"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-revision" ,rust-gix-revision-0.22)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revision-0.29
  (package
    (name "rust-gix-revision")
    (version "0.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07jyh9vabfbkva99m0mavsk68v3asp1k0c48as9dn35c36r22ims"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-date" ,rust-gix-date-0.9)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-revwalk" ,rust-gix-revwalk-0.15)
        ("rust-gix-trace" ,rust-gix-trace-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "This Gitoxide crate finds names for revisions and parsing specifications")
    (description
     "This package is part of Gitoxide, a pure Rust implementation of Git.  It
handles finding names and parsing specifications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-revision-0.27
  (package
    (inherit rust-gix-revision-0.29)
    (name "rust-gix-revision")
    (version "0.27.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kmd96vky58yf46g5nxqny8gl3df4447vb6x6w2lp30iq91kxc81"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.13)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revision-0.25
  (package
    (inherit rust-gix-revision-0.29)
    (name "rust-gix-revision")
    (version "0.25.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17ydyj6f75zsz6ygmh3im4631cdx5b58spxi9myr7g1k6vgy2y5a"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.11)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revision-0.24
  (package
    (inherit rust-gix-revision-0.25)
    (name "rust-gix-revision")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i4mb8d34ar2g7flwjbnf8vwb0xlzaa078sy66y38m6f20bxapgy"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revision-0.23
  (package
    (inherit rust-gix-revision-0.24)
    (name "rust-gix-revision")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1215fz886j5gzf31kg32g566vm9pds5679d4d9vg79sr6k3pma9c"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.9)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revision-0.22
  (package
    (inherit rust-gix-revision-0.23)
    (name "rust-gix-revision")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "128fi6mblg4ic6h1q5vy2zq6vly8hxhi9vxkpkskaymby9fb3i68"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.8)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revwalk-0.15
  (package
    (name "rust-gix-revwalk")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "111r3ddls12y1s884x01gai8ykdsn4qmnggghbk4w24k9da747ml"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
         ("rust-gix-date" ,rust-gix-date-0.9)
         ("rust-gix-hash" ,rust-gix-hash-0.14)
         ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
         ("rust-gix-object" ,rust-gix-object-0.44)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Utilities for walking Git's revision graph")
    (description
     "Utilities for walking Git's revision graph.  This crate is part of
Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-revwalk-0.13
  (package
    (inherit rust-gix-revwalk-0.15)
    (name "rust-gix-revwalk")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q23mcf4ji5q8qi3g86vxxgz4x4ykgqja0kyacgi9bvimg50q0qv"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revwalk-0.11
  (package
    (inherit rust-gix-revwalk-0.15)
    (name "rust-gix-revwalk")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pz411j9hpdls77qglgcwxk794pb4fbqnflz460dxg9bbkzfabbh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.23)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Utilities for walking the Git's revision graph")
    (description
     "Utilities for walking Git's revision graph.  This crate is part of
Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-revwalk-0.10
  (package
    (inherit rust-gix-revwalk-0.11)
    (name "rust-gix-revwalk")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pf15qvha76skcyvysmmxfvb7fpq3s716izizmss956ajcjfvm39"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revwalk-0.9
  (package
    (inherit rust-gix-revwalk-0.10)
    (name "rust-gix-revwalk")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q7sgvkm0zdpp09v51jgv7c77zff82fvyr82dzc7dmjc5s4qqvd1"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revwalk-0.8
  (package
    (inherit rust-gix-revwalk-0.9)
    (name "rust-gix-revwalk")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yd27ip3xhxplsw6cmyjz1vk6q5c0qhkn33icx2hiwij21mhr1z9"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.21)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revwalk-0.1
  (package
    (inherit rust-gix-revwalk-0.8)
    (name "rust-gix-revwalk")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jnkyb5yk8y76q5gbmdykm2snxn5v9dbc4jy3walz4a7hyx269mw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.16)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-hashtable" ,rust-gix-hashtable-0.2)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-sec-0.10
  (package
    (name "rust-gix-sec")
    (version "0.10.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-sec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mjgs5wgfc236i3293winiaplnx4sscc6blsh2bm75bsk7ppdf58"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this create provides a shared trust model")
    (description
     "This package is part of Gitoxide, it implements a shared trust model.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-status-0.13
  (package
    (name "rust-gix-status")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1spznjc0s41cnyzfhvm37yr0fpx0fjiima6cwjk1c34zcfx3a3gp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-gix-diff" ,rust-gix-diff-0.46)
        ("rust-gix-dir" ,rust-gix-dir-0.8)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-filter" ,rust-gix-filter-0.13)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-index" ,rust-gix-index-0.35)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
        ("rust-gix-worktree" ,rust-gix-worktree-0.36)
        ("rust-portable-atomic" ,rust-portable-atomic-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
     "Gitoxide project crate that provides @code{git status} functionality")
    (description
     "This Gitoxide project crate provides @code{git status} functionality.
Gitoxide is a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-status-0.11
  (package
    (inherit rust-gix-status-0.13)
    (name "rust-gix-status")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dipwd6r0nbyipqc94fdlcn60gd1bispkd3brq3x1hv5rf2b1xw3"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-diff" ,rust-gix-diff-0.44)
                       ("rust-gix-dir" ,rust-gix-dir-0.6)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-status-0.10
  (package
    (inherit rust-gix-status-0.13)
    (name "rust-gix-status")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j5z44b80ybaxy34valibksasrd5gny52vqk9mahjf3ii7cp6hrg"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-diff" ,rust-gix-diff-0.44)
                       ("rust-gix-dir" ,rust-gix-dir-0.5)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-status-0.4
  (package
    (inherit rust-gix-status-0.13)
    (name "rust-gix-status")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07diw4dvb49m3ip1vh1vn7fx4sdapysbwc5vhzxmc15c9dl70gwh"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-filter" ,rust-gix-filter-0.8)
                       ("rust-gix-fs" ,rust-gix-fs-0.9)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-index" ,rust-gix-index-0.28)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.29)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-status-0.3
  (package
    (inherit rust-gix-status-0.4)
    (name "rust-gix-status")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "091xhhk3g9ccizv5qw8gdj1h87v6qn84nqd5b7psda9hjd2i2ni5"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-filter" ,rust-gix-filter-0.7)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-index" ,rust-gix-index-0.27)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.28)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-status-0.2
  (package
    (inherit rust-gix-status-0.3)
    (name "rust-gix-status")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c3a1y91444vdl1krhhybhlcb5fmjcwll8g9df1fbg27zcgjfm0w"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-filter" ,rust-gix-filter-0.6)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-index" ,rust-gix-index-0.26)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.4)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.27)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-status-0.1
  (package
    (inherit rust-gix-status-0.2)
    (name "rust-gix-status")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yi2wl5c1zacr5nr8qj9g7rq9ylwp2ckvkjwgd0g41x75g202a08"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-index" ,rust-gix-index-0.25)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-submodule-0.14
  (package
    (name "rust-gix-submodule")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jw7rb181z5ns0njxh9pi4fa4d8n7lgfn5cg46rp5wy2ikvhm7aj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-gix-config" ,rust-gix-config-0.40)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
        ("rust-gix-refspec" ,rust-gix-refspec-0.25)
        ("rust-gix-url" ,rust-gix-url-0.27)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/GitoxideLabs/gitoxide")
    (synopsis
      "Functions for dealing with Git submodules")
    (description
     "Functions for dealing with Git submodules.  Part of Gitoxide a pure Rust
implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-submodule-0.12
  (package
    (inherit rust-gix-submodule-0.14)
    (name "rust-gix-submodule")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zalx2cc2b2sj8kqncbkbpnj1np9lxr09j4yscwmx000m9lhybhg"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-config" ,rust-gix-config-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.23)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-submodule-0.11
  (package
    (inherit rust-gix-submodule-0.14)
    (name "rust-gix-submodule")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b304hkx2r2b619n3lksvj08fkd7pdxzpr923dhvc55c4jcx874j"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-config" ,rust-gix-config-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.23)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-submodule-0.7
  (package
    (inherit rust-gix-submodule-0.14)
    (name "rust-gix-submodule")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0day1xp9pfxki9cmp0z4kyhh6ygrm427z2f9zkgj9pi2j903im11"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-config" ,rust-gix-config-0.33)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.5)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.21)
                       ("rust-gix-url" ,rust-gix-url-0.26)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-submodule-0.6
  (package
    (inherit rust-gix-submodule-0.7)
    (name "rust-gix-submodule")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y89k2c4isa9r4j9a5mim91dxx69s6ckqrl1i7mwmgcm1bvdg8q2"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-config" ,rust-gix-config-0.32)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.4)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.20)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-submodule-0.5
  (package
    (inherit rust-gix-submodule-0.6)
    (name "rust-gix-submodule")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hm1d7a9qb3zylln44bxcnmdy27zfajc6gj5g00kf95a2a6qr9xv"))))
    (arguments
     `(#:tests? #f  ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-config" ,rust-gix-config-0.31)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.4)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.19)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-submodule-0.4
  (package
    (inherit rust-gix-submodule-0.5)
    (name "rust-gix-submodule")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dz2w3ikhbf938rfamlqkmxl9hznz4i7mm9dmgrd70lj5vl500fx"))))
    (arguments
     `(#:tests? #f  ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-config" ,rust-gix-config-0.30)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.3)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.18)
                       ("rust-gix-url" ,rust-gix-url-0.24)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-tempfile-14
  (package
    (name "rust-gix-tempfile")
    (version "14.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ah8y51fbx6x9m71ihxk3zhny0c05kka5kd0a2hida4zjqkljsq4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dashmap" ,rust-dashmap-6)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-signal-hook" ,rust-signal-hook-0.3)
        ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Rust tempfile implementation with assured cleanup")
    (description
     "Part of Gitoxide a Rust implementation of Git.  This package provides a
tempfile capability with a global registry to assure clean-up.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-tempfile-12
  (package
    (inherit rust-gix-tempfile-14)
    (name "rust-gix-tempfile")
    (version "12.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aiypbizzx997lw8yvz2sk4nnmxz1yq233j5kc8my7wbf5nkgvx8"))))
    (arguments
     `(#:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-fs" ,rust-gix-fs-0.9)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-tempfile-11
  (package
    (inherit rust-gix-tempfile-12)
    (name "rust-gix-tempfile")
    (version "11.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08vykvfdgxvqqm63zav1rw730qm6cdnnvqni52dwcvm82j8x539q"))))
    (arguments
     `(#:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-tempfile-10
  (package
    (inherit rust-gix-tempfile-11)
    (name "rust-gix-tempfile")
    (version "10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kdc21s0dnqnbzfdazpsw8fclnw1gi3w4np71qlmgp0i7s7rgq2s"))))
    (arguments
     `(#:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-trace-0.1
  (package
    (name "rust-gix-trace")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-trace" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lmr5qqdk44xbz695asvwvc60ggaxrd12gjdl8izrw991h9dxg84"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-document-features" ,rust-document-features-0.2)
        ("rust-tracing-core" ,rust-tracing-core-0.1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Crate to provide minimal `tracing` support")
    (description
     "This package provides a crate to provide minimal `tracing` support that
can be turned off to zero cost.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-transport-0.42
  (package
    (name "rust-gix-transport")
    (version "0.42.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07s8lsq97r0hgg9znd2f0jaj49prm2bss9mjjxfs2h8vn35cq7a2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-base64" ,rust-base64-0.22)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-curl" ,rust-curl-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-lite" ,rust-futures-lite-2)
        ("rust-gix-command" ,rust-gix-command-0.3)
        ("rust-gix-credentials" ,rust-gix-credentials-0.24)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-packetline" ,rust-gix-packetline-0.17)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-gix-sec" ,rust-gix-sec-0.10)
        ("rust-gix-url" ,rust-gix-url-0.27)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-reqwest" ,rust-reqwest-0.12)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-blocking" ,rust-blocking-1)
        ("rust-maybe-async" ,rust-maybe-async-0.2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Implements the Git transport layer for Gitoxide")
    (description
     "This package is part of Gitoxide a Rust implementation of Git.  It
provides an implementation of the Git transport layer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-transport-0.40
  (package
    (inherit rust-gix-transport-0.42)
    (name "rust-gix-transport")
    (version "0.40.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w4pngjzbyvm68affvglnkzcyksgv5wxivnakx05lfg9acha40dy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.23)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-packetline" ,rust-gix-packetline-0.17)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.26)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-blocking" ,rust-blocking-1)
                                   ("rust-maybe-async" ,rust-maybe-async-0.2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implements the Git transport layer for Gitoxide")
    (description
     "This package is part of Gitoxide a Rust implementation of Git.  It
provides an implementation of the Git transport layer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-transport-0.39
  (package
    (inherit rust-gix-transport-0.40)
    (name "rust-gix-transport")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i5ig30l4gqyh70qhm1gdmm2aa3qf33galr0vp36h96nqkpwycgp"))))
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.22)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-packetline" ,rust-gix-packetline-0.17)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-blocking" ,rust-blocking-1)
                                   ("rust-maybe-async" ,rust-maybe-async-0.2))))))

(define-public rust-gix-transport-0.38
  (package
    (inherit rust-gix-transport-0.39)
    (name "rust-gix-transport")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lp7bg7pj9l2na92bdrbx0zjybi7j88c26vm341z492f6s9rl81g"))))
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.21)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-packetline" ,rust-gix-packetline-0.16)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-blocking" ,rust-blocking-1)
                                   ("rust-maybe-async" ,rust-maybe-async-0.2))))))

(define-public rust-gix-transport-0.37
  (package
    (inherit rust-gix-transport-0.38)
    (name "rust-gix-transport")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kpv0aqx7c3hbsc14rk7c01yd7b73mm14d4swnn6hpi4d9p75v5r"))))
    (arguments
     `(#:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-curl" ,rust-curl-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-lite" ,rust-futures-lite-1)
        ("rust-gix-command" ,rust-gix-command-0.2)
        ("rust-gix-credentials" ,rust-gix-credentials-0.20)
        ("rust-gix-features" ,rust-gix-features-0.35)
        ("rust-gix-packetline" ,rust-gix-packetline-0.16)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-gix-sec" ,rust-gix-sec-0.10)
        ("rust-gix-url" ,rust-gix-url-0.24)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-blocking" ,rust-blocking-1)
        ("rust-maybe-async" ,rust-maybe-async-0.2))))))

(define-public rust-gix-traverse-0.41
  (package
    (name "rust-gix-traverse")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "105prigk1pq7g3i3hvwh4y9rhc35ycl84cg9f9ag6kafz6da6383"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
        ("rust-gix-date" ,rust-gix-date-0.9)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-revwalk" ,rust-gix-revwalk-0.15)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Ways to traverse Git commit graphs and trees for Gix")
    (description "Part of Gitoxide, a pure Rust implementation of Git.  This
package is used to traverse Git commit graphs and trees.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-traverse-0.39
  (package
    (inherit rust-gix-traverse-0.41)
    (name "rust-gix-traverse")
    (version "0.39.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h48bbjym6fkdhy9q7p8v6rn9ksvkwxp8fs1415cyw8ya66a36g4"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.13)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-traverse-0.36
  (package
    (inherit rust-gix-traverse-0.41)
    (name "rust-gix-traverse")
    (version "0.36.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qbiq230wj8ak60zzg613pgflwnvsyj71x9liys0k957bd29w435"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.23)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.11)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-traverse-0.35
  (package
    (inherit rust-gix-traverse-0.36)
    (name "rust-gix-traverse")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x04z3xybwkr6wls0rpdr8n3pdhd091bsky8j9jj1812h44148fz"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-traverse-0.34
  (package
    (inherit rust-gix-traverse-0.35)
    (name "rust-gix-traverse")
    (version "0.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12pk1w89kj978jdfsg2fwmq5p4gv0i0wydh6pxmbf6sfgpn51l0l"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.9)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-traverse-0.33
  (package
    (inherit rust-gix-traverse-0.34)
    (name "rust-gix-traverse")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "180vp1piwlalsv8qkmmif03l7h2kdxfx5vawkclbmb236smh9vr2"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.21)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-url-0.27
  (package
    (name "rust-gix-url")
    (version "0.27.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "128c2rp6780qspiqasmyd6a3gdmcmq6kl19axllf28pvhig0qa7x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
         ("rust-document-features" ,rust-document-features-0.2)
         ("rust-gix-features" ,rust-gix-features-0.38)
         ("rust-gix-path" ,rust-gix-path-0.10)
         ("rust-home" ,rust-home-0.5)
         ("rust-serde" ,rust-serde-1)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-assert-matches" ,rust-assert-matches-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "This crate implements parsing and serialization of gix-url for Gitoxide")
    (description
     "This package is part of Gitoxide a Rust implementation of Git.  It
provides a crate for parsing and serialization of gix-url's.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-url-0.26
  (package
    (inherit rust-gix-url-0.27)
    (name "rust-gix-url")
    (version "0.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i6wjjaq1yhxqk9gbriv9raldhs04yy91ipys4qs4lkmxg61f3wg"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-assert-matches" ,rust-assert-matches-1))))))

(define-public rust-gix-url-0.25
  (package
    (inherit rust-gix-url-0.26)
    (name "rust-gix-url")
    (version "0.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01a0phpk3f0lrhavqm51cgpdwh925i2djiyslaj57ync24d7lhhc"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-assert-matches" ,rust-assert-matches-1))))))

(define-public rust-gix-url-0.24
  (package
    (inherit rust-gix-url-0.25)
    (name "rust-gix-url")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03s2ds2z6whd44qapyvz4kqfpniik0issv6s09rbys4cdvsfq9b1"))))
    (arguments
     `(#:tests? #f ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-libtest-mimic" ,rust-libtest-mimic-0.6))))))

(define-public rust-gix-utils-0.1
  (package
    (name "rust-gix-utils")
    (version "0.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0prkj13jwvcrd8gy8f73ci2i3nwkak88xpvdibcqwl4rjlz7whms"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-fastrand" ,rust-fastrand-2)
        ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Crate with `gitoxide` utilities that don't need feature toggles")
    (description
     "This package provides a crate with `gitoxide` utilities that don't need
feature toggles.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-validate-0.9
  (package
    (name "rust-gix-validate")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-validate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dq9flm4iz1pzsiqqjmmxxzsp2fsdarx18db695v71gmz44hslnd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Validation functions for various kinds of names in Git")
    (description "Part of Gitoxide, a pure Rust implementation of Git.  This
package contains validation functions for various kinds of names in Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-validate-0.8
  (package
    (inherit rust-gix-validate-0.9)
    (name "rust-gix-validate")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-validate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kqad8a2wdz69ma7hspi21pazgpkrc5hg4iw37gsvca99b9pvhl2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-validate-0.7
  (package
    (inherit rust-gix-validate-0.8)
    (name "rust-gix-validate")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-validate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h4hr3rpgwc7ixyynjp53s9il3sb0gq8ad332k8drwyfn8vkg6xs"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-0.36
  (package
    (name "rust-gix-worktree")
    (version "0.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16hl0gckhybirv86vv3ds5qhjrh4marmq39nbf38xfpjldvas4n3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-attributes" ,rust-gix-attributes-0.22)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-glob" ,rust-gix-glob-0.16)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-ignore" ,rust-gix-ignore-0.11)
        ("rust-gix-index" ,rust-gix-index-0.35)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-validate" ,rust-gix-validate-0.9)
        ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Gitoxide functions for handling shared Git worktree types and utilities")
    (description
     "This package provides functions for handling shared Git worktree related
types and utilities.  Part of Gitoxide a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-0.34
  (package
    (inherit rust-gix-worktree-0.36)
    (name "rust-gix-worktree")
    (version "0.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19p7xzrl1i4jw2fhzywqkfd00dn58k9nksll0qi7548bprp35xr6"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.11)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-worktree-0.29
  (package
    (inherit rust-gix-worktree-0.36)
    (name "rust-gix-worktree")
    (version "0.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lld035lsx3y0d2qxpnys2j63nhl36510i76k6arly7hpy52z62k"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.21)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-fs" ,rust-gix-fs-0.9)
                       ("rust-gix-glob" ,rust-gix-glob-0.15)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.10)
                       ("rust-gix-index" ,rust-gix-index-0.28)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-worktree-0.28
  (package
    (inherit rust-gix-worktree-0.29)
    (name "rust-gix-worktree")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d30k1748k5zdxa24ifww9ym16lga7bphz8ir3lan57f3ph0l7bz"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.9)
                       ("rust-gix-index" ,rust-gix-index-0.27)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-worktree-0.27
  (package
    (inherit rust-gix-worktree-0.28)
    (name "rust-gix-worktree")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zfpqbrxxwjjhjk1rn60rmajxm4f7ix2jbx44vklz9nv47kpkbyx"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.9)
                       ("rust-gix-index" ,rust-gix-index-0.26)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-worktree-0.26
  (package
    (inherit rust-gix-worktree-0.27)
    (name "rust-gix-worktree")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zhl1wlf4zl0ny1mwp7j24gzln63xy2grrh9cvaq5g8152bk4plz"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.8)
                       ("rust-gix-index" ,rust-gix-index-0.25)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-worktree-state-0.13
  (package
    (name "rust-gix-worktree-state")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hyyhvigwnb8cybxxyiqp5rxlw8nw39qs1nmmh5jqw7s2frw81bv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-filter" ,rust-gix-filter-0.13)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-glob" ,rust-gix-glob-0.16)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-index" ,rust-gix-index-0.35)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-worktree" ,rust-gix-worktree-0.36)
        ("rust-io-close" ,rust-io-close-0.3)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Gitoxide project library to set a Git worktree to a particular state")
    (description
     "This crate sets the Git worktree to a particular state.  Part of Gitoxide
a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-state-0.11
  (package
    (inherit rust-gix-worktree-state-0.13)
    (name "rust-gix-worktree-state")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n2wv2fgdryxn1pa0rg2k43lqc7kpjx4665ihnj6f47mnl2n5v9r"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-io-close" ,rust-io-close-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-state-0.6
  (package
    (inherit rust-gix-worktree-state-0.13)
    (name "rust-gix-worktree-state")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1890rq778ac6va1pki0d4379mpinirs892z71hvm3h1449rlxiyq"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-filter" ,rust-gix-filter-0.8)
                       ("rust-gix-fs" ,rust-gix-fs-0.9)
                       ("rust-gix-glob" ,rust-gix-glob-0.15)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-index" ,rust-gix-index-0.28)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.29)
                       ("rust-io-close" ,rust-io-close-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-state-0.5
  (package
    (inherit rust-gix-worktree-state-0.6)
    (name "rust-gix-worktree-state")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00gsa6vzxc8ya5h9yp33wa668ak6ah95llcilfycy331zqf3rvxg"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-filter" ,rust-gix-filter-0.7)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-index" ,rust-gix-index-0.27)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.28)
                       ("rust-io-close" ,rust-io-close-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-state-0.4
  (package
    (inherit rust-gix-worktree-state-0.5)
    (name "rust-gix-worktree-state")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m5y0qsf8r7sl6ffvi5ymd6497a5mb4k3pq31b072g5gvk6gr8il"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-filter" ,rust-gix-filter-0.6)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-index" ,rust-gix-index-0.26)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.27)
                       ("rust-io-close" ,rust-io-close-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-state-0.3
  (package
    (inherit rust-gix-worktree-state-0.4)
    (name "rust-gix-worktree-state")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bcpspxpvl8yybz8b0p64pjxb69b7kmkhsyv9jgarigjc1lv1bn3"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-filter" ,rust-gix-filter-0.5)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-index" ,rust-gix-index-0.25)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.26)
                       ("rust-io-close" ,rust-io-close-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-stream-0.15
  (package
    (name "rust-gix-worktree-stream")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jnvzmsp3h2yzv575a9d2dvkkfvzqgyxd0mnahmf5v53q63ips38"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-gix-attributes" ,rust-gix-attributes-0.22)
        ("rust-gix-features" ,rust-gix-features-0.38)
        ("rust-gix-filter" ,rust-gix-filter-0.13)
        ("rust-gix-fs" ,rust-gix-fs-0.11)
        ("rust-gix-hash" ,rust-gix-hash-0.14)
        ("rust-gix-object" ,rust-gix-object-0.44)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-traverse" ,rust-gix-traverse-0.41)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
      "Gitoxide library to generate a byte-stream from a git-tree")
    (description
      "This crate provides the ability to generate a byte-stream from a git-tree.
It's part of Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-stream-0.13
  (package
    (inherit rust-gix-worktree-stream-0.15)
    (name "rust-gix-worktree-stream")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08gd3fca4jjaygsawpm7s6f3p7rvb3br87ap8ipqahcs4jb4hpg3"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.39)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-stream-0.8
  (package
    (inherit rust-gix-worktree-stream-0.15)
    (name "rust-gix-worktree-stream")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05qb9603wdv15l3h27i9s657j6yrpdnnli0x9x9jvkcas98jk1mg"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-attributes" ,rust-gix-attributes-0.21)
                       ("rust-gix-features" ,rust-gix-features-0.37)
                       ("rust-gix-filter" ,rust-gix-filter-0.8)
                       ("rust-gix-fs" ,rust-gix-fs-0.9)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.40)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.36)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-stream-0.7
  (package
    (inherit rust-gix-worktree-stream-0.8)
    (name "rust-gix-worktree-stream")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v0hcsljsiw2c3vcjc5gvgd4iyw9m84h5cpc1fh960fkkqk3qy1w"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-filter" ,rust-gix-filter-0.7)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.39)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.35)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-stream-0.6
  (package
    (inherit rust-gix-worktree-stream-0.7)
    (name "rust-gix-worktree-stream")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b5gf6pq9ypxhg0x9dj9b1agrhbj7rz64r10d0kp6d69z2v38jzf"))))
    (arguments
     `(#:tests? #f ; using undeclared crates gix_worktree/gix_testtools
       #:cargo-inputs (("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-filter" ,rust-gix-filter-0.6)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.34)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-stream-0.5
  (package
    (inherit rust-gix-worktree-stream-0.6)
    (name "rust-gix-worktree-stream")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sbmcar5r2wrv5dacx3381ykvc06fpfsfad4dfnw6p7428xfk7y8"))))
    (arguments
     `(#:tests? #f ; using undeclared crates gix_worktree/gix_testtools
       #:cargo-inputs (("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-filter" ,rust-gix-filter-0.5)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.33)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-libgit2-sys-0.17
  (package
    (name "rust-libgit2-sys")
    (version "0.17.0+1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "093jxfl2i9vxdlgf7vk9d040sjwy0nq4fid640y7qix6m0k26iqh"))
       (modules '((guix build utils)))
       (snippet
        #~(begin (delete-file-recursively "libgit2")))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libssh2-sys" ,rust-libssh2-sys-0.3)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.8 openssl zlib))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Native bindings to the libgit2 library")
    (description
     "This package provides native Rust bindings to the @code{libgit2}
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libgit2-sys-0.16
  (package
    (inherit rust-libgit2-sys-0.17)
    (name "rust-libgit2-sys")
    (version "0.16.2+1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s149nkximw3n42925rp0jax1sf1zmf8gpcim2g9sp7fnkc2chgf"))
       (modules '((guix build utils)))
       (snippet
        #~(begin (delete-file-recursively "libgit2")))))
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libssh2-sys" ,rust-libssh2-sys-0.3)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.7 openssl zlib))))

(define-public rust-libgit2-sys-0.15
  (package
    (inherit rust-libgit2-sys-0.16)
    (name "rust-libgit2-sys")
    (version "0.15.2+1.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yllyq9wiryy257cfx8s7wadls24yzkxnhmbl95iz9ml3zhz43d8"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "libgit2")))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.3)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (inputs (list libgit2-1.6 openssl zlib))))

(define-public rust-libgit2-sys-0.14
  (package
    (inherit rust-libgit2-sys-0.15)
    (name "rust-libgit2-sys")
    (version "0.14.2+1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1926x5f84ykr8j2lsdmb0n0bj4jz173j5bm722cgwx8hnpv9agbz"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "libgit2")))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (inputs
     (list libgit2 openssl zlib))))

(define-public rust-libgit2-sys-0.13
  (package
    (inherit rust-libgit2-sys-0.14)
    (name "rust-libgit2-sys")
    (version "0.13.5+1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fkdgfqdkd38hfsyw3znq629gp7wdknzslym5l0g29k9q83fmrai"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "libgit2")))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (inputs
     (list libgit2-1.4 openssl zlib))))

(define-public rust-libgit2-sys-0.12
  (package
    (inherit rust-libgit2-sys-0.14)
    (name "rust-libgit2-sys")
    (version "0.12.26+1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "153l8nvz9p8vyd5840xi6fwblvhpn3c33jwdwsznyq4f4jcwiq8r"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "libgit2")))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-libgit2-sys-0.8
  (package
    (inherit rust-libgit2-sys-0.12)
    (name "rust-libgit2-sys")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0y2mibmx7wy91s2kmb2gfb29mrqlqaxpy5wcwr8s1lwws7b9w5sc"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "libgit2") #t))))))

(define-public rust-libpijul-1
  (package
    (name "rust-libpijul")
    (version "1.0.0-beta.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libpijul" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yjii3hh8s7cvp5aihngh49266iz9h5bwlfrcyd1dglinyv0f213"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; file not found for module `rollback`
       #:cargo-inputs (("rust-adler32" ,rust-adler32-1)
                       ("rust-aes" ,rust-aes-0.7)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-blake3" ,rust-blake3-1)
                       ("rust-bs58" ,rust-bs58-0.4)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-canonical-path" ,rust-canonical-path-2)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-chardetng" ,rust-chardetng-0.1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-curve25519-dalek" ,rust-curve25519-dalek-3)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-diffs" ,rust-diffs-0.5)
                       ("rust-ed25519-dalek" ,rust-ed25519-dalek-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-ignore" ,rust-ignore-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lru-cache" ,rust-lru-cache-0.1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-path-slash" ,rust-path-slash-0.1)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.9)
                       ("rust-pijul-macros" ,rust-pijul-macros-0.5)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-sanakirja" ,rust-sanakirja-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-twox-hash" ,rust-twox-hash-1)
                       ("rust-zstd-seekable" ,rust-zstd-seekable-0.1))
       #:cargo-development-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-detone" ,rust-detone-1)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3))))
    (home-page "https://nest.pijul.com/pijul/libpijul")
    (synopsis "Library component of the pijul version control system")
    (description
     "This crate contains the core API to access Pijul repositories.

The key object is a @code{Repository}, on which @code{Txn} (immutable
transactions) and @code{MutTxn} (mutable transactions) can be started, to
perform a variety of operations.

Another important object is a @code{Patch}, which encodes two different pieces
of information:

@itemize
@item Information about deleted and inserted lines between two versions of a
file.
@item Information about file moves, additions and deletions.
@end itemize")
    (license license:gpl2+)))

(define-public rust-pijul-config-0.0.1
  (package
    (name "rust-pijul-config")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pijul-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19xdcqyw2lh4r8rs5dfjzqf27ajfld2gimw07bhvnjixz0gplylq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-dialoguer" ,rust-dialoguer-0.10)
                       ("rust-dirs-next" ,rust-dirs-next-2)
                       ("rust-edit" ,rust-edit-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://nest.pijul.com/pijul/pijul")
    (synopsis "Configuration for the pijul version control system")
    (description
     "This package provides configuration for the pijul version control system.")
    (license license:gpl2)))

(define-public rust-pijul-identity-0.0.1
  (package
    (name "rust-pijul-identity")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pijul-identity" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hpsw5k3s47nf8hxmi7pb6552ld3xz688hqldz57n9v7d7k660m1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-dateparser" ,rust-dateparser-0.1)
                       ("rust-dirs-next" ,rust-dirs-next-2)
                       ("rust-keyring" ,rust-keyring-2)
                       ("rust-libpijul" ,rust-libpijul-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pijul-config" ,rust-pijul-config-0.0.1)
                       ("rust-pijul-interaction" ,rust-pijul-interaction-0.0.1)
                       ("rust-pijul-repository" ,rust-pijul-repository-0.0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-thrussh-keys" ,rust-thrussh-keys-0.21)
                       ("rust-toml" ,rust-toml-0.7)
                       ("rust-validator" ,rust-validator-0.15)
                       ("rust-whoami" ,rust-whoami-1))))
    (native-inputs (list pkg-config))
    (inputs (list libsodium openssl))
    (home-page "https://nest.pijul.com/pijul/pijul")
    (synopsis "Functionality to interact with Pijul identities")
    (description
     "This package provides functionality to interact with Pijul identities.")
    (license license:gpl2)))

(define-public rust-pijul-interaction-0.0.1
  (package
    (name "rust-pijul-interaction")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pijul-interaction" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vv1z0flsgvf09y18sv2jbk9sn0rllka15wdskpv9ziydp9xr1h7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dialoguer" ,rust-dialoguer-0.10)
                       ("rust-duplicate" ,rust-duplicate-1)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pijul-config" ,rust-pijul-config-0.0.1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://nest.pijul.com/pijul/pijul")
    (synopsis "Human friendly input/output for contexts such as terminals or GUIs")
    (description
     "This package provides human friendly input/output (progress bars,
passwords etc) for contexts such as terminals or GUIs.")
    (license license:gpl2)))

(define-public rust-pijul-macros-0.5
  (package
    (name "rust-pijul-macros")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pijul-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r3hivav1mzpzdpk2rj8flhl4vgp2r85gdi0kw5x8r8bv0y79cy2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://nest.pijul.com/pijul/pijul")
    (synopsis "Macros used to write libpijul")
    (description "This package provides macros used to write libpijul.")
    (license license:gpl2)))

(define-public rust-pijul-remote-1
  (package
    (name "rust-pijul-remote")
    (version "1.0.0-beta.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pijul-remote" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pxk6bv3q9ppqivj6rdy2lgv444zxxkqs8jgb1zq3412j6gpsa6d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-dirs-next" ,rust-dirs-next-2)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-keyring" ,rust-keyring-2)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libpijul" ,rust-libpijul-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pijul-config" ,rust-pijul-config-0.0.1)
                       ("rust-pijul-identity" ,rust-pijul-identity-0.0.1)
                       ("rust-pijul-interaction" ,rust-pijul-interaction-0.0.1)
                       ("rust-pijul-repository" ,rust-pijul-repository-0.0.1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-sanakirja" ,rust-sanakirja-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thrussh" ,rust-thrussh-0.34)
                       ("rust-thrussh-config" ,rust-thrussh-config-0.6)
                       ("rust-thrussh-keys" ,rust-thrussh-keys-0.22)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2))))
    (native-inputs (list pkg-config))
    (inputs (list libsodium openssl))
    (home-page "https://nest.pijul.com/pijul/pijul")
    (synopsis "Functionality to communicate with a pijul repository remotely")
    (description
     "This package provides functionality to communicate with a pijul
repository remotely.")
    (license license:gpl2)))

(define-public rust-pijul-repository-0.0.1
  (package
    (name "rust-pijul-repository")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pijul-repository" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06vl6id0amw1pxfhj6m34mvr171czikph8g21xg34k7c263maiq4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-libpijul" ,rust-libpijul-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pijul-config" ,rust-pijul-config-0.0.1)
                       ("rust-rlimit" ,rust-rlimit-0.9)
                       ("rust-toml" ,rust-toml-0.7))))
    (home-page "https://nest.pijul.com/pijul/pijul")
    (synopsis "Functionality for interacting with a pijul repository")
    (description
     "This package provides functionality for interacting with a pijul repository.")
    (license license:gpl2)))

