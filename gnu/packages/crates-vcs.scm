;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
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
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define-public rust-git-testament-0.2
  (package
    (name "rust-git-testament")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "git-testament" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c9l10wpyz39vhb5cijvbym6gmpmw3y3nb35l2hg6w42h1ygaswq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-git-testament-derive" ,rust-git-testament-derive-0.1)
        ("rust-no-std-compat" ,rust-no-std-compat-0.4))
       #:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/kinnison/git-testament/")
    (synopsis "Record git working tree status when compiling your crate")
    (description "Record git working tree status when compiling your crate")
    (license license:bsd-3)))

(define-public rust-git-testament-derive-0.1
  (package
    (name "rust-git-testament-derive")
    (version "0.1.14")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "git-testament-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rlais0i47mgsmp3r5jcqry2agjfyg5s9paj6mgvfykchssjsy2a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs
       (("rust-git-testament" ,rust-git-testament-0.2))))
    (home-page "https://github.com/kinnison/git-testament/")
    (synopsis "Record git working tree status when compiling your crate")
    (description
     "This package provides an inner procedural macro for git-testament.")
    (license license:bsd-3)))

(define-public rust-git-version-0.3
  (package
    (name "rust-git-version")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-version" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qj1rd19v8vg094b3fj0gy6ca53v93lhrl31wg1fs7g0y61qx4cl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-git-version-macro" ,rust-git-version-macro-0.3)
        ("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5))))
    (home-page "https://github.com/fusion-engineering/rust-git-version")
    (synopsis "Embed git information in your code at compile-time")
    (description
     "This crates compiles the git version (tag name, or hash otherwise) and
dirty state into your program.")
    (license license:bsd-2)))

(define-public rust-git-version-macro-0.3
  (package
    (name "rust-git-version-macro")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-version-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mynlf8sfaa4xx7qff0qgnr339fbf1svgr569yip067fzm97ma9l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/fusion-engineering/rust-git-version")
    (synopsis "Internal macro crate for git-version")
    (description
     "This is an internal macro crate for git-version.")
    (license license:bsd-2)))

(define-public rust-git2-0.18
  (package
    (name "rust-git2")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kf0kvg3i7p1223zs2h9fz99ndm0l9kdx3hcw63g73dh5nlppygv"))))
    (build-system cargo-build-system)
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
       #:cargo-development-inputs (("rust-structopt" ,rust-structopt-0.3)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-time" ,rust-time-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.7 libssh2 openssl zlib))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Rust bindings to libgit2")
    (description
     "This package provides bindings to libgit2 for interoperating with git
repositories.  This library is both threadsafe and memory safe and allows both
reading and writing git repositories.")
    (license (list license:expat license:asl2.0))))

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
       ("git" ,git-minimal)))           ;for a single test
    (inputs
     (list libgit2 libssh2 openssl zlib))))

(define-public rust-git2-0.13
  (package
    (inherit rust-git2-0.15)
    (name "rust-git2")
    (version "0.13.24")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07rlxwvl5iyyqwh0mci5v27lbicf9qiqm60maw1srz7i51x00pl4"))))
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
        ("rust-time" ,rust-time-0.1))))
    (inputs
     (list libgit2-1.3 libssh2 openssl zlib))))

(define-public rust-git2-0.11
  (package
    (inherit rust-git2-0.13)
    (name "rust-git2")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i0fgsr91r97hsjbgqnymkcyiyg0057m7m04116k3vmyqpvrwlbp"))))
    (arguments
     `(#:tests? #f      ; (signal: 11, SIGSEGV: invalid memory reference)
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-docopt" ,rust-docopt-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thread-id" ,rust-thread-id-3)
        ("rust-time" ,rust-time-0.1))))))

(define-public rust-git2-0.9
  (package
    (inherit rust-git2-0.11)
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

(define-public rust-git2-0.6
  (package
    (inherit rust-git2-0.11)
    (name "rust-git2-6")
    (version "0.6.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "115ys6vlfjy5pcwkip0wfzi4q3d1kimbl9isxvjyci1arnvlnnzf"))))
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.6)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-1))
       #:cargo-development-inputs
       (("rust-docopt" ,rust-docopt-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-time" ,rust-time-0.1))))
    (inputs
     (modify-inputs (package-inputs rust-git2-0.11)
       (prepend curl)))))

(define-public rust-git2-curl-0.18
  (package
    (name "rust-git2-curl")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2-curl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "132zzrrfw3cnfh9ffc9pfr94my97agnmk7pnfvzqr4kj5d1vgy7q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t                 ;need rust-civet and others
       #:cargo-inputs
       (("rust-curl" ,rust-curl-0.4)
        ("rust-git2" ,rust-git2-0.17)
        ("rust-log" ,rust-log-0.4)
        ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Libgit2 HTTP transport backend powered by @code{libcurl}")
    (description "Backend for an HTTP transport in @code{libgit2}, powered by
libcurl, which is intended to be used with the @code{git2} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-git2-curl-0.17
  (package
    (inherit rust-git2-curl-0.18)
    (name "rust-git2-curl")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2-curl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cydakv91acxkxfj0kazp9sc4mmr8l51649mi06wk9qv6kkg8xvm"))))
    (arguments
     `(#:skip-build? #t                 ;need rust-civet and others
       #:cargo-inputs
       (("rust-curl" ,rust-curl-0.4)
        ("rust-git2" ,rust-git2-0.16)
        ("rust-log" ,rust-log-0.4)
        ("rust-url" ,rust-url-2))))))
