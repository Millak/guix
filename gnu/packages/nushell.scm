;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages nushell)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-shell)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))


(define-public nushell
  (package
    (name "nushell")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hi8jhn22qyhhwxwgsncyxzmf06wd6lx0qgxn88022xggbwh7ipz"))
       (snippet
        #~(begin (delete-file
                   "tests/fixtures/partial_completions/partial-a/have_ext.exe")))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--"
         "--skip=path::canonicalize::canonicalize_tilde"
         "--skip=path::canonicalize::canonicalize_tilde_relative_to"
         "--skip=plugin_persistence"
         "--skip=plugins"
         "--skip=repl")
       ;#:features '("system-clipboard")
       #:install-source? #f
       #:cargo-inputs
       (("rust-crossterm" ,rust-crossterm-0.28)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-log" ,rust-log-0.4)
        ("rust-miette" ,rust-miette-7)
        ("rust-mimalloc" ,rust-mimalloc-0.1)
        ("rust-multipart-rs" ,rust-multipart-rs-0.1)
        ("rust-nix" ,rust-nix-0.29)
        ("rust-nu-cli" ,rust-nu-cli-0.101)
        ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.101)
        ("rust-nu-cmd-extra" ,rust-nu-cmd-extra-0.101)
        ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.101)
        ("rust-nu-cmd-plugin" ,rust-nu-cmd-plugin-0.101)
        ("rust-nu-command" ,rust-nu-command-0.101)
        ("rust-nu-engine" ,rust-nu-engine-0.101)
        ("rust-nu-explore" ,rust-nu-explore-0.101)
        ("rust-nu-lsp" ,rust-nu-lsp-0.101)
        ("rust-nu-parser" ,rust-nu-parser-0.101)
        ("rust-nu-path" ,rust-nu-path-0.101)
        ("rust-nu-plugin-engine" ,rust-nu-plugin-engine-0.101)
        ("rust-nu-protocol" ,rust-nu-protocol-0.101)
        ("rust-nu-std" ,rust-nu-std-0.101)
        ("rust-nu-system" ,rust-nu-system-0.101)
        ("rust-nu-utils" ,rust-nu-utils-0.101)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-reedline" ,rust-reedline-0.38)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-simplelog" ,rust-simplelog-0.12)
        ("rust-time" ,rust-time-0.3)
        ("rust-winresource" ,rust-winresource-0.1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-nu-plugin-core" ,rust-nu-plugin-core-0.101)
        ("rust-nu-plugin-protocol" ,rust-nu-plugin-protocol-0.101)
        ("rust-nu-test-support" ,rust-nu-test-support-0.101)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-rstest" ,rust-rstest-0.23)
        ("rust-serial-test" ,rust-serial-test-3)
        ("rust-tango-bench" ,rust-tango-bench-0.6)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list mimalloc openssl))
    (home-page "https://www.nushell.sh")
    (synopsis "Shell with a structured approach to the command line")
    (description
     "Nu draws inspiration from projects like PowerShell, functional
programming languages, and modern CLI tools.  Rather than thinking of files
and services as raw streams of text, Nu looks at each input as something with
structure.  For example, when you list the contents of a directory, what you
get back is a table of rows, where each row represents an item in that
directory.  These values can be piped through a series of steps, in a series
of commands called a ``pipeline''.")
    (license license:expat)))

;; Nushell plugins need to be built against the same nu-plugin protocol
;; version as nushell itself.

(define-public nu-plugin-formats
  (package
    (name "nu-plugin-formats")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_formats" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bpsqchhv3q9x38i5kn7gpfrxv875k0vgb7k3r732d7f59nxzd93"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-eml-parser" ,rust-eml-parser-0.1)
                       ("rust-ical" ,rust-ical-0.11)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-nu-plugin" ,rust-nu-plugin-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-plist" ,rust-plist-1)
                       ("rust-rust-ini" ,rust-rust-ini-0.21))
       #:cargo-development-inputs
       (("rust-nu-plugin-test-support" ,rust-nu-plugin-test-support-0.101))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu_plugin_formats")
    (synopsis "An I/O plugin for a set of file formats for Nushell")
    (description
     "This package provides An I/O plugin for a set of file formats for Nushell.")
    (license license:expat)))

(define-public nu-plugin-inc
  (package
    (name "nu-plugin-inc")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_inc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12kr3lnjhq6grdnhxrmlxgmvvgv7kc6fg18z6gfk2qim1sckikyd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-plugin" ,rust-nu-plugin-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101)
                       ("rust-semver" ,rust-semver-1))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu_plugin_inc")
    (synopsis "Version incrementer plugin for Nushell")
    (description
     "This package provides a version incrementer plugin for Nushell.")
    (license license:expat)))

(define-public nu-plugin-gstat
  (package
    (name "nu-plugin-gstat")
    (version "0.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_gstat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jpxbgkffwxh02ccjxd9mkb0z1j0c58mh75vw1c62g74775mdkpc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-git2" ,rust-git2-0.19)
                       ("rust-nu-plugin" ,rust-nu-plugin-0.101)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.101))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.8 libssh2 openssl zlib))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu_plugin_gstat")
    (synopsis "Git status plugin for Nushell")
    (description "This package provides a git status plugin for Nushell.")
    (license license:expat)))

