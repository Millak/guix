;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu packages c2rust)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages llvm)
  #:use-module ((guix licenses) #:prefix license:))

(define-public rust-c2rust-bitfields-derive-0.18
  (package
    (name "rust-c2rust-bitfields-derive")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-bitfields-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i95j6q1d61h1m1pk84i3ih00hsmbn8ib35xr129fz2rw81c3jyk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://c2rust.com/")
    (synopsis
     "C-compatible struct bitfield derive implementation used in the C2Rust project")
    (description
     "This package provides C-compatible struct bitfield derive implementation used in the C2Rust project.")
    (license license:bsd-3)))

(define-public rust-c2rust-bitfields-0.18
  (package
    (name "rust-c2rust-bitfields")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-bitfields" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h9cnyijk65zypv8dqbmr5r238pqq9pa8njrdzx09xhfmc3kyg5l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-c2rust-bitfields-derive" ,rust-c2rust-bitfields-derive-0.18))))
    (home-page "https://c2rust.com/")
    (synopsis
     "C-compatible struct bitfield implementation used in the C2Rust project")
    (description
     "This package provides C-compatible struct bitfield implementation used in the C2Rust project.")
    (license license:bsd-3)))

(define-public rust-c2rust-ast-printer-0.18
  (package
    (name "rust-c2rust-ast-printer")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-ast-printer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a02bnnxn1difq917c2rv8b7654ni65lyk37hdyklv9n96inr07r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-prettyplease" ,rust-prettyplease-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://c2rust.com/")
    (synopsis "Customized version of libsyntax rust pretty-printer")
    (description
     "This package provides Customized version of libsyntax rust pretty-printer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-c2rust-ast-builder-0.18
  (package
    (name "rust-c2rust-ast-builder")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-ast-builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w63rp66g6axkymxd16avxp3gjnphy3mg9938gsh52p4aak83nq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://c2rust.com/")
    (synopsis "Rust AST builder support crate for the C2Rust project")
    (description
     "This package provides Rust AST builder support crate for the C2Rust project.")
    (license license:bsd-3)))

(define-public rust-c2rust-build-paths-0.18
  (package
    (name "rust-c2rust-build-paths")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-build-paths" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b2liaxbqksgfbsmr6hacdia6czlq7m0pyqx3l2rrcfcnb2ksgv0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-print-bytes" ,rust-print-bytes-1))))
    (home-page "https://c2rust.com/")
    (synopsis
     "C2Rust utilities related to build paths, primarily at build time")
    (description
     "This package provides C2Rust utilities related to build paths, primarily at build time.")
    (license license:bsd-3)))
