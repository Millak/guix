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
  #:use-module (gnu packages sqlite)
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
    (version "0.103.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hpriinxjpwy4v99dd52gayhc2h3kmzkryzm5arm081kwaw33ay8"))))
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
       #:install-source? #f))
    (native-inputs (list pkg-config))
    (inputs (cons* mimalloc openssl sqlite (cargo-inputs 'nushell)))
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
    (version "0.103.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_formats" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v05w0kc77q431lbr2vnm8za4p3psj9zykkz2r5bydkzbk8frvkz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'nu-plugin-formats))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu_plugin_formats")
    (synopsis "An I/O plugin for a set of file formats for Nushell")
    (description
     "This package provides An I/O plugin for a set of file formats for Nushell.")
    (license license:expat)))

(define-public nu-plugin-inc
  (package
    (name "nu-plugin-inc")
    (version "0.103.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_inc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s7s1h1ffz72mv703a9gsi77hl7rsqkfxq7vhxqrz5p0cwh7fviw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'nu-plugin-inc))
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
