;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages crates-io))

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
