;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Pierre-Henry Fröhring <phfrohring@deeplinks.com>
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

(define-module (gnu packages elixir-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages elixir)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system mix)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public elixir-nimble-parsec
  (package
    (name "elixir-nimble-parsec")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "0rxiw6jzz77v0j460wmzcprhdgn71g1hrz3mcc6djn7bnb0f70i6"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (synopsis "Text-based parser combinators")
    (description
     "This library provides primitives for efficient parser combinators, allowing
for higher-level combinators through composition.")
    (home-page "https://hexdocs.pm/nimble_parsec/")
    (license license:asl2.0)))

(define-public elixir-makeup
  (package
    (name "elixir-makeup")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "19jpprryixi452jwhws3bbks6ki3wni9kgzah3srg22a3x8fsi8a"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-nimble-parsec))
    (arguments (list #:tests? #f)) ; no tests
    (synopsis "Syntax highlighter for source code")
    (description
     "Makeup is a generic syntax highlighter in the style of Pygments suitable for use in code hosting,
forums, wikis or other applications that need to prettify source code.")
    (home-page "https://hexdocs.pm/makeup/")
    (license license:bsd-2)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
