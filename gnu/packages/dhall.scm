;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages dhall)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (guix download)
  #:use-module (guix build-system haskell)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public dhall
  (package
    (name "dhall")
    (version "1.41.2")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "dhall" version))
              (sha256
               (base32
                "14m5rrvkid76qnvg0l14xw1mnqclhip3gjrz20g1lp4fd5p056ka"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dhall")))
    (inputs (list ghc-aeson
                  ghc-aeson-pretty
                  ghc-ansi-terminal
                  ghc-atomic-write
                  ghc-base16-bytestring
                  ghc-case-insensitive
                  ghc-cborg
                  ghc-cborg-json
                  ghc-contravariant
                  ghc-data-fix
                  ghc-diff
                  ghc-dotgen
                  ghc-either
                  ghc-half
                  ghc-hashable
                  ghc-indexed-traversable
                  ghc-lens-family-core
                  ghc-megaparsec
                  ghc-mmorph
                  ghc-network-uri
                  ghc-optparse-applicative
                  ghc-parsers
                  ghc-parser-combinators
                  ghc-prettyprinter
                  ghc-prettyprinter-ansi-terminal
                  ghc-pretty-simple
                  ghc-profunctors
                  ghc-repline
                  ghc-serialise
                  ghc-scientific
                  ghc-text-manipulate
                  ghc-text-short
                  ghc-th-lift-instances
                  ghc-unordered-containers
                  ghc-uri-encode
                  ghc-vector
                  ghc-cryptohash-sha256
                  ghc-http-types
                  ghc-http-client
                  ghc-http-client-tls))
    (native-inputs (list ghc-foldl
                         ghc-generic-random
                         ghc-quickcheck
                         ghc-quickcheck-instances
                         ghc-special-values
                         ghc-spoon
                         ghc-system-filepath
                         ghc-tasty
                         ghc-tasty-expected-failure
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-tasty-silver
                         ghc-temporary
                         ghc-turtle
                         ghc-mockery
                         ghc-doctest))
    (arguments
     `(#:tests? #f ; Tries to access httpbin.org
       #:cabal-revision ("4"
                         "0innb3cn98ynb8bd83jdyrm64ij7wcvajg5qcwzdwbyzpr62anfx")))
    (home-page "http://hackage.haskell.org/package/dhall")
    (synopsis "Configuration language guaranteed to terminate")
    (description
     "Dhall is an explicitly typed configuration language that is not Turing
complete.  Despite being Turing incomplete, Dhall is a real programming
language with a type-checker and evaluator.

Use this library to parse, type-check, evaluate, and pretty-print the Dhall
configuration language.  This package also includes an executable which
type-checks a Dhall file and reduces the file to a fully evaluated normal
form.")
    (license license:bsd-3)))
