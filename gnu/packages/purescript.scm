;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Bonface Munyoki Kilyungi <bonfacemunyoki@gmail.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages purescript)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module ((gnu packages python) #:select (python))
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system haskell)
  #:use-module ((guix licenses) #:prefix license:))

(define-public purescript
  (package
    (name "purescript")
    (version "0.15.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "purescript" version))
       (sha256
        (base32 "08pashk8pm4yjsaq2g94sqa2yd3rfq9fwpxa9qccvjv6in9zybf1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "purescript")))
    (inputs (list ghc-aeson
                  ghc-aeson-better-errors
                  ghc-ansi-terminal
                  ghc-blaze-html
                  ghc-bower-json
                  ghc-boxes
                  ghc-cborg
                  ghc-serialise
                  ghc-cheapskate
                  ghc-clock
                  ghc-cryptonite
                  ghc-data-ordlist
                  ghc-dlist
                  ghc-edit-distance
                  ghc-file-embed
                  ghc-glob
                  ghc-language-javascript
                  ghc-lens
                  ghc-lifted-async
                  ghc-lifted-base
                  ghc-memory
                  ghc-monad-control
                  ghc-monad-logger
                  ghc-monoidal-containers
                  ghc-parallel
                  ghc-pattern-arrows
                  ghc-protolude
                  ghc-regex-tdfa
                  ghc-safe
                  ghc-scientific
                  ghc-semigroups
                  ghc-semialign
                  ghc-sourcemap
                  ghc-stringsearch
                  ghc-these
                  ghc-transformers-base
                  ghc-utf8-string
                  ghc-vector
                  ghc-witherable
                  ghc-ansi-wl-pprint
                  ghc-network
                  ghc-optparse-applicative
                  ghc-gitrev))
    (native-inputs (list ghc-generic-random
                         ghc-hspec
                         ghc-hunit
                         ghc-newtype
                         ghc-quickcheck
                         ghc-regex-base
                         ghc-split
                         ghc-typed-process
                         ghc-happy))
    (arguments
     (list
      ;; Tests require npm
      #:tests? #f
       #:configure-flags
       #~(list "--flags=release")
       #:haddock? #f
       #:phases
       #~(modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "purescript.cabal"
               (("\\b(language-javascript|process)\\s+[^,]+" all dep)
                dep)
               (("happy:happy ==1.20.0") "happy:happy"))))
         (add-after 'register 'remove-libraries
           (lambda _
             (delete-file-recursively
               (string-append #$output "/lib")))))))
    (home-page "https://www.purescript.org/")
    (synopsis "Haskell inspired programming language compiling to JavaScript")
    (description
     "Purescript is a small strongly, statically typed programming language with
expressive types, inspired by Haskell and compiling to JavaScript.")
    (license license:bsd-3)))

