;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2018, 2019, 2021, 2026 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2019, 2023, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017–2019, 2021, 2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Tonton <tonton@riseup.net>
;;; Copyright © 2018, 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;; Copyright © 2019 Jacob MacDonald <jaccarmac@gmail.com>
;;; Copyright © 2019,2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 JoJo <jo@jo.zone>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020 Carlo Holl <carloholl@gmail.com>
;;; Copyright © 2020 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2021–2023 Alice BRENON <alice.brenon@ens-lyon.fr>
;;; Copyright © 2021 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2023 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2026 Konstantin Suntsov <protvin@disroot.org>
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

(define-module (gnu packages haskell-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public ghc-abstract-deque
  (package
    (name "ghc-abstract-deque")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "abstract-deque" version))
       (sha256
        (base32 "18jwswjxwzc9bjiy4ds6hw2a74ki797jmfcifxd2ga4kh7ri1ah9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "abstract-deque")))
    (inputs (list ghc-random ghc-atomic-primops))
    (home-page "https://github.com/rrnewton/haskell-lockfree/wiki")
    (synopsis
     "Abstract, parameterized interface to mutable Deques for Haskell")
    (description
     "This Haskell package provides an abstract interface to
highly-parameterizable queues/deques.

Background: There exists a feature space for queues that extends between:

@itemize
@item Simple, single-ended, non-concurrent, bounded queues

@item Double-ended, thread-safe, growable queues with important points
in between (such as the queues used for work stealing).
@end itemize

This package includes an interface for Deques that allows the programmer
to use a single API for all of the above, while using the type system to
select an efficient implementation given the requirements (using type families).

This package also includes a simple reference implementation based on
@code{IORef} and @code{Data.Sequence}.")
    (license license:bsd-3)))

(define-public ghc-abstract-par
  (package
    (name "ghc-abstract-par")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "abstract-par" version))
       (sha256
        (base32 "0q6qsniw4wks2pw6wzncb1p1j3k6al5njnvm2v5n494hplwqg2i4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "abstract-par")))
    (home-page "https://github.com/simonmar/monad-par")
    (synopsis "Abstract parallelization interface for Haskell")
    (description
     "This Haskell package is an abstract interface
only.  It provides a number of type clasess, but not an
implementation.  The type classes separate different levels
of @code{Par} functionality.  See the @code{Control.Monad.Par.Class}
module for more details.")
    (license license:bsd-3)))

(define-public ghc-active
  (package
    (name "ghc-active")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "active" version))
       (sha256
        (base32 "150kwir36aj9q219qi80mlqd0vxm4941dh6x4xp58rbd5a3mhmv1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "active")))
    (inputs (list ghc-vector ghc-semigroups ghc-semigroupoids ghc-lens
                  ghc-linear))
    (native-inputs (list ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("5"
                         "0wxl3pfdz4krx7lg1rckvmjkm2hj5vlwx3kyzzfrpsfhc9zq7f1g")))
    (home-page "http://hackage.haskell.org/package/active")
    (synopsis "Abstractions for animation")
    (description
     "This package defines an @code{Active} abstraction for
time-varying values with finite start and end times.  It is used for
describing animations within the
@url{https://archives.haskell.org/projects.haskell.org/diagrams/,
diagrams framework}.")
    (license license:bsd-3)))

(define-public ghc-adjunctions
  (package
    (name "ghc-adjunctions")
    (version "4.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "adjunctions" version))
       (sha256
        (base32 "16hqxd88998dgjib8k1dy78a1waww3hd33hqqgd17y9bxqf15swb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "adjunctions")))
    (inputs (list ghc-comonad
                  ghc-contravariant
                  ghc-distributive
                  ghc-free
                  ghc-profunctors
                  ghc-tagged
                  ghc-semigroupoids
                  ghc-semigroups
                  ghc-transformers-compat
                  ghc-void))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "http://github.com/ekmett/adjunctions/")
    (synopsis "Adjunctions and representable functors")
    (description "This library provides adjunctions and representable functors
for Haskell.")
    (license license:bsd-2)))

(define-public ghc-aeson-diff
  (package
    (name "ghc-aeson-diff")
    (version "1.1.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "aeson-diff" version))
       (sha256
        (base32 "0sd13q0nj0k1sam5xfj6dcjcki18f375sa69hm6i4xc6snfhn3cb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "aeson-diff")))
    (inputs (list ghc-aeson
                  ghc-edit-distance-vector
                  ghc-scientific
                  ghc-vector
                  ghc-optparse-applicative
                  ghc-yaml))
    (native-inputs (list ghc-quickcheck ghc-glob ghc-doctest cabal-doctest))
    (arguments
     `(#:cabal-revision ("1"
                         "1028adallw7bm72948lj322bb5a99gfs0qc1j0pnm8hryp6n7ma5")))
    (home-page "https://github.com/ysangkok/aeson-diff")
    (synopsis "Extract and apply patches to JSON documents")
    (description
     "This is a small library for working with changes to JSON
documents.  It includes a library and two command-line executables in the
style of the @command{diff} and @command{patch} commands available on many
systems.")
    (license license:bsd-3)))

(define-public ghc-alex
  (package
    (name "ghc-alex")
    (version "3.5.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "alex" version))
       (sha256
        (base32 "1plasa0h85dfcga2h4yd1bqfi8smx4ghscyg299nh3lhqkr71sm9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "alex")))
    (home-page "http://www.haskell.org/alex/")
    (synopsis "Tool for generating lexical analysers in Haskell")
    (description
     "Alex is a tool for generating lexical analysers in Haskell.  It takes a
description of tokens based on regular expressions and generates a Haskell
module containing code for scanning text efficiently.  It is similar to the
tool lex or flex for C/C++.")
    (license license:bsd-3)))

(define-public ghc-alsa-core
  (package
    (name "ghc-alsa-core")
    (version "0.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "alsa-core" version))
       (sha256
        (base32 "1avh4a419h9d2zsslg6j8hm87ppgsgqafz8ll037rk2yy1g4jl7b"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "alsa-core")))
    (inputs (list ghc-extensible-exceptions alsa-lib))
    (native-inputs (list pkg-config))
    (home-page "https://www.haskell.org/haskellwiki/ALSA")
    (synopsis "Binding to the ALSA Library API (Exceptions)")
    (description "This package provides access to ALSA infrastructure, that is
needed by both alsa-seq and alsa-pcm.")
    (license license:bsd-3)))

(define-public ghc-alsa-mixer
  (package
    (name "ghc-alsa-mixer")
    (version "0.3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "alsa-mixer" version))
       (sha256
        (base32 "0bxxmsnh2cx63gb19mzwslslwxqhz5m26pd19xnhgs9yyc3jhp57"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "alsa-mixer")))
    (inputs (list ghc-alsa-core))
    (native-inputs (list ghc-c2hs))
    (home-page "https://github.com/ttuegel/alsa-mixer")
    (synopsis "Bindings to the ALSA simple mixer API")
    (description
     "This package provides bindings to the ALSA simple mixer API.")
    (license license:bsd-3)))

(define-public ghc-annotated-wl-pprint
  (package
    (name "ghc-annotated-wl-pprint")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "annotated-wl-pprint" version))
       (sha256
        (base32 "061xfz6qany3wf95csl8dcik2pz22cn8iv1qchhm16isw5zjs9hc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "annotated-wl-pprint")))
    (arguments
     `(#:cabal-revision ("1"
                         "138k24qxvl90l7dwdw1b3w36mpw93n0xi0nljblqg88pxg7jcvjx")))
    (home-page "https://github.com/david-christiansen/annotated-wl-pprint")
    (synopsis "The Wadler/Leijen Pretty Printer, with annotation support")
    (description
     "This is a modified version of wl-pprint, which was based on
Wadler's paper \"A Prettier Printer\".  This version allows the library user
to annotate the text with semantic information, which can later be rendered in
a variety of ways.")
    (license license:bsd-3)))

(define-public ghc-ansi-terminal
  (package
    (name "ghc-ansi-terminal")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ansi-terminal" version))
       (sha256
        (base32 "14n0d3a4351mlin4dvk9xjg3x7dksnpkdg5l0gbby2fpbhb1mlc8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ansi-terminal")))
    (inputs (list ghc-ansi-terminal-types ghc-colour))
    (home-page "https://github.com/UnkindPartition/ansi-terminal")
    (synopsis "ANSI terminal support for Haskell")
    (description
     "This package provides ANSI terminal support for Haskell.  It
allows cursor movement, screen clearing, color output showing or hiding the
cursor, and changing the title.")
    (license license:bsd-3)))

(define-public ghc-ansi-wl-pprint
  (package
    (name "ghc-ansi-wl-pprint")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ansi-wl-pprint" version))
       (sha256
        (base32 "0ir7mxvdxm94i09j0lf415pylczzws8f2d8n29nldrbql49ihki3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ansi-wl-pprint")))
    (inputs (list ghc-prettyprinter-compat-ansi-wl-pprint))
    (home-page "http://github.com/ekmett/ansi-wl-pprint")
    (synopsis "Wadler/Leijen Pretty Printer for colored ANSI terminal output")
    (description
     "This is a pretty printing library based on Wadler's paper
\"A Prettier Printer\".  It has been enhanced with support for ANSI terminal
colored output using the ansi-terminal package.")
    (license license:bsd-3)))

(define-public ghc-appar
  (package
    (name "ghc-appar")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "appar" version))
       (sha256
        (base32 "07v3h766q9mnhphsm53718h1lds147ix7dj15kc5hnsj4vffvkn4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "appar")))
    (home-page "http://hackage.haskell.org/package/appar")
    (synopsis "Simple applicative parser")
    (description "This package provides a simple applicative parser in Parsec
style.")
    (license license:bsd-3)))

(define-public ghc-aspell-pipe
  (package
    (name "ghc-aspell-pipe")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "aspell-pipe" version))
       (sha256
        (base32 "09dw4v4j5pmqi8pdh3p7kk7f8pph5w33s7vd21fgvhv3arnrj6p8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "aspell-pipe")))
    (inputs (list ghc-async))
    (home-page "https://hackage.haskell.org/package/aspell-pipe")
    (synopsis "Pipe-based interface to the Aspell program")
    (description
     "This package provides a pipe-based interface to the Aspell program (no
dynamic linking required).")
    (license license:bsd-3)))

(define-public ghc-assoc
  (package
    (name "ghc-assoc")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "assoc" version))
       (sha256
        (base32 "1xhg7fqs8i067q2wmpma1yynsa9vbrhjh4pmbbcmv7zhzsvlj493"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "assoc")))
    (arguments
     `(#:cabal-revision ("1"
                         "0pqq27nzpsabvklgbldqls37mcl2hzs19qy6balsqk7b3x6rpcqa")))
    (home-page "http://hackage.haskell.org/package/assoc")
    (synopsis "Swap and assoc: Symmetric and Semigroupy Bifunctors")
    (description
     "Provides generalisations of @code{swap :: (a,b) -> (b,a)} and
@code{assoc :: ((a,b),c) -> (a,(b,c))} to @code{Bifunctor}s supporting
similar operations (e.g. @code{Either}, @code{These}).")
    (license license:bsd-3)))

(define-public ghc-async
  (package
    (name "ghc-async")
    (version "2.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "async" version))
       (sha256
        (base32 "1xqnixmcxbird7rxl124bn5swpyyxxx2jxpdsbx2l8drp8z4f60q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "async")))
    (inputs (list ghc-hashable))
    (native-inputs (list ghc-test-framework ghc-test-framework-hunit ghc-hunit))
    (arguments
     `(#:cabal-revision ("3"
                         "0fvnk4rz1d2j4n5pww17qy0km0blv2gqycnbjlyrg6kjcmhqqr5p")))
    (home-page "https://github.com/simonmar/async")
    (synopsis "Library to run IO operations asynchronously")
    (description
     "Async provides a library to run IO operations
asynchronously, and wait for their results.  It is a higher-level interface
over threads in Haskell, in which @code{Async a} is a concurrent thread that
will eventually deliver a value of type @code{a}.")
    (license license:bsd-3)))

(define-public ghc-atomic-counter
  (package
    (name "ghc-atomic-counter")
    (version "0.1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "atomic-counter" version))
       (sha256
        (base32 "016gyi2nzgr0lyd3g99snjjh49i2hgs8kbrjprn4439w3cwn6jyf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "atomic-counter")))
    (native-inputs (list ghc-quickcheck ghc-async ghc-tasty
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/sergv/atomic-counter")
    (synopsis "Mutable counters that can be modified with atomic operatinos")
    (description
     "This package defines Counter type that can be safely modified concurrently from
multiple threads.  The type supports only few operations, namely read, write,
cas (compare and swap), add, subtract and a few bitwise ones like or, and xor.
Most common use case is having a shared counter that multiple threads increment.
 Another potential use case is lightweight locks.")
    (license license:asl2.0)))

(define-public ghc-atomic-primops
  (package
    (name "ghc-atomic-primops")
    (version "0.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "atomic-primops" version))
       (sha256
        (base32 "1sp7ffyybnvq9s0b73rr33wjck3ffip2v56q1l87d1r5150wwh60"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "atomic-primops")))
    (inputs (list ghc-primitive))
    (home-page "https://github.com/rrnewton/haskell-lockfree/wiki")
    (synopsis "Safe approach to CAS and other atomic ops")
    (description
     "GHC 7.4 introduced a new @code{casMutVar} PrimOp which is difficult to
use safely, because pointer equality is a highly unstable property in Haskell.
This library provides a safer method based on the concept of @code{Ticket}s.")
    (license license:bsd-3)))

(define-public ghc-atomic-write
  (package
    (name "ghc-atomic-write")
    (version "0.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "atomic-write" version))
       (sha256
        (base32 "05i9vzcb6xhbh50gyr6h1lx63c0nrg6y964c46q9jn92ph9zf2ha"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "atomic-write")))
    (inputs (list ghc-temporary ghc-unix-compat))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "https://github.com/stackbuilders/atomic-write")
    (synopsis "Atomically write to a file")
    (description
     "Atomically write to a file on POSIX-compliant systems while preserving
permissions.  @code{mv} is an atomic operation.  This makes it simple to write
to a file atomically just by using the @code{mv} operation.  However, this
will destroy the permissions on the original file.  This library preserves
permissions while atomically writing to a file.")
    (license license:expat)))

(define-public ghc-attoparsec
  (package
    (name "ghc-attoparsec")
    (version "0.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "attoparsec" version))
       (sha256
        (base32 "0v4yjz4qi8bwhbyavqxlhsfb1iv07v10gxi64khmsmi4hvjpycrz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "attoparsec")))
    (inputs (list ghc-scientific))
    (native-inputs (list ghc-quickcheck ghc-quickcheck-unicode ghc-tasty
                         ghc-tasty-quickcheck ghc-vector))
    (arguments
     `(#:cabal-revision ("6"
                         "1wrm23wl373219znwbcgpdpyw6a9ahwwhbvx387h07vln459s4im")))
    (home-page "https://github.com/haskell/attoparsec")
    (synopsis "Fast combinator parsing for bytestrings and text")
    (description
     "This library provides a fast parser combinator library,
aimed particularly at dealing efficiently with network protocols and
complicated text/binary file formats.")
    (license license:bsd-3)))

(define-public ghc-attoparsec-aeson
  (package
    (name "ghc-attoparsec-aeson")
    (version "2.2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "attoparsec-aeson" version))
       (sha256
        (base32 "1pcyiwni9kvpg97k3sm9qrxcl2n8rh698af3867zzqbgl4ijr6zy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "attoparsec-aeson")))
    (inputs (list ghc-aeson
                  ghc-attoparsec
                  ghc-character-ps
                  ghc-integer-conversion
                  ghc-primitive
                  ghc-scientific
                  ghc-vector))
    (home-page "https://github.com/haskell/aeson")
    (synopsis "Parsing of aeson's Value with attoparsec")
    (description
     "Parsing of aeson's Value with attoparsec, originally from aeson.")
    (license license:bsd-3)))

(define-public ghc-attoparsec-iso8601
  (package
    (name "ghc-attoparsec-iso8601")
    (version "1.1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "attoparsec-iso8601" version))
       (sha256
        (base32 "0yn9l9drza1wcj59a9dzm4vnihwmsxk6zd3fqg6kgww1an8x3k9l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "attoparsec-iso8601")))
    (inputs (list ghc-attoparsec ghc-integer-conversion ghc-time-compat))
    (arguments
     `(#:cabal-revision ("1"
                         "0chjsgkkdvnj6zps4gj80dwdfxmic1dal0cs5jfmrw8jalkqflzl")))
    (home-page "https://github.com/haskell/aeson")
    (synopsis "Parse ISO 8601 dates")
    (description "Haskell library for parsing of ISO 8601 dates, originally
from aeson.")
    (license license:bsd-3)))

(define-public ghc-auto-update
  (package
    (name "ghc-auto-update")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "auto-update" version))
       (sha256
        (base32 "0m0f9v3cbacgs1fmbh82r2bqbs7ya76zz0g9zy0hk8p2kmfcq9cs"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "auto-update")))
    (native-inputs (list ghc-hspec ghc-retry ghc-hunit hspec-discover))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "Efficiently run periodic, on-demand actions")
    (description "This library provides mechanisms to efficiently run
periodic, on-demand actions in Haskell.")
    (license license:expat)))

(define-public ghc-aws
  (package
    (name "ghc-aws")
    (version "0.24.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "aws" version))
       (sha256
        (base32 "0x78fydxg8qsr77mn7sy544xzl35jwc9j44vl3l868l51xkx3jwj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "aws")))
    (arguments (list #:tests? #f)) ; tests require AWS credentials and networking
    (inputs (list ghc-aeson
                  ghc-attoparsec
                  ghc-attoparsec-aeson
                  ghc-base16-bytestring
                  ghc-base64-bytestring
                  ghc-blaze-builder
                  ghc-byteable
                  ghc-case-insensitive
                  ghc-cereal
                  ghc-conduit
                  ghc-conduit-extra
                  ghc-cryptonite
                  ghc-data-default
                  ghc-http-conduit
                  ghc-http-client-tls
                  ghc-http-types
                  ghc-lifted-base
                  ghc-memory
                  ghc-monad-control
                  ghc-old-locale
                  ghc-resourcet
                  ghc-safe
                  ghc-scientific
                  ghc-tagged
                  ghc-unordered-containers
                  ghc-utf8-string
                  ghc-vector
                  ghc-xml-conduit
                  ghc-network
                  ghc-network-bsd
                  ghc-errors))
    (native-inputs (list ghc-quickcheck
                         ghc-http-client
                         ghc-quickcheck-instances
                         ghc-tasty
                         ghc-tasty-quickcheck
                         ghc-transformers-base
                         ghc-quickcheck
                         ghc-http-client
                         ghc-quickcheck-instances
                         ghc-tasty
                         ghc-tasty-quickcheck
                         ghc-transformers-base
                         ghc-quickcheck
                         ghc-http-client
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-transformers-base))
    (home-page "http://github.com/aristidb/aws")
    (synopsis "Amazon Web Services for Haskell")
    (description
     "This package attempts to provide support for using
Amazon Web Services like S3 (storage), SQS (queuing) and others to
Haskell programmers.  The ultimate goal is to support all Amazon
Web Services.")
    (license license:bsd-3)))

(define-public ghc-base16
  (package
    (name "ghc-base16")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "base16" version))
       (sha256
        (base32 "1plwc4yrkvd5j6y09fjvyzhr05mzhzwz6z41fyb60y0bj5j66dl6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "base16")))
    (inputs (list ghc-primitive ghc-text-short))
    (native-inputs (list ghc-base16-bytestring
                         ghc-quickcheck
                         ghc-random-bytestring
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-text-short))
    (arguments
     `(#:cabal-revision
        ("4"
         "0xvk439dhk1znz3zc0127rqs09p7bqbp5pal77hcvwk8h4i43lsd")))
    (home-page "https://github.com/emilypi/base16")
    (synopsis "Fast RFC 4648-compliant Base16 encoding")
    (description
     "This library provides RFC 4648-compliant Base16 encoding and decoding
primitives, as well as support for textual values.")
    (license license:bsd-3)))

(define-public ghc-base16-bytestring
  (package
    (name "ghc-base16-bytestring")
    (version "1.0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "base16-bytestring" version))
       (sha256
        (base32 "1167f9jaivnabn6kg2gc421ac9njb67fr4v0adbj3qph7qa92nhx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "base16-bytestring")))
    (native-inputs (list ghc-hunit ghc-quickcheck ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2))
    (arguments
     `(#:cabal-revision ("1"
                         "1zg2c9jwpbmwnpfw5ail1bvnhasrx8zks8rzn3q7kz69ks7yi556")))
    (home-page "http://github.com/haskell/base16-bytestring")
    (synopsis "Fast base16 (hex) encoding and decoding for ByteStrings")
    (description
     "This package provides a Haskell library for working with base16-encoded
data quickly and efficiently, using the ByteString type.")
    (license license:bsd-3)))

(define-public ghc-base64-bytestring
  (package
    (name "ghc-base64-bytestring")
    (version "1.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "base64-bytestring" version))
       (sha256
        (base32 "1ja9vkgnpkzaw8gz6sm5jmgha6wg3m1j281m0nv1w9yyxlqfvy7v"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "base64-bytestring")))
    (native-inputs (list ghc-hunit ghc-quickcheck ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2))
    (arguments
     `(#:cabal-revision ("1"
                         "00wqskhc31agyxvm7546367qb33v5i3j31sibcw6vihli77mqc25")))
    (home-page "https://github.com/haskell/base64-bytestring")
    (synopsis "Base64 encoding and decoding for ByteStrings")
    (description "This library provides fast base64 encoding and decoding for
Haskell @code{ByteString}s.")
    (license license:bsd-3)))

(define-public ghc-base-compat
  (package
    (name "ghc-base-compat")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "base-compat" version))
       (sha256
        (base32 "1s9mk80lb8rscb0rndm9pkh1nmkkd4dnl7nymb3qypk43ra5brkl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "base-compat")))
    (home-page "http://hackage.haskell.org/package/base-compat")
    (synopsis "Haskell compiler compatibility library")
    (description
     "This library provides functions available in later versions
of base to a wider range of compilers, without requiring the use of CPP
pragmas in your code.")
    (license license:expat)))

(define-public ghc-base-compat-batteries
  (package
    (name "ghc-base-compat-batteries")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "base-compat-batteries" version))
       (sha256
        (base32 "1q9873jrfld2gqkw8xfhcmw1r3hqkvi58r1lxpvwh0nd0hpz8arx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "base-compat-batteries")))
    (inputs (list ghc-base-compat))
    (native-inputs (list ghc-hspec hspec-discover ghc-quickcheck))
    (home-page "http://hackage.haskell.org/package/base-compat-batteries")
    (synopsis "Extra batteries included base-compat")
    (description
     "This library provides functions available in later
versions of @code{base} to a wider range of compilers, without requiring
you to use CPP pragmas in your code.  This package provides the same API
as the @code{base-compat} library, but depends on compatibility
packages (such as @code{semigroups}) to offer a wider support window
than @code{base-compat}, which has no dependencies.")
    (license license:expat)))

(define-public ghc-basement
  (package
    (name "ghc-basement")
    (version "0.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "basement" version))
       (sha256
        (base32 "00332i4n98gh06x8ii4p8mhjpq0ch1bdan9hxmdblxpgk8j7xdvz"))))
    (build-system haskell-build-system)
    (arguments
     (if (target-32bit?)
         (list #:phases
               #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-for-32-bit-system
                     (lambda _
                       (define patch
                         #$(local-file
                            (search-patch "ghc-basement-fix-32-bit.patch")))

                       (invoke "patch" "-p1" "--force" "-i" patch)))))
         '()))
    (properties '((upstream-name . "basement")))
    (home-page "https://github.com/haskell-foundation/foundation#readme")
    (synopsis "Basic primitives for Foundation starter pack")
    (description
     "This package contains basic primitives for the Foundation set of
packages.")
    (license license:bsd-3)))

(define-public ghc-base-orphans
  (package
    (name "ghc-base-orphans")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "base-orphans" version))
       (sha256
        (base32 "1n5bpwzgw8xg00p23prajw0bj08kxh2ri3821ib1943ir1wm18qp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "base-orphans")))
    (native-inputs (list ghc-quickcheck ghc-hspec hspec-discover))
    (home-page "https://github.com/haskell-compat/base-orphans#readme")
    (synopsis "Orphan instances for backwards compatibility")
    (description
     "This package defines orphan instances that mimic instances
available in later versions of base to a wider (older) range of compilers.")
    (license license:expat)))

(define-public ghc-base-orphans-bootstrap
  (package
    (inherit ghc-base-orphans)
    (name "ghc-base-orphans-bootstrap")
    (arguments '(#:tests? #f))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-base-prelude
  (package
    (name "ghc-base-prelude")
    (version "1.6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "base-prelude" version))
       (sha256
        (base32 "1lqxa8lhnhiyxkqcwq82a8g2sizhagy3l0z7x57xrmn9y81sy241"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "base-prelude")))
    (home-page "https://github.com/nikita-volkov/base-prelude")
    (synopsis "The most complete prelude formed solely from the Haskell's base
package")
    (description
     "This Haskell package aims to reexport all the non-conflicting
and most general definitions from the \"base\" package.

This includes APIs for applicatives, arrows, monoids, foldables, traversables,
exceptions, generics, ST, MVars and STM.

This package will never have any dependencies other than \"base\".

Versioning policy:

The versioning policy of this package deviates from PVP in the sense
that its exports in part are transitively determined by the version of \"base\".
Therefore it's recommended for the users of @code{ghc-base-prelude} to specify
the bounds of \"base\" as well.")
    (license license:expat)))

(define-public ghc-base-unicode-symbols
  (package
    (name "ghc-base-unicode-symbols")
    (version "0.2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "base-unicode-symbols" version))
       (sha256
        (base32 "0qkhp4ybmx4nbqqkrmw3hkm47bv61i2wpi20qb09wvk10g2dcr23"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "base-unicode-symbols")))
    (home-page "http://haskell.org/haskellwiki/Unicode-symbols")
    (synopsis "Unicode alternatives for common functions and operators")
    (description
     "This package defines new symbols for a number of functions,
operators and types in the base package.  All symbols are documented with
their actual definition and information regarding their Unicode code point.
They should be completely interchangeable with their definitions.  For
further Unicode goodness you can enable the @code{UnicodeSyntax}
@url{https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exs.html#unicode-syntax,
language extension}.  This extension enables Unicode characters to be used to
stand for certain ASCII character sequences, i.e. → instead of @code{->},
∀ instead of @code{forall} and many others.")
    (license license:bsd-3)))

(define-public ghc-basic-prelude
  (package
    (name "ghc-basic-prelude")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "basic-prelude" version))
       (sha256
        (base32 "0yckmnvm6i4vw0mykj4fzl4ldsf67v8d2h0vp1bakyj84n4myx8h"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "basic-prelude")))
    (inputs (list ghc-hashable ghc-unordered-containers ghc-vector))
    (home-page "https://github.com/snoyberg/basic-prelude#readme")
    (synopsis
     "Enhanced core prelude; a common foundation for alternate preludes")
    (description
     "The premise of basic-prelude is that there are a lot of very commonly
desired features missing from the standard Prelude, such as commonly used
operators (<$> and >=>, for instance) and imports for common datatypes
(e.g., ByteString and Vector).  At the same time, there are lots of other
components which are more debatable, such as providing polymorphic versions
of common functions.

So basic-prelude is intended to give a common foundation for a number of
alternate preludes.  The package provides two modules: CorePrelude provides
the common ground for other preludes to build on top of, while BasicPrelude
exports CorePrelude together with commonly used list functions to provide a
drop-in replacement for the standard Prelude.

Users wishing to have an improved Prelude can use BasicPrelude.  Developers
wishing to create a new prelude should use CorePrelude.")
    (license license:expat)))

(define-public ghc-bencode
  (package
    (name "ghc-bencode")
    (version "0.6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bencode" version))
       (sha256
        (base32 "0znv0y3b3zm5jvhlvj5f5s7y93db67j9yd59w1bnrw2pqv30gqaq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bencode")))
    (inputs (list ghc-transformers-compat))
    (native-inputs (list ghc-quickcheck ghc-hspec))
    (home-page "http://hackage.haskell.org/package/bencode")
    (synopsis "Parsers and printers for bencoded data")
    (description
     "This library provides parsers and printers for bencoded data.  Bencode
is the encoding used by the peer-to-peer file sharing system BitTorrent for
storing and transmitting loosely structured data.")
    (license license:bsd-3)))

(define-public ghc-bifunctors
  (package
    (name "ghc-bifunctors")
    (version "5.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bifunctors" version))
       (sha256
        (base32 "0yk9v71xpwnxd6xia1bdr8pxbvrx4am6bjykqp1d1vk1a0lak1hh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bifunctors")))
    (inputs (list ghc-assoc ghc-comonad ghc-th-abstraction ghc-tagged))
    (native-inputs (list ghc-hspec ghc-quickcheck ghc-transformers-compat
                         hspec-discover))
    (arguments
     `(#:cabal-revision ("2"
                         "1ll0i0pjrmswpps64l1h6k83j7z2f4rxvr7r6iwb1axa9qf64nhi")))
    (home-page "http://github.com/ekmett/bifunctors/")
    (synopsis "Bifunctors for Haskell")
    (description "This package provides bifunctors for Haskell.")
    (license license:bsd-3)))

(define-public ghc-bimap
  (package
    (name "ghc-bimap")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bimap" version))
       (sha256
        (base32 "158cdwk9jwklcfgbn62dqq255i40w13ifggsdps87sxc5q7lpd5h"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bimap")))
    (native-inputs (list ghc-quickcheck))
    (home-page "https://github.com/joelwilliamson/bimap")
    (synopsis "Bidirectional mapping between two key types")
    (description
     "This package provides a data structure representing a bidirectional
mapping between two key types.  Each value in the bimap is associated with
exactly one value of the opposite type.")
    (license license:bsd-3)))

(define-public ghc-bindings-dsl
  (package
    (name "ghc-bindings-dsl")
    (version "1.0.25")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bindings-DSL" version))
       (sha256
        (base32 "0kqrd78nspl3lk4a0fqn47d8dirjg3b24dkvkigcrlb81hw35pk3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bindings-DSL")))
    (home-page "https://github.com/jwiegley/bindings-dsl/wiki")
    (synopsis "FFI domain specific language, on top of hsc2hs")
    (description
     "This is a set of macros to be used when writing Haskell FFI.  They were
designed to be able to fully describe C interfaces, so that @code{hsc2hs} can
extract from them all Haskell code needed to mimic such interfaces.  All
Haskell names used are automatically derived from C names, structures are
mapped to Haskell instances of @code{Storable}, and there are also macros you
can use with C code to help write bindings to inline functions or macro
functions.")
    (license license:bsd-3)))

(define-public ghc-bitarray
  (package
    (name "ghc-bitarray")
    (version "0.0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bitarray" version))
       (sha256
        (base32 "00nqd62cbh42qqqvcl6iv1i9kbv0f0mkiygv4j70wfh5cl86yzxj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bitarray")))
    (arguments
     `(#:cabal-revision ("1"
                         "10fk92v9afjqk43zi621jxl0n8kci0xjj32lz3vqa9xbh67zjz45")))
    (home-page "http://code.haskell.org/~bkomuves/")
    (synopsis "Mutable and immutable bit arrays")
    (description "The package provides mutable and immutable bit arrays.")
    (license license:bsd-3)))

(define-public ghc-bitvec
  (package
    (name "ghc-bitvec")
    (version "1.1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bitvec" version))
       (sha256
        (base32 "1ifyz0lsmgqz8yjyx4887m1wnm7ar389k6gkvcnk9mg1bgp7rll3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bitvec")))
    (inputs (list ghc-primitive ghc-vector))
    (native-inputs (list ghc-quickcheck-classes-base ghc-quickcheck-classes
                         ghc-tasty ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("3"
                         "1cw8gz65n5m20sy9wrxrg2kz2lskqcw81ib952jmha72c3ffcjs3")))
    (home-page "https://github.com/Bodigrim/bitvec")
    (synopsis "Space-efficient bit vectors")
    (description
     "This package provides a newtype over Bool with a Vector instance that
requires only 1/8 of the normal memory and is in some cases much faster.")
    (license license:bsd-3)))

(define-public ghc-blaze-builder
  (package
    (name "ghc-blaze-builder")
    (version "0.4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "blaze-builder" version))
       (sha256
        (base32 "04six8rkyrhfa96ykmp51kfcxxj96v1g5r2m9sdaz5xc5023c7cp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "blaze-builder")))
    (native-inputs (list ghc-hunit
                         ghc-quickcheck
                         ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2
                         ghc-utf8-string))
    (home-page "https://github.com/blaze-builder/blaze-builder")
    (synopsis "Efficient buffered output")
    (description
     "This library provides an implementation of the older
@code{blaze-builder} interface in terms of the new builder that shipped with
@code{bytestring-0.10.4.0}.  This implementation is mostly intended as a
bridge to the new builder, so that code that uses the old interface can
interoperate with code that uses the new implementation.")
    (license license:bsd-3)))

(define-public ghc-blaze-markup
  (package
    (name "ghc-blaze-markup")
    (version "0.8.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "blaze-markup" version))
       (sha256
        (base32 "1s1hb477smr0m8rvpp7vr768hvwv5rsv4w07phdqyzqz9a5sq1l6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "blaze-markup")))
    (inputs (list ghc-blaze-builder))
    (native-inputs (list ghc-hunit ghc-quickcheck ghc-tasty ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "1r6pkaip7bgv6i4f3klxbqa2vaas9rn4agdr6c57r2njh65j2zq2")))
    (home-page "http://jaspervdj.be/blaze")
    (synopsis "Fast markup combinator library for Haskell")
    (description "This library provides core modules of a markup combinator
library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-bloomfilter
  (package
    (name "ghc-bloomfilter")
    (version "2.0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bloomfilter" version))
       (sha256
        (base32 "0klb26ldkw32axv3927w489j71r2rc9pangsvznqjbljib9970hp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bloomfilter")))
    (native-inputs (list ghc-quickcheck ghc-random ghc-test-framework
                         ghc-test-framework-quickcheck2))
    (home-page "https://github.com/haskell-pkg-janitors/bloomfilter")
    (synopsis "Pure and impure Bloom filter implementations")
    (description
     "This package provides both mutable and immutable Bloom
filter data types, along with a family of hash functions and an easy-to-use
interface.")
    (license license:bsd-3)))

(define-public ghc-boxes
  (package
    (name "ghc-boxes")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "boxes" version))
       (sha256
        (base32 "1hsnmw95i58d4bkpxby3ddsj1cawypw4mdyb18m393s5i8p7iq9q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "boxes")))
    (inputs (list ghc-split))
    (native-inputs (list ghc-quickcheck))
    (home-page "http://hackage.haskell.org/package/boxes")
    (synopsis "2D text pretty-printing library")
    (description
     "Boxes is a pretty-printing library for laying out text in two dimensions,
using a simple box model.")
    (license license:bsd-3)))

(define-public ghc-byteable
  (package
    (name "ghc-byteable")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "byteable" version))
       (sha256
        (base32 "1qizg0kxxjqnd3cbrjhhidk5pbbciz0pb3z5kzikjjxnnnhk8fr4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "byteable")))
    (home-page "http://github.com/vincenthz/hs-byteable")
    (synopsis "Type class for sequence of bytes")
    (description
     "This package provides an abstract class to manipulate sequence of bytes.
The use case of this class is abstracting manipulation of types that are just
wrapping a bytestring with stronger and more meaniful name.")
    (license license:bsd-3)))

(define-public ghc-byteorder
  (package
    (name "ghc-byteorder")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "byteorder" version))
       (sha256
        (base32 "06995paxbxk8lldvarqpb3ygcjbg4v8dk4scib1rjzwlhssvn85x"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "byteorder")))
    (home-page "http://community.haskell.org/~aslatter/code/byteorder")
    (synopsis "Exposes the native endianness of the system")
    (description
     "This package is for working with the native byte-ordering of the
system.")
    (license license:bsd-3)))

(define-public ghc-bytes
  (package
    (name "ghc-bytes")
    (version "0.17.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bytes" version))
       (sha256
        (base32 "14x3wnjg7ik1kh3vy9ahfal0hl86v8z3kj1h152364mf94j9macc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bytes")))
    (inputs (list ghc-binary-orphans
                  ghc-cereal
                  ghc-hashable
                  ghc-transformers-compat
                  ghc-unordered-containers
                  ghc-scientific
                  ghc-void))
    (arguments
     `(#:cabal-revision ("1"
                         "0jhzp9ihka03fsgnjhhj3864p3zq500xqm2whjyvin3580wigc97")))
    (home-page "https://github.com/ekmett/bytes")
    (synopsis "Serialization between @code{binary} and @code{cereal}")
    (description
     "This package provides a simple compatibility shim that lets
you work with both @code{binary} and @code{cereal} with one chunk of
serialization code.")
    (license license:bsd-3)))

(define-public ghc-bytestring-builder
  (package
    (name "ghc-bytestring-builder")
    (version "0.10.8.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bytestring-builder" version))
       (sha256
        (base32 "0grcrgwwwcvwrs9az7l4d3kf0lsqfa9qpmjzf6iyanvwn9nyzyi7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bytestring-builder")))
    (home-page "http://hackage.haskell.org/package/bytestring-builder")
    (synopsis "The new bytestring builder, packaged outside of GHC")
    (description
     "This package provides the bytestring builder that is
debuting in bytestring-0.10.4.0, which should be shipping with GHC 7.8.
Compatibility package for older packages.")
    (license license:bsd-3)))

(define-public ghc-bytestring-lexing
  (package
    (name "ghc-bytestring-lexing")
    (version "0.5.0.15")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bytestring-lexing" version))
       (sha256
        (base32 "0m0375fjr20071s0sx596b45bw962q17i5m4icriq1jv4swx9f3j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bytestring-lexing")))
    (native-inputs (list ghc-tasty ghc-tasty-smallcheck ghc-tasty-quickcheck))
    (home-page "https://wrengr.org/software/hackage.html")
    (synopsis "Parse and produce literals from strict or lazy bytestrings")
    (description
     "This package provides tools to parse and produce literals efficiently
from strict or lazy bytestrings.")
    (license license:bsd-3)))

(define-public ghc-bzlib-conduit
  (package
    (name "ghc-bzlib-conduit")
    (version "0.3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bzlib-conduit" version))
       (sha256
        (base32 "0c7nhc3a93x648yf8pq5js81zr97fnfiahg2hjbn999nbwg89800"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bzlib-conduit")))
    (inputs (list ghc-bindings-dsl ghc-conduit ghc-data-default-class
                  ghc-resourcet))
    (native-inputs (list ghc-hspec ghc-random))
    (home-page "https://github.com/snoyberg/bzlib-conduit#readme")
    (synopsis "Streaming compression/decompression via conduits")
    (description
     "This package provides Haskell bindings to bzlib and Conduit support for
streaming compression and decompression.")
    (license license:bsd-3)))

(define-public ghc-c2hs
  (package
    (name "ghc-c2hs")
    (version "0.28.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "c2hs" version))
       (sha256
        (base32 "0k482wv94jbpwd96a2c2lc7qz9k8072slx7l7943472nzk7k41ir"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "c2hs")))
    (inputs (list ghc-language-c ghc-dlist))
    (native-inputs (list ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-hunit
                         ghc-shelly
                         ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-hunit
                         ghc-shelly))
    (arguments
     `(#:cabal-revision
       ("3" "1f1jxkj89bms4hsqwl47hldixfdx96l66cs8zjlf8i6j7c9qiz3s")
       #:phases
       (modify-phases %standard-phases
         ;; The tarball on Hackage does not ship these tests. See
         ;; https://github.com/haskell/c2hs/issues/269
         (add-after 'unpack 'disable-tests
           (lambda _
             (substitute* "tests/test-bugs.hs"
               ((", testCase \"Issue #242\" issue242") ""))
             (substitute* "tests/test-system.hs"
               ((", testCase \"Interruptible\" test_interruptible") ""))))
         (add-before 'check 'set-cc
           ;; add a cc executable in the path, needed for some tests to pass
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc"))
                   (tmpbin (tmpnam))
                   (curpath (getenv "PATH")))
               (mkdir-p tmpbin)
               (symlink (which "gcc") (string-append tmpbin "/cc"))
               (setenv "PATH" (string-append tmpbin ":" curpath)))
             #t))
         (add-after 'check 'remove-cc
           ;; clean the tmp dir made in 'set-cc
           (lambda _
             (let* ((cc-path (which "cc"))
                    (cc-dir (dirname cc-path)))
               (delete-file-recursively cc-dir)
               #t))))))
    (home-page "https://github.com/haskell/c2hs")
    (synopsis "Create Haskell bindings to C libraries")
    (description
     "C->Haskell assists in the development of Haskell bindings to
C libraries.  It extracts interface information from C header files and
generates Haskell code with foreign imports and marshaling.  Unlike writing
foreign imports by hand (or using hsc2hs), this ensures that C functions are
imported with the correct Haskell types.")
    (license license:gpl2)))

(define-public ghc-cairo
  (package
    (name "ghc-cairo")
    (version "0.13.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cairo" version))
       (sha256
        (base32 "1d3ba8gz72f8dz3m5c2i6y85b3vf7405m16al7ms7k9qjy1wcpby"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cairo")))
    (inputs (list ghc-utf8-string cairo))
    (native-inputs (list ghc-gtk2hs-buildtools pkg-config))
    (home-page "https://projects.haskell.org/gtk2hs/")
    (synopsis "Haskell bindings to the Cairo vector graphics library")
    (description
     "Cairo is a library to render high quality vector graphics.  There exist
various backends that allows rendering to Gtk windows, PDF, PS, PNG and SVG
documents, amongst others.")
    (license license:bsd-3)))

(define-public ghc-call-stack
  (package
    (name "ghc-call-stack")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "call-stack" version))
       (sha256
        (base32 "0yxq6v37kcmgv6rrna4g1ipr8mhkgf00ng2p359ybxq46j5cy2s3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "call-stack")))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           ;; Tests fail because they assume that they are not in any package,
           ;; but when running them with Cabal theu are in a package.
           ;; This is fixed by running the tests by hand, bypassing Cabal.
           (replace 'check
             (lambda _
               (invoke "runhaskell"
                       (string-append "-package-db=" (or (getenv "TMP") "/tmp")
                                      "/package.conf.d")
                       "-itest:src" "-package=nanospec" "test/Spec.hs"))))))
    (native-inputs (list ghc-nanospec-bootstrap))
    (home-page "https://github.com/sol/call-stack#readme")
    (synopsis "Use GHC call-stacks in a backward compatible way")
    (description "This package provides a compatibility layer for using GHC
call stacks with different versions of the compiler.")
    (license license:expat)))

;; This is used as an input to ghc-hunit.  We cannot use ghc-call-stack there,
;; because it depends on ghc-nanospec, which depends on ghc-hunit.
(define-public ghc-call-stack-boot
  (hidden-package
   (package
     (inherit ghc-call-stack)
     (arguments '(#:tests? #f))
     (native-inputs '()))))

(define-public ghc-case-insensitive
  (package
    (name "ghc-case-insensitive")
    (version "1.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "case-insensitive" version))
       (sha256
        (base32 "01p40hfjyldfds5jg6vlvvn3ihs4ki63xn6fh8yzngaz1izc2v99"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "case-insensitive")))
    (inputs (list ghc-hashable))
    (native-inputs (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (home-page "https://github.com/basvandijk/case-insensitive")
    (synopsis "Case insensitive string comparison")
    (description
     "The module @code{Data.CaseInsensitive} provides the @code{CI} type
constructor which can be parameterised by a string-like type like:
@code{String}, @code{ByteString}, @code{Text}, etc.  Comparisons of values of
the resulting type will be insensitive to cases.")
    (license license:bsd-3)))

(define-public ghc-cassava
  (package
    (name "ghc-cassava")
    (version "0.5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cassava" version))
       (sha256
        (base32 "0ps9b8lgc1ah186rlxsy8hdnwkfh60i351105309jykk63skc1nl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cassava")))
    (inputs (list ghc-attoparsec
                  ghc-hashable
                  ghc-scientific
                  ghc-text-short
                  ghc-unordered-containers
                  ghc-vector
                  ghc-only))
    (native-inputs (list ghc-hunit
                         ghc-quickcheck
                         ghc-quickcheck-instances
                         ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2))
    (home-page "https://github.com/haskell-hvr/cassava")
    (synopsis "CSV parsing and encoding library")
    (description
     "@code{cassava} is a library for parsing and encoding
@url{https://tools.ietf.org/html/rfc4180, RFC 4180} compliant @url{https://
en.wikipedia.org/wiki/Comma-separated_values, comma-separated values (CSV)}
data, which is a textual line-oriented format commonly used for exchanging
tabular data.

@code{cassava}'s API includes support for:

@itemize @bullet

@item
Index-based record-conversion
@item
Name-based record-conversion
@item
Typeclass directed conversion of fields and records
@item
Built-in field-conversion instances for standard types
@item
Customizable record-conversion instance derivation via GHC generics
@item
Low-level @url{https://hackage.haskell.org/package/bytestring), bytestring}
builders (see @url{https://hackage.haskell.org/package/cassava-0.5.2.0/docs/
Data-Csv-Builder.html, Data.Csv.Builder})
@item
Incremental decoding and encoding API (see @url{https://hackage.haskell.org/
package/cassava-0.5.2.0/docs/Data-Csv-Incremental.html, Data.Csv.Incremental})
@item
Streaming API for constant-space decoding (see @url{https://hackage.haskell.org/
package/cassava-0.5.2.0/docs/Data-Csv-Streaming.html, Data.Csv.Streaming})
@end itemize

Moreover, this library is designed to be easy to use; for instance, here's a
very simple example of encoding CSV data:

@verbatim
>>> Data.Csv.encode [(\"John\",27),(\"Jane\",28)]
\"John,27\\r\\nJane,28\\r\\n\"
@end verbatim
")
    (license license:bsd-3)))

(define-public ghc-cassava-megaparsec
  (package
    (name "ghc-cassava-megaparsec")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cassava-megaparsec" version))
       (sha256
        (base32 "11p7cdxmb3s21g3cmzs8gj5ydfml4yzm55xzq92v2pb76wrrcca6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cassava-megaparsec")))
    (inputs (list ghc-cassava ghc-megaparsec ghc-unordered-containers
                  ghc-vector))
    (native-inputs (list ghc-hspec ghc-hspec-megaparsec))
    (home-page "https://github.com/stackbuilders/cassava-megaparsec")
    (synopsis "Megaparsec parser for CSV files that plays nicely with Cassava")
    (description
     "Alternative parser for the Cassava package written with Megaparsec that
provides for better error messages at the expense of some speed.")
    (license license:expat)))

(define-public ghc-cborg
  (package
    (name "ghc-cborg")
    (version "0.2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cborg" version))
       (sha256
        (base32 "15y7p5rsv76fpklh4rgrxlxxaivpbchxdfdw96mqqjgw7060gzhp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cborg")))
    (inputs (list ghc-half ghc-primitive))
    (native-inputs (list ghc-base-orphans
                         ghc-aeson
                         ghc-base64-bytestring
                         ghc-base16-bytestring
                         ghc-quickcheck
                         ghc-random
                         ghc-scientific
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-vector))
    (arguments
     `(#:cabal-revision ("3"
                         "1ahqlq51kjc8cf5sybbmrh4rf6vsbkcd67rhxhrr9rc5w6nl9h27")))
    (home-page "http://hackage.haskell.org/package/cborg")
    (synopsis "Concise Binary Object Representation")
    (description
     "This package (formerly binary-serialise-cbor) provides an
efficient implementation of the Concise Binary Object
Representation (CBOR), as specified by RFC 7049 at
https://tools.ietf.org/html/rfc7049.

If you are looking for a library for serialisation of Haskell values, have a
look at the @url{https://hackage.haskell.org/package/serialise} package, which
is built upon this library.

An implementation of the standard bijection between CBOR and JSON is provided
by the @url{https://hackage.haskell.org/package/cborg-json} package.

Also see @code{https://hackage.haskell.org/package/cbor-tool} for a convenient
command-line utility for working with CBOR data.")
    (license license:bsd-3)))

(define-public ghc-cborg-json
  (package
    (name "ghc-cborg-json")
    (version "0.2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cborg-json" version))
       (sha256
        (base32 "1p6xdimwypmlsc0zdyw1vyyapnhwn2g8b9n0a83ca6h4r90722yv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cborg-json")))
    (inputs (list ghc-aeson
                  ghc-aeson-pretty
                  ghc-base64-bytestring
                  ghc-unordered-containers
                  ghc-scientific
                  ghc-vector
                  ghc-cborg))
    (native-inputs (list ghc-base-orphans ghc-base16-bytestring ghc-quickcheck
                         ghc-tasty ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("4"
                         "06pjqx8v7j8f6rvkf84vahva8y02lykaymnjdrjqrc5rgy01c6m0")))
    (home-page "https://github.com/well-typed/cborg")
    (synopsis "Library for encoding JSON as CBOR")
    (description "This package implements the bijection between JSON and CBOR
defined in the CBOR specification, RFC 7049.")
    (license license:bsd-3)))

(define-public ghc-cereal
  (package
    (name "ghc-cereal")
    (version "0.5.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cereal" version))
       (sha256
        (base32 "0shg3q933cvf18j1gmxill48d4sl4mvxj2qkj6yya9hvcqh5544r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cereal")))
    (native-inputs (list ghc-quickcheck ghc-test-framework
                         ghc-test-framework-quickcheck2))
    (home-page "https://github.com/GaloisInc/cereal")
    (synopsis "Binary serialization library")
    (description
     "This package provides a binary serialization library,
similar to @code{binary}, that introduces an @code{isolate} primitive for
parser isolation, and labeled blocks for better error messages.")
    (license license:bsd-3)))

(define-public ghc-cereal-conduit
  (package
    (name "ghc-cereal-conduit")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cereal-conduit" version))
       (sha256
        (base32 "1srr7agvgfw78q5s1npjq5sgynvhjgllpihiv37ylkwqm4c4ap6r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cereal-conduit")))
    (inputs (list ghc-conduit ghc-resourcet ghc-cereal))
    (native-inputs (list ghc-hunit))
    (arguments
     `(#:cabal-revision ("2"
                         "1w26az4pj699qbpa9pz55g4svkbs92ls5k6wkzjn40ghisnjk2q6")))
    (home-page "https://github.com/snoyberg/conduit")
    (synopsis
     "Turn Data.Serialize Gets and Puts into Sources, Sinks, and Conduits")
    (description
     "This package turn @code{Data.Serialize} @code{Gets} and @code{Puts} into
@code{Sources}, @code{Sinks}, and @code{Conduits}.")
    (license license:bsd-3)))

;; XXX: bytestring <0.11, time >=1.5 && <1.10
(define-public ghc-cgi
  (package
    (name "ghc-cgi")
    (version "3001.5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cgi" version))
       (sha256
        (base32 "0n7a1vfja26340xfg4ni5zrcwqcc1wmpsxmyksyhri99kb6g9rm0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cgi")))
    (inputs (list ghc-multipart ghc-network-uri))
    (home-page "https://github.com/cheecheeo/haskell-cgi")
    (synopsis "Library for writing CGI programs")
    (description "This is a Haskell library for writing CGI programs.")
    (license license:bsd-3)))

(define-public ghc-character-ps
  (package
    (name "ghc-character-ps")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "character-ps" version))
       (sha256
        (base32 "172jq74b9cs7yyyng1h2yjs1ypg7w6sk5a38j4z6s8wbwgyp3pi2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "character-ps")))
    (home-page "https://github.com/phadej/character-ps")
    (synopsis "Pattern synonyms for ASCII characters for Word8, Word16 etc")
    (description
     "Pattern synonyms for ASCII characters, e.g. . @@ pattern SPACE :: Word8 pattern
SPACE = 0x20 @@.")
    (license license:bsd-3)))

(define-public ghc-charset
  (package
    (name "ghc-charset")
    (version "0.3.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "charset" version))
       (sha256
        (base32 "0bbjrbgqdxiz47c0g5am6xh4lk3llbyiiqcz162md7df8kacds0w"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "charset")))
    (inputs (list ghc-unordered-containers))
    (home-page "http://github.com/ekmett/charset")
    (synopsis "Fast unicode character sets for Haskell")
    (description "This package provides fast unicode character sets for
Haskell, based on complemented PATRICIA tries.")
    (license license:bsd-3)))

(define-public ghc-chart
  (package
    (name "ghc-chart")
    (version "1.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Chart" version))
       (sha256
        (base32 "0nyzdag9p56vknrphdnqjsf19fmw9abs81avdm2vjgh9cnw2y7hc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Chart")))
    (inputs (list ghc-old-locale
                  ghc-lens
                  ghc-colour
                  ghc-data-default-class
                  ghc-operational
                  ghc-vector))
    (arguments
     `(#:cabal-revision ("2"
                         "1a9z8an5yhsqbawzahmg77g9l6jvavhxbk2v48k4j8fyr7sy544q")))
    (home-page "https://github.com/timbod7/haskell-chart/wiki")
    (synopsis "Library for generating 2D charts and plots")
    (description
     "This package provides a library for generating 2D charts and plots, with
backends provided by the @code{Cairo} and @code{Diagrams} libraries.")
    (license license:bsd-3)))

(define-public ghc-chart-cairo
  (package
    (name "ghc-chart-cairo")
    (version "1.9.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Chart-cairo" version))
       (sha256
        (base32 "0x10l9y38bscx88n849k9ybn7axp4j9hlivc1jv9wwvv4gqw5jr7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Chart-cairo")))
    (inputs (list ghc-old-locale
                  ghc-cairo
                  ghc-colour
                  ghc-data-default-class
                  ghc-operational
                  ghc-lens
                  ghc-chart))
    (arguments
     `(#:cabal-revision ("3"
                         "0rl4var9s1521n6ryxp59kglfip2bw1svm3mx72ya8zc4yj3z5rb")))
    (home-page "https://github.com/timbod7/haskell-chart/wiki")
    (synopsis "Cairo backend for Charts")
    (description "This package provides a Cairo vector graphics rendering
backend for the Charts library.")
    (license license:bsd-3)))

(define-public ghc-chasingbottoms
  (package
    (name "ghc-chasingbottoms")
    (version "1.3.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ChasingBottoms" version))
       (sha256
        (base32 "08zg018arf4qvp970dcnf0nyaqp7wkp5ba2dhck3v4l49k5cax9m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ChasingBottoms")))
    (inputs (list ghc-quickcheck ghc-random ghc-syb))
    (home-page "http://hackage.haskell.org/package/ChasingBottoms")
    (synopsis "Testing of partial and infinite values in Haskell")
    (description
     "This is a library for testing code involving bottoms or infinite values.
For the underlying theory and a larger example involving use of QuickCheck,
see the article
@uref{http://www.cse.chalmers.se/~nad/publications/danielsson-jansson-mpc2004.html,
\"Chasing Bottoms A Case Study in Program Verification in the Presence of
Partial and Infinite Values\"}.")
    (license license:expat)))

(define-public ghc-cheapskate
  (package
    (name "ghc-cheapskate")
    (version "0.1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cheapskate" version))
       (sha256
        (base32
         "17n6laihqrjn62l8qw4565nf77zkvrl68bjmc3vzr4ckqfblhdzd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cheapskate")))
    (inputs
     (list ghc-blaze-html ghc-xss-sanitize ghc-data-default ghc-syb
           ghc-uniplate))
    (home-page "https://github.com/jgm/cheapskate")
    (synopsis "Experimental markdown processor")
    (description "Cheapskate is an experimental Markdown processor in pure
Haskell.  It aims to process Markdown efficiently and in the most forgiving
possible way.  It is designed to deal with any input, including garbage, with
linear performance.  Output is sanitized by default for protection against
cross-site scripting (@dfn{XSS}) attacks.")
    (license license:bsd-3)))

(define-public ghc-checkers
  (package
    (name "ghc-checkers")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "checkers" version))
       (sha256
        (base32 "1r4rsa4k0fy8xig3m530ryflry9viv9v47g4gh7h0ld27rbd6z60"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "checkers")))
    (inputs (list ghc-random ghc-quickcheck ghc-semigroupoids))
    (arguments
     `(#:cabal-revision ("1"
                         "0wkvf57zd7i87z18vj285whjpcl9pscpwxz2cp7v7w6kk0769p0i")))
    (home-page "https://github.com/haskell-checkers/checkers")
    (synopsis "Check properties on standard classes and data structures")
    (description
     "Checkers wraps up the expected properties associated with various
standard type classes as @code{QuickCheck} properties.  It also provides some
morphism properties, arbitrary instances, and generator combinators for common
data types.")
    (license license:bsd-3)))

(define-public ghc-chell
  (package
    (name "ghc-chell")
    (version "0.5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "chell" version))
       (sha256
        (base32 "1iy1x5pn5y08zsl5f79vfxjm0asi2vy9hrags7jj9s8fh1dh7fxv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "chell")))
    (inputs (list ghc-options ghc-patience ghc-random ghc-ansi-terminal))
    (arguments
     `(#:cabal-revision ("4"
                         "13l0zrpighm50gcsjzj3x29f6m8i9av7c2q12cflqlkv74kl4y03")))
    (home-page "https://github.com/typeclasses/chell")
    (synopsis "Simple and intuitive library for automated testing")
    (description
     "Chell is a simple and intuitive library for automated testing.
It natively supports assertion-based testing, and can use companion
libraries such as @code{chell-quickcheck} to support more complex
testing strategies.")
    (license license:expat)))

(define-public ghc-chell-quickcheck
  (package
    (name "ghc-chell-quickcheck")
    (version "0.2.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "chell-quickcheck" version))
       (sha256
        (base32
         "046cs6f65s9nrsac6782gw4n61dpgjgz7iv7p8ag6civywj32m4i"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "chell-quickcheck")))
    (arguments
     (list
      #:cabal-revision '("2" "05b1w5pc2d1ks3vnj4x2hvamk5gxvk3n8fj9kp963gbrn8n1bk6a")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-cabal-file 'fix-build-with-quickcheck-2.15
            (lambda _
              (let ((patch #$(local-file
                              (search-patch
                               "ghc-chell-quickcheck-enable-qc-2.15.patch"))))
                (invoke "patch" "--force" "--no-backup-if-mismatch" "-p1"
                        "--input" patch)))))))
    (inputs
     (list ghc-chell ghc-random ghc-quickcheck))
    (home-page "https://john-millikin.com/software/chell/")
    (synopsis "QuickCheck support for the Chell testing library")
    (description "More complex tests for @code{chell}.")
    (license license:expat)))

(define-public ghc-chunked-data
  (package
    (name "ghc-chunked-data")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "chunked-data" version))
       (sha256
        (base32 "16m7y7fwrirbjbqqcsfmr4yxa9qvfax6r7pw0zl9ky71ms0wa47p"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "chunked-data")))
    (inputs (list ghc-semigroups ghc-vector))
    (home-page "https://github.com/snoyberg/mono-traversable#readme")
    (synopsis "Typeclasses for dealing with various chunked data
representations for Haskell")
    (description "This Haskell package was originally present in
classy-prelude.")
    (license license:expat)))

(define-public ghc-clock
  (package
    (name "ghc-clock")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "clock" version))
       (sha256
        (base32 "0bnzcx3qmcyvaywzgah9z9cqwbiwib8xbynm9hrmx2kqzs58ksba"))
       (patches (search-patches "ghc-clock-realfrag.patch"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "clock")))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck))
    (home-page "https://github.com/corsis/clock")
    (synopsis "High-resolution clock for Haskell")
    (description
     "A package for convenient access to high-resolution clock and
timer functions of different operating systems via a unified API.")
    (license license:bsd-3)))

;; This package builds `clock` without tests, since the tests rely on tasty
;; and tasty-quickcheck, which in turn require clock to build.
(define-public ghc-clock-bootstrap
  (package
    (inherit ghc-clock)
    (name "ghc-clock-bootstrap")
    (arguments '(#:tests? #f))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-cmark
  (package
    (name "ghc-cmark")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cmark" version))
       (sha256
        (base32 "0ajwb2azv57q4240f76h9xqivkfi16vhi4g2sr4nasr4rmkns789"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cmark")))
    (native-inputs (list ghc-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "11c07f13fwn2cdcrary8bs1ym2hjqkm58l9pcdq8avi8dpayb52r")))
    (home-page "https://github.com/jgm/cmark-hs")
    (synopsis "Fast, accurate CommonMark (Markdown) parser and renderer")
    (description
     "This package provides Haskell bindings for
@uref{https://github.com/jgm/cmark, libcmark}, the reference parser for
CommonMark, a fully specified variant of Markdown.  It includes bundled libcmark
sources, and does not require prior installation of the C library.")
    (license license:bsd-3)))

(define-public ghc-cmark-gfm
  (package
    (name "ghc-cmark-gfm")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cmark-gfm" version))
       (sha256
        (base32 "0sd8q42j51ba7ymyxk5360mhvhbnirsd371d3sggl6sbslxzp34m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cmark-gfm")))
    (native-inputs (list ghc-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "1385k2xvxy9ssw69g606vwnpm07sf919prciwvnaipsxiwwbjwl6")))
    (home-page "https://github.com/kivikakk/cmark-gfm-hs")
    (synopsis "Fast, accurate GitHub Flavored Markdown parser and renderer")
    (description
     "This package provides Haskell bindings for libcmark-gfm, the reference
parser for GitHub Flavored Markdown, a fully specified variant of Markdown.
It includes sources for libcmark-gfm and does not require prior installation
of the C library.")
    (license license:bsd-3)))

(define-public ghc-cmdargs
  (package
    (name "ghc-cmdargs")
    (version "0.10.22")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cmdargs" version))
       (sha256
        (base32 "1vwvdszal6clkvsqd1amcap8wy3cp19x8qmhgc1i7kwmhxzjxcdq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cmdargs")))
    (inputs (list ghc-semigroups))
    (home-page "https://github.com/ndmitchell/cmdargs#readme")
    (synopsis "Command line argument processing")
    (description
     "This library provides an easy way to define command line parsers.")
    (license license:bsd-3)))

(define-public ghc-code-page
  (package
    (name "ghc-code-page")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "code-page" version))
       (sha256
        (base32 "1aiavczjk6f2kc1cdwjc1mwkr4d9shiz3xwmfbzsdn0yqqchxydj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "code-page")))
    (home-page "https://github.com/RyanGlScott/code-page")
    (synopsis "Windows code page library for Haskell")
    (description
     "A cross-platform library with functions for adjusting
code pages on Windows.  On all other operating systems, the library does
nothing.")
    (license license:bsd-3)))

(define-public ghc-choice
  (package
    (name "ghc-choice")
    (version "0.2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "choice" version))
       (sha256
        (base32 "099c5ibal8llzr5nh62dnpb1pwj30gxqwdcjx0rds51ga2j32z5i"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "choice")))
    (home-page "https://github.com/mboes/choice")
    (synopsis "Provides a type to mitigate Boolean blindness")
    (description
      "Package providing a @code{Choice} type of labeled booleans to mitigate
the Boolean blindness problem, which refers to the problem that boolean literals
on their own aren't very informative (e.g., what does @code{True} mean in a
given context?).")
    (license license:bsd-3)))

(define-public ghc-colour
  (package
    (name "ghc-colour")
    (version "2.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "colour" version))
       (sha256
        (base32 "0wgqj64mh2y2zk77kv59k3xb3dk4wmgfp988y74sp9a4d76mvlrc"))))
    (build-system haskell-build-system)
    (arguments
     ;; The tests for this package have the following dependency cycle:
     ;; ghc-test-framework -> ghc-ansi-terminal -> ghc-colour. Additionally,
     ;; the tests are not buildable with dependency versions in stackage
     `(#:tests? #f))
    (properties '((upstream-name . "colour")))
    (home-page "http://www.haskell.org/haskellwiki/Colour")
    (synopsis "Model for human colour perception")
    (description
     "This package provides a data type for colours and transparency.
Colours can be blended and composed.  Various colour spaces are
supported.  A module of colour names (\"Data.Colour.Names\") is provided.")
    (license license:expat)))

(define-public ghc-hscolour
  (package
    (name "ghc-hscolour")
    (version "1.25")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hscolour" version))
       (sha256
        (base32 "0z679khnmb6as1zcdb44n9qjk7in32jpm4ldscpqg7jrapd31kjl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hscolour")))
    (home-page "http://code.haskell.org/~malcolm/hscolour/")
    (synopsis "Colourise Haskell code")
    (description
     "hscolour is a small Haskell script to colourise Haskell code.  It currently has
six output formats: ANSI terminal codes (optionally XTerm-256colour codes), HTML
3.2 with <font> tags, HTML 4.01 with CSS, HTML 4.01 with CSS and mouseover
annotations, XHTML 1.0 with inline CSS styling, LaTeX, and mIRC chat codes.")
    (license license:lgpl2.1)))

(define-public ghc-comonad
  (package
    (name "ghc-comonad")
    (version "5.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "comonad" version))
       (sha256
        (base32 "12d7g3c2x1jb6jlmdgwdmi840z3p91b0l7fkfsdml1c0kas0xyv9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "comonad")))
    (inputs (list ghc-tagged ghc-transformers-compat ghc-distributive
                  ghc-indexed-traversable))
    (arguments
     `(#:cabal-revision ("1"
                         "0ly3cy3p99zvizspassk0wjnw9bz0spc11s69s790g2qpxwyvbbm")))
    (home-page "http://github.com/ekmett/comonad/")
    (synopsis "Comonads for Haskell")
    (description "This library provides @code{Comonad}s for Haskell.")
    (license license:bsd-3)))

(define-public ghc-concatenative
  (package
    (name "ghc-concatenative")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "concatenative" version))
              (sha256
               (base32
                "05xwqvcdnk8bsyj698ab9jxpa1nk23pf3m7wi9mwmw0q8n99fngd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "concatenative")))
    (home-page
     "https://patch-tag.com/r/salazar/concatenative/snapshot/current/content/pretty")
    (synopsis "Library for postfix control flow")
    (description
     "Concatenative gives Haskell Factor-style combinators and arrows for
postfix notation.  For more information on stack based languages, see
@uref{https://concatenative.org}.")
    (license license:bsd-3)))

(define-public ghc-concurrent-output
  (package
    (name "ghc-concurrent-output")
    (version "1.10.21")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "concurrent-output" version))
       (sha256
        (base32 "04mmlfgamhki4fgi8cxjdqlmxif32d5dkrimzlwka0m18fpz682a"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "concurrent-output")))
    (inputs (list ghc-async ghc-ansi-terminal ghc-terminal-size))
    (home-page "http://hackage.haskell.org/package/concurrent-output")
    (synopsis "Ungarble output from several threads or commands")
    (description
     "Lets multiple threads and external processes concurrently output to the
console, without it getting all garbled up.

Built on top of that is a way of defining multiple output regions, which are
automatically laid out on the screen and can be individually updated by
concurrent threads.  Can be used for progress displays etc.")
    (license license:bsd-2)))

(define-public ghc-conduit
  (package
    (name "ghc-conduit")
    (version "1.3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "conduit" version))
       (sha256
        (base32 "0gxsahlfaqjkmr0a2bm5s1i3p5rnzqma8gd85yccpr577vq2m439"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "conduit")))
    (inputs (list ghc-resourcet ghc-primitive ghc-unliftio-core
                  ghc-mono-traversable ghc-vector))
    (native-inputs (list ghc-hspec
                         ghc-quickcheck
                         ghc-safe
                         ghc-split
                         ghc-silently
                         ghc-unliftio))
    (home-page "http://github.com/snoyberg/conduit")
    (synopsis "Streaming data library")
    (description
     "The conduit package is a solution to the streaming data problem,
allowing for production, transformation, and consumption of streams of data
in constant memory.  It is an alternative to lazy I/O which guarantees
deterministic resource handling, and fits in the same general solution
space as enumerator/iteratee and pipes.")
    (license license:expat)))

(define-public ghc-conduit-algorithms
  (package
    (name "ghc-conduit-algorithms")
    (version "0.0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "conduit-algorithms" version))
       (sha256
        (base32 "1gjw7a1q6spvds53j5bvcxz906s8p3jn3phiq52bf42pfzf7yw4k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "conduit-algorithms")))
    (inputs (list ghc-async
                  ghc-bzlib-conduit
                  ghc-conduit
                  ghc-conduit-combinators
                  ghc-conduit-extra
                  ghc-conduit-zstd
                  ghc-fingertree
                  ghc-lzma
                  ghc-monad-control
                  ghc-resourcet
                  ghc-stm-conduit
                  ghc-streaming-commons
                  ghc-unliftio-core
                  ghc-vector))
    (native-inputs (list ghc-hunit
                         ghc-quickcheck
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-tasty-th))
    (home-page "https://github.com/luispedro/conduit-algorithms#readme")
    (synopsis "Conduit-based algorithms")
    (description
     "This package provides algorithms on @code{Conduits}, including higher
level asynchronous processing and some other utilities.")
    (license license:expat)))

(define-public ghc-conduit-combinators
  (package
    (name "ghc-conduit-combinators")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "conduit-combinators" version))
       (sha256
        (base32 "1lz70vwp4y4lpsivxl0cshq7aq3968rh48r6rjvpyaj2l0bdj5wp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "conduit-combinators")))
    (home-page "https://github.com/snoyberg/mono-traversable#readme")
    (synopsis "Commonly used conduit functions, for both chunked and
unchunked data")
    (description
     "This Haskell package provides a replacement for Data.Conduit.List,
as well as a convenient Conduit module.")
    (license license:expat)))

(define-public ghc-conduit-extra
  (package
    (name "ghc-conduit-extra")
    (version "1.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "conduit-extra" version))
       (sha256
        (base32 "08l2728vyr3dppnj4z3yagi2265ixp8g8ayhz07x3x88jj73w7s9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "conduit-extra")))
    (inputs (list ghc-conduit
                  ghc-async
                  ghc-attoparsec
                  ghc-network
                  ghc-primitive
                  ghc-resourcet
                  ghc-streaming-commons
                  ghc-unliftio-core
                  ghc-typed-process))
    (native-inputs (list ghc-hspec hspec-discover ghc-quickcheck ghc-transformers-base))
    (arguments
     `(#:cabal-revision ("1"
                         "1fq0cs2fcn2kd1mvp9ygsp7rm5qridwp1wwnr60jmpahvihb4cp9")))
    (home-page "http://github.com/snoyberg/conduit")
    (synopsis "Conduit adapters for common libraries")
    (description
     "The @code{conduit} package itself maintains relative small dependencies.
The purpose of this package is to collect commonly used utility functions
wrapping other library dependencies, without depending on heavier-weight
dependencies.  The basic idea is that this package should only depend on
@code{haskell-platform} packages and @code{conduit}.")
    (license license:expat)))

(define-public ghc-conduit-zstd
  (package
    (name "ghc-conduit-zstd")
    (version "0.0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "conduit-zstd" version))
       (sha256
        (base32 "0f0ir4zs3skw33c8mfppxhfsyqh1c2cnc4gkf8bvv3bdiikdj1yl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "conduit-zstd")))
    (inputs (list ghc-conduit ghc-zstd))
    (native-inputs (list ghc-conduit-combinators ghc-conduit-extra
                         ghc-quickcheck-instances ghc-tasty
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/luispedro/conduit-zstd#readme")
    (synopsis "Conduit-based ZStd Compression")
    (description
     "Zstandard compression packaged as a conduit.  This is
a very thin wrapper around the
@url{https://github.com/facebookexperimental/hs-zstd/, official hs-zstd
interface}.")
    (license license:expat)))

(define-public ghc-config-ini
  (package
    (name "ghc-config-ini")
    (version "0.2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "config-ini" version))
       (sha256
        (base32 "00b9b590566hrxrjn31jkq70768dnrzzsjrasrnhdvd6p92iq5rs"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "config-ini")))
    (inputs (list ghc-unordered-containers ghc-megaparsec))
    (native-inputs (list ghc-ini ghc-hedgehog ghc-doctest ghc-microlens))
    (arguments
     `(#:cabal-revision ("1"
                         "1cjpz7q0lsxac1r011ik34fcsi4lsy634ayxyblzyszigvks7r9a")))
    (home-page "https://github.com/aisamanra/config-ini")
    (synopsis "Monadic Haskell DSL for parsing simple INI configuration files")
    (description
     "The @code{config-ini} Haskell library exports some simple monadic
functions to ease the parsing of @file{.ini}-style configuration files, and
to write and update them in an efficient @i{diff-minimal} way.  This means that
if you parse a file, update a single field, and reserialize, that file should
differ only in the field we changed and @emph{that's it}: field order, comments,
and incidental whitespace will remain unchanged.  The library aims to produce
human-readable error messages when things go wrong.")
    (license license:bsd-3)))

(define-public ghc-config-schema
  (package
    (name "ghc-config-schema")
    (version "1.3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "config-schema" version))
       (sha256
        (base32 "1j5br9y4s51ajxyg4aldibywqhf4qrxhrypac8jgca2irxdwb29w"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision ("7"
                         "19yg0k7cixxfhwxfdyy6js7rgfiy8ag8n8n3wlbkcsacl3r4jwca")))
    (inputs
     (list ghc-config-value
           ghc-free
           ghc-kan-extensions
           ghc-semigroupoids))
    (properties '((upstream-name . "config-schema")))
    (home-page "https://github.com/glguy/config-schema")
    (synopsis "Schema definitions for the config-value package")
    (description
     "This package makes it possible to define schemas for use when loading
configuration files using the config-value format.  These schemas can be used to
process a configuration file into a Haskell value or to automatically generate
documentation for the file format.")
    (license license:isc)))

(define-public ghc-config-value
  (package
    (name "ghc-config-value")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "config-value" version))
       (sha256
        (base32 "0pkcwxg91wali7986k03d7q940hb078hlsxfknqhkp2spr3d1f3w"))))
    (build-system haskell-build-system)
    (native-inputs (list ghc-alex ghc-happy))
    (properties '((upstream-name . "config-value")))
    (arguments
     `(#:cabal-revision ("9"
                         "082fxqjf40fn14m6w5j0pq21qbl89l6yflxpy9wcna2nmv9rc3sk")))
    (home-page "https://github.com/glguy/config-value")
    (synopsis "Simple, layout-based value language similar to YAML or JSON")
    (description
     "This package implements a language similar to YAML or JSON but with fewer
special cases and fewer dependencies.  It emphasizes layout structure for
sections and lists, and requires quotes around strings.")
    (license license:expat)))

(define-public ghc-configurator
  (package
    (name "ghc-configurator")
    (version "0.3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "configurator" version))
       (sha256
        (base32 "1d1iq1knwiq6ia5g64rw5hqm6dakz912qj13r89737rfcxmrkfbf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "configurator")))
    (inputs (list ghc-attoparsec ghc-hashable ghc-unix-compat
                  ghc-unordered-containers))
    (native-inputs (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (home-page "http://github.com/bos/configurator")
    (synopsis "Configuration management")
    (description
     "This package provides a configuration management library for programs
and daemons.  The features include:

@enumerate
@item Automatic, dynamic reloading in response to modifications to
  configuration files.
@item A simple, but flexible, configuration language, supporting several of
  the most commonly needed types of data, along with interpolation of strings
  from the configuration or the system environment (e.g. @code{$(HOME)}).
@item Subscription-based notification of changes to configuration properties.
@item An @code{import} directive allows the configuration of a complex
  application to be split across several smaller files, or common configuration
  data to be shared across several applications.
@end enumerate
")
    (license license:bsd-3)))

(define-public ghc-constraints
  (package
    (name "ghc-constraints")
    (version "0.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "constraints" version))
       (sha256
        (base32 "1y0rzl41wwkr3gqzf3ymf0mns86qkafamqindvcfq1qqbn09y92g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "constraints")))
    (inputs (list ghc-boring ghc-hashable))
    (native-inputs (list ghc-hspec hspec-discover))
    (arguments
     `(#:cabal-revision ("1"
                         "1rx81r735ih046j4afgqlbmqlsf3zk2c5d8k9mj3gndasplm66iq")))
    (home-page "http://github.com/ekmett/constraints/")
    (synopsis "Constraint manipulation")
    (description
     "GHC 7.4 gave us the ability to talk about @code{ConstraintKinds}.
They stopped crashing the compiler in GHC 7.6.  This package provides
a vocabulary for working with them.")
    (license license:bsd-2)))

(define-public ghc-constraints-extras
  (package
    (name "ghc-constraints-extras")
    (version "0.4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "constraints-extras" version))
       (sha256
        (base32 "0b28rc1wb2c231za7w7j90s6m8bdxgidhxhia0bfpfpi4lkdnzyc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "constraints-extras")))
    (inputs (list ghc-constraints ghc-aeson))
    (home-page "https://github.com/obsidiansystems/constraints-extras")
    (synopsis "Utility package for constraints")
    (description
     "Convenience functions and TH for working with constraints.  See
@file{README.md} for example usage.")
    (license license:bsd-3)))

(define-public ghc-contravariant
  (package
    (name "ghc-contravariant")
    (version "1.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "contravariant" version))
       (sha256
        (base32 "1ynz89vfn7czxpa203zmdqknkvpylzzl9rlkpasx1anph1jxcbq6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "contravariant")))
    (inputs (list ghc-statevar))
    (home-page "http://github.com/ekmett/contravariant/")
    (synopsis "Contravariant functors")
    (description "Contravariant functors for Haskell.")
    (license license:bsd-3)))

(define-public ghc-contravariant-extras
  (package
    (name "ghc-contravariant-extras")
    (version "0.3.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "contravariant-extras" version))
       (sha256
        (base32 "13i9cip0qb8piib2sfq35qvb978npra2w53zvc28fxxnik9icfig"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "contravariant-extras")))
    (inputs (list ghc-contravariant ghc-template-haskell-compat-v0208))
    (home-page "https://github.com/nikita-volkov/contravariant-extras")
    (synopsis "Extras for the @code{ghc-contravariant} Haskell package")
    (description "This Haskell package provides extras for the
@code{ghc-contravariant} package.")
    (license license:expat)))

(define-public ghc-control-monad-free
  (package
    (name "ghc-control-monad-free")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "control-monad-free" version))
       (sha256
        (base32 "1habgf7byffqf1rqjkzpihvdhclaafgqsqpfpwp3fgpj5ayk1j33"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "control-monad-free")))
    (home-page "http://github.com/pepeiborra/control-monad-free")
    (synopsis "Free monads and monad transformers")
    (description
     "This package provides datatypes to construct Free monads, Free monad
transformers, and useful instances.  In addition it provides the constructs to
avoid quadratic complexity of left associative bind, as explained in:

@itemize @bullet
@item
Janis Voigtlander, @cite{Asymptotic Improvement of Computations over
Free Monads, MPC'08}
@end itemize")
    (license license:public-domain)))

(define-public ghc-convertible
  (package
    (name "ghc-convertible")
    (version "1.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "convertible" version))
       (sha256
        (base32 "1vwc6h1z88xkw4bq3js8x9x86jnk3amdskyksca77p0kwiqbs7lr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "convertible")))
    (inputs (list ghc-old-time))
    (native-inputs (list ghc-quickcheck))
    (home-page "http://hackage.haskell.org/package/convertible")
    (synopsis "Typeclasses and instances for converting between types")
    (description
     "This package provides a typeclass with a single function that is
designed to help convert between different types: numeric values, dates and
times, and the like.  The conversions perform bounds checking and return a
pure @code{Either} value.  This means that you need not remember which specific
function performs the conversion you desire.")
    (license license:bsd-3)))

(define-public ghc-csv
  (package
    (name "ghc-csv")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "csv" version))
       (sha256
        (base32 "00767ai09wm7f0yzmpqck3cpgxncpr9djnmmz5l17ajz69139x4c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "csv")))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           ;; The package includes an old Setup.hs script that no longer works
           ;; with modern Cabal versions
           (add-before 'generate-setuphs 'remove-setup.hs
             (lambda _
               (delete-file "Setup.hs"))))))
    (home-page "http://hackage.haskell.org/package/csv")
    (synopsis "CSV loader and dumper")
    (description
     "This library parses and dumps documents that are formatted according to
RFC 4180, @cite{The common Format and MIME Type for Comma-Separated
Values (CSV) Files}.  This format is used, among many other things, as a
lingua franca for spreadsheets, and for certain web services.")
    (license license:expat)))

(define-public ghc-data-accessor
  (package
    (name "ghc-data-accessor")
    (version "0.2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-accessor" version))
       (sha256
        (base32 "14ap1lxizxkgphl4kg8lr3ny9lblx1k6hm8i9nm7l43yla8cg8q6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-accessor")))
    (arguments
     `(#:cabal-revision ("2"
                         "0qzccxgxfiyas435z14k8mkxz10fpyj0vrl856hiw0inv228cn9z")))
    (home-page "http://www.haskell.org/haskellwiki/Record_access")
    (synopsis
     "Haskell utilities for accessing and manipulating fields of records")
    (description "This package provides Haskell modules for accessing and
manipulating fields of records.")
    (license license:bsd-3)))

(define-public ghc-data-accessor-transformers
  (package
    (name "ghc-data-accessor-transformers")
    (version "0.2.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-accessor-transformers" version))
       (sha256
        (base32 "1m18bdhddi4l7ijd1mighjbjdw5qkznsgrqb8532cv9al55r9y83"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-accessor-transformers")))
    (inputs (list ghc-data-accessor))
    (home-page "http://www.haskell.org/haskellwiki/Record_access")
    (synopsis "Use Accessor to access state in transformers State monad")
    (description "This package provides Haskell modules to allow use of
Accessor to access state in transformers State monad.")
    (license license:bsd-3)))

(define-public ghc-data-clist
  (package
    (name "ghc-data-clist")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-clist" version))
       (sha256
        (base32 "04mj0d1yp0l27v2my51w9q5zpdrdhp29fdyvmwqgxxp8f6yiwfhw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-clist")))
    (native-inputs (list ghc-quickcheck))
    (arguments
     (list
      #:cabal-revision '("1"
                         "09922p8ydfgqsy29p9qfiss70bks85bzz6g9s3gzrd93lpzhiba7")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'relax-dependency-contraints
            (lambda _
              (substitute* "data-clist.cabal"
                (("QuickCheck [0-9 .&|<>=*]*") "QuickCheck")))))))
    (home-page "https://github.com/sw17ch/data-clist")
    (synopsis "Simple, functional, bidirectional circular list type")
    (description
     "This Haskell library provides a simple purely functional circular list,
or ring, data type: a circular data structure such that if you continue rotating
the ring in either direction, you'll eventually return to the element you first
observed.")
    (license license:bsd-3)))

(define-public ghc-data-default
  (package
    (name "ghc-data-default")
    (version "0.8.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-default" version))
       (sha256
        (base32 "1jmn83y7ss7896xvykpxxp4kfmvxa65avw0asg0z15i1xkl5s26f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-default")))
    (home-page "http://hackage.haskell.org/package/data-default")
    (synopsis "Types with default values")
    (description
     "This package defines a class for types with a default value, and
provides instances for types from the base, containers, dlist and old-locale
packages.")
    (license license:bsd-3)))

(define-public ghc-data-default-class
  (package
    (name "ghc-data-default-class")
    (version "0.2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-default-class" version))
       (sha256
        (base32 "1d6m12yv5vjciwbig484jrv9qpy7v762k51rpalcimhbzg231r8a"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-default-class")))
    (inputs (list ghc-data-default))
    (home-page "http://hackage.haskell.org/package/data-default-class")
    (synopsis "Types with default values")
    (description "This package defines a class for types with default values.")
    (license license:bsd-3)))

(define-public ghc-data-fix
  (package
    (name "ghc-data-fix")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-fix" version))
       (sha256
        (base32 "0dxb2s2bxdl4fcd0kfybprz9kwsskdwshdh543lvjyh4ik8m5w4d"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-fix")))
    (inputs (list ghc-hashable))
    (arguments
     `(#:cabal-revision ("1"
                         "06r6054jfaqqf8yx21m86x5bzpnkjmqrbppyf3b7h26ad1hvwy7f")))
    (home-page "https://github.com/spell-music/data-fix")
    (synopsis "Fixpoint data types")
    (description
     "Fixpoint types and recursion schemes.  If you define your AST as
fixpoint type, you get fold and unfold operations for free.

Thanks for contribution to: Matej Kollar, Herbert Valerio Riedel")
    (license license:bsd-3)))

(define-public ghc-data-hash
  (package
    (name "ghc-data-hash")
    (version "0.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-hash" version))
       (sha256
        (base32 "1ghbqvc48gf9p8wiy71hdpaj7by3b9cw6wgwi3qqz8iw054xs5wi"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-hash")))
    (native-inputs (list ghc-quickcheck ghc-test-framework
                         ghc-test-framework-quickcheck2))
    (home-page "http://hackage.haskell.org/package/data-hash")
    (synopsis "Combinators for building fast hashing functions")
    (description
     "This package provides combinators for building fast hashing functions.
It includes hashing functions for all basic Haskell98 types.")
    (license license:bsd-3)))

(define-public ghc-data-ordlist
  (package
    (name "ghc-data-ordlist")
    (version "0.4.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-ordlist" version))
       (sha256
        (base32 "03a9ix1fcx08viwv2jg5ndw1qbkydyyrmjvqr9wasmcik9x1wv3g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-ordlist")))
    (home-page "http://hackage.haskell.org/package/data-ordlist")
    (synopsis "Set and bag operations on ordered lists")
    (description
     "This module provides set and multiset operations on ordered lists.")
    (license license:bsd-3)))

(define-public ghc-dbus
  (package
    (name "ghc-dbus")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "dbus" version))
       (sha256
        (base32 "016xrx8gnvldpwgalpsxzvkwagavpzw9m7j65w5msskaxk474ln7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dbus")))
    (arguments (list #:tests? #f)) ; Network tests fail to connect.
    (inputs (list ghc-cereal
                  ghc-conduit
                  ghc-lens
                  ghc-network
                  ghc-random
                  ghc-split
                  ghc-th-lift
                  ghc-vector
                  ghc-xml-conduit
                  ghc-xml-types))
    (native-inputs (list ;ghc-alex
                         ghc-extra
                         ghc-quickcheck
                         ghc-resourcet
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-temporary))
    (home-page "https://github.com/rblaze/haskell-dbus#readme")
    (synopsis "Client library for the D-Bus IPC system")
    (description
     "D-Bus is a simple, message-based protocol for inter-process
communication, which allows applications to interact with other parts
of the machine and the user's session using remote procedure
calls.   D-Bus is a essential part of the modern Linux desktop, where
it replaces earlier protocols such as CORBA and DCOP.  This library
is an implementation of the D-Bus protocol in Haskell.  It can be used
to add D-Bus support to Haskell applications, without the awkward
interfaces common to foreign bindings.")
    (license license:asl2.0)))

(define-public ghc-decimal
  (package
    (name "ghc-decimal")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Decimal" version))
       (sha256
        (base32 "19w7i9f0lbiyzwa0v3bm95233vi7f1688f0xms6cnjsf88h04ym3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Decimal")))
    (native-inputs (list ghc-hunit ghc-quickcheck ghc-test-framework
                         ghc-test-framework-quickcheck2
                         ghc-test-framework-hunit))
    (home-page "https://github.com/PaulJohnson/Haskell-Decimal")
    (synopsis "Decimal numbers with variable precision")
    (description
     "A decimal number has an integer mantissa and a negative exponent.
The exponent can be interpreted as the number of decimal places in the
value.")
    (license license:bsd-3)))

(define-public ghc-deepseq-generics
  (package
    (name "ghc-deepseq-generics")
    (version "0.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "deepseq-generics" version))
       (sha256
        (base32 "1ygfppvlxhm2zl3jvnalinl7cp790m7p31408lzgsw2jah63k6ai"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "deepseq-generics")))
    (native-inputs (list ghc-test-framework ghc-test-framework-hunit ghc-hunit))
    (home-page "https://github.com/haskell-hvr/deepseq-generics")
    (synopsis "Generic RNF implementation")
    (description
     "This package provides a @code{GHC.Generics}-based
@code{Control.DeepSeq.Generics.genericRnf} function which can be used for
providing an @code{rnf} implementation.")
    (license license:bsd-3)))

(define-public ghc-dense-linear-algebra
  (package
    (name "ghc-dense-linear-algebra")
    (version "0.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "dense-linear-algebra" version))
       (sha256
        (base32 "1m7jjxahqxj7ilic3r9806mwp5rnnsmn8vvipkmk40xl65wplxzp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dense-linear-algebra")))
    (inputs (list ghc-math-functions
                  ghc-primitive
                  ghc-vector
                  ghc-vector-algorithms
                  ghc-vector-th-unbox
                  ghc-vector-binary-instances))
    (native-inputs (list ghc-hspec ghc-quickcheck))
    (home-page "http://hackage.haskell.org/package/dense-linear-algebra")
    (synopsis "Simple and incomplete implementation of linear algebra")
    (description "This library is simply a collection of linear-algebra
related modules split from the statistics library.")
    (license license:bsd-2)))

(define-public ghc-diagrams-core
  (package
    (name "ghc-diagrams-core")
    (version "1.5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "diagrams-core" version))
       (sha256
        (base32 "168kjikw3x21pjgfy3lmxmrm89g9zlhbypkmzdg5xz9rl7acn7rc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "diagrams-core")))
    (inputs (list ghc-unordered-containers
                  ghc-semigroups
                  ghc-monoid-extras
                  ghc-dual-tree
                  ghc-lens
                  ghc-linear
                  ghc-adjunctions
                  ghc-distributive
                  ghc-profunctors))
    (arguments
     `(#:cabal-revision ("4"
                         "0lq4hs7h7ja9x8wz05z21fi9b507r8capxwknmadrxznrh892kky")))
    (home-page "https://diagrams.github.io")
    (synopsis "Core libraries for diagrams embedded domain-specific language")
    (description
     "This package provides the core modules underlying
diagrams, an embedded domain-specific language for compositional,
declarative drawing.")
    (license license:bsd-3)))

(define-public ghc-diagrams-lib
  (package
    (name "ghc-diagrams-lib")
    (version "1.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "diagrams-lib" version))
       (sha256
        (base32 "0s09qkhlp0w7ga20i482icw48y0l8rgqb38818ny64h3ivh8s5hm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "diagrams-lib")))
    (inputs (list ghc-semigroups
                  ghc-monoid-extras
                  ghc-dual-tree
                  ghc-diagrams-core
                  ghc-diagrams-solve
                  ghc-active
                  ghc-colour
                  ghc-data-default
                  ghc-fingertree
                  ghc-intervals
                  ghc-lens
                  ghc-tagged
                  ghc-optparse-applicative
                  ghc-juicypixels
                  ghc-hashable
                  ghc-linear
                  ghc-adjunctions
                  ghc-distributive
                  ghc-unordered-containers
                  ghc-profunctors
                  ghc-cereal
                  ghc-fail))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck
                         ghc-quickcheck ghc-numeric-extras))
    (home-page "http://diagrams.github.io")
    (synopsis "Embedded domain-specific language for declarative graphics")
    (description
     "Diagrams is a flexible, extensible embedded
domain-specific language (EDSL) for creating graphics of many types.
Graphics can be created in arbitrary vector spaces and rendered with
multiple backends.  This package provides a standard library of
primitives and operations for creating diagrams.")
    (license license:bsd-3)))

(define-public ghc-diagrams-solve
  (package
    (name "ghc-diagrams-solve")
    (version "0.1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "diagrams-solve" version))
       (sha256
        (base32 "19rymk08gh8bq7afa5p2hjj4747j0ardacbdvh205czfhirmdmga"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "diagrams-solve")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck))
    (home-page "https://diagrams.github.io")
    (synopsis "Pure Haskell solver routines used by diagrams")
    (description
     "This library provides Pure Haskell solver routines for
use by the
@url{https://archives.haskell.org/projects.haskell.org/diagrams/,
diagrams framework}.  It currently includes routines for finding real
roots of low-degree (@math{n < 5}) polynomials, and solving tridiagonal
and cyclic tridiagonal linear systems.")
    (license license:bsd-3)))

(define-public ghc-diagrams-svg
  (package
    (name "ghc-diagrams-svg")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "diagrams-svg" version))
       (sha256
        (base32 "1g11fvcgx99xg71c9sd6m7pfclnzcfx72alcx3avlb4qzz56wn52"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "diagrams-svg")))
    (inputs (list ghc-base64-bytestring
                  ghc-colour
                  ghc-diagrams-core
                  ghc-diagrams-lib
                  ghc-monoid-extras
                  ghc-svg-builder
                  ghc-juicypixels
                  ghc-split
                  ghc-lens
                  ghc-hashable
                  ghc-optparse-applicative
                  ghc-semigroups))
    (arguments
     `(#:cabal-revision ("2"
                         "1d7n707vmcbk1l1fi956hagyyzzn3hd11wxyabm1mirv8qxrha0s")))
    (home-page "https://diagrams.github.io/")
    (synopsis "Scalable Vector Graphics backend for the diagrams framework")
    (description "This package provides a modular backend for rendering
diagrams created with the diagrams embedded domain-specific
language (EDSL) to Scalable Vector Graphics (SVG) files.")
    (license license:bsd-3)))

(define-public ghc-dictionary-sharing
  (package
    (name "ghc-dictionary-sharing")
    (version "0.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "dictionary-sharing" version))
       (sha256
        (base32 "00aspv943qdqhlk39mbk00kb1dsa5r0caj8sslrn81fnsn252fwc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dictionary-sharing")))
    (arguments
     `(#:cabal-revision ("4"
                         "18v6x0pjih851q5d8cdm79bhpdh6wxv9p6z746y7wppmy9j943qy")))
    (home-page "http://hackage.haskell.org/package/dictionary-sharing")
    (synopsis "Sharing/memoization of class members")
    (description "This library provides tools for ensuring that class
members are shared.")
    (license license:bsd-3)))

(define-public ghc-diff
  (package
    (name "ghc-diff")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Diff" version))
       (sha256
        (base32 "11ad6ng3pqjb9dixnqk35bpgzjzva7ifnnjjlz0z5xzbsp9jcznd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Diff")))
    (native-inputs (list ghc-quickcheck ghc-test-framework
                         ghc-test-framework-quickcheck2))
    (home-page
     "https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.6927")
    (synopsis "O(ND) diff algorithm in Haskell")
    (description
     "This package provides an implementation of the standard diff algorithm,
and utilities for pretty printing.")
    (license license:bsd-3)))

(define-public ghc-directory-ospath-streaming
  (package
    (name "ghc-directory-ospath-streaming")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "directory-ospath-streaming" version))
       (sha256
        (base32 "1xsfaps14pm68qfva4crqsmpqqcl8yjxrv514a6mxc9xw6z8zphs"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "directory-ospath-streaming")))
    (inputs (list ghc-os-string ghc-atomic-counter))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-random))
    (home-page "https://github.com/sergv/directory-ospath-streaming")
    (synopsis "Stream directory entries in constant memory in vanilla IO")
    (description
     "Reading of directory contents in constant memory, i.e.  in an iterative fashion
without storing all directory elements in memory.  From another perspective,
this reading interface allows stopping at any point without loading every
directory element.  Also defines general-purpose recursive directory traversals.
 Both Windows and Unix systems are supported.")
    (license license:asl2.0)))

(define-public ghc-disk-free-space
  (package
    (name "ghc-disk-free-space")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "disk-free-space" version))
       (sha256
        (base32 "07rqj8k1vh3cykq9yidpjxhgh1f7vgmjs6y1nv5kq2217ff4yypi"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "disk-free-space")))
    (arguments
     `(#:cabal-revision ("3"
                         "0x0wjycr3rhw9vcq51b4sz8cf7mcvx7whhywv72y25r9385lxb3i")))
    (home-page "https://github.com/redneb/disk-free-space")
    (synopsis "Retrieve information about disk space usage")
    (description "A cross-platform library for retrieving information about
disk space usage.")
    (license license:bsd-3)))

(define-public ghc-distributive
  (package
    (name "ghc-distributive")
    (version "0.6.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "distributive" version))
       (sha256
        (base32 "14bb66qyfn43bj688igfvnfjw7iycjf4n2k38sm8rxbqw2916dfp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "distributive")))
    (inputs (list ghc-base-orphans ghc-tagged ghc-semigroups-bootstrap))
    (native-inputs (list ghc-generic-deriving ghc-hspec hspec-discover))
    (arguments
     `(#:cabal-revision ("1"
                         "033890dfyd23dh7g7px863l0hr1b881jnhv4kgwaq16a3iagb68g")))
    (home-page "http://github.com/ekmett/distributive/")
    (synopsis "Distributive functors for Haskell")
    (description "This package provides distributive functors for Haskell.
Dual to @code{Traversable}.")
    (license license:bsd-3)))

(define-public ghc-djot
  (package
    (name "ghc-djot")
    (version "0.1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "djot" version))
       (sha256
        (base32 "17fqnzacnnraij9lwca25761sn5gaxjd42vnpmd397fdzd5dm6fh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "djot")))
    (inputs (list ghc-doclayout))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck))
    (home-page "http://hackage.haskell.org/package/djot")
    (synopsis "Parser and renderer for djot light markup syntax.")
    (description
     "Djot (<https://djot.net>) is a light markup language.  This package provides a
data structure to represent djot documents, a very fast parser, and functions to
render a parsed document as HTML and as djot.")
    (license license:expat)))

(define-public ghc-dlist
  (package
    (name "ghc-dlist")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "dlist" version))
       (sha256
        (base32 "0581a60xw4gw7pmqlmg5w2hr4hm9yjgx4c2z6v63y5xv51rn6g8p"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dlist")))
    (native-inputs (list ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "1lk1ladz44chpfi1gx8yglzrxbzd9yyb258gs6rnm0as9icjfiw5")))
    (home-page "https://github.com/spl/dlist")
    (synopsis "Difference lists")
    (description
     "Difference lists are a list-like type supporting O(1) append.  This is
particularly useful for efficient logging and pretty printing (e.g. with the
Writer monad), where list append quickly becomes too expensive.")
    (license license:bsd-3)))

(define-public ghc-doctemplates
  (package
    (name "ghc-doctemplates")
    (version "0.11.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "doctemplates" version))
       (sha256
        (base32 "1ysmd7dl12gh4a1ci9g6qfwz2836dqb22g5l5q9941dac4c4al0i"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "doctemplates")))
    (inputs (list ghc-safe
                  ghc-text-conversions
                  ghc-aeson
                  ghc-doclayout
                  ghc-vector
                  ghc-scientific))
    (native-inputs (list ghc-glob ghc-tasty ghc-tasty-golden ghc-tasty-hunit
                         ghc-temporary))
    (home-page "https://github.com/jgm/doctemplates#readme")
    (synopsis "Pandoc-style document templates")
    (description
     "This package provides a simple text templating system used by pandoc.")
    (license license:bsd-3)))

(define-public ghc-doctest
  (package
    (name "ghc-doctest")
    (version "0.24.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "doctest" version))
       (sha256
        (base32 "1dpffnr24zaricmkwc13npap569crwwfha1w9vz3fhywmh0dnfjk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "doctest")))
    ;; The tests depend on cabal-install which depends on doctest
    (arguments (list #:tests? #f))
    (inputs (list ghc-code-page ghc-paths ghc-syb ghc-temporary))
    (home-page "https://github.com/sol/doctest#readme")
    (synopsis "Test interactive Haskell examples")
    (description
     "The doctest program checks examples in source code comments.
It is modeled after doctest for Python, see
@uref{https://docs.python.org/library/doctest.html, the Doctest website}.")
    (license license:expat)))

(define-public ghc-dotgen
  (package
    (name "ghc-dotgen")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "dotgen" version))
       (sha256
        (base32 "1jcn5m9342jrdq7jln2v9msf9978ngrx0pq9rrjh8izhvbvph76s"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dotgen")))
    (home-page "https://github.com/ku-fpg/dotgen")
    (synopsis "Simple interface for building .dot graph files")
    (description
     "This package provides a simple interface for building .dot graph
files, for input into the dot and graphviz tools.  It includes a
monadic interface for building graphs.")
    (license license:bsd-3)))

(define-public ghc-double-conversion
  (package
    (name "ghc-double-conversion")
    (version "2.0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "double-conversion" version))
       (sha256
        (base32 "1fj358zamb5zr1h481wf02c2aijqk3imk7flhpzps9bvwjv9kilq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "double-conversion")))
    (native-inputs (list ghc-hunit ghc-test-framework ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2))
    (home-page "https://github.com/haskell/double-conversion")
    (synopsis
     "Fast conversion between double precision floating point and text")
    (description
     "This package provides a library that performs fast, accurate conversion
between double precision floating point and text.")
    (license license:bsd-3)))

(define-public ghc-dual-tree
  (package
    (name "ghc-dual-tree")
    (version "0.2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "dual-tree" version))
       (sha256
        (base32 "19nm34d166fhlkk7npx0iq9kbx7300a82bg75q1sx98jqfa4nffh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dual-tree")))
    (inputs (list ghc-semigroups ghc-monoid-extras))
    (native-inputs (list ghc-quickcheck ghc-testing-feat))
    (arguments
     `(#:cabal-revision ("5"
                         "0ihx1l9s864z4pvqkahy0ch6k4fqrz4yzb0dqwrnagsa2akbz9c4")))
    (home-page "http://hackage.haskell.org/package/dual-tree")
    (synopsis "Rose trees with cached and accumulating monoidal annotations")
    (description
     "Rose (@math{n}-ary) trees with both upwards- (i.e.
cached) and downwards-traveling (i.e. accumulating) monoidal
annotations.  This is used as the core data structure underlying the
@url{https://archives.haskell.org/projects.haskell.org/diagrams/,
diagrams framework}, but potentially has other applications as well.")
    (license license:bsd-3)))

(define-public ghc-easy-file
  (package
    (name "ghc-easy-file")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "easy-file" version))
       (sha256
        (base32 "1fzj9x9br57rcik3dvwxqb5mqy524g6xg2d670l6dcrv9f8s03zf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "easy-file")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "http://github.com/kazu-yamamoto/easy-file")
    (synopsis "File handling library for Haskell")
    (description "This library provides file handling utilities for Haskell.")
    (license license:bsd-3)))

(define-public ghc-easyplot
  (package
    (name "ghc-easyplot")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "easyplot" version))
       (sha256
        (base32 "18kndgvdj2apjpfga6fp7m16y1gx8zrwp3c5vfj03sx4v6jvciqk"))
       (snippet #~(rename-file "Setup.lhs" "Setup.hs")))) ; the file lacks lhs syntax
    (build-system haskell-build-system)
    (properties '((upstream-name . "easyplot")))
    (home-page "http://hub.darcs.net/scravy/easyplot")
    (synopsis "Haskell plotting library based on gnuplot")
    (description "This package provides a plotting library for
Haskell, using gnuplot for rendering.")
    (license license:expat)))

(define-public ghc-echo
  (package
    (name "ghc-echo")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "echo" version))
       (sha256
        (base32 "0hqfdd4kvpp59cjjv790bkf72yqr9xjfqlbjcrdsc9a8j3r1pzn9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "echo")))
    (home-page "https://github.com/RyanGlScott/echo")
    (synopsis "Echo terminal input portably")
    (description
     "The @code{base} library exposes the @code{hGetEcho} and
@code{hSetEcho} functions for querying and setting echo status, but
unfortunately, neither function works with MinTTY consoles on Windows.
This library provides an alternative interface which works with both
MinTTY and other consoles.")
    (license license:bsd-3)))

(define-public ghc-edit-distance
  (package
    (name "ghc-edit-distance")
    (version "0.2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "edit-distance" version))
       (sha256
        (base32 "0jkca97zyv23yyilp3jydcrzxqhyk27swhzh82llvban5zp8b21y"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "edit-distance")))
    (inputs (list ghc-random))
    (native-inputs (list ghc-test-framework ghc-quickcheck
                         ghc-test-framework-quickcheck2))
    (arguments
     (list
      #:cabal-revision '("1"
                         "1vjn4ryzdilz7l1ad7czh11nw48h5mj8if7ij3q0mmc3sffa8csd")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'relax-dependencies
            (lambda _
              (substitute* "edit-distance.cabal"
                (("QuickCheck [ <>=^*&|0-9.]*") "QuickCheck < 2.16")))))))
    (home-page "http://github.com/phadej/edit-distance")
    (synopsis "Levenshtein and restricted Damerau-Levenshtein edit distances")
    (description
     "This package provides optimized functions to determine the edit
distances for fuzzy matching, including Levenshtein and restricted
Damerau-Levenshtein algorithms.")
    (license license:bsd-3)))

(define-public ghc-edit-distance-vector
  (package
    (name "ghc-edit-distance-vector")
    (version "1.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "edit-distance-vector" version))
       (sha256
        (base32 "07qgc8dyi9kkzkd3xcd78wdlljy0xwhz65b4r2qg2piidpcdvpxp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "edit-distance-vector")))
    (inputs (list ghc-vector))
    (native-inputs (list ghc-quickcheck ghc-quickcheck-instances))
    (home-page "https://github.com/thsutton/edit-distance-vector")
    (synopsis "Calculate edit distances and edit scripts between vectors")
    (description
     "This package provides implementation of the
Wagner-Fischer dynamic programming algorithm to find the optimal edit
script and cost between two sequences.  The implementation in this
package is specialised to sequences represented with @code{Data.Vector}
but is otherwise agnostic to:
@itemize
@item The type of values in the vectors;
@item The type representing edit operations; and
@item The type representing the cost of operations.
@end itemize")
    (license license:bsd-3)) )

(define-public ghc-either
  (package
    (name "ghc-either")
    (version "5.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "either" version))
       (sha256
        (base32 "00a8h2jgrpqdlsi8vjrm2qa6rmw33ksirxv9s6i90nlmhhg6jrkd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "either")))
    (inputs (list ghc-bifunctors ghc-profunctors ghc-semigroupoids))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck ghc-quickcheck))
    (home-page "http://github.com/ekmett/either/")
    (synopsis "Provides an either monad transformer for Haskell")
    (description "This Haskell package provides an either monad transformer.")
    (license license:bsd-3)))

(define-public ghc-email-validate
  (package
    (name "ghc-email-validate")
    (version "2.3.2.21")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "email-validate" version))
       (sha256
        (base32 "132ijz65csl1ki32nhw3d95x9vzl3lc22z2zhivv1yqh30lfpfgq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "email-validate")))
    (inputs (list ghc-attoparsec))
    (native-inputs (list ghc-hspec ghc-quickcheck))
    (home-page "https://github.com/Porges/email-validate-hs")
    (synopsis "Email address validator for Haskell")
    (description
     "This Haskell package provides a validator that can validate an email
address string against RFC 5322.")
    (license license:bsd-3)))

(define-public ghc-enclosed-exceptions
  (package
    (name "ghc-enclosed-exceptions")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "enclosed-exceptions" version))
       (sha256
        (base32 "1fghjj7nkiddrf03ks8brjpr5x25yi9fs7xg6adbi4mc2gqr6vdg"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "enclosed-exceptions")))
    (inputs (list ghc-lifted-base ghc-monad-control ghc-transformers-base))
    (native-inputs (list ghc-async ghc-hspec ghc-quickcheck))
    (home-page "https://github.com/jcristovao/enclosed-exceptions")
    (synopsis "Catch all exceptions from within an enclosed computation")
    (description
     "This library implements a technique to catch all exceptions raised
within an enclosed computation, while remaining responsive to (external)
asynchronous exceptions.")
    (license license:expat)))

(define-public ghc-encoding
  (package
    (name "ghc-encoding")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "encoding" version))
       (sha256
        (base32 "07fclvjvd25cb21j6hakkcrk83f46ri9mkc9qdgllhb5p9zwxdig"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "encoding")))
    (inputs (list ghc-extensible-exceptions ghc-regex-compat))
    (native-inputs (list ghc-hunit ghc-quickcheck))
    (home-page "https://github.com/dmwit/encoding")
    (synopsis "A library for various character encodings")
    (description
     "Haskell has excellect handling of unicode, the Char type covers all unicode
chars.  Unfortunately, there's no possibility to read or write something to the
outer world in an encoding other than ascii due to the lack of support for
encodings.  This library should help with that.")
    (license license:bsd-3)))

(define-public ghc-equivalence
  (package
    (name "ghc-equivalence")
    (version "0.4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "equivalence" version))
       (sha256
        (base32 "1wib20n367x3rjwgpr13jim535karbq155x6mwysqfhmr4fwcvcj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "equivalence")))
    (inputs (list ghc-stmonadtrans))
    (native-inputs (list ghc-quickcheck))
    (home-page "https://github.com/pa-ba/equivalence")
    (synopsis "Maintaining an equivalence relation implemented as union-find")
    (description
     "This is an implementation of Tarjan's Union-Find algorithm (Robert E.@:
Tarjan.  \"Efficiency of a Good But Not Linear Set Union Algorithm\",JACM
22(2), 1975) in order to maintain an equivalence relation.  This
implementation is a port of the @code{union-find} package using the @code{ST}
monad transformer (instead of the IO monad).")
    (license license:bsd-3)))

(define-public ghc-erf
  (package
    (name "ghc-erf")
    (version "2.0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "erf" version))
       (sha256
        (base32 "0dxk2r32ajmmc05vaxcp0yw6vgv4lkbmh8jcshncn98xgsfbgw14"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "erf")))
    (home-page "http://hackage.haskell.org/package/erf")
    (synopsis "The error function, erf, and related functions for Haskell")
    (description
     "This Haskell library provides a type class for the
error function, erf, and related functions.  Instances for Float and
Double.")
    (license license:bsd-3)))

(define-public ghc-errors
  (package
    (name "ghc-errors")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "errors" version))
       (sha256
        (base32 "0x8znwn31qcx6kqx99wp7bc86kckfb39ncz3zxvj1s07kxlfawk7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "errors")))
    (inputs (list ghc-transformers-compat ghc-safe))
    (arguments
     `(#:cabal-revision ("5"
                         "0ljfsadhqbqjivrr08x11zazpl115902ikvyhxq6nmg6zp54w7al")))
    (home-page "http://hackage.haskell.org/package/errors")
    (synopsis "Error handling library for Haskell")
    (description
     "This library encourages an error-handling style that
directly uses the type system, rather than out-of-band exceptions.")
    (license license:bsd-3)))

(define-public ghc-esqueleto
  (package
    (name "ghc-esqueleto")
    (version "3.6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "esqueleto" version))
       (sha256
        (base32 "1x7bd4l7ax6d47mhkbfdqsfjay0qn362is393cnbbm0395v2ijlh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "esqueleto")))
    (inputs (list ghc-aeson
                  ghc-attoparsec
                  ghc-blaze-html
                  ghc-conduit
                  ghc-monad-logger
                  ghc-persistent
                  ghc-resourcet
                  ghc-tagged
                  ghc-unliftio
                  ghc-unordered-containers))
    (native-inputs (list ghc-hspec
                         ghc-hspec-core
                         ghc-mysql
                         ghc-mysql-simple
                         ghc-persistent-mysql
                         ghc-persistent-postgresql
                         ghc-persistent-sqlite
                         ghc-postgresql-simple
                         ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1xfigma6ha5rhll6cg05iz6lzh88w3pgniarib7fzvi6rxyg3a17")
       #:tests? #f)) ; tests require a running postgresql server
    (home-page "https://github.com/bitemyapp/esqueleto")
    (synopsis "Type-safe embedded domain specific language for SQL queries")
    (description
     "This library provides a type-safe embedded domain specific
language (EDSL) for SQL queries that works with SQL backends as provided by
@code{ghc-persistent}.  Its language closely resembles SQL, so you don't have
to learn new concepts, just new syntax, and it's fairly easy to predict the
generated SQL and optimize it for your backend.")
    (license license:bsd-3)))

(define-public ghc-exactprint
  (package
    (name "ghc-exactprint")
    (version "1.10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ghc-exactprint" version))
       (sha256
        (base32 "14jwkx0q2bidcv5ss7zmkvl41z264k7siy9fh7py27h7azb539v0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ghc-exactprint")))
    (inputs (list ghc-syb))
    (native-inputs (list ghc-hunit ghc-diff ghc-extra ghc-paths ghc-silently))
    (home-page "http://hackage.haskell.org/package/ghc-exactprint")
    (synopsis "ExactPrint for GHC")
    (description
     "Using the API Annotations available from GHC 7.10.2, this library
provides a means to round-trip any code that can be compiled by GHC, currently
excluding @file{.lhs} files.")
    (license license:bsd-3)))

(define-public ghc-exceptions
  (package
    (name "ghc-exceptions")
    (version "0.10.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "exceptions" version))
       (sha256
        (base32 "0h5y2rqg7kz4ic59n5i7619766mzfpqcdill3l712nihs3q2nk4v"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "exceptions")))
    (native-inputs (list ghc-test-framework ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2 ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("3"
                         "0qv4z7p78ld8kz11xppkbax288y0admpidldxxc9bk6k5yfcn43a")))
    (home-page "http://github.com/ekmett/exceptions/")
    (synopsis "Extensible optionally-pure exceptions")
    (description "This library provides extensible optionally-pure exceptions
for Haskell.")
    (license license:bsd-3)))

(define-public ghc-executable-path
  (package
    (name "ghc-executable-path")
    (version "0.0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "executable-path" version))
       (sha256
        (base32 "0vxwmnsvx13cawcyhbyljkds0l1vr996ijldycx7nj0asjv45iww"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "executable-path")))
    (home-page "http://code.haskell.org/~bkomuves/")
    (synopsis "Find out the full path of the executable")
    (description
     "The documentation of @code{System.Environment.getProgName} says that
\"However, this is hard-to-impossible to implement on some non-Unix OSes, so
instead, for maximum portability, we just return the leafname of the program
as invoked.\" This library tries to provide the missing path.")
    (license license:public-domain)))

(define-public ghc-extensible-exceptions
  (package
    (name "ghc-extensible-exceptions")
    (version "0.1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "extensible-exceptions" version))
       (sha256
        (base32 "1273nqws9ij1rp1bsq5jc7k2jxpqa0svawdbim05lf302y0firbc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "extensible-exceptions")))
    (home-page "http://hackage.haskell.org/package/extensible-exceptions")
    (synopsis "Extensible exceptions for Haskell")
    (description
     "This package provides extensible exceptions for both new and old
versions of GHC (i.e., < 6.10).")
    (license license:bsd-3)))

(define-public ghc-extra
  (package
    (name "ghc-extra")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "extra" version))
       (sha256
        (base32 "18c9ad7wjf6q4yp0sagxhwyjpm9frw9kk27ih2x0nmjhmrgcx91g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "extra")))
    (inputs (list ghc-clock-bootstrap))
    ;; testsuite depends indirectly on ghc-extra via many packages
    (arguments (list #:tests? #f))
    (home-page "https://github.com/ndmitchell/extra#readme")
    (synopsis "Extra Haskell functions")
    (description
     "This library provides extra functions for the standard
Haskell libraries.  Most functions are simple additions, filling out missing
functionality.  A few functions are available in later versions of GHC, but
this package makes them available back to GHC 7.2.")
    (license license:bsd-3)))

(define-public ghc-fail
  (package
    (name "ghc-fail")
    (version "4.9.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "fail" version))
       (sha256
        (base32 "18nlj6xvnggy61gwbyrpmvbdkq928wv0wx2zcsljb52kbhddnp3d"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "fail")))
    (home-page "https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail")
    (synopsis "Forward-compatible MonadFail class")
    (description
     "This package contains the @code{Control.Monad.Fail} module providing the
@uref{https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail, MonadFail}
class that became available in
@uref{https://hackage.haskell.org/package/base-4.9.0.0, base-4.9.0.0} for
older @code{base} package versions.  This package turns into an empty package
when used with GHC versions which already provide the
@code{Control.Monad.Fail} module.")
    (license license:bsd-3)))

(define-public ghc-fast-logger
  (package
    (name "ghc-fast-logger")
    (version "3.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "fast-logger" version))
       (sha256
        (base32 "1hy5cczg64q6cafahfcfjsij48w80zskgjnn3ks0w5w4vqiccrmx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "fast-logger")))
    (inputs (list ghc-auto-update ghc-easy-file ghc-unix-time ghc-unix-compat
                  ghc-bytestring-builder))
    (native-inputs (list ghc-async ghc-hspec hspec-discover))
    (home-page "https://github.com/kazu-yamamoto/logger")
    (synopsis "Fast logging system")
    (description "This library provides a fast logging system for Haskell.")
    (license license:bsd-3)))

(define-public ghc-fdo-notify
  (package
    (name "ghc-fdo-notify")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "fdo-notify" version))
       (sha256
        (base32 "1n4zk1i7g34w0wk5zy8n4r63xbglxf62h8j78kv5fc2yn95l30vh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "fdo-notify")))
    (inputs (list ghc-dbus))
    (home-page "http://bitbucket.org/taejo/fdo-notify/")
    (synopsis "Desktop Notifications client")
    (description
     "This package provides a library for issuing notifications using
@code{FreeDesktop.org's} Desktop Notifications protocol.  This protocol is
supported by services such as Ubuntu's @code{NotifyOSD}.")
    (license license:bsd-3)))

(define-public ghc-feed
  (package
    (name "ghc-feed")
    (version "1.3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "feed" version))
       (sha256
        (base32 "0marh7qmggq1z5339nid3gil7k786d3yk79b0rwfkxxaxmr41xd8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "feed")))
    (inputs (list ghc-base-compat
                  ghc-old-locale
                  ghc-old-time
                  ghc-safe
                  ghc-time-locale-compat
                  ghc-utf8-string
                  ghc-xml-types
                  ghc-xml-conduit))
    (arguments
     `(#:cabal-revision ("5"
                         "0y9f6dcgmmfzgxq9dbgs6lypd6pmcb0x1qvvkj20l74ba9k30v96")
       #:tests? #f)) ; tests fail to compile with current xml-conduit
    (home-page "https://github.com/haskell-party/feed")
    (synopsis "Haskell package for handling various syndication formats")
    (description
     "This Haskell package includes tools for generating and
consuming feeds in both RSS (Really Simple Syndication) and Atom format.")
    (license license:bsd-3)))

(define-public ghc-fgl
  (package
    (name "ghc-fgl")
    (version "5.8.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "fgl" version))
       (sha256
        (base32 "1hb3mgqqz67qwfw2893bslj4mkhs4g0y51c6zpc6r2h6caqibjm4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "fgl")))
    (native-inputs (list ghc-quickcheck ghc-hspec))
    (arguments
     `(#:cabal-revision ("1"
                         "13yn7h8rwmdjwscli9cpn44dp5pm2c0km7b3v1cmfq4na16pczsh")))
    (home-page "http://hackage.haskell.org/package/fgl")
    (synopsis "Martin Erwig's Functional Graph Library")
    (description
     "The functional graph library, FGL, is a collection of type
and function definitions to address graph problems.  The basis of the library
is an inductive definition of graphs in the style of algebraic data types that
encourages inductive, recursive definitions of graph algorithms.")
    (license license:bsd-3)))

(define-public ghc-fgl-arbitrary
  (package
    (name "ghc-fgl-arbitrary")
    (version "0.2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "fgl-arbitrary" version))
       (sha256
        (base32
         "1mykbd1r43gpsn10ys8q3nr0i4wnhn6wq23hcici18mxxji11wkc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "fgl-arbitrary")))
    (inputs
     (list ghc-fgl ghc-quickcheck ghc-hspec))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "fgl-arbitrary.cabal"
               (("hspec >= 2\\.1 && < 2\\.8") "hspec")
               (("QuickCheck >= 2.3 && < 2.15") "QuickCheck")))))))
    (home-page "https://hackage.haskell.org/package/fgl-arbitrary")
    (synopsis "QuickCheck support for fgl")
    (description
     "Provides Arbitrary instances for fgl graphs to avoid adding a
QuickCheck dependency for fgl whilst still making the instances
available to others.  Also available are non-fgl-specific functions
for generating graph-like data structures.")
    (license license:bsd-3)))

(define-public ghc-file-embed
  (package
    (name "ghc-file-embed")
    (version "0.0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "file-embed" version))
       (sha256
        (base32 "05glld1cy9yx2g1xlbkl4bpdf18j8l2kj5nxgiamaqwkzwp6f62z"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "file-embed")))
    (home-page "https://github.com/snoyberg/file-embed")
    (synopsis "Use Template Haskell to embed file contents directly")
    (description
     "This package allows you to use Template Haskell to read a file or all
the files in a directory, and turn them into @code{(path, bytestring)} pairs
embedded in your Haskell code.")
    (license license:bsd-2)))

(define-public ghc-filemanip
  (package
    (name "ghc-filemanip")
    (version "0.3.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "filemanip" version))
       (sha256
        (base32 "0ilqr8jv41zxcj5qyicg29m8s30b9v70x6f9h2h2rw5ap8bxldl8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "filemanip")))
    (inputs (list ghc-unix-compat))
    (arguments
     `(#:cabal-revision ("1"
                         "1l53qqlh9w7572n5dxk8rq0p8vsvg6m1afbak6xzdx0kgg8j6y8a")))
    (home-page "https://github.com/bos/filemanip")
    (synopsis "File and directory manipulation for Haskell")
    (description
     "This package provides a Haskell library for working with files and
directories.  It includes code for pattern matching, finding files, modifying
file contents, and more.")
    (license license:bsd-3)))

;; Deprecated.
(define-public ghc-filepath-bytestring
  (package
    (name "ghc-filepath-bytestring")
    (version "1.5.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "filepath-bytestring" version))
       (sha256
        (base32 "0jz6mpr6fvxijvkib9500x25np7j836wrvy8jd23l16r4rhnhmxy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "filepath-bytestring")))
    (native-inputs (list ghc-quickcheck))
    (home-page "http://hackage.haskell.org/package/filepath-bytestring")
    (synopsis "Library for manipulating RawFilePaths in a cross-platform way")
    (description
     "This package provides a drop-in replacement for the standard
@code{filepath} library, operating on @code{RawFilePath} values rather than
@code{FilePath} values to get the speed benefits of using @code{ByteStrings}.")
    (license license:bsd-3)))

(define-public ghc-findbin
  (package
    (name "ghc-findbin")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "FindBin" version))
       (sha256
        (base32 "197xvn05yysmibm1p5wzxfa256lvpbknr5d1l2ws6g40w1kpk717"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "FindBin")))
    (home-page "https://github.com/audreyt/findbin")
    (synopsis "Get the absolute path of the running program")
    (description
     "This module locates the full directory of the running program, to allow
the use of paths relative to it.  FindBin supports invocation of Haskell
programs via \"ghci\", via \"runhaskell/runghc\", as well as compiled as
an executable.")
    (license license:bsd-3)))

(define-public ghc-fingertree
  (package
    (name "ghc-fingertree")
    (version "0.1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "fingertree" version))
       (sha256
        (base32 "1aww2c2alnkaaigh0xx2cvx6s8qddzlfy1xcwf0fddnf9p2psqgj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "fingertree")))
    (native-inputs (list ghc-hunit ghc-quickcheck ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2))
    (home-page "http://hackage.haskell.org/package/fingertree")
    (synopsis "Generic finger-tree structure")
    (description
     "This library provides finger trees, a general sequence
representation with arbitrary annotations, for use as a base for
implementations of various collection types.  It includes examples, as
described in section 4 of Ralf Hinze and Ross Paterson, \"Finger trees: a
simple general-purpose data structure\".")
    (license license:bsd-3)))

(define-public ghc-finite-typelits
  (package
    (name "ghc-finite-typelits")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "finite-typelits" version))
       (sha256
        (base32 "0i786r2l3k9fxkpyy6rsi8my6kkar7y8yxk7h9gncm0z6kmvrnvk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "finite-typelits")))
    (inputs (list ghc-tagged))
    (native-inputs (list ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1967xzvdhlk92ifn980pm05jxjnmra32wlfzx7l2p2gn1cydzlhv")))
    (home-page "https://github.com/mniip/finite-typelits")
    (synopsis "Finitely many values, indexed by type-level naturals")
    (description
     "This package provides a Haskell type inhabited by finitely many values
and indexed by type-level naturals.")
    (license license:bsd-3)))

(define-public ghc-fixed
  (package
    (name "ghc-fixed")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "fixed" version))
       (sha256
        (base32 "10l2sh179xarb774q92cff2gkb20rsrlilfwp1fk61rzmz9yn64j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "fixed")))
    (home-page "http://github.com/ekmett/fixed")
    (synopsis "Signed 15.16 precision fixed point arithmetic")
    (description
     "This package provides functions for signed 15.16 precision fixed point
arithmetic.")
    (license license:bsd-3)))

(define-public ghc-fmlist
  (package
    (name "ghc-fmlist")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "fmlist" version))
       (sha256
        (base32 "19h95ph7lh7llw6j1v1rssrdi5k7xw8x0iac9rgzss371s2w3g9d"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "fmlist")))
    (inputs (list ghc-fail))
    (home-page "https://github.com/sjoerdvisscher/fmlist")
    (synopsis "FoldMap lists")
    (description
     "FoldMap lists are lists represented by their
@code{foldMap} function.  FoldMap lists have @math{O(1)} cons, snoc and
append, just like DLists, but other operations might have favorable
performance characteristics as well.  These wild claims are still
completely unverified though.")
    (license license:bsd-3)))

(define-public ghc-foldl
  (package
    (name "ghc-foldl")
    (version "1.4.18")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "foldl" version))
       (sha256
        (base32 "03jhj5p017r6f75vb0dk6igcaklgykjpp23j5wk7blzph18z9n6a"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "foldl")))
    (inputs (list ghc-random
                  ghc-primitive
                  ghc-vector
                  ghc-unordered-containers
                  ghc-hashable
                  ghc-contravariant
                  ghc-profunctors
                  ghc-semigroupoids
                  ghc-comonad
                  ghc-semigroups))
    (native-inputs (list ghc-doctest))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (if tests?
                 (let* ((tmpdir (or (getenv "TMP")
                                    "/tmp"))
                        (package-db
                         (string-append tmpdir "/package.conf.d")))
                   (setenv "GHC_PACKAGE_PATH" package-db)
                   (invoke "./dist/build/doctest/doctest")
                   (unsetenv "GHC_PACKAGE_PATH"))))))
       #:cabal-revision '("1"
                          "1isis41wbfpgdqgrhapkcqqrkd20kxxl8qcqyqmrr41pvgca0hma")))
    (home-page "http://hackage.haskell.org/package/foldl")
    (synopsis "Composable, streaming, and efficient left folds for Haskell")
    (description
     "This Haskell library provides strict left folds that stream
in constant memory, and you can combine folds using @code{Applicative} style
to derive new folds.  Derived folds still traverse the container just once
and are often as efficient as hand-written folds.")
    (license license:bsd-3)))

(define-public ghc-foundation
  (package
    (name "ghc-foundation")
    (version "0.0.30")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "foundation" version))
       (sha256
        (base32 "11hdqd01ggdr7fjw3w00giay06bzz97qqiiq60vi1l1dzz1wrwzn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "foundation")))
    (inputs (list ghc-basement))
    (home-page "https://github.com/haskell-foundation/foundation")
    (synopsis "Alternative prelude with batteries and no dependencies")
    (description
     "This package provides a custom prelude with no dependencies apart from
the base package.

Foundation has the following goals:

@enumerate
@item provide a base like sets of modules that provide a consistent set of
   features and bugfixes across multiple versions of GHC (unlike base).
@item provide a better and more efficient prelude than base's prelude.
@item be self-sufficient: no external dependencies apart from base;
@item provide better data-types: packed unicode string by default, arrays;
@item Numerical classes that better represent mathematical things (no more
   all-in-one @code{Num});
@item I/O system with less lazy IO.
@end enumerate
")
    (license license:bsd-3)))

(define-public ghc-free
  (package
    (name "ghc-free")
    (version "5.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "free" version))
       (sha256
        (base32 "12agp68cwwixcwfwnvk2xamg34a2x6ax7s1naxv66chpi5y7z1kj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "free")))
    (inputs (list ghc-comonad
                  ghc-distributive
                  ghc-indexed-traversable
                  ghc-profunctors
                  ghc-semigroupoids
                  ghc-th-abstraction
                  ghc-transformers-base))
    (arguments
     `(#:cabal-revision ("7"
                         "0h43xp4f38bpxhs5s06x1jw6d6zv55hhyhj6cmdbmfw7d6k94fbz")))
    (home-page "http://github.com/ekmett/free/")
    (synopsis "Unrestricted monads for Haskell")
    (description
     "This library provides free monads, which are useful for many
tree-like structures and domain specific languages.  If @code{f} is a
@code{Functor} then the free @code{Monad} on @code{f} is the type of trees
whose nodes are labeled with the constructors of @code{f}.  The word \"free\"
is used in the sense of \"unrestricted\" rather than \"zero-cost\": @code{Free
f} makes no constraining assumptions beyond those given by @code{f} and the
definition of @code{Monad}.")
    (license license:bsd-3)))

(define-public ghc-fsnotify
  (package
    (name "ghc-fsnotify")
    (version "0.4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "fsnotify" version))
       (sha256
        (base32 "06v3yb8vpvk43qb0r0063q1rr1rf2c0l9plhs6pm1gzhwbcszcc2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "fsnotify")))
    (inputs (list ghc-async
                  ghc-monad-control
                  ghc-safe-exceptions
                  ghc-unix-compat
                  ghc-hinotify
                  ghc-monad-logger
                  ghc-random
                  ghc-retry
                  ghc-string-interpolate
                  ghc-temporary
                  ghc-unliftio))
    (native-inputs (list ghc-sandwich))
    (home-page "https://github.com/haskell-fswatch/hfsnotify")
    (synopsis "Cross platform library for file change notification")
    (description
     "Cross platform library for file creation, modification, and
deletion notification.  This library builds upon existing libraries for platform
specific Windows, Mac, and Linux file system event notification.")
    (license license:bsd-3)))

(define-public ghc-generically
  (package
    (name "ghc-generically")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "generically" version))
       (sha256
        (base32 "1ks3pi6mpma83xffplz8vmimyhvzpnhmcgvk3bvl3c64pqva9i84"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "generically")))
    (arguments
     `(#:cabal-revision ("4"
                         "0bj5vhnrggf9ka89z628hcxx1g3r00lv2rrrjv3dfbaqbj7jfr1z")))
    (home-page "http://hackage.haskell.org/package/generically")
    (synopsis "Generically newtype to use with DerivingVia")
    (description
     "This is a compatibility package as @@Generically@@ and @@Generically1@@ newtypes
are available since @@base-4.17@@ in GHC.Generics'.")
    (license license:bsd-3)))

(define-public ghc-generic-deriving
  (package
    (name "ghc-generic-deriving")
    (version "1.14.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "generic-deriving" version))
       (sha256
        (base32 "1bxjar1kc29nma3whxb0kqgjgxmmm7wvhql7pyick8rj39zw35gi"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "generic-deriving")))
    (inputs (list ghc-th-abstraction))
    (native-inputs (list ghc-hspec hspec-discover))
    (arguments
     `(#:cabal-revision ("1"
                         "0mdcj86qy4qnjlx4nh3bi4nrplmycjci7mns4zp0w3ipj0fhfz7l")))
    (home-page "https://github.com/dreixel/generic-deriving")
    (synopsis "Generalise the deriving mechanism to arbitrary classes")
    (description "This package provides functionality for generalising the
deriving mechanism in Haskell to arbitrary classes.")
    (license license:bsd-3)))

(define-public ghc-generic-random
  (package
    (name "ghc-generic-random")
    (version "1.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "generic-random" version))
       (sha256
        (base32 "02iczjf2xc4sxfi234nf6irfj5slvf3p5hpaxl8r5nc8hy052d6x"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "generic-random")))
    (inputs (list ghc-quickcheck))
    (native-inputs (list ghc-inspection-testing ghc-inspection-testing))
    (home-page "http://github.com/lysxia/generic-random")
    (synopsis "Generic random generators for QuickCheck")
    (description
     "Derive instances of @code{Arbitrary} for QuickCheck, with various options
to customize implementations.

Automating the arbitrary boilerplate also ensures that when a type changes to
have more or fewer constructors, then the generator either fixes itself to
generate that new case (when using the uniform distribution) or causes a
compilation error so you remember to fix it (when using an explicit
distribution).

This package also offers a simple (optional) strategy to ensure termination
for recursive types: make @code{Test.QuickCheck.Gen}'s size parameter decrease
at every recursive call; when it reaches zero, sample directly from a
trivially terminating generator given explicitly (@code{genericArbitraryRec}
and @code{withBaseCase}) or implicitly (@code{genericArbitrary'}).")
    (license license:expat)))

(define-public ghc-generics-sop
  (package
    (name "ghc-generics-sop")
    (version "0.5.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "generics-sop" version))
       (sha256
        (base32 "0zkri1w7qdqlxcfx0kzld7ai5g7xzxwxjxjfa7wnjx09fqhiqsk1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "generics-sop")))
    (inputs (list ghc-sop-core ghc-th-abstraction))
    (arguments
     `(#:cabal-revision ("2"
                         "0xacmrv5jscb2jxqpvrnrd8cq80zk2ww6f3ajkdl9y4nl2h68a56")))
    (home-page "http://hackage.haskell.org/package/generics-sop")
    (synopsis "Generic Programming using True Sums of Products for Haskell")
    (description
     "This Haskell package supports the definition of generic
functions.  Datatypes are viewed in a uniform, structured way: the choice
between constructors is represented using an n-ary sum, and the arguments of
each constructor are represented using an n-ary product.")
    (license license:bsd-3)))

(define-public ghc-genvalidity
  (package
    (name "ghc-genvalidity")
    (version "1.1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "genvalidity" version))
       (sha256
        (base32 "0l3xprs2gbf9xcgmm5813rbprway8p2qwxnqnxwb53snxfms8c0f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "genvalidity")))
    (inputs (list ghc-quickcheck ghc-random ghc-validity))
    (native-inputs (list ghc-hspec ghc-hspec-core hspec-discover))
    (home-page "https://github.com/NorfairKing/validity#readme")
    (synopsis "Testing utilities for the @code{validity} library")
    (description
     "This package provides testing utilities that are useful in conjunction
with the @code{Validity} typeclass.")
    (license license:expat)))

(define-public ghc-genvalidity-property
  (package
    (name "ghc-genvalidity-property")
    (version "1.0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "genvalidity-property" version))
       (sha256
        (base32 "1nxcdq04rkckrb3v49pjx378n5s828k24x7hix6manyxqmd3hplw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "genvalidity-property")))
    (inputs (list ghc-quickcheck ghc-genvalidity ghc-hspec ghc-pretty-show
                  ghc-validity))
    (native-inputs (list hspec-discover))
    (home-page "https://github.com/NorfairKing/validity#readme")
    (synopsis "Standard properties for functions on @code{Validity} types")
    (description
     "This package supplements the @code{Validity} typeclass with standard
properties for functions operating on them.")
    (license license:expat)))

(define-public ghc-getopt-generics
  (package
    (name "ghc-getopt-generics")
    (version "0.13.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "getopt-generics" version))
       (sha256
        (base32 "00xswyi9y49qab2fpkdx7isx40kfa93p3gfransivzgg9m3si37d"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "getopt-generics")))
    (inputs (list ghc-base-compat ghc-base-orphans ghc-generics-sop ghc-tagged))
    (native-inputs (list ghc-quickcheck ghc-hspec hspec-discover ghc-safe ghc-silently))
    (home-page "https://github.com/soenkehahn/getopt-generics#readme")
    (synopsis "Create command line interfaces with ease")
    (description "This library provides tools to create command line
interfaces with ease.")
    (license license:bsd-3)))

(define-public ghc-gitrev
  (package
    (name "ghc-gitrev")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "gitrev" version))
       (sha256
        (base32 "0cl3lfm6k1h8fxp2vxa6ihfp4v8igkz9h35iwyq2frzm4kdn96d8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "gitrev")))
    (inputs (list ghc-base-compat))
    (home-page "https://github.com/acfoltzer/gitrev")
    (synopsis "Compile git revision info into Haskell projects")
    (description
     "This package provides some handy Template Haskell splices for including
the current git hash and branch in the code of your project.  This is useful
for including in panic messages, @command{--version} output, or diagnostic
info for more informative bug reports.")
    (license license:bsd-3)))

(define-public ghc-glob
  (package
    (name "ghc-glob")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Glob" version))
       (sha256
        (base32
         "1h3kh46qds4nqvixm4myy1kb5slg53f44hfn8aymrlr7hjn75xka"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Glob")))
    (arguments
     `(#:cabal-revision
       ("3" "1080rd5073g87rfm5whimb72b75105lqanybrbsfi14gmvndnbfx")))
    (inputs
     (list ghc-dlist ghc-semigroups-bootstrap ghc-transformers-compat))
    (native-inputs
     (list ghc-hunit ghc-quickcheck ghc-test-framework
           ghc-test-framework-hunit ghc-test-framework-quickcheck2))
    (home-page "https://iki.fi/matti.niemenmaa/glob/")
    (synopsis "Haskell library matching glob patterns against file paths")
    (description "This package provides a Haskell library for @dfn{globbing}:
matching patterns against file paths.")
    (license license:bsd-3)))

(define-public ghc-gluraw
  (package
    (name "ghc-gluraw")
    (version "2.0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "GLURaw" version))
       (sha256
        (base32 "1b3rnva77k9naw5bl573bqgmsq7n9i8rrrvfvhbjcndqgmzhkini"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "GLURaw")))
    (inputs (list glu ghc-openglraw))
    (home-page "http://www.haskell.org/haskellwiki/Opengl")
    (synopsis "Raw Haskell bindings GLU")
    (description
     "GLURaw is a raw Haskell binding for the GLU 1.3 OpenGL
utility library.  It is basically a 1:1 mapping of GLU's C API, intended as a
basis for a nicer interface.")
    (license license:bsd-3)))

(define-public ghc-glut
  (package
    (name "ghc-glut")
    (version "2.7.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "GLUT" version))
       (sha256
        (base32 "0vdkfj4wjzigdpzgr5l001y9wkhwgl00mclr26gf93kps14fkymn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "GLUT")))
    (inputs (list ghc-statevar ghc-opengl ghc-openglraw ghc-random))
    (arguments
     `(#:cabal-revision ("4"
                         "0xfm8l90llyyqj3aip0i87ykq936zfl3bc7sibgdng8dhi5xj7y3")))
    (home-page "http://www.haskell.org/haskellwiki/Opengl")
    (synopsis "Haskell bindings for the OpenGL Utility Toolkit")
    (description
     "This library provides Haskell bindings for the OpenGL
Utility Toolkit, a window system-independent toolkit for writing OpenGL
programs.")
    (license license:bsd-3)))

(define-public ghc-gnuplot
  (package
    (name "ghc-gnuplot")
    (version "0.5.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "gnuplot" version))
       (sha256
        (base32 "1glahh3si5bpazsklnpwxx4h4ivgb4wyngc032797zq1496fhhm3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "gnuplot")))
    (inputs (list ghc-temporary ghc-utility-ht ghc-data-accessor-transformers
                  ghc-data-accessor ghc-semigroups))
    (arguments
     `(#:cabal-revision ("3"
                         "1rwlkr94h6lzp6aa3p4i5dgdl1i9nl7mahxny2qz9ggjbj4yiw65")))
    (home-page "http://www.haskell.org/haskellwiki/Gnuplot")
    (synopsis "2D and 3D plots using gnuplot")
    (description "This package provides a Haskell module for creating 2D and
3D plots using gnuplot.")
    (license license:bsd-3)))

(define-public ghc-graphviz
  (package
    (name "ghc-graphviz")
    (version "2999.20.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "graphviz" version))
       (sha256
        (base32 "10mksxlg0pn82z6sfz8xnnmhn03ddi4cml9fh3wlgph1ksjhzz4g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "graphviz")))
    (inputs (list ghc-temporary
                  ghc-fgl
                  ghc-polyparse
                  ghc-colour
                  ghc-wl-pprint-text
                  ghc-dlist))
    (native-inputs (list ghc-fgl-arbitrary ghc-hspec ghc-quickcheck))
    (home-page "http://hackage.haskell.org/package/graphviz")
    (synopsis "Bindings to Graphviz for graph visualisation")
    (description
     "This library provides bindings for the Dot language used by
the @uref{https://graphviz.org/, Graphviz} suite of programs for
visualising graphs, as well as functions to call those programs.
Main features of the graphviz library include:

@enumerate
@item Almost complete coverage of all Graphviz attributes and syntax
@item Support for specifying clusters
@item The ability to use a custom node type
@item Functions for running a Graphviz layout tool with all specified output types
@item Generate and parse Dot code with two options: strict and liberal
@item Functions to convert FGL graphs and other graph-like data structures
@item Round-trip support for passing an FGL graph through Graphviz to augment node
and edge labels with positional information, etc.
@end enumerate
")
    (license license:bsd-3)))

(define-public ghc-groups
  (package
    (name "ghc-groups")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "groups" version))
       (sha256
        (base32 "0f5c8dg9b74glfw2sdvdcl9c8igs6knz1bayk4gvvzvypsl547nf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "groups")))
    (home-page "http://hackage.haskell.org/package/groups")
    (synopsis "Haskell 98 groups")
    (description "This package provides Haskell 98 groups.  A group is a
monoid with invertibility.")
    (license license:bsd-3)))

(define-public ghc-gtk2hs-buildtools
  (package
    (name "ghc-gtk2hs-buildtools")
    (version "0.13.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "gtk2hs-buildtools" version))
       (sha256
        (base32 "15g4y6i0w0grnl1vi2c51k10qzhdbnqqk2vp2p3pclx5n41g6213"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "gtk2hs-buildtools")))
    (inputs (list ghc-random ghc-hashtables))
    (native-inputs (list ghc-alex ghc-happy))
    (home-page "http://projects.haskell.org/gtk2hs/")
    (synopsis "Tools to build the Gtk2Hs suite of user interface libraries")
    (description
     "This package provides a set of helper programs necessary to build the
Gtk2Hs suite of libraries.  These tools include a modified c2hs binding tool
that is used to generate FFI declarations, a tool to build a type hierarchy
that mirrors the C type hierarchy of GObjects found in glib, and a generator
for signal declarations that are used to call back from C to Haskell.  These
tools are not needed to actually run Gtk2Hs programs.")
    (license license:gpl2)))

(define-public ghc-hackage-security
  (package
    (name "ghc-hackage-security")
    (version "0.6.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hackage-security" version))
       (sha256
        (base32 "05sckvvwj10krkhp1457mgp1hgq45p7r2sp850g3b5689i91mvqx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hackage-security")))
    (inputs (list ghc-base16-bytestring
                  ghc-base64-bytestring
                  ghc-cryptohash-sha256
                  ghc-ed25519
                  ghc-network-uri
                  ghc-network
                  ghc-tar
                  ghc-zlib
                  ghc-cabal-syntax))
    (native-inputs (list ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-quickcheck
                         ghc-aeson
                         ghc-vector
                         ghc-unordered-containers
                         ghc-temporary))
    (arguments
     `(#:cabal-revision ("1"
                         "1si6mkc8gimkpqkdl2wyzxp14v7yphp40hxvp77im7bhr8brsa77")))
    (home-page "https://github.com/haskell/hackage-security")
    (synopsis "Hackage security library")
    (description
     "This Hackage security library provides both server and
client utilities for securing @uref{https://hackage.haskell.org/, the
Hackage package server}.  It is based on
@uref{http://theupdateframework.com/, The Update Framework}, a set of
recommendations developed by security researchers at various universities
in the US as well as developers on the @uref{https://www.torproject.org/,
Tor project}.")
    (license license:bsd-3)))

(define-public ghc-haddock
  (package
    (name "ghc-haddock")
    (version "2.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haddock" version))
       (sha256
        (base32
         "0jqp37pbz4zjqc3dm0jkcsdqsh2ql9ygnr06m75bbk330yqchnl3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "haddock")))
    (arguments
     `(#:tests? #f ; TODO: haddock-test does not build.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'add-haddock-to-path
           (lambda _
             (setenv "PATH" (string-append (getcwd) "/dist/build/haddock"
                                           ":" (getenv "PATH")))
             #t)))))
    (inputs (list ghc-haddock-api))
;    (native-inputs
;     `(("ghc-haddock-test" ,ghc-haddock-test)
;       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis
     "Documentation-generation tool for Haskell libraries")
    (description
     "Haddock is a documentation-generation tool for Haskell libraries.")
    (license license:bsd-3)))

(define-public ghc-haddock-api
  (package
    (name "ghc-haddock-api")
    (version "2.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haddock-api" version))
       (sha256
        (base32
         "0ris5m61vig5nh5y2ddm98midl3v51vzgfgvsfyhm3nwk5hif6ay"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "haddock-api")))
    (inputs
     (list ghc-paths ghc-haddock-library))
    (native-inputs
     (list ghc-quickcheck ghc-hspec hspec-discover))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "haddock-api.cabal"
               (("haddock-library \\^>= 1\\.9\\.0") "haddock-library")
               (("hspec           \\^>= 2.8") "hspec")))))))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis "API for documentation-generation tool Haddock")
    (description "This package provides an API to Haddock, the
documentation-generation tool for Haskell libraries.")
    (license license:bsd-3)))

(define-public ghc-haddock-library
  (package
    (name "ghc-haddock-library")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haddock-library" version))
       (sha256
        (base32 "02m2pr1jyn0k86bjqksn2vrpyv0y40sj3rq5svcs5c3qlg4mw1vw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "haddock-library")))
    (native-inputs (list ghc-base-compat
                         ghc-quickcheck
                         ghc-hspec
                         hspec-discover
                         ghc-base-compat
                         ghc-optparse-applicative
                         ghc-tree-diff))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'configure 'relax-dependency-versions
             (lambda _
               (substitute* "haddock-library.cabal"
                 (("(base-compat|filepath) +[0-9<>=^.|& ]*" all package)
                  package)))))
       #:cabal-revision '("5"
                          "1gi861bwyizq164pl2ikqr3zmklifndizlr5hn1ly0zq58ram3yi")))
    (home-page "http://www.haskell.org/haddock/")
    (synopsis "Library exposing some functionality of Haddock")
    (description
     "Haddock is a documentation-generation tool for Haskell libraries.  These
modules expose some functionality of it without pulling in the GHC dependency.
Please note that the API is likely to change so specify upper bounds in your
project if you can't release often.  For interacting with Haddock itself, see
the ‘haddock’ package.")
    (license license:bsd-3)))

;; This package is needed for testing 'ghc-haddock'.  It is no longer
;; published to Hackage, but it is maintained in the Haddock Git
;; repository.
(define ghc-haddock-test
  (package
    (name "ghc-haddock-test")
    (version "2.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/haskell/haddock")
             (commit (string-append "haddock-" version "-release"))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ywxmqqan10gs0ppybdmdgsmvkzkpw7yirj2rw4qylg3x49a9zca"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "haddock-test"))))))
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-xml" ,ghc-xml)))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis "Test utilities for Haddock")
    (description "This package provides test utilities for Haddock.")
    (license license:bsd-3)
    (properties '((hidden? #t)))))

(define-public ghc-hakyll
  (package
    (name "ghc-hakyll")
    ;; XXX: Stackage LTS version is incompatible with our packaged
    ;; version of optparse-applicative and template-haskell.
    (version "4.17.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hakyll" version))
       (sha256
        (base32 "0gk7ar6iz6wrz9z7zibqjn1y65h9572cmlfhq38n86jsrrff4am0"))))
    (build-system haskell-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "tests/Hakyll/Core/UnixFilter/Tests.hs"
                (("unixFilter \"([^\"]+)\"" all cmd)
                 (string-append
                   "unixFilter "
                   "\""
                   (search-input-file inputs (string-append "/bin/" cmd))
                   "\""))))))))
    (properties '((upstream-name . "hakyll")))
    (inputs (list ghc-aeson
                  ghc-blaze-html
                  ghc-data-default
                  ghc-file-embed
                  ghc-hashable
                  ghc-lrucache
                  ghc-network-uri
                  ghc-optparse-applicative
                  ghc-random
                  ghc-regex-tdfa
                  ghc-resourcet
                  ghc-scientific
                  ghc-tagsoup
                  ghc-time-locale-compat
                  ghc-vector
                  ghc-wai-app-static
                  ghc-yaml
                  ghc-xml-conduit
                  ghc-wai
                  ghc-warp
                  ghc-http-types
                  ghc-fsnotify
                  ghc-http-conduit
                  ghc-pandoc
                  ghc-pandoc-types))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-golden
                         ghc-tasty-hunit ghc-tasty-quickcheck util-linux))
    (home-page "https://jaspervdj.be/hakyll/")
    (synopsis
     "This package provides a Haskell-based static website compiler library")
    (description
     "Hakyll is a static website compiler library.  That is, it provides you
with the tools to create a simple or advanced static website using a Haskell
@acronym{EDSL, Embedded Domain Specific Language} and input formats such as
Markdown or @acronym{RST, reStructuredText}.")
    (license license:bsd-3)))

(define-public ghc-half
  (package
    (name "ghc-half")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "half" version))
       (sha256
        (base32 "00mb2xfz0q8sq8zxqpw3ycp1p8gjhlgc0wxh5xr7kzyn52b08xpl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "half")))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-quickcheck))
    (home-page "http://github.com/ekmett/half")
    (synopsis "Half-precision floating-point computations")
    (description "This library provides a half-precision floating-point
computation library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-happy-lib
  (package
    (name "ghc-happy-lib")
    (version "2.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "happy-lib" version))
       (sha256
        (base32 "0sgj004khhy95xlw514s7pl8vk02fmssh3sn7kxgmdgjlg2b49gn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "happy-lib")))
    (home-page "https://www.haskell.org/happy/")
    (synopsis
     "Happy is a parser generator for Haskell implemented using this library")
    (description
     "Given a grammar specification in BNF, Happy generates Haskell code to
parse the grammar. Happy works in a similar way to the yacc tool for C.")
    (license license:bsd-2)))

(define-public ghc-happy
  (package
    (name "ghc-happy")
    (version "2.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "happy" version))
       (sha256
        (base32 "0aky5m9r3420h88ghq0qf9ck09g8ysr1aqlgb531vlc2n050yfcy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "happy")))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'check 'fix-testsuite
             (lambda _
               (setenv "HAPPY" (string-append (getcwd) "/dist/build/happy/happy")))))))
    (inputs (list ghc-happy-lib))
    (native-inputs (list which))
    (home-page "https://www.haskell.org/happy/")
    (synopsis "Parser generator for Haskell")
    (description
     "Happy is a parser generator for Haskell.  Given a grammar
specification in BNF, Happy generates Haskell code to parse the grammar.
Happy works in a similar way to the yacc tool for C.")
    (license license:bsd-2)))

(define-public ghc-hashable
  (package
    (name "ghc-hashable")
    (version "1.5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hashable" version))
       (sha256
        (base32 "0snvrnh9q4arjwk6hdvb4zwb22sh7rlgvr9px7bnqpys3273m2z5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hashable")))
    (native-inputs (list ghc-hunit
                         ghc-quickcheck
                         ghc-random
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-primitive
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1vsq3wv397lp208p7zs8fplxdix4jmv688dj4ych4983prn188rg")))
    (home-page "http://github.com/haskell-unordered-containers/hashable")
    (synopsis "Class for types that can be converted to a hash value")
    (description
     "This package defines a class, @code{Hashable}, for types that can be
converted to a hash value.  This class exists for the benefit of hashing-based
data structures.  The package provides instances for basic types and a way to
combine hash values.")
    (license license:bsd-3)))

(define-public ghc-hashable-bootstrap
  (package
    (inherit ghc-hashable)
    (name "ghc-hashable-bootstrap")
    (arguments
     `(#:tests? #f
       ,@(package-arguments ghc-hashable)))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-hashtables
  (package
    (name "ghc-hashtables")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hashtables" version))
       (sha256
        (base32 "03gga0iz3mg42wbjsydvk0cqkhzipyphf2ff8n7r233cxsgjj3na"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hashtables")))
    (inputs (list ghc-hashable ghc-primitive ghc-vector))
    (native-inputs (list ghc-mwc-random ghc-quickcheck ghc-tasty
                         ghc-tasty-quickcheck ghc-tasty-hunit))
    (home-page "http://github.com/gregorycollins/hashtables")
    (synopsis "Haskell Mutable hash tables in the ST monad")
    (description
     "This package provides a Haskell library including a
couple of different implementations of mutable hash tables in the ST
monad, as well as a typeclass abstracting their common operations, and
a set of wrappers to use the hash tables in the IO monad.")
    (license license:bsd-3)))

(define-public ghc-haskeline
  (package
    (name "ghc-haskeline")
    (version "0.8.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haskeline" version))
       (sha256
        (base32 "0kxdgy3s6pakb7yqk9jy747xv6n3plzwxnnzabqnyk83cs0d8n22"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "haskeline")))
    (native-inputs (list ghc-hunit which))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'fix-path
            (lambda _
              (setenv "PATH" (string-append "dist/build/haskeline-examples-Test:" (getenv "PATH"))))))))
    (home-page "https://github.com/judah/haskeline")
    (synopsis "Command-line interface for user input, written in Haskell")
    (description
     "Haskeline provides a user interface for line input in command-line
programs.  This library is similar in purpose to readline, but since it is
written in Haskell it is (hopefully) more easily used in other Haskell
programs.

Haskeline runs both on POSIX-compatible systems and on Windows.")
    (license license:bsd-3)))

(define-public ghc-haskell-lexer
  (package
    (name "ghc-haskell-lexer")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haskell-lexer" version))
       (sha256
        (base32 "0h8s19j2flby3qrvb93j1d2lpjl1jgin8lj6dqpb7c86h59f2xlx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "haskell-lexer")))
    (home-page "https://github.com/yav/haskell-lexer")
    (synopsis "Fully compliant Haskell 98 lexer")
    (description "This package provides a fully compliant Haskell 98 lexer.")
    (license license:expat)))

(define-public ghc-haskell-src
  (package
    (name "ghc-haskell-src")
    (version "1.0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haskell-src" version))
       (sha256
        (base32 "1bairsbik3n9pbhkl547793sh9dp15n32a93di2n1xa75bbiy3zy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "haskell-src")))
    (inputs (list ghc-syb))
    (native-inputs (list ghc-happy))
    (home-page "http://hackage.haskell.org/package/haskell-src")
    (synopsis "Support for manipulating Haskell source code")
    (description
     "The @code{haskell-src} package provides support for manipulating Haskell
source code.  The package provides a lexer, parser and pretty-printer, and a
definition of a Haskell abstract syntax tree (AST).  Common uses of this
package are to parse or generate Haskell 98 code.")
    (license license:bsd-3)))

(define-public ghc-haskell-src-exts
  (package
    (name "ghc-haskell-src-exts")
    (version "1.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haskell-src-exts" version))
       (sha256
        (base32 "01bcrxs9af4yqpclw43aijmsd1g19qhyzb47blz7vzwz2r3k11b7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "haskell-src-exts")))
    (native-inputs (list ghc-smallcheck ghc-tasty ghc-tasty-smallcheck
                         ghc-tasty-golden ghc-pretty-show ghc-happy))
    (home-page "https://github.com/haskell-suite/haskell-src-exts")
    (synopsis "Library for manipulating Haskell source")
    (description
     "Haskell-Source with Extensions (HSE, haskell-src-exts) is an
extension of the standard @code{haskell-src} package, and handles most
registered syntactic extensions to Haskell.  All extensions implemented in GHC
are supported.  Apart from these standard extensions, it also handles regular
patterns as per the HaRP extension as well as HSX-style embedded XML syntax.")
    (license license:bsd-3)))

(define-public ghc-haskell-src-exts-util
  (package
    (name "ghc-haskell-src-exts-util")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haskell-src-exts-util" version))
       (sha256
        (base32 "0fvqi72m74p7q5sbpy8m2chm8a1lgy10mfrcxcz8wrh59vngj0n8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "haskell-src-exts-util")))
    (inputs (list ghc-data-default ghc-haskell-src-exts ghc-semigroups
                  ghc-uniplate))
    (home-page "https://github.com/pepeiborra/haskell-src-exts-util")
    (synopsis "Helper functions for working with haskell-src-exts trees")
    (description "This package provides helper functions for working with
@code{haskell-src-exts} trees.")
    (license license:bsd-3)))

(define-public ghc-haskell-src-meta
  (package
    (name "ghc-haskell-src-meta")
    (version "0.8.15")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haskell-src-meta" version))
       (sha256
        (base32 "1ghdddbrhv4qwa7fgibafs7y2mr1rxrh2w67irs15wfmk4qvdsi6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "haskell-src-meta")))
    (inputs (list ghc-haskell-src-exts ghc-syb ghc-th-orphans))
    (native-inputs (list ghc-hunit ghc-tasty ghc-tasty-hunit))
    (home-page "http://hackage.haskell.org/package/haskell-src-meta")
    (synopsis "Parse source to template-haskell abstract syntax")
    (description "This package provides tools to parse Haskell sources to the
template-haskell abstract syntax.")
    (license license:bsd-3)))

(define-public ghc-hasktags
  (package
    (name "ghc-hasktags")
    (version "0.73.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hasktags" version))
       (sha256
        (base32 "0w0g50ggdmkpxgwqdwdbizw3cz8q86l1xwhvj3bjczh72q5xa0nh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hasktags")))
    (inputs (list ghc-json ghc-microlens-platform ghc-utf8-string
                  ghc-optparse-applicative))
    (native-inputs (list ghc-hunit))
    (arguments
     `(#:cabal-revision ("2"
                         "175kmkz4k2dssn4dg0d288allkqwx8y6m3p7pcgg88km8d2p0mgp")))
    (home-page "http://github.com/MarcWeber/hasktags")
    (synopsis "Make @code{Ctags} and @code{Etags} files for Haskell programs")
    (description
     "This package provides a means of generating tag files for Emacs and
Vim.")
    (license license:bsd-3)))

(define-public ghc-hcodecs
  (package
    (name "ghc-hcodecs")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "HCodecs" version))
              (sha256
               (base32
                "0gbspig721viwncsfg9m4qc9jbl9rz93ra74d5b1zw9pw7rhy5ji"))))
    (build-system haskell-build-system)
    (inputs (list ghc-fail
                  ghc-random
                  ghc-semigroups))
    (native-inputs (list ghc-quickcheck))
    (home-page "https://github.com/Mokosha/HCodecs")
    (synopsis "Read, write and manipulate MIDI, WAVE and SoundFont2 multimedia files")
    (description "This library provides functions to read, write and manipulate MIDI, WAVE
and SoundFont2 multimedia files.  It is written entirely in Haskell (without any FFI).
It uses efficient parsing and building combinators for binary data stored in ByteStrings
(based on the one in binary package).")
    (license license:bsd-3)))

(define-public ghc-hex
  (package
    (name "ghc-hex")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hex" version))
       (sha256
        (base32 "1mc66758254d93m7vab7q6lhn7qphzxd6wyc3v6yq1diy0gji4va"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hex")))
    (home-page "https://github.com/taruti/haskell-hex")
    (synopsis "Convert strings into hexadecimal and back")
    (description "This package converts between bytestrings and their
hexadecimal string representation.")
    (license license:bsd-3)))

(define-public ghc-hindent
  (package
    (name "ghc-hindent")
    (version "6.3.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hindent" version))
              (sha256
               (base32
                "0axbrrpqm4p8d43ypdsgi345xz7zbdll9a1zv235dia4cxk0mpv4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hindent")))
    (inputs (list ghc-async
                  ghc-lib-parser
                  ghc-lib-parser-ex
                  ghc-monad-loops
                  ghc-optparse-applicative
                  ghc-path
                  ghc-path-io
                  ghc-regex-tdfa
                  ghc-split
                  ghc-syb
                  ghc-unicode-show
                  ghc-utf8-string
                  ghc-yaml))
    (native-inputs (list ghc-hspec ghc-diff))
    (home-page "https://github.com/mihaimaruseac/hindent")
    (synopsis "Extensible Haskell pretty printer")
    (description
     "This package provides automatic formatting for Haskell files.  Both a
library and an executable.")
    (license license:bsd-3)))

(define-public ghc-hinotify
  (package
    (name "ghc-hinotify")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hinotify" version))
       (sha256
        (base32 "1h6hvy9zcwvp5ww592b9q9rs5v86dacq6r01k75mzjakmjbnzm57"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hinotify")))
    (inputs (list ghc-async))
    (home-page "https://github.com/kolmodin/hinotify")
    (synopsis "Haskell binding to inotify")
    (description
     "This library provides a wrapper to the Linux kernel's inotify
feature, allowing applications to subscribe to notifications when a file is
accessed or modified.")
    (license license:bsd-3)))

(define-public ghc-hledger-lib
  (package
    (name "ghc-hledger-lib")
    (version "1.43.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hledger-lib" version))
       (sha256
        (base32 "18037qwz7d0h4i86ac0w3hkrvx22vdxf04fjbg0qjlizgb3dlazf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hledger-lib")))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           ;; The doctests require GHC_PACKAGE_PATH but Setup.hs fails
           ;; if it is defined, so we run them separately
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (if tests?
                 (let* ((tmpdir (or (getenv "TMP") "/tmp"))
                        (package-db (string-append tmpdir "/package.conf.d")))
                   (invoke "runhaskell" "Setup.hs" "test" "unittest")
                   (setenv "GHC_PACKAGE_PATH" package-db)
                   (invoke "./dist/build/doctest/doctest")
                   (unsetenv "GHC_PACKAGE_PATH"))
                 (format #t "Testsuite not run.%~")))))))
    (inputs (list ghc-decimal
                  ghc-glob
                  ghc-aeson
                  ghc-aeson-pretty
                  ghc-ansi-terminal
                  ghc-blaze-html
                  ghc-blaze-markup
                  ghc-call-stack
                  ghc-cassava
                  ghc-cassava-megaparsec
                  ghc-cmdargs
                  ghc-colour
                  ghc-data-default
                  ghc-doclayout
                  ghc-encoding
                  ghc-extra
                  ghc-file-embed
                  ghc-hashtables
                  ghc-lucid
                  ghc-megaparsec
                  ghc-microlens
                  ghc-microlens-th
                  ghc-parser-combinators
                  ghc-pretty-simple
                  ghc-regex-tdfa
                  ghc-safe
                  ghc-tabular
                  ghc-tasty
                  ghc-tasty-hunit
                  ghc-terminal-size
                  ghc-timeit
                  ghc-uglymemo
                  ghc-unordered-containers
                  ghc-utf8-string))
    (native-inputs (list ghc-doctest))
    (home-page "http://hledger.org")
    (synopsis "Reusable library providing the core functionality of hledger")
    (description
     "A reusable library containing hledger's core functionality.
This is used by most hledger* packages so that they support the same common
file formats, command line options, reports etc.

hledger is a robust, cross-platform set of tools for tracking money, time, or
any other commodity, using double-entry accounting and a simple, editable file
format, with command-line, terminal and web interfaces.  It is a Haskell
rewrite of Ledger, and one of the leading implementations of Plain Text
Accounting.")
    (license license:gpl3)))

(define-public ghc-hmatrix
  (package
    (name "ghc-hmatrix")
    (version "0.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hmatrix" version))
       (sha256
        (base32 "05462prqkbqpxfbzsgsp8waf0sirg2qz6lzsk7r1ll752n7gqkbg"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hmatrix")))
    (inputs (list ghc-random
                  ghc-split
                  ghc-primitive
                  ghc-storable-complex
                  ghc-semigroups
                  ghc-vector))
    (arguments
     `(#:cabal-revision ("1"
                         "154n2hddfk90rqd9fwwhfjnq6ab701nglsrdjss71brza93wjy8d")))
    (home-page "https://github.com/haskell-numerics/hmatrix")
    (synopsis "Haskell numeric linear algebra library")
    (description "The HMatrix package provides a Haskell library for
dealing with linear systems, matrix decompositions, and other
numerical computations based on BLAS and LAPACK.")
    (license license:bsd-3)))

(define-public ghc-hmatrix-gsl
  (package
    (name "ghc-hmatrix-gsl")
    (version "0.19.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hmatrix-gsl" version))
       (sha256
        (base32 "0v6dla426x4ywaq59jm89ql1i42n39iw6z0j378xwb676v9kfxhm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hmatrix-gsl")))
    (inputs (list ghc-hmatrix ghc-vector ghc-random))
    (home-page "https://github.com/albertoruiz/hmatrix")
    (synopsis "Haskell GSL binding")
    (description
     "This Haskell library provides a purely functional
interface to selected numerical computations, internally implemented
using GSL.")
    (license license:gpl3)))

(define-public ghc-hmatrix-gsl-stats
  (package
    (name "ghc-hmatrix-gsl-stats")
    (version "0.4.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hmatrix-gsl-stats" version))
       (sha256
        (base32 "1cq049sj3q5r06x7i35hqrkf2jc4p4kfi9zv0jmi2vp7w4644i5q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hmatrix-gsl-stats")))
    (inputs (list ghc-vector ghc-storable-complex ghc-hmatrix))
    (home-page "http://code.haskell.org/hmatrix-gsl-stats")
    (synopsis "GSL Statistics interface for Haskell")
    (description "This Haskell library provides a purely functional
interface for statistics based on hmatrix and GSL.")
    (license license:bsd-3)))

(define-public ghc-hmatrix-special
  (package
    (name "ghc-hmatrix-special")
    (version "0.19.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hmatrix-special" version))
       (sha256
        (base32 "1mywr61kr852sbff26n9x95kswx9l4ycbv6s68qsbkh02xzqq7qz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hmatrix-special")))
    (inputs (list ghc-hmatrix ghc-hmatrix-gsl))
    (home-page "https://github.com/albertoruiz/hmatrix")
    (synopsis "Haskell interface to GSL special functions")
    (description "This library provides an interface to GSL special
functions for Haskell.")
    (license license:gpl3)))

(define-public ghc-hookup
  (package
    (name "ghc-hookup")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hookup" version))
       (sha256
        (base32 "1p8mkb71bbs3lv7n1krcngaskn2s2icm3sl30qs8dsla7ww8afqm"))
       (snippet
         #~(substitute* "hookup.cabal"
             (("(base|network)  *[0-9<>=^&|. ]*" _ package)
              package)))
       (modules '((guix build utils)))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-async
           ghc-network
           ghc-attoparsec
           ghc-hsopenssl
           ghc-hsopenssl-x509-system))
    (properties '((upstream-name . "hookup")))
    (home-page "https://github.com/glguy/irc-core")
    (synopsis "Abstracts network connections over SOCKS5 and TLS")
    (description
     "This package provides an abstraction for communicating with line-oriented
network services while abstracting over the use of SOCKS5 and TLS (via
OpenSSL)")
    (license license:isc)))

(define-public ghc-hostname
  (package
    (name "ghc-hostname")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hostname" version))
       (sha256
        (base32 "0p6gm4328946qxc295zb6vhwhf07l1fma82vd0siylnsnsqxlhwv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hostname")))
    (home-page "http://hackage.haskell.org/package/hostname")
    (synopsis "Hostname in Haskell")
    (description "Network.HostName is a simple package providing a means to
determine the hostname.")
    (license license:bsd-3)))

(define-public ghc-hourglass
  (package
    (name "ghc-hourglass")
    (version "0.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hourglass" version))
       (sha256
        (base32 "0jnay5j13vpz6i1rkaj3j0d9v8jfpri499xn3l7wd01f81f5ncs4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hourglass")))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit
                         ghc-old-locale))
    (arguments
      (list
       #:tests? #f ; tests require a badly outdated time library
       #:cabal-revision '("1"
                          "0yc9k9lrx6z5r52mk3hra4v74ksk590d89lrj934bj1hrnv6ri45")))
    (home-page "https://github.com/vincenthz/hs-hourglass")
    (synopsis "Simple time-related library for Haskell")
    (description
     "This is a simple time library providing a simple but powerful and
performant API.  The backbone of the library are the @code{Timeable} and
@code{Time} type classes.  Each @code{Timeable} instances can be converted to
a type that has a @code{Time} instances, and thus are different
representations of current time.")
    (license license:bsd-3)))

(define-public ghc-hpack
  (package
    (name "ghc-hpack")
    (version "0.38.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hpack" version))
       (sha256
        (base32 "1g47rf3pglfkjyk3qfz6wvjp0zh16s4qhayqyyzxg91aqq3fqqd6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hpack")))
    (arguments (list #:tests? #f)) ; tests require internet access
    (inputs (list ghc-glob
                  ghc-aeson
                  ghc-bifunctors
                  ghc-crypton
                  ghc-http-client
                  ghc-http-client-tls
                  ghc-http-types
                  ghc-infer-license
                  ghc-scientific
                  ghc-unordered-containers
                  ghc-vector
                  ghc-yaml))
    (native-inputs (list hspec-discover))
    (home-page "https://github.com/sol/hpack#readme")
    (synopsis "Tools for an alternative Haskell package format")
    (description
     "Hpack is a format for Haskell packages.  It is an alternative to the
Cabal package format and follows different design principles.  Hpack packages
are described in a file named @code{package.yaml}.  Both @code{cabal2nix} and
@code{stack} support @code{package.yaml} natively.  For other build tools the
@code{hpack} executable can be used to generate a @code{.cabal} file from
@code{package.yaml}.")
    (license license:expat)))

(define-public ghc-hspec-megaparsec
  (package
    (name "ghc-hspec-megaparsec")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-megaparsec" version))
       (sha256
        (base32 "0bbajckrjxynjmd8ax2xv4k5fbjgrypnmbg8amgwwpy0jrzzbx35"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-megaparsec")))
    (inputs (list ghc-hspec-expectations ghc-megaparsec))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/mrkkrp/hspec-megaparsec")
    (synopsis "Utility functions for testing Megaparsec parsers with Hspec")
    (description
     "Provides a small set of helper functions for testing Megaparsec parsers
with Hspec.")
    (license license:bsd-3)))

(define-public ghc-hs-bibutils
  (package
    (name "ghc-hs-bibutils")
    (version "6.10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hs-bibutils" version))
       (sha256
        (base32 "1wnpy1v5rbii2iwlcc9psnww8pkirv9zl21s64cmbi6q7dv15g3n"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hs-bibutils")))
    (inputs (list ghc-syb))
    (home-page "https://github.com/wilx/hs-bibutils")
    (synopsis "Haskell bindings to bibutils")
    (description
     "This package provides Haskell bindings to @code{bibutils}, a library
that interconverts between various bibliography formats using a common
MODS-format XML intermediate.")
    (license license:gpl2+)))

(define-public ghc-hs-conllu
  (package
    (name "ghc-hs-conllu")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hs-conllu" version))
              (sha256
               (base32
                "1azh4g5kdng8v729ldgblkmrdqrc501rgm9wwqx6gkqwwzn8w3r4"))
              (snippet
               #~(substitute* "hs-conllu.cabal"
                  (("(containers|filepath)  *[0-9<>=^&|.* ]*" _ package)
                   package)))
              (modules '((guix build utils)))))
    (build-system haskell-build-system)
    (inputs (list ghc-megaparsec ghc-void))
    (home-page "https://github.com/arademaker/hs-conllu")
    (synopsis "CoNLL-U validating parser and utils")
    (description
     "Utilities to parse, print, diff, and analyse data in CoNLL-U, a format
used in linguistics to represent the syntactic annotation of sentences.  See
@url{https://universaldependencies.org/format.html}")
    (license license:lgpl3)))

(define-public ghc-hslogger
  (package
    (name "ghc-hslogger")
    (version "1.3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslogger" version))
       (sha256
        (base32 "0hz6v02p89ihr0130hzya78h54lf6kw3vgf7idnxpqwy8v9a49zb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslogger")))
    (inputs (list ghc-network-bsd ghc-network))
    (native-inputs (list ghc-hunit))
    (home-page "https://github.com/haskell-hvr/hslogger/wiki")
    (synopsis
     "Logging framework for Haskell, similar to Python's logging module")
    (description
     "Hslogger lets each log message have a priority and source be
associated with it.  The programmer can then define global handlers that route
or filter messages based on the priority and source.  It also has a syslog
handler built in.")
    (license license:bsd-3)))

(define-public ghc-hslua
  (package
    (name "ghc-hslua")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua" version))
       (sha256
        (base32 "096x45rz1czsnilpn3my5vyafw9dn8qdnmf0apz0q3y3wa4840j9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua")))
    (inputs (list ghc-hslua-aeson
                  ghc-hslua-core
                  ghc-hslua-classes
                  ghc-hslua-marshalling
                  ghc-hslua-objectorientation
                  ghc-hslua-packaging
                  ghc-hslua-typing))
    (native-inputs (list ghc-lua
                         ghc-lua-arbitrary
                         ghc-quickcheck
                         ghc-quickcheck-instances
                         ghc-tasty-hslua
                         ghc-tasty
                         ghc-tasty-hunit))
    (home-page "https://hslua.org/")
    (synopsis "Lua language interpreter embedding in Haskell")
    (description
     "The Scripting.Lua module is a wrapper of the Lua language interpreter as
described in @url{https://www.lua.org/}.")
    (license license:expat)))

(define-public ghc-hslua-module-system
  (package
    (name "ghc-hslua-module-system")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-module-system" version))
       (sha256
        (base32 "0skdgb21x2zdyv1m3ai4n8axnk85i3s08pvsrkjwwsfcr7v3r432"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-module-system")))
    (inputs (list ghc-hslua-core ghc-hslua-packaging ghc-hslua-marshalling
                  ghc-temporary))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-lua))
    (home-page "https://github.com/hslua/hslua")
    (synopsis "Lua module wrapper around Haskell's System module")
    (description
     "This library provides access to system information and
functionality to Lua scripts via Haskell's @code{System} module.  Intended
usage for this package is to preload it by adding the loader function to
@code{package.preload}.  Note that the Lua @code{package} library must have
already been loaded before the loader can be added.")
    (license license:expat)))

(define-public ghc-hslua-module-text
  (package
    (name "ghc-hslua-module-text")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-module-text" version))
       (sha256
        (base32 "16635kdanaiwn5rdmkaga6d9jhw8zrvhpnqsyqm0zap98n6b146b"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-module-text")))
    (inputs (list ghc-hslua-core ghc-hslua-packaging ghc-hslua-marshalling))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-lua))
    (home-page "https://github.com/hslua/hslua")
    (synopsis "Lua module for text")
    (description
     "This package provides a UTF-8 aware subset of Lua's @code{string} module
for Haskell.  The functions provided by this module are @code{upper},
@code{lower}, @code{len}, @code{reverse}, and @code{sub}.")
    (license license:expat)))

(define-public ghc-hsyaml
  (package
    (name "ghc-hsyaml")
    (version "0.2.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "HsYAML" version))
       (sha256
        (base32 "13av46629msknp1spmcczgd2hpsyj0ca590vpiy7df8l6cfwjyk5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "HsYAML")))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1l5ig8a1c13rwcx530li93p0kkxcsjpjyr303v19z6n8zmdvnz6a")))
    (home-page "https://github.com/haskell-hvr/HsYAML")
    (synopsis "Pure Haskell YAML 1.2 parser")
    (description
     "This library provides a
@url{http://yaml.org/spec/1.2/spec.html, YAML 1.2} parser implementation
for Haskell.  Its features include:

@itemize
@item Pure Haskell implementation with small dependency footprint and
emphasis on strict compliance with the YAML 1.2 specification.

@item Direct decoding to native Haskell types via (aeson-inspired)
typeclass-based API (see @code{Data.YAML}).

@item Support for constructing custom YAML node graph
representation (including support for cyclic YAML data structures).

@item Support for the standard (untyped) @emph{Failsafe}, (strict)
@emph{JSON}, and (flexible) @emph{Core} ``schemas'' providing implicit
typing rules as defined in the YAML 1.2 specification (including support
for user-defined custom schemas).

@item Event-based API resembling LibYAML's Event-based API (see
@code{Data.YAML.Event}).

@item Low-level API access to lexical token-based scanner (see
@code{Data.YAML.Token}).
@end itemize")
    (license license:gpl2)))

(define-public ghc-http-api-data
  (package
    (name "ghc-http-api-data")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "http-api-data" version))
       (sha256
        (base32 "0d42xkm60i3irxcvwixvn5c01paz2kpfsz29vhxz08ir83zsk16w"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-api-data")))
    (inputs (list ghc-cookie
                  ghc-hashable
                  ghc-http-types
                  ghc-text-iso8601
                  ghc-tagged
                  ghc-time-compat
                  ghc-unordered-containers
                  ghc-uuid-types))
    (native-inputs (list ghc-hunit ghc-hspec ghc-quickcheck
                         ghc-quickcheck-instances ghc-hspec hspec-discover))
    (home-page "http://github.com/fizruk/http-api-data")
    (synopsis "Convert to/from HTTP API data like URL pieces, headers and
query parameters")
    (description "This Haskell package defines typeclasses used for converting
Haskell data types to and from HTTP API data.")
    (license license:bsd-3)))

(define-public ghc-ieee754
  (package
    (name "ghc-ieee754")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ieee754" version))
       (sha256
        (base32 "1lcs521g9lzy9d7337vg4w7q7s8500rfqy7rcifcz6pm6yfgyb8f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ieee754")))
    (home-page "http://github.com/patperry/hs-ieee754")
    (synopsis "Utilities for dealing with IEEE floating point numbers")
    (description
     "Utilities for dealing with IEEE floating point numbers,
ported from the Tango math library; approximate and exact equality comparisons
for general types.")
    (license license:bsd-3)))

(define-public ghc-ifelse
  (package
    (name "ghc-ifelse")
    (version "0.85")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "IfElse" version))
       (sha256
        (base32 "1kfx1bwfjczj93a8yqz1n8snqiq5655qgzwv1lrycry8wb1vzlwa"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "IfElse")))
    (home-page "http://hackage.haskell.org/package/IfElse")
    (synopsis "Monadic control flow with anaphoric variants")
    (description
     "This library provides functions for control flow inside of
monads with anaphoric variants on @code{if} and @code{when} and a C-like
@code{switch} function.")
    (license license:bsd-3)))

(define-public ghc-indents
  (package
    (name "ghc-indents")
    (version "0.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "indents" version))
       (sha256
        (base32 "0dpcwiz0dwn5aqdsc50plfaawh86adhf7jx5dsmhn5q5nz32qn51"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "indents")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("2"
                         "0rdj5w8d5ykb2sh88xsdgddxyp50dij4zb8bbb9220yfs0l18dsy")))
    (home-page "http://github.com/jaspervdj/indents")
    (synopsis "Indentation sensitive parser-combinators for parsec")
    (description
     "This library provides functions for use in parsing indentation sensitive
contexts.  It parses blocks of lines all indented to the same level as well as
lines continued at an indented level below.")
    (license license:bsd-3)))

(define-public ghc-infer-license
  (package
    (name "ghc-infer-license")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "infer-license" version))
       (sha256
        (base32 "0wlfm6bf55kfvm74xar9lmjg5v1103rs9m3grw1rq5bmcmhzxrhj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "infer-license")))
    (inputs (list ghc-text-metrics))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "http://hackage.haskell.org/package/infer-license")
    (synopsis "Infer software license from a given license file")
    (description "This library provides tools to infer a software
license from a given license file.")
    (license license:expat)))

(define-public ghc-ini
  (package
    (name "ghc-ini")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ini" version))
       (sha256
        (base32 "0dp9c48vli8z6058yajnqg9hyf9swglk8ga4wcwl03aal7n8r7gp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ini")))
    (inputs (list ghc-attoparsec ghc-unordered-containers))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/andreasabel/ini")
    (synopsis
     "Haskell library to easily handle configuration files in the INI format")
    (description
     "The @code{ghc-ini} Haskell library lets programmers quickly and easily
read and write configuration files in the simple INI format.")
    (license license:bsd-3)))

(define-public ghc-inline-c
  (package
    (name "ghc-inline-c")
    (version "0.9.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "inline-c" version))
       (sha256
        (base32 "1cd4bqb4gzd8sgh8icnnzdyqnh81x7150ibx7mqjaygj9672w3bd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "inline-c")))
    (inputs (list ghc-prettyprinter ghc-hashable ghc-parsers
                  ghc-unordered-containers ghc-vector))
    (native-inputs (list ghc-quickcheck
                         ghc-hspec
                         ghc-quickcheck
                         ghc-raw-strings-qq
                         ghc-regex-posix
                         ghc-split))
    (home-page "http://hackage.haskell.org/package/inline-c")
    (synopsis "Write Haskell source files including C code inline")
    (description
     "inline-c lets you seamlessly call C libraries and embed high-performance
inline C code in Haskell modules.  Haskell and C can be freely intermixed in
the same source file, and data passed to and from code in either language with
minimal overhead.  No FFI required.")
    (license license:expat)))

(define-public ghc-inline-c-cpp
  (package
    (name "ghc-inline-c-cpp")
    (version "0.5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "inline-c-cpp" version))
       (sha256
        (base32 "0m8rkmjmqh8xy41ci87z3c7x9z2r5p8j4qwibqw189rrgy5g1471"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "inline-c-cpp")))
    (inputs (list ghc-inline-c ghc-safe-exceptions))
    (native-inputs (list ghc-hspec ghc-vector))
    (home-page "http://hackage.haskell.org/package/inline-c-cpp")
    (synopsis "Lets you embed C++ code into Haskell")
    (description
     "This package provides utilities to inline C++ code into Haskell using
@code{inline-c}.")
    (license license:expat)))

(define-public ghc-integer-conversion
  (package
    (name "ghc-integer-conversion")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "integer-conversion" version))
       (sha256
        (base32 "0nhm487gdg17w02wwqwr56wa8lkv0g4n9g1y6pv10cq792h690f1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "integer-conversion")))
    (inputs (list ghc-primitive))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1jf1wqbs6cgjbc1igy4lll1qxz4ynknspqpgbpzwn911dvhll7cl")))
    (home-page "https://github.com/phadej/integer-conversion")
    (synopsis "Conversion from strings to Integer")
    (description
     "The naive @@foldl (\\acc d -> acc * 10 + d) 0@@ is expensive (quadratic!) for
large @@Integer@@s.  This package provides sub-quadratic implementation.")
    (license license:bsd-3)))

(define-public ghc-integer-logarithms
  (package
    (name "ghc-integer-logarithms")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "integer-logarithms" version))
       (sha256
        (base32 "0icg8k0h7yc3aynsbidppwyfkjnq8spaczdi5bby5jqq4mncg4va"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "integer-logarithms")))
    (native-inputs (list ghc-quickcheck
                         ghc-smallcheck
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-tasty-smallcheck))
    (home-page "https://github.com/haskellari/integer-logarithms")
    (synopsis "Integer logarithms")
    (description
     "This package provides the following modules:
@code{Math.NumberTheory.Logarithms} and
@code{Math.NumberTheory.Powers.Integer} from the @code{arithmoi} package,
@code{GHC.Integer.Logarithms.Compat} and
@code{Math.NumberTheory.Power.Natural}, as well as some additional functions
in migrated modules.")
    (license license:expat)))

(define-public ghc-interpolate
  (package
    (name "ghc-interpolate")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "interpolate" version))
       (sha256
        (base32 "03jrkj9c62w0c2awym8mhpsgpd0jffl50cqwfrm7bbdfhd8dsxi7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "interpolate")))
    (inputs (list ghc-haskell-src-meta))
    (native-inputs (list ghc-quickcheck ghc-base-compat ghc-hspec
                         hspec-discover ghc-quickcheck-instances))
    (home-page "https://github.com/sol/interpolate#readme")
    (synopsis "String interpolation library")
    (description "This package provides a string interpolation library for
Haskell.")
    (license license:expat)))

(define-public ghc-intervalmap
  (package
    (name "ghc-intervalmap")
    (version "0.6.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "IntervalMap" version))
       (sha256
        (base32 "17v9q1vnm3pzrr5xhv8xvxqh27facwwfladrr10l57fzibp82265"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "IntervalMap")))
    (native-inputs (list ghc-quickcheck ghc-quickcheck ghc-quickcheck
                         ghc-quickcheck))
    (home-page "http://www.chr-breitkopf.de/comp/IntervalMap")
    (synopsis "Containers for intervals, with efficient search")
    (description
     "This package provides ordered containers of intervals, with efficient
search for all keys containing a point or overlapping an interval.  See the
example code on the home page for a quick introduction.")
    (license license:bsd-3)))

(define-public ghc-intervals
  (package
    (name "ghc-intervals")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "intervals" version))
       (sha256
        (base32 "07qsz1pzfgbxllavj8d428i3vnz7a5a9cxikimzd0rsz9dlprdnn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "intervals")))
    (inputs (list ghc-distributive))
    (native-inputs (list ghc-quickcheck))
    (home-page "http://github.com/ekmett/intervals")
    (synopsis "Interval arithmetic")
    (description
     "This library provides @code{Numeric.Interval.Interval},
which represents a closed, convex set of floating point values.")
    (license license:bsd-3)))

(define-public ghc-invariant
  (package
    (name "ghc-invariant")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "invariant" version))
       (sha256
        (base32 "1cxfy1s3p91g5n1z85058lc27xy4xfl3dnkvxcxn3m70wd7apqm9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "invariant")))
    (inputs (list ghc-bifunctors
                  ghc-comonad
                  ghc-contravariant
                  ghc-profunctors
                  ghc-statevar
                  ghc-tagged
                  ghc-th-abstraction
                  ghc-transformers-compat
                  ghc-unordered-containers))
    (native-inputs (list ghc-hspec ghc-quickcheck hspec-discover))
    (arguments
     `(#:cabal-revision ("1"
                         "1inib3bc400cghq70lqslw5a6v9cha05lkrvicayvzwk9sgn16ja")))
    (home-page "https://github.com/nfrisby/invariant-functors")
    (synopsis "Haskell98 invariant functors")
    (description
     "Haskell98 invariant functors (also known as exponential
functors).  For more information, see Edward Kmett's article
@uref{http://comonad.com/reader/2008/rotten-bananas/, Rotten Bananas}.")
    (license license:bsd-2)))

(define-public ghc-io-streams
  (package
    (name "ghc-io-streams")
    (version "1.5.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "io-streams" version))
       (sha256
        (base32 "1zn4iyd18g9jc1qdgixp6hi56nj7czy4jdz2xca59hcn2q2xarfk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "io-streams")))
    (inputs (list ghc-attoparsec ghc-primitive ghc-vector ghc-zlib-bindings
                  ghc-network))
    (native-inputs (list ghc-zlib
                         ghc-hunit
                         ghc-quickcheck
                         ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2))
    (arguments
     `(#:cabal-revision ("6"
                         "12nra580v0l6ijqqg6ccbhqmpczbb8r7g0iqp1hcsg0pbxjmkywj")))
    (home-page "http://hackage.haskell.org/package/io-streams")
    (synopsis "Simple and composable stream I/O")
    (description "This library contains simple and easy-to-use
primitives for I/O using streams.")
    (license license:bsd-3)))

(define-public ghc-io-streams-haproxy
  (package
    (name "ghc-io-streams-haproxy")
    (version "1.0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "io-streams-haproxy" version))
       (sha256
        (base32 "1dcn5hd4fiwyq7m01r6fi93vfvygca5s6mz87c78m0zyj29clkmp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "io-streams-haproxy")))
    (inputs (list ghc-attoparsec ghc-io-streams ghc-network))
    (native-inputs (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (arguments
     `(#:cabal-revision ("9"
                         "1waziyv0f1iap83abgn4ax1zwbdbhfhzh72smzm1azqzl6ggcdq2")))
    (home-page "http://snapframework.com/")
    (synopsis "HAProxy protocol 1.5 support for io-streams")
    (description
     "HAProxy protocol version 1.5 support
(see @uref{http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt})
for applications using io-streams.  The proxy protocol allows information
about a networked peer (like remote address and port) to be propagated
through a forwarding proxy that is configured to speak this protocol.")
    (license license:bsd-3)))

(define-public ghc-iproute
  (package
    (name "ghc-iproute")
    (version "1.7.15")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "iproute" version))
       (sha256
        (base32 "19abgdk9pk6n8qmvfcpqp282dgbn1mxmg5fsla4xryg6w2kk38qq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "iproute")))
    (inputs (list ghc-appar ghc-byteorder ghc-network ghc-semigroups))
    (native-inputs (list ghc-hspec ghc-quickcheck ghc-safe hspec-discover))
    (home-page "http://www.mew.org/~kazu/proj/iproute/")
    (synopsis "IP routing table")
    (description
     "IP Routing Table is a tree of IP ranges to search one of
them on the longest match base.  It is a kind of TRIE with one way branching
removed.  Both IPv4 and IPv6 are supported.")
    (license license:bsd-3)))

(define-public ghc-ipynb
  (package
    (name "ghc-ipynb")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ipynb" version))
       (sha256
        (base32 "1iwia4sxg40m4d290gys72wabqmkqx24ywsaranwzk2wx5s3sx4s"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ipynb")))
    (inputs (list ghc-unordered-containers ghc-base64-bytestring ghc-aeson
                  ghc-semigroups))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-microlens-aeson
                         ghc-microlens))
    (arguments
     `(#:cabal-revision ("1"
                         "0fl9x5amq0g5dg57dcgc0g4ir0r1fdbx06aldsqdwzdc9zs97v6k")))
    (home-page "http://hackage.haskell.org/package/ipynb")
    (synopsis "Data structure for working with Jupyter notebooks")
    (description
     "This library defines a data structure for representing
Jupyter notebooks, along with @code{ToJSON} and @code{FromJSON}
instances for conversion to and from JSON .ipynb files.")
    (license license:bsd-3)))

(define-public ghc-irc-core
  (package
  (name "ghc-irc-core")
  (version "2.13")
  (source
   (origin
     (method url-fetch)
     (uri (hackage-uri "irc-core" version))
     (sha256
      (base32 "0z95cl8pg6zhzmz642bzgxc0525645whvxbmz6h1i8g1mll3i6kx"))))
  (build-system haskell-build-system)
  (native-inputs
   (list ghc-hunit))
  (inputs
   (list ghc-base64-bytestring
         ghc-attoparsec
         ghc-hashable
         ghc-primitive ghc-vector))
  (properties '((upstream-name . "irc-core")))
  (home-page "https://github.com/glguy/irc-core")
  (synopsis "IRC core library for glirc")
  (description
   "This is the IRC core library for glirc.  The client is available in its own
glirc package.")
  (license license:isc)))

(define-public ghc-isocline
  (package
    (name "ghc-isocline")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "isocline" version))
       (sha256
        (base32 "0s2lwypsvzxcgcml3b3q9g0acwg6ph2q47p42i9a9kc2h2gcd44h"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "isocline")))
    (home-page "https://github.com/daanx/isocline")
    (synopsis "A portable alternative to GNU Readline")
    (description
     "A Haskell wrapper around the @url{https://github.com/daanx/isocline,
Isocline C library} which can provide an alternative to GNU Readline. The
Isocline library is included whole and there are no runtime dependencies.")
    (license license:expat)))

(define-public ghc-iwlib
  (package
    (name "ghc-iwlib")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "iwlib" version))
       (sha256
        (base32 "0khmfwql4vwj55idsxmhjhrbqzfir3g9wm5lmpvnf77mm95cfpdz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "iwlib")))
    (arguments
     `(#:extra-directories ("wireless-tools")))
    (inputs
     (list wireless-tools))
    (home-page "https://github.com/jaor/iwlib")
    (synopsis "Haskell binding to the iw wireless networking library")
    (description
     "IWlib is a thin Haskell binding to the iw C library.  It provides
information about the current wireless network connections, and adapters on
supported systems.")
    (license license:bsd-3)))

(define-public ghc-json
  (package
    (name "ghc-json")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "json" version))
       (sha256
        (base32 "1476fxrfybch9j2mr6yacbvhnggj5ksir1a42114j8s8w89anyfh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "json")))
    (inputs (list ghc-syb))
    (home-page "http://hackage.haskell.org/package/json")
    (synopsis "Serializes Haskell data to and from JSON")
    (description
     "This package provides a parser and pretty printer for
converting between Haskell values and JSON.  @acronym{JavaScript Object
Notation, JSON} is a lightweight data-interchange format.")
    (license license:bsd-3)))

(define-public ghc-juicypixels
  (package
    (name "ghc-juicypixels")
    (version "3.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "JuicyPixels" version))
       (sha256
        (base32 "0kc68bjj3izbdvw67kyzg74fv1ksj2dclq5gxzlnajv87rfsqi1y"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "JuicyPixels")))
    (inputs (list ghc-zlib ghc-vector ghc-primitive))
    (home-page "https://github.com/Twinside/Juicy.Pixels")
    (synopsis "Picture loading and serialization library")
    (description
     "This library can load and store images in PNG, Bitmap, JPEG, Radiance,
TIFF and GIF formats.")
    (license license:bsd-3)))

(define-public ghc-kan-extensions
  (package
    (name "ghc-kan-extensions")
    (version "5.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "kan-extensions" version))
       (sha256
        (base32 "0n716zyihbnq3s1zhqbh3fm0qzhgy2hk79ziy8b6bvydjpzsq8y3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "kan-extensions")))
    (inputs (list ghc-adjunctions
                  ghc-comonad
                  ghc-contravariant
                  ghc-distributive
                  ghc-invariant
                  ghc-free
                  ghc-profunctors
                  ghc-semigroupoids
                  ghc-tagged))
    (home-page "http://github.com/ekmett/kan-extensions/")
    (synopsis "Kan extensions library")
    (description
     "This library provides Kan extensions, Kan lifts, various
forms of the Yoneda lemma, and (co)density (co)monads for Haskell.")
    (license license:bsd-3)))

(define-public ghc-language-c
  (package
    (name "ghc-language-c")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "language-c" version))
       (sha256
        (base32 "0m3dphd0r0n763a5rrg0z4fmiaqn7nkjq15l4vif332zrmgipb37"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "language-c")))
    (native-inputs (list ghc-happy ghc-alex))
    (arguments
     `(#:cabal-revision ("1"
                         "1ffvpasi3yj59fffwdjx6c8wjby5pv42fmfzm7pisnpczmv5hsx6")))
    (home-page "https://visq.github.io/language-c/")
    (synopsis "Analysis and generation of C code")
    (description
     "Language C is a Haskell library for the analysis and generation of C code.
It features a complete, well-tested parser and pretty printer for all of C99
and a large set of GNU extensions.")
    (license license:bsd-3)))

(define-public ghc-language-glsl
  (package
    (name "ghc-language-glsl")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "language-glsl" version))
       (sha256
        (base32
         "0hdg67ainlqpjjghg3qin6fg4p783m0zmjqh4rd5gyizwiplxkp1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "language-glsl")))
    (inputs (list ghc-prettyclass))
    (arguments
     `(#:tests? #f
       #:cabal-revision
       ("1" "10ac9pk4jy75k03j1ns4b5136l4kw8krr2d2nw2fdmpm5jzyghc5")))
    (home-page "https://hackage.haskell.org/package/language-glsl")
    (synopsis "GLSL abstract syntax tree, parser, and pretty-printer")
    (description "This package is a Haskell library for the
representation, parsing, and pretty-printing of GLSL 1.50 code.")
    (license license:bsd-3)))

(define-public ghc-language-haskell-extract
  (package
    (name "ghc-language-haskell-extract")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "language-haskell-extract" version))
       (patches (search-patches "ghc-language-haskell-extract-ghc-8.10.patch"))
       (sha256
        (base32
         "1nxcs7g8a1sp91bzpy4cj6s31k5pvc3gvig04cbrggv5cvjidnhl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "language-haskell-extract")))
    (arguments
     `(#:cabal-revision
       ("1" "1chx4g8ngb1hpyh3r9rbl8rkjkm67klms4wmw3p1g2llg47vvqip")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "language-haskell-extract.cabal"
               (("(template-haskell)\\s+[^,]+" all dep)
                dep)))))))
    (inputs
     (list ghc-regex-posix ghc-template-haskell))
    (home-page "https://github.com/finnsson/template-helper")
    (synopsis "Haskell module to automatically extract functions from
the local code")
    (description "This package contains helper functions on top of
Template Haskell.

For example, @code{functionExtractor} extracts all functions after a
regexp-pattern, which can be useful if you wish to extract all functions
beginning with @code{test} (for a test framework) or all functions beginning
with @code{wc} (for a web service).")
    (license license:bsd-3)))

(define-public ghc-language-python
  (package
    (name "ghc-language-python")
    (version "0.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "language-python" version))
       (sha256
        (base32 "1mf3czvnh9582klv0c9g7pcn1wx4qjwpvhv8la6afaifv6y5lki2"))
       (patches (search-patches "ghc-language-python-fix-build-with-happy-2.patch"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision ("2"
                         "024fn653gmxw4ndmqvg1d3lwmxbvrlllc9iw2zw0c3nkcgcv39sg")))
    (native-inputs (list ghc-alex ghc-happy))
    (inputs (list ghc-monads-tf ghc-utf8-string))
    (home-page "http://github.com/bjpop/language-python")
    (synopsis "Parse and pretty print Python code in Haskell")
    (description
     "@code{language-python} is a Haskell library for lexical analysis,
parsing and pretty printing Python code.  It supports versions 2.x and 3.x of
Python.")
    (license license:bsd-3)
    (properties '((upstream-name . "language-python")))))

(define-public ghc-lens
  (package
    (name "ghc-lens")
    (version "5.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lens" version))
       (sha256
        (base32 "1s0ziznj60l9z3z5dacq58kaq8cdfxcz0r75f5hwj25ivzrsrszg"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lens")))
    (inputs (list ghc-assoc
                  ghc-base-orphans
                  ghc-bifunctors
                  ghc-call-stack
                  ghc-comonad
                  ghc-contravariant
                  ghc-distributive
                  ghc-free
                  ghc-hashable
                  ghc-indexed-traversable
                  ghc-indexed-traversable-instances
                  ghc-kan-extensions
                  ghc-parallel
                  ghc-profunctors
                  ghc-reflection
                  ghc-semigroupoids
                  ghc-strict
                  ghc-tagged
                  ghc-th-abstraction
                  ghc-these
                  ghc-transformers-compat
                  ghc-unordered-containers
                  ghc-vector))
    (native-inputs (list ghc-quickcheck
                         ghc-tasty
                         ghc-tasty-quickcheck
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-simple-reflect))
    (home-page "http://github.com/ekmett/lens/")
    (synopsis "Lenses, Folds and Traversals")
    (description
     "This library provides @code{Control.Lens}.  The combinators
in @code{Control.Lens} provide a highly generic toolbox for composing families
of getters, folds, isomorphisms, traversals, setters and lenses and their
indexed variants.")
    (license license:bsd-2)))

(define-public ghc-lens-family-core
  (package
    (name "ghc-lens-family-core")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lens-family-core" version))
       (sha256
        (base32 "0r8v42ybwvl1ayz2502mpjl2nc4815699k3f30qln5b9g6qk26lv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lens-family-core")))
    (home-page "http://hackage.haskell.org/package/lens-family-core")
    (synopsis "Haskell 98 Lens Families")
    (description
     "This package provides first class functional references.  In addition to
the usual operations of getting, setting and composition, plus integration
with the state monad, lens families provide some unique features:

@itemize
@item Polymorphic updating
@item Traversals
@item Cast projection functions to read-only lenses
@item Cast @code{toList} functions to read-only traversals
@item Cast semantic editor combinators to modify-only traversals
@end itemize

For optimal first-class support use the lens-family package with rank 2/rank N
polymorphism.  @code{Lens.Family.Clone} allows for first-class support of
lenses and traversals for those who require Haskell 98.")
    (license license:bsd-3)))

(define-public ghc-generic-lens-core
  (package
    (name "ghc-generic-lens-core")
    (version "2.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "generic-lens-core" version))
       (sha256
        (base32 "08i4c9yb6z84iknrnl9f3f343121j7ilp0a679v81nsjm9xz3rlf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "generic-lens-core")))
    (inputs (list ghc-indexed-profunctors))
    (arguments
     `(#:cabal-revision ("2"
                         "028vm0h89civn7f4cvrh3b67s2qd82g4qn5src0mkm68gngz6bqd")))
    (home-page "https://github.com/kcsongor/generic-lens")
    (synopsis "Generically derive traversals, lenses and prisms")
    (description
     "This library uses GHC.Generics to derive efficient optics (traversals,
lenses and prisms) for algebraic data types in a type-directed way, with a
focus on good type inference and error messages when possible.  This package
is the shared internal logic of the @code{generic-lens} and
@code{generic-optics} libraries.")
    (license license:bsd-3)))

(define-public ghc-generic-lens
  (package
    (name "ghc-generic-lens")
    (version "2.2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "generic-lens" version))
       (sha256
        (base32 "0s4b51s11ssmndmx9m9zbwgv9rb27ajwihsrk10hn582rp4ck3c6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "generic-lens")))
    (inputs (list ghc-generic-lens-core ghc-profunctors))
    (native-inputs (list ghc-lens
                         ghc-inspection-testing
                         ghc-hunit
                         ghc-lens
                         ghc-hunit
                         ghc-lens
                         ghc-hunit
                         ghc-doctest
                         ghc-lens))
    (arguments
     (list
      #:cabal-revision '("1"
                         "0ib9848rh56v0dc1giiax2zi2w7is6ahb2cj6ry3p0hwapfd3p49")
      #:phases
      #~ (modify-phases %standard-phases
           ;; The doctests require GHC_PACKAGE_PATH but Setup.hs fails
           ;; if it is defined, so we run them separately
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (if tests?
                 (let* ((tmpdir (or (getenv "TMP") "/tmp"))
                        (package-db (string-append tmpdir "/package.conf.d")))
                   (invoke "runhaskell" "Setup.hs" "test" "inspection-tests"
                           "generic-lens-syb-tree" "generic-lens-bifunctor")
                   (setenv "GHC_PACKAGE_PATH" package-db)
                   (invoke "./dist/build/doctests/doctests")
                   (unsetenv "GHC_PACKAGE_PATH"))
                 (format #t "Testsuite not run.%~")))))))
    (home-page "https://github.com/kcsongor/generic-lens")
    (synopsis "Generically derive traversals, lenses and prisms")
    (description
     "This library uses @code{GHC.Generics} to derive efficient
optics (traversals, lenses and prisms) for algebraic data types in a
type-directed way, with a focus on good type inference and error messages when
possible.  The library exposes a van Laarhoven interface.  For an alternative
interface, supporting an opaque optic type, see @code{generic-optics}.")
    (license license:bsd-3)))

(define-public ghc-these-lens
  (package
    (name "ghc-these-lens")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "these-lens" version))
       (sha256
        (base32 "159dp25rbhcnmpxvvabjl8s9df9mvqi30p81vrmh7kgg86xa050y"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "these-lens")))
    (inputs (list ghc-these ghc-lens))
    (arguments
     `(#:cabal-revision ("1"
                         "07dyn6kqh8apxvzigc64k12h7b0wic8pzy4c5zw4mnsbn8v0l8bh")))
    (home-page "https://github.com/haskellari/these")
    (synopsis "Lenses for These")
    (description
     "This package provides Prism and Traversals for @code{These}.")
    (license license:bsd-3)))

(define-public ghc-libffi
  (package
    (name "ghc-libffi")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "libffi" version))
       (sha256
        (base32 "1w9ssmjx521f4lmaynmh1zargl2zmfvvpq2bldsvnwldfdgikbkn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "libffi")))
    (home-page "http://haskell.org/haskellwiki/Library/libffi")
    (synopsis "Haskell binding to libffi")
    (description
     "A binding to libffi, allowing C functions of types only known at runtime
to be called from Haskell.")
    (license license:bsd-3)))

(define-public ghc-libmpd
  (package
    (name "ghc-libmpd")
    (version "0.10.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "libmpd" version))
       (sha256
        (base32 "0ca3ispg92aj49fjzg5ykv7ggvdzb8lsvrh8rfh6fbnivqyzqn56"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "libmpd")))
    (inputs (list ghc-attoparsec ghc-data-default-class ghc-network
                  ghc-safe-exceptions ghc-utf8-string))
    (native-inputs (list ghc-quickcheck ghc-hspec hspec-discover))
    (home-page "http://github.com/vimus/libmpd-haskell#readme")
    (synopsis "Haskell client library for the Music Player Daemon")
    (description "This package provides a pure Haskell client library for the
Music Player Daemon.")
    (license license:expat)))

(define-public ghc-lib-parser
  (package
    (name "ghc-lib-parser")
    (version "9.12.2.20250421")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ghc-lib-parser" version))
       (sha256
        (base32 "1lscfnbpmyv0fahl9y33kwiypnp45iljljax69dxhlr9zdijv7x4"))))
    (build-system haskell-build-system)
    (native-inputs (list ghc-alex ghc-happy))
    (properties '((upstream-name . "ghc-lib-parser")))
    (home-page "https://github.com/digital-asset/ghc-lib")
    (synopsis "The GHC API, decoupled from GHC versions")
    (description
     "This library implements the GHC API.  It is like the
compiler-provided @code{ghc} package, but it can be loaded on many
compiler versions.")
    (license license:bsd-3)))

(define-public ghc-libxml
  (package
    (name "ghc-libxml")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "libxml" version))
       (sha256
        (base32
         "01zvk86kg726lf2vnlr7dxiz7g3xwi5a4ak9gcfbwyhynkzjmsfi"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "libxml")))
    (inputs
     (list libxml2))
    (arguments
     `(#:configure-flags
       `(,(string-append "--extra-include-dirs="
                         (assoc-ref %build-inputs "libxml2")
                         "/include/libxml2"))))
    (home-page "https://hackage.haskell.org/package/libxml")
    (synopsis "Haskell bindings to libxml2")
    (description
     "This library provides minimal Haskell binding to libxml2.")
    (license license:bsd-3)))

(define-public ghc-libyaml
  (package
    (name "ghc-libyaml")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "libyaml" version))
       (sha256
        (base32 "04zslsxp7fblxr9hq3512czgb9h81n27sdd4h2sy9d7bn74pwafn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "libyaml")))
    (inputs (list ghc-conduit ghc-resourcet ghc-libyaml-clib))
    (home-page "https://github.com/snoyberg/yaml#readme")
    (synopsis "Low-level, streaming YAML interface")
    (description "This package provides a Haskell wrapper over the
LibYAML C library.")
    (license license:bsd-3)))

(define-public ghc-libyaml-clib
  (package
    (name "ghc-libyaml-clib")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "libyaml-clib" version))
       (sha256
        (base32 "0jaif8y10ql8rmkfhm6nwfk65q8rnpk58a6j5cf4gksz9v2nnlh4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "libyaml-clib")))
    (home-page "https://github.com/hasufell/streamly-yaml#readme")
    (synopsis "libyaml clibs")
    (description "libyaml C source code for yaml bindings.")
    (license license:expat)))

(define-public ghc-lifted-async
  (package
    (name "ghc-lifted-async")
    (version "0.10.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lifted-async" version))
       (sha256
        (base32 "0m9xzlj9hrbs0j4sak2jdvm13l66mpr2k99xcv7rhy8wfssvz0f2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lifted-async")))
    (inputs (list ghc-async ghc-lifted-base ghc-transformers-base
                  ghc-monad-control ghc-constraints))
    (native-inputs (list ghc-hunit
                         ghc-tasty
                         ghc-tasty-expected-failure
                         ghc-tasty-hunit
                         ghc-tasty-th
                         ghc-tasty-hunit
                         ghc-tasty-th))
    (home-page "https://github.com/maoe/lifted-async")
    (synopsis
     "Run lifted IO operations asynchronously and wait for their results")
    (description
     "This package provides IO operations from @code{async} package lifted to any
instance of @code{MonadBase} or @code{MonadBaseControl}.")
    (license license:bsd-3)))

(define-public ghc-lifted-base
  (package
    (name "ghc-lifted-base")
    (version "0.2.3.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lifted-base" version))
       (sha256
        (base32 "1i8p8d3rkdh21bhgjjh32vd7qqjr7jq7p59qds0aw2kmargsjd61"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lifted-base")))
    (inputs (list ghc-transformers-base ghc-monad-control))
    (arguments (list #:tests? #f)) ; Tests depend on inexistent modules
    (home-page "https://github.com/basvandijk/lifted-base")
    (synopsis "Lifted IO operations from the base library")
    (description
     "Lifted-base exports IO operations from the @code{base}
library lifted to any instance of @code{MonadBase} or @code{MonadBaseControl}.
Note that not all modules from @code{base} are converted yet.  The package
includes a copy of the @code{monad-peel} test suite written by Anders
Kaseorg.")
    (license license:bsd-3)))

(define-public ghc-linear
  (package
    (name "ghc-linear")
    (version "1.23.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "linear" version))
       (sha256
        (base32 "05v91is8rwm34a86gra2q03d5f1klj4nmlxx8r3cx0gbkdhrvmmv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "linear")))
    (inputs (list ghc-adjunctions
                  ghc-base-orphans
                  ghc-bytes
                  ghc-cereal
                  ghc-distributive
                  ghc-hashable
                  ghc-indexed-traversable
                  ghc-lens
                  ghc-random
                  ghc-reflection
                  ghc-semigroupoids
                  ghc-tagged
                  ghc-transformers-compat
                  ghc-unordered-containers
                  ghc-vector
                  ghc-void
                  ghc-semigroups))
    (native-inputs (list ghc-simple-reflect ghc-tasty ghc-tasty-hunit
                         ghc-tasty-quickcheck ghc-quickcheck))
    (home-page "http://github.com/ekmett/linear/")
    (synopsis "Linear algebra library for Haskell")
    (description
     "This package provides types and combinators for linear algebra on free
vector spaces.")
    (license license:bsd-3)))

(define-public ghc-listlike
  (package
    (name "ghc-listlike")
    (version "4.7.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ListLike" version))
       (sha256
        (base32 "1i397ig6pz1aa404gfjz2s340mlc97x9yywprx3hp684q81ybirv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ListLike")))
    (inputs (list ghc-vector ghc-dlist ghc-fmlist ghc-utf8-string))
    (native-inputs (list ghc-hunit ghc-quickcheck))
    (home-page "http://github.com/ddssff/listlike")
    (synopsis "Generic support for list-like structures")
    (description
     "The ListLike module provides a common interface to the
various Haskell types that are list-like.  Predefined interfaces include
standard Haskell lists, Arrays, ByteStrings, and lazy ByteStrings.
Custom types can easily be made ListLike instances as well.

ListLike also provides for String-like types, such as String and
ByteString, for types that support input and output, and for types that
can handle infinite lists.")
    (license license:bsd-3)))

(define-public ghc-logging-facade
  (package
    (name "ghc-logging-facade")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "logging-facade" version))
       (sha256
        (base32 "0rn12j77gn3p84khrmbn5kq6fyj44i3z1hrdm29apikp7csv65ib"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "logging-facade")))
    (inputs (list ghc-call-stack))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "https://github.com/sol/logging-facade#readme")
    (synopsis "Simple logging abstraction that allows multiple back-ends")
    (description
     "This package provides a simple logging abstraction that allows multiple
back-ends.")
    (license license:expat)))

(define-public ghc-logging-facade-bootstrap
  (package
    (inherit ghc-logging-facade)
    (name "ghc-logging-facade-bootstrap")
    (arguments `(#:tests? #f))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-logict
  (package
    (name "ghc-logict")
    (version "0.8.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "logict" version))
       (sha256
        (base32 "1vxb8vyfhvl901kfywvr4czwmiz3ah4l9rlcrx7djs4f3kwfd6hq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "logict")))
    (native-inputs (list ghc-async ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/Bodigrim/logict#readme")
    (synopsis "Backtracking logic-programming monad")
    (description
     "This library provides a continuation-based, backtracking,
logic programming monad.  An adaptation of the two-continuation implementation
found in the paper \"Backtracking, Interleaving, and Terminating Monad
Transformers\" available @uref{http://okmij.org/ftp/papers/LogicT.pdf,
online}.")
    (license license:bsd-3)))

(define-public ghc-lrucache
  (package
    (name "ghc-lrucache")
    (version "1.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lrucache" version))
       (sha256
        (base32 "11avhnjnb89rvn2s41jhh5r40zgp7r6kb5c0hcfiibpabqvv46pw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lrucache")))
    (inputs (list ghc-contravariant))
    (arguments
     `(#:cabal-revision ("1"
                         "0v2wc5k2knvv5knbarzspmbzf657r52jyjm9kf6r4ylsmi9cbq0k")))
    (home-page "https://github.com/chowells79/lrucache")
    (synopsis "Implementation of a simple LRU cache")
    (description
     "This package contains a simple and pure @acronym{LRU, Least Recently
Used} cache.  The implementation is based on @code{Data.Map} from the
@code{containers} library.  Further, the package also containes a multiple IO
wrapper that enables atomic updates of the cache.")
    (license license:bsd-3)))

(define-public ghc-lucid
  (package
    (name "ghc-lucid")
    (version "2.11.20250303")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lucid" version))
       (sha256
        (base32 "1x24nzfjrwqwn1pl8qk4zxd0rndlha79k3swykkrqm24x5bj7rmv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lucid")))
    (inputs (list ghc-blaze-builder ghc-hashable ghc-mmorph))
    (native-inputs (list ghc-hunit ghc-hspec ghc-bifunctors))
    (home-page "https://github.com/chrisdone/lucid")
    (synopsis "Haskell DSL for rendering HTML")
    (description
     "Clear to write, read and edit Haskell DSL for HTML.

@itemize @bullet
@item
Names are consistent, and do not conflict with base or are keywords
(all have suffix @code{-}).
@item
Same combinator can be used for attributes and elements
(e.g. @code{style_}).
@end itemize")
    (license license:bsd-3)))

(define-public ghc-lzma
  (package
    (name "ghc-lzma")
    (version "0.0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lzma" version))
       (sha256
        (base32 "1wfwxa927607kfqy9qs8lxm11gngl3ikwwwm0d7i3km2j612g9c0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lzma")))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-hunit
                         ghc-tasty-quickcheck pkg-config))
    (arguments
     `(#:cabal-revision ("1"
                         "1lby8y4pf5kagdnywyjadv3hv9mfv3ygfdjrlh9gakyqgj8wjjhb")))
    (home-page "https://github.com/hvr/lzma")
    (synopsis "LZMA/XZ compression and decompression")
    (description
     "This package provides a pure interface for compressing and
decompressing LZMA streams of data represented as lazy @code{ByteString}s.  A
monadic incremental interface is provided as well.")
    (license license:bsd-3)))

(define-public ghc-lzma-conduit
  (package
    (name "ghc-lzma-conduit")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "lzma-conduit" version))
              (sha256
               (base32
                "1pmvmchrg429b2yk485x0066lxcr37cbyczlyp3ala2iaq8hm61z"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lzma-conduit")))
    (inputs (list ghc-conduit ghc-lzma ghc-resourcet))
    (native-inputs (list ghc-base-compat
                         ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2
                         ghc-hunit
                         ghc-quickcheck))
    (home-page "https://github.com/alphaHeavy/lzma-conduit")
    (synopsis "Conduit interface for lzma/xz compression")
    (description
     "This package provides a @code{Conduit} interface for the LZMA
compression algorithm used in the @code{.xz} file format.")
    (license license:bsd-3)))

(define-public ghc-magic
  (package
    (name "ghc-magic")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "magic" version))
       (sha256
        (base32 "10p0gjjjwr1dda7hahwrwn5njbfhl67arq3v3nf1jr3vymlkn75j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "magic")))
    (home-page "http://hackage.haskell.org/package/magic")
    (synopsis "Interface to C file/magic library")
    (description
     "This package provides a full-featured binding to the C libmagic library.
With it, you can determine the type of a file by examining its contents rather
than its name.")
    (license license:bsd-3)))

(define-public ghc-managed
  (package
    (name "ghc-managed")
    (version "1.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "managed" version))
       (sha256
        (base32 "0ngpk6zkpnc9hl9a46pgkc8ii4d7y06xci52birc5vy1a2fwl8is"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "managed")))
    (home-page "http://hackage.haskell.org/package/managed")
    (synopsis "Monad for managed values")
    (description
     "In Haskell you very often acquire values using the with... idiom using
functions of type (a -> IO r) -> IO r.  This idiom forms a Monad, which is a
special case of the ContT monad (from transformers) or the Codensity
monad (from kan-extensions).  The main purpose behind this package is to
provide a restricted form of these monads specialized to this unusually common
case.

The reason this package defines a specialized version of these types
is to:

@itemize
@item be more beginner-friendly,
@item simplify inferred types and error messages, and:
@item provide some additional type class instances that would otherwise be
orphan instances
@end itemize")
    (license license:bsd-3)))

(define-public ghc-markdown-unlit
  (package
    (name "ghc-markdown-unlit")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "markdown-unlit" version))
       (sha256
        (base32 "0nkvg33i8vkpb774lph306c7xwl8ib26ily5zjy37np43xc1i2yk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "markdown-unlit")))
    (inputs (list ghc-base-compat))
    (native-inputs (list ghc-quickcheck ghc-hspec ghc-silently
                         ghc-stringbuilder ghc-temporary ghc-hspec-discover))
    (home-page "https://github.com/sol/markdown-unlit#readme")
    (synopsis "Literate Haskell support for Markdown")
    (description "This package allows you to have a README.md that at the
same time is a literate Haskell program.")
    (license license:expat)))

(define-public ghc-math-functions
  (package
    (name "ghc-math-functions")
    (version "0.3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "math-functions" version))
       (sha256
        (base32 "1ypqza0v1qbm94yjj536ynh7njlcz36s1cj8c0slbx7ga3fxhh94"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "math-functions")))
    (inputs (list ghc-data-default-class ghc-vector ghc-primitive))
    (native-inputs (list ghc-vector-th-unbox
                         ghc-erf
                         ghc-quickcheck
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/haskell/math-functions")
    (synopsis "Special functions and Chebyshev polynomials for Haskell")
    (description
     "This Haskell library provides implementations of
special mathematical functions and Chebyshev polynomials.  These
functions are often useful in statistical and numerical computing.")
    (license license:bsd-2)))

(define-public ghc-megaparsec
  (package
    (name "ghc-megaparsec")
    (version "9.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "megaparsec" version))
       (sha256
        (base32 "15zc66lplq5382wayigcw9kql08nvp9403a8f9xaw85z4lv45vdr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "megaparsec")))
    (inputs (list ghc-case-insensitive ghc-parser-combinators ghc-scientific))
    (home-page "https://github.com/mrkkrp/megaparsec")
    (synopsis "Monadic parser combinators")
    (description
     "This is an industrial-strength monadic parser combinator library.
Megaparsec is a feature-rich package that strikes a nice balance between
speed, flexibility, and quality of parse errors.")
    (license license:bsd-2)))

(define-public ghc-memory
  (package
    (name "ghc-memory")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "memory" version))
       (sha256
        (base32 "0gifhvvq4za0sdlqjv38cwpnywiilmr8gmndwss82jz273vbckpx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "memory")))
    (inputs (list ghc-basement))
    (native-inputs (list ghc-foundation))
    (arguments
     `(#:cabal-revision ("1"
                         "1y8r0gn7vbk82bj6fyvxny0dwg9r2prnd8f9fkqvd01g6mkyjkcz")))
    (home-page "https://github.com/vincenthz/hs-memory")
    (synopsis "Memory abstractions for Haskell")
    (description
     "This package provides memory abstractions, such as chunk of memory,
polymorphic byte array management and manipulation functions.  It contains a
polymorphic byte array abstraction and functions similar to strict ByteString,
different type of byte array abstraction, raw memory IO operations (memory
set, memory copy, ..) and more")
    (license license:bsd-3)))

(define-public ghc-memotrie
  (package
    (name "ghc-memotrie")
    (version "0.6.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "MemoTrie" version))
       (sha256
        (base32 "08141kdn9d2md1nz0xfz5868rn4ya7li93k7f2rwdhga6vqsp9pp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "MemoTrie")))
    (inputs (list ghc-newtype-generics))
    (home-page "https://github.com/conal/MemoTrie")
    (synopsis "Trie-based memo functions")
    (description "This package provides a functional library for creating
efficient memo functions using tries.")
    (license license:bsd-3)))

(define-public ghc-microlens
  (package
    (name "ghc-microlens")
    (version "0.4.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "microlens" version))
       (sha256
        (base32 "0blj96kbgf0vivc8pv0gpvlaljxcffvxqm6zvr5n7c2g7rhjlyan"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "microlens")))
    (home-page "http://github.com/stevenfontanella/microlens")
    (synopsis "Provides a tiny lens Haskell library with no dependencies")
    (description
     "This Haskell package provides a lens library, just like
@code{ghc-lens}, but smaller.  It provides essential lenses and
traversals (like @code{_1} and @code{_Just}), as well as ones which are simply
nice to have (like @code{each}, @code{at}, and @code{ix}), and some
combinators (like @code{failing} and @code{singular}), but everything else is
stripped.  As the result, this package has no dependencies.")
    (license license:bsd-3)))

(define-public ghc-microlens-aeson
  (package
    (name "ghc-microlens-aeson")
    (version "2.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "microlens-aeson" version))
       (sha256
        (base32 "1cnmasig3wq132k1j89qj7g1bsamhpbjg492cynch4lb2r8bwxkc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "microlens-aeson")))
    (inputs (list ghc-aeson
                  ghc-hashable
                  ghc-microlens
                  ghc-vector
                  ghc-attoparsec
                  ghc-scientific))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "http://github.com/fosskers/microlens-aeson/")
    (synopsis "Law-abiding lenses for Aeson, using microlens")
    (description "This library provides law-abiding lenses for Aeson, using
microlens.")
    (license license:expat)))

(define-public ghc-microlens-ghc
  (package
    (name "ghc-microlens-ghc")
    (version "0.4.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "microlens-ghc" version))
       (sha256
        (base32 "0xdhlby2ygjdiwnmpd1d3rr74qv7syq356f6mmq3zj607ikkbv86"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "microlens-ghc")))
    (inputs (list ghc-microlens))
    (home-page "http://github.com/stevenfontanella/microlens")
    (synopsis "Use @code{microlens} with GHC libraries like @code{array}")
    (description
     "This library provides everything that @code{microlens}
provides plus instances to make @code{each}, @code{at}, and @code{ix}
usable with arrays, @code{ByteString}, and containers.  This package is
a part of the @uref{https://hackage.haskell.org/package/microlens,
microlens} family; see the readme
@uref{https://github.com/aelve/microlens#readme, on Github}.")
    (license license:bsd-3)))

(define-public ghc-microlens-mtl
  (package
    (name "ghc-microlens-mtl")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "microlens-mtl" version))
       (sha256
        (base32 "13w4fx1kslm8yy5liwrw59y47nq773bxmv3nwg7k79gjw5r9rmzv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "microlens-mtl")))
    (inputs (list ghc-microlens ghc-transformers-compat))
    (home-page "http://github.com/stevenfontanella/microlens")
    (synopsis "@code{microlens} support for Reader/Writer/State from mtl")
    (description
     "This package contains functions (like @code{view} or @code{+=}) which
work on @code{MonadReader}, @code{MonadWriter}, and @code{MonadState} from the
mtl package.  This package is a part of the
@uref{https://hackage.haskell.org/package/microlens, microlens} family; see the
readme @uref{https://github.com/aelve/microlens#readme, on Github}.")
    (license license:bsd-3)))

(define-public ghc-microlens-platform
  (package
    (name "ghc-microlens-platform")
    (version "0.4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "microlens-platform" version))
       (sha256
        (base32 "0d37rzskqr94grq75a00wgwlz8wrm6awxjw0r396hwwjl8abwipm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "microlens-platform")))
    (inputs (list ghc-hashable
                  ghc-microlens
                  ghc-microlens-ghc
                  ghc-microlens-mtl
                  ghc-microlens-th
                  ghc-unordered-containers
                  ghc-vector))
    (home-page "http://github.com/stevenfontanella/microlens")
    (synopsis "Feature-complete microlens")
    (description
     "This package exports a module which is the recommended starting point
for using @uref{https://hackage.haskell.org/package/microlens, microlens} if
you aren't trying to keep your dependencies minimal.  By importing
@code{Lens.Micro.Platform} you get all functions and instances from
@uref{https://hackage.haskell.org/package/microlens, microlens},
@uref{https://hackage.haskell.org/package/microlens-th, microlens-th},
@uref{https://hackage.haskell.org/package/microlens-mtl, microlens-mtl},
@uref{https://hackage.haskell.org/package/microlens-ghc, microlens-ghc}, as
well as instances for @code{Vector}, @code{Text}, and @code{HashMap}.  The
minor and major versions of @code{microlens-platform} are incremented whenever
the minor and major versions of any other @code{microlens} package are
incremented, so you can depend on the exact version of
@code{microlens-platform} without specifying the version of @code{microlens}
you need.  This package is a part of the
@uref{https://hackage.haskell.org/package/microlens, microlens} family; see the
readme @uref{https://github.com/aelve/microlens#readme, on Github}.")
    (license license:bsd-3)))

(define-public ghc-microlens-th
  (package
    (name "ghc-microlens-th")
    (version "0.4.3.17")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "microlens-th" version))
       (sha256
        (base32 "150a9kgab4l6324dkf9vpvgbwarw89xfhcdhdj8awcm3gh12qxhx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "microlens-th")))
    (inputs (list ghc-microlens ghc-th-abstraction))
    (native-inputs (list ghc-tagged))
    (home-page "http://github.com/stevenfontanella/microlens")
    (synopsis "Automatic generation of record lenses for
@code{ghc-microlens}")
    (description
     "This Haskell package lets you automatically generate lenses
for data types; code was extracted from the lens package, and therefore
generated lenses are fully compatible with ones generated by lens (and can be
used both from lens and microlens).")
    (license license:bsd-3)))

(define-public ghc-microstache
  (package
    (name "ghc-microstache")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "microstache" version))
       (sha256
        (base32 "13w9macbi0krdilyp7dvzcg48di89biyz1axd7vvl3ylggjr1wim"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "microstache")))
    (inputs (list ghc-aeson ghc-unordered-containers ghc-vector))
    (native-inputs (list ghc-base-orphans
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-base-orphans
                         ghc-tasty
                         ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "13hqvjzb7k03bxnvyyflfw1rs6hyc3z16b7n2r52xsk32lrmz9c6")))
    (home-page "https://github.com/haskellari/microstache")
    (synopsis "Mustache templates for Haskell")
    (description
     "This library provides Mustache templates for Haskell based on the
@code{stache} library using @code{parsec} instead of @code{megaparsec}.")
    (license license:bsd-3)))

(define-public ghc-missingh
  (package
    (name "ghc-missingh")
    (version "1.6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "MissingH" version))
       (sha256
        (base32 "17ckc5hck9ng9rqx2afj1xac0d7m1p0vqfc5mdmmlkg5nza432is"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "MissingH")))
    (inputs (list ghc-hslogger
                  ghc-old-locale
                  ghc-old-time
                  ghc-regex-compat
                  ghc-network-bsd
                  ghc-network))
    (native-inputs (list ghc-hunit))
    (home-page "http://hackage.haskell.org/package/MissingH")
    (synopsis "Large utility library")
    (description
     "MissingH is a library of all sorts of utility functions for Haskell
programmers.  It is written in pure Haskell and thus should be extremely
portable and easy to use.")
    (license license:bsd-3)))

(define-public ghc-mmap
  (package
    (name "ghc-mmap")
    (version "0.5.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mmap" version))
       (sha256
        (base32 "1y5mk3yf4b8r6rzmlx1xqn4skaigrqnv08sqq0v7r3nbw42bpz2q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mmap")))
    (home-page "http://hackage.haskell.org/package/mmap")
    (synopsis "Memory mapped files for Haskell")
    (description
     "This library provides a wrapper to @code{mmap}, allowing files or
devices to be lazily loaded into memory as strict or lazy @code{ByteStrings},
@code{ForeignPtrs} or plain @code{Ptrs}, using the virtual memory subsystem to
do on-demand loading.")
    (license license:bsd-3)))

(define-public ghc-mmorph
  (package
    (name "ghc-mmorph")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mmorph" version))
       (sha256
        (base32 "1rjclyxyr5ajnpmkrlwap77h5fmdwys8bpwfj0n87v33hh1dcn8f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mmorph")))
    (inputs (list ghc-transformers-compat ghc-fail))
    (home-page "http://hackage.haskell.org/package/mmorph")
    (synopsis "Monad morphisms")
    (description
     "This library provides monad morphism utilities, most commonly used for
manipulating monad transformer stacks.")
    (license license:bsd-3)))

(define-public ghc-mockery
  (package
    (name "ghc-mockery")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mockery" version))
       (sha256
        (base32 "09ypgm3z69gq8mj6y66ss58kbjnk15r8frwcwbqcfbfksfnfv8dp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mockery")))
    (inputs (list ghc-base-compat ghc-temporary ghc-logging-facade))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "http://hackage.haskell.org/package/mockery")
    (synopsis "Support functions for automated testing")
    (description
     "The mockery package provides support functions for automated testing.")
    (license license:expat)))

(define-public ghc-mockery-bootstrap
  (package
    (inherit ghc-mockery)
    (name "ghc-mockery-bootstrap")
    (arguments `(#:tests? #f))
    (inputs (modify-inputs (package-inputs ghc-mockery)
              (replace "ghc-logging-facade" ghc-logging-facade-bootstrap)
              (replace "ghc-temporary" ghc-temporary-bootstrap)))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-monad-control
  (package
    (name "ghc-monad-control")
    (version "1.0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "monad-control" version))
       (sha256
        (base32 "0g3if9km8ik80bcy130a826ig9wlk4bnf0qli3vmwdwr9nhaw2xf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "monad-control")))
    (inputs (list ghc-transformers-base ghc-transformers-compat))
    (home-page "https://github.com/basvandijk/monad-control")
    (synopsis "Monad transformers to lift control operations like exception
catching")
    (description
     "This package defines the type class @code{MonadBaseControl},
a subset of @code{MonadBase} into which generic control operations such as
@code{catch} can be lifted from @code{IO} or any other base monad.")
    (license license:bsd-3)))

(define-public ghc-monad-logger
  (package
    (name "ghc-monad-logger")
    (version "0.3.42")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "monad-logger" version))
       (sha256
        (base32 "042h2bmgzl2jjcnqbb6nj64kpwgzik0spx11ks5gb89asin488v6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "monad-logger")))
    (inputs (list ghc-conduit
                  ghc-conduit-extra
                  ghc-fast-logger
                  ghc-lifted-base
                  ghc-monad-control
                  ghc-monad-loops
                  ghc-resourcet
                  ghc-stm-chans
                  ghc-transformers-base
                  ghc-transformers-compat
                  ghc-unliftio-core))
    (home-page "https://github.com/snoyberg/monad-logger#readme")
    (synopsis "Provides a class of monads which can log messages for Haskell")
    (description
     "This Haskell package uses a monad transformer approach
for logging.

This package provides Template Haskell functions for determining source
code locations of messages.")
    (license license:expat)))

(define-public ghc-monad-loops
  (package
    (name "ghc-monad-loops")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "monad-loops" version))
       (sha256
        (base32
         "062c2sn3hc8h50p1mhqkpyv6x8dydz2zh3ridvlfjq9nqimszaky"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "monad-loops")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/mokus0/monad-loops")
    (synopsis "Monadic loops for Haskell")
    (description "This Haskell package provides some useful control
operators for looping.")
    (license license:public-domain)))

(define-public ghc-monad-par
  (package
    (name "ghc-monad-par")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "monad-par" version))
       (sha256
        (base32 "10b2wir7g7pr7gf3d8azkv829fl2fmxzgy8wc4livlwks0vax9jf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "monad-par")))
    (inputs (list ghc-abstract-par ghc-abstract-deque ghc-monad-par-extras
                  ghc-mwc-random ghc-parallel))
    (native-inputs (list ghc-quickcheck
                         ghc-hunit
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2
                         ghc-test-framework
                         ghc-test-framework-th))
    (arguments
     `(#:cabal-revision ("1"
                         "0n329g47a5bfx21fd2j1w2y0ngka87mpfsy1c6yxw57s5x0wda77")))
    (home-page "https://github.com/simonmar/monad-par")
    (synopsis "Haskell library for parallel programming based on a monad")
    (description
     "The @code{Par} monad offers an API for parallel
programming.  The library works for parallelising both pure and @code{IO}
computations, although only the pure version is deterministic.  The default
implementation provides a work-stealing scheduler and supports forking tasks
that are much lighter weight than IO-threads.")
    (license license:bsd-3)))

(define-public ghc-monad-par-extras
  (package
    (name "ghc-monad-par-extras")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "monad-par-extras" version))
       (sha256
        (base32 "0bl4bd6jzdc5zm20q1g67ppkfh6j6yn8fwj6msjayj621cck67p2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "monad-par-extras")))
    (inputs (list ghc-abstract-par ghc-cereal ghc-random))
    (home-page "https://github.com/simonmar/monad-par")
    (synopsis "Combinators and extra features for Par monads for Haskell")
    (description
     "This Haskell package provides additional data structures,
and other added capabilities layered on top of the @code{Par} monad.")
    (license license:bsd-3)))

(define-public ghc-monad-parallel
  (package
    (name "ghc-monad-parallel")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "monad-parallel" version))
       (sha256
        (base32 "1j905cwc440g7rvbhsdkqf50ag7p2bi6cy2rqsk918rn80fqqra4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "monad-parallel")))
    (inputs (list ghc-parallel ghc-transformers-compat))
    (home-page "https://hub.darcs.net/blamario/SCC.wiki/")
    (synopsis "Parallel execution of monadic computations")
    (description
     "This package defines classes of monads that can perform multiple
executions in parallel and combine their results.  For any monad that's an
instance of the class, the package re-implements a subset of the
@code{Control.Monad} interface, but with parallel execution.")
    (license license:bsd-3)))

(define-public ghc-monadrandom
  (package
    (name "ghc-monadrandom")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "MonadRandom" version))
       (sha256
        (base32 "0sk61xbpagiwfpmfskysc2f7y05rpmaz0q8hr0a7m6f2xyw1rw02"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "MonadRandom")))
    (inputs (list ghc-transformers-compat ghc-primitive ghc-random ghc-fail))
    (home-page "http://hackage.haskell.org/package/MonadRandom")
    (synopsis "Random-number generation monad for Haskell")
    (description "This Haskell package provides support for computations
which consume random values.")
    (license license:bsd-3)))

(define-public ghc-monads-tf
  (package
    (name "ghc-monads-tf")
    (version "0.3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "monads-tf" version))
       (sha256
        (base32 "00jzz9lqpz3s5xwvmc5xi300jkkjv9bk62k0jgwnqfv6py9x5g11"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "monads-tf")))
    (arguments
     `(#:cabal-revision ("2"
                         "1wyfl2w8pfkg233180qlk65kka41iqb2hgxsyx700sfqd4p9vs36")))
    (home-page "https://github.com/typeclasses/monads-tf")
    (synopsis "Monad classes, using type families")
    (description
     "Monad classes using type families, with instances for various monad transformers,
inspired by the paper 'Functional Programming with Overloading and Higher-Order
Polymorphism', by Mark P Jones.  This package is almost a compatible replacement for
the @code{mtl-tf} package.")
    (license license:bsd-3)))

(define-public ghc-mono-traversable
  (package
    (name "ghc-mono-traversable")
    (version "1.0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mono-traversable" version))
       (sha256
        (base32 "1kf5qwicr8ld4bgkzijxwpzppfwdr4wsl1rg8009a5n06jikxnad"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mono-traversable")))
    (inputs (list ghc-hashable ghc-split ghc-unordered-containers ghc-vector
                  ghc-vector-algorithms))
    (native-inputs (list ghc-hunit ghc-quickcheck ghc-foldl ghc-hspec))
    (home-page "https://github.com/snoyberg/mono-traversable#readme")
    (synopsis
     "Haskell classes for mapping, folding, and traversing monomorphic
containers")
    (description
     "This Haskell package provides Monomorphic variants of the
Functor, Foldable, and Traversable typeclasses.  If you understand Haskell's
basic typeclasses, you understand mono-traversable.  In addition to what
you are used to, it adds on an IsSequence typeclass and has code for marking
data structures as non-empty.")
    (license license:expat)))

(define-public ghc-monoid-extras
  (package
    (name "ghc-monoid-extras")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "monoid-extras" version))
       (sha256
        (base32 "0c25hcvsw6xqdgy6p8q5jdgxmnqhiq7z2hm43cn0yh9nk2y294ws"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "monoid-extras")))
    (inputs (list ghc-groups ghc-semigroupoids))
    (home-page "http://hackage.haskell.org/package/monoid-extras")
    (synopsis "Various extra monoid-related definitions and utilities")
    (description
     "This package provides various extra monoid-related
definitions and utilities, such as monoid actions, monoid coproducts,
semi-direct products, \"deletable\" monoids, \"split\" monoids, and
\"cut\" monoids.")
    (license license:bsd-3)))

(define-public ghc-mountpoints
  (package
    (name "ghc-mountpoints")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mountpoints" version))
       (sha256
        (base32 "1hnm31pqcffphyc463wf0vbik9fzm5lb2r4wjdc1y4dqzmjdzz37"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mountpoints")))
    (home-page "http://hackage.haskell.org/package/mountpoints")
    (synopsis "Haskell library for listing mount points")
    (description "This library provides Haskell bindings for checking
currently mounted filesystems.")
    (license license:lgpl2.1+)))

(define-public ghc-mtl-compat
  (package
    (name "ghc-mtl-compat")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mtl-compat" version))
       (sha256
        (base32 "17iszr5yb4f17g8mq6i74hsamii8z6m2qfsmgzs78mhiwa7kjm8r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mtl-compat")))
    (home-page "https://github.com/haskell-compat/mtl-compat")
    (synopsis "Backported Control.Monad.Except module from mtl")
    (description
     "This package backports the Control.Monad.Except module from mtl (if
using mtl-2.2.0.1 or earlier), which reexports the ExceptT monad transformer
and the MonadError class.

This package should only be used if there is a need to use the
Control.Monad.Except module specifically.  If you just want the mtl class
instances for ExceptT, use transformers-compat instead, since mtl-compat does
nothing but reexport the instances from that package.

Note that unlike how mtl-2.2 or later works, the Control.Monad.Except
module defined in this package exports all of ExceptT's monad class instances.
Therefore, you may have to declare @code{import Control.Monad.Except ()} at
the top of your file to get all of the ExceptT instances in scope.")
    (license license:bsd-3)))

(define-public ghc-murmur-hash
  (package
    (name "ghc-murmur-hash")
    (version "0.1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "murmur-hash" version))
       (sha256
        (base32 "1zkd575b6rgs0js4vsr56hy7xhjmvbwxibiwcm6q2wgz5c9igaki"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "murmur-hash")))
    (home-page "https://github.com/nominolo/murmur-hash")
    (synopsis "MurmurHash2 implementation for Haskell")
    (description
     "This package provides an implementation of MurmurHash2, a good, fast,
general-purpose, non-cryptographic hashing function.  See
@url{https://sites.google.com/site/murmurhash/} for details.  This
implementation is pure Haskell, so it might be a bit slower than a C FFI
binding.")
    (license license:bsd-3)))

(define-public ghc-mwc-random
  (package
    (name "ghc-mwc-random")
    (version "0.15.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mwc-random" version))
       (sha256
        (base32 "0cs12ycr966ff5k4z515rqxnw3a0hrjf5dafm8k96469ww3anhsq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mwc-random")))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'configure 'relax-dependency-versions
             (lambda _
               (substitute* "mwc-random.cabal"
                 (("doctest [<>=0-9. &|]*") "doctest"))))
           ;; The doctests require GHC_PACKAGE_PATH but Setup.hs fails
           ;; if it is defined, so we run them separately
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (if tests?
                 (let* ((tmpdir (or (getenv "TMP") "/tmp"))
                        (package-db (string-append tmpdir "/package.conf.d")))
                   (invoke "runhaskell" "Setup.lhs" "test" "mwc-prop-tests")
                   (setenv "GHC_PACKAGE_PATH" package-db)
                   (invoke "./dist/build/mwc-doctests/mwc-doctests")
                   (unsetenv "GHC_PACKAGE_PATH"))
                 (format #t "Testsuite not run.%~")))))))
    (inputs (list ghc-primitive ghc-random ghc-vector ghc-math-functions))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-quickcheck
                         ghc-tasty-hunit ghc-doctest))
    (home-page "https://github.com/haskell/mwc-random")
    (synopsis "Random number generation library for Haskell")
    (description
     "This Haskell package contains code for generating
high quality random numbers that follow either a uniform or normal
distribution.  The generated numbers are suitable for use in
statistical applications.

The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)
multiply-with-carry generator, which has a period of 2^{8222} and
fares well in tests of randomness.  It is also extremely fast,
between 2 and 3 times faster than the Mersenne Twister.")
    (license license:bsd-3)))

(define-public ghc-nats
  (package
    (name "ghc-nats")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "nats" version))
       (sha256
        (base32 "1v40drmhixck3pz3mdfghamh73l4rp71mzcviipv1y8jhrfxilmr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "nats")))
    (inputs (list ghc-hashable))
    (arguments
     `(#:cabal-revision ("4"
                         "0qccypqkfs7hi0v2bsjfqfhpi2jgsnpfwi9vmcqh7jxk5g08njk0")))
    (home-page "http://github.com/ekmett/nats/")
    (synopsis "Natural numbers")
    (description "This library provides the natural numbers for Haskell.")
    (license license:bsd-3)))

(define-public ghc-nats-bootstrap
  (package
    (inherit ghc-nats)
    (name "ghc-nats-bootstrap")
    (inputs
     `(("ghc-hashable" ,ghc-hashable-bootstrap)))
    (properties '((hidden? #t)))))

(define-public ghc-ncurses
  (package
    (name "ghc-ncurses")
    (version "0.2.16")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "ncurses" version))
        (sha256
         (base32
          "0gsyyaqyh5r9zc0rhwpj5spyd6i4w2vj61h4nihgmmh0yyqvf3z5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ncurses")))
    (arguments
     '(#:extra-directories ("ncurses")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-includes
           (lambda _
             (substitute* '("cbits/hsncurses-shim.h"
                            "lib/UI/NCurses.chs"
                            "lib/UI/NCurses/Enums.chs"
                            "lib/UI/NCurses/Panel.chs")
               (("<ncursesw/") "<"))
             ;; KEY_EVENT doesn't appear in our ncurses.h
             (substitute* "lib/UI/NCurses/Enums.chs"
               ((".*KEY_EVENT.*") "")))))
       #:cabal-revision
       ("1"
        "1wfdy716s5p1sqp2gsg43x8wch2dxg0vmbbndlb2h3d8c9jzxnca")))
    (inputs (list ncurses))
    (native-inputs (list ghc-c2hs))
    (home-page "https://john-millikin.com/software/haskell-ncurses/")
    (synopsis "Modernised bindings to GNU ncurses")
    (description "GNU ncurses is a library for creating command-line application
with pseudo-graphical interfaces.  This package is a nice, modern binding to GNU
ncurses.")
    (license license:gpl3)))

(define-public ghc-network
  (package
    (name "ghc-network")
    (version "3.2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "network" version))
       (sha256
        (base32 "1j2zbjqpnrwkhi5673by8z1dp92mh3glik8a4r7jcxvxxdg8wy2i"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "network")))
    (native-inputs (list ghc-hunit ghc-temporary ghc-hspec ghc-quickcheck
                         hspec-discover))
    (home-page "https://github.com/haskell/network")
    (synopsis "Low-level networking interface")
    (description "This package provides a low-level networking interface.")
    (license license:bsd-3)))

(define-public ghc-network-bsd
  (package
    (name "ghc-network-bsd")
    (version "2.8.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "network-bsd" version))
       (sha256
        (base32 "0kid0811lv4x761fd5gv6lsc8p5j2bn41rfd366pjb642p562jfr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "network-bsd")))
    (inputs (list ghc-network))
    (arguments
     `(#:cabal-revision ("6"
                         "173jmmqfmqwv6am1yk127ixbdqzng9sjj45v9f9g3cvf3kw6fy6c")))
    (home-page "https://github.com/haskell/network-bsd")
    (synopsis "POSIX network database (<netdb.h>) API")
    (description "This package provides Haskell bindings to the the POSIX
network database (<netdb.h>) API.")
    (license license:bsd-3)))

(define-public ghc-network-byte-order
  (package
    (name "ghc-network-byte-order")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "network-byte-order" version))
       (sha256
        (base32 "0yc6s2zdxkr2awmf56vqwds417ix8rgq33ffsk44wdk7gyny0328"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "network-byte-order")))
    (home-page "http://hackage.haskell.org/package/network-byte-order")
    (synopsis "Network byte order utilities")
    (description "This library provides peek and poke functions for network
byte order.")
    (license license:bsd-3)))

(define-public ghc-network-info
  (package
    (name "ghc-network-info")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "network-info" version))
       (sha256
        (base32 "015lm3b8n8sb16qsffjxz1jvijyy0z600ch0sm8h6a685wqqhbcv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "network-info")))
    (home-page "http://github.com/jacobstanley/network-info")
    (synopsis "Access the local computer's basic network configuration")
    (description
     "This Haskell library provides simple read-only access to the
local computer's networking configuration.  It is currently capable of
getting a list of all the network interfaces and their respective
IPv4, IPv6 and MAC addresses.")
    (license license:bsd-3)))

(define-public ghc-network-multicast
  (package
    (name "ghc-network-multicast")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "network-multicast" version))
       (sha256
        (base32 "0whvi0pbwjy6dbwfdf9rv1j3yr3lcmfp3q7a8pwq63g537l4l2l3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "network-multicast")))
    (inputs (list ghc-network ghc-network-bsd))
    (home-page "http://hackage.haskell.org/package/network-multicast")
    (synopsis "Simple multicast library for Haskell")
    (description
     "This package provides the Network.Multicast Haskell module for
sending UDP datagrams over multicast (class D) addresses.")
    (license license:public-domain)))

(define-public ghc-network-uri
  (package
    (name "ghc-network-uri")
    (version "2.6.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "network-uri" version))
       (sha256
        (base32 "0a3jg6aykwm1yw32nh137hi6r86w2640xwl1p18352bf29rqj64w"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "network-uri")))
    (inputs (list ghc-th-compat))
    (native-inputs (list ghc-hunit ghc-tasty ghc-tasty-hunit
                         ghc-tasty-quickcheck ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "12350bi5sqamk1mdgkm757q3p2q96bvhm2yvl98mcawnfdrvbzvg")))
    (home-page "https://github.com/haskell/network-uri")
    (synopsis "Library for URI manipulation")
    (description
     "This package provides an URI manipulation interface.  In
@code{network-2.6} the @code{Network.URI} module was split off from the
@code{network} package into this package.")
    (license license:bsd-3)))

(define-public ghc-newtype-generics
  (package
    (name "ghc-newtype-generics")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "newtype-generics" version))
       (sha256
        (base32 "0km7cp041bgdgrxrbrawz611mcylxp943880a2yg228a09961b51"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "newtype-generics")))
    (native-inputs (list ghc-hspec hspec-discover))
    (arguments
     `(#:cabal-revision ("5"
                         "0arlqrq482ai3j6cwgb8kc34zc8y3ghg8fgrxxcnw752hzrl1g71")))
    (home-page "http://github.com/sjakobi/newtype-generics")
    (synopsis "Typeclass and set of functions for working with newtypes")
    (description
     "The @code{Newtype} typeclass represents the packing and
unpacking of a newtype, and allows you to operate under that newtype with
functions such as @code{ala}.  Generics support was added in version 0.4,
making this package a full replacement for the original newtype package,
and an alternative to newtype-th.")
    (license license:bsd-3)))

(define-public ghc-non-negative
  (package
    (name "ghc-non-negative")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "non-negative" version))
       (sha256
        (base32 "0f01q916dzkl1i0v15qrw9cviycki5g3fgi6x8gs45iwbzssq52n"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "non-negative")))
    (inputs (list ghc-semigroups ghc-utility-ht ghc-quickcheck))
    (home-page "http://code.haskell.org/~thielema/non-negative/")
    (synopsis "Non-negative numbers class")
    (description
     "This library provides a class for non-negative numbers,
a wrapper which can turn any ordered numeric type into a member of that
class, and a lazy number type for non-negative numbers (a generalization
of Peano numbers).")
    (license license:gpl3+)))

(define-public ghc-nonce
  (package
    (name "ghc-nonce")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "nonce" version))
       (sha256
        (base32 "1q9ph0aq51mvdvydnriqd12sfin36pfb8f588zgac1ybn8r64ksb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "nonce")))
    (inputs (list ghc-base64-bytestring ghc-entropy ghc-unliftio
                  ghc-unliftio-core))
    (arguments
     `(#:cabal-revision ("2"
                         "09xvg4lpmb1hw153afhbjrdg9v3npfwpdfhpv5y8b0qvb4zi3n9q")))
    (home-page "https://github.com/prowdsponsor/nonce")
    (synopsis "Generate cryptographic nonces in Haskell")
    (description
     "A nonce is an arbitrary number used only once in a cryptographic
communication.  This package contain helper functions for generating nonces.
There are many kinds of nonces used in different situations.  It's not
guaranteed that by using the nonces from this package you won't have any
security issues.  Please make sure that the nonces generated via this
package are usable on your design.")
    (license license:bsd-3)))

(define-public ghc-numeric-extras
  (package
    (name "ghc-numeric-extras")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "numeric-extras" version))
       (sha256
        (base32 "1mk11c0gz1yjy5b8dvq6czfny57pln0bs7x28fz38qyr44872067"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "numeric-extras")))
    (home-page "http://github.com/ekmett/numeric-extras")
    (synopsis "Useful tools from the C standard library")
    (description "This library provides some useful tools from the C
standard library.")
    (license license:bsd-3)))

(define-public ghc-objectname
  (package
    (name "ghc-objectname")
    (version "1.1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ObjectName" version))
       (sha256
        (base32 "0xdkfc97salzj5s3fvmwk4k0097dcd8c4xcr5ghhv9mz0wcxm9gz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ObjectName")))
    (home-page "https://github.com/svenpanne/ObjectName")
    (synopsis "Helper library for Haskell OpenGL")
    (description
     "This tiny package contains the class ObjectName, which
corresponds to the general notion of explicitly handled identifiers for API
objects, e.g. a texture object name in OpenGL or a buffer object name in
OpenAL.")
    (license license:bsd-3)))

(define-public ghc-old-locale
  (package
    (name "ghc-old-locale")
    (version "1.0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "old-locale" version))
       (sha256
        (base32 "0l3viphiszvz5wqzg7a45zp40grwlab941q5ay29iyw8p3v8pbyv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "old-locale")))
    (arguments
     `(#:cabal-revision ("2"
                         "04b9vn007hlvsrx4ksd3r8r3kbyaj2kvwxchdrmd4370qzi8p6gs")))
    (home-page "http://hackage.haskell.org/package/old-locale")
    (synopsis "Adapt to locale conventions")
    (description
     "This package provides the ability to adapt to locale conventions such as
date and time formats.")
    (license license:bsd-3)))

(define-public ghc-old-time
  (package
    (name "ghc-old-time")
    (version "1.1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "old-time" version))
       (sha256
        (base32 "0vqn9iifrg601734pvvl3khyxv2y7dxr25z3a9pnfjljgdzyn8hy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "old-time")))
    (inputs (list ghc-old-locale))
    (home-page "http://hackage.haskell.org/package/old-time")
    (synopsis "Time compatibility library for Haskell")
    (description
     "Old-time is a package for backwards compatibility with the
old @code{time} library.  For new projects, the newer
@uref{https://hackage.haskell.org/package/time, time library} is recommended.")
    (license license:bsd-3)))

(define-public ghc-only
  (package
    (name "ghc-only")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Only" version))
       (sha256
        (base32 "0rdj3a629fk2vp121jq8mf2smkblrz5w3cxhlsyx6my2x29s2ymb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Only")))
    (arguments
     `(#:cabal-revision ("2"
                         "01rvhsm4gyvjpgakrj7nzlfb1bjhkkx87xj3hng2x00g3qc3s0y6")))
    (home-page "http://hackage.haskell.org/package/Only")
    (synopsis "The 1-tuple type or single-value collection")
    (description
     "This package provides a canonical anonymous 1-tuple type missing from
Haskell for attaching typeclass instances.

There is also the @url{https://hackage.haskell.org/package/OneTuple, OneTuple
package} which by using a boxed @code{data}-type provides a 1-tuple type which
has laziness properties which are more faithful to the ones of Haskell's
native tuples; whereas the primary purpose of @code{Only} is to provide the
traditionally so named type-wrapper for attaching typeclass instances.")
    (license license:bsd-3)))

(define-public ghc-opengl
  (package
    (name "ghc-opengl")
    (version "3.0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "OpenGL" version))
       (sha256
        (base32 "069fg8jcxqq2z9iikynd8vi3jxm2b5y3qywdh4bdviyzab3zy1as"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "OpenGL")))
    (inputs (list ghc-objectname ghc-statevar ghc-openglraw ghc-gluraw glu))
    (arguments
     `(#:cabal-revision ("4"
                         "121998ckmpama7cfl63m5nvin46cqzip3ypzmy7v4y96ks6s1n5w")))
    (home-page "https://wiki.haskell.org/OpenGL")
    (synopsis "Haskell bindings for the OpenGL graphics system")
    (description
     "This package provides Haskell bindings for the OpenGL
graphics system (GL, version 4.5) and its accompanying utility library (GLU,
version 1.3).")
    (license license:bsd-3)))

(define-public ghc-openglraw
  (package
    (name "ghc-openglraw")
    (version "3.3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "OpenGLRaw" version))
       (sha256
        (base32 "07nk0rgm6jcxz6yshwhv5lj5frs6371w3hdjxwa4biws2kmbs6hj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "OpenGLRaw")))
    (inputs (list ghc-fixed ghc-half glu))
    (arguments
     `(#:cabal-revision ("3"
                         "1wbqfgcswng27v76r7rgy1zlb4wpap2ibjf3hbcrdz39sbxlbdq1")))
    (home-page "http://www.haskell.org/haskellwiki/Opengl")
    (synopsis "Raw Haskell bindings for the OpenGL graphics system")
    (description
     "OpenGLRaw is a raw Haskell binding for the OpenGL 4.5
graphics system and lots of OpenGL extensions.  It is basically a 1:1 mapping
of OpenGL's C API, intended as a basis for a nicer interface.  OpenGLRaw
offers access to all necessary functions, tokens and types plus a general
facility for loading extension entries.  The module hierarchy closely mirrors
the naming structure of the OpenGL extensions, making it easy to find the
right module to import.  All API entries are loaded dynamically, so no special
C header files are needed for building this package.  If an API entry is not
found at runtime, a userError is thrown.")
    (license license:bsd-3)))

(define-public ghc-operational
  (package
    (name "ghc-operational")
    (version "0.2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "operational" version))
       (sha256
        (base32 "1dx6vpmg21fskxyz12ba26hffk25b2qk9sznqfczgaamn6rahzc5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "operational")))
    (inputs (list ghc-random))
    (arguments
     `(#:cabal-revision ("1"
                         "0hdqwjm1jp6f8n8qglg9ylz07sbhrc7cm4kvcglymi2s4i9mdsai")))
    (home-page "https://github.com/HeinrichApfelmus/operational")
    (synopsis
     "Implementation of difficult monads made easy with operational semantics")
    (description
     "This library makes it easy to implement monads with tricky control
flow.  This is useful for: writing web applications in a sequential style,
programming games with a uniform interface for human and AI players and easy
replay capabilities, implementing fast parser monads, designing monadic
DSLs, etc.")
    (license license:bsd-3)))

(define-public ghc-optional-args
  (package
    (name "ghc-optional-args")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "optional-args" version))
       (sha256
        (base32 "1r5hhn6xvc01grggxdyy48daibwzi0aikgidq0ahpa6bfynm8d1f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "optional-args")))
    (arguments
     `(#:cabal-revision ("2"
                         "1f3j092lk2qa6hi2p4iciyyxg27mzm37dlcw5hg3ch2wh6jw0a37")))
    (home-page "http://hackage.haskell.org/package/optional-args")
    (synopsis "Optional function arguments")
    (description
     "This library provides a type for specifying @code{Optional} function
arguments.")
    (license license:bsd-3)))

(define-public ghc-options
  (package
    (name "ghc-options")
    (version "1.2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "options" version))
       (sha256
        (base32 "0jjz7b69qrsrbfz07xq43v70habxk8sj2gdlbkwh0gbifyhqykbf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "options")))
    (inputs (list ghc-monads-tf))
    (native-inputs (list ghc-hspec ghc-patience))
    (arguments
     `(#:cabal-revision ("2"
                         "1aqs45xs42hzga0k9fryrj10my7crgbnmyip58vxrfd0s43rqdxq")))
    (home-page "https://github.com/typeclasses/options/")
    (synopsis "Powerful and easy-to-use command-line option parser")
    (description
     "The @code{options} package lets library and application developers
easily work with command-line options.")
    (license license:expat)))

;; See ghc-system-filepath-bootstrap, chell and chell-quickcheck are required for tests.
(define-public ghc-options-bootstrap
  (package
    (name "ghc-options-bootstrap")
    (version "1.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/options/options-"
             version ".tar.gz"))
       (sha256
        (base32
         "0qjs0v1ny52w51n5582d4z8wy9h6n0zw1xb5dh686ff5wadflgi8"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("ghc-monads-tf" ,ghc-monads-tf)))
    (home-page "https://john-millikin.com/software/haskell-options/")
    (synopsis "Powerful and easy-to-use command-line option parser")
    (description
     "The @code{options} package lets library and application developers
easily work with command-line options.")
    (properties '((hidden? . #t)))            ;for bootstrapping purposes only
    (license license:expat)))


(define-public ghc-optparse-applicative
  (package
    (name "ghc-optparse-applicative")
    (version "0.18.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "optparse-applicative" version))
       (sha256
        (base32 "14f4xihvklrdbc0banwzh5wwqfmyi1d34r43hsw2ks16zns1m0b3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "optparse-applicative")))
    (inputs (list ghc-transformers-compat ghc-prettyprinter
                  ghc-prettyprinter-ansi-terminal))
    (native-inputs (list ghc-quickcheck))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           ;; Tests pass with 2.15.1 even though the package requires < 2.15
           (add-before 'configure 'relax-quickcheck-version
             (lambda _
               (substitute* "optparse-applicative.cabal"
                 (("(QuickCheck *[^<]*)< *2.15" all bound)
                  (string-append bound "< 2.16"))))))
       #:cabal-revision '("1"
                          "10kd3gn961kb20acrqah70gkla2d3qypr37z0pzypry73a3762gk")))
    (home-page "https://github.com/pcapriotti/optparse-applicative")
    (synopsis "Utilities and combinators for parsing command line options")
    (description "This package provides utilities and combinators for parsing
command line options in Haskell.")
    (license license:bsd-3)))

(define-public ghc-jira-wiki-markup
  (package
    (name "ghc-jira-wiki-markup")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "jira-wiki-markup" version))
       (sha256
        (base32 "0875x0x8v92zh89m28xq3y8gb9c8ca7dm790zczipkrwkhv3v8lw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "jira-wiki-markup")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "19lkrzzhqjy5rjg7xcdwsrkna7nygjs0ayq7sm3114b1kbs8hahl")))
    (home-page "https://github.com/tarleb/jira-wiki-markup")
    (synopsis "Handle Jira wiki markup")
    (description
     "Parse jira wiki text into an abstract syntax tree for easy transformation
to other formats.")
    (license license:expat)))

(define-public ghc-emojis
  (package
    (name "ghc-emojis")
    (version "0.1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "emojis" version))
       (sha256
        (base32 "1r8a8xjnsqbyzg96fla2s1cg6804297w6487rnrvjhqxgccxc040"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "emojis")))
    (native-inputs (list ghc-hunit))
    (home-page "https://github.com/jgm/emojis#readme")
    (synopsis "Conversion between emoji characters and their names")
    (description
     "This package provides functions for converting emoji names to emoji
characters and vice versa.

How does it differ from the @code{emoji} package?
@itemize
@item It supports a fuller range of emojis, including all those supported by
GitHub
@item It supports lookup of emoji aliases from emoji
@item It uses Text rather than String
@item It has a lighter dependency footprint: in particular, it does not
require aeson
@item It does not require TemplateHaskell
@end itemize")
    (license license:bsd-3)))

(define-public ghc-text-conversions
  (package
    (name "ghc-text-conversions")
    (version "0.3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-conversions" version))
       (sha256
        (base32 "0pbjlzsjd3m8np5p6iq7zb0bx6n40d8jha76r8s07s4wg2x0yxy8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-conversions")))
    (inputs (list ghc-base16-bytestring ghc-base64-bytestring))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "https://github.com/cjdev/text-conversions")
    (synopsis "Safe conversions between textual types")
    (description "Safe conversions between textual types")
    (license license:isc)))

(define-public ghc-text-icu
  (package
    (name "ghc-text-icu")
    (version "0.8.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-icu" version))
       (sha256
        (base32 "1nzd7al2vpm07xa19w9vy6f696bm4z48h0m4fljsxjg4v0wblbj4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-icu")))
    (inputs (list icu4c))
    (native-inputs (list ghc-hunit
                         ghc-quickcheck
                         ghc-random
                         ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2
                         ghc-semigroups
                         pkg-config))
    (arguments
     `(#:cabal-revision ("1"
                         "1zqbcsnf9943mk8i9lyj1ab2xvg6dw9k6jfjhblxqcliq53c3dpp")))
    (home-page "https://github.com/haskell/text-icu")
    (synopsis "Bindings to the ICU library")
    (description
     "Haskell bindings to the International Components for Unicode (ICU) libraries.
These libraries provide robust and full-featured Unicode services on a wide
variety of platforms. .  Features include: . * Both pure and impure bindings, to
allow for fine control over efficiency and ease of use. . * Breaking of strings
on character, word, sentence, and line boundaries. . * Access to the Unicode
Character Database (UCD) of character metadata. . * String collation functions,
for locales where the conventions for lexicographic ordering differ from the
simple numeric ordering of character codes. . * Character set conversion
functions, allowing conversion between Unicode and over 220 character encodings.
. * Unicode normalization. (When implementations keep strings in a normalized
form, they can be assured that equivalent strings have a unique binary
representation.) . * Regular expression search and replace. . * Security checks
for visually confusable (spoofable) strings. . * Bidirectional Unicode algorithm
. * Calendar objects holding dates and times. . * Number and calendar
formatting.")
    (license license:bsd-3)))

(define-public ghc-text-iso8601
  (package
    (name "ghc-text-iso8601")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-iso8601" version))
       (sha256
        (base32 "00l2m3xj39mrz7lbmbnir4k9jbn3jm3xa2q6zkh0zr7arsvx5slx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-iso8601")))
    (inputs (list ghc-integer-conversion ghc-time-compat))
    (native-inputs (list ghc-quickcheck ghc-quickcheck-instances ghc-tasty
                         ghc-tasty-hunit ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "0jaqs685h5zip0vac6wrbz37mw3kvkjsifzr5fcki4wnxxg64yp0")))
    (home-page "https://github.com/haskell/aeson")
    (synopsis "Converting time to and from ISO 8601 text.")
    (description
     "Converting time to and from IS0 8601 text.  Specifically the
@url{https://datatracker.ietf.org/doc/html/rfc3339, RFC3339} profile.")
    (license license:bsd-3)))

(define-public ghc-text-short
  (package
    (name "ghc-text-short")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-short" version))
       (sha256
        (base32 "1avfamw6sig6r5zpm2hbas373qbrdszi4q33gds9ihvxil9ylww1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-short")))
    (inputs (list ghc-hashable))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("3"
                         "0fhnd5dd46pjja8rpawq8ld93ix31q9wa18bkfv6lr7nsg699gj1")))
    (home-page "http://hackage.haskell.org/package/text-short")
    (synopsis "Memory-efficient representation of Unicode text strings")
    (description
     "This package provides the @code{ShortText} type which
is suitable for keeping many short strings in memory.  This is similar
to how @code{ShortByteString} relates to @code{ByteString}.

The main difference between @code{Text} and @code{ShortText} is that
@code{ShortText} uses UTF-8 instead of UTF-16 internally and also doesn't
support zero-copy slicing (thereby saving 2 words).  Consequently, the memory
footprint of a (boxed) @code{ShortText} value is 4 words (2 words when unboxed)
plus the length of the UTF-8 encoded payload.")
    (license license:bsd-3)))

(define-public ghc-text-zipper
  (package
    (name "ghc-text-zipper")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-zipper" version))
       (sha256
        (base32 "1acq583wmgb53viqslbkgl454300fawg5lryxddfiy1mqk3iqlh6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-zipper")))
    (inputs (list ghc-vector))
    (native-inputs (list ghc-hspec hspec-discover ghc-quickcheck))
    (home-page "https://github.com/jtdaugherty/text-zipper/")
    (synopsis "Text editor zipper library")
    (description
     "This Haskell library provides a two-dimensional zipper data structure for
editing text.  The structure represents the body of text and an editing cursor
which can be moved through it, along with a set of editing transformations.

Text zippers are generalized over the set of data types that might be used to
store lists of characters (e.g., @code{String}, @code{T.Text}, etc.).
Implementations using both of these examples are provided.")
    (license license:bsd-3)))

(define-public ghc-doclayout
  (package
    (name "ghc-doclayout")
    (version "0.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "doclayout" version))
       (sha256
        (base32 "01vh5j9w0xprh2p9njnrmfj2lpivbfg4r1rksshgaj8b9fqdh8b5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "doclayout")))
    (inputs (list ghc-emojis ghc-safe))
    (native-inputs (list ghc-tasty ghc-tasty-golden ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/jgm/doclayout")
    (synopsis "Pretty-printing library for laying out text documents")
    (description
     "doclayout is a pretty-printing library for laying out text documents,
with several features not present in pretty-printing libraries designed for
code.  It was designed for use in @code{Pandoc}.")
    (license license:bsd-3)))

(define-public ghc-pandoc
  (package
    (name "ghc-pandoc")
    (version "3.7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pandoc" version))
       (sha256
        (base32 "1l33amh5dkbxbgicvk4hh231b8x36fb90jlpxmgqwfqldk7j3lmz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pandoc")))
    (inputs (list ghc-xml
                  ghc-xml-conduit
                  ghc-xml-types
                  ghc-glob
                  ghc-juicypixels
                  ghc-aeson
                  ghc-aeson-pretty
                  ghc-attoparsec
                  ghc-base64-bytestring
                  ghc-blaze-html
                  ghc-blaze-markup
                  ghc-case-insensitive
                  ghc-citeproc
                  ghc-commonmark
                  ghc-commonmark-extensions
                  ghc-commonmark-pandoc
                  ghc-crypton
                  ghc-crypton-connection
                  ghc-data-default
                  ghc-doclayout
                  ghc-doctemplates
                  ghc-emojis
                  ghc-file-embed
                  ghc-gridtables
                  ghc-haddock-library
                  ghc-http-client
                  ghc-http-client-tls
                  ghc-http-types
                  ghc-ipynb
                  ghc-jira-wiki-markup
                  ghc-mime-types
                  ghc-network
                  ghc-network-uri
                  ghc-pandoc-types
                  ghc-pretty-show
                  ghc-random
                  ghc-safe
                  ghc-scientific
                  ghc-skylighting
                  ghc-skylighting-core
                  ghc-split
                  ghc-syb
                  ghc-tagsoup
                  ghc-temporary
                  ghc-texmath
                  ghc-text-conversions
                  ghc-unicode-collation
                  ghc-unicode-transforms
                  ghc-yaml
                  ghc-libyaml
                  ghc-zip-archive
                  ghc-zlib
                  ghc-typst
                  ghc-vector
                  ghc-djot
                  ghc-tls
                  ghc-crypton-x509-system))
    (native-inputs (list ghc-diff ghc-tasty ghc-tasty-golden ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://pandoc.org")
    (synopsis "Conversion between markup formats")
    (description
     "Pandoc is a Haskell library for converting from one markup format to
another, and a command-line tool that uses this library.  It can read and
write Markdown and (subsets of) other formats, such as HTML, reStructuredText,
LaTeX, DocBook, and many more.

Pandoc extends standard Markdown syntax with footnotes, embedded LaTeX,
definition lists, tables, and other features.  A compatibility mode is
provided for those who need a drop-in replacement for Markdown.pl.")
    (license license:gpl2+)))

(define-public ghc-pandoc-server
  (package
    (name "ghc-pandoc-server")
    (version "0.1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pandoc-server" version))
       (sha256
        (base32 "1686kl05pr6bqrmg5dda1d7m9dmk0c087fpg54r9v5iy1x732gws"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pandoc-server")))
    (inputs (list ghc-pandoc
                  ghc-pandoc-types
                  ghc-aeson
                  ghc-base64-bytestring
                  ghc-doctemplates
                  ghc-data-default
                  ghc-unicode-collation
                  ghc-servant-server
                  ghc-skylighting
                  ghc-wai
                  ghc-wai-cors))
    (home-page "https://pandoc.org")
    (synopsis "Pandoc document conversion as an HTTP servant-server")
    (description
     "Pandoc-server provides pandoc's document conversion functions in an HTTP server.")
    (license license:gpl2+)))

(define-public ghc-pandoc-lua-engine
  (package
    (name "ghc-pandoc-lua-engine")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pandoc-lua-engine" version))
       (sha256
        (base32 "1s5g1mvl13pa411kyd2jp5jz0lw8alxqpv984nnfnq17d2nj4mkw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pandoc-lua-engine")))
    (inputs (list ghc-aeson
                  ghc-crypton
                  ghc-citeproc
                  ghc-data-default
                  ghc-doclayout
                  ghc-doctemplates
                  ghc-hslua
                  ghc-hslua-module-doclayout
                  ghc-hslua-module-path
                  ghc-hslua-module-system
                  ghc-hslua-module-text
                  ghc-hslua-module-version
                  ghc-hslua-module-zip
                  ghc-hslua-repl
                  ghc-lpeg
                  ghc-pandoc
                  ghc-pandoc-lua-marshal
                  ghc-pandoc-types))
    (native-inputs (list ghc-tasty ghc-tasty-golden ghc-tasty-hunit
                         ghc-tasty-lua))
    (arguments
     `(#:cabal-revision ("2"
                         "0a00gmr5ymp5jl6dp6f1qcmyxcqz1nqp2d4y6x1342zhqsi1zs8s")))
    (home-page "https://pandoc.org")
    (synopsis "Lua engine to power custom pandoc conversions")
    (description
     "This package provides a pandoc scripting engine based on Lua.")
    (license license:gpl2+)))

(define-public pandoc
  (package
    (name "pandoc")
    (version "3.7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pandoc-cli" version))
       (sha256
        (base32 "0g9x7h2aimiffnv03pcvai64kpwxykz18kd126x92lpsdjwclkgz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pandoc-cli")))
    (arguments
      (list
       ;; Create entirely self-contained binary by embedding the data files
       ;; in the binary itself. Required for python-pypandoc.
       #:configure-flags #~(list "-fembed_data_files")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'install-more
             (lambda _
               (let ((bash (string-append #$output "/etc/bash_completion.d/pandoc"))
                     (man1 (string-append #$output "/share/man/man1")))
                 (mkdir-p (dirname bash))
                 (with-output-to-file bash
                   (lambda _
                     (invoke (string-append #$output "/bin/pandoc")
                             "--bash-completion")))
                 (mkdir-p man1)
                 (install-file "man/pandoc.1" man1)
                 (install-file "man/pandoc-lua.1" man1)
                 (install-file "man/pandoc-server.1" man1)))))))
    (inputs (list ghc-pandoc
                  ghc-pandoc-server
                  ghc-wai-extra
                  ghc-warp
                  ghc-safe
                  ghc-hslua-cli
                  ghc-pandoc-lua-engine
                  ghc-temporary))
    (home-page "https://pandoc.org")
    (synopsis "Conversion between documentation formats")
    (description
     "Pandoc-cli provides a command-line executable that uses the pandoc library to
convert between markup formats.")
    (license license:gpl2+)))

(define-public ghc-pandoc-types
  (package
    (name "ghc-pandoc-types")
    (version "1.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pandoc-types" version))
       (sha256
        (base32 "1hd18l1c5yh7x24gsligkbraadq12hn7mim16xyjnicdsa1s03xd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pandoc-types")))
    (inputs (list ghc-syb ghc-aeson ghc-quickcheck))
    (native-inputs (list ghc-test-framework ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2 ghc-hunit))
    (arguments
     `(#:cabal-revision ("3"
                         "0w2n4vzxs3jasrivaq49clxdlccnfv2gh4mkp8s7krxa1arambrz")))
    (home-page "https://pandoc.org/")
    (synopsis "Types for representing a structured document")
    (description
     "This module defines the @code{Pandoc} data structure, which is used by
pandoc to represent structured documents.  It also provides functions for
building up, manipulating and serialising @code{Pandoc} structures.")
    (license license:bsd-3)))

(define-public ghc-parallel
  (package
    (name "ghc-parallel")
    (version "3.2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "parallel" version))
       (sha256
        (base32 "1xkfi96w6yfpppd0nw1rnszdxmvifwzm699ilv6332ra3akm610p"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "parallel")))
    (arguments
     `(#:cabal-revision ("10"
                         "0rm92b8ny5qxalhg83dk2i4b4ca455vrbnqad905waz18z16xx1x")))
    (home-page "http://hackage.haskell.org/package/parallel")
    (synopsis "Parallel programming library")
    (description "This package provides a library for parallel programming.")
    (license license:bsd-3)))

(define-public ghc-parsec
  (package
    (name "ghc-parsec")
    (version "3.1.18.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "parsec" version))
              (sha256
               (base32
                "1byha0j1i0h5wwzafm0csx0y8maga17r6my2z5w2ciki789rybs0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "parsec")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/haskell/parsec")
    (synopsis "Monadic parser combinators")
    (description "Parsec is designed from scratch as an industrial-strength
parser library.  It is simple, safe, well documented (on the package
homepage), has extensive libraries, good error messages, and is fast.  It is
defined as a monad transformer that can be stacked on arbitrary monads, and it
is also parametric in the input stream type.")
    (license license:bsd-3)))

(define-public ghc-parsec-numbers
  (package
    (name "ghc-parsec-numbers")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "parsec-numbers" version))
       (sha256
        (base32 "1gzy4v3r02kvdxvgg1nj83mmb6aph2v4ilf9c7y6nbvi2x49l0bp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "parsec-numbers")))
    (home-page "http://hackage.haskell.org/package/parsec-numbers")
    (synopsis "Utilities for parsing numbers from strings")
    (description
     "This package provides the number parsers without the need to use a large
(and unportable) token parser.")
    (license license:bsd-3)))

(define-public ghc-parser-combinators
  (package
    (name "ghc-parser-combinators")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "parser-combinators" version))
       (sha256
        (base32 "0is45q3q6ngfqvzpwwga9phbwk45v7g1q2x1rlm95a7q946yy44k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "parser-combinators")))
    (arguments
     `(#:cabal-revision ("1"
                         "0xkqxas64dd77lgc341b526rip7vva0ipy2kbczmfga41lz5fnc6")))
    (home-page "https://github.com/mrkkrp/parser-combinators")
    (synopsis "Commonly useful parser combinators")
    (description
     "This is a lightweight package providing commonly useful parser
combinators.")
    (license license:bsd-3)))

(define-public ghc-parsers
  (package
    (name "ghc-parsers")
    (version "0.12.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "parsers" version))
       (sha256
        (base32 "1g16qrhacjzfcja2wn5936xz9bwqs80xxmj189d2lwwyga5m77nx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "parsers")))
    (inputs (list ghc-charset ghc-scientific ghc-unordered-containers
                  ghc-attoparsec ghc-semigroups))
    (native-inputs (list ghc-quickcheck ghc-quickcheck-instances))
    (arguments
     `(#:cabal-revision ("1"
                         "11y65sabwqcliqxwdss8pjvliy0w4m3b4amd1acf0jgmx4bhxdf6")))
    (home-page "http://github.com/ekmett/parsers/")
    (synopsis "Parsing combinators")
    (description
     "This library provides convenient combinators for working
with and building parsing combinator libraries.  Given a few simple instances,
you get access to a large number of canned definitions.  Instances exist for
the parsers provided by @code{parsec}, @code{attoparsec} and @code{base}'s
@code{Text.Read}.")
    (license license:bsd-3)))

(define-public ghc-path
  ;; Upstream didn't create a Git tag for release 0.9.6.  Therefore, we build
  ;; from the commit which bumped the Cabal version to 0.9.6.
  (let ((commit "17294d1c9c185a69b79fedb8ff6c6f853a500d2e")
        (revision "1"))
    (package
      (name "ghc-path")
      (version (git-version "0.9.6" revision commit))
      ;; Need to build from Git since the hackage tarball is missing Include.h
      ;; files required to make the tests pass.
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/commercialhaskell/path")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02ivlhnw8rc05zbk706fpab7607axyw2fx9jkgi5knpwwkxwwv6d"))))
      (build-system haskell-build-system)
      (properties '((upstream-name . "path")))
      (inputs (list ghc-aeson ghc-hashable))
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (add-before 'configure 'update-constraints
                      (lambda _
                        (substitute* "path.cabal"
                          (("hashable   >= 1.2     && < 1.5")
                           "hashable")))))))
      (native-inputs (list ghc-hspec
                           ghc-quickcheck
                           ghc-genvalidity
                           ghc-genvalidity-hspec
                           ghc-validity-bytestring))
      (home-page "http://hackage.haskell.org/package/path")
      (synopsis "Support for well-typed paths")
      (description "This package introduces a type for paths upholding useful
  invariants.")
      (license license:bsd-3))))

(define-public ghc-path-io
  (package
    (name "ghc-path-io")
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "path-io" version))
       (sha256
        (base32 "1a4s4fc2rbzri1cb27gzlm0v24k5g2975smvqg2j4d4h6xmpzbfd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "path-io")))
    (inputs (list ghc-dlist ghc-path ghc-temporary ghc-unix-compat))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/mrkkrp/path-io")
    (synopsis "Functions for manipulating well-typed paths")
    (description
     "This package provides an interface to the @code{directory}
package for users of @code{path}.  It also implements some missing stuff like
recursive scanning and copying of directories, working with temporary
files/directories, and more.")
    (license license:bsd-3)))

(define-public ghc-paths
  (package
    (name "ghc-paths")
    (version "0.1.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ghc-paths" version))
       (sha256
        (base32 "1164w9pqnf7rjm05mmfjznz7rrn415blrkk1kjc0gjvks1vfdjvf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ghc-paths")))
    (arguments
     `(#:cabal-revision ("8"
                         "1ymhq0il4l8a91n8iqfrq9q57sj6zfkmvpjf8nwgavwclfi50ka2")))
    (home-page "http://hackage.haskell.org/package/ghc-paths")
    (synopsis "Knowledge of GHC's installation directories")
    (description "Knowledge of GHC's installation directories.")
    (license license:bsd-3)))

(define-public ghc-patience
  (package
    (name "ghc-patience")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "patience" version))
       (sha256
        (base32 "1i1b37lgi31c17yrjyf8pdm4nf5lq8vw90z3rri78hf0k66d0p3i"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "patience")))
    (arguments
     `(#:cabal-revision ("1"
                         "0vldslyv381bmiw9f22wv46jmdcnxcjz5b8xqh8n7h7zzc5qs6j9")))
    (home-page "https://github.com/chessai/patience")
    (synopsis "Patience diff and longest increasing subsequence")
    (description
     "This library implements the 'patience diff' algorithm, as well as the
patience algorithm for the longest increasing subsequence problem.
Patience diff computes the difference between two lists, for example the lines
of two versions of a source file.  It provides a good balance between
performance, nice output for humans, and simplicity of implementation.")
    (license license:bsd-3)))

(define-public ghc-pattern-arrows
  (package
    (name "ghc-pattern-arrows")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pattern-arrows" version))
       (sha256
        (base32
         "13q7bj19hd60rnjfc05wxlyck8llxy11z3mns8kxg197wxrdkhkg"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pattern-arrows")))
    (home-page
     "https://blog.functorial.com/posts/2013-10-27-Pretty-Printing-Arrows.html")
    (synopsis "Arrows for Pretty Printing")
    (description
     "A library for generating concise pretty printers based on precedence
rules.")
    (license license:expat)))

(define-public ghc-pcre-light
  (package
    (name "ghc-pcre-light")
    (version "0.4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pcre-light" version))
       (sha256
        (base32 "1rhlp0v32ahwlh5293xyq04my8f2ln1mfycpg01cm0qvmng2irmw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pcre-light")))
    (inputs (list pcre))
    (native-inputs (list ghc-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "0yafqik2hsb9x2k79kz6k5r3awk1g6gzyq7yxjb5grm3czgh4hhx")))
    (home-page "http://hackage.haskell.org/package/pcre-light")
    (synopsis "Haskell library for Perl 5 compatible regular expressions")
    (description
     "This package provides a small, efficient, and portable regex
library for Perl 5 compatible regular expressions.  The PCRE library is a set
of functions that implement regular expression pattern matching using the same
syntax and semantics as Perl 5.")
    (license license:bsd-3)))

(define-public ghc-peano
  (package
    (name "ghc-peano")
    (version "0.1.0.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "peano" version))
              (sha256
               (base32
                "0yzcxrl41dacvx2wkyxjj7hgvz56l4qb59r4h9rmaqd7jcwx5z9i"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision ("3"
                         "0wl22dnz6ld300cg6id3lw991bp8kdfi8h0nbv37vn79i1zdcj5n")))
    (home-page "https://hackage.haskell.org/package/peano")
    (synopsis "Peano numbers")
    (description "Provides an efficient Haskell implementation of Peano
numbers")
    (license license:bsd-3)))

(define-public ghc-persistent
  (package
    (name "ghc-persistent")
    (version "2.17.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "persistent" version))
       (sha256
        (base32 "0c3kz36nv9n8xd21z7hgk8djj2ldvzfmy3qrmxhr8p617s18182g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "persistent")))
    (inputs (list ghc-aeson
                  ghc-attoparsec
                  ghc-attoparsec-aeson
                  ghc-base64-bytestring
                  ghc-blaze-html
                  ghc-conduit
                  ghc-fast-logger
                  ghc-http-api-data
                  ghc-lift-type
                  ghc-megaparsec
                  ghc-monad-logger
                  ghc-path-pieces
                  ghc-replace-megaparsec
                  ghc-resource-pool
                  ghc-resourcet
                  ghc-scientific
                  ghc-semigroupoids
                  ghc-silently
                  ghc-th-lift-instances
                  ghc-unliftio
                  ghc-unliftio-core
                  ghc-unordered-containers
                  ghc-vault
                  ghc-vector))
    (native-inputs (list ghc-hspec hspec-discover ghc-quickcheck
                         ghc-quickcheck-instances ghc-shakespeare))
    (home-page "http://www.yesodweb.com/book/persistent")
    (synopsis "Type-safe, multi-backend data serialization for Haskell")
    (description
     "This Haskell package allows Haskell programs to access data
storage systems like PostgreSQL, SQLite, and MariaDB in a type-safe way.")
    (license license:expat)))

(define-public ghc-persistent-sqlite
  (package
    (name "ghc-persistent-sqlite")
    (version "2.13.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "persistent-sqlite" version))
       (sha256
        (base32 "14yn3a5nqjq1b7ss6xl2455nwq92kbwc94q675jiyi4gzh85xfd0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "persistent-sqlite")))
    (inputs (list ghc-aeson
                  ghc-conduit
                  ghc-microlens-th
                  ghc-monad-logger
                  ghc-persistent
                  ghc-resource-pool
                  ghc-resourcet
                  ghc-unliftio-core
                  ghc-unordered-containers))
    (native-inputs (list ghc-fast-logger
                         ghc-hspec
                         ghc-hunit
                         ghc-microlens
                         ghc-persistent-test
                         ghc-quickcheck
                         ghc-system-fileio
                         ghc-system-filepath
                         ghc-temporary))
    (home-page "http://www.yesodweb.com/book/persistent")
    (synopsis "Backend for the persistent library using sqlite3")
    (description
     "This Haskell package includes a thin sqlite3 wrapper based
on the direct-sqlite package, as well as the entire C library, so there are no
system dependencies.")
    (license license:expat)))

(define-public ghc-persistent-template
  (package
    (name "ghc-persistent-template")
    (version "2.12.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "persistent-template" version))
       (sha256
        (base32 "0c9cs27j43azimj74s2m2cdks87682ibpy1xbyzvygipgmb8nj6w"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "persistent-template")))
    (home-page "http://www.yesodweb.com/book/persistent")
    (synopsis "Type-safe, non-relational, multi-backend persistence")
    (description "This Haskell package provides interfaces and helper
functions for the ghc-persistent package.")
    (license license:expat)))

(define-public ghc-persistent-test
  (package
    (name "ghc-persistent-test")
    (version "2.13.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "persistent-test" version))
       (sha256
        (base32 "0mzj2k07yhbx8674c1yh6brl8clncqc7ci8295hjazrmr528rc2x"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "persistent-test")))
    (inputs (list ghc-aeson
                  ghc-blaze-html
                  ghc-conduit
                  ghc-hspec
                  ghc-hspec-expectations
                  ghc-http-api-data
                  ghc-hunit
                  ghc-monad-control
                  ghc-monad-logger
                  ghc-path-pieces
                  ghc-persistent
                  ghc-quickcheck
                  ghc-quickcheck-instances
                  ghc-random
                  ghc-resourcet
                  ghc-transformers-base
                  ghc-unliftio
                  ghc-unliftio-core
                  ghc-unordered-containers))
    (home-page "http://www.yesodweb.com/book/persistent")
    (synopsis "Tests for the Persistent database library")
    (description
     "This is only for use in developing libraries that should conform to
the persistent interface, not for users of the persistent suite of database
libraries.")
    (license license:expat)))

(define-public ghc-pgp-wordlist
  (package
    (name "ghc-pgp-wordlist")
    ;; The 0.1.0.3 release no longer builds and the maintainer
    ;; has not made a new release with the fix
    (version (git-version "0.1.0.3" "1"
                          "1f0cfd90d62179952cbfd59c3405283a1d364272"))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/quchen/pgp-wordlist")
              (commit "1f0cfd90d62179952cbfd59c3405283a1d364272")))
       (sha256
        (base32
         "1n6wf85302hv19gy4h0gpzn5cscnb10mw9fyn85yiaxasn1508aq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pgp-wordlist")))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                (let* ((tmpdir (or (getenv "TMP")
                                   "/tmp"))
                       (dependency-package-db
                        (string-append tmpdir "/package.conf.d")))
                  (invoke "runhaskell" "Setup.hs" "test" "tasty")
                  (setenv "GHC_PACKAGE_PATH" dependency-package-db)
                  (invoke "./dist/build/doctest/doctest")
                  (unsetenv "GHC_PACKAGE_PATH"))
                (format #t "Testsuite not run.%~")))))))
    (inputs
     (list ghc-vector))
    (native-inputs
     (list ghc-hunit ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck
           ghc-doctest))
    (home-page
     "https://github.com/quchen/pgp-wordlist")
    (synopsis
     "Translate between binary data and a human-readable collection of words")
    (description
     "The PGP Word List consists of two phonetic alphabets, each with one word
per possible byte value.  A string of bytes is translated with these
alphabets, alternating between them at each byte.

The PGP words corresponding to the bytes 5B 1D CA 6E are \"erase breakaway
spellbind headwaters\", for example.

For further information, see
@url{http://en.wikipedia.org/wiki/PGP_word_list}.")
    (license license:bsd-3)))

(define-public ghc-pipes
  (package
    (name "ghc-pipes")
    (version "4.3.16")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pipes" version))
       (sha256
        (base32 "163lx5sf68zx5kik5h1fjsyckwr9shdsn5k2dsjq3mhg077nxqgl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pipes")))
    (inputs (list ghc-mmorph ghc-void ghc-fail ghc-semigroups))
    (native-inputs (list ghc-quickcheck ghc-test-framework
                         ghc-test-framework-quickcheck2))
    (arguments
     `(#:cabal-revision ("6"
                         "16s8a1ijakhsk73ny2vrw6a8r2dszgncd0wk735ii6csg3l2c9pm")))
    (home-page "http://hackage.haskell.org/package/pipes")
    (synopsis "Compositional pipelines")
    (description
     "A clean and powerful stream processing library that lets you build
and connect reusable streaming components.  Advantages over traditional streaming
libraries:
@itemize
@item Concise API: Use simple commands like @code{for}, (@code{>->}), @code{await},
and @code{yield}
@item Blazing fast: Implementation tuned for speed, including shortcut fusion
@item Lightweight Dependency: pipes is small and compiles very rapidly, including
dependencies
@item Elegant semantics: Use practical category theory
@item ListT: Correct implementation of @code{ListT} that interconverts with pipes
@item Bidirectionality: Implement duplex channels
@end itemize")
    (license license:bsd-3)))

(define-public ghc-pointedlist
  (package
    (name "ghc-pointedlist")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pointedlist" version))
       (sha256
        (base32 "16xsrzqql7i4z6a3xy07sqnbyqdmcar1jiacla58y4mvkkwb0g3l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pointedlist")))
    (home-page "http://hackage.haskell.org/package/pointedlist")
    (synopsis "Zipper-like comonad which works as a list, tracking a position")
    (description
     "A PointedList tracks the position in a non-empty list which works
similarly to a zipper.  A current item is always required, and therefore
the list may never be empty.  A circular PointedList wraps around to the
other end when progressing past the actual edge.")
    (license license:bsd-3)))

(define-public ghc-polyparse
  (package
    (name "ghc-polyparse")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "polyparse" version))
       (sha256
        (base32 "0yvhg718dlksiw3v27m2d8m1sn4r4f5s0p56zq3lynhy1sc74k0w"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "polyparse")))
    (arguments
     `(#:cabal-revision ("9"
                         "0abzqic0askffn6nmh1f08d9rd9fykdlcdd9gj1c7p1ykp9zlq29")))
    (home-page "http://code.haskell.org/~malcolm/polyparse/")
    (synopsis "Alternative parser combinator libraries")
    (description
     "This package provides a variety of alternative parser combinator
libraries, including the original HuttonMeijer set.  The Poly sets have
features like good error reporting, arbitrary token type, running state, lazy
parsing, and so on.  Finally, Text.Parse is a proposed replacement for the
standard Read class, for better deserialisation of Haskell values from
Strings.")
    (license license:lgpl2.1)))

(define-public ghc-portmidi
  (package
    (name "ghc-portmidi")
    (version "0.2.0.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "PortMidi" version))
              (sha256
               (base32
                "1jb722gwgx1fdyv64nj3by22970l3r04ibc3fa3hnp3k4l2jvk0f"))))
    (build-system haskell-build-system)
    (inputs (list alsa-lib))
    (home-page "https://github.com/PortMidi/PortMidi-haskell")
    (synopsis "Haskell bindings for PortMedia/PortMidi")
    (description "This is a Haskell module for PortMidi audio library,
which supports real-time MIDI input and output.")
    (license license:bsd-3)))

(define-public ghc-pqueue
  (package
    (name "ghc-pqueue")
    (version "1.5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pqueue" version))
       (sha256
        (base32
         "00hzrhz3n55ahyv2h183l72jsl3q01p4ns0063p0vjaa5j6qpy0v"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pqueue")))
    (arguments
     (list #:cabal-revision
           '("3" "0g7zaidq3gf08kzwp4xrbi3f76qdv4ckx6nblzi5j55awcsp08xy")))
    (inputs (list ghc-indexed-traversable))
    (native-inputs
     (list ghc-tasty ghc-tasty-quickcheck))
    (home-page "https://hackage.haskell.org/package/pqueue")
    (synopsis "Reliable, persistent, fast priority queues")
    (description
     "This package provides a fast, reliable priority queue implementation
based on a binomial heap.")
    (license license:bsd-3)))

(define-public ghc-prelude-extras
  (package
    (name "ghc-prelude-extras")
    (version "0.4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "prelude-extras" version))
       (sha256
        (base32
         "0xzqdf3nl2h0ra4gnslm1m1nsxlsgc0hh6ky3vn578vh11zhifq9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "prelude-extras")))
    (home-page "https://github.com/ekmett/prelude-extras")
    (synopsis "Higher order versions of Prelude classes")
    (description "This library provides higher order versions of
@code{Prelude} classes to ease programming with polymorphic recursion and
reduce @code{UndecidableInstances}.")
    (license license:bsd-3)))

(define-public ghc-prettyclass
  (package
    (name "ghc-prettyclass")
    (version "1.0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "prettyclass" version))
       (sha256
        (base32 "11l9ajci7nh1r547hx8hgxrhq8mh5gdq30pdf845wvilg9p48dz5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "prettyclass")))
    (home-page "http://hackage.haskell.org/package/prettyclass")
    (synopsis "Pretty printing class similar to Show")
    (description
     "This package provides a pretty printing class similar
to @code{Show}, based on the HughesPJ pretty printing library.  It
provides the pretty printing class and instances for the Prelude
types.")
    (license license:bsd-3)))

(define-public ghc-prettyprinter
  (package
    (name "ghc-prettyprinter")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "prettyprinter" version))
       (sha256
        (base32 "0i8b3wjjpdvp5b857j065jwyrpgcnzgk75imrj7i3yhl668acvjy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "prettyprinter")))
    ;; Many packages required by the test suite depend (some indirectly) on
    ;; ghc-prettyprinter
    (arguments (list #:tests? #f))
    (home-page "http://github.com/quchen/prettyprinter")
    (synopsis
     "Modern, easy to use, well-documented, extensible pretty-printer")
    (description
     "A prettyprinter/text rendering engine.  Easy to use, well-documented,
ANSI terminal backend exists, HTML backend is trivial to implement, no name
clashes, @code{Text}-based, extensible.")
    (license license:bsd-2)))

(define-public ghc-prettyprinter-ansi-terminal
  (package
    (name "ghc-prettyprinter-ansi-terminal")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "prettyprinter-ansi-terminal" version))
       (sha256
        (base32 "1cqxbcmy9ykk4pssq5hp6h51g2h547zfz549awh0c1fni8q3jdw1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "prettyprinter-ansi-terminal")))
    (inputs (list ghc-ansi-terminal ghc-prettyprinter))
    ;; tests only check documentation and cause a dependency cycle
    (arguments (list #:tests? #f))
    (home-page "http://github.com/quchen/prettyprinter")
    (synopsis "ANSI terminal backend for the prettyprinter package")
    (description "ANSI terminal backend for the prettyprinter package.")
    (license license:bsd-2)))

(define-public ghc-prettyprinter-compat-ansi-wl-pprint
  (package
    (name "ghc-prettyprinter-compat-ansi-wl-pprint")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "prettyprinter-compat-ansi-wl-pprint" version))
       (sha256
        (base32 "0mcy0621lx0zmc2csdq348r21f932f2w51y62jzyz4cby58p5ch5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "prettyprinter-compat-ansi-wl-pprint")))
    (inputs (list ghc-prettyprinter ghc-prettyprinter-ansi-terminal))
    (home-page "http://github.com/quchen/prettyprinter")
    (synopsis
     "Drop-in compatibility package to migrate from @code{ansi-wl-pprint} to
@code{prettyprinter}.")
    (description "Drop-in compatibility package to migrate from @code{ansi-wl-pprint} to
@code{prettyprinter}.")
    (license license:bsd-2)))

(define-public ghc-pretty-hex
  (package
    (name "ghc-pretty-hex")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pretty-hex" version))
       (sha256
        (base32 "0c8pa0rdb2q8rf4acy4gww0hj5lrzclzdh52yi2aiaaij4lqzir7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pretty-hex")))
    (home-page "http://hackage.haskell.org/package/pretty-hex")
    (synopsis "Haskell library for hex dumps of ByteStrings")
    (description
     "This Haskell library generates pretty hex dumps of
ByteStrings in the style of other common *nix hex dump tools.")
    (license license:bsd-3)))

(define-public ghc-pretty-show
  (package
    (name "ghc-pretty-show")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pretty-show" version))
       (sha256
        (base32 "1lkgvbv00v1amvpqli6y4dzsbs25l4v3wlagvhwx8qxhw2390zrh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pretty-show")))
    (inputs (list ghc-haskell-lexer))
    (native-inputs (list ghc-happy))
    (home-page "http://wiki.github.com/yav/pretty-show")
    (synopsis "Tools for working with derived `Show` instances")
    (description
     "This package provides a library and an executable for working with
derived @code{Show} instances.  By using the library, derived @code{Show}
instances can be parsed into a generic data structure.  The @code{ppsh} tool
uses the library to produce human-readable versions of @code{Show} instances,
which can be quite handy for debugging Haskell programs.  We can also render
complex generic values into an interactive Html page, for easier
examination.")
    (license license:expat)))

(define-public ghc-pretty-simple
  (package
    (name "ghc-pretty-simple")
    (version "4.1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pretty-simple" version))
       (sha256
        (base32 "03nhhavr66ikwmkma3mw2a7dz9w0mhaxj49dgf2cf76v3jq1vara"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pretty-simple")))
    (inputs (list ghc-prettyprinter ghc-prettyprinter-ansi-terminal
                  ghc-optparse-applicative ghc-aeson))
    (home-page "https://github.com/cdepillabout/pretty-simple")
    (synopsis "Pretty printer for data types with a 'Show' instance")
    (description
     "Pretty-simple is a pretty printer for Haskell data types that have a
Show instance.")
    (license license:bsd-3)))

(define-public ghc-primitive
  (package
    (name "ghc-primitive")
    (version "0.9.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "primitive" version))
       (sha256
        (base32 "0xixplp2b5sh2sx6hqllhr8bcsd028v7ry2pibdwayrwh50xxd24"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "primitive")))
    (native-inputs (list ghc-base-orphans
                         ghc-quickcheck-classes-base
                         ghc-quickcheck
                         ghc-tasty
                         ghc-tasty-quickcheck
                         ghc-tagged
                         ghc-transformers-compat))
    (home-page "https://github.com/haskell/primitive")
    (synopsis "Primitive memory-related operations")
    (description
     "This package provides various primitive memory-related operations.")
    (license license:bsd-3)))

(define-public ghc-primitive-bootstrap
  (package
    (inherit ghc-primitive)
    (name "ghc-primitive-bootstrap")
    (arguments
     `(#:tests? #f
       ,@(package-arguments ghc-primitive)))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-primitive-addr
  (package
    (name "ghc-primitive-addr")
    (version "0.1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "primitive-addr" version))
       (sha256
        (base32 "1bs6xmlsv77187hqwcygv3nv2ynjgjwf3564vkk5bp8vjawigz72"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "primitive-addr")))
    (inputs (list ghc-primitive))
    (home-page "https://github.com/byteverse/primitive-addr")
    (synopsis "Addresses to unmanaged memory")
    (description
     "This library provides the @code{Data.Primitive.Addr} module that was a part
of the @code{primitive} library before @code{primitive-0.7.0.0}.")
    (license license:bsd-3)))

(define-public ghc-process-extras
  (package
    (name "ghc-process-extras")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "process-extras" version))
       (sha256
        (base32 "0klqgr37f1z2z6i0a9b0giapmq0p35l5k9kz1p7f0k1597w7agi9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "process-extras")))
    (inputs (list ghc-data-default ghc-listlike ghc-generic-deriving))
    (native-inputs (list ghc-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "1lpl0qvbk3dvfg36qr1xvwd916jdrcjbviiqmh9x9m1zqkq3jpxz")))
    (home-page "https://github.com/seereason/process-extras")
    (synopsis "Extra tools for managing processes")
    (description
     "This package extends
@url{https://hackage.haskell.org/package/process}.  It allows you to read
process input and output as ByteStrings or Text, or write your own
ProcessOutput instance.  It also provides lazy process input and output,
and a ProcessMaker class for more flexibility in the process creation
API.")
    (license license:expat)))

(define-public ghc-profunctors
  (package
    (name "ghc-profunctors")
    (version "5.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "profunctors" version))
       (sha256
        (base32 "1wqf3isrrgmqxz5h42phsa7lawl6442r1da89hg82bld6qkz9imr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "profunctors")))
    (inputs (list ghc-base-orphans
                  ghc-bifunctors
                  ghc-comonad
                  ghc-contravariant
                  ghc-distributive
                  ghc-tagged))
    (home-page "http://github.com/ekmett/profunctors/")
    (synopsis "Profunctors for Haskell")
    (description "This library provides profunctors for Haskell.")
    (license license:bsd-3)))

(define-public ghc-quote-quot
  (package
    (name "ghc-quote-quot")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "quote-quot" version))
       (sha256
        (base32 "1xsd5vs97dwp3wnz862mplakkryi44brr73aqrrv76svkj82bp37"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quote-quot")))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck))
    (home-page "https://github.com/Bodigrim/quote-quot#readme")
    (synopsis "Divide without division")
    (description
     "Generate routines for integer division, employing arithmetic and bitwise
operations only, which are __2.5x-3.5x faster__ than quot'.  Divisors must be
known in compile-time and be positive.")
    (license license:bsd-3)))

(define-public ghc-indexed-profunctors
  (package
    (name "ghc-indexed-profunctors")
    (version "0.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "indexed-profunctors" version))
       (sha256
        (base32 "166329a5jmrs4q1ycb132gq7kbrdyzrvrxzzzwp5czmv00lvns9f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "indexed-profunctors")))
    (home-page "http://hackage.haskell.org/package/indexed-profunctors")
    (synopsis "Utilities for indexed profunctors")
    (description
     "This package contains basic definitions related to indexed profunctors.  These
are primarily intended as internal utilities to support the @code{optics} and
@code{generic-lens} package families.")
    (license license:bsd-3)))

(define-public ghc-project-template
  (package
    (name "ghc-project-template")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "project-template" version))
       (sha256
        (base32 "0ac43x36i6b595jhflif1qqhri1rrqw90ama5n7rsh0ffnzyb69d"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "project-template")))
    (inputs (list ghc-base64-bytestring ghc-conduit ghc-conduit-extra
                  ghc-resourcet))
    (native-inputs (list ghc-hspec hspec-discover ghc-quickcheck))
    (home-page "https://github.com/fpco/haskell-ide")
    (synopsis "Specify Haskell project templates and generate files")
    (description
     "Haskell library for both generating and consuming project templates.

ost IDEs provide the concept of a project template: instead of writing all
of the code for a project from scratch, you select a template, answer a few
questions, and a bunch of files are automatically generated.

project-template tries to provide a canonical Haskell library for implementing
the ideal templating system.")
    (license license:bsd-3)))

(define-public ghc-protolude
  (package
    (name "ghc-protolude")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "protolude" version))
       (sha256
        (base32 "11q4qivjsqxfmb93nzxabipsxmmcpdajrkb8n5hx53awkx15j2n8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "protolude")))
    (inputs (list ghc-async ghc-hashable ghc-mtl-compat
                  ghc-transformers-compat))
    (home-page "https://github.com/sdiehl/protolude")
    (synopsis "Sensible set of defaults for writing custom Preludes")
    (description
     "Protolude gives you sensible defaults for writing custom Preludes to
replace the standard one provided by GHC.")
    (license license:expat)))

(define-public ghc-psqueue
  (package
    (name "ghc-psqueue")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "PSQueue" version))
       (sha256
        (base32 "1jm8fswmmlrlg0cxgc358sc4jinjligm5qzqrzdli8pi8d06p5ni"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "PSQueue")))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-quickcheck
                         ghc-tasty-hunit))
    (home-page "http://hackage.haskell.org/package/PSQueue")
    (synopsis "Priority search queue")
    (description
     "A @dfn{priority search queue} efficiently supports the operations of
both a search tree and a priority queue.  A @code{Binding} is a product of
a key and a priority.  Bindings can be inserted, deleted, modified and queried
in logarithmic time, and the binding with the least priority can be retrieved
in constant time.  A queue can be built from a list of bindings, sorted by
keys, in linear time.")
    (license license:bsd-3)))

(define-public ghc-psqueues
  (package
    (name "ghc-psqueues")
    (version "0.2.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "psqueues" version))
       (sha256
        (base32 "0kkr0ya5j0g8z86ihq1h3m7hr88hy6yg0bkrg6vdwmhr6vqlcfzn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "psqueues")))
    (inputs (list ghc-hashable))
    (native-inputs (list ghc-hunit
                         ghc-quickcheck
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-tagged))
    (home-page "http://hackage.haskell.org/package/psqueues")
    (synopsis "Pure priority search queues")
    (description
     "The psqueues package provides
@uref{https://en.wikipedia.org/wiki/Priority_queue, Priority Search Queues} in
three different flavors:

@itemize
@item @code{OrdPSQ k p v}, which uses the @code{Ord k} instance to provide
fast insertion, deletion and lookup.  This implementation is based on Ralf
Hinze's @uref{http://citeseer.ist.psu.edu/hinze01simple.html, A Simple
Implementation Technique for Priority Search Queues}.

Hence, it is similar to the @uref{https://hackage.haskell.org/package/PSQueue,
PSQueue} library, although it is considerably faster and provides a slightly
different API.

@item @code{IntPSQ p v} is a far more efficient implementation.  It fixes the
key type to @code{Int} and uses a
@code{https://en.wikipedia.org/wiki/Radix_tree, radix tree} (like @code{IntMap})
with an additional min-heap property.

@item @code{HashPSQ k p v} is a fairly straightforward extension
of @code{IntPSQ}: it simply uses the keys' hashes as indices in the
@code{IntPSQ}.  If there are any hash collisions, it uses an
@code{OrdPSQ} to resolve those.  The performance of this implementation
is comparable to that of @code{IntPSQ}, but it is more widely
applicable since the keys are not restricted to @code{Int},
but rather to any @code{Hashable} datatype.
@end itemize

Each of the three implementations provides the same API, so they can
be used interchangeably.

Typical applications of Priority Search Queues include:

@itemize
@item Caches, and more specifically LRU Caches;
@item Schedulers;
@item Pathfinding algorithms, such as Dijkstra's and A*.
@end itemize")
    (license license:bsd-3)))

(define-public ghc-pwstore-fast
  (package
    (name "ghc-pwstore-fast")
    (version "2.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pwstore-fast" version))
       (sha256
        (base32 "1cpvlwzg3qznhygrr78f75p65mnljd9v5cvnagfxjqppnrkay6bj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pwstore-fast")))
    (inputs (list ghc-base64-bytestring ghc-cryptohash ghc-random ghc-byteable))
    (home-page "https://github.com/PeterScott/pwstore")
    (synopsis "Secure password storage")
    (description
     "To store passwords securely, they should be salted, then hashed with
a slow hash function.  This library uses PBKDF1-SHA256, and handles all the
details.  It uses the cryptohash package for speed; if you need a pure
Haskell library, pwstore-purehaskell has the exact same API, but uses only
pure Haskell.  It is about 25 times slower than this package, but still quite
usable.")
    (license license:bsd-3)))

(define-public ghc-random
  (package
    (name "ghc-random")
    (version "1.2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "random" version))
       (sha256
        (base32 "1p2i636bk1q62dzrlrl92mirrz0ynf93bxs5yql07r6ilwk1kj79"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "random")))
    ;; ghc-random is widely used and causes quite a few loops, so disable tests.
    (arguments (list #:tests? #f))
    (inputs (list ghc-splitmix-bootstrap))
    (home-page "https://hackage.haskell.org/package/random")
    (synopsis "Random number library")
    (description
     "This package provides a basic random number generation
library, including the ability to split random number generators.")
    (license license:bsd-3)))

(define-public ghc-random-bootstrap
  (package
    (inherit ghc-random)
    (name "ghc-random-bootstrap")
    (arguments
     `(#:tests? #f
       ,@(package-arguments ghc-random)))
    (inputs (list ghc-splitmix-bootstrap))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-raw-strings-qq
  (package
    (name "ghc-raw-strings-qq")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "raw-strings-qq" version))
       (sha256
        (base32 "1lxy1wy3awf52968iy5y9r5z4qgnn2sxkdrh7js3m9gadb11w09f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "raw-strings-qq")))
    (native-inputs (list ghc-hunit))
    (home-page "https://github.com/23Skidoo/raw-strings-qq")
    (synopsis "Raw string literals for Haskell")
    (description
     "This package provides a quasiquoter for raw string literals, i.e. string
literals that don't recognise the standard escape sequences.  Basically, they
make your code more readable by freeing you from the responsibility to escape
backslashes.  They are useful when working with regular expressions,
DOS/Windows paths and markup languages (such as XML).")
    (license license:bsd-3)))

(define-public ghc-readable
  (package
    (name "ghc-readable")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "readable" version))
       (sha256
        (base32 "1ja39cg26wy2fs00gi12x7iq5k8i366pbqi3p916skfa5jnkfc3h"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "readable")))
    (arguments
     `(#:cabal-revision ("2"
                         "1190pzpd10r8d59h7ks1yahnpj8h8hal2i7y6mx488bwc9iixdqk")))
    (home-page "https://github.com/mightybyte/readable")
    (synopsis "Type class for reading from Text and ByteString")
    (description
     "This package provides a @code{Readable} type class for
reading data types from @code{ByteString} and @code{Text}.  It also
includes efficient implementations for common data types.")
    (license license:bsd-3)))

(define-public ghc-rebase
  (package
    (name "ghc-rebase")
    (version "1.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "rebase" version))
       (sha256
        (base32 "1yzzh8zdvzhl9wzh6zb1f30by49nhw2xlwpiqzq7i048dibza6bc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "rebase")))
    (inputs (list ghc-bifunctors
                  ghc-comonad
                  ghc-contravariant
                  ghc-dlist
                  ghc-either
                  ghc-groups
                  ghc-hashable
                  ghc-invariant
                  ghc-profunctors
                  ghc-scientific
                  ghc-selective
                  ghc-semigroupoids
                  ghc-time-compat
                  ghc-unordered-containers
                  ghc-uuid-types
                  ghc-vector
                  ghc-vector-instances
                  ghc-void))
    (home-page "https://github.com/nikita-volkov/rebase")
    (synopsis "Progressive alternative to the base package
for Haskell")
    (description
     "This Haskell package is intended for those who are
tired of keeping long lists of dependencies to the same essential libraries
in each package as well as the endless imports of the same APIs all over again.

It also supports the modern tendencies in the language.

To solve those problems this package does the following:

@itemize
@item Reexport the original APIs under the @code{Rebase} namespace.

@item Export all the possible non-conflicting symbols from the
@code{Rebase.Prelude} module.

@item Give priority to the modern practices in the conflicting cases.
@end itemize

The policy behind the package is only to reexport the non-ambiguous and
non-controversial APIs, which the community has obviously settled on.
The package is intended to rapidly evolve with the contribution from
the community, with the missing features being added with pull-requests.")
    (license license:expat)))

(define-public ghc-reducers
  (package
    (name "ghc-reducers")
    (version "3.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "reducers" version))
       (sha256
        (base32 "0742ry9xjjmhwvlv100d3nz75k7wqr42262kw21n5mmwrka6358h"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "reducers")))
    (inputs (list ghc-fingertree ghc-hashable ghc-unordered-containers
                  ghc-semigroupoids))
    (arguments
     `(#:cabal-revision ("1"
                         "06iyj53308dxgrji857hqnq83ga52bw2dmp1szvxzc2vbnq0qfv8")))
    (home-page "http://github.com/ekmett/reducers/")
    (synopsis
     "Semigroups, specialized containers and a general map/reduce framework")
    (description "This library provides various semigroups, specialized
containers and a general map/reduce framework for Haskell.")
    (license license:bsd-3)))

(define-public ghc-refact
  (package
    (name "ghc-refact")
    (version "0.3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "refact" version))
       (sha256
        (base32 "0v0zxcx29b8jxs2kgy9csykqcp8kzhdvyylw2xfwmj4pfxr2kl0a"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "refact")))
    (home-page "http://hackage.haskell.org/package/refact")
    (synopsis "Specify refactorings to perform with apply-refact")
    (description
     "This library provides a datatype which can be interpreted by
@code{apply-refact}.  It exists as a separate library so that applications can
specify refactorings without depending on GHC.")
    (license license:bsd-3)))

(define-public ghc-reflection
  (package
    (name "ghc-reflection")
    (version "2.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "reflection" version))
       (sha256
        (base32 "1cwzwncb7zvzdcj5s5pc1qrkh7xj6kyz9b30qq2imvnh3bjdb9y6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "reflection")))
    (native-inputs (list ghc-hspec ghc-quickcheck hspec-discover))
    (arguments
     `(#:cabal-revision ("1"
                         "1q717za8dn36gxhjbr1vmaw6hv5k4id2230yxl3a6627i34qvwpa")))
    (home-page "http://github.com/ekmett/reflection")
    (synopsis "Reify arbitrary terms into types that can be reflected back
into terms")
    (description
     "This package addresses the 'configuration problem' which is
propagating configurations that are available at run-time, allowing multiple
configurations to coexist without resorting to mutable global variables or
@code{System.IO.Unsafe.unsafePerformIO}.")
    (license license:bsd-3)))

(define-public ghc-reflex
  (package
    (name "ghc-reflex")
    (version "0.9.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "reflex" version))
       (sha256
        (base32 "1qh2xbg4q2gif25hinz72j8ka2w976lccklknwgijxaayh92if4a"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "reflex")))
    (inputs (list ghc-memotrie
                  ghc-bifunctors
                  ghc-comonad
                  ghc-commutative-semigroups
                  ghc-constraints
                  ghc-constraints-extras
                  ghc-data-default
                  ghc-dependent-map
                  ghc-dependent-sum
                  ghc-exception-transformers
                  ghc-lens
                  ghc-mmorph
                  ghc-monad-control
                  ghc-patch
                  ghc-prim-uniq
                  ghc-primitive
                  ghc-profunctors
                  ghc-random
                  ghc-ref-tf
                  ghc-reflection
                  ghc-semigroupoids
                  ghc-syb
                  ghc-unbounded-delays
                  ghc-witherable
                  ghc-these
                  ghc-semialign
                  ghc-monoidal-containers
                  ghc-haskell-src-exts
                  ghc-haskell-src-meta))
    (native-inputs (list ghc-split
                         ghc-filemanip
                         ghc-these-lens
                         ghc-hspec
                         ghc-these-lens
                         ghc-these-lens
                         ghc-these-lens))
    (home-page "https://reflex-frp.org")
    (synopsis "Higher-order functional reactive programming")
    (description
     "This library lets you write interactive programs without callbacks or
side-effects.  Functional Reactive Programming (FRP) uses composable events
and time-varying values to describe interactive systems as pure functions.
Just like other pure functional code, functional reactive code is easier to
get right on the first try, maintain, and reuse.  Reflex is a
fully-deterministic, higher-order FRP interface and an engine that efficiently
implements that interface.")
    (license license:bsd-3)))

(define-public ghc-reflex-sdl2
  (let ((commit "6dadf2c4f383b8a58fcd73616996b219c4f93972")
        (revision "1"))
    (package
      (name "ghc-reflex-sdl2")
      (version (git-version "0.3.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/schell/reflex-sdl2")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "06lxfgp18l1car6wd07mbjn4yblnp89acf1i67nd815p2hx0ihbz"))))
      (build-system haskell-build-system)
      (properties '((upstream-name . "reflex-sdl2")))
      (inputs (list ghc-async
                    ghc-dependent-sum
                    ghc-exception-transformers
                    ghc-ref-tf
                    ghc-primitive
                    ghc-reflex
                    ghc-sdl2))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'configure 'update-constraints
             (lambda _
               (substitute* "reflex-sdl2.cabal"
                 (("\\bref-tf +>= 0\\.4 +&& < 0\\.5\\b") "ref-tf")))))))
      (home-page "https://github.com/schell/reflex-sdl2")
      (synopsis "SDL2 and Reflex functional reactive programming")
      (description
       "This package provides a minimal host for SDL2-based Reflex
applications.")
      (license license:expat))))

(define-public ghc-regex
  (package
    (name "ghc-regex")
    (version "1.1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "regex" version))
       (sha256
        (base32 "1nzyfkqmclmawmphvksvm9l64awqgnypic4xplc2s9sjcj4h814a"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "regex")))
    (inputs (list ghc-base-compat
                  ghc-hashable
                  ghc-regex-base
                  ghc-regex-pcre-builtin
                  ghc-regex-tdfa
                  ghc-time-locale-compat
                  ghc-unordered-containers
                  ghc-utf8-string))
    (home-page "http://regex.uk")
    (synopsis "Toolkit for regex-base")
    (description
     "This package provides a regular expression toolkit for @code{regex-base}
with compile-time checking of regular expression syntax, data types for
matches and captures, a text replacement toolkit, portable options, high-level
AWK-like tools for building text processing apps, regular expression macros
with parsers and test bench, comprehensive documentation, tutorials and
copious examples.")
    (license license:bsd-3)))

(define-public ghc-regex-applicative
  (package
    (name "ghc-regex-applicative")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "regex-applicative" version))
       (sha256
        (base32 "0di66pi2kq5rrsn0k6pwakzwa0bgi9jfb2csm72kp5gzqdws8s8p"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "regex-applicative")))
    (inputs (list ghc-filtrable))
    (native-inputs (list ghc-smallcheck ghc-tasty ghc-tasty-smallcheck
                         ghc-tasty-hunit))
    (home-page "https://github.com/feuerbach/regex-applicative")
    (synopsis "Regex-based parsing with applicative interface")
    (description
     "@code{regex-applicative} is a Haskell library for parsing using
regular expressions.  Parsers can be built using Applicative interface.")
    (license license:expat)))

(define-public ghc-regex-base
  (package
    (name "ghc-regex-base")
    (version "0.94.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "regex-base" version))
       (sha256
        (base32 "1gs76xbda39gq8wzb8as3y49npa93vfrndf4q78hsyccb7p2vjp8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "regex-base")))
    (home-page "https://wiki.haskell.org/Regular_expressions")
    (synopsis "Replaces/Enhances Text.Regex")
    (description
     "@code{Text.Regex.Base} provides the interface API for
regex-posix, regex-pcre, regex-parsec, regex-tdfa, regex-dfa.")
    (license license:bsd-3)))

(define-public ghc-regex-compat
  (package
    (name "ghc-regex-compat")
    (version "0.95.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "regex-compat" version))
       (sha256
        (base32 "071hpcqj18gygdj9scd8bm1zxg9vvp3m0sf6f7a7nsk1qgcd2zp2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "regex-compat")))
    (inputs (list ghc-regex-base ghc-regex-posix))
    (home-page "https://wiki.haskell.org/Regular_expressions")
    (synopsis "Replaces/Enhances Text.Regex")
    (description "This library provides one module layer over
@code{regex-posix} to replace @code{Text.Regex}.")
    (license license:bsd-3)))

(define-public ghc-regex-pcre
  (package
    (name "ghc-regex-pcre")
    (version "0.95.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "regex-pcre" version))
       (sha256
        (base32 "1rdpznvbmg3ra3pjshykds8ra97yqg23nxfxw5dd6sigcv1pp5i9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "regex-pcre")))
    (inputs (list ghc-regex-base pcre))
    (native-inputs (list pkg-config))
    (home-page "https://wiki.haskell.org/Regular_expressions")
    (synopsis "Enhancement of the builtin Text.Regex library")
    (description
     "This package is an enhancement of the @code{Text.Regex} library.
It wraps the @code{PCRE} C library providing Perl-compatible regular
expressions.")
    (license license:bsd-3)))

(define-public ghc-regex-pcre-builtin
  (package
    (name "ghc-regex-pcre-builtin")
    (version "0.95.2.3.8.44")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "regex-pcre-builtin" version))
       (sha256
        (base32 "0pn55ssrwr05c9sa9jvp0knvzjksz04wn3pmzf5dz4xgbyjadkna"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "regex-pcre-builtin")))
    (inputs (list ghc-regex-base))
    (arguments
     `(#:cabal-revision ("6"
                         "1rn3649yqqrbd177mbyk12gdpbm3kdzjgnjqxfv68crah237y08j")))
    (home-page "http://hackage.haskell.org/package/regex-pcre-builtin")
    (synopsis "Enhancement of the builtin Text.Regex library")
    (description
     "This package is an enhancement of the @code{Text.Regex} library,
providing the PCRE backend to accompany regex-base, with bundled code from
@url{https://www.pcre.org}.")
    (license license:bsd-3)))

(define-public ghc-regex-posix
  (package
    (name "ghc-regex-posix")
    (version "0.96.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "regex-posix" version))
       (sha256
        (base32 "0l41mapdlq8cvlqzd15f99yrqrxlvl2n790v9p2ywpawqdh08mvy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "regex-posix")))
    (inputs (list ghc-regex-base))
    (home-page "http://hackage.haskell.org/package/regex-posix")
    (synopsis "POSIX regular expressions for Haskell")
    (description "This library provides the POSIX regex backend used by the
Haskell library @code{regex-base}.")
    (license license:bsd-3)))

(define-public ghc-regex-tdfa
  (package
    (name "ghc-regex-tdfa")
    (version "1.3.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "regex-tdfa" version))
       (sha256
        (base32 "15x7pisdvi0afg7ybxpga8aa4q38x9pz9ml7nhz1f7s4nw9w72q7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "regex-tdfa")))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (if tests?
                 (begin
                   ;; The doctests require GHC_PACKAGE_PATH but Setup.hs errors
                   ;; out if it is defined so we run them by hand
                   (invoke "runhaskell" "Setup.hs" "test" "regex-tdfa-unittest")
                   (setenv "GHC_PACKAGE_PATH"
                           (string-append (or (getenv "TMP")
                                              "/tmp")
                                          "/package.conf.d"))
                   (invoke "./dist/build/doctest/doctest" "--ghc-arg=-idist/build/autogen")
                   (unsetenv "GHC_PACKAGE_PATH"))
                 (format #t "Testsuite not run.~%")))))))
    (inputs (list ghc-regex-base))
    (native-inputs (list ghc-utf8-string ghc-doctest-parallel))
    (home-page "https://wiki.haskell.org/Regular_expressions")
    (synopsis "POSIX extended regular expressions in Haskell")
    (description
     "Regex-tdfa is a pure Haskell regular expression library implementing POSIX
extended regular expressions.  It is a \"tagged\" DFA regex engine. It is
inspired by libtre.")
    (license license:bsd-3)))

(define-public ghc-replace-megaparsec
  (package
    (name "ghc-replace-megaparsec")
    (version "1.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "replace-megaparsec" version))
       (sha256
        (base32 "134ns97fhsg0a0jn5pjfnbv9jj40p0ljinx7y572gaw0lpfwc4x1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "replace-megaparsec")))
    (inputs (list ghc-megaparsec ghc-parser-combinators))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/jamesdbrock/replace-megaparsec")
    (synopsis
     "Find, replace, split string patterns with Megaparsec parsers (instead of regex)")
    (description
     "Find text patterns, replace the patterns, split on the patterns.  Use Megaparsec
monadic parsers instead of regular expressions for pattern matching.")
    (license license:bsd-2)))

(define-public ghc-repline
  (package
    (name "ghc-repline")
    (version "0.4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "repline" version))
       (sha256
        (base32 "04iy7z3cmkwjhf90jdjqfv2cjcmn2206p4xmjshfn3fda4sawrcl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "repline")))
    (home-page "https://github.com/sdiehl/repline")
    (synopsis "Haskeline wrapper for GHCi-like REPL interfaces")
    (description
     "Haskeline wrapper for GHCi-like REPL interfaces.  Composable with
normal mtl transformers.")
    (license license:expat)))

(define-public ghc-rerebase
  (package
    (name "ghc-rerebase")
    (version "1.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "rerebase" version))
       (sha256
        (base32 "047wlwvffmgg70d70dsv6160wq6hfxp4frb0414np270grq0vk3p"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "rerebase")))
    (inputs (list ghc-rebase))
    (home-page "https://github.com/nikita-volkov/rerebase")
    (synopsis "Reexports from ``base'' with many other standard libraries")
    (description
     "A rich drop-in replacement for @code{base}.  For details and
documentation please visit @uref{https://github.com/nikita-volkov/rerebase,
the project's home page}.")
    (license license:expat)))

(define-public ghc-resolv
  (package
    (name "ghc-resolv")
    (version "0.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "resolv" version))
       (sha256
        (base32 "00viayfl655hibms5nh3nr3j7jhb1bvlhiscinvwnb4bp25a80kp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "resolv")))
    (inputs (list ghc-base16-bytestring))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "http://hackage.haskell.org/package/resolv")
    (synopsis "Domain Name Service (DNS) lookup via @code{libresolv}")
    (description
     "This package implements an API for accessing the
@uref{https://tools.ietf.org/html/rfc1035, Domain Name Service (DNS)}
resolver service via the standard @code{libresolv} system library (whose
API is often available directly via the standard @code{libc} C library) on
Unix systems.")
    (license license:gpl3)))

(define-public ghc-resource-pool
  (package
    (name "ghc-resource-pool")
    (version "0.4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "resource-pool" version))
       (sha256
        (base32 "1cg99a88zlaxxb1aqjv8f2xip7wr6a8k0mwiyxjqsy3m7qz7h3cc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "resource-pool")))
    (inputs (list ghc-hashable ghc-primitive))
    (home-page "http://hackage.haskell.org/package/resource-pool")
    (synopsis "Striped resource pooling implementation in Haskell")
    (description
     "This Haskell package provides striped pooling abstraction
for managing flexibly-sized collections of resources such as database
connections.")
    (license license:bsd-3)))

(define-public ghc-resourcet
  (package
    (name "ghc-resourcet")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "resourcet" version))
       (sha256
        (base32 "0swrz7h73m86x3937gdiay3z30y9hn35n86v5brh38j2xs2ifq7c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "resourcet")))
    (inputs (list ghc-unliftio-core ghc-primitive))
    (native-inputs (list ghc-hspec))
    (home-page "http://github.com/snoyberg/conduit")
    (synopsis "Deterministic allocation and freeing of scarce resources")
    (description "ResourceT is a monad transformer which creates a region of
code where you can safely allocate resources.")
    (license license:bsd-3)))

(define-public ghc-retry
  (package
    (name "ghc-retry")
    (version "0.9.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "retry" version))
       (sha256
        (base32 "1mky1dfllmx6dr1gayf636n3z5xrfmam3rhs5vx7c3wj9c8kabk2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "retry")))
    (inputs (list ghc-random ghc-mtl-compat ghc-unliftio-core))
    (native-inputs (list ghc-hunit ghc-tasty ghc-tasty-hunit
                         ghc-tasty-hedgehog ghc-hedgehog))
    (home-page "http://github.com/Soostone/retry")
    (synopsis "Retry combinators for monadic actions that may fail")
    (description
     "This package exposes combinators that can wrap
arbitrary monadic actions.  They run the action and potentially retry
running it with some configurable delay for a configurable number of
times.  The purpose is to make it easier to work with IO and especially
network IO actions that often experience temporary failure and warrant
retrying of the original action.  For example, a database query may time
out for a while, in which case we should hang back for a bit and retry
the query instead of simply raising an exception.")
    (license license:bsd-3)))

(define-public ghc-rfc5051
  (package
    (name "ghc-rfc5051")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "rfc5051" version))
       (sha256
        (base32 "0nri7js5ymywh2gi3li25wrkl1nf712qhbzw5hn46fib83qsq73k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "rfc5051")))
    (home-page "http://hackage.haskell.org/package/rfc5051")
    (synopsis "Simple unicode collation as per RFC5051")
    (description
     "This library implements @code{unicode-casemap}, the simple, non
locale-sensitive unicode collation algorithm described in RFC 5051.  Proper
unicode collation can be done using @code{text-icu}, but that is a big
dependency that depends on a large C library, and @code{rfc5051} might be
better for some purposes.")
    (license license:bsd-3)))

(define-public ghc-rio
  (package
    (name "ghc-rio")
    (version "0.1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "rio" version))
       (sha256
        (base32 "0rpc4f2yvw0y6mqz9ykm3778j6srya7ssww691kpf9nb8vddgjb6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "rio")))
    (inputs (list ghc-hashable
                  ghc-microlens
                  ghc-microlens-mtl
                  ghc-primitive
                  ghc-typed-process
                  ghc-unliftio
                  ghc-unliftio-core
                  ghc-unordered-containers
                  ghc-vector))
    (native-inputs (list ghc-quickcheck ghc-hspec hspec-discover))
    (home-page "https://github.com/commercialhaskell/rio#readme")
    (synopsis "Standard library for Haskell")
    (description
     "This package works as a prelude replacement for Haskell,
providing more functionality and types out of the box than the standard
prelude (such as common data types like @code{ByteString} and
@code{Text}), as well as removing common ``gotchas'', like partial
functions and lazy I/O.  The guiding principle here is:
@itemize
@item If something is safe to use in general and has no expected naming
conflicts, expose it.
@item If something should not always be used, or has naming conflicts,
expose it from another module in the hierarchy.
@end itemize")
    (license license:expat)))

(define-public ghc-roman-numerals
  (package
    (name "ghc-roman-numerals")
    (version "0.5.1.5")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "roman-numerals" version))
              (sha256
               (base32
                "10da5vls9l5i255bapms4b2r7dnwmxgsaa1cdll2lrmid5dikixr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "roman-numerals")))
    (inputs (list ghc-base-unicode-symbols))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "roman-numerals.cabal"
               (("\\b(bytestring|text)\\s+[^,\n]+" all dep)
                dep)))))))
    (home-page "https://github.com/roelvandijk/roman-numerals")
    (synopsis "Parsing and pretty printing of Roman numerals")
    (description
     "This library provides functions for parsing and pretty printing Roman numerals.
Because the notation of Roman numerals has varied through the centuries this
package allows for some customisation using a configuration that is passed to
the conversion functions.")
    (license license:bsd-3)))

(define-public ghc-safe
  (package
    (name "ghc-safe")
    (version "0.3.21")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "safe" version))
       (sha256
        (base32 "00bz022kvs0wd3rs9ycx0nxfyc2gqlg3q13lhx4fqydy5fjgx09c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "safe")))
    (native-inputs (list ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1xx3qq5xc4qbfzshaaqxnhdpl5bdwk3qbkiqbcaqhrw78lbfla5r")))
    (home-page "https://github.com/ndmitchell/safe#readme")
    (synopsis "Library of safe (exception free) functions")
    (description
     "This library provides wrappers around @code{Prelude} and
@code{Data.List} functions, such as @code{head} and @code{!!}, that can throw
exceptions.")
    (license license:bsd-3)))

(define-public ghc-safe-exceptions
  (package
    (name "ghc-safe-exceptions")
    (version "0.1.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "safe-exceptions" version))
       (sha256
        (base32 "1xhyljfvf1zpr7gpi9xgqmi9xsiv5vcjz52gz65zyq4v1kaxhl9w"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "safe-exceptions")))
    (native-inputs (list ghc-hspec ghc-void hspec-discover))
    (arguments
     `(#:cabal-revision ("1"
                         "1laifqnsvli7x74asal5l4qhsvgvc6hycjqmrg7qmmabsldjddwb")))
    (home-page "https://github.com/fpco/safe-exceptions#readme")
    (synopsis "Safe, consistent, and easy exception handling")
    (description
     "Runtime exceptions - as exposed in @code{base} by the
@code{Control.Exception} module - have long been an intimidating part of the
Haskell ecosystem.  This package is intended to overcome this.  It provides a
safe and simple API on top of the existing exception handling machinery.  The
API is equivalent to the underlying implementation in terms of power but
encourages best practices to minimize the chances of getting the exception
handling wrong.")
    (license license:expat)))

(define-public ghc-safeio
  (package
    (name "ghc-safeio")
    (version "0.0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "safeio" version))
       (sha256
        (base32 "0dbkk6ia7acil45b2dbvlnfdssyy1azlj0c8gg7pyp3x5fm67v13"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "safeio")))
    (inputs (list ghc-conduit ghc-conduit-combinators ghc-resourcet))
    (native-inputs (list ghc-hunit ghc-tasty ghc-tasty-hunit ghc-tasty-th))
    (home-page "https://github.com/luispedro/safeio#readme")
    (synopsis "Write output to disk atomically")
    (description
     "This package implements utilities to perform atomic output so as to
avoid the problem of partial intermediate files.")
    (license license:expat)))

(define-public ghc-safesemaphore
  (package
    (name "ghc-safesemaphore")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "SafeSemaphore" version))
       (sha256
        (base32 "0rpg9j6fy70i0b9dkrip9d6wim0nac0snp7qzbhykjkqlcvvgr91"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "SafeSemaphore")))
    (native-inputs (list ghc-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "1k61gqgfh6n3sj8ni8sfvpcm39nqc2msjfxk2pgmhfabvv48w5hv")))
    (home-page "https://github.com/ChrisKuklewicz/SafeSemaphore")
    (synopsis "Exception safe semaphores")
    (description
     "This library provides exception safe semaphores that can be
used in place of @code{QSem}, @code{QSemN}, and @code{SampleVar}, all of which
are not exception safe and can be broken by @code{killThread}.")
    (license license:bsd-3)))

(define-public ghc-sandi
  (package
    (name "ghc-sandi")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "sandi" version))
       (sha256
        (base32 "1ndgai8idlxyccvkz5zsgq06v58blc30i6hkky5b1sf5x6gs2h29"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sandi")))
    (inputs (list ghc-conduit))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck
                         ghc-tasty-th))
    (arguments
     `(#:cabal-revision ("1"
                         "1aj9i1ir6ks3bdb47yvqlxv2azrz09p69ggr73m0cxvir9rd0y5j")))
    (home-page "http://hackage.haskell.org/package/sandi")
    (synopsis "Data encoding library")
    (description "Reasonably fast data encoding library.")
    (license license:bsd-3)))

(define-public ghc-sandwich
  (package
    (name "ghc-sandwich")
    (version "0.3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "sandwich" version))
       (sha256
        (base32 "1xx99hlscbqyyl5z8yby4x13bnkxzbxcxq817jf4b4v2qyha1a0b"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sandwich")))
    (inputs (list ghc-aeson
                  ghc-ansi-terminal
                  ghc-async
                  ghc-brick
                  ghc-colour
                  ghc-free
                  ghc-microlens
                  ghc-microlens-th
                  ghc-monad-control
                  ghc-monad-logger
                  ghc-optparse-applicative
                  ghc-pretty-show
                  ghc-retry
                  ghc-safe
                  ghc-string-interpolate
                  ghc-transformers-base
                  ghc-unliftio
                  ghc-unliftio-core
                  ghc-vector
                  ghc-vty
                  ghc-vty-crossplatform))
    (home-page "https://codedownio.github.io/sandwich")
    (synopsis "Test framework inspired by Hspec")
    (description
     "A test framework for Haskell, inspired by and (almost) a drop-in
replacement for Hspec.  It has a number of powerful features and integrations,
such as interactive terminal user interface for viewing test progress and
built-in profiling support.")
    (license license:bsd-3)))

(define-public ghc-say
  (package
    (name "ghc-say")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "say" version))
       (sha256
        (base32 "1r5kffjfwpas45g74sip8glrj1m9nygrnxjm7xgw898rq9pnafgn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "say")))
    (native-inputs (list ghc-hspec hspec-discover ghc-unliftio))
    ; XXX: Tests fail on GHC 9.10.2. See https://github.com/fpco/say/issues/5
    (arguments (list #:tests? #f))
    (home-page "https://github.com/fpco/say#readme")
    (synopsis "Send textual messages to a Handle in a thread-friendly way")
    (description
     "A thread safe API to write a line of textual data to a Handle, such
as sending some messages to the terminal - that has the following properties:
@itemize
@item Properly handle character encoding settings on the Handle
@item For reasonably sized messages, ensure that the entire message is written
 in one chunk to avoid interleaving data with other threads
@item Avoid unnecessary memory allocations and copies
@item Minimize locking.
@end itemize")
    (license license:expat)))

(define-public ghc-scientific
  (package
    (name "ghc-scientific")
    (version "0.3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "scientific" version))
       (sha256
        (base32 "1kqqf8hyffrkqp6cgjxgxm9nc18ql7jj5rjjirqxf9mam2y47cqk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "scientific")))
    (inputs (list ghc-hashable ghc-integer-logarithms ghc-primitive))
    (native-inputs (list ghc-quickcheck
                         ghc-smallcheck
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-tasty-smallcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "0bkks5hdi3w7xx2k1dvj8alsz1i7ljjaqmrp8hw024fh3mqad0xs")))
    (home-page "https://github.com/basvandijk/scientific")
    (synopsis "Numbers represented using scientific notation")
    (description
     "This package provides @code{Data.Scientific}, which provides
the number type @code{Scientific}.  Scientific numbers are arbitrary precision
and space efficient.  They are represented using
@uref{https://en.wikipedia.org/wiki/Scientific_notation, scientific
notation}.")
    (license license:bsd-3)))

(define-public ghc-sdl
  (package
    (name "ghc-sdl")
    (version "0.6.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "SDL" version))
       (sha256
        (base32
         "00y67v80a8l09i3k76z09lg25kw72ivl09nag8ckdlk4a0cfnzfq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "SDL")))
    (inputs
     (list sdl))
    (home-page "https://hackage.haskell.org/package/SDL")
    (synopsis "LibSDL for Haskell")
    (description "Simple DirectMedia Layer (libSDL) is a cross-platform
multimedia library designed to provide low level access to audio, keyboard,
mouse, joystick, 3D hardware via OpenGL, and 2D video framebuffer.  It is used
by MPEG playback software, emulators, and many popular games, including the
award winning Linux port of \"Civilization: Call To Power.\"")
    (license license:bsd-3)))

(define-public ghc-sdl2
  (package
    (name "ghc-sdl2")
    (version "2.5.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "sdl2" version))
       (sha256
        (base32 "1k60zwqr0kgalw3lyqy6vs9bg8bg40cp64snx8n6rh99050y5cr5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sdl2")))
    ; the tests fail due to wrong performance scaling. The issue is reported
    ; upstream but no solution seems to exist at the moment
    ; https://github.com/haskell-game/sdl2/issues/298
    (arguments (list #:tests? #f))
    (inputs (list ghc-statevar ghc-vector ghc-linear sdl2))
    (native-inputs (list ghc-weigh pkg-config))
    (home-page "http://hackage.haskell.org/package/sdl2")
    (synopsis "High- and low-level bindings to the SDL library")
    (description
     "This package contains bindings to the SDL 2 library, in both high- and
low-level forms.  The @code{SDL} namespace contains high-level bindings, where
enumerations are split into sum types, and we perform automatic
error-checking.  The @code{SDL.Raw} namespace contains an almost 1-1
translation of the C API into Haskell FFI calls.  As such, this does not
contain sum types nor error checking.  Thus this namespace is suitable for
building your own abstraction over SDL, but is not recommended for day-to-day
programming.")
    (license license:bsd-3)))

(define-public ghc-sdl2-image
  (package
    (name "ghc-sdl2-image")
    (version "2.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "sdl2-image" version))
       (sha256
        (base32 "03cjlmj844gmfxqn9mp8333hpsg227kaipgs6g68xwg0cvch696j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sdl2-image")))
    (inputs (list ghc-sdl2 sdl2 sdl2-image))
    (native-inputs (list pkg-config))
    (home-page "http://hackage.haskell.org/package/sdl2-image")
    (synopsis "Bindings to SDL2_image")
    (description "This package provides Haskell bindings to
@code{SDL2_image}.")
    (license license:expat)))

(define-public ghc-sdl2-mixer
  (package
    (name "ghc-sdl2-mixer")
    (version "1.2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "sdl2-mixer" version))
       (sha256
        (base32 "16fgnxq2nmifbz3lrr7dn1qj57l5f2kzv124lya1fjaxmwk1h52q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sdl2-mixer")))
    (inputs (list ghc-data-default-class ghc-lifted-base ghc-monad-control
                  ghc-sdl2 ghc-vector sdl2-mixer))
    (native-inputs (list pkg-config))
    (home-page "http://hackage.haskell.org/package/sdl2-mixer")
    (synopsis "Bindings to SDL2 mixer")
    (description "This package provides Haskell bindings to
@code{SDL2_mixer}.")
    (license license:bsd-3)))

(define-public ghc-sdl2-ttf
  (package
    (name "ghc-sdl2-ttf")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "sdl2-ttf" version))
       (sha256
        (base32 "0sm5lrdif5wmz3iah1658zlr7yr45d1hfihb2hdxdia4h7z1j0mn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sdl2-ttf")))
    (inputs (list ghc-sdl2 ghc-th-abstraction sdl2 sdl2-ttf))
    (native-inputs (list pkg-config))
    (home-page "http://hackage.haskell.org/package/sdl2-ttf")
    (synopsis "Bindings to SDL2_ttf")
    (description "This package provides Haskell bindings to SDL2_ttf C++
library.")
    (license license:bsd-3)))

(define-public ghc-sdl2-gfx
  (package
    (name "ghc-sdl2-gfx")
    (version "0.3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "sdl2-gfx" version))
       (sha256
        (base32 "0r9m54ffkp1dv2ffz9i9318qhvpinc76iih7vg1dwq3siwgpxaxw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sdl2-gfx")))
    (inputs (list ghc-lifted-base ghc-monad-control ghc-sdl2 ghc-vector sdl2-gfx sdl2))
    (native-inputs (list pkg-config))
    (home-page "http://hackage.haskell.org/package/sdl2-gfx")
    (synopsis "Haskell bindings to SDL2_gfx")
    (description
     "This package provides Haskell bindings to the SDL2_gfx graphics
library.")
    (license license:expat)))

(define-public ghc-sdl-image
  (package
    (name "ghc-sdl-image")
    (version "0.6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "SDL-image" version))
       (sha256
        (base32
         "1gxwrvswgwjw6g7ym52gik22l9l3ljy592phv97jdmcf3gi6qcg1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "SDL-image")))
    (arguments
     `(#:configure-flags
       (let* ((sdl-image (assoc-ref %build-inputs "sdl-image"))
              (sdl-image-include (string-append sdl-image "/include/SDL")))
         (list (string-append "--extra-include-dirs=" sdl-image-include)))))
    (inputs
     (list ghc-sdl sdl-image))
    (home-page "https://hackage.haskell.org/package/SDL-image")
    (synopsis "Haskell bindings to libSDL_image")
    (description "SDL_image is an image file loading library.  It loads images
as SDL surfaces, and supports the following formats: BMP, GIF, JPEG, LBM, PCX,
PNG, PNM, TGA, TIFF, XCF, XPM, XV.")
    (license license:bsd-3)))

(define-public ghc-sdl-mixer
  (package
    (name "ghc-sdl-mixer")
    (version "0.6.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "SDL-mixer" version))
       (sha256
        (base32
         "0k26hqgdh789ka3mv4dsk6rin6x6vwcs6hjmnsqq7j3mnrh1342r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "SDL-mixer")))
    (arguments
     `(#:configure-flags
       (let* ((sdl-mixer (assoc-ref %build-inputs "sdl-mixer"))
              (sdl-mixer-include (string-append sdl-mixer "/include/SDL")))
         (list (string-append "--extra-include-dirs=" sdl-mixer-include)))))
    (inputs
     (list ghc-sdl sdl-mixer))
    (home-page "https://hackage.haskell.org/package/SDL-mixer")
    (synopsis "Haskell bindings to libSDL_mixer")
    (description "SDL_mixer is a sample multi-channel audio mixer library.  It
supports any number of simultaneously playing channels of 16 bit stereo audio,
plus a single channel of music, mixed by the popular MikMod MOD, Timidity
MIDI, Ogg Vorbis, and SMPEG MP3 libraries.")
    (license license:bsd-3)))

(define-public ghc-securemem
  (package
    (name "ghc-securemem")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "securemem" version))
       (sha256
        (base32 "19hnw2cfbsfjynxq1bq9f6djbxhsc1k751ml0y1ab3ah913mm29j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "securemem")))
    (inputs (list ghc-byteable ghc-memory))
    (home-page "https://github.com/vincenthz/hs-securemem")
    (synopsis "Auto-scrubbing and const-time-eq memory chunk abstraction for
Haskell")
    (description
     "SecureMem is similar to ByteString, except that it provides
a memory chunk that will be auto-scrubbed after it run out of scope.")
    (license license:bsd-3)))

(define-public ghc-semaphore-compat
  (package
    (name "ghc-semaphore-compat")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "semaphore-compat" version))
       (sha256
        (base32 "1qnrdqayrdazmsflh37p1igd25nh1cfgn4k1v3jwwb0w0amnyvhw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "semaphore-compat")))
    (arguments
     `(#:cabal-revision ("4"
                         "1sgk940k24ig1r50ycz4w79591hqjys4sdmfifgsr6zcq3183zrd")))
    (home-page "https://gitlab.haskell.org/ghc/semaphore-compat")
    (synopsis "Cross-platform abstraction for system semaphores")
    (description
     "This package provides a cross-platform implementation of system semaphores that
abstracts over the `unix` and `Win32` libraries.")
    (license license:bsd-3)))

(define-public ghc-semialign
  (package
    (name "ghc-semialign")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "semialign" version))
       (sha256
        (base32 "1xwx1icyggjbjflgn75bsqw34dmpsd15qqmz13ljxv7zak17ps36"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "semialign")))
    (inputs (list ghc-these
                  ghc-hashable
                  ghc-indexed-traversable
                  ghc-indexed-traversable-instances
                  ghc-tagged
                  ghc-unordered-containers
                  ghc-vector
                  ghc-semigroupoids))
    (arguments
     `(#:cabal-revision ("2"
                         "1swx9cwxbgkp9dj49fcqxncq8lx19sh4k9k4hlqgac1jwjlibrfz")))
    (home-page "https://github.com/haskellari/these")
    (synopsis "Align and Zip type-classes from the common Semialign ancestor")
    (description
     "The major use of @code{These} of this is provided by the
@code{align} member of @code{Semialign} class, representing a
generalized notion of \"zipping with padding\" that combines
structures without truncating to the size of the smaller input.  It
turns out that @code{zip} operation fits well the @code{Semialign}
class, forming lattice-like structure.")
    (license license:bsd-3)))

(define-public ghc-semigroupoids
  (package
    (name "ghc-semigroupoids")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "semigroupoids" version))
       (sha256
        (base32 "07yc5759y4njlb2f7s2yy3ji9akp7xw03w7nybaga514hqq20lqx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "semigroupoids")))
    (inputs (list ghc-base-orphans
                  ghc-bifunctors
                  ghc-transformers-compat
                  ghc-contravariant
                  ghc-distributive
                  ghc-comonad
                  ghc-tagged
                  ghc-hashable
                  ghc-unordered-containers))
    (arguments
     `(#:cabal-revision ("2"
                         "089c5hjsjm5dnxmdr1059nhy6pmz63123z0hvn6shf40v2k0dvmz")))
    (home-page "http://github.com/ekmett/semigroupoids")
    (synopsis "Semigroupoids operations for Haskell")
    (description
     "This library provides a wide array of (semi)groupoids and
operations for working with them.  A @code{Semigroupoid} is a @code{Category}
without the requirement of identity arrows for every object in the category.
A @code{Category} is any @code{Semigroupoid} for which the Yoneda lemma holds.
Finally, to work with these weaker structures it is beneficial to have
containers that can provide stronger guarantees about their contents, so
versions of @code{Traversable} and @code{Foldable} that can be folded with
just a @code{Semigroup} are added.")
    (license license:bsd-2)))

(define-public ghc-semigroups
  (package
    (name "ghc-semigroups")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "semigroups" version))
       (sha256
        (base32 "1qbk6scp1rzb69dy8mz26p6az5vi16g2lzwmwnfshh3br4rjwbch"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "semigroups")))
    (inputs (list ghc-nats ghc-tagged ghc-hashable ghc-unordered-containers
                  ghc-transformers-compat))
    (home-page "http://github.com/ekmett/semigroups/")
    (synopsis "Semigroup operations for Haskell")
    (description
     "This package provides semigroups for Haskell.  In
mathematics, a semigroup is an algebraic structure consisting of a set
together with an associative binary operation.  A semigroup generalizes a
monoid in that there might not exist an identity element.  It
also (originally) generalized a group (a monoid with all inverses) to a type
where every element did not have to have an inverse, thus the name
semigroup.")
    (license license:bsd-3)))

(define-public ghc-semigroups-bootstrap
  (package
    (inherit ghc-semigroups)
    (name "ghc-semigroups-bootstrap")
    (inputs
     (list ghc-nats-bootstrap ghc-tagged
           ghc-unordered-containers-bootstrap ghc-hashable-bootstrap))
    (properties '((hidden? #t)))))

(define-public ghc-semirings
  (package
    (name "ghc-semirings")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "semirings" version))
       (sha256
        (base32 "1rjxzs1ypgn50nkniln5bbjkxfphs48y0jvf8b3y4v8r4bi0mj4g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "semirings")))
    (inputs (list ghc-hashable ghc-unordered-containers))
    (home-page "http://github.com/chessai/semirings")
    (synopsis "Two monoids as one, in holy haskimony")
    (description
     "Haskellers are usually familiar with monoids and semigroups.  A monoid has an
appending operation @code{<>} (or @code{mappend}), and an identity element,
@code{mempty}.  A semigroup has an appending @code{<>} operation, but does not
require a @code{mempty} element.  A Semiring has two appending operations,
@code{plus} and @code{times}, and two respective identity elements,
@code{zero} and @code{one}.  More formally, a Semiring R is a set equipped
with two binary relations @code{+} and @code{*}, such that: (R,+) is a
commutative monoid with identity element 0, (R,*) is a monoid with identity
element 1, (*) left and right distributes over addition, and . multiplication
by @code{0} annihilates R.")
    (license license:bsd-3)))

(define-public ghc-serialise
  (package
    (name "ghc-serialise")
    (version "0.2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "serialise" version))
       (sha256
        (base32 "1x3p9vi6daf50xgv5xxjnclqcq9ynqg1qw7af3ppa1nizycrg533"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "serialise")))
    (inputs (list ghc-cborg
                  ghc-half
                  ghc-hashable
                  ghc-primitive
                  ghc-strict
                  ghc-these
                  ghc-unordered-containers
                  ghc-vector))
    (arguments
     `(#:tests? #f ; Tests fail to build with current dependency versions
       #:cabal-revision ("5"
                         "0kfai48gza3zzi3s3ll1gng2wbpdmr5z5isx8snlh49vafsqjzx6")))
    (home-page "https://github.com/well-typed/cborg")
    (synopsis "Binary serialisation library for Haskell values")
    (description
     "This package (formerly binary-serialise-cbor) provides pure,
efficient serialization of Haskell values directly into ByteStrings for
storage or transmission purposes.  By providing a set of type class instances,
you can also serialise any custom data type you have as well.

The underlying binary format used is the 'Concise Binary Object
Representation', or CBOR, specified in RFC 7049.  As a result, serialised
Haskell values have implicit structure outside of the Haskell program itself,
meaning they can be inspected or analyzed without custom tools.

An implementation of the standard bijection between CBOR and JSON is
provided by the https://hackage.haskell.org/package/cborg-json
package.  Also see https://hackage.haskell.org/package/cbor-tool for a
convenient command-line utility for working with CBOR data.")
    (license license:bsd-3)))

(define-public ghc-setenv
  (package
    (name "ghc-setenv")
    (version "0.1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "setenv" version))
       (sha256
        (base32 "0cnbgrvb9byyahb37zlqrj05rj25v190crgcw8wmlgf0mwwxyn73"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "setenv")))
    (arguments
     `(#:cabal-revision ("1"
                         "0ny4g3kjys0hqg41mnwrsymy1bwhl8l169kis4y4fa58sb06m4f5")))
    (home-page "http://hackage.haskell.org/package/setenv")
    (synopsis "Library for setting environment variables")
    (description "This package provides a Haskell library for setting
environment variables.")
    (license license:expat)))

(define-public ghc-setlocale
  (package
    (name "ghc-setlocale")
    (version "1.0.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "setlocale" version))
       (sha256
        (base32 "19rv89jkhq5ic7j5rzpygnmsbzim2mn8ip0m292za613q88gywir"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "setlocale")))
    (arguments
     `(#:cabal-revision ("6"
                         "18i818q67cxfgz7q8zm6a0z032rh0yjhk375f99jwqh9da2h67fb")))
    (home-page "https://gitlab.com/Kritzefitz/haskell-setlocale/")
    (synopsis "Haskell bindings to setlocale")
    (description "This package provides Haskell bindings to the
@code{setlocale} C function.")
    (license license:bsd-3)))

(define-public ghc-shakespeare
  (package
    (name "ghc-shakespeare")
    (version "2.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "shakespeare" version))
       (sha256
        (base32 "0byj0zhxi1pr8l5f18phzkwcf7z38lyk2zznz8hbkqadfgrmbdkc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "shakespeare")))
    (inputs (list ghc-aeson
                  ghc-blaze-markup
                  ghc-blaze-html
                  ghc-file-embed
                  ghc-vector
                  ghc-unordered-containers
                  ghc-scientific
                  ghc-th-lift))
    (native-inputs (list ghc-hspec ghc-hunit hspec-discover))
    (home-page "http://www.yesodweb.com/book/shakespearean-templates")
    (synopsis "Family of type-safe template languages for Haskell")
    (description
     "This Haskell package provides a family of type-safe
templates with simple variable interpolation.  Shakespeare templates can
be used inline with a quasi-quoter or in an external file and it
interpolates variables according to the type being inserted.")
    (license license:expat)))

(define-public ghc-shelly
  (package
    (name "ghc-shelly")
    (version "1.12.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "shelly" version))
       (sha256
        (base32 "16fy3mgky92w85g4vhnl3xf9bxjil3kc2vh85qy85jg2kz5mw208"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "shelly")))
    (inputs (list ghc-async
                  ghc-enclosed-exceptions
                  ghc-lifted-base
                  ghc-monad-control
                  ghc-transformers-base
                  ghc-unix-compat))
    (native-inputs (list ghc-lifted-async ghc-hspec ghc-hspec-contrib
                         ghc-hunit))
    (home-page "https://github.com/gregwebs/Shelly.hs")
    (synopsis "Shell-like (systems) programming in Haskell")
    (description
     "Shelly provides convenient systems programming in Haskell, similar in
spirit to POSIX shells.  Shelly is originally forked  from the Shellish package.")
    (license license:bsd-3)))

(define-public ghc-silently
  (package
    (name "ghc-silently")
    (version "1.2.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "silently" version))
       (sha256
        (base32 "0vkl2998n3g3vnzh08vp2lnaavdfk14n0wf7gl04n061cn4n08sy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "silently")))
    (native-inputs (list ghc-nanospec ghc-temporary ghc-nanospec ghc-temporary))
    (home-page "https://github.com/hspec/silently")
    (synopsis "Prevent writing to stdout")
    (description "This package provides functions to prevent or capture
writing to stdout and other handles.")
    (license license:bsd-3)))

(define-public ghc-silently-bootstrap
  (package
    (inherit ghc-silently)
    (name "ghc-silently-bootstrap")
    (arguments `(#:tests? #f))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-simple-reflect
  (package
    (name "ghc-simple-reflect")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "simple-reflect" version))
       (sha256
        (base32 "0ayvrx5cm8n6db21jiyjmk5h93pw7cz1707hih09hlhk9jh5x0h7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "simple-reflect")))
    (home-page
     "http://twanvl.nl/blog/haskell/simple-reflection-of-expressions")
    (synopsis "Simple reflection of expressions containing variables")
    (description
     "This package allows simple reflection of expressions containing
variables.  Reflection here means that a Haskell expression is turned into a
string.  The primary aim of this package is teaching and understanding; there
are no options for manipulating the reflected expressions beyond showing
them.")
    (license license:bsd-3)))

(define-public ghc-simple-sendfile
  (package
    (name "ghc-simple-sendfile")
    (version "0.2.32")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "simple-sendfile" version))
       (sha256
        (base32 "18r8d1in3x9r1p9fmvmypf9922icjbm1ksvqvp95dnf3gakd0a4z"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "simple-sendfile")))
    (inputs (list ghc-network))
    (native-inputs (list ghc-hunit
                         ghc-conduit
                         ghc-conduit-extra
                         ghc-easy-file
                         ghc-hspec
                         hspec-discover
                         ghc-resourcet))
    (home-page "http://hackage.haskell.org/package/simple-sendfile")
    (synopsis "Cross platform library for the sendfile system call")
    (description "This library tries to call minimum system calls which
are the bottleneck of web servers.")
    (license license:bsd-3)))

(define-public ghc-size-based
  (package
    (name "ghc-size-based")
    (version "0.1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "size-based" version))
       (sha256
        (base32 "1xc31iy57v9hm97hhr26ws2wwsf56gczwnq7q8ckiy5pgw6fmr1g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "size-based")))
    (inputs (list ghc-dictionary-sharing ghc-testing-type-modifiers))
    (arguments
     `(#:cabal-revision ("1"
                         "0idqj2k42anjwaq0zi6x7iz9jbwy6z3q1zjiml44v2ak21dswxga")))
    (home-page "http://hackage.haskell.org/package/size-based")
    (synopsis "Sized functors for size-based enumerations")
    (description "This library provides a framework for size-based
enumerations.")
    (license license:bsd-3)))

(define-public ghc-skylighting-core
  (package
    (name "ghc-skylighting-core")
    (version "0.14.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "skylighting-core" version))
       (sha256
        (base32 "1zvdgrqqr9xlbnrrp2lbrxqdbpjl11j13qbpibp5rl3y5azqn89y"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "skylighting-core")))
    (inputs (list ghc-aeson
                  ghc-case-insensitive
                  ghc-attoparsec
                  ghc-utf8-string
                  ghc-xml-conduit
                  ghc-safe
                  ghc-base64-bytestring
                  ghc-colour))
    (native-inputs (list ghc-tasty
                         ghc-tasty-golden
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-quickcheck
                         ghc-diff
                         ghc-pretty-show))
    (arguments
     `(#:cabal-revision ("1"
                         "0yz5yjvllkxwjzdg7jhm7ma8lv7ymwy3cl4myv0j4krgp30lcdc8")))
    (home-page "https://github.com/jgm/skylighting")
    (synopsis "Syntax highlighting library")
    (description
     "Skylighting is a syntax highlighting library with support
for over one hundred languages.  It derives its tokenizers from XML syntax
definitions used by KDE's @code{KSyntaxHighlighting} framework, so any syntax
supported by that framework can be added.  An optional command-line program is
provided.  Skylighting is intended to be the successor to highlighting-kate.")
    (license license:bsd-3)))

(define-public ghc-skylighting-format-blaze-html
  (package
    (name "ghc-skylighting-format-blaze-html")
    (version "0.1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "skylighting-format-blaze-html" version))
       (sha256
        (base32 "1rjjfcnq1395zmgxv123yy3khia7swrhcr77h8lg4h5jxgm2rs24"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "skylighting-format-blaze-html")))
    (inputs (list ghc-skylighting-core ghc-blaze-html))
    (home-page "https://github.com/jgm/skylighting")
    (synopsis "HTML formatter for skylighting syntax highlighting library")
    (description
     "This module allows tokens produced by skylighting-core to be rendered as HTML.")
    (license license:bsd-3)))

(define-public ghc-skylighting-format-latex
  (package
    (name "ghc-skylighting-format-latex")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "skylighting-format-latex" version))
       (sha256
        (base32 "0y7v5aifwar24i976pw32scfdywjwy2ad05ajhdf8l84nsd6rdlp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "skylighting-format-latex")))
    (inputs (list ghc-skylighting-core))
    (home-page "https://github.com/jgm/skylighting")
    (synopsis "LaTeX formatter for skylighting syntax highlighting library")
    (description
     "This module allows tokens produced by skylighting-core to be rendered as LaTeX
macros.")
    (license license:bsd-3)))

(define-public ghc-skylighting-format-context
  (package
    (name "ghc-skylighting-format-context")
    (version "0.1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "skylighting-format-context" version))
       (sha256
        (base32 "1gc8pjbhd1npka22m5m7s5333jcqxskgzmqj17m95dl97phi6hh0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "skylighting-format-context")))
    (inputs (list ghc-skylighting-core))
    (home-page "https://github.com/jgm/skylighting")
    (synopsis "ConTeXt formatter for skylighting syntax highlighting library")
    (description
     "This module allows tokens produced by skylighting-core to be rendered as ConTeXt
commands.")
    (license license:bsd-3)))

(define-public ghc-skylighting-format-ansi
  (package
    (name "ghc-skylighting-format-ansi")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "skylighting-format-ansi" version))
       (sha256
        (base32 "16qavv10g5yqwi60axj7q595ll605vmnfjgdxyi029nd5rnaipr3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "skylighting-format-ansi")))
    (inputs (list ghc-skylighting-core ghc-ansi-terminal ghc-colour))
    (home-page "https://github.com/jgm/skylighting")
    (synopsis "ANSI formatter for skylighting syntax highlighting library")
    (description
     "This module allows tokens produced by skylighting-core to be rendered as ANSI
colored text.")
    (license license:bsd-3)))

(define-public ghc-skylighting-format-typst
  (package
    (name "ghc-skylighting-format-typst")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "skylighting-format-typst" version))
       (sha256
        (base32 "1r1cczzi2in239a474ikbaf0x0y5yz2p8ik4nyzihs3gjzdx4k4r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "skylighting-format-typst")))
    (inputs (list ghc-skylighting-core))
    (home-page "https://github.com/jgm/skylighting")
    (synopsis "Typst formatter for skylighting syntax highlighting library")
    (description
     "This module allows tokens produced by skylighting-core to be rendered as Typst.")
    (license license:bsd-3)))

(define-public ghc-skylighting
  (package
    (name "ghc-skylighting")
    (version "0.14.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "skylighting" version))
       (sha256
        (base32 "1g73jsxq4ybbghiqyrs3aly5km90qsmp2yvyp399hr7f7pjkpqza"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "skylighting")))
    (inputs (list ghc-skylighting-core
                  ghc-skylighting-format-ansi
                  ghc-skylighting-format-context
                  ghc-skylighting-format-latex
                  ghc-skylighting-format-blaze-html
                  ghc-skylighting-format-typst
                  ghc-pretty-show
                  ghc-blaze-html))
    (home-page "https://github.com/jgm/skylighting")
    (synopsis "Syntax highlighting library")
    (description
     "Skylighting is a syntax highlighting library with support for over one hundred
languages.  It derives its tokenizers from XML syntax definitions used by KDE's
KSyntaxHighlighting framework, so any syntax supported by that framework can be
added.  An optional command-line program is provided.  Skylighting is intended
to be the successor to highlighting-kate.  This package provides generated
syntax modules based on the KDE XML definitions provided by the
@code{skylighting-core} package.")
    (license license:gpl2)))

(define-public ghc-smallcheck
  (package
    (name "ghc-smallcheck")
    (version "1.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "smallcheck" version))
       (sha256
        (base32 "07zyb3hnq242mdwak5briqc48wakp9pjsfizl78l06070i824hz0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "smallcheck")))
    (inputs (list ghc-logict ghc-semigroups ghc-nats ghc-void))
    (home-page "https://github.com/Bodigrim/smallcheck")
    (synopsis "Property-based testing library")
    (description
     "SmallCheck is a testing library that verifies
properties for all test cases up to some depth.  The test cases are generated
automatically by SmallCheck.")
    (license license:bsd-3)))

(define-public ghc-socks
  (package
    (name "ghc-socks")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "socks" version))
       (sha256
        (base32 "0wvaxy3dkv97wrncjv1rxrmjr4014hgxz82kixvcwqdhidalfi3k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "socks")))
    (inputs (list ghc-cereal ghc-network ghc-basement))
    (home-page "http://github.com/vincenthz/hs-socks")
    (synopsis "SOCKS proxy (version 5) implementation")
    (description
     "This library provides a SOCKS proxy (version 5) implementation.")
    (license license:bsd-3)))

(define-public ghc-sop-core
  (package
    (name "ghc-sop-core")
    (version "0.5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "sop-core" version))
       (sha256
        (base32 "0rbj56icbaqlcxx5xwvbx4n4vmyv6cfcv7s45n1fv3drahigvgw7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sop-core")))
    (arguments
     `(#:cabal-revision ("5"
                         "0kls940ickggjbib991d2f5hfcci0v7bgx8977gq0ca2zyplrdqb")))
    (home-page "http://hackage.haskell.org/package/sop-core")
    (synopsis "True Sums of Products")
    (description
     "This package provides an implementation of
@math{n}-ary sums and @math{n}-ary products.  The module @code{Data.SOP}
is the main module of this library and contains more detailed
documentation.  The main use case of this package is to serve as the
core of @url{https://hackage.haskell.org/package/generics-sop,
generics-sop}.")
    (license license:bsd-3)))

(define-public ghc-special-values
  (package
    (name "ghc-special-values")
    (version "0.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "special-values" version))
       (sha256
        (base32 "1kkdw2c4d2hha99v9f89ahmifjxp7fxmxyfwq9a8xk6s0h9xs51w"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "special-values")))
    (inputs (list ghc-scientific ghc-ieee754 ghc-nats))
    (arguments
     `(#:cabal-revision ("3"
                         "1g9bcyawr0dvbn0402fh90fiv8bhpblxman9lk9q9b5c8k35x1ga")))
    (home-page "https://github.com/minad/special-values#readme")
    (synopsis "Typeclass providing special values")
    (description
     "Special values are provided by a SpecialValues typeclass.  Those can be
used for example by QuickCheck, see quickcheck-special.")
    (license license:expat)))

(define-public ghc-split
  (package
    (name "ghc-split")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "split" version))
       (sha256
        (base32 "04x9figcib2zwki2rk29i2n5r73ykbyx9j2lhcisphcphd741njj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "split")))
    (native-inputs (list ghc-quickcheck))
    (home-page "http://hackage.haskell.org/package/split")
    (synopsis "Combinator library for splitting lists")
    (description
     "This package provides a collection of Haskell functions for
splitting lists into parts, akin to the @code{split} function found in several
mainstream languages.")
    (license license:bsd-3)))

(define-public ghc-splitmix
  (package
    (name "ghc-splitmix")
    (version "0.1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "splitmix" version))
       (sha256
        (base32 "0w32z3rhsnijb9s5k6h60rhbzgzkw8xq1glfbjbl1znlkgbx1g5n"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "splitmix")))
    (native-inputs (list ghc-hunit
                         ghc-math-functions
                         ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-async
                         ghc-random
                         ghc-tf-random
                         ghc-vector
                         ghc-base-compat-batteries
                         testu01))
    (home-page "http://hackage.haskell.org/package/splitmix")
    (synopsis "Fast and splittable pseudorandom number generator")
    (description
     "This package provides a Pure Haskell implementation of the
SplitMix pseudorandom number generator.  SplitMix is a \"splittable\"
pseudorandom number generator that is quite fast: 9 64-bit
arithmetic/logical operations per 64 bits generated.  SplitMix is tested
with two standard statistical test suites (DieHarder and TestU01, this
implementation only using the former) and it appears to be adequate for
\"everyday\" use, such as Monte Carlo algorithms and randomized data
structures where speed is important.  In particular, it @strong{should not
be used for cryptographic or security applications}, because generated
sequences of pseudorandom values are too predictable (the mixing functions
are easily inverted, and two successive outputs suffice to reconstruct the
internal state).")
    (license license:bsd-3)))

(define-public ghc-splitmix-bootstrap
  (package
    (inherit ghc-splitmix)
    (name "ghc-splitmix-bootstrap")
    (arguments `(#:tests? #f))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-spoon
  (package
    (name "ghc-spoon")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "spoon" version))
       (sha256
        (base32 "1m41k0mfy6fpfrv2ym4m5jsjaj9xdfl2iqpppd3c4d0fffv51cxr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "spoon")))
    (arguments
     `(#:cabal-revision ("1"
                         "09s5jjcsg4g4qxchq9g2l4i9d5zh3rixpkbiysqcgl69kj8mwv74")))
    (home-page "http://hackage.haskell.org/package/spoon")
    (synopsis "Catch errors thrown from pure computations")
    (description
     "Takes an error-throwing expression and puts it back in the Maybe it
belongs in.

Note that this suffers from the
@url{https://ghc.haskell.org/trac/ghc/ticket/5902}.  Buyer beware.")
    (license license:bsd-3)))

(define-public ghc-statevar
  (package
    (name "ghc-statevar")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "StateVar" version))
       (sha256
        (base32 "098q4lk60najzpbfal4bg4sh7izxm840aa5h4ycaamjn77d3jjsy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "StateVar")))
    (home-page "https://github.com/haskell-opengl/StateVar")
    (synopsis "State variables for Haskell")
    (description
     "This package provides state variables, which are references
in the @code{IO} monad, like @code{IORef}s or parts of the OpenGL state.")
    (license license:bsd-3)))

(define-public ghc-statistics
  (package
    (name "ghc-statistics")
    (version "0.16.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "statistics" version))
       (sha256
        (base32 "1rx1dckaj54hzx03zqf4rz43hp80rxxgi8dp31rwy9qjckk4dv03"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "statistics")))
    (inputs (list ghc-math-functions
                  ghc-mwc-random
                  ghc-random
                  ghc-aeson
                  ghc-async
                  ghc-primitive
                  ghc-dense-linear-algebra
                  ghc-parallel
                  ghc-vector
                  ghc-vector-algorithms
                  ghc-vector-th-unbox
                  ghc-vector-binary-instances
                  ghc-data-default-class))
    (native-inputs (list ghc-quickcheck
                         ghc-erf
                         ghc-ieee754
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-tasty-expected-failure
                         ghc-doctest))
    (arguments
     (list
      #:cabal-revision
      '("1" "1996zyq4n7c5zh36h3nhzx5xyd7z6fa3mqsldrgii56g7ixq1rkz")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                (let* ((tmpdir (or (getenv "TMP")
                                   "/tmp"))
                       (dependency-package-db
                        (string-append tmpdir "/package.conf.d")))
                  (invoke "runhaskell" "Setup.hs" "test" "statistics-tests")
                  (setenv "GHC_PACKAGE_PATH" dependency-package-db)
                  (invoke "./dist/build/statistics-doctests/statistics-doctests")
                  (unsetenv "GHC_PACKAGE_PATH"))
                (format #t "Testsuite not run.%~")))))))
    (home-page "https://github.com/haskell/statistics")
    (synopsis "Haskell library of statistical types, data, and functions")
    (description
     "This library provides a number of common functions
and types useful in statistics.  We focus on high performance, numerical
robustness, and use of good algorithms.  Where possible, we provide references
to the statistical literature.

The library's facilities can be divided into four broad categories:

@itemize
@item Working with widely used discrete and continuous probability
distributions.  (There are dozens of exotic distributions in use; we focus
on the most common.)

@item Computing with sample data: quantile estimation, kernel density
estimation, histograms, bootstrap methods, significance testing,
and regression and autocorrelation analysis.

@item Random variate generation under several different distributions.

@item Common statistical tests for significant differences between samples.
@end itemize")
    (license license:bsd-2)))

(define-public ghc-stm-chans
  (package
    (name "ghc-stm-chans")
    (version "3.0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "stm-chans" version))
       (sha256
        (base32 "0p9jq5fq3g77kf2kq807zrwqpw0z9a6zhw57h21wk4yb6zshs1ks"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "stm-chans")))
    (home-page "https://wrengr.org/software/hackage.html")
    (synopsis "Additional types of channels for ghc-stm")
    (description
     "This Haskell package offers a collection of channel types,
similar to @code{Control.Concurrent.STM.@{TChan,TQueue@}} but with additional
features.")
    (license license:bsd-3)))

(define-public ghc-stm-conduit
  (package
    (name "ghc-stm-conduit")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "stm-conduit" version))
       (sha256
        (base32 "0hhlxvpp7mah8dcvkknh6skx44jfk3092zz2w52zlr255bkmn3p8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "stm-conduit")))
    (inputs (list ghc-stm-chans
                  ghc-cereal
                  ghc-cereal-conduit
                  ghc-conduit
                  ghc-conduit-extra
                  ghc-resourcet
                  ghc-async
                  ghc-monad-loops
                  ghc-unliftio))
    (native-inputs (list ghc-doctest
                         ghc-quickcheck
                         ghc-hunit
                         ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2))
    (arguments
     `(#:cabal-revision ("1"
                         "1iyk2wfkpyq3jn0lybgf21b95rmkzgpvr8m066j06z4xngcvab36")))
    (home-page "https://github.com/cgaebel/stm-conduit")
    (synopsis
     "Introduces conduits to channels and promotes using conduits concurrently")
    (description
     "This package provides two simple conduit wrappers around STM channels: a
source and a sink.")
    (license license:bsd-3)))

(define-public ghc-stm-delay
  (package
    (name "ghc-stm-delay")
    (version "0.1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "stm-delay" version))
       (sha256
        (base32 "0k60cpqzqy8c6xk5qw5135a7hlxnh670kb7fhjmz819hsi1n7vq5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "stm-delay")))
    (native-inputs (list ghc-async ghc-async))
    (home-page "https://github.com/joeyadams/haskell-stm-delay")
    (synopsis "Updatable one-shot timer polled with STM")
    (description
     "This library lets you create a one-shot timer, poll it using STM, and
update it to ring at a different time than initially specified.  It uses GHC
event manager timeouts when available, yielding performance similar to
@code{threadDelay} and @code{registerDelay}.  Otherwise, it falls back to
forked threads and @code{threadDelay}.")
    (license license:bsd-3)))

(define-public ghc-stmonadtrans
  (package
    (name "ghc-stmonadtrans")
    (version "0.4.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "STMonadTrans" version))
       (sha256
        (base32 "0559yqgkcs520bhipf9pxy6rc7ydy9m48yj5s233g67g83k1ya3v"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "STMonadTrans")))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit))
    (home-page "https://github.com/josefs/STMonadTrans")
    (synopsis "Monad transformer version of the ST monad")
    (description
     "This package provides a monad transformer version of the @code{ST} monad
for strict state threads.")
    (license license:bsd-3)))

(define-public ghc-storable-complex
  (package
    (name "ghc-storable-complex")
    (version "0.2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "storable-complex" version))
       (sha256
        (base32 "0fnwbfmd5vsaaqvf9182qdcjrzcfjd1zhdyvjwzifbwvn6r9kx4s"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "storable-complex")))
    (inputs (list ghc-base-orphans))
    (home-page "https://github.com/cartazio/storable-complex")
    (synopsis "Haskell Storable instance for Complex")
    (description
     "This package provides a Haskell library including a
Storable instance for Complex which is binary compatible with C99, C++
and Fortran complex data types.")
    (license license:bsd-3)))

(define-public ghc-storable-record
  (package
    (name "ghc-storable-record")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "storable-record" version))
       (sha256
        (base32 "1c1f58v13nxpq2ix30d2kpvsamk44apl6ms1a2pq54fkjk44didy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "storable-record")))
    (inputs (list ghc-quickcheck ghc-semigroups ghc-utility-ht
                  ghc-storablevector ghc-timeit))
    (home-page "http://code.haskell.org/~thielema/storable-record/")
    (synopsis "Elegant definition of Storable instances for records")
    (description
     "With this package you can build a Storable instance of
a record type from Storable instances of its elements in an elegant way.
It does not do any magic, just a bit arithmetic to compute the right
offsets, that would be otherwise done manually or by a preprocessor like
C2HS.  There is no guarantee that the generated memory layout is
compatible with that of a corresponding C struct.  However, the module
generates the smallest layout that is possible with respect to the
alignment of the record elements.")
    (license license:bsd-3)))

(define-public ghc-storable-tuple
  (package
    (name "ghc-storable-tuple")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "storable-tuple" version))
       (sha256
        (base32 "0g2rhqxrl1yjvvqwxmfgflgyyrds0kkcvzjjmwk07mir8aj4yjq3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "storable-tuple")))
    (inputs (list ghc-storable-record ghc-utility-ht ghc-base-orphans))
    (home-page "http://code.haskell.org/~thielema/storable-tuple/")
    (synopsis "Storable instance for pairs and triples")
    (description
     "This package provides a Storable instance for pairs
and triples which should be binary compatible with C99 and C++.  The
only purpose of this package is to provide a standard location for this
instance so that other packages needing this instance can play nicely
together.")
    (license license:bsd-3)))

(define-public ghc-storablevector
  (package
    (name "ghc-storablevector")
    (version "0.2.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "storablevector" version))
       (sha256
        (base32 "03nq5930yjpdvnyh93pjxzh3xjsracnnzcyqc0j3yiwadggbjy35"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "storablevector")))
    (inputs (list ghc-non-negative
                  ghc-utility-ht
                  ghc-semigroups
                  ghc-unsafe
                  ghc-quickcheck
                  ghc-syb))
    (native-inputs (list ghc-random))
    (arguments
     `(#:cabal-revision ("1"
                         "0rc3y0sw2lf92cxhrbpcypb7hp4s4cspj81ragcs6sxvf0jj79j2")))
    (home-page "http://www.haskell.org/haskellwiki/Storable_Vector")
    (synopsis "Fast, packed, strict storable arrays with a list interface")
    (description
     "This library provides fast, packed, strict storable
arrays with a list interface, a chunky lazy list interface with variable
chunk size and an interface for write access via the ST monad.  This is
much like bytestring and binary but can be used for every
@code{Foreign.Storable.Storable} type.  See also
@url{https://hackage.haskell.org/package/vector}, a library with a
similar intention.

This library does not do advanced fusion optimization, since especially
for lazy vectors this would either be incorrect or not applicable.  See
@url{https://hackage.haskell.org/package/storablevector-streamfusion} for
a library that provides fusion with lazy lists.")
    (license license:bsd-3)))

(define-public ghc-streaming-commons
  (package
    (name "ghc-streaming-commons")
    (version "0.2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "streaming-commons" version))
       (sha256
        (base32 "0mqyxdikd76q0ls5lz0bfdwzqhyvf8hwxl5x1c5lgfas3zwllf16"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "streaming-commons")))
    (inputs (list ghc-async ghc-network ghc-random ghc-zlib))
    (native-inputs (list ghc-hspec ghc-quickcheck hspec-discover))
    (home-page "https://github.com/fpco/streaming-commons")
    (synopsis "Conduit and pipes needed by some streaming data libraries")
    (description
     "This package provides low-dependency functionality commonly
needed by various Haskell streaming data libraries, such as @code{conduit} and
@code{pipe}s.")
    (license license:expat)))

(define-public ghc-strict
  (package
    (name "ghc-strict")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "strict" version))
       (sha256
        (base32 "12rgzrxb1dz5qb1sqmwdyiyhxbpa5rrzlyr293ki4qx8qa094wbp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "strict")))
    (inputs (list ghc-assoc ghc-hashable ghc-these))
    (arguments
     `(#:cabal-revision ("1"
                         "1wh1p76sahrzqd58kdlvk85c38cr7w3ib33cb95bp33lqyvp7hsq")))
    (home-page "https://github.com/haskell-strict/strict")
    (synopsis "Strict data types and String IO")
    (description
     "This package provides strict versions of some standard Haskell data
types, such as pairs, @code{Maybe} and @code{Either}.  It also contains strict
IO operations.")
    (license license:bsd-3)))

(define-public ghc-stringbuilder
  (package
    (name "ghc-stringbuilder")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "stringbuilder" version))
       (sha256
        (base32 "1fh3csx1wcssn8xyvl4ip4aprh9l4qyz2kk8mgjvqvc0vb2bsy6q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "stringbuilder")))
    (native-inputs (list ghc-hspec ghc-quickcheck))
    (home-page "http://hackage.haskell.org/package/stringbuilder")
    (synopsis "Writer monad for multi-line string literals")
    (description "This package provides a writer monad for multi-line string
literals.")
    (license license:expat)))

(define-public ghc-string-interpolate
  (package
    (name "ghc-string-interpolate")
    (version "0.3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "string-interpolate" version))
       (sha256
        (base32 "13hb3spabggr6gsn9xhwpwldjvpl2l7z4lgssis82c40n108b0w8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "string-interpolate")))
    (inputs (list ghc-split ghc-haskell-src-exts ghc-haskell-src-meta
                  ghc-text-conversions ghc-utf8-string))
    (native-inputs (list ghc-quickcheck
                         ghc-hspec
                         ghc-hspec-core
                         ghc-quickcheck-instances
                         ghc-quickcheck-text
                         ghc-quickcheck-unicode
                         ghc-unordered-containers))
    (arguments
     `(#:cabal-revision ("3"
                         "0grq9v023186gfq3a2as9974qlwcjx3dhxqczpq22bq2wfpw24x7")))
    (home-page
     "https://gitlab.com/williamyaoh/string-interpolate/blob/master/README.md")
    (synopsis "Haskell string/text/bytestring interpolation that just works")
    (description
     "This package provides QuasiQuoters for Unicode-aware string interpolation
that handles all textual types.")
    (license license:bsd-3)))

(define-public ghc-string-qq
  (package
    (name "ghc-string-qq")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "string-qq" version))
       (sha256
        (base32 "14k4wxp3fgy5yl0hg1m06lrrpligp1xmi0v54fhwqh0x6nvmbs23"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "string-qq")))
    (native-inputs (list ghc-hunit))
    (home-page "http://hackage.haskell.org/package/string-qq")
    (synopsis
     "QuasiQuoter for non-interpolated strings, texts and bytestrings")
    (description
     "This package provides a quasiquoter for non-interpolated strings, texts
and bytestrings.")
    (license license:public-domain)))

(define-public ghc-stringsearch
  (package
    (name "ghc-stringsearch")
    (version "0.3.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "stringsearch" version))
       (sha256
        (base32 "0jpy9xjcjdbpi3wk6mg7xwd7wfi2mma70p97v1ij5i8bj9qijpr9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "stringsearch")))
    (arguments
     `(#:cabal-revision ("2"
                         "01w6m4fxxccm9xh63648y6fs50y7l727sjrpda5b21k0jh1vnwnd")))
    (home-page "https://bitbucket.org/dafis/stringsearch")
    (synopsis "Fast searching, splitting and replacing of ByteStrings")
    (description
     "This package provides several functions to quickly search
for substrings in strict or lazy @code{ByteStrings}.  It also provides
functions for breaking or splitting on substrings and replacing all
occurrences of a substring (the first in case of overlaps) with another.")
    (license license:bsd-3)))

(define-public ghc-svg-builder
  (package
    (name "ghc-svg-builder")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "svg-builder" version))
       (sha256
        (base32 "1k420f497lzkymmxin88ql6ib8dziic43avykv31yq65rgrf7l2g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "svg-builder")))
    (inputs (list ghc-blaze-builder ghc-hashable ghc-unordered-containers))
    (arguments
     `(#:cabal-revision ("11"
                         "01dxjh49d6kysmvzv529s4jniqpc875sdnkia9dvxx6b73mrsyqs")))
    (home-page "https://github.com/diagrams/svg-builder.git")
    (synopsis "Domain-specific language for building Scalable Vector Graphics")
    (description "Easy-to-write domain-specific language (DSL) for
building Scalable Vector Graphics (SVG).")
    (license license:bsd-3)))

(define-public ghc-syb
  (package
    (name "ghc-syb")
    (version "0.7.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "syb" version))
       (sha256
        (base32 "0q0y5412766xz90lghs4sdna48hawk7csqb3708bjann4a41wz7c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "syb")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/dreixel/syb")
    (synopsis "Scrap Your Boilerplate")
    (description
     "This package contains the generics system described in the
/Scrap Your Boilerplate/ papers (see
@uref{http://www.cs.uu.nl/wiki/GenericProgramming/SYB, the website}).  It
defines the @code{Data} class of types permitting folding and unfolding of
constructor applications, instances of this class for primitive types, and a
variety of traversals.")
    (license license:bsd-3)))

(define-public ghc-system-fileio
  (package
    (name "ghc-system-fileio")
    (version "0.3.16.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "system-fileio" version))
       (sha256
        (base32 "16593sfb47snq4vixl0qv6119j3yva0nynygz24vcw3ggqmflhrp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "system-fileio")))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "system-fileio.cabal"
               (("chell >= 0\\.4 && < 0\\.5") "chell >= 0.4"))
             #t)))))
    (inputs (list ghc-system-filepath))
    (native-inputs (list ghc-chell ghc-temporary))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "Consistent file system interaction across GHC versions")
    (description
     "This is a small wrapper around the directory, unix, and Win32 packages,
for use with system-filepath.  It provides a consistent API to the various
versions of these packages distributed with different versions of GHC.
In particular, this library supports working with POSIX files that have paths
which can't be decoded in the current locale encoding.")
    (license license:expat)))

(define-public ghc-system-filepath
  (package
    (name "ghc-system-filepath")
    (version "0.4.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "system-filepath" version))
       (sha256
        (base32 "19fs8g1p07ckb0ydak4fczz58ngy3aywkliv1hbcvlc5w512j8ig"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "system-filepath")))
    (native-inputs
     (list ghc-chell ghc-chell-quickcheck ghc-quickcheck))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "High-level, byte-based file and directory path manipulations")
    (description
     "Provides a FilePath datatype and utility functions for operating on it.
Unlike the filepath package, this package does not simply reuse String,
increasing type safety.")
    (license license:expat)))

(define-public ghc-tabular
  (package
    (name "ghc-tabular")
    (version "0.2.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tabular" version))
       (sha256
        (base32 "0z936gh8n8i8qdkagyxwd9gqq13skd5fv013vdvwsibrxkm0czfb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tabular")))
    (inputs (list ghc-csv ghc-html))
    (arguments
     `(#:cabal-revision ("1"
                         "1llb610scvdb9grkd0y7ilh39a3v2pnkgaknabq2an3izr49z01r")))
    (home-page "https://github.com/bgamari/tabular")
    (synopsis "Two-dimensional data tables with rendering functions")
    (description
     "Tabular provides a Haskell representation of two-dimensional data
tables, the kind that you might find in a spreadsheet or or a research report.
It also comes with some default rendering functions for turning those tables
into ASCII art, simple text with an arbitrary delimiter, CSV, HTML or LaTeX.

Below is an example of the kind of output this library produces.  The tabular
package can group rows and columns, each group having one of three
separators (no line, single line, double line) between its members.

@example

    || memtest 1 | memtest 2 ||  time test  | time test 2
====++===========+===========++=============+============
A 1 ||       hog |  terrible ||        slow |      slower
A 2 ||       pig |   not bad ||        fast |     slowest
----++-----------+-----------++-------------+------------
B 1 ||      good |     awful || intolerable |    bearable
B 2 ||    better | no chance ||    crawling |     amazing
B 3 ||       meh |   well... ||  worst ever |          ok

@end example")
    (license license:bsd-3)))

(define-public ghc-tagged
  (package
    (name "ghc-tagged")
    (version "0.8.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tagged" version))
       (sha256
        (base32 "1137jm5zbnirv7padqqhc0ky8l3npqn8v2fjasjscjs1pf7diakd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tagged")))
    (home-page "http://github.com/ekmett/tagged")
    (synopsis "Haskell phantom types to avoid passing dummy arguments")
    (description "This library provides phantom types for Haskell 98, to avoid
having to unsafely pass dummy arguments.")
    (license license:bsd-3)))

(define-public ghc-tar
  (package
    (name "ghc-tar")
    (version "0.6.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tar" version))
       (sha256
        (base32 "1apkq11xg0rqbgs83hag85r4ibdw7v09n1qj0l0962d80h0aajbr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tar")))
    (inputs (list ghc-directory-ospath-streaming ghc-file-io))
    (native-inputs (list ghc-file-embed
                         ghc-quickcheck
                         ghc-tasty
                         ghc-tasty-quickcheck
                         ghc-temporary))
    (arguments
     `(#:cabal-revision ("2"
                         "074f5a4qcdl5vb7334i41azj4aj8i5ql03qrlr3hb5smxhvvk386")))
    (home-page "http://hackage.haskell.org/package/tar")
    (synopsis "Reading, writing and manipulating \".tar\" archive files")
    (description
     "This library is for working with \\\"@.tar@\\\" archive files.
It can read and write a range of common variations of the tar archive format
including V7, POSIX USTAR and GNU formats.  It provides support for packing and
unpacking portable archives.  This makes it suitable for distribution but not
backup because details like file ownership and exact permissions are not
preserved.  It also provides features for random access to archive content using
an index.")
    (license license:bsd-3)))

(define-public ghc-tar-conduit
  (package
    (name "ghc-tar-conduit")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tar-conduit" version))
       (sha256
        (base32 "0kavvr0sc1bmzimgpa00hdrihnvfpi4f9rmkg3yl8pckgv2ad8b1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tar-conduit")))
    (inputs (list ghc-conduit ghc-conduit-combinators ghc-safe-exceptions))
    (native-inputs (list ghc-quickcheck ghc-conduit-extra ghc-hspec ghc-hspec
                         ghc-weigh))
    (home-page "https://github.com/snoyberg/tar-conduit#readme")
    (synopsis "Extract and create tar files using conduit for streaming")
    (description "This library provides a conduit-based, streaming
interface for extracting and creating tar files.")
    (license license:expat)))

(define-public ghc-temporary
  (package
    (name "ghc-temporary")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "temporary" version))
       (sha256
        (base32 "144qhwfwg37l3k313raf4ssiz16jbgwlm1nf4flgqpsbd69jji4c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "temporary")))
    (inputs (list ghc-random))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-base-compat))
    (home-page "https://github.com/feuerbach/temporary")
    (synopsis "Temporary file and directory support")
    (description
     "The functions for creating temporary files and directories
in the Haskelll base library are quite limited.  This library just repackages
the Cabal implementations of its own temporary file and folder functions so
that you can use them without linking against Cabal or depending on it being
installed.")
    (license license:bsd-3)))

(define-public ghc-temporary-bootstrap
  (package
    (inherit ghc-temporary)
    (name "ghc-temporary-bootstrap")
    (arguments `(#:tests? #f))
    (inputs (list ghc-random-bootstrap))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-temporary-rc
  (package
    (name "ghc-temporary-rc")
    (version "1.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "temporary-rc" version))
       (sha256
        (base32 "1nqih0qks439k3pr5kmbbc8rjdw730slrxlflqb27fbxbzb8skqs"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "temporary-rc")))
    (home-page "http://www.github.com/feuerbach/temporary")
    (synopsis "Portable temporary file and directory support")
    (description
     "The functions for creating temporary files and directories in the base
library are quite limited.  The unixutils package contains some good ones, but
they aren't portable to Windows.  This library just repackages the Cabal
implementations of its own temporary file and folder functions so that you can
use them without linking against Cabal or depending on it being installed.
This is a better maintained fork of the \"temporary\" package.")
    (license license:bsd-3)))

(define-public ghc-terminal-size
  (package
    (name "ghc-terminal-size")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "terminal-size" version))
       (sha256
        (base32 "0jbznrlf95lc6ajhh26h1qgcmbr3bj753i8jlkrsrnkcjbb71w5h"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "terminal-size")))
    (arguments
     `(#:cabal-revision ("1"
                         "0w2pjmravr1fnvmrcnj13dxrf43miqypmh0fhrz1g1pb1qrg3mr1")))
    (home-page "http://hackage.haskell.org/package/terminal-size")
    (synopsis "Get terminal window height and width")
    (description "Get terminal window height and width without ncurses
dependency.")
    (license license:bsd-3)))

(define-public ghc-texmath
  (package
    (name "ghc-texmath")
    (version "0.12.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "texmath" version))
       (sha256
        (base32 "0xpv5zxaixn2kkc3kn547jg7rkg6bl2mrmxiwvxf2r0qgj4kmr2p"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "texmath")))
    (inputs (list ghc-syb ghc-xml ghc-pandoc-types ghc-typst-symbols ghc-split))
    (native-inputs (list ghc-pretty-show ghc-tasty ghc-tasty-golden ghc-tagged))
    (home-page "http://github.com/jgm/texmath")
    (synopsis "Conversion between formats used to represent mathematics")
    (description
     "The texmath library provides functions to read and write TeX math,
presentation MathML, and OMML (Office Math Markup Language, used in Microsoft
Office).  Support is also included for converting math formats to pandoc's
native format (allowing conversion, via pandoc, to a variety of different
markup formats).  The TeX reader supports basic LaTeX and AMS extensions, and
it can parse and apply LaTeX macros.")
    (license license:gpl2)))

(define-public ghc-text-ansi
  (package
    (name "ghc-text-ansi")
    (version "0.3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-ansi" version))
       (sha256
        (base32 "0iiq5zlcjh25mf9b4vlhyc1dwglwh8b03qj1wasbngzvmphvcy00"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-ansi")))
    (inputs (list ghc-text-builder-linear))
    (arguments
     `(#:cabal-revision ("2"
                         "1bjri84hc9qqp03zjmbrgrwgk989253x7jj2xzwbq579q5yxpj8h")))
    (home-page "https://github.com/awkward-squad/text-ansi")
    (synopsis "Text styling for ANSI terminals.")
    (description
     "Text styling for ANSI terminals using SGR codes, as defined by the
<https://www.ecma-international.org/publications-and-standards/standards/ecma-48
ECMA-48> standard. .  Supports foreground\\/background color, bold\\/faint
intensity, italic, single\\/double underline, strikethrough, frame, encircle, and
overline escape sequences.  Some styles may not work on your terminal. .  Also
features terminal detection, so redirecting styled output to a file will
automatically strip the ANSI escape sequences.")
    (license license:bsd-3)))

(define-public ghc-text-binary
  (package
    (name "ghc-text-binary")
    (version "0.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-binary" version))
       (sha256
        (base32 "18gl10pwg3qwsk0za3c70j4n6a9129wwf1b7d3a461h816yv55xn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-binary")))
    (home-page "https://github.com/kawu/text-binary")
    (synopsis "Binary instances for text types")
    (description
     "This package provides a compatibility layer providing @code{Binary}
instances for strict and lazy text types for versions older than 1.2.1 of the
text package.")
    (license license:bsd-2)))

(define-public ghc-text-builder-linear
  (package
    (name "ghc-text-builder-linear")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-builder-linear" version))
       (sha256
        (base32 "1nw2gazirrkylrfvnfyyqc4kw9s0cazya44phgiypv0m8nzvpjs9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-builder-linear")))
    (inputs (list ghc-quote-quot))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck))
    (home-page "https://github.com/Bodigrim/linear-builder")
    (synopsis "Builder for Text and ByteString based on linear types")
    (description
     "Strict Text and @code{ByteString} builder, which hides mutable buffer behind
linear types and takes amortized linear time.")
    (license license:bsd-3)))

(define-public ghc-text-manipulate
  (package
    (name "ghc-text-manipulate")
    (version "0.3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-manipulate" version))
       (sha256
        (base32 "1g06ldl6cdnyr31xlks5qm1sj44ccrdvq4bf8dk032mzfkpyyrws"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-manipulate")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/brendanhay/text-manipulate")
    (synopsis
     "Case conversion, word boundary manipulation, and textual subjugation")
    (description
     "Manipulate identifiers and structurally non-complex pieces of text by
delimiting word boundaries via a combination of whitespace,
control-characters, and case-sensitivity.

Has support for common idioms like casing of programmatic variable names,
taking, dropping, and splitting by word, and modifying the first character of
a piece of text.

Caution: this library makes heavy use of the text library's internal loop
optimisation framework.  Since internal modules are not guaranteed to have a
stable API there is potential for build breakage when the text dependency is
upgraded.  Consider yourself warned!")
    (license license:mpl2.0)))

(define-public ghc-text-metrics
  (package
    (name "ghc-text-metrics")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-metrics" version))
       (sha256
        (base32 "0gl5xxagdgs32m5xh58zlgwnysg0i19m31gg2lpm58x9d1bal81k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "text-metrics")))
    (inputs (list ghc-vector ghc-primitive))
    (native-inputs (list ghc-quickcheck ghc-hspec))
    (home-page "https://github.com/mrkkrp/text-metrics")
    (synopsis "Calculate various string metrics efficiently")
    (description "This library provides tools to calculate various
string metrics efficiently.")
    (license license:bsd-3)))

(define-public ghc-tf-random
  (package
    (name "ghc-tf-random")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tf-random" version))
       (sha256
        (base32 "0445r2nns6009fmq0xbfpyv7jpzwv0snccjdg7hwj4xk4z0cwc1f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tf-random")))
    (inputs (list ghc-primitive-bootstrap ghc-random))
    (home-page "http://hackage.haskell.org/package/tf-random")
    (synopsis "High-quality splittable pseudorandom number generator")
    (description
     "This package contains an implementation of a high-quality
splittable pseudorandom number generator.  The generator is based on a
cryptographic hash function built on top of the ThreeFish block cipher.  See
the paper \"Splittable Pseudorandom Number Generators Using Cryptographic
Hashing\" by Claessen, Pałka for details and the rationale of the design.")
    (license license:bsd-3)))

(define-public ghc-th-abstraction
  (package
    (name "ghc-th-abstraction")
    (version "0.7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-abstraction" version))
       (sha256
        (base32 "1i843j7lhq6qly9zknrw3nhb17ac3badmxwn1pfn2sscp951idpr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "th-abstraction")))
    (arguments
     `(#:cabal-revision ("1"
                         "1n2gpcajn2sm8zw5ibkjwvhd3k1m0lvvjrixwcq0c1xhhlyl049h")))
    (home-page "https://github.com/glguy/th-abstraction")
    (synopsis "Nicer interface for reified information about data types")
    (description
     "This package normalizes variations in the interface for inspecting
datatype information via Template Haskell so that packages and support a
single, easier to use informational datatype while supporting many versions of
Template Haskell.")
    (license license:isc)))

(define-public ghc-th-env
  (package
    (name "ghc-th-env")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-env" version))
       (sha256
        (base32 "01gmycna12sg2f0zslhjnjx8s86shsvmw5jpw5n5z93bvxkb20gw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "th-env")))
    (inputs (list ghc-th-compat))
    (native-inputs (list ghc-markdown-unlit))
    (home-page "https://github.com/dzhus/th-env")
    (synopsis
     "Template Haskell splices that expand to an environment variable")
    (description
     "This package provides a Template Haskell splice that expand to the value
of an environment variable.  This can be used to embed build-time parameters in
the application.")
    (license license:bsd-3)))

(define-public ghc-th-expand-syns
  (package
    (name "ghc-th-expand-syns")
    (version "0.4.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-expand-syns" version))
       (sha256
        (base32 "03zgqq0bwcjmy4d3m00gqzh4r1big2yh9v69rxvg72cw69krkq8q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "th-expand-syns")))
    (inputs (list ghc-syb ghc-th-abstraction))
    (home-page "https://github.com/DanielSchuessler/th-expand-syns")
    (synopsis "Expands type synonyms in Template Haskell ASTs")
    (description
     "This package enables users to expand type synonyms in Template Haskell
@dfn{abstract syntax trees} (ASTs).")
    (license license:bsd-3)))

(define-public ghc-th-lift
  (package
    (name "ghc-th-lift")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-lift" version))
       (sha256
        (base32 "07zgl42fq7ijmm29x7q10iv02hk6lqs7snysxdad03pq1vbrlc04"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "th-lift")))
    (inputs (list ghc-th-abstraction))
    (home-page "http://github.com/RyanGlScott/th-lift")
    (synopsis "Derive Template Haskell's Lift class for datatypes")
    (description
     "This is a Haskell library to derive Template Haskell's Lift class for
datatypes.")
    (license license:bsd-3)))

(define-public ghc-th-lift-instances
  (package
    (name "ghc-th-lift-instances")
    (version "0.1.20")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-lift-instances" version))
       (sha256
        (base32 "0w6qc7xzyjymhh8hv72rlszh3n2xyzzamlfcl1hs9k6xbbww6czm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "th-lift-instances")))
    (inputs (list ghc-vector ghc-th-lift))
    (native-inputs (list ghc-quickcheck))
    (home-page "http://github.com/bennofs/th-lift-instances/")
    (synopsis "Lift instances for template-haskell for common data types")
    (description
     "Most data types in the Haskell platform do not have Lift
instances.  This package provides orphan instances for @code{containers},
@code{text}, @code{bytestring} and @code{vector}.")
    (license license:bsd-3)))

(define-public ghc-th-orphans
  (package
    (name "ghc-th-orphans")
    (version "0.13.16")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-orphans" version))
       (sha256
        (base32 "04x95fwsiczbi4gxadnnz6z39hy72hsj1smfaa52ljhwh8sh3479"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "th-orphans")))
    (inputs (list ghc-th-compat ghc-th-lift ghc-th-reify-many))
    (native-inputs (list ghc-hspec))
    (home-page "http://hackage.haskell.org/package/th-orphans")
    (synopsis "Orphan instances for TH datatypes")
    (description
     "This package provides orphan instances for Template Haskell datatypes.  In particular,
instances for @code{Ord} and @code{Lift}, as well as a few missing @code{Show}
and @code{Eq} instances.  These instances used to live in the haskell-src-meta
package, and that's where the version number started.")
    (license license:bsd-3)))

(define-public ghc-these
  (package
    (name "ghc-these")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "these" version))
       (sha256
        (base32 "1pp44amwvpl0m762zahg632prlxx0ca2r10n3a0bznjy6qrxkmhp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "these")))
    (inputs (list ghc-assoc ghc-hashable))
    (arguments
     `(#:cabal-revision ("2"
                         "0dm1gq5phif8v0pfcycxhsrb53hnn63w56jmxlgyl5bcx7npmi9m")))
    (home-page "https://github.com/haskellari/these")
    (synopsis "Either-or-both data type")
    (description
     "This package provides a data type @code{These a b} which can
hold a value of either type or values of each type.  This is usually
thought of as an \"inclusive or\" type (contrasting @code{Either a b} as
\"exclusive or\") or as an \"outer join\" type (contrasting @code{(a, b)}
as \"inner join\").

@code{data These a b = This a | That b | These a b}

Since version 1, this package was split into parts:

@itemize
@item
https://hackage.haskell.org/package/semialign For @code{Align} and
@code{Zip} type-classes.
@item
https://hackage.haskell.org/package/semialign-indexed For
@code{SemialignWithIndex} class, providing @code{ialignWith} and
@code{izipWith}
@item
https://hackage.haskell.org/package/these-lens For lens combinators.
@item
http://hackage.haskell.org/package/monad-chronicle For transformers
variant of @code{These}.
@end itemize")
    (license license:bsd-3)))

(define-public ghc-threads
  (package
    (name "ghc-threads")
    (version "0.5.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "threads" version))
       (sha256
        (base32 "17jaq8gn5p3pgvwvswam379vdmdcq2241n3kwy6mmrisa45db564"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "threads")))
    (arguments
     (list
      #:tests? #f)) ;depend on ghc-concurrent-extra, which stopped building
    (home-page "https://github.com/basvandijk/threads")
    (synopsis "Fork threads and wait for their result")
    (description
     "This package provides functions to fork threads and
wait for their result, whether it's an exception or a normal value.
Besides waiting for the termination of a single thread this package also
provides functions to wait for a group of threads to terminate.  This
package is similar to the @code{threadmanager}, @code{async} and
@code{spawn} packages.  The advantages of this package are:

@itemize
@item Simpler API.
@item More efficient in both space and time.
@item No space-leak when forking a large number of threads.
@item Correct handling of asynchronous exceptions.
@item GHC specific functionality like @code{forkOn} and
@code{forkIOWithUnmask}.
@end itemize")
    (license license:bsd-3)))

(define-public ghc-th-reify-many
  (package
    (name "ghc-th-reify-many")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-reify-many" version))
       (sha256
        (base32 "19g4gc1q3zxbylmvrgk3dqjzychq2k02i7fwvs3vhbrg4ihhw9cx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "th-reify-many")))
    (inputs (list ghc-safe ghc-th-expand-syns))
    (home-page "http://github.com/mgsloan/th-reify-many")
    (synopsis "Recurseively reify template haskell datatype info")
    (description
     "th-reify-many provides functions for recursively reifying top level
declarations.  The main intended use case is for enumerating the names of
datatypes reachable from an initial datatype, and passing these names to some
function which generates instances.")
    (license license:bsd-3)))

(define-public ghc-time-compat
  (package
    (name "ghc-time-compat")
    (version "1.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "time-compat" version))
       (sha256
        (base32 "1w2wxz9q3w04mwjm43xk8nrs61fhaf82xpz5q13in4sv99lzhbjh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "time-compat")))
    (inputs (list ghc-base-orphans ghc-hashable))
    (native-inputs (list ghc-hunit
                         ghc-quickcheck
                         ghc-random
                         ghc-tagged
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/haskellari/time-compat")
    (synopsis "Compatibility package for time")
    (description "This package tries to compat as many @code{time}
features as possible.")
    (license license:bsd-3)))

(define-public ghc-time-hourglass
  (package
    (name "ghc-time-hourglass")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "time-hourglass" version))
       (sha256
        (base32 "08lx5dhs529wqxqh454svyx1l814hwzcn1l669h320x5vms7qz4w"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "time-hourglass")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck))
    (home-page "https://github.com/mpilgrem/time-hourglass")
    (synopsis "A simple and efficient time library")
    (description
     "This package provides a simple and efficient time library.  A key part of the
library is the `Timeable` and `Time` type classes.  Types representing time
values that are instances of the classes allow easy conversion between values of
one time type and another.")
    (license license:bsd-3)))

(define-public ghc-time-locale-compat
  (package
    (name "ghc-time-locale-compat")
    (version "0.1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "time-locale-compat" version))
       (sha256
        (base32 "0b2hmj8wwrfkndwzgm11qr496ca2ahwdxcj3m0ii91bxvrk1bzq7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "time-locale-compat")))
    (inputs (list ghc-old-locale))
    (home-page "https://github.com/khibino/haskell-time-locale-compat")
    (synopsis "Compatibility of TimeLocale between old-locale and time-1.5")
    (description "This package contains a wrapped name module for
@code{TimeLocale}.")
    (license license:bsd-3)))

(define-public ghc-time-manager
  (package
    (name "ghc-time-manager")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "time-manager" version))
       (sha256
        (base32 "1s387nka1nxii026ly4awrz74acs4ci141mh3mvsz4j47cyw7dzf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "time-manager")))
    (inputs (list ghc-auto-update))
    (home-page "http://github.com/yesodweb/wai")
    (synopsis "Scalable timer")
    (description "This library contains scalable timer functions provided by a
timer manager.")
    (license license:expat)))

(define-public ghc-timeit
  (package
    (name "ghc-timeit")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "timeit" version))
       (sha256
        (base32 "1sliqpvl501rlcj6s0lhmsf5ym24j4h881wzc1f1wdyvg3jz8kd1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "timeit")))
    (arguments
     `(#:cabal-revision ("2"
                         "1vgxfk2021jh6jk3dimchmf9f71844zj080342qvnn5lck7c7mrm")))
    (home-page "https://github.com/merijn/timeit")
    (synopsis "Time monadic computations with an IO base")
    (description "This package provides a simple wrapper to show the
used CPU time of monadic computation with an IO base.")
    (license license:bsd-3)))

(define-public ghc-timezone-series
  (package
    (name "ghc-timezone-series")
    (version "0.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "timezone-series" version))
       (sha256
        (base32 "18n6w7jxwlysq5mvb1sp1z57nyrsgn2ans642fy5rhmpwcavgvr8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "timezone-series")))
    (arguments
     `(#:cabal-revision ("1"
                         "1ak05p8z1q2nispv1xw32j7lhfmf3sfj2ibjrxpm347s37fmxnwc")))
    (home-page "http://projects.haskell.org/time-ng/")
    (synopsis "Enhanced timezone handling for Time")
    (description
     "This package endows @code{Data.Time}, from the time package, with several
data types and functions for enhanced processing of timezones.  For one way to
create timezone series, see the ghc-timezone-olson package.")
    (license license:bsd-3)))

(define-public ghc-timezone-olson
  (package
    (name "ghc-timezone-olson")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "timezone-olson" version))
       (sha256
        (base32 "10f5843sza2ikj2sg9fjhf5dhnhcidad86cdjmrj1y6zclkiqmdc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "timezone-olson")))
    (inputs (list ghc-timezone-series ghc-extensible-exceptions))
    (home-page "http://projects.haskell.org/time-ng/")
    (synopsis "Parser and renderer for binary Olson timezone files")
    (description
     "A parser and renderer for binary Olson timezone files whose format
is specified by the tzfile(5) man page on Unix-like systems.  For more
information about this format, see
@url{http://www.iana.org/time-zones/repository/tz-link.html}.  Functions
are provided for converting the parsed data into @code{TimeZoneSeries}
objects from the timezone-series package.")
    (license license:bsd-3)))

(define-public ghc-tldr
  (package
    (name "ghc-tldr")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tldr" version))
       (sha256
        (base32 "1yypb9zhsj9ks7bbw2sayqv3rn9y8z3w5p1xmsnwb4w99dqmvcx5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tldr")))
    (inputs (list ghc-ansi-terminal
                  ghc-attoparsec
                  ghc-cmark
                  ghc-http-conduit
                  ghc-optparse-applicative
                  ghc-semigroups
                  ghc-zip-archive))
    (native-inputs (list ghc-tasty ghc-tasty-golden))
    (home-page "https://github.com/psibi/tldr-hs#readme")
    (synopsis "Haskell tldr client")
    (description
     "This package provides the @command{tldr} command and a
Haskell client library allowing users to update and view @code{tldr} pages
from a shell.  The @code{tldr} pages are a community effort to simplify the
man pages with practical examples.")
    (license license:bsd-3)))

(define-public ghc-torrent
  (package
    (name "ghc-torrent")
    (version "10000.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "torrent" version))
       (sha256
        (base32 "1pp9qfpai7v8vlylw4zfgmnbznwjldqlbl3p6awlhzkpszvqzgny"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "torrent")))
    (inputs (list ghc-bencode ghc-syb))
    (home-page "http://hackage.haskell.org/package/torrent")
    (synopsis "BitTorrent file parser and generator")
    (description "This library provides support for parsing and generating
BitTorrent files.")
    (license license:bsd-3)))

(define-public ghc-transformers
  (package
    (name "ghc-transformers")
    (version "0.6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "transformers" version))
       (sha256
        (base32 "09fpjawkixgm3xpas89wkpkn1jfpxz035crnp97if2hh1y759ll1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "transformers")))
    (home-page "http://hackage.haskell.org/package/transformers")
    (synopsis "Concrete functor and monad transformers")
    (description
     "Transformers provides functor and monad transformers, inspired by the
paper \"Functional Programming with Overloading and Higher-Order
Polymorphism\", by Mark P Jones, in Advanced School of Functional Programming,
1995 @url{http://web.cecs.pdx.edu/~mpj/pubs/springschool.html}.

This package contains:
@itemize
@item the monad transformer class (in @code{Control.Monad.Trans.Class})
@item concrete functor and monad transformers, each with associated operations
and functions to lift operations associated with other transformers.
@end itemize

This package can be used on its own in portable Haskell code, in which case
operations need to be manually lifted through transformer stacks (see
@code{Control.Monad.Trans.Class} for some examples).  Alternatively, it can be
used with the non-portable monad classes in the mtl or monads-tf packages,
which automatically lift operations introduced by monad transformers through
other transformers.")
    (license license:bsd-3)))

(define-public ghc-transformers-base
  (package
    (name "ghc-transformers-base")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "transformers-base" version))
       (sha256
        (base32 "146g69yxmlrmvqnzwcw4frxfl3z04lda9zqwcqib34dnkrlghfrj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "transformers-base")))
    (inputs (list ghc-transformers-compat ghc-base-orphans))
    (home-page "https://github.com/mvv/transformers-base")
    (synopsis "Backported transformer library")
    (description
     "Backported versions of types that were added to transformers in
transformers 0.3 and 0.4 for users who need strict transformers 0.2 or 0.3
compatibility to run on old versions of the platform.")
    (license license:bsd-3)))

(define-public ghc-transformers-compat
  (package
    (name "ghc-transformers-compat")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "transformers-compat" version))
       (sha256
        (base32 "0slxrkxi8xa1bmi9saq9x8bz52clrf2slf877m3ckjzkr4276b5n"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "transformers-compat")))
    (inputs (list ghc-generic-deriving))
    (home-page "http://github.com/ekmett/transformers-compat/")
    (synopsis "Small compatibility shim between transformers 0.3 and 0.4")
    (description
     "This package includes backported versions of types that were
added to transformers in transformers 0.3 and 0.4 for users who need strict
transformers 0.2 or 0.3 compatibility to run on old versions of the platform,
but also need those types.")
    (license license:bsd-3)))

(define-public ghc-exception-transformers
  (package
    (name "ghc-exception-transformers")
    (version "0.4.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "exception-transformers" version))
       (sha256
        (base32 "11qlr78hlp1ivsx2l06sac56wj900l6diwh6dv7jfzgary8dax4k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "exception-transformers")))
    (inputs (list ghc-fail ghc-transformers-compat))
    (native-inputs (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (home-page "http://hackage.haskell.org/package/exception-transformers")
    (synopsis "Type classes and monads for unchecked extensible exceptions")
    (description
     "This package provides type classes, a monad and a monad transformer that support
unchecked extensible exceptions as well as asynchronous exceptions.  It is
compatible with the transformers package.")
    (license license:bsd-3)))

(define-public ghc-tree-diff
  (package
    (name "ghc-tree-diff")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tree-diff" version))
       (sha256
        (base32 "0fqfyrab0bf98z251lsfvl2jdcaja6ikfn9q537jbxkx402fi6jy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tree-diff")))
    (inputs (list ghc-aeson
                  ghc-ansi-terminal
                  ghc-ansi-wl-pprint
                  ghc-hashable
                  ghc-parsers
                  ghc-primitive
                  ghc-quickcheck
                  ghc-scientific
                  ghc-semialign
                  ghc-strict
                  ghc-tagged
                  ghc-these
                  ghc-unordered-containers
                  ghc-uuid-types
                  ghc-vector))
    (native-inputs (list ghc-tasty ghc-tasty-golden ghc-tasty-quickcheck
                         ghc-trifecta))
    (arguments
     `(#:cabal-revision ("2"
                         "1hb62nd833n7gmg508qkbndbj13p8dscwfanilwifbxqwwcn98ah")))
    (home-page "https://github.com/phadej/tree-diff")
    (synopsis "Compute difference between (expression) trees")
    (description
     "This Haskell library provides a function for computing
the difference between (expression) trees.  It also provides a way to
compute the difference between arbitrary abstract datatypes (ADTs) using
@code{Generics}-derivable helpers.")
    (license license:bsd-3)))

(define-public ghc-trifecta
  (package
    (name "ghc-trifecta")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "trifecta" version))
       (sha256
        (base32 "0a4wvj3g27xmkck150ci6wfp8lqbdlrqgi9q8y3k0cqsvfpl5n6l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "trifecta")))
    (inputs (list ghc-ansi-terminal
                  ghc-blaze-builder
                  ghc-blaze-html
                  ghc-blaze-markup
                  ghc-charset
                  ghc-comonad
                  ghc-fingertree
                  ghc-hashable
                  ghc-indexed-traversable
                  ghc-lens
                  ghc-parsers
                  ghc-prettyprinter
                  ghc-prettyprinter-ansi-terminal
                  ghc-profunctors
                  ghc-reducers
                  ghc-unordered-containers
                  ghc-utf8-string))
    (native-inputs (list ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "1r61x5lzrsax5n3a38hjk78k7p4xspz6zdw5gsyyyqwl57cd2b6g")))
    (home-page "http://github.com/ekmett/trifecta/")
    (synopsis "Parser combinator library with convenient diagnostics")
    (description "Trifecta is a modern parser combinator library for Haskell,
with slicing and Clang-style colored diagnostics.")
    (license license:bsd-3)))

(define-public ghc-tuple-th
  (package
    (name "ghc-tuple-th")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tuple-th" version))
       (sha256
        (base32 "1mrl4vvxmby7sf1paf7hklzidnr6wq55822i73smqyz0xpf3gsjn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tuple-th")))
    (home-page "http://hackage.haskell.org/package/tuple-th")
    (synopsis "Generate utility functions for tuples of statically known size
for Haskell")
    (description
     "This Haskell package contains Template Haskell functions for
generating functions similar to those in @code{Data.List} for tuples of
statically known size.")
    (license license:bsd-3)))

(define-public ghc-turtle
  (package
    (name "ghc-turtle")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "turtle" version))
       (sha256
        (base32 "0bav7b2ghbwdhpa4b5a7812psr6hqk7wydvgxjy5dsb62nqnc08s"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "turtle")))
    (inputs (list ghc-async
                  ghc-clock
                  ghc-foldl
                  ghc-hostname
                  ghc-managed
                  ghc-streaming-commons
                  ghc-temporary
                  ghc-optional-args
                  ghc-unix-compat
                  ghc-ansi-wl-pprint
                  ghc-optparse-applicative))
    (native-inputs (list ghc-doctest ghc-tasty ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("3"
                         "19i3n3hd2a0rkdz1ikwdgwhg4ds5pcfah25vgk0jnmwf71h0qwbm")))
    (home-page "http://hackage.haskell.org/package/turtle")
    (synopsis "Shell programming, Haskell-style")
    (description
     "Turtle is a reimplementation of the Unix command line environment in
Haskell so that you can use Haskell as both a shell and a scripting
language.  Features include:

@itemize
@item Batteries included: Command an extended suite of predefined utilities.
@item Interoperability: You can still run external shell commands.
@item Portability: Works on Windows, OS X, and Linux.
@item Exception safety: Safely acquire and release resources.
@item Streaming: Transform or fold command output in constant space.
@item Patterns: Use typed regular expressions that can parse structured values.
@item Formatting: Type-safe printf-style text formatting.
@item Modern: Supports text and system-filepath.
@end itemize

Read \"Turtle.Tutorial\" for a detailed tutorial or \"Turtle.Prelude\" for a
quick-start guide.  Turtle is designed to be beginner-friendly, but as a
result lacks certain features, like tracing commands.  If you feel comfortable
using turtle then you should also check out the Shelly library which provides
similar functionality.")
    (license license:bsd-3)))

(define-public ghc-typed-process
  (package
    (name "ghc-typed-process")
    (version "0.2.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "typed-process" version))
       (sha256
        (base32 "1823mbibn4wgx5y7d1xywayjiz3hlv1mg3dcahpdyhdm2dh9c04l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "typed-process")))
    (inputs (list ghc-async ghc-unliftio-core))
    (native-inputs (list ghc-base64-bytestring
                         ghc-hspec
                         hspec-discover
                         ghc-temporary
                         ghc-base64-bytestring
                         ghc-hspec
                         ghc-temporary))
    (home-page "https://github.com/fpco/typed-process")
    (synopsis "Run external processes with strong typing of streams")
    (description
     "This library provides the ability to launch and interact with external
processes.  It wraps around the @code{process} library, and intends to improve
upon it.")
    (license license:expat)))

(define-public ghc-uglymemo
  (package
    (name "ghc-uglymemo")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "uglymemo" version))
       (sha256
        (base32 "0ixqg5d0ly1r18jbgaa89i6kjzgi6c5hanw1b1y8c5fbq14yz2gy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "uglymemo")))
    (home-page "http://hackage.haskell.org/package/uglymemo")
    (synopsis "Simple memoization function for Haskell")
    (description
     "This package provides a simple (but internally ugly) memoization
function.")
    (license license:public-domain)))

(define-public ghc-unagi-chan
  (package
    (name "ghc-unagi-chan")
    (version "0.4.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unagi-chan" version))
       (sha256
        (base32 "1d98a6s7rydjlf2p3jv6j7wglq8ahf8kgcibji5fiy6y0ymz9mnr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unagi-chan")))
    (inputs (list ghc-atomic-primops ghc-primitive))
    (home-page "http://hackage.haskell.org/package/unagi-chan")
    (synopsis "Fast concurrent queues with a Chan-like API, and more")
    (description
     "This library provides implementations of concurrent FIFO queues (for
both general boxed and primitive unboxed values) that are fast, perform well
under contention, and offer a Chan-like interface.  The library may be of
limited usefulness outside of x86 architectures where the fetch-and-add
instruction is not available.")
    (license license:bsd-3)))

(define-public ghc-unbounded-delays
  (package
    (name "ghc-unbounded-delays")
    (version "0.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unbounded-delays" version))
       (sha256
        (base32 "11b1vmlfv4pmmpl4kva58w7cf50xsj819cq3wzqgnbz3px9pxbar"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unbounded-delays")))
    (home-page "https://github.com/basvandijk/unbounded-delays")
    (synopsis "Unbounded thread delays and timeouts")
    (description
     "The @code{threadDelay} and @code{timeout} functions from the
Haskell base library use the bounded @code{Int} type for specifying the delay
or timeout period.  This package provides alternative functions which use the
unbounded @code{Integer} type.")
    (license license:bsd-3)))

(define-public ghc-unexceptionalio
  (package
    (name "ghc-unexceptionalio")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unexceptionalio" version))
       (sha256
        (base32 "07py2nffdgxpz8sryvqcghzb2kiiagpdf5ja1dia4z0rpwi79smh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unexceptionalio")))
    (native-inputs (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (home-page "https://github.com/singpolyma/unexceptionalio")
    (synopsis "IO without any non-error, synchronous exceptions")
    (description "When you've caught all the exceptions that can be
handled safely, this is what you're left with.")
    (license license:isc)))

(define-public ghc-unicode-data
  (package
    (name "ghc-unicode-data")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unicode-data" version))
       (sha256
        (base32 "0iwr096kwvjmx32a2drzz5hlam5sy0ca2m15ih33r8imhlb371xj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unicode-data")))
    (native-inputs (list ghc-hspec))
    (arguments
     `(#:cabal-revision ("2"
                         "1y592fjr7i0416nzxr8g91apm8ff9gls5lwr16bc7zsh9hdyahja")))
    (home-page "http://github.com/composewell/unicode-data")
    (synopsis "Access Unicode Character Database (UCD)")
    (description
     "This package provides Haskell APIs to efficiently access the
<https://www.unicode.org/ucd/ Unicode character database> (UCD).  Performance is
the primary goal in the design of this package.  The Haskell data structures
are generated programmatically from the UCD files.")
    (license license:asl2.0)))

(define-public ghc-unicode-show
  (package
    (name "ghc-unicode-show")
    (version "0.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unicode-show" version))
       (sha256
        (base32 "1sizp1wmnx1vgckwyf5nawqf9s7ibrwacgznnc1m4j5q1hydbbzl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unicode-show")))
    (inputs (list ghc-safe))
    (native-inputs (list ghc-hspec ghc-quickcheck))
    (home-page "https://github.com/haskell-jp/unicode-show")
    (synopsis "Unescape Unicode characters for printing")
    (description
     "This package provides variants of the @code{show} and @code{print}
functions that do not escape non-ascii characters.")
    (license license:bsd-3)))

(define-public ghc-unicode-transforms
  (package
    (name "ghc-unicode-transforms")
    (version "0.4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unicode-transforms" version))
       (sha256
        (base32 "1z29jvli2rqkynfxni1gibl81458j7h8lrb8fg6lpnj8svhy2y1j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unicode-transforms")))
    (inputs (list ghc-unicode-data))
    (native-inputs (list ghc-quickcheck ghc-quickcheck ghc-hspec ghc-split))
    (arguments
     `(#:cabal-revision ("7"
                         "1lsb6788m8ibpddfx0ah3v5c09q9i0d1ik92bpgsx5ygx1xcnsj4")))
    (home-page "http://github.com/composewell/unicode-transforms")
    (synopsis "Unicode normalization")
    (description
     "This library provides tools for fast Unicode 12.1.0
normalization in Haskell (normalization forms C, KC, D, and KD).")
    (license license:bsd-3)))

(define-public ghc-uniplate
  (package
    (name "ghc-uniplate")
    (version "1.6.13")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "uniplate" version))
       (sha256
        (base32 "1lis5qcb5j7yzd1cqjaqpb6bmkyjfb7l4nhk3ykmcma4513cjxz7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "uniplate")))
    (inputs (list ghc-syb ghc-hashable ghc-unordered-containers))
    (arguments
     `(#:cabal-revision ("1"
                         "1rvvzmi43gbrww0f17dzchm3g61zvm97arrfa5raljqb1mbibdy8")))
    (home-page "https://github.com/ndmitchell/uniplate#readme")
    (synopsis "Simple, concise and fast generic operations")
    (description
     "Uniplate is a library for writing simple and concise generic
operations.  Uniplate has similar goals to the original Scrap Your Boilerplate
work, but is substantially simpler and faster.")
    (license license:bsd-3)))

(define-public ghc-unique
  (package
    (name "ghc-unique")
    (version "0.4.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Unique" version))
       (sha256
        (base32 "1wv9dskvs2im5a97h3xm161sbqcbw81d7knk81y876ghqqf3hr56"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Unique")))
    (inputs (list ghc-extra ghc-hashable ghc-unordered-containers))
    (native-inputs (list ghc-hspec ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "0cxifpgiq110618hzas1yz3g7ngcliw1skb9nahw3arsz3c3zifj")))
    (home-page "https://hackage.haskell.org/package/Unique")
    (synopsis "Haskell functionality like \"uniq\" tool")
    (description
     "This library provides the functions to find unique and duplicate
elements in a list.")
    (license license:bsd-3)))

(define-public ghc-unix-compat
  (package
    (name "ghc-unix-compat")
    (version "0.7.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unix-compat" version))
       (sha256
        (base32 "0m20wi8z3bzdc9b61rilb9hrbrd0wwzyc06rfbk7a30h99i135iz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unix-compat")))
    (native-inputs (list ghc-monad-parallel ghc-hspec ghc-hunit ghc-temporary))
    (home-page "https://github.com/haskell-pkg-janitors/unix-compat")
    (synopsis "Portable POSIX-compatibility layer")
    (description
     "This package provides portable implementations of parts of the unix package.
This package re-exports the unix package when available.  When it isn't
available, portable implementations are used.")
    (license license:bsd-3)))

(define-public ghc-unix-time
  (package
    (name "ghc-unix-time")
    (version "0.4.17")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unix-time" version))
       (sha256
        (base32 "130z416958xqd6yvjidmm66674y9vkwgxj965kvwhnncbnz0afpn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unix-time")))
    (inputs (list ghc-old-time))
    (native-inputs (list ghc-old-locale ghc-quickcheck ghc-hspec hspec-discover))
    (home-page "http://hackage.haskell.org/package/unix-time")
    (synopsis "Unix time parser/formatter and utilities")
    (description "This library provides fast parsing and formatting utilities
for Unix time in Haskell.")
    (license license:bsd-3)))

(define-public ghc-unliftio
  (package
    (name "ghc-unliftio")
    (version "0.2.25.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unliftio" version))
       (sha256
        (base32 "0vikd29av2kk8sf09f2q3x9pgg8v90znsgpqdp0c4rk9xa6nzz1v"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unliftio")))
    (inputs (list ghc-async ghc-safe-exceptions ghc-unliftio-core ghc-nats))
    (native-inputs (list ghc-quickcheck ghc-hspec hspec-discover))
    (home-page "https://github.com/fpco/unliftio/tree/master/unliftio#readme")
    (synopsis "Provides MonadUnliftIO typecplass for unlifting monads to
IO")
    (description
     "This Haskell package provides the core @code{MonadUnliftIO}
typeclass, a number of common instances, and a collection of common functions
working with it.")
    (license license:expat)))

(define-public ghc-unliftio-core
  (package
    (name "ghc-unliftio-core")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unliftio-core" version))
       (sha256
        (base32 "1qz3gxcq1x8fjgq6fqsnws5vgkgbjcx332p3hldxdnaninx4qf4r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unliftio-core")))
    (arguments
     `(#:cabal-revision ("4"
                         "0ah7x2k1p5d43iwr2xr12z5fk5jdxb9l7jjd73cr0lwbhmpp78pn")))
    (home-page
     "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme")
    (synopsis "The MonadUnliftIO typeclass for unlifting monads to IO")
    (description
     "This Haskell package provides the core @code{MonadUnliftIO}
typeclass, instances for base and transformers, and basic utility
functions.")
    (license license:expat)))

(define-public ghc-unordered-containers
  (package
    (name "ghc-unordered-containers")
    (version "0.2.20")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unordered-containers" version))
       (sha256
        (base32 "07gij1y9zhqg2dq8wy815j7s0zk2k65sqg4wvhwjsn80ry3v5kyr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unordered-containers")))
    (inputs (list ghc-hashable))
    (native-inputs (list ghc-chasingbottoms
                         ghc-hunit
                         ghc-quickcheck
                         ghc-random
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-nothunks-bootstrap))
    (arguments
     `(#:cabal-revision ("4"
                         "08p4xp43mlqyl8ayh7k22p0q0kxlby9y071rpcr8jsf2lvfvqg13")))
    (home-page
     "https://github.com/haskell-unordered-containers/unordered-containers")
    (synopsis "Efficient hashing-based container types")
    (description
     "Efficient hashing-based container types.  The containers have been
optimized for performance critical use, both in terms of large data quantities
and high speed.")
    (license license:bsd-3)))

(define-public ghc-unordered-containers-bootstrap
  (package
    (inherit ghc-unordered-containers)
    (name "ghc-unordered-containers-bootstrap")
    (arguments `(#:tests? #f
                 ,@(package-arguments ghc-unordered-containers)))
    (inputs
     `(("ghc-hashable" ,ghc-hashable-bootstrap)))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-commutative-semigroups
  (package
    (name "ghc-commutative-semigroups")
    (version "0.2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "commutative-semigroups" version))
       (sha256
        (base32 "06s7mw3j2g6yinabw3w1ncpph6q4n36almbn4xv7lqi5k7bb297q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "commutative-semigroups")))
    (home-page "http://hackage.haskell.org/package/commutative-semigroups")
    (synopsis "Commutative semigroups")
    (description
     "This package provides a commutative semigroup is a semigroup where the order of
arguments to mappend does not matter.")
    (license license:bsd-3)))

(define-public ghc-dependent-sum
  (package
    (name "ghc-dependent-sum")
    (version "0.7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "dependent-sum" version))
       (sha256
        (base32 "1frw5965v8i6xqdgs95gg8asgdqcqnmfahz0pmbwiaw5ybn62rc2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dependent-sum")))
    (inputs (list ghc-constraints-extras ghc-some))
    (arguments
     `(#:cabal-revision ("2"
                         "09648zwf1wg42yk5ykbv1wvgz2bibjrwvcx6wpm4jscv8d2h61pi")))
    (home-page "https://github.com/obsidiansystems/dependent-sum")
    (synopsis "Dependent sum type")
    (description
     "This package provides a dependent sum is a generalization of a
particular way of thinking about the @code{Either} type.  @code{Either a b}
can be thought of as a 2-tuple @code{(tag, value)}, where the value of the tag
determines the type of the value.  In particular, either @code{tag = Left} and
@code{value :: a} or @code{tag = Right} and @code{value :: b}.  This package
allows you to define your own dependent sum types by using your own \"tag\"
types.")
    (license license:public-domain)))

(define-public ghc-dependent-map
  (package
    (name "ghc-dependent-map")
    (version "0.4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "dependent-map" version))
       (sha256
        (base32 "0b0zhyl3wkl4kkrxvq7vwjz3gn0ndxjjgyw9cky8a6xyv190pkjk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dependent-map")))
    (inputs (list ghc-dependent-sum ghc-constraints-extras))
    (arguments
     `(#:cabal-revision ("2"
                         "18jqk1p4paaylqdvglw03v7fhyvlg59csl4kpf067wwpdpyaqs3l")))
    (home-page "https://github.com/obsidiansystems/dependent-map")
    (synopsis "Dependent finite maps (partial dependent products)")
    (description
     "This package provides a type called @@DMap@@ which generalizes @@Data.Map.Map@@,
allowing keys to specify the type of value that can be associated with them.")

    ;; XXX: The 'LICENSE' file lists several licenses, stating "I have no idea
    ;; which, if any, of the following licenses apply […].  Any modifications
    ;; by myself I release into the public domain […]"."
    (license license:public-domain)))

(define-public ghc-unsafe
  (package
    (name "ghc-unsafe")
    (version "0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unsafe" version))
       (sha256
        (base32 "0hc6xr1i3hkz25gdgfx1jqgpsc9mwa05bkfynp0mcfdlyz6782nz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unsafe")))
    (home-page "http://code.haskell.org/~thielema/unsafe/")
    (synopsis "Unified interface to unsafe functions")
    (description
     "Safe Haskell introduced the notion of safe and unsafe
modules.  In order to make as many as possible modules ``safe'', the
well-known unsafe functions were moved to distinguished modules.  This
makes it hard to write packages that work with both old and new versions
of GHC.  This package provides a single module System.Unsafe that
exports the unsafe functions from the base package.  It provides them in
a style ready for qualification, that is, you should import them by
@code{import qualified System.Unsafe as Unsafe}.")
    (license license:bsd-3)))

(define-public ghc-uri-bytestring
  (package
    (name "ghc-uri-bytestring")
    (version "0.4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "uri-bytestring" version))
       (sha256
        (base32 "0xbrm2q7smj0ar7bz7c1nw510si8bfnb9kd4pvdrxbcpyj9zsg12"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "uri-bytestring")))
    (inputs (list ghc-attoparsec ghc-blaze-builder ghc-th-lift-instances))
    (native-inputs (list ghc-hunit
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-hedgehog
                         ghc-tasty-hedgehog
                         ghc-base-compat
                         ghc-safe))
    (home-page "https://github.com/Soostone/uri-bytestring")
    (synopsis "Haskell URI parsing as ByteStrings")
    (description
     "This Haskell package aims to be an RFC3986 compliant URI
parser that uses ByteStrings for parsing and representing the URI data.")
    (license license:bsd-3)))

(define-public ghc-utf8-light
  (package
    (name "ghc-utf8-light")
    (version "0.4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "utf8-light" version))
       (sha256
        (base32 "0415hapndlsnzvmm3bk2fl42h4vn1izky7jb3lbby3mzzzd8d1fx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "utf8-light")))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'configure 'relax-depency-versions
             (lambda _
               (substitute* "utf8-light.cabal"
                 (("hspec [<>=0-9. &|]*") "hspec")))))))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "http://hackage.haskell.org/package/utf8-light")
    (synopsis "Lightweight unicode support for Haskell")
    (description
     "This package profides a class for encoding and decoding UTF8 strings
with instances for several common types.  It also includes several functions
for working with UTF8.  It aims to be lightweight, depending only on Base and
including only one module.")
    (license license:bsd-3)))

(define-public ghc-utf8-string
  (package
    (name "ghc-utf8-string")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "utf8-string" version))
       (sha256
        (base32 "16mh36ffva9rh6k37bi1046pgpj14h0cnmj1iir700v0lynxwj7f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "utf8-string")))
    (native-inputs (list ghc-hunit))
    (home-page "https://github.com/glguy/utf8-string/")
    (synopsis "Support for reading and writing UTF8 Strings")
    (description
     "A UTF8 layer for Strings.  The utf8-string package provides operations
for encoding UTF8 strings to Word8 lists and back, and for reading and writing
UTF8 without truncation.")
    (license license:bsd-3)))

(define-public ghc-utility-ht
  (package
    (name "ghc-utility-ht")
    (version "0.0.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "utility-ht" version))
       (sha256
        (base32 "0b1pn25pdhsaigi2qbih13fagcaskbrsr2pmhk5gh63djn0w13ha"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "utility-ht")))
    (native-inputs (list ghc-quickcheck ghc-doctest-exitcode-stdio
                         ghc-doctest-lib))
    (home-page "http://hackage.haskell.org/package/utility-ht")
    (synopsis "Haskell helper functions for Lists, Maybes, Tuples, Functions")
    (description "This package includes Hakell modules providing various
helper functions for Lists, Maybes, Tuples, Functions.")
    (license license:bsd-3)))

(define-public ghc-uuid
  (package
    (name "ghc-uuid")
    (version "1.3.16")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "uuid" version))
       (sha256
        (base32 "1xhdf8zw2n2h4c0f8xf76c2gsjvldz34zgc4sn6wqpzcnk7pjihs"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "uuid")))
    (inputs (list ghc-cryptohash-md5
                  ghc-cryptohash-sha1
                  ghc-entropy
                  ghc-network-info
                  ghc-random
                  ghc-uuid-types))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("3"
                         "153pkkljg08pdv6hjdzzq67dzwy0k4c4k68ha110ak4d96iibc6r")))
    (home-page "https://github.com/haskell-hvr/uuid")
    (synopsis "Haskell library to create, compare, parse, and print UUIDs")
    (description
     "This Haskell library provides utilities creating, comparing,
parsing and printing @dfn{Universally Unique Identifiers} or UUIDs.")
    (license license:bsd-3)))

(define-public ghc-uuid-types
  (package
    (name "ghc-uuid-types")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "uuid-types" version))
       (sha256
        (base32 "0zimp0v7hx073rcb4y9l463jfg4y3yqxdbmw975d6vrx919xj3by"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "uuid-types")))
    (inputs (list ghc-hashable ghc-random))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("3"
                         "183dz7p96r7da14064xkvdzxs66gzlxi26xca5m6yrypm20vdhjs")))
    (home-page "https://github.com/haskell-hvr/uuid")
    (synopsis "Haskell type definitions for UUIDs")
    (description
     "This Haskell library contains type definitions for
@dfn{Universally Unique Identifiers} or
@uref{https://en.wikipedia.org/wiki/UUID, UUIDs}, and basic conversion
functions.")
    (license license:bsd-3)))

(define-public ghc-validation
  (package
    (name "ghc-validation")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "validation" version))
       (sha256
        (base32 "159pvlzs5caabay4irs6dgrxpyhrcakyxqv7fvhs8cnarlafjhbv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "validation")))
    (inputs (list ghc-assoc ghc-semigroups ghc-semigroupoids ghc-bifunctors
                  ghc-lens))
    (native-inputs (list ghc-hedgehog ghc-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "0lmrs8yrb075l91r7iq8yk7hy2scdd3z1335wmackhcfw8z5bp1z")))
    (home-page "https://github.com/qfpl/validation")
    (synopsis "Data-type like Either but with an accumulating Applicative")
    (description
     "A data-type like Either but with differing properties and type-class
instances.

Library support is provided for this different representation, including
@code{lens}-related functions for converting between each and abstracting over
their similarities.

The @code{Validation} data type is isomorphic to @code{Either}, but has an
instance of @code{Applicative} that accumulates on the error side.  That is to
say, if two (or more) errors are encountered, they are appended using a
@code{Semigroup} operation.

As a consequence of this @code{Applicative} instance, there is no
corresponding @code{Bind} or @code{Monad} instance.  @code{Validation} is an
example of, \"An applicative functor that is not a monad.\"")
    (license license:bsd-3)))

(define-public ghc-validity
  (package
    (name "ghc-validity")
    (version "0.12.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "validity" version))
       (sha256
        (base32 "1px6qaabr1k1szx9sl4vjqnwwlyj590s6h21p54ycfjj744md2p2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "validity")))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "https://github.com/NorfairKing/validity#readme")
    (synopsis "Validity typeclass")
    (description
     "Values of custom types usually have invariants imposed upon them.  This
package provides the @code{Validity} type class, which makes these invariants
explicit by providing a function to check whether the invariants hold.")
    (license license:expat)))

(define-public ghc-validity-bytestring
  (package
    (name "ghc-validity-bytestring")
    (version "0.4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "validity-bytestring" version))
       (sha256
        (base32 "0ck7pn8c8srwdwpcx6x4ihixff07kigq8q9sjkc3zzyf54n93f3x"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "validity-bytestring")))
    (inputs (list ghc-validity))
    (home-page "https://github.com/NorfairKing/validity")
    (synopsis "Validity instances for bytestring")
    (description "Provides instances of the @code{Validity} type class for lazy
and strict @code{ByteString}s and @code{ShortByteString}s.")
    (license license:expat)))

(define-public ghc-vault
  (package
    (name "ghc-vault")
    (version "0.3.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vault" version))
       (sha256
        (base32 "181ksk1yixjg0jiggw5jvm8am8m8c7lim4xaixf8qnaqvxm6namc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vault")))
    (inputs (list ghc-unordered-containers ghc-hashable ghc-semigroups))
    (arguments
     `(#:cabal-revision ("10"
                         "03nw9b08pqhk9ck7lqcixsnh3sxz7zz3jvb3xxfmc2kksrfc4b11")))
    (home-page "https://github.com/HeinrichApfelmus/vault")
    (synopsis "Persistent store for arbitrary values")
    (description
     "This package provides vaults for Haskell.  A vault is a
persistent store for values of arbitrary types.  It's like having first-class
access to the storage space behind @code{IORefs}.  The data structure is
analogous to a bank vault, where you can access different bank boxes with
different keys; hence the name.  Also provided is a @code{locker} type,
representing a store for a single element.")
    (license license:bsd-3)))

(define-public ghc-vector
  (package
    (name "ghc-vector")
    (version "0.13.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vector" version))
       (sha256
        (base32 "08y4j6nii17wc3fs3d2za0yifd3gqf73g8zyqdnsry6bhv3h7wi8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vector")))
    (inputs (list ghc-primitive ghc-vector-stream ghc-random ghc-tasty))
    (native-inputs (list ghc-base-orphans
                         ghc-quickcheck
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-base-orphans
                         ghc-quickcheck
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-doctest
                         ghc-tasty-inspection-testing))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (if tests?
                 (let* ((tmpdir (or (getenv "TMP")
                                    "/tmp"))
                        (dependency-package-db
                         (string-append tmpdir "/package.conf.d"))
                        (vector-package-db
                         (string-append (getcwd) "/dist/package.conf.inplace")))
                   (invoke "runhaskell" "Setup.hs" "test" "vector-tests-O0"
                           "vector-tests-O2" "vector-inspection")
                   (setenv "GHC_PACKAGE_PATH"
                           (string-append vector-package-db ":"
                                          dependency-package-db))
                   (invoke "./dist/build/vector-doctest/vector-doctest")
                   (unsetenv "GHC_PACKAGE_PATH"))
                 (format #t "Testsuite not run.%~")))))
       #:cabal-revision '("2"
                          "1d3ma9zldfwlz7s41hmaz6jcxwpyh12cld44n1ys7n2jvb43ihws")))
    (home-page "https://github.com/haskell/vector")
    (synopsis "Efficient Arrays")
    (description
     "This library provides an efficient implementation of
Int-indexed arrays (both mutable and immutable), with a powerful loop
optimisation framework.")
    (license license:bsd-3)))

(define-public ghc-vector-algorithms
  (package
    (name "ghc-vector-algorithms")
    (version "0.9.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vector-algorithms" version))
       (sha256
        (base32 "1clfam0brcpjwpzz14di6d51nx5mnsk7sd9bd250srq2d1kp9dnj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vector-algorithms")))
    (inputs (list ghc-bitvec ghc-vector ghc-primitive))
    (native-inputs (list ghc-quickcheck))
    (home-page "https://github.com/erikd/vector-algorithms/")
    (synopsis "Algorithms for vector arrays in Haskell")
    (description "This Haskell library algorithms for vector arrays.")
    (license license:bsd-3)))

(define-public ghc-vector-binary-instances
  (package
    (name "ghc-vector-binary-instances")
    (version "0.2.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vector-binary-instances" version))
       (sha256
        (base32 "0kgmlb4rf89b18d348cf2k06xfhdpamhmvq7iz5pab5014hknbmp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vector-binary-instances")))
    (inputs (list ghc-vector))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("5"
                         "1svw25aid1vby7288b36d2mbqcvmggfr3ndv8ymj2y2jm72z5a4v")))
    (home-page "https://github.com/haskell/vector-binary-instances")
    (synopsis "Instances of Data.Binary and Data.Serialize for vector")
    (description
     "This library provides instances of @code{Binary} for the
types defined in the @code{vector} package, making it easy to serialize
vectors to and from disk.  We use the generic interface to vectors, so all
vector types are supported.  Specific instances are provided for unboxed,
boxed and storable vectors.")
    (license license:bsd-3)))

(define-public ghc-vector-builder
  (package
    (name "ghc-vector-builder")
    (version "0.3.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vector-builder" version))
       (sha256
        (base32 "1zspnyzahxahirx8gvrw5fnn6xnmsb24cm6vd8wldpp3355jdlbx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vector-builder")))
    (inputs (list ghc-vector))
    (native-inputs (list ghc-attoparsec
                         ghc-quickcheck-instances
                         ghc-rerebase
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/nikita-volkov/vector-builder")
    (synopsis "Vector builder for Haskell")
    (description
     "This Haskell package provides an API for constructing vectors.
It provides the composable @code{Builder} abstraction, which has instances of the
@code{Monoid} and @code{Semigroup} classes.

You would first use the @code{Builder} abstraction to specify the structure of
the vector; then you can execute the builder to actually produce the
vector.")
    (license license:expat)))

(define-public ghc-vector-hashtables
  (package
    (name "ghc-vector-hashtables")
    (version "0.1.2.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "vector-hashtables" version))
              (sha256
               (base32
                "1cdfvrpnia7bgqaw8yg0n23svbsdz72gss0hrkrvc5rwzxwhz49k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vector-hashtables")))
    (inputs (list ghc-primitive ghc-vector ghc-hashable))
    (native-inputs (list ghc-hspec ghc-quickcheck ghc-quickcheck-instances
                         hspec-discover))
    (home-page "https://github.com/klapaucius/vector-hashtables#readme")
    (synopsis "Efficient vector-based mutable hashtables implementation")
    (description
     "This package provides efficient vector-based hashtable implementation
similar to .NET Generic Dictionary implementation (at the time of 2015).")
    (license license:bsd-3)))

(define-public ghc-vector-th-unbox
  (package
    (name "ghc-vector-th-unbox")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vector-th-unbox" version))
       (sha256
        (base32 "0j81m09xxv24zziv0nanfppckzmas5184jr3npjhc9w49r3cm94a"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vector-th-unbox")))
    (inputs (list ghc-vector))
    (native-inputs (list ghc-data-default))
    (arguments
     `(#:cabal-revision ("8"
                         "1dzn47l5hqs541gjfq09r10rrqxw0wsj727kxkv334lw926npljh")))
    (home-page "https://github.com/tsurucapital/vector-th-unbox")
    (synopsis "Deriver for Data.Vector.Unboxed using Template Haskell")
    (description
     "This Haskell library provides a Template Haskell
deriver for unboxed vectors, given a pair of coercion functions to
and from some existing type with an Unbox instance.")
    (license license:bsd-3)))

(define-public ghc-void
  (package
    (name "ghc-void")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "void" version))
       (sha256
        (base32 "05vk3x1r9a2pqnzfji475m5gdih2im1h7rbi2sc67p1pvj6pbbsk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "void")))
    (home-page "http://github.com/ekmett/void")
    (synopsis "Logically uninhabited data type")
    (description
     "A Haskell 98 logically uninhabited data type, used to indicate that a
given term should not exist.")
    (license license:bsd-3)))

(define-public ghc-vty
  (package
    (name "ghc-vty")
    (version "6.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vty" version))
       (sha256
        (base32 "0iha15inmig2j2f4kljwb2jhbqa8vsh2aa8l7y5fsanjiciscrbi"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision ("2"
                         "1dcnkfghflgn3qv72pdpa6npfdrrjqhil3wnxd75rbd4mf7812hf")))
    (properties '((upstream-name . "vty")))
    (inputs (list ghc-blaze-builder ghc-microlens ghc-microlens-mtl
                  ghc-utf8-string ghc-vector))
    (home-page "https://github.com/jtdaugherty/vty")
    (synopsis "Simple terminal UI library")
    (description
     "vty is a terminal GUI library in the niche of ncurses, intended to be easy
to use and to provide good support for common terminal types.")
    (license license:bsd-3)))

(define-public ghc-vty-crossplatform
  (package
    (name "ghc-vty-crossplatform")
    (version "0.4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vty-crossplatform" version))
       (sha256
        (base32 "06iwxgqrqzz05hmic7z5hxd48x0i49sk935vm0xfi0xq28sl7r9m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vty-crossplatform")))
    (inputs (list ghc-vty ghc-vty-unix ghc-random ghc-string-qq))
    (home-page "http://hackage.haskell.org/package/vty-crossplatform")
    (synopsis "Cross-platform support for Vty")
    (description
     "This package provides a generic interface for multiple Vty platforms in
one package so you don't have to conditionally depend on them in your cabal
file.")
    (license license:bsd-3)))

(define-public ghc-vty-unix
  (package
    (name "ghc-vty-unix")
    (version "0.2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vty-unix" version))
       (sha256
        (base32 "1hfxc7qw884vlq8qshhyndl3zs10jc2xr6i69vhasjywkvh6gay2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vty-unix")))
    (inputs (list ghc-blaze-builder
                  ghc-vty
                  ghc-vector
                  ghc-utf8-string
                  ghc-microlens
                  ghc-microlens-mtl
                  ghc-microlens-th
                  ghc-ansi-terminal))
    (home-page "http://hackage.haskell.org/package/vty-unix")
    (synopsis "Unix backend for Vty")
    (description "This package provides Unix terminal support for Vty.")
    (license license:bsd-3)))

(define-public ghc-wave
  (package
    (name "ghc-wave")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wave" version))
       (sha256
        (base32 "1b8qm9jl453z9pklmqcz13f2abl69ab0j31151gz77l4dgxnnqzl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wave")))
    (inputs (list ghc-cereal))
    (native-inputs (list ghc-quickcheck ghc-hspec ghc-hspec-discover ghc-temporary))
    (arguments
     (list
      #:cabal-revision '("1"
                         "1j6ycd1v6c5khkmybzss2vbfm93n28dh1ah8sipqqpd94yqwvdiz")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'relax-dependencies
            (lambda _
              (substitute* "wave.cabal"
                (("bytestring [<=>.0-9 &|]*") "bytestring < 0.13")))))))
    (home-page "https://github.com/mrkkrp/wave")
    (synopsis "Work with WAVE and RF64 files in Haskell")
    (description "This package allows you to work with WAVE and RF64
files in Haskell.")
    (license license:bsd-3)))

(define-public ghc-wcwidth
  (package
    (name "ghc-wcwidth")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wcwidth" version))
       (sha256
        (base32 "1n1fq7v64b59ajf5g50iqj9sa34wm7s2j3viay0kxpmvlcv8gipz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wcwidth")))
    (inputs (list ghc-setlocale ghc-utf8-string ghc-attoparsec))
    (home-page "http://github.com/solidsnack/wcwidth/")
    (synopsis "Haskell bindings to wcwidth")
    (description
     "This package provides Haskell bindings to your system's
native wcwidth and a command line tool to examine the widths assigned by it.
The command line tool can compile a width table to Haskell code that assigns
widths to the Char type.")
    (license license:bsd-3)))

(define-public ghc-weigh
  (package
    (name "ghc-weigh")
    (version "0.0.18")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "weigh" version))
       (sha256
        (base32 "0bzix1dzq8ndhfxzpsm2sdjssdd0vzy7psj7gkg0rnvbcgf2w1nl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "weigh")))
    (inputs (list ghc-split ghc-temporary ghc-criterion-measurement))
    (home-page "https://github.com/fpco/weigh#readme")
    (synopsis "Measure allocations of a Haskell functions/values")
    (description "This package provides tools to measure the memory usage of a
Haskell value or function.")
    (license license:bsd-3)))

(define-public ghc-wizards
  (package
    (name "ghc-wizards")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wizards" version))
       (sha256
        (base32 "1clvbd1ckhvy29qrbmpkn7bya7300fq6znnps23nn3nxyrxhsr85"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wizards")))
    (inputs (list ghc-control-monad-free))
    (arguments
     `(#:cabal-revision ("3"
                         "0ra3vbxiasm7277zyxpvqpij2nf4lgc1rsv91b00cpp3bs4rhxyb")))
    (home-page "http://hackage.haskell.org/package/wizards")
    (synopsis "High level, generic library for interrogative user interfaces")
    (description
     "@code{wizards} is a package designed for the quick and painless
development of @emph{interrogative} programs, which revolve around a dialogue
with the user, who is asked a series of questions in a sequence much like an
installation wizard.

Everything from interactive system scripts, to installation wizards, to
full-blown shells can be implemented with the support of @code{wizards}.

It is developed transparently on top of a free monad, which separates out the
semantics of the program from any particular interface.  A variety of backends
exist, including console-based @code{System.Console.Wizard.Haskeline} and
@code{System.Console.Wizard.BasicIO}, and the pure
@code{System.Console.Wizard.Pure}.  It is also possible to write your own
backends, or extend existing back-ends with new features.  While both built-in
IO backends operate on a console, there is no reason why @code{wizards} cannot
also be used for making GUI wizard interfaces.")
    (license license:bsd-3)))

(define-public ghc-wl-pprint
  (package
    (name "ghc-wl-pprint")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wl-pprint" version))
       (sha256
        (base32 "0kn7y8pdrv8f87zhd5mifcl8fy3b2zvnzmzwhdqhxxlyzwiq6z0c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wl-pprint")))
    (home-page "http://hackage.haskell.org/package/wl-pprint")
    (synopsis "Wadler/Leijen pretty printer")
    (description
     "This is a pretty printing library based on Wadler's paper @i{A Prettier
Printer}.  This version allows the library user to declare overlapping
instances of the @code{Pretty} class.")
    (license license:bsd-3)))

(define-public ghc-wl-pprint-annotated
  (package
    (name "ghc-wl-pprint-annotated")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wl-pprint-annotated" version))
       (sha256
        (base32 "1br7qyf27iza213inwhf9bm2k6in0zbmfw6w4clqlc9f9cj2nrkb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wl-pprint-annotated")))
    (inputs (list ghc-semigroups))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("4"
                         "0via6f17s80d2kgw5ir7ii9ahmgp5ppky7pxschx4jrj3a88rypy")))
    (home-page "https://github.com/minad/wl-pprint-annotated#readme")
    (synopsis "Wadler/Leijen pretty printer with annotation support")
    (description
     "Annotations are useful for coloring.  This is a limited version of
@code{wl-pprint-extras} without support for point effects and without the free
monad.  Like in @code{annotated-wl-pprint}, only annotations are supported.
Compared to @code{annotated-wl-pprint} this library provides a slightly
modernized interface.")
    (license license:bsd-3)))

(define-public ghc-wl-pprint-text
  (package
    (name "ghc-wl-pprint-text")
    (version "1.2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wl-pprint-text" version))
       (sha256
        (base32 "0axivwh7bxmljxpfnccs66knxzrqck07byxmp2j737xbb26pf5cj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wl-pprint-text")))
    (inputs (list ghc-base-compat))
    (arguments
     `(#:cabal-revision ("3"
                         "1gc1vjzdhax9ybkjynsxkq4ah9ds14cy81wm1c4p9fnw2h356jhw")))
    (home-page "http://hackage.haskell.org/package/wl-pprint-text")
    (synopsis "Wadler/Leijen Pretty Printer for Text values")
    (description "A clone of wl-pprint for use with the text library.")
    (license license:bsd-3)))

(define-public ghc-word-wrap
  (package
    (name "ghc-word-wrap")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "word-wrap" version))
       (sha256
        (base32 "0i57233g4p9p8c0jf9mp3pvknwgv1lsrxm4mxjay38rw0372jpzq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "word-wrap")))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/jtdaugherty/word-wrap/")
    (synopsis "Haskell library for word-wrapping text")
    (description
     "The @code{word-wrap} Haskell library wraps long lines of text.")
    (license license:bsd-3)))

(define-public ghc-word8
  (package
    (name "ghc-word8")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "word8" version))
       (sha256
        (base32 "12jx7f13d2h1djq4fh4dyrab61sm49mj1w61j3rzp2vjfm696c16"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "word8")))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "http://hackage.haskell.org/package/word8")
    (synopsis "Word8 library for Haskell")
    (description "Word8 library to be used with @code{Data.ByteString}.")
    (license license:bsd-3)))

(define-public ghc-wordexp
  (package
    (name "ghc-wordexp")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wordexp" version))
       (sha256
        (base32
         "1mbcrq89jz0dcibw66w0jdy4f4bfpx4zwjfs98rm3jjgdikwdzb4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wordexp")))
    (native-inputs (list ghc-c2hs))
    (inputs
     (list ghc-semigroups))
    (home-page "https://hackage.haskell.org/package/wordexp")
    (synopsis "Library wrapping @code{wordexp} for Haskell")
    (description "@code{wordexp(3)} wrapper library for Haskell to perform
word expansion like a posix-shell.")
    (license license:bsd-3)))

(define-public ghc-x11
  (package
    (name "ghc-x11")
    (version "1.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "X11" version))
       (sha256
        (base32 "0hnj2q310a6s0h479hq8jsmywymvxdjxg13zw46mmdndynwd2jnq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "X11")))
    (inputs (list libx11 libxrandr libxinerama libxscrnsaver
                  ghc-data-default-class))
    (arguments
     `(#:extra-directories
       ("libx11" "libxrandr" "libxinerama" "libxscrnsaver")
       #:cabal-revision ("1"
                         "005g8q56bxc2w0cf2xgydqfs1r07bf17syv5smffvfx36h8gw78d")))
    (home-page "https://github.com/xmonad/X11")
    (synopsis "Bindings to the X11 graphics library")
    (description
     "This package provides Haskell bindings to the X11 graphics library.  The
bindings are a direct translation of the C bindings.")
    (license license:bsd-3)))

(define-public ghc-x11-xft
  (package
    (name "ghc-x11-xft")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "X11-xft" version))
       (sha256
        (base32 "05m988r45jiqpxqsw3vafz158whlwfcl7v8z9nnqnqz9mggd4032"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "X11-xft")))
    (arguments
     `(#:extra-directories ("libx11" "libxft" "xorgproto")))
    (inputs (list ghc-x11 ghc-utf8-string libx11 libxft xorgproto))
    (native-inputs (list pkg-config))
    (home-page "https://hackage.haskell.org/package/X11-xft")
    (synopsis "Bindings to Xft")
    (description
     "Bindings to the Xft, X Free Type interface library, and some Xrender
parts.")
    (license license:bsd-3)))

(define-public ghc-xdg-basedir
  (package
    (name "ghc-xdg-basedir")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "xdg-basedir" version))
       (sha256
        (base32 "0azlzaxp2dn4l1nr7shsxah2magk1szf6fx0mv75az00qsjw6qg4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xdg-basedir")))
    (home-page "http://github.com/willdonnelly/xdg-basedir")
    (synopsis "XDG Base Directory library for Haskell")
    (description
     "This package provides a library implementing the XDG Base Directory spec.")
    (license license:bsd-3)))

(define-public ghc-xml
  (package
    (name "ghc-xml")
    (version "1.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "xml" version))
       (sha256
        (base32 "0g814lj7vaxvib2g3r734221k80k7ap9czv9hinifn8syals3l9j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xml")))
    (arguments
     `(#:cabal-revision ("2"
                         "15cxa19dp8nqvrrp0bmndkdas2jzg573x8ri75r6kiv8r4vkv8y7")))
    (home-page "https://github.com/GaloisInc/xml")
    (synopsis "Simple XML library for Haskell")
    (description "This package provides a simple XML library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-xml-conduit
  (package
    (name "ghc-xml-conduit")
    (version "1.10.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "xml-conduit" version))
       (sha256
        (base32 "09nwn4yv2z8hv7shfpmv9avpxci21kk0dgbslgaymml0jny0la31"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xml-conduit")))
    (inputs (list ghc-conduit
                  ghc-conduit-extra
                  ghc-resourcet
                  ghc-xml-types
                  ghc-attoparsec
                  ghc-data-default
                  ghc-blaze-markup
                  ghc-blaze-html))
    (native-inputs (list ghc-hspec ghc-hunit ghc-doctest cabal-doctest))
    (home-page "http://github.com/snoyberg/xml")
    (synopsis "Utilities for dealing with XML with the conduit package")
    (description
     "This package provides pure-Haskell utilities for dealing with XML with
the @code{conduit} package.")
    (license license:expat)))

(define-public ghc-xml-types
  (package
    (name "ghc-xml-types")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "xml-types" version))
       (sha256
        (base32 "102cm0nvfmf9gn8hvn5z8qvmg931laczs33wwd5iyz9bc37f9mfs"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xml-types")))
    (home-page "https://git.singpolyma.net/xml-types-haskell")
    (synopsis "Basic types for representing XML")
    (description "This package provides basic types for representing XML
documents.")
    (license license:expat)))

(define-public ghc-xml-hamlet
  (package
    (name "ghc-xml-hamlet")
    (version "0.5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "xml-hamlet" version))
       (sha256
        (base32 "109fck1626d74s00ssjffg837584wf7dxpswkil37wqqfy94mw2z"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xml-hamlet")))
    (inputs (list ghc-shakespeare ghc-xml-conduit))
    (native-inputs (list ghc-hspec ghc-hunit))
    (home-page "http://www.yesodweb.com/")
    (synopsis "Hamlet-style quasiquoter for XML content")
    (description
     "This package provides a type-safe tool for generating XML
code via quasi-quoting built on top of @code{ghc-shakespeare}.")
    (license license:bsd-3)))

(define-public ghc-yaml
  (package
    (name "ghc-yaml")
    (version "0.11.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "yaml" version))
       (sha256
        (base32 "0bywv5q9a9yc8zxn4si5kp9gbfjrx8ham2n52d2ggzmhwlz94x7f"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "yaml")))
    (inputs (list ghc-aeson
                  ghc-attoparsec
                  ghc-conduit
                  ghc-libyaml
                  ghc-resourcet
                  ghc-scientific
                  ghc-unordered-containers
                  ghc-vector
                  ghc-optparse-applicative))
    (native-inputs (list ghc-hunit
                         ghc-base-compat
                         ghc-hspec
                         hspec-discover
                         ghc-mockery
                         ghc-raw-strings-qq
                         ghc-temporary))
    (arguments
     `(#:cabal-revision ("2"
                         "13gq30d720vaw4slwd14pi0pg116kazyjzxw1pjnhc7vw1cih2kg")))
    (home-page "https://github.com/snoyberg/yaml#readme")
    (synopsis "Parsing and rendering YAML documents")
    (description
     "This package provides a library to parse and render YAML documents.")
    (license license:bsd-3)))

(define-public ghc-zip-archive
  (package
    (name "ghc-zip-archive")
    (version "0.4.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "zip-archive" version))
       (sha256
        (base32 "0p6b4n4z3qa9f5vh25lqf7b8gdf5qcfs4zsnlzr12m0xgysfdnk1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "zip-archive")))
    (inputs (list ghc-zlib ghc-digest))
    (native-inputs (list ghc-hunit ghc-temporary unzip which))
    (arguments
     `(#:cabal-revision ("1"
                         "0vz55ja77fvza28clp1xn92ca1621dqhpb2gm4zi2yjjsp8gc95j")))
    (home-page "http://github.com/jgm/zip-archive")
    (synopsis "Zip archive library for Haskell")
    (description
     "The zip-archive library provides functions for creating,
modifying, and extracting files from zip archives in Haskell.")
    (license license:bsd-3)))

(define-public ghc-zlib
  (package
    (name "ghc-zlib")
    (version "0.7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "zlib" version))
       (sha256
        (base32 "1g2md8z0ijcbrqlx9q9i49myi2lnlvzmma1ajmsd5y0xp2v3ipbf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "zlib")))
    (inputs (list zlib))
    (native-inputs (list pkg-config ghc-quickcheck ghc-tasty ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "01m2afpizlscn65v12vmcmmycs66a14xb8nsgrm5145lq1slmrl5")))
    (home-page "http://hackage.haskell.org/package/zlib")
    (synopsis "Compression and decompression in the gzip and zlib formats")
    (description
     "This package provides a pure interface for compressing and decompressing
streams of data represented as lazy @code{ByteString}s.  It uses the zlib C
library so it has high performance.  It supports the @code{zlib}, @code{gzip}
and @code{raw} compression formats.  It provides a convenient high level API
suitable for most tasks and for the few cases where more control is needed it
provides access to the full zlib feature set.")
    (license license:bsd-3)))

(define-public ghc-zlib-bindings
  (package
    (name "ghc-zlib-bindings")
    (version "0.1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "zlib-bindings" version))
       (sha256
        (base32 "02ciywlz4wdlymgc3jsnicz9kzvymjw1www2163gxidnz4wb8fy8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "zlib-bindings")))
    (inputs (list ghc-zlib))
    (native-inputs (list ghc-hspec ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "0fq49694gqkab8m0vq4i879blswczwd66n7xh4r4gwiahf0ryvqc")))
    (home-page "http://github.com/snapframework/zlib-bindings")
    (synopsis "Low-level bindings to the @code{zlib} package")
    (description "This package provides low-level bindings to the
@code{zlib} package.")
    (license license:bsd-3)))

(define-public ghc-zstd
  (package
    (name "ghc-zstd")
    (version "0.1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "zstd" version))
       (sha256
        (base32 "0vghl48cxcqy72sqk2gpi7rvy5ya36j13vndaxi6kck6bqivbhm0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "zstd")))
    (native-inputs (list ghc-quickcheck ghc-test-framework
                         ghc-test-framework-quickcheck2))
    (home-page "https://github.com/luispedro/hs-zstd")
    (synopsis "Haskell bindings to the Zstandard compression algorithm")
    (description
     "This library provides Haskell bindings to the
Zstandard compression algorithm, a fast lossless compression algorithm
targeting real-time compression scenarios at zlib-level and better
compression ratios.")
    (license license:bsd-3)))

(define-public ghc-indexed-traversable
  (package
    (name "ghc-indexed-traversable")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "indexed-traversable" version))
       (sha256
        (base32 "08ivs1shxnvw5fzklvg7yh4xy0nnh3gsglm059fa5m9svyphkgjq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "indexed-traversable")))
    (arguments
     `(#:cabal-revision ("1"
                         "0zbys0254a7bsq4x297s1lagcbw7va5bkjikh8j7rhd0cm5fina2")))
    (home-page "http://hackage.haskell.org/package/indexed-traversable")
    (synopsis "Indexed Functor, Foldable, and Traversable typeclasses")
    (description
     "This Haskell package provides three useful generalizations:

@example
class Functor f => FunctorWithIndex i f | f -> i where
  imap :: (i -> a -> b) -> f a -> f b
@end example

@example
class Foldable f => FoldableWithIndex i f | f -> i where
  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m
@end example

@example
class (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t)
       => TraversableWithIndex i t | t -> i where
  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)
@end example

The @code{ghc-keys} package provides similar functionality, but uses
associated @code{TypeFamilies} instead of @code{FunctionalDependencies}.")
    (license license:bsd-2)))

(define-public ghc-type-equality
  (package
    (name "ghc-type-equality")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "type-equality" version))
       (sha256
        (base32 "1jfdm0g5r285bbarqc6pb6nmjg03m1s6jdh5lq0zr7xasd8q429b"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "type-equality")))
    (arguments
     `(#:cabal-revision ("1"
                         "0syx1a6bx22nyllb5y9x80znl5j94b0dqa9qcfk95d047j25mdjh")))
    (home-page "https://github.com/hesselink/type-equality")
    (synopsis "@code{Data.Type.Equality} compatibility package")
    (description
     "This library defines a propositional equality data type, shims
@code{Data.Type.Equality} as well as possible for older GHC versions (< 7.8).

@example
data a :~: b where
  Refl :: a :~: a
@end example

The module @code{Data.Type.Equality.Hetero} shims @@code{:~~:} equality, and
for compilers with @code{PolyKinds}.")
    (license license:bsd-3)))

(define-public ghc-typst-symbols
  (package
    (name "ghc-typst-symbols")
    (version "0.1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "typst-symbols" version))
       (sha256
        (base32 "1va29x72r0w0bms7wfsrhbnfn8cha1ghbaj33y62kflm50k9hwrg"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "typst-symbols")))
    (home-page "https://github.com/jgm/typst-symbols")
    (synopsis "Symbol and emoji lookup for typst language")
    (description
     "This package defines symbol and emoji codes for the typst language
(<https://typst.app>).")
    (license license:expat)))

(define-public ghc-typst
  (package
    (name "ghc-typst")
    (version "0.8.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "typst" version))
       (sha256
        (base32 "1q17vy9x5g64zzw5156bdh3mippin6ygkc3yqk0mbffkrfdcn21m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "typst")))
    (inputs (list ghc-typst-symbols
                  ghc-vector
                  ghc-ordered-containers
                  ghc-cassava
                  ghc-aeson
                  ghc-scientific
                  ghc-xml-conduit
                  ghc-yaml
                  ghc-toml-parser
                  ghc-regex-tdfa))
    (native-inputs (list ghc-pretty-show ghc-tasty ghc-tasty-golden))
    (home-page "http://hackage.haskell.org/package/typst")
    (synopsis "Parsing and evaluating typst syntax.")
    (description
     "This package provides a library for parsing and evaluating typst syntax.  Typst
(<https://typst.app>) is a document layout and formatting language.  This
library targets typst 0.13 and currently offers only partial support.")
    (license license:bsd-3)))

(define-public ghc-selective
  (package
    (name "ghc-selective")
    (version "0.7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "selective" version))
       (sha256
        (base32 "1qxfnqidlqw5hbsqs80i77nrkanz73jzyd4w157gamkr3kklpyyv"))
       (snippet
         #~(substitute* "selective.cabal"
             (("QuickCheck [<>=0-9. &|]*") "QuickCheck < 2.16")))
       (modules '((guix build utils)))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "selective")))
    (native-inputs (list ghc-quickcheck))
    (home-page "https://github.com/snowleopard/selective")
    (synopsis "Selective applicative functors")
    (description
     "This package implements selective applicative functors, which allow you
to declare your effects statically, and select which to execute dynamically.
See the
@uref{https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf, paper
on selective functors} for more details.")
    (license license:expat)))

(define-public ghc-keys
  (package
    (name "ghc-keys")
    (version "3.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "keys" version))
       (sha256
        (base32 "04l9ssmns3v2xzfrk5pxcacvl8nh26rsw5hhw22v4zxzbh9s44ll"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "keys")))
    (inputs (list ghc-comonad
                  ghc-free
                  ghc-hashable
                  ghc-semigroupoids
                  ghc-semigroups
                  ghc-tagged
                  ghc-transformers-compat
                  ghc-unordered-containers))
    (arguments
     `(#:cabal-revision ("1"
                         "05ma1kakwvvm618fmlwhkz16230w3qsn3p10c3zjysjhn1g0hhyf")))
    (home-page "http://github.com/ekmett/keys/")
    (synopsis "Keyed functors and containers")
    (description
     "This package provides a bunch of ad hoc classes for accessing parts of
a container.  In practice this package is largely subsumed by the
@code{ghc-lens}, but it is maintained for now as it has much simpler
dependencies.")
    (license license:bsd-3)))

(define-public ghc-pointed
  (package
    (name "ghc-pointed")
    (version "5.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pointed" version))
       (sha256
        (base32 "1wbfpd978pnkrbi05sf0yj6f50flxr9vl5m85xv3y22hzg6rb7gr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pointed")))
    (inputs (list ghc-data-default-class
                  ghc-comonad
                  ghc-kan-extensions
                  ghc-semigroupoids
                  ghc-tagged
                  ghc-transformers-compat
                  ghc-hashable
                  ghc-unordered-containers))
    (arguments
     `(#:cabal-revision ("1"
                         "0419jrc452kc24m764drlihmmx3aayzrf5lvrdvq6d327bkx2byh")))
    (home-page "http://github.com/ekmett/pointed/")
    (synopsis "Pointed and copointed data types")
    (description
     "This Haskell library provides pointed and copointed data types.")
    (license license:bsd-3)))

(define-public ghc-vector-instances
  (package
    (name "ghc-vector-instances")
    (version "3.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vector-instances" version))
       (sha256
        (base32 "1ajc65vj5j02qzfx11zvgmfx4lh5r99h4hg8wacdkyk1vw1rh9b7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vector-instances")))
    (inputs (list ghc-vector
                  ghc-semigroupoids
                  ghc-comonad
                  ghc-pointed
                  ghc-keys
                  ghc-hashable))
    (home-page "http://github.com/ekmett/vector-instances")
    (synopsis "Orphan instances for @code{Data.Vector}")
    (description
     "This Haskell library provides a place for lots of orphan instances for
the @code{ghc-vector} package.")
    (license license:bsd-3)))

(define-public ghc-vector-stream
  (package
    (name "ghc-vector-stream")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vector-stream" version))
       (sha256
        (base32 "0z5z88flyassdpgga412qci6brr9gyljbx875wd479fy9crhgxfh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vector-stream")))
    (arguments
     `(#:cabal-revision ("4"
                         "17i8x8vsnb853pagq4zsm6zfgv4zdxk8j1b42ylnj8f72ggycfs8")))
    (home-page "https://github.com/haskell/vector")
    (synopsis "Efficient Streams")
    (description
     "Simple yet powerful monadic streams that are used as a backbone for vector
package fusion functionality.")
    (license license:bsd-3)))

(define-public ghc-th-compat
  (package
    (name "ghc-th-compat")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-compat" version))
       (sha256
        (base32 "1sx4l374vjw3clc7a3vk8kkq37kxzbv9h3nh0racjaw7b70a10dp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "th-compat")))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "https://github.com/haskell-compat/th-compat")
    (synopsis
     "Backward- and forward-compatible @code{Quote} and @code{Code} types")
    (description
     "This package defines a @code{Language.Haskell.TH.Syntax.Compat} module,
which backports the @code{Quote} and @code{Code} types to work across a wide
range of @code{template-haskell} versions.  On recent versions of
@code{template-haskell} (2.17.0.0 or later), this module simply re-exports
@code{Quote} and @code{Code} from @code{Language.Haskell.TH.Syntax}.")
    (license license:bsd-3)))

(define-public ghc-filepattern
  (package
    (name "ghc-filepattern")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "filepattern" version))
       (sha256
        (base32 "0dlnwnwhsfdkwm69z66wj5d2x9n3la55glq4fsn5rxm2kr1msi6c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "filepattern")))
    (inputs (list ghc-extra))
    (native-inputs (list ghc-quickcheck))
    (home-page "https://github.com/ndmitchell/filepattern#readme")
    (synopsis "File path glob-like matching")
    (description
     "This package provides Haskell library for matching files using patterns
such as @code{\\\"src\\/**\\/*.png\\\"} for all @@file{.png} files recursively
under the @@file{src} directory.

Some of its features include:

@itemize
@item All matching is O(n).

@item Most functions pre-compute some information given only one argument.

@item Uses @code{match} and @code{substitute} to extract suitable strings from
the @code{*} and @code{**} matches, and substitutes them back into other
patterns.

@item Uses @code{step} and @code{matchMany} to perform bulk matching of many
patterns against many paths simultaneously.

@item Uses @code{System.FilePattern.Directory} to perform optimised directory
traverals using patterns.
@end itemize")
    (license license:bsd-3)))

(define-public ghc-lib-parser-ex
  (package
    (name "ghc-lib-parser-ex")
    (version "9.12.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ghc-lib-parser-ex" version))
       (sha256
        (base32 "0w8mfa0g55m5i5ysxlkgci1grssi76za4523ygmsicb0ibq9ajb2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ghc-lib-parser-ex")))
    (inputs (list ghc-lib-parser ghc-uniplate ghc-extra
                  ghc-optparse-applicative))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/shayne-fletcher/ghc-lib-parser-ex#readme")
    (synopsis "Algorithms on GHC parse trees")
    (description
     "The @code{ghc-lib-parser-ex} package contains GHC API parse tree utilities.")
    (license license:bsd-3)))

(define-public ghc-lift-type
  (package
    (name "ghc-lift-type")
    (version "0.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lift-type" version))
       (sha256
        (base32 "1i43px33w8pjhm4s7z2ys3546qshsd7dnjyxlhq0prkhfjfg4rbc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lift-type")))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/parsonsmatt/lift-type#readme")
    (synopsis
     "Lift a type from a Typeable constraint to a Template Haskell type")
    (description
     "This library provides a utility function @code{liftType}, which accepts a type
application argument and returns the Template Haskell @code{Type} representation of
it.")
    (license license:bsd-3)))

(define-public ghc-unicode-collation
  (package
    (name "ghc-unicode-collation")
    (version "0.1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unicode-collation" version))
       (sha256
        (base32 "0c1s1n4cqhjibiv05h5qfl4cg4h11ny4jay0gqnqg36z9kb66hfn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "unicode-collation")))
    (inputs (list ghc-th-lift-instances))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit
                         ghc-unicode-transforms ghc-doctest))
    (arguments
     `(#:cabal-revision ("2"
                         "01jnv53mizky61zr1i469wk2ry19mpczgylfxg3ilbj9221wv7zm")))
    (home-page "https://github.com/jgm/unicode-collation")
    (synopsis "Haskell implementation of the Unicode Collation Algorithm")
    (description
     "This library provides a pure Haskell implementation of the Unicode
Collation Algorithm described at @uref{http://www.unicode.org/reports/tr10/}.
It is not as fully-featured or as performant as @code{text-icu}, but it avoids
a dependency on a large C library.  Locale-specific tailorings are also
provided.")
    (license license:bsd-3)))

(define-public ghc-citeproc
  (package
    (name "ghc-citeproc")
    (version "0.9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "citeproc" version))
       (sha256
        (base32 "1s1gdd7piwssp5b6bwbfyp9sfna052v3rayav7di44yapm5dazmr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "citeproc")))
    (inputs (list ghc-safe
                  ghc-case-insensitive
                  ghc-vector
                  ghc-scientific
                  ghc-uniplate
                  ghc-xml-conduit
                  ghc-attoparsec
                  ghc-data-default
                  ghc-aeson
                  ghc-file-embed
                  ghc-pandoc-types
                  ghc-unicode-collation
                  ghc-base-compat
                  ghc-aeson-pretty))
    (native-inputs (list ghc-timeit ghc-diff))
    (home-page "http://hackage.haskell.org/package/citeproc")
    (synopsis "Generate citations and bibliography from CSL styles")
    (description
     "@code{ghc-citeproc} parses @acronym{Citation Style Language, CSL} style files
and uses them to generate a list of formatted citations and bibliography
entries.  For more information about CSL, see @uref{https://citationstyles.org/}.")
    (license license:bsd-2)))

(define-public ghc-commonmark
  (package
    (name "ghc-commonmark")
    (version "0.2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "commonmark" version))
       (sha256
        (base32 "0wwh0smqdmyb1qa992d0jva24yml4lxmmwr3av3c8s0xh3z7vkpv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "commonmark")))
    (inputs (list ghc-unicode-transforms ghc-unicode-data))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit))
    (home-page "https://github.com/jgm/commonmark-hs")
    (synopsis "Pure Haskell Commonmark parser")
    (description
     "This library provides the core data types and functions for parsing
@uref{https://spec.commonmark.org, Commonmark}.  The parser is fully
Commonmark-compliant and passes the test suite.  It is designed to be
customizable and easily extensible.  To customize the output, create an AST,
or support a new output format, one need only define some new typeclass
instances.  It is also easy to add new syntax elements or modify existing
ones.

Accurate information about source positions is available for all block and
inline elements.  Thus the library can be used to create an accurate syntax
highlighter or an editor with live preview.  The parser has been designed for
robust performance even in pathological cases that tend to cause stack
overflows or exponential slowdowns in other parsers, with parsing speed that
varies linearly with input length.")
    (license license:bsd-3)))

(define-public ghc-commonmark-extensions
  (package
    (name "ghc-commonmark-extensions")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "commonmark-extensions" version))
       (sha256
        (base32 "1qhd6q00ccdr0l9zv1vknpc2fx0p4avdf9zcd27q20q3fdp0phsz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "commonmark-extensions")))
    (inputs (list ghc-network-uri ghc-commonmark ghc-emojis))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/jgm/commonmark-hs")
    (synopsis "Extensions for @code{ghc-commonmark}")
    (description
     "This library provides some useful extensions for @code{ghc-commonmark}
to parser core commonmark syntax: smart quotes, definition lists, tables,
footnotes, math, and more.")
    (license license:bsd-3)))

(define-public ghc-commonmark-pandoc
  (package
    (name "ghc-commonmark-pandoc")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "commonmark-pandoc" version))
       (sha256
        (base32 "187gkvv0pp7bb8np8nns80aqprd2zpw7hxp9knvylckayysgig5l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "commonmark-pandoc")))
    (inputs (list ghc-commonmark ghc-commonmark-extensions ghc-pandoc-types))
    (home-page "https://github.com/jgm/commonmark-hs")
    (synopsis "Bridge between Commonmark and Pandoc AST")
    (description
     "This library provides typeclasses for rendering @code{ghc-commonmark} to
Pandoc types.")
    (license license:bsd-3)))

(define-public ghc-criterion-measurement
  (package
    (name "ghc-criterion-measurement")
    (version "0.2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "criterion-measurement" version))
       (sha256
        (base32 "1qnzgkmjlv3m6zr8l3dfnc1b5jy2fyjxb7s1qzhdcww4bsixqxfc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "criterion-measurement")))
    (inputs (list ghc-aeson ghc-base-compat ghc-vector))
    (home-page "https://github.com/haskell/criterion")
    (synopsis "Criterion measurement functionality and associated types")
    (description
     "Measurement-related functionality extracted from Criterion, with minimal
dependencies.  The rationale for this is to enable alternative analysis
front-ends.")
    (license license:bsd-3)))

(define-public ghc-criterion
  (package
    (name "ghc-criterion")
    (version "1.6.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "criterion" version))
       (sha256
        (base32 "0l9gxar759nskhm7gskr3j08bw8515amw6rr4n3zx3978dxg8aq6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "criterion")))
    (inputs (list ghc-aeson
                  ghc-base-compat-batteries
                  ghc-binary-orphans
                  ghc-cassava
                  ghc-code-page
                  ghc-criterion-measurement
                  ghc-glob
                  ghc-microstache
                  ghc-js-chart
                  ghc-mwc-random
                  ghc-optparse-applicative
                  ghc-prettyprinter
                  ghc-prettyprinter-ansi-terminal
                  ghc-statistics
                  ghc-transformers-compat
                  ghc-vector
                  ghc-vector-algorithms))
    (native-inputs (list ghc-hunit
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-quickcheck
                         ghc-hunit
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-hunit
                         ghc-base-compat
                         ghc-tasty
                         ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "0wwzijzvqrv7swpalr24i3j4pjcjm266ybhhah853d783zz37vzz")))
    (home-page "http://www.serpentine.com/criterion")
    (synopsis "Robust, reliable performance measurement and analysis")
    (description
     "This library provides a powerful but simple way to measure software performance.
 It provides both a framework for executing and analysing benchmarks and a set
of driver functions that makes it easy to build and run benchmarks, and to
analyse their results.")
    (license license:bsd-3)))

(define-public ghc-hclip
  (package
    (name "ghc-hclip")
    (version "3.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Hclip" version))
       (sha256
        (base32 "04ppwm7vfzndrys8x1n8vfb41vzwx59r9xp4dkbiqmrms390pj6q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Hclip")))
    (inputs (list ghc-strict))
    (home-page "https://github.com/jetho/Hclip")
    (synopsis
     "Small cross-platform library for reading and modifying the system clipboard")
    (description
     "This package provides a small cross-platform library for reading and
modifying the system clipboard.  It uses @code{xclip} or @code{xsel}
at runtime.")
    (license license:bsd-3)))

(define-public ghc-hslua-module-path
  (package
    (name "ghc-hslua-module-path")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-module-path" version))
       (sha256
        (base32 "035q8ll2bkm25m4q24zby35gy6ihrg21qlqmmk6af8rz09kjyal0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-module-path")))
    (inputs (list ghc-hslua-core ghc-hslua-marshalling ghc-hslua-packaging))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-lua))
    (home-page "https://hslua.org/")
    (synopsis "Lua module to work with file paths")
    (description
     "This Haskell library provides a Lua module to work with file paths in a
platform independent way.")
    (license license:expat)))

(define-public ghc-template-haskell
  (package
    (name "ghc-template-haskell")
    (version "2.22.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "template-haskell" version))
       (sha256
        (base32 "0cn9n5jyzn9h1ab76rr2cyxaysh2rk7ywcj92lxsf49fah4vx35g"))))
    (build-system haskell-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'relax-dependencies
            (lambda _
              (substitute* "template-haskell.cabal"
                (("ghc-boot-th [0-9 &|<>=*^.]*") "ghc-boot-th")))))))
    (properties '((upstream-name . "template-haskell")))
    (home-page "http://hackage.haskell.org/package/template-haskell")
    (synopsis "Support library for Template Haskell")
    (description
     "This package provides modules containing facilities for manipulating
Haskell source code using Template Haskell.  See
@uref{http://www.haskell.org/haskellwiki/Template_Haskell} for more
information.")
    (license license:bsd-3)))

(define-public ghc-genvalidity-hspec
  (package
    (name "ghc-genvalidity-hspec")
    (version "1.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "genvalidity-hspec" version))
       (sha256
        (base32 "0z9qk1yd3hz7zxbsa6j4xydy962im7ihi1r36n94hcvj5lvi5zsj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "genvalidity-hspec")))
    (inputs (list ghc-quickcheck
                  ghc-genvalidity
                  ghc-genvalidity-property
                  ghc-hspec
                  ghc-hspec-core
                  ghc-validity))
    (native-inputs (list hspec-discover))
    (home-page "https://github.com/NorfairKing/validity#readme")
    (synopsis "Standard spec's for @code{GenValidity} instances")
    (description
     "This haskell library provides validity and validity-based testing for
@code{ghc-hspec}.")
    (license license:expat)))

(define-public ghc-binary-orphans
  (package
    (name "ghc-binary-orphans")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "binary-orphans" version))
       (sha256
        (base32 "13a08if69wdmc8hb7jynhllgh8jh7qbk9spygb096l9aijgaxyrr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "binary-orphans")))
    (native-inputs (list ghc-onetuple
                         ghc-quickcheck
                         ghc-quickcheck-instances
                         ghc-tagged
                         ghc-tasty
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "0pqzv84g0jkx2329p6qv10kd4nbms9ic704ljw4jsrxfg9bdy5qj")))
    (home-page "http://hackage.haskell.org/package/binary-orphans")
    (synopsis "Compatibility package for binary")
    (description "This package provides instances defined in later versions of
@code{ghc-binary} package.")
    (license license:bsd-3)))

(define-public ghc-netlink
  (package
    (name "ghc-netlink")
    (version "1.1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "netlink" version))
       (sha256
        (base32 "1q8sxycv93sap6dgbw70scklnpjj5vav6qlvsxm5500jlvb3jnf0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "netlink")))
    (inputs
     (list ghc-cereal ghc-monad-loops ghc-pretty-hex ghc-language-c
           ghc-regex-pcre))
    (home-page "https://github.com/Ongy/netlink-hs")
    (synopsis "Netlink communication for Haskell")
    (description
     "This is library provides utility to use Netlink from Haskell.  The scope of
this package extends over general Netlink support to specific implementations
of Netlink families.")
    (license license:bsd-3)))

(define-public ghc-doctest-driver-gen
  (package
    (name "ghc-doctest-driver-gen")
    (version "0.3.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "doctest-driver-gen" version))
       (sha256
        (base32 "0x6d2crc8jibixq0fdzbbqls7l79hb8la3mp9yd1dgikwp1bbncz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "doctest-driver-gen")))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'add-doctest-driver-to-PATH
            (lambda _
              (setenv "PATH" (string-append (getenv "PATH")
                                            ":dist/build/doctest-driver-gen")))))))
    (native-inputs (list ghc-doctest))
    (home-page "https://github.com/Hexirp/doctest-driver-gen#readme")
    (synopsis "Generate driver file for Doctest's Cabal integration")
    (description
     "@code{ghc-doctest-driver-gen} is a Doctest's driver file generator.  It
lets you automatically generate driver file for Doctest's Cabal integration.")
    (license license:bsd-3)))

(define-public ghc-template-haskell-compat-v0208
  (package
    (name "ghc-template-haskell-compat-v0208")
    (version "0.1.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "template-haskell-compat-v0208" version))
       (sha256
        (base32 "07wx8k16rhhkm3mx1by4np4zdi0kgn1i9li1jnsk07ymr26rydai"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "template-haskell-compat-v0208")))
    (home-page
     "https://github.com/nikita-volkov/template-haskell-compat-v0208")
    (synopsis
     "Backwards compatibility layer for Template Haskell newer than 2.8")
    (description
     "This package provides a backwards compatibility layer for Template
Haskell newer than 2.8.")
    (license license:expat)))

(define-public ghc-mysql
  (package
    (name "ghc-mysql")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mysql" version))
       (sha256
        (base32 "051w428arxbix06a52dacqjpnkfx42zbazxsd3l9d857dsd0kl3g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mysql")))
    (arguments `(#:tests? #f)) ; TODO: Fails to connect to server.
    (inputs (list (list mariadb "dev") openssl zlib))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/paul-rouse/mysql")
    (synopsis "Low-level MySQL client library")
    (description
     "This library provides Haskell bindings to the MySQL @code{mysqlclient}
client library.  It is a fairly faithful, low level library that implements
most of the MySQL client API.  The major departure from the C API is that in
Haskell, resource management is mostly automatic and safe.

This library deliberately avoids the question of providing a ``good'' API.
Its purpose is to serve as a base upon which higher-level libraries can be
built.")
    (license license:bsd-3)))

(define-public ghc-blaze-textual
  (package
    (name "ghc-blaze-textual")
    (version "0.2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "blaze-textual" version))
       (sha256
        (base32 "1chpaynfqiykqdk4jrmwxczj01wph8qfb411600l0gj3g34wlanx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "blaze-textual")))
    (inputs (list ghc-blaze-builder ghc-old-locale ghc-vector))
    (native-inputs (list ghc-quickcheck ghc-double-conversion
                         ghc-test-framework ghc-test-framework-quickcheck2))
    (home-page "http://github.com/swamp-agr/blaze-textual")
    (synopsis "Fast rendering of common datatypes")
    (description
     "@code{ghc-blaze-textual} is a fast Haskell library for rendering common
Haskell datatypes in text form using the @code{ghc-blaze-builder} library.")
    (license license:bsd-3)))

(define-public ghc-mysql-simple
  (package
    (name "ghc-mysql-simple")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mysql-simple" version))
       (sha256
        (base32 "0hwv1hlr65m5l2zrrj5zmvrjz9y2814jy05l17l5jb4j4j5xw3z2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mysql-simple")))
    (arguments (list #:tests? #f)) ; tests require a running MySql database
    (inputs (list ghc-attoparsec
                  ghc-base16-bytestring
                  ghc-blaze-builder
                  ghc-mysql
                  ghc-pcre-light
                  pcre
                  ghc-old-locale
                  ghc-vector))
    (home-page "https://github.com/paul-rouse/mysql-simple")
    (synopsis "Mid-level MySQL client library")
    (description
     "This library implements mid-level Haskell bindings to the MySQL
@code{mysqlclient} client library.  It is aimed at speed and ease of use.")
    (license license:bsd-3)))

(define-public ghc-persistent-qq
  (package
    (name "ghc-persistent-qq")
    (version "2.12.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "persistent-qq" version))
       (sha256
        (base32 "1swbhc1gqn2njc4ycpizqbkl77xrz7a8sizlcb6lcizb9zaakkf4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "persistent-qq")))
    (inputs (list ghc-haskell-src-meta ghc-persistent))
    (native-inputs (list ghc-aeson
                         ghc-fast-logger
                         ghc-hspec
                         ghc-hunit
                         ghc-monad-logger
                         ghc-persistent-sqlite
                         ghc-resourcet
                         ghc-unliftio))
    (home-page "https://github.com/yesodweb/persistent#readme")
    (synopsis "Quasi-quoter for raw SQL for @code{ghc-persistent}")
    (description
     "This package provides a quasi-quoter for raw @acronym{SQL, Structured Query
Language} for @code{ghc-persistent}.")
    (license license:expat)))

(define-public ghc-persistent-mysql
  (package
    (name "ghc-persistent-mysql")
    (version "2.13.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "persistent-mysql" version))
       (sha256
        (base32 "1dg709kz1rrgj3ir24a8pww47my03h3k5vcn2qld7h2ffcbnlxd9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "persistent-mysql")))
    (arguments (list #:tests? #f)) ; tests require a running MySql database
    (inputs (list ghc-persistent
                  ghc-aeson
                  ghc-blaze-builder
                  ghc-conduit
                  ghc-monad-logger
                  ghc-mysql
                  ghc-mysql-simple
                  ghc-resourcet
                  ghc-resource-pool
                  ghc-unliftio-core
                  pcre))
    (home-page "http://www.yesodweb.com/book/persistent")
    (synopsis
     "Backend for the @code{ghc-persistent} library using MySQL database server")
    (description
     "This package contains a backend for @code{ghc-persistent} using the MySQL database
server.  Internally it uses the @code{ghc-mysql-simple} and @code{mysql} packages in order
to access the database.  This package supports only MySQL 5.1 and above.
However, it has been tested only on MySQL 5.5.  Only the InnoDB storage engine
is officially supported.")
    (license license:expat)))

(define-public ghc-hspec-expectations-lifted
  (package
    (name "ghc-hspec-expectations-lifted")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-expectations-lifted" version))
       (sha256
        (base32 "0a1qwz0n80lph8m9cq6cb06m8bsmqgg8ifx0acpylvrrkd8g3k92"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-expectations-lifted")))
    (inputs (list ghc-hspec-expectations))
    (home-page "http://hackage.haskell.org/package/hspec-expectations-lifted")
    (synopsis
     "Version of @code{ghc-hspec-expectations} generalized to @code{MonadIO}")
    (description
     "This package provides a version of @code{ghc-hspec-expectations} generalized
to @code{MonadIO}.")
    (license license:expat)))

(define-public ghc-string-conversions
  (package
    (name "ghc-string-conversions")
    (version "0.4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "string-conversions" version))
       (sha256
        (base32 "150rdank90h7v08x0wq4dffjbxv2daf5v9sqfs5mab76kinwxg26"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "string-conversions")))
    (inputs (list ghc-utf8-string))
    (native-inputs (list ghc-hspec hspec-discover
                         ghc-quickcheck-instances ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1g3wxx579mhs2icxngi78pvjfybbk606a6vgns88pg6ws5hrvx4s")))
    (home-page "https://github.com/soenkehahn/string-conversions#readme")
    (synopsis "Simplify dealing with different types for strings")
    (description
     "This package provides a simple type class for converting values of different
string types into values of other string types.")
    (license license:bsd-3)))

(define-public ghc-postgresql-libpq-configure
  (package
    (name "ghc-postgresql-libpq-configure")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "postgresql-libpq-configure" version))
       (sha256
        (base32 "1n5jjnnflh2ldqvcs44al572240s2435bh5m761hrpbmai5y6kwd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "postgresql-libpq-configure")))
    (inputs (list postgresql))
    (home-page "https://github.com/haskellari/postgresql-libpq")
    (synopsis "low-level binding to libpq: configure based provider")
    (description
     "This is a binding to libpq: the C application programmer's interface to
@code{PostgreSQL}.  libpq is a set of library functions that allow client
programs to pass queries to the @code{PostgreSQL} backend server and to receive
the results of these queries.")
    (license license:bsd-3)))

(define-public ghc-postgresql-libpq
  (package
    (name "ghc-postgresql-libpq")
    (version "0.11.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "postgresql-libpq" version))
       (sha256
        (base32 "18yj7vb51r72ybzi7849w83b79gydnh7az1wkc037fz6iwhb2jh3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "postgresql-libpq")))
    (inputs (list ghc-postgresql-libpq-configure))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "1i546w5an064cbikp66a4yq7j8gmi2iy9vkm1sax6yjzfpgsqzya")))
    (home-page "https://github.com/haskellari/postgresql-libpq")
    (synopsis "Low-level bindings to @code{libpq}")
    (description
     "This package provides bindings to @code{libpq}: the C application
programmer's interface to PostgreSQL.  @code{libpq} is a set of library
functions that allow client programs to pass queries to the PostgreSQL backend
server and to receive the results of these queries.")
    (license license:bsd-3)))

(define-public ghc-postgresql-simple
  (package
    (name "ghc-postgresql-simple")
    (version "0.7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "postgresql-simple" version))
       (sha256
        (base32 "16dbydsi5hib4zksl2sri0mz0hwfnagb2f757q1xc603clhg6z2r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "postgresql-simple")))
    (inputs (list ghc-time-compat
                  ghc-aeson
                  ghc-attoparsec
                  ghc-case-insensitive
                  ghc-hashable
                  ghc-only
                  ghc-postgresql-libpq
                  ghc-scientific
                  ghc-uuid-types
                  ghc-vector))
    (native-inputs (list ghc-inspection-testing
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-base16-bytestring
                         ghc-cryptohash-md5
                         ghc-tasty
                         ghc-tasty-golden
                         ghc-tasty-hunit))
    (home-page "http://hackage.haskell.org/package/postgresql-simple")
    (synopsis "Mid-Level PostgreSQL client library")
    (description
     "This package provides a mid-Level PostgreSQL client library, forked from
@code{ghc-mysql-simple}.")
    (license license:bsd-3)))

(define-public ghc-persistent-postgresql
  (package
    (name "ghc-persistent-postgresql")
    (version "2.13.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "persistent-postgresql" version))
       (sha256
        (base32 "1774fh28jls2r692164ln66ipa6gl3sqj8pb04nf3sl1m498qjd7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "persistent-postgresql")))
    (arguments (list #:tests? #f)) ; tests require a running postgresql server
    (inputs (list ghc-aeson
                  ghc-attoparsec
                  ghc-blaze-builder
                  ghc-conduit
                  ghc-monad-logger
                  ghc-persistent
                  ghc-postgresql-libpq
                  ghc-postgresql-simple
                  ghc-resource-pool
                  ghc-resourcet
                  ghc-string-conversions
                  ghc-unliftio-core
                  ghc-vault
                  ghc-unliftio))
    (native-inputs (list ghc-fast-logger
                         ghc-hspec
                         ghc-hspec-expectations
                         ghc-hspec-expectations-lifted
                         ghc-http-api-data
                         ghc-hunit
                         ghc-path-pieces
                         ghc-persistent-qq
                         ghc-persistent-test
                         ghc-quickcheck
                         ghc-quickcheck-instances
                         ghc-unordered-containers
                         ghc-vector))
    (home-page "http://www.yesodweb.com/book/persistent")
    (synopsis "Backend for the @code{ghc-persistent library} using Postgresql")
    (description
     "This package provides a backend for the @code{ghc-persistent} library
using the @code{ghc-postgresql-simple} package.")
    (license license:expat)))

(define-public ghc-filtrable
  (package
    (name "ghc-filtrable")
    (version "0.1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "filtrable" version))
       (sha256
        (base32 "058jl7wjaxzvcayc9qzpikxvi9x42civ4sb02jh66rcvpndbfh5y"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "filtrable")))
    (native-inputs (list ghc-smallcheck ghc-tasty ghc-tasty-smallcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "0ajsh1600c8rkgrallz0m2b3cwqy76yy52niikx2prj0z7k7lbv6")))
    (home-page "https://github.com/strake/filtrable.hs")
    (synopsis "Class of filtrable containers")
    (description "This package provides filtrable containers.")
    (license license:bsd-3)))

(define-public ghc-filelock
  (package
    (name "ghc-filelock")
    (version "0.1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "filelock" version))
       (sha256
        (base32 "0bmnj888w2srz2rywmh13dqwmqsqyzkgkz952h1gdd7ycvlj5avj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "filelock")))
    (native-inputs (list ghc-async))
    (home-page "http://github.com/haskell-pkg-janitors/filelock")
    (synopsis "Portable interface to file locking")
    (description
     "This package provides an interface to file locking functionalities.")
    (license license:public-domain)))

(define-public ghc-file-io
  (package
    (name "ghc-file-io")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "file-io" version))
       (sha256
        (base32 "0nvxp3d7j2fdkfcvk5n7swc0id7c7gzp3g0jr4q4vpljqzj1j2ii"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "file-io")))
    (native-inputs (list ghc-tasty
                         ghc-tasty-hunit
                         ghc-temporary))
    (home-page "https://github.com/hasufell/file-io")
    (synopsis "Basic file IO operations via 'OsPath'")
    (description
     "Basic file IO operations like Prelude, but for @code{OsPath}'.")
    (license license:bsd-3)))

(define-public ghc-hsyaml-aeson
  (package
    (name "ghc-hsyaml-aeson")
    (version "0.2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "HsYAML-aeson" version))
       (sha256
        (base32 "12f9fdkgbg9gk7gbf5v3w0b68s382rxjkpxmvsq0ga02v7nhlvna"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "HsYAML-aeson")))
    (inputs (list ghc-hsyaml ghc-aeson ghc-scientific ghc-unordered-containers
                  ghc-vector))
    (home-page "http://hackage.haskell.org/package/HsYAML-aeson")
    (synopsis "JSON to YAML adapter")
    (description
     "The @uref{https://yaml.org/spec/1.2/spec.html, YAML 1.2} format provides
a much richer data-model and feature-set than the
@uref{https://tools.ietf.org/html/rfc7159, @acronym{JavaScript Object
Notation, JSON}} format.  However, sometimes it's desirable to ignore the extra
capabilities and treat YAML as if it was merely a more convenient markup
format for humans to write JSON data.  To this end this module provides a
compatibility layer atop @code{ghc-hsyaml} ,which allows decoding YAML
documents in the more limited JSON data-model while also providing convenience
by reusing @code{ghc-aeson}'s @code{FromJSON} instances for decoding the YAML
data into native Haskell data types.")
    (license license:gpl2+)))

(define-public ghc-lukko
  (package
    (name "ghc-lukko")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lukko" version))
       (sha256
        (base32 "0vcqds8ihpjw6zv5rxdzzykh05wayx376wvz77s63d15ls56zn3j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lukko")))
    (native-inputs (list ghc-async
                         ghc-singleton-bool
                         ghc-tasty
                         ghc-tasty-expected-failure
                         ghc-tasty-hunit
                         ghc-temporary))
    (arguments
     `(#:cabal-revision ("1"
                         "0a6ah941w8d3y4km53h5palz38dabna05p132kff31g323cahd43")))
    (home-page "http://hackage.haskell.org/package/lukko")
    (synopsis "File locking")
    (description
     "This package provides access to platform dependent file locking APIs.  There
are alternative file locking packages:

@itemize
@item @code{GHC.IO.Handle.Lock} in @code{base >= 4.10} is good enough for most
use cases.  However, uses only @code{Handle}s so these locks cannot be used
for intra-process locking.

@item @code{ghc-filelock} doesn't support @acronym{OFD, open file descriptor}
locking.
@end itemize")

    (license (list license:gpl2+ license:bsd-3))))

(define-public ghc-dec
  (package
    (name "ghc-dec")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "dec" version))
       (sha256
        (base32 "0bbzn9kqb1zdvi4kd37p3lhx0rkdbyq98hqcn9qv5y67s6a3c5gv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "dec")))
    (inputs (list ghc-boring))
    (arguments
     `(#:cabal-revision ("1"
                         "1xwkjk0shcbwlmiz8zsx69r652zfkh3k2bj1vpzdsjj241g3n6r0")))
    (home-page "https://github.com/phadej/dec")
    (synopsis "Decidable propositions")
    (description
     "This package provides a @code{Dec} type for representing deciable
relations.

@example
type Neg a = a -> Void

data Dec a
    = Yes a
    | No (Neg a)
@end example")
    (license license:bsd-3)))

(define-public ghc-ansi2html
  (package
    (name "ghc-ansi2html")
    (version "0.9")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "Ansi2Html" version))
              (sha256
               (base32
                "1dqq1rnx1w0cn4w11knmxvn7qy4lg4m39dgw4rs6r2pjqzgrwarh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Ansi2Html")))
    (home-page "http://janzzstimmpfle.de/~jens/software/Ansi2Html/")
    (synopsis "Convert ANSI Terminal Sequences to nice HTML markup")
    (description
     "This package enables integration of terminal screen state in html
pages.")
    (license license:bsd-3)))

(define-public ghc-ansi-terminal-types
  (package
    (name "ghc-ansi-terminal-types")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ansi-terminal-types" version))
       (sha256
        (base32 "12d625xa33qwwzpw75zpw05mk2a5qvqwj8jdkbcrp27iawhwxjcz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ansi-terminal-types")))
    (inputs (list ghc-colour))
    (home-page "https://github.com/UnkindPartition/ansi-terminal")
    (synopsis "Types and functions used to represent SGR aspects")
    (description
     "The \\'ANSI\\ standards refer to the visual style of displaying characters as
their \\'graphic rendition\\'.  The \\'ANSI\\ codes to establish the graphic
rendition for subsequent text are referred to as SELECT GRAPHIC RENDITION (SGR).
 This package exposes modules that export types and functions used to represent
SGR aspects.")
    (license license:bsd-3)))

(define-public ghc-open-browser
  (package
    (name "ghc-open-browser")
    (version "0.4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "open-browser" version))
       (sha256
        (base32 "0pgqrdwmzw70yfqvbssc01b8n3aqw3l6a92j16vby9x0cv803zyy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "open-browser")))
    (home-page "https://github.com/mpilgrem/open-browser")
    (synopsis "Open a web browser from Haskell")
    (description "Haskell library for opening the web browser.")
    (license license:bsd-3)))

(define-public ghc-singleton-bool
  (package
    (name "ghc-singleton-bool")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "singleton-bool" version))
       (sha256
        (base32 "0wql2gdy93f62ghv93xcqjm6ajs5glyz8si605wrll9vp79g66r6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "singleton-bool")))
    (inputs (list ghc-boring ghc-dec ghc-some))
    (arguments
     `(#:cabal-revision ("1"
                         "1hbfb98zrk78cfbbw32yf9p37qy4m9q347ivy16jvr7s158kr1pj")))
    (home-page "https://github.com/phadej/singleton-bool#readme")
    (synopsis "Type-level booleans")
    (description "This package provides Type-level booleans.")
    (license license:bsd-3)))

(define-public ghc-breakpoint
  (package
    (name "ghc-breakpoint")
    (version "0.1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "breakpoint" version))
       (sha256
        (base32 "1x70c0m111557r947zcdgcrfsbnm0j4dzw3b885pxvfz7i58pysc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "breakpoint")))
    (inputs (list ghc-pretty-simple ghc-ansi-terminal))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "http://hackage.haskell.org/package/breakpoint")
    (synopsis "Set breakpoints using a GHC plugin")
    (description
     "This package provides a plugin that allows you to set breakpoints for debugging
purposes.  See the
[README](https://github.com/aaronallen8455/breakpoint#breakpoint) for details.")
    (license license:expat)))

(define-public ghc-brick
  (package
    (name "ghc-brick")
    (version "2.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "brick" version))
       (sha256
        (base32 "1jdhagw5ihp9g5pinmbb1mf3fjcbh4aim9qg9i4b1na9zn07s0f1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "brick")))
    (inputs (list ghc-vty
                  ghc-vty-crossplatform
                  ghc-bimap
                  ghc-data-clist
                  ghc-microlens
                  ghc-microlens-th
                  ghc-microlens-mtl
                  ghc-config-ini
                  ghc-vector
                  ghc-text-zipper
                  ghc-unix-compat
                  ghc-word-wrap
                  ghc-unordered-containers
                  ghc-hashable
                  ghc-random
                  ghc-microlens-platform))
    (native-inputs (list ghc-quickcheck))
    (home-page "https://github.com/jtdaugherty/brick/")
    (synopsis "Declarative terminal user interface library")
    (description
     "Brick helps you write @dfn{terminal user interfaces} (TUIs).  You write
an event handler and a drawing function and the library does the rest.")
    (license license:bsd-3)))

(define-public ghc-brick-skylighting
  (package
    (name "ghc-brick-skylighting")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "brick-skylighting" version))
       (sha256
        (base32 "1nw2x9zn0jlvykm89v80fh4187bxgn8l4cljgnf4mp4ci7aqjmkr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "brick-skylighting")))
    (inputs (list ghc-brick ghc-vty ghc-skylighting-core))
    (home-page "https://github.com/jtdaugherty/brick-skylighting/")
    (synopsis "Show syntax-highlighted text in your Brick UI")
    (description
     "This package provides a module to use Skylighting to perform syntax
highlighting and display the results in Brick-based interfaces.")
    (license license:bsd-3)))

(define-public ghc-githash
  (package
    (name "ghc-githash")
    (version "0.1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "githash" version))
       (sha256
        (base32 "1m1hyfahvvsf46fy69zj27z4af0m9dlhc8i3qgjc9jfrdg1fgm8s"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "githash")))
    (inputs (list ghc-th-compat))
    (native-inputs (list ghc-hspec hspec-discover ghc-temporary ghc-unliftio
                         git-minimal/pinned))
    (home-page "https://github.com/snoyberg/githash#readme")
    (synopsis "Compile git revision info into Haskell projects")
    (description "Please see the README and documentation at
<https://www.stackage.org/package/githash>")
    (license license:bsd-3)))

(define-public ghc-git-lfs
  (package
    (name "ghc-git-lfs")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "git-lfs" version))
       (sha256
        (base32 "1lzlz9v05iabpkw09bgxxv4nj5yk7a0vzg6rx0kr0qrsir8c3jh2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "git-lfs")))
    (inputs (list ghc-http-client ghc-http-types ghc-aeson ghc-network-uri
                  ghc-case-insensitive))
    (home-page "http://hackage.haskell.org/package/git-lfs")
    (synopsis "Git Large File Storage protocol")
    (description "An implementation of the git-lfs protocol.")
    (license license:agpl3)))

(define-public ghc-nothunks
  (package
    (name "ghc-nothunks")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "nothunks" version))
              (sha256
               (base32
                "0pcpgv4pp0likra1rxyf70w48qn0nyqqghagym1x73j4zr9gk0rp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "nothunks")))
    (inputs (list ghc-vector ghc-wherefrom-compat))
    (native-inputs (list ghc-hedgehog ghc-random ghc-tasty ghc-tasty-hedgehog))
    (arguments
      (list
       ;#:tests? #f ; Fail to compile.
       #:cabal-revision
       '("1" "1qsabpyjcwkm75jh7pa6yv2aza0z50rpn0q27sxjxmhw1gbv0rja")))
    (home-page "https://hackage.haskell.org/package/nothunks")
    (synopsis "Examine values for unexpected thunks")
    (description
     "Long lived application data typically should not contain any thunks.  This
library can be used to examine values for unexpected thunks, which can then be
used in assertions.  This can be invaluable in avoiding memory leaks, or
tracking down existing ones.")
    (license license:expat)))

(define-public ghc-nothunks-bootstrap
  (package
    (inherit ghc-nothunks)
    (name "ghc-nothunks-bootstrap")
    (arguments `(#:tests? #f
                 ,@(package-arguments ghc-nothunks)))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-barbies
  (package
    (name "ghc-barbies")
    (version "2.1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "barbies" version))
       (sha256
        (base32 "1dyjsjal1ffdscm3y1wzrczlv56hpf50bwdmmvdfiy55ys9j15vk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "barbies")))
    (inputs (list ghc-distributive))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/jcpetruzza/barbies#readme")
    (synopsis "Classes for working with types that can change clothes")
    (description
     "Types that are parametric on a functor are like Barbies that have an outfit for
each role.  This package provides the basic abstractions to work with them
comfortably.")
    (license license:bsd-3)))

(define-public ghc-onetuple
  (package
    (name "ghc-onetuple")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "OneTuple" version))
       (sha256
        (base32 "0mdbga3a5pjzszlq9bny3zgfbz25w2q2bjw2h6q1fk80yjhahk8p"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "OneTuple")))
    (native-inputs (list ghc-hashable))
    (arguments
     `(#:cabal-revision ("1"
                         "1m54gsnc1bqy30vv3fab7axxmfns23cass11cvjp0afshgwncwnx")))
    (home-page "http://hackage.haskell.org/package/OneTuple")
    (synopsis "Singleton Tuple")
    (description
     "This package is a compatibility package for a singleton data type . > data Solo
a = Solo a .  Note: it's not a @@newtype@@ . @@Solo@@ is available in
@@base-4.16@@ (GHC-9.2).")
    (license license:bsd-3)))

(define-public ghc-indexed-traversable-instances
  (package
    (name "ghc-indexed-traversable-instances")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "indexed-traversable-instances" version))
       (sha256
        (base32 "1hf75x729c3348yvgxk0pjab2mmwi0xxcw3h2yb6c78lp8pvcarw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "indexed-traversable-instances")))
    (inputs (list ghc-indexed-traversable ghc-onetuple ghc-tagged
                  ghc-unordered-containers ghc-vector))
    (native-inputs (list ghc-quickcheck ghc-quickcheck-instances ghc-tasty
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "12fxhsx9ay2yqk5967kd389lqdpfkx2g3svgkrgw3qha0z40zpv8")))
    (home-page
     "http://hackage.haskell.org/package/indexed-traversable-instances")
    (synopsis
     "More instances of FunctorWithIndex, FoldableWithIndex, TraversableWithIndex")
    (description
     "This package provides extra instances for type-classes in the
[indexed-traversable](https://hackage.haskell.org/package/indexed-traversable)
package. .  The intention is to keep this package minimal; it provides instances
that formerly existed in @@lens@@ or @@optics-extra@@.  We recommend putting
other instances directly into their defining packages.  The
@@indexed-traversable@@ package is light, having only GHC boot libraries as its
dependencies.")
    (license license:bsd-2)))

(define-public ghc-wherefrom-compat
  (package
    (name "ghc-wherefrom-compat")
    (version "0.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wherefrom-compat" version))
       (sha256
        (base32 "125kkv55c38p00hcga7q19jk9p4p9nabal64zfkq79ihjwpf5qy1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wherefrom-compat")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "13dw2gim9cj9njng0k0rgl4cvas2digjr74h2iavkzjimzz0iijk")))
    (home-page "http://hackage.haskell.org/package/wherefrom-compat")
    (synopsis "A compatibility layer for GHC's 'wherefrom' function")
    (description
     "This package provides a compatibility layer for GHC's wherefrom function, which
exposes info provenance information.  Each major version of this library exports
a different version of this interface.")
    (license license:bsd-2)))

(define-public ghc-witherable
  (package
    (name "ghc-witherable")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "witherable" version))
       (sha256
        (base32 "1lccj7s6b2x3w1zq15fffwxwj9yqq0z0ra9k06y3rcb5wk0lqhs8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "witherable")))
    (inputs (list ghc-base-orphans
                  ghc-hashable
                  ghc-unordered-containers
                  ghc-vector
                  ghc-indexed-traversable
                  ghc-indexed-traversable-instances))
    (native-inputs (list ghc-quickcheck ghc-quickcheck-instances ghc-tasty
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1pl1m14hhz03rx5j4i4zdi9v0ph65s5rs6qpqdprjdyaxf4bbfl5")))
    (home-page "https://github.com/fumieval/witherable")
    (synopsis "Filterable traversable")
    (description
     "This package provides a stronger variant of `traverse` which can remove elements
and generalised mapMaybe, catMaybes, filter")
    (license license:bsd-3)))

(define-public ghc-hspec-discover
  (package
    (name "ghc-hspec-discover")
    (version "2.11.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-discover" version))
       (sha256
        (base32 "044vgsy45ff00h9z2k3jgn2m37npcjiacc4cifahrjlmwa7a7ylp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-discover")))
    (native-inputs (list ghc-quickcheck ghc-hspec-meta ghc-mockery))
    (home-page "https://hspec.github.io/")
    (synopsis "Automatically discover and run Hspec tests")
    (description "Automatically discover and run Hspec tests .
<http://hspec.github.io/hspec-discover.html>")
    (license license:expat)))

(define-public ghc-doctest-parallel
  (package
    (name "ghc-doctest-parallel")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "doctest-parallel" version))
       (sha256
        (base32 "1y907fg2y7ayddwv38rjv6nyc18w682dxwkq3msqnlkddglqlxfx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "doctest-parallel")))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (if tests?
                 (begin
                   (setenv "GHC_PACKAGE_PATH" (string-append (or (getenv "TMP")
                                                                 "/tmp")
                                                             "/package.conf.d"))
                   (invoke "./dist/build/spectests/spectests")
                   (unsetenv "GHC_PACKAGE_PATH"))
                 (format #t "Testsuite not run.~%")))))))
    (inputs (list ghc-glob
                  ghc-base-compat
                  ghc-code-page
                  ghc-exactprint
                  ghc-paths
                  ghc-random-bootstrap
                  ghc-syb
                  ghc-unordered-containers-bootstrap))
    (native-inputs (list ghc-hunit
                         ghc-quickcheck
                         ghc-hspec
                         ghc-hspec-core
                         ghc-mockery
                         ghc-setenv
                         ghc-silently
                         ghc-stringbuilder))
    (home-page "https://github.com/martijnbastiaan/doctest-parallel#readme")
    (synopsis "Test interactive Haskell examples")
    (description
     "The doctest program checks examples in source code comments.  It is modeled
after doctest for Python (<https://docs.python.org/3/library/doctest.html>). .
Documentation is at
<https://github.com/martijnbastiaan/doctest-parallel#readme>.")
    (license license:expat)))

(define-public ghc-pcg-random
  (package
    (name "ghc-pcg-random")
    (version "0.1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pcg-random" version))
       (sha256
        (base32 "09hnckb3xzb3spn79jvqlsbg05zm9r1l3dqq44ka07ik4zbagjbf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pcg-random")))
    (inputs (list ghc-primitive ghc-random ghc-entropy))
    (native-inputs (list ghc-doctest cabal-doctest))
    (arguments
     `(#:cabal-revision ("1"
                         "1f8h0lv34cmqaxccg2yf6q4s8r5g2s8q8s9kql212iggd2l3vv77")))
    (home-page "http://github.com/cchalmers/pcg-random")
    (synopsis "Haskell bindings to the PCG random number generator")
    (description
     "PCG is a family of simple fast space-efficient statistically good algorithms for
random number generation.  Unlike many general-purpose RNGs, they are also hard
to predict. .  This library implements bindings to the standard C
implementation.  This includes the standard, unique, fast and single variants in
the pcg family.  There is a pure implementation that can be used as a generator
with the random package as well as a faster primitive api that includes
functions for generating common types. .  The generators in this module are
suitable for use in parallel but make sure threads don't share the same
generator or things will go horribly wrong.")
    (license license:bsd-3)))

(define-public ghc-random-bytestring
  (package
    (name "ghc-random-bytestring")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "random-bytestring" version))
       (sha256
        (base32 "0f4n41gqxxggadysvx3vg2iq89z7i7692ccrfmiajq73lbp6y34j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "random-bytestring")))
    (inputs (list ghc-mwc-random ghc-nats ghc-pcg-random))
    (home-page "https://www.github.com/larskuhtz/random-bytestring")
    (synopsis "Efficient generation of random bytestrings")
    (description
     "__This package is deprecated__.  Please, use genByteString from the [random
package (version >=1.2)](https://hackage.haskell.org/package/random) instead. .
Efficient generation of random bytestrings.  The implementation populates
uninitialized memory with uniformily distributed random 64 bit words (and 8 bit
words for remaining bytes at the end of the bytestring). .  Random words are
generated using the PRNG from the
[mwc-random](https://hackage.haskell.org/package/mwc-random) package or the
[pcg-random](https://hackage.haskell.org/package/pcg-random) package.  It is
also possible to use a custom PRNG by providing an instance for the RandomWords
type class and using the function generate from the module
\"Data.ByteString.Random.Internal\". .  The generated byte strings are suitable
for statistical applications.  They are /not/ suitable for cryptographic
applications. .
![benchmarks](https://hackage.haskell.org/package/random-bytestring-0.1.3.2/src/benchmarks.png)
. ![detailed
benchmarks](https://hackage.haskell.org/package/random-bytestring-0.1.3.2/src/benchmarks-details.png)")
    (license license:expat)))

(define-public ghc-base64
  (package
    (name "ghc-base64")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "base64" version))
       (sha256
        (base32 "1dmjy4pkz66s3wa99lkc0wc4bdjkdkr57a8rsgb5z50432gj6hkr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "base64")))
    (inputs (list ghc-text-short))
    (native-inputs (list ghc-base64-bytestring
                         ghc-quickcheck
                         ghc-random-bytestring
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "1jp2pc5r4w0vmd2piidzh0h308gx7gdl1xxk9mndc381c8bvkj0m")))
    (home-page "https://github.com/emilypi/base64")
    (synopsis "Modern RFC 4648-compliant Base64 library")
    (description
     "RFC 4648-compliant Base64 with an eye towards performance and modernity
(additional support for RFC 7049 standards)")
    (license license:bsd-3)))

(define-public ghc-ordered-containers
  (package
    (name "ghc-ordered-containers")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ordered-containers" version))
       (sha256
        (base32 "0ip5msvvyj5zbsci6fv9f3x6p3wpwrrqpq4yhz104ag14hz6g89x"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ordered-containers")))
    (inputs (list ghc-hashable))
    (home-page "http://hackage.haskell.org/package/ordered-containers")
    (synopsis "Haskell types")
    (description "Set- and Map-like types that remember the order elements
were inserted")
    (license license:bsd-3)))

(define-public ghc-os-string
  (package
    (name "ghc-os-string")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "os-string" version))
       (sha256
        (base32 "094jqkfq09qapzm8qy7f1p6snnwval17h0fbsxnm9wg2rbncm2ln"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "os-string")))
    (native-inputs (list ghc-quickcheck ghc-quickcheck
                         ghc-quickcheck-classes-base))
    (home-page "https://github.com/haskell/os-string/blob/master/README.md")
    (synopsis "Library for manipulating Operating system strings.")
    (description
     "This package provides functionality for manipulating @code{@@OsString}@@ values,
and is shipped with <https://www.haskell.org/ghc/ GHC>.")
    (license license:bsd-3)))

(define-public ghc-cabal-install-solver
  (package
    (name "ghc-cabal-install-solver")
    (version "3.12.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cabal-install-solver" version))
       (sha256
        (base32 "0yjy49awzs79adx1xn705v3cpmqhvgpym49jaws9h0z9ag9s3nvi"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cabal-install-solver")))
    (inputs (list ghc-cabal-syntax ghc-edit-distance ghc-network-uri))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "0bai933mfjv0gazbbvvwaglzhgqnzdf3i1zl8r18cc9amwrh4rag")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'update-constraints
                    (lambda _
                      (substitute* "cabal-install-solver.cabal"
                        (("tasty-quickcheck <0.11") "tasty-quickcheck")))))))
    (home-page "https://www.haskell.org/cabal/")
    (synopsis
     "Solver component of command-line interface for Cabal and Hackage")
    (description
     "The solver component used in cabal-install command-line program.")
    (license license:bsd-3)))

(define-public ghc-cabal-syntax
  (package
    (name "ghc-cabal-syntax")
    (version "3.12.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Cabal-syntax" version))
       (sha256
        (base32 "1d045h470yykabz5d89896d6v2p1vng373s7bgysfdmzqhgd7wp5"))))
    (build-system haskell-build-system)
    (native-inputs (list ghc-alex))
    (properties '((upstream-name . "Cabal-syntax")))
    (home-page "http://www.haskell.org/cabal/")
    (synopsis "Library for working with .cabal files")
    (description
     "This library provides tools for reading and manipulating the .cabal file format.
.  Version 3.6 (unlike the following versions) is a dummy package that prevents
module name clases between Cabal and Cabal-syntax if used together with a Cabal
flag as described below. .  In Cabal-3.7 this package was split off.  To avoid
module name clashes, you can add this to your .cabal file: . > flag Cabal-syntax
> description: Use the new Cabal-syntax package > default: False > manual: False
> > library > -- ... > if flag(Cabal-syntax) > build-depends: Cabal-syntax >=
3.7 > else > build-depends: Cabal < 3.7, Cabal-syntax < 3.7 .  This will default
to the older build, but will allow consumers to opt-in to the newer libraries by
requiring Cabal or Cabal-syntax >= 3.7")
    (license license:bsd-3)))

(define-public ghc-tasty-hslua
  (package
    (name "ghc-tasty-hslua")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-hslua" version))
       (sha256
        (base32 "066q54kw3y3knxgxpkmhdspb7bdxkv0z68zi2r81sm9xsqg17a5b"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-hslua")))
    (inputs (list ghc-hslua-core ghc-tasty ghc-tasty-hunit))
    (home-page "https://hslua.org/")
    (synopsis "Tasty helpers to test HsLua")
    (description
     "Various tasty helpers and utilities to test HsLua oparations.  Built on top of
tasty-hunit.")
    (license license:expat)))

(define-public ghc-hslua-marshalling
  (package
    (name "ghc-hslua-marshalling")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-marshalling" version))
       (sha256
        (base32 "0v7hpzhj24zif89x4h2j3ji1ch7qifj7xh4r1rfvbsg48pzxjj89"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-marshalling")))
    (inputs (list ghc-hslua-core))
    (native-inputs (list ghc-lua-arbitrary
                         ghc-quickcheck
                         ghc-quickcheck-instances
                         ghc-tasty-hslua
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://hslua.org/")
    (synopsis "Marshalling of values between Haskell and Lua")
    (description
     "This package provides functions to marshal values from Haskell to Lua, and /vice
versa/. .  This package is part of HsLua, a Haskell framework built around the
embeddable scripting language <https://lua.org Lua>.")
    (license license:expat)))

(define-public ghc-lua-arbitrary
  (package
    (name "ghc-lua-arbitrary")
    (version "1.0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lua-arbitrary" version))
       (sha256
        (base32 "0kbvcgi54ycl8zfdkc80ap5yhz0dml9bjdgmzx9l9m4rkhyi9xnm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lua-arbitrary")))
    (inputs (list ghc-lua ghc-quickcheck))
    (home-page "https://hslua.org/")
    (synopsis "Arbitrary instances for Lua types")
    (description
     "This package provides instances for QuickCheck's \\\"Arbitrary\\\" typeclass.")
    (license license:expat)))

(define-public ghc-lua
  (package
    (name "ghc-lua")
    (version "2.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lua" version))
       (sha256
        (base32 "0xvhfq8ms5wbchrscxaqf4a9panfnzgz5xdlg86790nydab2kals"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lua")))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://hslua.org/")
    (synopsis "Lua, an embeddable scripting language")
    (description
     "This package provides bindings and types to bridge Haskell and
<https://www.lua.org/ Lua>. .  The full Lua interpreter version 5.4.4 is
included.  Alternatively, a system-wide Lua installation can be linked instead.")
    (license license:expat)))

(define-public ghc-hslua-core
  (package
    (name "ghc-hslua-core")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-core" version))
       (sha256
        (base32 "0h3d2r5wkbz0d2gylmc282mn0c7b7bfglmchr5hs7vq20206zv0l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-core")))
    (inputs (list ghc-lua))
    (native-inputs (list ghc-lua-arbitrary
                         ghc-quickcheck
                         ghc-quickcheck-instances
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://hslua.org/")
    (synopsis "Bindings to Lua, an embeddable scripting language")
    (description
     "Wrappers and helpers to bridge Haskell and <https://www.lua.org/ Lua>. .  It
builds upon the /lua/ package, which allows bundling a Lua interpreter with a
Haskell program.")
    (license license:expat)))

(define-public ghc-hslua-aeson
  (package
    (name "ghc-hslua-aeson")
    (version "2.3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-aeson" version))
       (sha256
        (base32 "09a5xbl8ib79vln01naw9cd5g100arfx5nm41n0xbk272cjf7cgg"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-aeson")))
    (inputs (list ghc-aeson
                  ghc-hashable
                  ghc-hslua-core
                  ghc-hslua-marshalling
                  ghc-scientific
                  ghc-unordered-containers
                  ghc-vector))
    (native-inputs (list ghc-quickcheck ghc-quickcheck-instances ghc-tasty
                         ghc-tasty-quickcheck ghc-tasty-hunit))
    (home-page "https://hslua.org/")
    (synopsis "Allow aeson data types to be used with Lua")
    (description
     "This package provides instances to push and receive any datatype encodable as
JSON to and from the Lua stack.")
    (license license:expat)))

(define-public ghc-gridtables
  (package
    (name "ghc-gridtables")
    (version "0.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "gridtables" version))
       (sha256
        (base32 "1smhbb2jxysbqhxww5rikjfnhsdbf0gq3kgnn6ikjzcrqwdk9b6n"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "gridtables")))
    (inputs (list ghc-doclayout))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("1"
                         "0ay4ywi8w5kk4blf8jqxhivzprp1ivpdlw6racr1692psyizmxi0")))
    (home-page "https://github.com/tarleb/gridtables")
    (synopsis "Parser for reStructuredText-style grid tables")
    (description
     "This package provides a parser for plain-text representations of tables.  This
package supports table headers, cells spanning multiple columns or rows, as well
as a way to specify column alignments.")
    (license license:expat)))

(define-public ghc-lpeg
  (package
    (name "ghc-lpeg")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lpeg" version))
       (sha256
        (base32 "0yav34yxrkbgnkcd3870smay5s3cypyd28m0fsg2jhlikgmhj5a1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lpeg")))
    (inputs (list ghc-lua))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://hslua.org/")
    (synopsis "LPeg â Parsing Expression Grammars For Lua")
    (description
     "This package contains the C sources of LPeg, as well as some tiny Haskell helper
to load the package. . <http://www.inf.puc-rio.br/~roberto/lpeg/>")
    (license license:expat)))

(define-public ghc-pandoc-lua-marshal
  (package
    (name "ghc-pandoc-lua-marshal")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pandoc-lua-marshal" version))
       (sha256
        (base32 "0869amr9w5s90dha694vy6rwfni7p1wp9dyjyyk2jvh8h22gcpr0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pandoc-lua-marshal")))
    (inputs (list ghc-aeson
                  ghc-hslua
                  ghc-hslua-list
                  ghc-hslua-marshalling
                  ghc-pandoc-types
                  ghc-safe))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-hunit
                         ghc-tasty-lua ghc-tasty-quickcheck))
    (home-page "https://github.com/pandoc/pandoc-lua-marshal")
    (synopsis "Use pandoc types in Lua")
    (description
     "This package provides functions to marshal and unmarshal pandoc document types
to and from Lua. .  The values of most types are pushed to pandoc as \"userdata\"
objects that wrap a stable pointer to the Haskell value; these objects come with
methods to access and modify their properties. .  Sequences are pushed as normal
Lua tables, but are augmented with convenience functions.")
    (license license:expat)))

(define-public ghc-should-not-typecheck
  (package
    (name "ghc-should-not-typecheck")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "should-not-typecheck" version))
       (sha256
        (base32 "14fmv0mv2v4fqzynamlrmdj6d1l65aw1srf1wv19nrq7rrqaqf7m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "should-not-typecheck")))
    (inputs (list ghc-hunit))
    (native-inputs (list ghc-hspec ghc-hspec-expectations))
    (home-page "http://github.com/CRogers/should-not-typecheck")
    (synopsis
     "HUnit/hspec assertion library to verify that an expression does not typecheck")
    (description
     "For examples and an introduction to the library please take a look at the
<https://github.com/CRogers/should-not-typecheck#should-not-typecheck- README>
on github.")
    (license license:bsd-3)))

(define-public ghc-hspec-wai
  (package
    (name "ghc-hspec-wai")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-wai" version))
       (sha256
        (base32 "03wiksic5y9a2g6a86nsxrnajdgdvpv17w02h5qla0zp9zs6pa1j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-wai")))
    (inputs (list ghc-quickcheck
                  ghc-base-compat
                  ghc-case-insensitive
                  ghc-hspec-core
                  ghc-hspec-expectations
                  ghc-http-types
                  ghc-wai
                  ghc-wai-extra))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "https://github.com/hspec/hspec-wai#readme")
    (synopsis "Experimental Hspec support for testing WAI applications")
    (description "Experimental Hspec support for testing WAI applications")
    (license license:expat)))

(define-public ghc-http-media
  (package
    (name "ghc-http-media")
    (version "0.8.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "http-media" version))
       (sha256
        (base32 "10zi0c6v0vngxadlbzfp1y1fgyx1ac5gvyvl0gv6gl8m3grss80r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-media")))
    (inputs (list ghc-case-insensitive ghc-utf8-string))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-quickcheck))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'configure 'relax-depency-versions
             (lambda _
               (substitute* "http-media.cabal"
                 (("tasty-quickcheck [<>=0-9. &|]*") "tasty-quickcheck")))))
       #:cabal-revision '("2"
                          "12bj8xqpvaxvrnif4qzkjamdxdrlg2hsfqa7q5n2irzr2hpkjni0")))
    (home-page "https://github.com/zmthy/http-media")
    (synopsis "Processing HTTP Content-Type and Accept headers")
    (description
     "This library is intended to be a comprehensive solution to parsing and selecting
quality-indexed values in HTTP headers.  It is capable of parsing both media
types and language parameters from the Accept and Content header families, and
can be extended to match against other accept headers as well.  Selecting the
appropriate header value is achieved by comparing a list of server options
against the quality-indexed values supplied by the client. .  In the following
example, the Accept header is parsed and then matched against a list of server
options to serve the appropriate media using mapAcceptMedia': . > getHeader >>=
maybe send406Error sendResourceWith .  mapAcceptMedia > [ (\"text/html\", asHtml)
> , (\"application/json\", asJson) > ] .  Similarly, the Content-Type header can
be used to produce a parser for request bodies based on the given content type
with mapContentMedia': . > getContentType >>= maybe send415Error
readRequestBodyWith .  mapContentMedia > [ (\"application/json\", parseJson) > ,
(\"text/plain\", parseText) > ] .  The API is agnostic to your choice of server.")
    (license license:expat)))

(define-public ghc-servant
  (package
    (name "ghc-servant")
    (version "0.20.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "servant" version))
       (sha256
        (base32 "00k6pwqxpyjp5qm5pjl8qb75iqmpql5iv3ac43xdvikcixffcwzj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "servant")))
    (inputs (list ghc-constraints
                  ghc-sop-core
                  ghc-generics-sop
                  ghc-http-api-data
                  ghc-singleton-bool
                  ghc-aeson
                  ghc-attoparsec
                  ghc-bifunctors
                  ghc-case-insensitive
                  ghc-http-media
                  ghc-http-types
                  ghc-mmorph
                  ghc-network-uri
                  ghc-quickcheck
                  ghc-vault))
    (native-inputs (list ghc-hspec ghc-quickcheck-instances hspec-discover))
    (arguments
     `(#:cabal-revision ("2"
                         "0wvq6jj6js7sxq1rrn4v6749zfwkz3cl8dsypf5cvbpkz1qp4d7j")))
    (home-page "http://docs.servant.dev/")
    (synopsis "Family of combinators for defining webservices APIs")
    (description
     "This package provides a family of combinators for defining webservices APIs and
serving them .  You can learn about the basics in the
<http://docs.servant.dev/en/stable/tutorial/index.html tutorial>. .
<https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md
CHANGELOG>")
    (license license:bsd-3)))

(define-public ghc-servant-client
  (package
    (name "ghc-servant-client")
    (version "0.20.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "servant-client" version))
       (sha256
        (base32 "0kxmixgv5nmir2bk3zfrhaal4969rf414wi2ccnngjm3395bqrwn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "servant-client")))
    (inputs (list ghc-servant
                  ghc-servant-client-core
                  ghc-base-compat
                  ghc-http-client
                  ghc-http-media
                  ghc-http-types
                  ghc-kan-extensions
                  ghc-monad-control
                  ghc-semigroupoids
                  ghc-transformers-base))
    (native-inputs (list ghc-aeson
                         ghc-http-api-data
                         ghc-sop-core
                         ghc-generics-sop
                         ghc-wai
                         ghc-warp
                         ghc-entropy
                         ghc-hspec
                         ghc-hunit
                         ghc-network
                         ghc-quickcheck
                         ghc-servant-server
                         ghc-markdown-unlit
                         hspec-discover))
    (arguments
     `(#:cabal-revision ("2"
                         "01if9an74258ri4sg91z64f200wl5z9i368ngc25wcgqkzpda3xd")))
    (home-page "http://docs.servant.dev/")
    (synopsis "Automatic derivation of querying functions for servant")
    (description
     "This library lets you derive automatically Haskell functions that let you query
each endpoint of a <http://hackage.haskell.org/package/servant servant>
webservice. .  See <http://docs.servant.dev/en/stable/tutorial/Client.html the
client section of the tutorial>. .
<https://github.com/haskell-servant/servant/blob/master/servant-client/CHANGELOG.md
CHANGELOG>.")
    (license license:bsd-3)))

(define-public ghc-servant-client-core
  (package
    (name "ghc-servant-client-core")
    (version "0.20.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "servant-client-core" version))
       (sha256
        (base32 "1vv6xf340hyk60vv6jb1zxfpsb7x2ykacb84yrn3h1w4k075hlyn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "servant-client-core")))
    (inputs (list ghc-attoparsec
                  ghc-constraints
                  ghc-servant
                  ghc-aeson
                  ghc-base-compat
                  ghc-base64-bytestring
                  ghc-free
                  ghc-http-media
                  ghc-http-types
                  ghc-network-uri
                  ghc-safe
                  ghc-sop-core))
    (native-inputs (list hspec-discover ghc-hspec ghc-quickcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "1gnnbybxpvvc82p45iqqiggrw545m6qmkgwfnv18rv83p5lqzcv0")))
    (home-page "http://docs.servant.dev/")
    (synopsis
     "Core functionality and class for client function generation for servant APIs")
    (description
     "This library provides backend-agnostic generation of client functions.  For more
information, see the README.")
    (license license:bsd-3)))

(define-public ghc-servant-server
  (package
    (name "ghc-servant-server")
    (version "0.20.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "servant-server" version))
       (sha256
        (base32 "05crwklbncd393zq00gi04zgnfyy2wk31s0xf5hy6yjrsbshlmih"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "servant-server")))
    (inputs (list ghc-constraints
                  ghc-http-api-data
                  ghc-servant
                  ghc-base64-bytestring
                  ghc-http-media
                  ghc-http-types
                  ghc-monad-control
                  ghc-network
                  ghc-resourcet
                  ghc-sop-core
                  ghc-tagged
                  ghc-transformers-base
                  ghc-wai
                  ghc-wai-app-static
                  ghc-word8
                  ghc-base-compat
                  ghc-aeson
                  ghc-warp))
    (native-inputs (list ghc-safe
                         ghc-hspec
                         ghc-hspec-wai
                         hspec-discover
                         ghc-should-not-typecheck
                         ghc-temporary
                         ghc-wai-extra))
    (arguments
     `(#:cabal-revision ("1"
                         "1z2h1gmxphwd76chyah405ww4ciyxq7rvggghr6lh0z1m3p2k90h")))
    (home-page "http://docs.servant.dev/")
    (synopsis
     "Family of combinators for defining webservices APIs and serving them")
    (description
     "This package provides a family of combinators for defining webservices APIs and
serving them .  You can learn about the basics in the
<http://docs.servant.dev/en/stable/tutorial/index.html tutorial>. .
<https://github.com/haskell-servant/servant/blob/master/servant-server/example/greet.hs
Here> is a runnable example, with comments, that defines a dummy API and
implements a webserver that serves this API, using this package. .
<https://github.com/haskell-servant/servant/blob/master/servant-server/CHANGELOG.md
CHANGELOG>")
    (license license:bsd-3)))

(define-public ghc-boring
  (package
    (name "ghc-boring")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "boring" version))
       (sha256
        (base32 "11pgndkjvy2j0jfaww92nmlkn2r27v6a253hzdc7dcb9zwgsz2wj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "boring")))
    (inputs (list ghc-tagged))
    (arguments
     `(#:cabal-revision ("1"
                         "139fba0i0ksh9chy0c86cv9v8nldwd62rw8h4a33g8rh131hbmzn")))
    (home-page "https://github.com/phadej/boring")
    (synopsis "Boring and Absurd types")
    (description
     "* @@Boring@@ types are isomorphic to @@()@@. . * @@Absurd@@ types are isomorphic
to @@Void@@. .  See [What does () mean in Haskell -answer by Conor
McBride](https://stackoverflow.com/questions/33112439/what-does-mean-in-haskell/33115522#33115522)")
    (license license:bsd-3)))

(define-public ghc-some
  (package
    (name "ghc-some")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "some" version))
       (sha256
        (base32 "1fdzhi2rmcigb1c727dyzfak8rgb77bzfr33k1cp987lbnnhd9pp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "some")))
    (arguments
     `(#:cabal-revision ("2"
                         "1w4xi6k44bjyrvhq70550fwrvqfybrq747aws708q18zsbriandc")))
    (home-page "https://github.com/haskellari/some")
    (synopsis "Existential type: Some")
    (description
     "This library defines an existential type Some'. . @@ data Some f where \\ Some ::
f a -> Some f @@ .  in few variants, and utilities to work with it. .  If you
are unsure which variant to use, use the one in \"Data.Some\" module.")
    (license license:bsd-3)))

(define-public ghc-hslua-classes
  (package
    (name "ghc-hslua-classes")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-classes" version))
       (sha256
        (base32 "185lynrinz1y38346b80jx8ag77ka53sg606wdlgzsqrx7rk66kq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-classes")))
    (inputs (list ghc-hslua-core ghc-hslua-marshalling))
    (native-inputs (list ghc-lua-arbitrary
                         ghc-quickcheck
                         ghc-quickcheck-instances
                         ghc-tasty
                         ghc-tasty-hslua
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://hslua.org/")
    (synopsis "Type classes for HsLua")
    (description
     "Type classes for convenient marshalling and calling of Lua functions.")
    (license license:expat)))

(define-public ghc-hslua-cli
  (package
    (name "ghc-hslua-cli")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-cli" version))
       (sha256
        (base32 "1sn8mc18lqn8c0pm7rms623p49vpmmhbp07bh561x6q96fgjcwzm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-cli")))
    (inputs (list ghc-hslua-core ghc-hslua-marshalling ghc-hslua-repl ghc-lua))
    (home-page "https://hslua.org/")
    (synopsis "Command-line interface for Lua")
    (description
     "This package provides an embeddable command-line interface for Lua.  The
interface is compatible with the standard Lua interpreter, i.e., the `lua`
executable provided in a default Lua installation.")
    (license license:expat)))

(define-public ghc-hslua-module-zip
  (package
    (name "ghc-hslua-module-zip")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-module-zip" version))
       (sha256
        (base32 "1ij2rmy8m4pw7k7w5vvb3g934kms60vhzhhp8kryknbi6bsg8lsy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-module-zip")))
    (inputs (list ghc-hslua-core
                  ghc-hslua-packaging
                  ghc-hslua-list
                  ghc-hslua-marshalling
                  ghc-hslua-typing
                  ghc-zip-archive))
    (native-inputs (list ghc-hslua-module-system ghc-tasty ghc-tasty-hunit
                         ghc-tasty-lua))
    (home-page "https://hslua.org/")
    (synopsis "Lua module to work with file zips.")
    (description
     "Module with function for creating, modifying, and extracting files from zip
archives.")
    (license license:expat)))

(define-public ghc-hslua-objectorientation
  (package
    (name "ghc-hslua-objectorientation")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-objectorientation" version))
       (sha256
        (base32 "1avxiqcr2k4wdi3da1h4qwis589xvvdz0abggcklbigjc08vf90q"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-objectorientation")))
    (inputs (list ghc-hslua-core ghc-hslua-marshalling ghc-hslua-typing))
    (native-inputs (list ghc-lua-arbitrary
                         ghc-quickcheck
                         ghc-quickcheck-instances
                         ghc-tasty
                         ghc-tasty-hslua
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://hslua.org/")
    (synopsis "Object orientation tools for HsLua")
    (description
     "Expose Haskell objects to Lua with an object oriented interface.")
    (license license:expat)))

(define-public ghc-hslua-packaging
  (package
    (name "ghc-hslua-packaging")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-packaging" version))
       (sha256
        (base32 "0wr1az0mq0q4xk0x4an0sxsnnjvpcfhcgqdlmp23yylzkbbaxp1n"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-packaging")))
    (inputs (list ghc-hslua-core ghc-hslua-marshalling
                  ghc-hslua-objectorientation ghc-hslua-typing))
    (native-inputs (list ghc-tasty-hslua ghc-tasty ghc-tasty-hunit))
    (home-page "https://hslua.org/")
    (synopsis "Utilities to build Lua modules")
    (description
     "Utilities to package up Haskell functions and values into a Lua module. .  This
package is part of HsLua, a Haskell framework built around the embeddable
scripting language <https://lua.org Lua>.")
    (license license:expat)))

(define-public ghc-hslua-module-version
  (package
    (name "ghc-hslua-module-version")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-module-version" version))
       (sha256
        (base32 "0h0a4gk17bi7hh34yh5dva7zz1pyc5b8lm8kij5ri3jnsm259r29"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-module-version")))
    (inputs (list ghc-hslua-core ghc-hslua-marshalling ghc-hslua-packaging))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-lua))
    (home-page "https://hslua.org/")
    (synopsis "Lua module to work with version specifiers")
    (description "Wrapper for the Data.Version.Version Haskell type.")
    (license license:expat)))

(define-public ghc-recv
  (package
    (name "ghc-recv")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "recv" version))
       (sha256
        (base32 "08dp76haq3yb1bzxvg8hv0p060c07a0w7vrq63fh75ajgz2pgbwd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "recv")))
    (inputs (list ghc-network))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "http://github.com/yesodweb/wai")
    (synopsis "Efficient network recv")
    (description "Network recv based on buffer pools")
    (license license:bsd-3)))

(define-public ghc-glib
  (package
    (name "ghc-glib")
    (version "0.13.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "glib" version))
       (sha256
        (base32 "03b9s9fzg17mh4dcxmgnb43a0r5237gh91x6blpwawynrvi9bmmg"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "glib")))
    (inputs (list ghc-utf8-string glib))
    (native-inputs (list ghc-gtk2hs-buildtools pkg-config))
    (home-page "https://projects.haskell.org/gtk2hs/")
    (synopsis "GLib bindings for for Gtk2Hs")
    (description
     "GLib is a collection of C data structures and utility functions for the GObject
system, main loop implementation, for strings and common data structures dealing
with Unicode.  This package only binds as much functionality as required to
support the packages that wrap libraries that are themselves based on GLib.")
    (license license:lgpl2.1)))

(define-public ghc-pango
  (package
    (name "ghc-pango")
    (version "0.13.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pango" version))
       (sha256
        (base32 "0ahrx81picvm6mbv1vdgk19l6yyzi75z0y1shkbvc9zs23czlz62"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pango")))
    (inputs (list ghc-glib ghc-cairo pango))
    (native-inputs (list ghc-gtk2hs-buildtools pkg-config))
    (home-page "http://projects.haskell.org/gtk2hs/")
    (synopsis "Haskell bindings to the Pango text rendering engine")
    (description
     "This package provides a wrapper around the Pango C library that allows
high-quality rendering of Unicode text.  It can be used either with Cairo to
output text in PDF, PS or other documents or with Gtk+ to display text
on-screen.")
    (license license:lgpl2.1)))

(define-public ghc-monoidal-containers
  (package
    (name "ghc-monoidal-containers")
    (version "0.6.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "monoidal-containers" version))
       (sha256
        (base32 "0i2hc4x1y6437az5cg1rg8p57m1m6k742h5vbdw0vr4hrq2ldsqi"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "monoidal-containers")))
    (inputs (list ghc-aeson
                  ghc-hashable
                  ghc-lens
                  ghc-newtype
                  ghc-unordered-containers
                  ghc-witherable
                  ghc-semialign
                  ghc-these))
    (home-page "http://github.com/bgamari/monoidal-containers")
    (synopsis "Containers with monoidal accumulation")
    (description
     "Containers with merging via monoidal accumulation.  The Monoid instances
provided by the @code{containers} and @code{unordered-containers} packages merge
structures in a left-biased manner instead of using the underlying monoidal
structure of the value.  This package wraps the types provided by these
packages, but provides @code{Monoid} instances implemented in terms of the value
type's mappend'.")
    (license license:bsd-3)))

(define-public ghc-newtype
  (package
    (name "ghc-newtype")
    (version "0.2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "newtype" version))
       (sha256
        (base32 "1b7bamnd0p8vmxvlg39g5d4a2av49kx10rdyz04ixa28pg8zy01s"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "newtype")))
    (arguments
     `(#:cabal-revision ("3"
                         "0yll88ydchd2gqcvdk28fchf2vygpd42ky2bigg4ga08jan2nacx")))
    (home-page "http://hackage.haskell.org/package/newtype")
    (synopsis "Typeclass and set of functions for working with newtypes")
    (description
     "Per Conor McBride, the Newtype typeclass represents the packing and unpacking of
a @code{newtype}, and allows you to operate under that @code{newtype} with functions
such as ala'.")
    (license license:bsd-3)))

(define-public ghc-hspec-hedgehog
  (package
    (name "ghc-hspec-hedgehog")
    (version "0.3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-hedgehog" version))
       (sha256
        (base32 "1px71jwxvqdh837fvlmx4smcvci9bbkygb7n10vasfpyb2k0yhzy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-hedgehog")))
    (inputs (list ghc-quickcheck ghc-hedgehog ghc-hspec ghc-hspec-core
                  ghc-splitmix))
    (native-inputs (list ghc-hunit hspec-discover))
    (home-page "https://github.com/hspec/hspec-hedgehog#readme")
    (synopsis "Integrate Hedgehog and Hspec")
    (description "An integration library for hspec and hedgehog.")
    (license license:bsd-3)))

(define-public ghc-validation-selective
  (package
    (name "ghc-validation-selective")
    (version "0.2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "validation-selective" version))
       (sha256
        (base32 "11s9qsp5w19lbk6vg6psr3864xdsx2kmx3gcmnn2qkx6wsblx24s"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "validation-selective")))
    (inputs (list ghc-selective))
    (native-inputs (list ghc-hedgehog ghc-hspec ghc-hspec-hedgehog ghc-doctest))
    (arguments
     `(#:cabal-revision ("5"
                         "05lksfm5micvk2s6isscjf6ipkwd79698cczlr0ipgn8wmm87drz")))
    (home-page "https://github.com/kowainik/validation-selective")
    (synopsis "Data validation based on Applicative and Selective functors")
    (description
     "Lightweight pure data validation based on Applicative and Selective functors.")
    (license license:mpl2.0)))

(define-public ghc-vcr
  (package
    (name "ghc-vcr")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vcr" version))
       (sha256
        (base32 "1s6gp1m84izlsvw5z7ll39mw2r456xmbh7cx53f8gkwl2m2pyyrq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "vcr")))
    (inputs (list ghc-hunit
                  ghc-async
                  ghc-case-insensitive
                  ghc-http-client
                  ghc-http-types
                  ghc-network-uri
                  ghc-yaml))
    (native-inputs (list hspec-discover ghc-hspec ghc-http-client-tls ghc-http-conduit
                         ghc-mockery))
    (home-page "https://github.com/assertible/vcr")
    (synopsis "Record and replay HTTP interactions")
    (description
     "Haskell library for recording and replaying HTTP interactions, making your
tests fast, deterministic, and free from real network dependencies.")
    (license license:expat)))

(define-public ghc-tdigest
  (package
    (name "ghc-tdigest")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tdigest" version))
       (sha256
        (base32 "1v3j0041hjhvnwr77r1i1flvl9b1n8z0d2g83b63fr97kyl2fc8r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tdigest")))
    (inputs (list ghc-reducers ghc-vector ghc-vector-algorithms))
    (native-inputs (list ghc-semigroups ghc-tasty ghc-tasty-quickcheck))
    (home-page "https://github.com/phadej/haskell-tdigest#readme")
    (synopsis "On-line accumulation of rank-based statistics")
    (description
     "This package provides a new data structure for accurate on-line accumulation of
rank-based statistics such as quantiles and trimmed means. .  See original
paper: \"Computing extremely accurate quantiles using t-digest\" by Ted Dunning
and Otmar Ertl for more details
<https://github.com/tdunning/t-digest/blob/07b8f2ca2be8d0a9f04df2feadad5ddc1bb73c88/docs/t-digest-paper/histo.pdf>.")
    (license license:bsd-3)))

(define-public ghc-tomland
  (package
    (name "ghc-tomland")
    (version "1.3.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tomland" version))
       (sha256
        (base32 "1asnz773mrbg8fkfabq5w24v63sgqljspc4p4nmf4dm6abm2p6d0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tomland")))
    (inputs (list ghc-hashable ghc-megaparsec ghc-parser-combinators
                  ghc-unordered-containers ghc-validation-selective))
    (native-inputs (list ghc-hedgehog ghc-hspec ghc-hspec-hedgehog
                         ghc-hspec-megaparsec))
    (arguments
     `(#:cabal-revision ("3"
                         "0aclzlwr7xmjzda327vzfb8av90g3lpcln1h1gkw76x5w7xq662s")))
    (home-page "https://github.com/kowainik/tomland")
    (synopsis "Bidirectional TOML serialization")
    (description "Implementation of bidirectional TOML serialization.")
    (license license:mpl2.0)))

(define-public ghc-toml-parser
  (package
    (name "ghc-toml-parser")
    (version "2.0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "toml-parser" version))
       (sha256
        (base32 "0fm3anvslylamazr4jgm3y3v3sjh0jv5ydf565cfm1ma9kw4kbhv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "toml-parser")))
    (inputs (list ghc-prettyprinter))
    (native-inputs (list ghc-alex ghc-happy ghc-hspec hspec-discover
                         ghc-markdown-unlit))
    (arguments
     `(#:cabal-revision ("1"
                         "0p6h6yh2x93bgspan1s5hkwg10s834m5gkx1bha9y3ljppffpg2c")))
    (home-page "https://github.com/glguy/toml-parser")
    (synopsis "TOML 1.0.0 parser")
    (description "TOML parser using generated lexers and parsers with careful
attention to the TOML 1.0.0 semantics for defining tables.")
    (license license:isc)))

(define-public ghc-hslua-list
  (package
    (name "ghc-hslua-list")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-list" version))
       (sha256
        (base32 "0lyrk95nnsdwi3zfaf4blvih49002wy5hxjj7l49ryc93padkvyk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-list")))
    (inputs (list ghc-hslua-core))
    (native-inputs (list ghc-tasty ghc-tasty-lua))
    (home-page "https://hslua.org/")
    (synopsis "Opinionated, but extensible Lua list type.")
    (description "List type for Lua, with a Haskell interface.")
    (license license:expat)))

(define-public ghc-hslua-module-doclayout
  (package
    (name "ghc-hslua-module-doclayout")
    (version "1.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-module-doclayout" version))
       (sha256
        (base32 "139l4sh9pllm0zjgv3w7scbpd0cgn23r95fdlchavsdfwkpvcx17"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-module-doclayout")))
    (inputs (list ghc-doclayout ghc-hslua))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-tasty-lua))
    (home-page "https://github.com/hslua/hslua-module-doclayout")
    (synopsis "Lua module wrapping Text.DocLayout")
    (description "Lua module wrapping @code{Text.DocLayout}.")
    (license license:expat)))

(define-public ghc-hslua-repl
  (package
    (name "ghc-hslua-repl")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-repl" version))
       (sha256
        (base32 "00n624vs0509sy8lmnid97nfmlwpi60wzibpkjsj5nbmp0xcsi42"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-repl")))
    (inputs (list ghc-hslua-core ghc-isocline ghc-lua))
    (home-page "https://hslua.org/")
    (synopsis "Isocline-based Lua REPL")
    (description
     "An embeddable Lua REPL built with Isocline and @code{HsLua}.")
    (license license:expat)))

(define-public ghc-hslua-typing
  (package
    (name "ghc-hslua-typing")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hslua-typing" version))
       (sha256
        (base32 "0k09g97ysi5db6a3rdfj2j6wsb12dbhvnbcgqvy686mpa6rwg6j4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hslua-typing")))
    (inputs (list ghc-hslua-core ghc-hslua-marshalling))
    (native-inputs (list ghc-quickcheck
                         ghc-lua-arbitrary
                         ghc-quickcheck-instances
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://hslua.org/")
    (synopsis "Type specifiers for Lua.")
    (description
     "Structure to hold detailed type information.  The primary use-case at this time
are auto-generated docs.")
    (license license:expat)))

(define-public ghc-random-shuffle
  (package
    (name "ghc-random-shuffle")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "random-shuffle" version))
       (sha256
        (base32 "0586bnlh0g2isc44jbjvafkcl4yw6lp1db8x6vr0pza0y08l8w2j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "random-shuffle")))
    (inputs (list ghc-random ghc-monadrandom))
    (home-page "http://hackage.haskell.org/package/random-shuffle")
    (synopsis "Random shuffle implementation")
    (description
     "Random shuffle implementation, on immutable lists.  Based on
@url{http://okmij.org/ftp/Haskell/perfect-shuffle.txt, perfect shuffle
implementation by Oleg Kiselyov}.")
    (license license:bsd-3)))

(define-public ghc-deriving-aeson
  (package
    (name "ghc-deriving-aeson")
    (version "0.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "deriving-aeson" version))
       (sha256
        (base32 "0xrny17zfsfjrl6042na32q7msm939p3ns6x7iw47xc21an55yrs"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "deriving-aeson")))
    (inputs (list ghc-aeson))
    (home-page "http://hackage.haskell.org/package/deriving-aeson")
    (synopsis "Type driven generic aeson instance customisation")
    (description
     "This package provides a newtype wrapper with FromJSON/ToJSON instances
customisable via a phantom type parameter.  The instances can be rendered to the
original type using DerivingVia.")
    (license license:bsd-3)))

(define-public ghc-leancheck
  (package
    (name "ghc-leancheck")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "leancheck" version))
       (sha256
        (base32 "15fg0bnrh7apla4y2c47gxb4jrkrvrcb3swrl3mva2lymnnxzhbd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "leancheck")))
    (home-page "https://github.com/rudymatela/leancheck#readme")
    (synopsis "Enumerative property-based testing")
    (description
     "LeanCheck is a simple enumerative property-based testing library.  Properties
are defined as Haskell functions returning a boolean value which should be true
for all possible choices of argument values.  LeanCheck applies enumerated
argument values to these properties in search for a counterexample.  Properties
can be viewed as parameterized unit tests.  LeanCheck works by producing tiers
of test values: a possibly infinite list of finite sublists of
same-and-increasingly-sized values.")
    (license license:bsd-3)))

(define-public ghc-test-framework-leancheck
  (package
    (name "ghc-test-framework-leancheck")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "test-framework-leancheck" version))
       (sha256
        (base32 "0aa21r999jj59plzkn1px02k3a87znwhagdjmdsik2xvy5wrzgzv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "test-framework-leancheck")))
    (inputs (list ghc-test-framework ghc-leancheck))
    (home-page "https://github.com/rudymatela/test-framework-leancheck#readme")
    (synopsis "LeanCheck support for test-framework")
    (description
     "LeanCheck support for @code{test-framework}.  This package can be used
to incorporate LeanCheck tests into test-framework test suites.")
    (license license:bsd-3)))

(define-public ghc-prim-uniq
  (package
    (name "ghc-prim-uniq")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "prim-uniq" version))
       (sha256
        (base32 "1l7jlv3pfasn89n2wpgff972npy423vqsidkkn5crxfyqjyzxbdv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "prim-uniq")))
    (inputs (list ghc-dependent-sum ghc-primitive))
    (home-page "https://github.com/obsidiansystems/prim-uniq")
    (synopsis "Opaque unique identifiers in primitive state monads")
    (description
     "This library provides opaque unique identifiers in primitive state
monads and a GADT-like type using them as witnesses of type equality.")
    (license license:public-domain)))

(define-public ghc-patch
  (package
    (name "ghc-patch")
    (version "0.0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "patch" version))
       (sha256
        (base32 "149rbr0yy22m2rbvq68bmq33nrczji27f328r9zyc7gklhry55sv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "patch")))
    (inputs (list ghc-constraints-extras
                  ghc-commutative-semigroups
                  ghc-dependent-map
                  ghc-dependent-sum
                  ghc-lens
                  ghc-indexed-traversable
                  ghc-semigroupoids
                  ghc-witherable
                  ghc-base-orphans
                  ghc-these
                  ghc-semialign
                  ghc-monoidal-containers))
    (native-inputs (list ghc-hedgehog ghc-hunit ghc-filemanip))
    (home-page "https://obsidian.systems")
    (synopsis
     "Data structures for describing changes to other data structures")
    (description
     "This library provides data structures for describing changes to other
data structures.  In this library, a patch is something that can be applied,
analogous to a function, and which distinguishes returning the argument it was
provided from returning something else.")
    (license license:bsd-3)))

(define-public ghc-ref-tf
  (package
    (name "ghc-ref-tf")
    (version "0.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ref-tf" version))
       (sha256
        (base32 "0isilgcbw12zyh8s2liaj5r9r5m3yg1xskyhag6f36qi60y29hx5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ref-tf")))
    (home-page "http://hackage.haskell.org/package/ref-tf")
    (synopsis "Type class for monads with references using type families")
    (description
     "This package contains a @code{MonadRef} type class that abstracts over
the details of manipulating references, allowing one to write code that can
operate in either the @code{ST} monad or the @code{IO} monad.")
    (license license:bsd-3)))

(define-public ghc-data-array-byte
  (package
    (name "ghc-data-array-byte")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-array-byte" version))
       (sha256
        (base32 "002n0af7q08q3fmgsc5b47s1clirxy0lrqglwxzhabg0nfhfrdhv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-array-byte")))
    (native-inputs (list ghc-quickcheck-classes-base ghc-tasty
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("5"
                         "06xfi70zgpv77nqrrnk649vdzji6cgp40a69i41kw05p7xaa1whc")))
    (home-page "https://github.com/Bodigrim/data-array-byte")
    (synopsis "Compatibility layer for Data.Array.Byte")
    (description
     "Compatibility layer for
@url{https://hackage.haskell.org/package/base/docs/Data-Array-Byte.html,Data.Array.Byte}
providing boxed wrappers for @code{ByteArray} and
@code{MutableByteArray} and relevant instances for GHC < 9.4.")
    (license license:bsd-3)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;

