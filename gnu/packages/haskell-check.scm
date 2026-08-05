;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017, 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Tonton <tonton@riseup.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Carlo Holl <carloholl@gmail.com>
;;; Copyright © 2021 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 zamfofex <zamfofex@twdb.moe>
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

(define-module (gnu packages haskell-check)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ghc-tasty-ant-xml
  (package
    (name "ghc-tasty-ant-xml")
    (version "1.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-ant-xml" version))
       (sha256
        (base32 "00zgsd6jjfwasr69d0y781vhjr7d8p1jbaaz4pn75ljf33akd92l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-ant-xml")))
    (inputs (list ghc-generic-deriving ghc-tagged ghc-tasty ghc-xml))
    (home-page "http://github.com/ocharles/tasty-ant-xml")
    (synopsis "Render tasty output to XML for Jenkins")
    (description
     "A tasty ingredient to output test results in XML, using the Ant
schema.  This XML can be consumed by the Jenkins continuous integration
framework.")
    (license license:bsd-3)))

(define-public ghc-tasty-smallcheck
  (package
    (name "ghc-tasty-smallcheck")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-smallcheck" version))
       (sha256
        (base32 "0csgwn3vch0jnpqyyfnrfjq4z0dpl67imh5a7byll3hhlyidgjym"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-smallcheck")))
    (inputs (list ghc-tasty ghc-smallcheck ghc-tagged ghc-optparse-applicative))
    (arguments
     `(#:cabal-revision ("1"
                         "0033ha2w9rzc1rxpzh1dkfdrn256i5lvb41pqbdh2i6kli0v5vmh")))
    (home-page "https://github.com/feuerbach/tasty")
    (synopsis "SmallCheck support for the Tasty test framework")
    (description "This package provides SmallCheck support for the Tasty
Haskell test framework.")
    (license license:expat)))

(define-public ghc-tasty-quickcheck
  (package
    (name "ghc-tasty-quickcheck")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-quickcheck" version))
       (sha256
        (base32 "0si4ccgqlv8h33d6310rrqba7f4pz3g8cinqfj42yd7damsdxm73"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-quickcheck")))
    (inputs (list ghc-tagged ghc-tasty ghc-random ghc-quickcheck
                  ghc-optparse-applicative))
    (native-inputs (list ghc-regex-tdfa ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("3"
                         "1wzvha4xam8npx5mk33c056grmrqnjd6m38nnm6d7y99w2mn1a7w")))
    (home-page "https://github.com/UnkindPartition/tasty")
    (synopsis "QuickCheck support for the Tasty test framework")
    (description "This package provides QuickCheck support for the Tasty
Haskell test framework.")
    (license license:expat)))

(define-public ghc-tasty-golden
  (package
    (name "ghc-tasty-golden")
    (version "2.3.5")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tasty-golden" version))
              (sha256
               (base32
                "03klnxn9rcv0l7fl4w8q6s59fzl1328j1wzwi1za4gb0l90vadwb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-golden")))
    (inputs (list ghc-tasty
                  ghc-typed-process
                  ghc-optparse-applicative
                  ghc-temporary
                  ghc-tagged
                  ghc-async))
    (native-inputs (list ghc-tasty-hunit))
    (home-page "https://github.com/UnkindPartition/tasty-golden")
    (synopsis "Golden tests support for tasty")
    (description
     "This package provides support for @code{golden testing}.  A @dfn{golden
test} is an IO action that writes its result to a file.  To pass the test, this
output file should be identical to the corresponding @code{golden} file, which
contains the correct result for the test.")
    (license license:expat)))

(define-public ghc-tasty
  (package
    (name "ghc-tasty")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty" version))
       (sha256
        (base32 "10076vlklbcyiz7plakrihava5sy3dvwhskjldqzhfl18jvcg82l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty")))
    (inputs
      (list ghc-tagged
            ghc-optparse-applicative
            ghc-ansi-terminal
            ghc-unbounded-delays)) ; needed on 32-bit architectures
    (arguments
     `(#:cabal-revision ("2"
                         "04llcf1i3gawdik0bjhxdgls2wkiqlx0gi76nfh784nv2qzxlpbb")))
    (home-page "https://github.com/UnkindPartition/tasty")
    (synopsis "Modern and extensible testing framework")
    (description
     "Tasty is a modern testing framework for Haskell.  It lets
you combine your unit tests, golden tests, QuickCheck/SmallCheck properties,
and any other types of tests into a single test suite.")
    (license license:expat)))

(define-public ghc-tasty-hedgehog
  (package
    (name "ghc-tasty-hedgehog")
    (version "1.4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-hedgehog" version))
       (sha256
        (base32 "0lki03z0p38x0dkqx5cqga30zy5m31gxn1saqylja9bi6bbq8d25"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-hedgehog")))
    (inputs (list ghc-tagged ghc-tasty ghc-hedgehog))
    (native-inputs (list ghc-tasty-expected-failure))
    (arguments
     `(#:cabal-revision ("6"
                         "1rb8ncp6xyy7jr6v0hyls9m529ba0djndsxgxmkgr52rk3qq8lrc")))
    (home-page "https://github.com/qfpl/tasty-hedgehog")
    (synopsis "Integration for tasty and hedgehog")
    (description
     "This package provides the means for integrating the
@url{https://hackage.haskell.org/package/hedgehog, hedgehog testing library}
with the @url{https://hackage.haskell.org/package/tasty, tasty testing
framework}.")
    (license license:bsd-3)))

(define-public ghc-tasty-hspec
  (package
    (name "ghc-tasty-hspec")
    (version "1.2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-hspec" version))
       (sha256
        (base32 "1hk1nkjvhp89xxgzj6dhbgw0fknnghpng6afq4i39hjkwv5p78ni"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-hspec")))
    (inputs (list ghc-hspec
                  ghc-hspec-api
                  ghc-hspec-core
                  ghc-quickcheck
                  ghc-tasty
                  ghc-tasty-smallcheck
                  ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("7"
                         "0s1y34i8g7fva0z10ws3ipcy2jmlvqk0v4hdbx8rqnby5n0l5kay")))
    (home-page "https://github.com/mitchellwrosen/tasty-hspec")
    (synopsis "Hspec support for the Tasty test framework")
    (description
     "This package provides a Tasty provider for Hspec test suites.")
    (license license:bsd-3)))

(define-public ghc-tasty-hunit
  (package
    (name "ghc-tasty-hunit")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-hunit" version))
       (sha256
        (base32 "1xh33ss7dncm7zanzkzh7ywb3a46k7vkcbh6v8jb767gq6kizw2s"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-hunit")))
    (inputs (list ghc-tasty ghc-call-stack-boot))
    (home-page "https://github.com/UnkindPartition/tasty")
    (synopsis "HUnit support for the Tasty test framework")
    (description "This package provides HUnit support for the Tasty Haskell
test framework.")
    (license license:expat)))

(define-public ghc-tasty-kat
  (package
    (name "ghc-tasty-kat")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tasty-kat" version))
              (sha256
               (base32
                "14yvlpli6cv6bn3kh8mlfp4x1l6ns4fvmfv6hmj75cvxyzq029d7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-kat")))
    (inputs
     (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit))
    (home-page "https://github.com/vincenthz/tasty-kat")
    (synopsis "Known Answer Tests (KAT) framework for tasty")
    (description
     "This package provides a @dfn{Known Answer Tests} (KAT) framework for
tasty.")
    (license license:expat)))

(define-public ghc-tasty-lua
  (package
    (name "ghc-tasty-lua")
    (version "1.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-lua" version))
       (sha256
        (base32 "03b2n3gw2w70cnl57w3sh3cv5ka270sf07jlxpb4zs0z5gh83p1r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-lua")))
    (inputs (list ghc-hslua-core
                  ghc-hslua-marshalling
                  ghc-lua-arbitrary
                  ghc-tasty
                  ghc-quickcheck
                  ghc-file-embed))
    (native-inputs (list ghc-tasty-hunit))
    (home-page "https://github.com/hslua/hslua")
    (synopsis "Write tests in Lua, integrate into tasty")
    (description "This package gives users the ability to define tasty tests
from Lua.")
    (license license:expat)))

(define-public ghc-tasty-th
  (package
    (name "ghc-tasty-th")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-th" version))
       (sha256
        (base32 "0b2ivrw2257m4cy4rjnkwqlarh83j1y3zywnmaqqqbvy667sqnj3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-th")))
    (inputs (list ghc-haskell-src-exts ghc-tasty))
    (native-inputs (list ghc-tasty-hunit))
    (home-page "https://github.com/bennofs/tasty-th")
    (synopsis "Automatically generate tasty TestTrees")
    (description
     "Tasty-th automatically generates tasty TestTrees from functions of the
current module, using TemplateHaskell.  This is a fork the original
test-framework-th package, modified to work with tasty instead of
test-framework.")
    (license license:bsd-3)))

(define-public ghc-tasty-rerun
  (package
    (name "ghc-tasty-rerun")
    (version "1.1.20")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-rerun" version))
       (sha256
        (base32 "0px58jm1yqbg32qf2s0yk09d2qdjxkkz9df89f31q3nzw85jv2ky"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-rerun")))
    (inputs (list ghc-optparse-applicative ghc-split ghc-tagged ghc-tasty))
    (arguments
     `(#:cabal-revision ("1"
                         "13xmx91hp7i0qzrhada9ckliqkynwlwa8x6pjbvxjcy1y0qsd7hk")))
    (home-page "https://github.com/ocharles/tasty-rerun")
    (synopsis "Run tests by filtering the test tree")
    (description
     "This package adds the ability to run tests by filtering the
test tree based on the result of a previous test run.  You can use this to run
only those tests that failed in the last run, or to only run the tests that have
been added since previous test run.")
    (license license:bsd-3)))

(define-public ghc-tasty-expected-failure
  (package
    (name "ghc-tasty-expected-failure")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-expected-failure" version))
       (sha256
        (base32
         "0zlgxs24d54byfhvwdg85xk1572zpjs71bjlxxrxcvralrfcq1yb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-expected-failure")))
    (arguments `(#:tests? #f)) ; TODO: Loops.
;    (native-inputs
;     `(("ghc-tasty-hunit" ,ghc-tasty-hunit)
;       ("ghc-tasty-golden" ,ghc-tasty-golden)
;       ("ghc-hedgehog" ,ghc-hedgehog)
;       ("ghc-tasty-hedgehog" ,ghc-tasty-hedgehog)))
    (inputs
     (list ghc-tagged ghc-tasty ghc-unbounded-delays))
    (home-page "https://github.com/nomeata/tasty-expected-failure")
    (synopsis "Mark tasty tests as failure expected")
    (description
     "With the function @code{Test.Tasty.ExpectedFailure.expectFail} in the
provided module @code{Test.Tasty.ExpectedFailure}, you can mark that you
expect test cases to fail, and not to pass.  This can be used for test-driven
development.")
    (license license:expat)))

(define-public ghc-quickcheck-instances
  (package
    (name "ghc-quickcheck-instances")
    (version "0.3.33")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "quickcheck-instances" version))
       (sha256
        (base32 "0rl8y3rb4fm4nqz122bp5f2aya4f8bc9m9i9n2vwlyq2gdacs0v8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quickcheck-instances")))
    (inputs (list ghc-quickcheck
                  ghc-splitmix-bootstrap
                  ghc-case-insensitive
                  ghc-data-fix
                  ghc-hashable
                  ghc-integer-logarithms
                  ghc-old-time
                  ghc-onetuple
                  ghc-primitive
                  ghc-scientific
                  ghc-strict
                  ghc-tagged
                  ghc-text-short
                  ghc-these
                  ghc-time-compat
                  ghc-unordered-containers
                  ghc-uuid-types
                  ghc-vector))
    (arguments
     `(#:cabal-revision ("1"
                         "1xkc7rsfgya4rwiizh0yfincws3knpdnh08m280v1dgik4kv37vh")))
    (home-page "https://github.com/haskellari/qc-instances")
    (synopsis "Common quickcheck instances")
    (description "This package provides QuickCheck instances for types
provided by the Haskell Platform.")
    (license license:bsd-3)))

(define-public ghc-quickcheck-unicode
  (package
    (name "ghc-quickcheck-unicode")
    (version "1.0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "quickcheck-unicode" version))
       (sha256
        (base32 "0s43s1bzbg3gwsjgm7fpyksd1339f0m26dlw2famxwyzgvm0a80k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quickcheck-unicode")))
    (inputs (list ghc-quickcheck))
    (home-page "https://github.com/bos/quickcheck-unicode")
    (synopsis "Generator functions Unicode-related tests")
    (description "This package provides generator and shrink functions for
testing Unicode-related software.")
    (license license:bsd-2)))

(define-public ghc-quickcheck-text
  (package
    (name "ghc-quickcheck-text")
    (version "0.1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "quickcheck-text" version))
       (sha256
        (base32 "02dbs0k6igmsa1hcw8yfvp09v7038vp4zlsp9706km3cmswgshj4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quickcheck-text")))
    (inputs (list ghc-quickcheck))
    (home-page "https://github.com/olorin/quickcheck-text")
    (synopsis "Alternative arbitrary instance for Text")
    (description
     "The usual Arbitrary instance for Text (in
<https://hackage.haskell.org/package/quickcheck-instances quickcheck-instances>)
only has single-byte instances and so isn't an ideal representation of a valid
UTF-8 character.  This package has generators for one-, two- and three-byte
UTF-8 characters (all that are currently in use).")
    (license license:expat)))

(define-public ghc-quickcheck-io
  (package
    (name "ghc-quickcheck-io")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "quickcheck-io" version))
       (sha256
        (base32
         "08k4v7pkgjf30pv5j2dfv1gqv6hclxlniyq2sps8zq4zswcr2xzv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quickcheck-io")))
    (inputs
     (list ghc-quickcheck ghc-hunit))
    (home-page
     "https://github.com/hspec/quickcheck-io#readme")
    (synopsis "Use HUnit assertions as QuickCheck properties")
    (description "This package provides an orphan instance that allows you to
use HUnit assertions as QuickCheck properties.")
    (license license:expat)))

(define-public ghc-quickcheck
  (package
    (name "ghc-quickcheck")
    (version "2.15.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "QuickCheck" version))
       (sha256
        (base32 "0zvfydg44ibs1br522rzvdlxj9mpz0h62js1hay1sj5gvdnj3cm3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "QuickCheck")))
    (inputs (list ghc-random-bootstrap ghc-splitmix-bootstrap))
    (arguments
     `(#:cabal-revision ("1"
                         "0cgfp4s51cjphsn9cls6rndisvqmi94vn95xan9g1yz6p5xk7z8c")))
    (home-page "https://github.com/nick8325/quickcheck")
    (synopsis "Automatic testing of Haskell programs")
    (description
     "QuickCheck is a library for random testing of program properties.  The
programmer provides a specification of the program, in the form of properties
which functions should satisfy, and QuickCheck then tests that the properties
hold in a large number of randomly generated cases.  Specifications are
expressed in Haskell, using combinators defined in the QuickCheck library.")
    (license license:bsd-3)))

(define-public ghc-quickcheck-assertions
  (package
    (name "ghc-quickcheck-assertions")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "quickcheck-assertions" version))
       (sha256
        (base32 "1kyam4cy7qmnizjwjm8jamq43w7f0fs6ljfplwj0ib6wi2kjh0wv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quickcheck-assertions")))
    (inputs (list ghc-quickcheck ghc-ieee754 ghc-pretty-show))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/s9gf4ult/quickcheck-assertions")
    (synopsis "HUnit-like assertions for QuickCheck")
    (description
     "This Haskell library provides convenient assertions with pretty-printed
failure messages for QuickCheck properties, that are similar to those of
HUnit.")
    (license license:lgpl3)))

(define-public ghc-test-framework
  (package
    (name "ghc-test-framework")
    (version "0.8.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "test-framework" version))
       (sha256
        (base32 "04ijf5x6xx8i5lqv9ir33zs1rfzc4qkwwz8c1fdycnzvydcv4dnp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "test-framework")))
    (inputs (list ghc-ansi-terminal
                  ghc-ansi-wl-pprint
                  ghc-random
                  ghc-regex-posix
                  ghc-old-locale
                  ghc-xml
                  ghc-hostname))
    (native-inputs (list ghc-hunit ghc-quickcheck ghc-semigroups-bootstrap))
    (arguments
     `(#:cabal-revision ("1"
                         "1yv1qsr6bxphxk9430id9bqhfmkffdqmfg0k017dp9pnn4pqj0zh")))
    (home-page "https://github.com/haskell/test-framework#readme")
    (synopsis "Framework for running and organising tests")
    (description
     "This package allows tests such as QuickCheck properties and HUnit test
cases to be assembled into test groups, run in parallel (but reported in
deterministic order, to aid diff interpretation) and filtered and controlled
by command line options.  All of this comes with colored test output, progress
reporting and test statistics output.")
    (license license:bsd-3)))

(define-public ghc-test-framework-hunit
  (package
    (name "ghc-test-framework-hunit")
    (version "0.3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "test-framework-hunit" version))
       (sha256
        (base32
         "1y0b6vg8nfm43v90lxxcydhi6qlxhfy4vpxbzm5ic2w55bh8xjwm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "test-framework-hunit")))
    (arguments
     `(#:cabal-revision
       ("3" "0i9mlalv7cl1iq43ld5myrnpszq5rxmd79hk495dcb08rglhgl3z")))
    (inputs
     (list ghc-extensible-exceptions ghc-hunit ghc-test-framework))
    (home-page "https://batterseapower.github.io/test-framework/")
    (synopsis "HUnit support for test-framework")
    (description
     "This package provides HUnit support for the test-framework package.")
    (license license:bsd-3)))

(define-public ghc-test-framework-quickcheck2
  (package
    (name "ghc-test-framework-quickcheck2")
    (version "0.3.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "test-framework-quickcheck2" version))
       (sha256
        (base32 "1d0w2q9sm8aayk0aj1zr2irpnqwpzixn6pdfq1i904vs1kkb2xin"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "test-framework-quickcheck2")))
    (inputs (list ghc-test-framework ghc-quickcheck ghc-extensible-exceptions
                  ghc-random))
    (arguments
     `(#:cabal-revision ("1"
                         "1af2gw9gvq143jdqmsnxj23cgss9ffdyr67951a5x151aps04y7z")))
    (home-page "https://github.com/haskell/test-framework")
    (synopsis "QuickCheck2 support for test-framework")
    (description
     "This package provides QuickCheck2 support for the test-framework
package.")
    (license license:bsd-3)))

(define-public ghc-test-framework-smallcheck
  (package
    (name "ghc-test-framework-smallcheck")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "test-framework-smallcheck" version))
       (sha256
        (base32 "1xpgpk1gp4w7w46b4rhj80fa0bcyz8asj2dcjb5x1c37b7rw90b0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "test-framework-smallcheck")))
    (inputs (list ghc-test-framework ghc-smallcheck))
    (home-page "https://github.com/feuerbach/smallcheck")
    (synopsis "SmallCheck support for test-framework")
    (description
     "This package lets programmers use SmallCheck properties in Haskell's
test-framework.  New projects should use ghc-tasty-smallcheck instead.")
    (license license:bsd-3)))

(define-public ghc-test-framework-th
  (package
    (name "ghc-test-framework-th")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "test-framework-th" version))
       (sha256
        (base32
         "12lw7yj02jb9s0i7rb98jjam43j2h0gzmnbj9zi933fx7sg0sy4b"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "test-framework-th")))
    (inputs
     (list ghc-test-framework ghc-language-haskell-extract
           ghc-haskell-src-exts ghc-regex-posix))
    (home-page "https://github.com/finnsson/test-generator")
    (synopsis "Auto generate the HUnit- and Quickcheck-bulk-code
using Template Haskell")
    (description "This library contains two functions:
@code{defaultMainGenerator} and @code{testGroupGenerator}.

@code{defaultMainGenerator} will extract all functions beginning with
@code{case_}, @code{prop_}, or @code{test_} in the module and put them in a
@code{testGroup}.

@code{testGroupGenerator} is like @code{defaultMainGenerator} but without
@code{defaultMain}.  It is useful if you need a function for the testgroup
\(e.g. if you want to be able to call the testgroup from another module).")
    (license license:bsd-3)))

(define-public ghc-hunit
  (package
    (name "ghc-hunit")
    (version "1.6.2.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "HUnit" version))
       (sha256
        (base32
         "1as4sw5y39c3zrmr6sb8zbw74c9gdn4401y0dx45ih7zf6457dxh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "HUnit")))
    (inputs
     ;; We cannot use ghc-call-stack there, because it depends on
     ;; ghc-nanospec, which depends on ghc-hunit.
     (list ghc-call-stack-boot))
    (home-page "https://hunit.sourceforge.net/")
    (synopsis "Unit testing framework for Haskell")
    (description
     "HUnit is a unit testing framework for Haskell, inspired by the
JUnit tool for Java.")
    (license license:bsd-3)))

(define-public hspec-discover
  (package
    (name "hspec-discover")
    (version "2.11.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-discover" version))
       (sha256
        (base32 "044vgsy45ff00h9z2k3jgn2m37npcjiacc4cifahrjlmwa7a7ylp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-discover")))
    (native-inputs (list ghc-quickcheck ghc-hspec-meta ghc-mockery-bootstrap))
    (home-page "https://hspec.github.io/")
    (synopsis "Automatically discover and run Hspec tests")
    (description "hspec-discover is a tool which automatically discovers and
runs Hspec tests.")
    (license license:expat)))

(define-public ghc-hspec-core
  (package
    (name "ghc-hspec-core")
    (version "2.11.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-core" version))
       (sha256
        (base32 "030400w95775jrivbi7n1nnx6j5z717rqd3986ggklb8h9hjalfc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-core")))
    (inputs (list ghc-hunit
                  ghc-quickcheck
                  ghc-ansi-terminal
                  ghc-call-stack
                  ghc-haskell-lexer
                  ghc-hspec-expectations
                  ghc-quickcheck-io
                  ghc-random
                  ghc-tf-random))
    (native-inputs (list ghc-base-orphans-bootstrap ghc-hspec-meta
                         ghc-silently-bootstrap ghc-temporary-bootstrap))
    (arguments
     `(#:cabal-revision ("1"
                         "0yq9nnawcgbgxiz4ymfa8k66jrvgrhmv8j7g880x8k6q8q4ncqlq")))
    (home-page "https://hspec.github.io/")
    (synopsis "Testing framework for Haskell")
    (description "This library exposes internal types and functions that can
be used to extend Hspec's functionality.")
    (license license:expat)))

(define-public ghc-hspec-meta
  (package
    (name "ghc-hspec-meta")
    (version "2.11.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-meta" version))
       (sha256
        (base32 "1612pg5gihqjxrzqqvbbgckaqiwq3rmz3rg07lrjhzklg975nj69"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-meta")))
    (inputs (list ghc-hunit
                  ghc-quickcheck
                  ghc-ansi-terminal
                  ghc-call-stack-boot
                  ghc-haskell-lexer
                  ghc-hspec-expectations
                  ghc-quickcheck-io
                  ghc-random
                  ghc-tf-random))
    (arguments
     `(#:cabal-revision ("2"
                         "1jrk14s51psb0zjici56220iyb98i3q06sd3rsyx594s3cddgn5d")))
    (home-page "https://hspec.github.io/")
    (synopsis "Version of Hspec to test Hspec itself")
    (description "This library provides a stable version of Hspec which is
used to test the in-development version of Hspec.")
    (license license:expat)))

(define-public ghc-hspec
  (package
    (name "ghc-hspec")
    (version "2.11.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec" version))
       (sha256
        (base32 "1zdgkn0gkcphcsfqcqwcfqiqvkz12ljks46v3czpaysndz8icfip"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec")))
    (inputs (list ghc-quickcheck ghc-hspec-core hspec-discover
                  ghc-hspec-expectations))
    (home-page "https://hspec.github.io/")
    (synopsis "Testing Framework for Haskell")
    (description "This library provides the Hspec testing framework for
Haskell, inspired by the Ruby library RSpec.")
    (license license:expat)))

(define-public ghc-hspec-api
  (package
    (name "ghc-hspec-api")
    (version "2.11.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-api" version))
       (sha256
        (base32 "1xn2b2hafyq6qm3zaia9nw27ir4cg8v2qn499bz2zwnp2vkjra07"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-api")))
    (inputs (list ghc-hspec-core))
    (native-inputs (list hspec-discover ghc-hspec))
    (home-page "https://hspec.github.io/")
    (synopsis "A Testing Framework for Haskell")
    (description
     "This package provides a stable API that can be used to extend Hspec's
functionality.")
    (license license:expat)))

(define-public ghc-hspec-contrib
  (package
    (name "ghc-hspec-contrib")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-contrib" version))
       (sha256
        (base32 "0002xzvyh790iwf1y33d4nflrbp5sxvpsp010srcfryf1n3qx2b0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-contrib")))
    (inputs (list ghc-hunit ghc-call-stack ghc-hspec-core))
    (native-inputs (list ghc-quickcheck ghc-hspec hspec-discover))
    (home-page "https://hspec.github.io/")
    (synopsis "Contributed functionality for Hspec")
    (description "This package provides contributed Hspec extensions.")
    (license license:expat)))

(define-public ghc-hspec-expectations
  (package
    (name "ghc-hspec-expectations")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-expectations" version))
       (sha256
        (base32 "1zr1pqchcwglfr5dvcrgc1l5x924n9w09n2zr68dmkqf4dzdx3bv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-expectations")))
    (inputs (list ghc-hunit ghc-call-stack))
    (arguments
    ;; Tests depend on ghc-nanospec.
     `(#:tests? #f
       #:cabal-revision ("2"
                         "14zzsjqcz1zbnvi50i82lx84nc8b5da7ar5cazzh44lklyag0ds2")))
    (home-page "https://github.com/hspec/hspec-expectations#readme")
    (synopsis "Catchy combinators for HUnit")
    (description
     "This library provides catchy combinators for HUnit, see
@uref{https://github.com/sol/hspec-expectations#readme, the README}.")
    (license license:expat)))

(define-public ghc-nanospec
  (package
    (name "ghc-nanospec")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "nanospec" version))
       (sha256
        (base32
         "1rcmhl9bhyfvanalnf1r86wkx6rq6wdvagnw1h011jcnnb1cq56g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "nanospec")))
    (inputs (list ghc-silently-bootstrap))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/hspec/nanospec#readme")
    (synopsis "Lightweight implementation of a subset of Hspec's API")
    (description
     "Nanospec is a lightweight implementation of a subset of Hspec's API with
minimal dependencies.")
    (license license:expat)))

(define-public ghc-nanospec-bootstrap
  (package
    (inherit ghc-nanospec)
    (name "ghc-nanospec-bootstrap")
    (arguments '(#:tests? #f))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-crypto-cipher-tests
  (package
    (name "ghc-crypto-cipher-tests")
    (version "0.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypto-cipher-tests" version))
       (sha256
        (base32
         "19wqignlq90qwpam01hnmmrxaxh5lkax9l1l6rlbi4a07nvp1dnz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypto-cipher-tests")))
    (inputs (list ghc-quickcheck
                  ghc-hunit
                  ghc-test-framework
                  ghc-test-framework-quickcheck2
                  ghc-test-framework-hunit
                  ghc-byteable
                  ghc-securemem
                  ghc-crypto-cipher-types))
    (home-page "https://github.com/vincenthz/hs-crypto-cipher")
    (synopsis "Generic cryptography cipher tests for Haskell")
    (description "This Haskell package contains generic tests for
cryptographic ciphers, and is used by the test runners of various Haskell
implementations of cryptographic ciphers.")
    (license license:bsd-3)))

(define-public ghc-hedgehog
  (package
    (name "ghc-hedgehog")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hedgehog" version))
       (sha256
        (base32 "0sl6x9q9kyrpv73565w9na9dm10wzxdl0qgiraqarffynfgn0hg9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hedgehog")))
    (inputs (list ghc-ansi-terminal
                  ghc-async
                  ghc-barbies
                  ghc-concurrent-output
                  ghc-erf
                  ghc-lifted-async
                  ghc-mmorph
                  ghc-monad-control
                  ghc-pretty-show
                  ghc-primitive
                  ghc-random
                  ghc-resourcet
                  ghc-safe-exceptions
                  ghc-transformers-base
                  ghc-wl-pprint-annotated))
    (arguments
     `(#:cabal-revision ("2"
                         "1m0b9m8dw5nw4b3w1jbp5fd0b0kqlyvvy3qsfxc7md77iafxq169")))
    (home-page "https://hedgehog.qa")
    (synopsis "Property-based testing in the spirt of QuickCheck")
    (description
     "Hedgehog is a property-based testing system, in the spirit of
QuickCheck.  Hedgehog uses integrated shrinking, so shrinks obey the invariants
of generated values by construction.

To get started quickly, see the examples:
@uref{https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-example}")
    (license license:bsd-3)))

;; Deprecated. Don’t use.
(define-public cabal-doctest
  (package
    (name "cabal-doctest")
    (version "1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cabal-doctest" version))
       (sha256
        (base32 "0gwjpwv2v7c7gs2dvf7ixsxx6likmgw5yi0fy4bqc0i7nkqg4bfw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cabal-doctest")))
    (home-page "https://github.com/ulidtko/cabal-doctest")
    (synopsis "Setup.hs helper for running doctests")
    (description
     "To properly work, the @code{doctest} package needs plenty of
configuration.  This library provides the common bits for writing custom
@file{Setup.hs} files.")
    (license license:bsd-3)))

(define-public ghc-testing-type-modifiers
  (package
    (name "ghc-testing-type-modifiers")
    (version "0.1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "testing-type-modifiers" version))
        (sha256
          (base32
            "1wh2n95n39ivv6kbqn42vbzrj8zagsmk6f2al2qj40bg5kgdl2q5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "testing-type-modifiers")))
    (home-page "https://hackage.haskell.org/package/testing-type-modifiers")
    (synopsis "Data type modifiers for property based testing")
    (description "Property based testing libraries such as QuickCheck tend to
include type modifiers.  Most of them are used to quantify over subsets of a
type.  This library is intended to supply these modifiers to be used by
testing libraries, in an effort to make properties more portable between
testing frameworks.")
    (license license:unlicense)))

(define-public ghc-testing-feat
  (package
    (name "ghc-testing-feat")
    (version "1.1.1.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "testing-feat" version))
              (sha256
               (base32
                "14d6licgrkiw36xj1cshnqxcbx5iwzxwq731xlb1wb5n2sw8ijf2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "testing-feat")))
    (inputs (list ghc-quickcheck ghc-size-based ghc-testing-type-modifiers))
    (home-page "https://github.com/size-based/testing-feat")
    (synopsis "Functional Enumeration of Algebraic Types")
    (description
     "Feat (Functional Enumeration of Algebraic Types)
provides enumerations as functions from natural numbers to
values (similar to @code{toEnum} but for any algebraic data type).  This
can be used for SmallCheck-style systematic testing, QuickCheck-style
random testing, and hybrids of the two.")
    (license license:bsd-3)))

(define-public ghc-inspection-testing
  (package
    (name "ghc-inspection-testing")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "inspection-testing" version))
       (sha256
        (base32 "0zi1q86sd9jy5dpqfs2j71acdl7kvik0ps78xirpdhyldhwwyqws"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "inspection-testing")))
    (home-page "https://github.com/nomeata/inspection-testing")
    (synopsis "GHC plugin to do inspection testing")
    (description
     "Some carefully crafted libraries make promises to their users beyond
functionality and performance.

Examples are: Fusion libraries promise intermediate data structures to be
eliminated.  Generic programming libraries promise that the generic
implementation is identical to the hand-written one.  Some libraries may
promise allocation-free or branch-free code.

Conventionally, the modus operandi in all these cases is that the library
author manually inspects the (intermediate or final) code produced by the
compiler.  This is not only tedious, but makes it very likely that some change,
either in the library itself or the surrounding eco-system, breaks the
library's promised without anyone noticing.

This package provides a disciplined way of specifying such properties, and
have them checked by the compiler.  This way, this checking can be part of the
regular development cycle and regressions caught early.

See the documentation in \"Test.Inspection\" or the project webpage for more
examples and more information.")
    (license license:expat)))

(define-public ghc-quickcheck-classes
  (package
    (name "ghc-quickcheck-classes")
    (version "0.6.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "quickcheck-classes" version))
       (sha256
        (base32 "19iw15mvb7gws3ljdxqwsbb4pmfc0sfflf8szgmrhiqr3k82mqv2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quickcheck-classes")))
    (inputs (list ghc-quickcheck
                  ghc-primitive
                  ghc-primitive-addr
                  ghc-quickcheck-classes-base
                  ghc-semigroups-bootstrap
                  ghc-fail
                  ghc-tagged
                  ghc-aeson
                  ghc-semigroupoids
                  ghc-semirings
                  ghc-vector))
    (native-inputs (list ghc-base-orphans ghc-base-orphans ghc-tasty
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("3"
                         "1pawam5rsdcdv21fqc87khzqjm0ixwzklfxd6gk3b2qrr5q66bn5")))
    (home-page "https://github.com/andrewthad/quickcheck-classes#readme")
    (synopsis "QuickCheck common typeclasses")
    (description
     "This library provides QuickCheck properties to ensure that typeclass
instances adhere to the set of laws that they are supposed to.  There are
other libraries that do similar things, such as @code{genvalidity-hspec} and
@code{checkers}.  This library differs from other solutions by not introducing
any new typeclasses that the user needs to learn.  /Note:/ on GHC < 8.5, this
library uses the higher-kinded typeclasses (@code{Data.Functor.Classes.Show1},
@code{Data.Functor.Classes.Eq1}, @code{Data.Functor.Classes.Ord1}, etc.), but
on GHC >= 8.5, it uses @code{-XQuantifiedConstraints} to express these
constraints more cleanly.")
    (license license:bsd-3)))

(define-public ghc-quickcheck-classes-base
  (package
    (name "ghc-quickcheck-classes-base")
    (version "0.6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "quickcheck-classes-base" version))
       (sha256
        (base32 "16c6gq4cqpkwnq1pzkhm6r7mrwk4an50ha5w77bmiia2qkhla6ch"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quickcheck-classes-base")))
    (inputs (list ghc-quickcheck
                  ghc-contravariant
                  ghc-bifunctors
                  ghc-semigroups-bootstrap
                  ghc-fail
                  ghc-tagged))
    (arguments
     `(#:cabal-revision ("1"
                         "1p3v38jhpx0r6rnvaspkkivl8xyq2mq4xnmycgmkj1gr77vplkdr")))
    (home-page "https://github.com/andrewthad/quickcheck-classes#readme")
    (synopsis "QuickCheck common typeclasses from `base`")
    (description
     "This library is a minimal variant of `quickcheck-classes` that only
provides laws for typeclasses from `base`.  The main purpose of splitting this
out is so that `primitive` can depend on `quickcheck-classes-base` in its test
suite, avoiding the circular dependency that arises if `quickcheck-classes` is
used instead.  This library provides QuickCheck properties to ensure that
typeclass instances adhere to the set of laws that they are supposed to.
There are other libraries that do similar things, such as `genvalidity-hspec`
and `checkers`.  This library differs from other solutions by not introducing
any new typeclasses that the user needs to learn.  @emph{Note:} on GHC < 8.5,
this library uses the higher-kinded typeclasses
(@code{Data.Functor.Classes.Show1}, @code{Data.Functor.Classes.Eq1},
@code{Data.Functor.Classes.Ord1}, etc.), but on GHC >= 8.5, it uses
`-XQuantifiedConstraints` to express these constraints more cleanly.")
    (license license:bsd-3)))

(define-public ghc-doctest-lib
  (package
    (name "ghc-doctest-lib")
    (version "0.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "doctest-lib" version))
       (sha256
        (base32 "1hb3zx1xzib3v41blnwcbhc2v0rzwdzq7gm4sajqndimwjkkxc67"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "doctest-lib")))
    (home-page "https://hub.darcs.net/thielema/doctest-lib/")
    (synopsis "Parts of doctest exposed as library")
    (description
     "Parts of doctest exposed as library.  For use with the doctest-extract utility.")
    (license license:expat)))

(define-public ghc-doctest-exitcode-stdio
  (package
    (name "ghc-doctest-exitcode-stdio")
    (version "0.0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "doctest-exitcode-stdio" version))
       (sha256
        (base32 "0kg5xiw4giyvqpcj6cxqqnysvixhxlwm0pbg3qks8dzwb5w79dvk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "doctest-exitcode-stdio")))
    (inputs (list ghc-doctest-lib ghc-quickcheck ghc-semigroups))
    (home-page "https://hub.darcs.net/thielema/doctest-exitcode-stdio/")
    (synopsis "Run Doctests in a @code{Cabal.Test.exitcode-stdio} environment")
    (description
     "This package allows on to run Doctests in a Cabal.Test.exitcode-stdio
environment.")
    (license license:bsd-3)))

(define-public ghc-tasty-silver
  (package
    (name "ghc-tasty-silver")
    (version "3.3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-silver" version))
       (sha256
        (base32 "01w3576kymglcddinh10m1wgy71dia49k2pnw5y1c97jjrhanf17"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-silver")))
    (inputs (list ghc-ansi-terminal
                  ghc-optparse-applicative
                  ghc-process-extras
                  ghc-regex-tdfa
                  ghc-silently
                  ghc-tagged
                  ghc-tasty
                  ghc-temporary))
    (native-inputs (list ghc-tasty-hunit))
    (home-page "https://github.com/phile314/tasty-silver")
    (synopsis "Fancy test runner, including support for golden tests")
    (description
     "This package provides a fancy test runner and support for @dfn{golden
testing}.  A golden test is an IO action that writes its result to a file.  To
pass the test, this output file should be identical to the corresponding
``golden'' file, which contains the correct result for the test.  The test
runner allows filtering tests using regexes, and to interactively inspect the
result of golden tests.")
    (license license:expat)))

(define-public ghc-tasty-inspection-testing
  (package
    (name "ghc-tasty-inspection-testing")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-inspection-testing" version))
       (sha256
        (base32 "0wl2xlnbmqdkwhi4ic6a4q4lxf9qg433lidi8d0hlp3ykrjpbcay"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-inspection-testing")))
    (inputs (list ghc-inspection-testing ghc-tasty))
    (arguments
     `(#:cabal-revision ("2"
                         "0z9al0hyq381fw146agbpz7rf24rwms1w91m7s0k1w3xbfw16l9n")))
    (home-page "https://github.com/Bodigrim/tasty-inspection-testing")
    (synopsis "Inspection testing support for tasty")
    (description
     "Integrate @@inspection-testing@@ into @@tasty@@ test suites.")
    (license license:expat)))

(define-public ghc-proctest
  (package
    (name "ghc-proctest")
    (version "0.1.3.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "proctest" version))
              (sha256
               (base32
                "0yvhc0l5aj170ymhjx4ig0im0yd7wfa150m5lqhabr8j7cf16l7y"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "proctest")))
    (inputs (list ghc-hunit ghc-hspec ghc-quickcheck))
    (home-page "https://github.com/nh2/proctest")
    (synopsis "IO library for testing interactive command line programs")
    (description
     "This package provides an IO library for testing interactive command line
programs.  Proctest aims to simplify interacting with and testing terminal
programs, providing convenience functions for starting programs and reading
their output.  All blocking operations support timeouts so that misbehaving
programs cannot block your test pipeline.  Find more examples and contribute
at @url{https://github.com/nh2/proctest}.")
    (license license:expat)))
