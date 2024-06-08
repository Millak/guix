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
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-ant-xml" version))
       (sha256
        (base32
         "0h9mllhw9cd0rn34xhj8grwmbny7z7hpd8qmp9lfcdj0s4qx9vx8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-ant-xml")))
    (inputs
     (list ghc-generic-deriving ghc-xml ghc-tagged ghc-tasty))
    (home-page
     "https://github.com/ocharles/tasty-ant-xml")
    (synopsis
     "Render tasty output to XML for Jenkins")
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
        (base32
         "0csgwn3vch0jnpqyyfnrfjq4z0dpl67imh5a7byll3hhlyidgjym"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-smallcheck")))
    (inputs
     (list ghc-tasty ghc-smallcheck ghc-async ghc-tagged))
    (home-page "https://documentup.com/feuerbach/tasty")
    (synopsis "SmallCheck support for the Tasty test framework")
    (description "This package provides SmallCheck support for the Tasty
Haskell test framework.")
    (license license:bsd-3)))

(define-public ghc-tasty-quickcheck
  (package
    (name "ghc-tasty-quickcheck")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tasty-quickcheck" version))
              (sha256
               (base32
                "1qnc6rdvjvlw08q6sln2n98rvj0s0pp689h6w4z58smjbn0lr25l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-quickcheck")))
    (inputs (list ghc-tagged ghc-tasty ghc-random ghc-quickcheck
                  ghc-optparse-applicative))
    (native-inputs (list ghc-tasty-hunit ghc-pcre-light))
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
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tasty" version))
              (sha256
               (base32
                "006bf4gyc30i2gvb17hj1mzrh1kwnwf7l050x3f72wi6c2axl87l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty")))
    (inputs
      (append
        (list ghc-tagged ghc-optparse-applicative ghc-ansi-terminal)
        ;; TODO: Add ghc-unbounded-delays unconditionally on next rebuild cycle.
        (if (member (%current-system) '("i686-linux"))
            (list ghc-unbounded-delays)
            '())))
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
    (version "1.3.1.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tasty-hedgehog" version))
              (sha256
               (base32
                "1iq452mvd9wc9pfmjsmm848jwp3cvsk1faf2mlr21vcs0yaxvq3m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-hedgehog")))
    (inputs (list ghc-tagged ghc-tasty ghc-hedgehog))
    (native-inputs (list ghc-tasty-expected-failure))
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
    (version "1.2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-hspec" version))
       (sha256
        (base32 "0cfcpi25jmnmzfzsx364qsj68q6gyph5z112kl8ja222hnhhr2n2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-hspec")))
    (inputs (list ghc-hspec
                  ghc-hspec-core
                  ghc-quickcheck
                  ghc-tasty
                  ghc-tasty-smallcheck
                  ghc-tasty-quickcheck
                  ghc-tagged))
    (home-page "https://github.com/mitchellwrosen/tasty-hspec")
    (synopsis "Hspec support for the Tasty test framework")
    (description
     "This package provides a Tasty provider for Hspec test suites.")
    (license license:bsd-3)))

(define-public ghc-tasty-hunit
  (package
    (name "ghc-tasty-hunit")
    (version "0.10.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-hunit" version))
       (sha256
        (base32
         "0gz6zz3w7s44pymw33xcxnawryl27zk33766sab96nz2xh91kvxp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-hunit")))
    (inputs
     (list ghc-call-stack-boot ghc-tasty))
    (home-page "http://documentup.com/feuerbach/tasty")
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
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tasty-lua" version))
              (sha256
               (base32
                "1vnyvgcjsvqhwwyqkbgqksr9ppj5whiihpwcqkg33sl7jj3ysdwv"))))
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
        (base32
         "0b2ivrw2257m4cy4rjnkwqlarh83j1y3zywnmaqqqbvy667sqnj3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-th")))
    (inputs
     (list ghc-haskell-src-exts ghc-tasty ghc-tasty-hunit))
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
    (version "1.1.18")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tasty-rerun" version))
              (sha256
               (base32
                "0sccp5zx9v2rx741nbmgd8mzjhy5m4v74hk26d23xz93ph8aqx7s"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-rerun")))
    (inputs
     (list ghc-optparse-applicative ghc-reducers ghc-split ghc-tagged
           ghc-tasty))
    (arguments
     `(#:cabal-revision ("3"
                         "0091arn90cx5rzn5n2bpb9alzybwraf9yj7hb0bwdfyamzpf3pkb")))
    (home-page "https://github.com/ocharles/tasty-rerun")
    (synopsis "Run tests by filtering the test tree")
    (description "This package adds the ability to run tests by filtering the
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
expect test cases to fail, and not to pass. This can be used for test-driven
development.")
    (license license:expat)))

(define-public ghc-quickcheck-instances
  (package
    (name "ghc-quickcheck-instances")
    (version "0.3.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "quickcheck-instances" version))
       (sha256
        (base32 "0jx2wfy7y5dr14s9i457g2aah4isjxry4mlbqhj7vlav6ib84gdj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quickcheck-instances")))
    (inputs (list ghc-quickcheck
                  ghc-splitmix
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
                  ghc-these
                  ghc-time-compat
                  ghc-transformers-compat
                  ghc-unordered-containers
                  ghc-uuid-types
                  ghc-vector
                  ghc-data-array-byte
                  ghc-text-short))
    (arguments
     `(#:cabal-revision ("2"
                         "118xy4z4dy4bpkzsp98daiv3l4n5j7ph9my0saca7cqjybqwkcip")))
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
        (base32
         "0s43s1bzbg3gwsjgm7fpyksd1339f0m26dlw2famxwyzgvm0a80k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "quickcheck-unicode")))
    (inputs (list ghc-quickcheck))
    (home-page
     "https://github.com/bos/quickcheck-unicode")
    (synopsis "Generator functions Unicode-related tests")
    (description "This package provides generator and shrink functions for
testing Unicode-related software.")
    (license license:bsd-3)))

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
    (version "2.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "QuickCheck" version))
       (sha256
        (base32 "0085lwy14r7hk7ibmv8d7d54ja9zin0ijf0b27xai898dfrj43sw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "QuickCheck")))
    (inputs
     (list ghc-random ghc-splitmix-bootstrap))
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
    (native-inputs
     (list ghc-hspec))
    (inputs
     (list ghc-ieee754 ghc-pretty-show ghc-quickcheck))
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
    (version "0.8.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "test-framework" version))
       (sha256
        (base32
         "1hhacrzam6b8f10hyldmjw8pb7frdxh04rfg3farxcxwbnhwgbpm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "test-framework")))
    (arguments
     `(#:tests? #f  ; FIXME: Tests do not build.
       #:cabal-revision
       ("6" "0wbq9wiaag69nsqxwijzhs5y1hb9kbpkp1x65dvx158cxp8i9w9r")))
    ;(native-inputs
    ; (list ghc-hunit ghc-quickcheck))
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-hostname" ,ghc-hostname)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-random" ,ghc-random)
       ("ghc-regex-posix" ,ghc-regex-posix)
       ("ghc-xml" ,ghc-xml)
       ("ghc-libxml" ,ghc-libxml)
       ("ghc-semigroups" ,ghc-semigroups-bootstrap)))
    (home-page "https://batterseapower.github.io/test-framework/")
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
    (version "0.3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "test-framework-quickcheck2" version))
       (sha256
        (base32
         "0ngf9vvby4nrdf1i7dxf5m9jn0g2pkq32w48xdr92n9hxka7ixn9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "test-framework-quickcheck2")))
    (arguments
     `(#:cabal-revision
       ("3" "0mglqfimla4vvv80mg08aj76zf4993wmngqlirh05h8i9nmgv6lh")))
    (inputs
     (list ghc-extensible-exceptions ghc-quickcheck ghc-random
           ghc-test-framework))
    (home-page "https://batterseapower.github.io/test-framework/")
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
    (inputs
     (list ghc-smallcheck ghc-test-framework))
    (home-page "https://github.com/Bodigrim/smallcheck")
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
    (version "2.9.7")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hspec-discover" version))
              (sha256
               (base32
                "0536kdxjw6p8b6gcwvmr22jbmb6cgzbddi0fkd01b2m847z37sb5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-discover")))
    (native-inputs (list ghc-quickcheck ghc-hspec-meta ghc-mockery-bootstrap))
    (home-page "http://hspec.github.io/")
    (synopsis "Automatically discover and run Hspec tests")
    (description "hspec-discover is a tool which automatically discovers and
runs Hspec tests.")
    (license license:expat)))

(define-public ghc-hspec-core
  (package
    (name "ghc-hspec-core")
    (version "2.9.7")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hspec-core" version))
              (sha256
               (base32
                "040rzqiqwkp373jjpij8lkmv08pp2ya92zzcf95bw8px215rp08n"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-core")))
    (inputs (list ghc-hunit
                  ghc-quickcheck
                  ghc-ansi-terminal
                  ghc-call-stack
                  ghc-clock
                  ghc-hspec-expectations
                  ghc-quickcheck-io
                  ghc-random
                  ghc-setenv
                  ghc-tf-random))
    (native-inputs (list ghc-base-orphans-bootstrap ghc-hspec-meta
                         ghc-silently-bootstrap ghc-temporary))
    (home-page "http://hspec.github.io/")
    (synopsis "Testing framework for Haskell")
    (description "This library exposes internal types and functions that can
be used to extend Hspec's functionality.")
    (license license:expat)))

(define-public ghc-hspec-meta
  (package
    (name "ghc-hspec-meta")
    (version "2.9.3")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hspec-meta" version))
              (sha256
               (base32
                "1raxwpsmcijl3x2h5naw6aydhbiknxvhj3x7v384bi1rqi51ainm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-meta")))
    (inputs (list ghc-quickcheck
                  ghc-ansi-terminal
                  ghc-call-stack-boot
                  ghc-clock
                  ghc-quickcheck-io
                  ghc-random
                  ghc-setenv))
    (home-page "http://hspec.github.io/")
    (synopsis "Version of Hspec to test Hspec itself")
    (description "This library provides a stable version of Hspec which is
used to test the in-development version of Hspec.")
    (license license:expat)))

(define-public ghc-hspec
  (package
    (name "ghc-hspec")
    (version "2.9.7")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hspec" version))
              (sha256
               (base32
                "092sfqjkargxxszp9jjqa8ldjz0xv34jwn6k21q59ys5ckvsrpc1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec")))
    (inputs (list ghc-quickcheck ghc-hspec-core hspec-discover
                  ghc-hspec-expectations))
    (home-page "http://hspec.github.io/")
    (synopsis "Testing Framework for Haskell")
    (description "This library provides the Hspec testing framework for
Haskell, inspired by the Ruby library RSpec.")
    (license license:expat)))

(define-public ghc-hspec-contrib
  (package
    (name "ghc-hspec-contrib")
    (version "0.5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-contrib" version))
       (sha256
        (base32 "1nyb5n2jiq920yyf3flzyxrs5xpfyppl3jn18zhviyysjjk5drpx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-contrib")))
    (inputs (list ghc-hunit ghc-call-stack ghc-hspec-core))
    (native-inputs (list ghc-quickcheck ghc-hspec hspec-discover))
    (arguments (list #:tests? #f)) ; Fail to build.
    (home-page "https://hspec.github.io/")
    (synopsis "Contributed functionality for Hspec")
    (description "This package provides contributed Hspec extensions.")
    (license license:expat)))

(define-public ghc-hspec-expectations
  (package
    (name "ghc-hspec-expectations")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hspec-expectations" version))
       (sha256
        (base32
         "1vxl9zazbaapijr6zmcj72j9wf7ka1pirrjbwddwwddg3zm0g5l1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hspec-expectations")))
    ;; Tests depend on ghc-nanospec.
    (arguments '(#:tests? #f))
    (inputs (list ghc-hunit))
    (home-page "https://github.com/sol/hspec-expectations")
    (synopsis "Catchy combinators for HUnit")
    (description "This library provides catchy combinators for HUnit, see
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
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hedgehog" version))
              (sha256
               (base32
                "0dbk75hk6hqpzkjdlpw3s63qhm42kqigij33p321by6xndb59jg1"))))
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
                  ghc-transformers-base
                  ghc-wl-pprint-annotated))
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
    (version "1.0.9")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "cabal-doctest" version))
              (sha256
               (base32
                "0wxs0xkspc80h0g8ks792lrzldxvcnhc9rja1j0k678ijs20hmjm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cabal-doctest")))
    (arguments
     `(#:cabal-revision ("2"
                         "0868js0qgfhvmyw4hjzrvmlzyqsm8dfsbmqhybxb90x44xi3r0i1")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "cabal-doctest.cabal"
               (("\\b(Cabal|base)\\s+[^,]+" all dep)
                dep)))))))
    (home-page "https://github.com/haskellari/cabal-doctest")
    (synopsis "Setup.hs helper for running doctests")
    (description
     "To properly work, the @code{doctest} package needs plenty of
configuration.  This library provides the common bits for writing custom
@file{Setup.hs} files.")
    (license license:bsd-3)))

;; Deprecated. Don’t use.
(define-public ghc-cabal-doctest
  (deprecated-package "ghc-cabal-doctest" cabal-doctest))

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
    (version "0.4.6.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "inspection-testing" version))
              (sha256
               (base32
                "0mxff0v3ciccbk4b8kxnh4752fzbwn7213qd8xji0csv6gi2w83y"))))
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
    (inputs
      (list ghc-quickcheck
            ghc-primitive
            ghc-primitive-addr
            ghc-quickcheck-classes-base
            ghc-aeson
            ghc-semigroupoids
            ghc-semirings
            ghc-vector))
    (native-inputs
      (list ghc-base-orphans
            ghc-tagged
            ghc-base-orphans
            ghc-tagged
            ghc-tasty
            ghc-tasty-quickcheck))
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
    (inputs
      (list ghc-quickcheck
            ghc-contravariant
            ghc-bifunctors
            ghc-semigroups
            ghc-fail
            ghc-tagged))
    (home-page "https://github.com/andrewthad/quickcheck-classes#readme")
    (synopsis "QuickCheck common typeclasses from `base`")
    (description
      "This library is a minimal variant of `quickcheck-classes`
that only provides laws for typeclasses from `base`. The main
purpose of splitting this out is so that `primitive` can depend
on `quickcheck-classes-base` in its test suite, avoiding the
circular dependency that arises if `quickcheck-classes` is used
instead. . This library provides QuickCheck properties to ensure
that typeclass instances adhere to the set of laws that they are
supposed to. There are other libraries that do similar things,
such as `genvalidity-hspec` and `checkers`. This library differs
from other solutions by not introducing any new typeclasses that
the user needs to learn. . /Note:/ on GHC < 8.5, this library
uses the higher-kinded typeclasses ('Data.Functor.Classes.Show1',
'Data.Functor.Classes.Eq1', 'Data.Functor.Classes.Ord1', etc.),
but on GHC >= 8.5, it uses `-XQuantifiedConstraints` to express
these constraints more cleanly.")
    (license license:bsd-3)))

(define-public ghc-doctest-lib
  (package
    (name "ghc-doctest-lib")
    (version "0.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "doctest-lib" version))
        (sha256
          (base32 "1vswam0dhw52dihgnzirh18gqs8rj8h6jd7pl6y1mg2f9f9zmih2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "doctest-lib")))
    (home-page "https://hub.darcs.net/thielema/doctest-lib/")
    (synopsis "Parts of doctest exposed as library")
    (description
      "Parts of doctest exposed as library. For use with the doctest-extract utility.")
    (license license:expat)))

(define-public ghc-doctest-exitcode-stdio
  (package
    (name "ghc-doctest-exitcode-stdio")
    (version "0.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "doctest-exitcode-stdio" version))
        (sha256
          (base32 "1g3c7yrqq2mwqbmvs8vkx1a3cf0p0x74b7fnn344dsk7bsfpgv0x"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "doctest-exitcode-stdio")))
    (inputs
      (list ghc-doctest-lib ghc-quickcheck ghc-semigroups))
    (arguments
     `(#:cabal-revision ("1"
                         "1065s8bch6zhl6mc8nhvfpwd1irmjd04z7xgycbpihc14x4ijim3")))
    (home-page "https://hub.darcs.net/thielema/doctest-exitcode-stdio/")
    (synopsis "Run Doctests in a @code{Cabal.Test.exitcode-stdio} environment")
    (description
      "This package allows on to run Doctests in a Cabal.Test.exitcode-stdio
environment.")
    (license license:bsd-3)))

(define-public ghc-tasty-silver
  (package
    (name "ghc-tasty-silver")
    (version "3.3.1.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tasty-silver" version))
              (sha256
               (base32
                "13j0zs0ciijv9q2nncna1gbgsgw2g7xc228hzmqic1750n3ybz9m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-silver")))
    (inputs (list ghc-ansi-terminal
                  ghc-async
                  ghc-optparse-applicative
                  ghc-process-extras
                  ghc-regex-tdfa
                  ghc-silently
                  ghc-tagged
                  ghc-tasty
                  ghc-temporary
                  ghc-semigroups))
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
    (version "0.1.0.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tasty-inspection-testing" version))
              (sha256
               (base32
                "0p46w44f19w7lvdzyg3vq6qzix0rjp8p23ilxz82dviq38lgmifp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-inspection-testing")))
    (inputs (list ghc-inspection-testing ghc-tasty))
    (home-page "https://github.com/Bodigrim/tasty-inspection-testing")
    (synopsis "Inspection testing support for tasty")
    (description
     "Integrate @@inspection-testing@@ into @@tasty@@ test suites.")
    (license license:expat)))

(define-public ghc-proctest
  (package
    (name "ghc-proctest")
    (version "0.1.3.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "proctest" version))
              (sha256
               (base32
                "02iz323arx9zwclvspgaaqz81bp6jdnj89pjm08n2gamg39zsbdn"))))
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
