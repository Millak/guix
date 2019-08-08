;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-autocfg
  (package
    (name "rust-autocfg")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "autocfg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0asl6fnc35yk5l2rxwhp25v128jgm45dp754h9z8x51b6n90w4r2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cuviper/autocfg")
    (synopsis "Automatic cfg for Rust compiler features")
    (description "Rust library for build scripts to automatically configure
code based on compiler support.  Code snippets are dynamically tested to see
if the @code{rustc} will accept them, rather than hard-coding specific version
support.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-bencher
  (package
    (name "rust-bencher")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bencher" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1x8p2xblgqssay8cdykp5pkfc0np0jk5bs5cx4f5av097aav9zbx"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/bencher/")
    (synopsis "Port of the libtest benchmark runner to Rust stable")
    (description "This package provides a port of the libtest (unstable Rust)
benchmark runner to Rust stable releases.  Supports running benchmarks and
filtering based on the name.  Benchmark execution works exactly the same way
and no more (caveat: black_box is still missing!).")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-bitflags
  (package
    (name "rust-bitflags")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1zc1qb1hwsnl2d8rhzicsv9kqd5b2hwbrscrcfw5as4sfr35659x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "Macro to generate structures which behave like bitflags")
    (description "This package provides a macro to generate structures which
behave like a set of bitflags.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cfg-if
  (package
    (name "rust-cfg-if")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg-if" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0csygklgz3ybpr0670rkip49zh76m43ar3k7xgypkzbzrwycx1ml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis "Define an item depending on parameters")
    (description "This package provides a macro to ergonomically define an item
depending on a large number of #[cfg] parameters.  Structured like an
@code{if-else} chain, the first matching branch is the item that gets emitted.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-discard
  (package
    (name "rust-discard")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "discard" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h67ni5bxvg95s91wgicily4ix7lcw7cq0a5gy9njrybaibhyb91"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Pauan/rust-discard")
    (synopsis "Allow for intentionally leaking memory")
    (description "There are situations where you need to intentionally leak some
memory but not other memory.  This package provides a discard trait which allows
for intentionally leaking memory")
    (license license:expat)))

(define-public rust-doc-comment
  (package
    (name "rust-doc-comment")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "doc-comment" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15rsqxgarfpb1yim9sbp9yfgj7p2dq6v51c6bq1a62paii9ylgcj"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/GuillaumeGomez/doc-comment")
    (synopsis "Macro to generate doc comments")
    (description "This package provides a way to generate doc comments
from macros.")
    (license license:expat)))

(define-public rust-dtoa
  (package
    (name "rust-dtoa")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dtoa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0phbm7i0dpn44gzi07683zxaicjap5064w62pidci4fhhciv8mza"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/dtoa")
    (synopsis "Fast functions for printing floating-point primitives")
    (description "This crate provides fast functions for printing
floating-point primitives to an @code{io::Write}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fallible-iterator
  (package
    (name "rust-fallible-iterator")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fallible-iterator" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xq759lsr8gqss7hva42azn3whgrbrs2sd9xpn92c5ickxm1fhs4"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/sfackler/rust-fallible-iterator")
    (synopsis "Fallible iterator traits")
    (description "If the @code{std} or @code{alloc} features are enabled, this
crate provides implementations for @code{Box}, @code{Vec}, @code{BTreeMap}, and
@code{BTreeSet}.  If the @code{std} feature is enabled, this crate additionally
provides implementations for @code{HashMap} and @code{HashSet}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fnv
  (package
    (name "rust-fnv")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fnv" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ww56bi1r5b8id3ns9j3qxbi7w5h005rzhiryy0zi9h97raqbb9g"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/rust-fnv")
    (synopsis "implementation of the Fowler-Noll-Vo hash function")
    (description "The @code{fnv} hash function is a custom @code{Hasher}
implementation that is more efficient for smaller hash keys.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fs-extra
  (package
    (name "rust-fs-extra")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fs_extra" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0x6675wdhsx277k1k1235jwcv38naf20d8kwrk948ds26hh4lajz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/webdesus/fs_extra")
    (synopsis "Extra filesystem methods")
    (description "Expanding opportunities standard library @code{std::fs} and
@code{std::io}.  Recursively copy folders with recept information about
process and much more.")
    (license license:expat)))

(define-public rust-futures
  (package
    (name "rust-futures")
    (version "0.1.28")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0saq8ffjw1pwf1pzhw3kq1z7dfq6wpd8x93dnni6vbkc799kkp25"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/futures-rs")
    (synopsis "Implementation of zero-cost futures in Rust")
    (description "An implementation of @code{futures} and @code{streams}
featuring zero allocations, composability, and iterator-like interfaces.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hex
  (package
    (name "rust-hex")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0xsdcjiik5j750j67zk42qdnmm4ahirk3gmkmcqgq7qls2jjcl40"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/KokaKiwi/rust-hex")
    (synopsis "Encode and decode data to/from hexadecimals")
    (description "This crate allows for encoding and decoding data into/from
hexadecimal representation.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-itoa
  (package
    (name "rust-itoa")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0zvg2d9qv3avhf3d8ggglh6fdyw8kkwqg3r4622ly5yhxnvnc4jh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/itoa")
    (synopsis "Fast functions for printing integer primitives")
    (description "This crate provides fast functions for printing integer
primitives to an @code{io::Write}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-json
  (package
    (name "rust-json")
    (version "0.11.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "json" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1hj8c6xj5c2aqqszi8naaflmcdbya1i9byyjrq4iybxjb4q91mq1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/maciejhirsz/json-rust")
    (synopsis "JSON implementation in Rust")
    (description "This crate provides a JSON implementation in Rust, reducing
friction with idiomatic Rust structs to ease interopability.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-maplit
  (package
    (name "rust-maplit")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "maplit" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0hsczmvd6zkqgzqdjp5hfyg7f339n68w83n4pxvnsszrzssbdjq8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/maplit")
    (synopsis "Collection of Map macros")
    (description "This crate provides a collection of @code{literal} macros for
@code{HashMap}, @code{HashSet}, @code{BTreeMap}, and @code{BTreeSet.}")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-matches
  (package
    (name "rust-matches")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matches" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "020axl4q7rk9vz90phs7f8jas4imxal9y9kxl4z4v7a6719mrz3z"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/SimonSapin/rust-std-candidates")
    (synopsis "Macro to evaluate whether an expression matches a pattern.")
    (description "This package provides a macro to evaluate, as a boolean,
whether an expression matches a pattern.")
    (license license:expat)))

(define-public rust-md5
  (package
    (name "rust-md5")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md5" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "17b2xm4h4cvxsdjsf3kdrzqv2za60kak961xzi5kmw6g6djcssvy"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/stainless-steel/md5")
    (synopsis "MD5 hash function in Rust")
    (description "The package provides the MD5 hash function.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-peeking-take-while
  (package
    (name "rust-peeking-take-while")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "peeking_take_while" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16bhqr6rdyrp12zv381cxaaqqd0pwysvm1q8h2ygihvypvfprc8r"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/fitzgen/peeking_take_while")
    (synopsis "Provides the peeking_take_while iterator adaptor method")
    (description
      "Like @code{Iterator::take_while}, but calls the predicate on a peeked
value.  This allows you to use @code{Iterator::by_ref} and
@code{Iterator::take_while} together, and still get the first value for which
the @code{take_while} predicate returned false after dropping the @code{by_ref}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-percent-encoding
  (package
    (name "rust-percent-encoding")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "percent-encoding" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0m6rkp3iy11la04p6z3492rns6n693pvmx585dvfmzzlzak2hkxs"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/rust-url/")
    (synopsis "Percent encoding and decoding")
    (description "This crate provides percent encoding and decoding.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pin-utils
  (package
    (name "rust-pin-utils")
    (version "0.1.0-alpha.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pin-utils" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "11xmyx00n4m37d546by2rxb8ryxs12v55cc172i3yak1rqccd52q"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/pin-utils")
    (synopsis "Utilities for pinning")
    (description "This crate provides utilities for pinning values on the stack.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-plain
  (package
    (name "rust-plain")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "plain" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/randomites/plain")
    (synopsis "Rust library that allows reinterpreting data safely")
    (description "This package provides a small Rust library that allows users
 to reinterpret data of certain types safely.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pocket-resources
  (package
    (name "rust-pocket-resources")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pocket-resources" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1n2i5vmi8fdbw89wm5nz1ws1z9f1qax911p6ksg4scmdg23z6df1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tomaka/pocket-resources")
    (synopsis "Include resources in your applications")
    (description "This crate allows you to include resources in your
applications.")
    (license license:expat)))

(define-public rust-ppv-lite86
  (package
    (name "rust-ppv-lite86")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ppv-lite86" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06snnv338w341nicfqba2jgln5dsla72ndkgrw7h1dfdb3vgkjz3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cryptocorrosion/cryptocorrosion")
    (synopsis "Implementation of the crypto-simd API for x86")
    (description "This crate provides an implementation of the crypto-simd API
for x86.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-proc-macro2
  (package
    (name "rust-proc-macro2")
    (version "0.4.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nd71fl24sys066jrha6j7i34nfkjv44yzw8yww9742wmc8j0gfg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-unicode-xid" ,rust-unicode-xid))
        #:cargo-development-inputs (("rust-quote" ,rust-quote))))
    (home-page "https://github.com/alexcrichton/proc-macro2")
    (synopsis "Stable implementation of the upcoming new `proc_macro` API")
    (description "This package provides a stable implementation of the upcoming new
`proc_macro` API.  Comes with an option, off by default, to also reimplement itself
in terms of the upstream unstable API.")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-quick-error
  (package
    (name "rust-quick-error")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quick-error" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1w6kgwwv7p7zr0yyg5rb315lkk24bimywklwx7fsvsbwi10bjx4j"))))
    (build-system cargo-build-system)
    (home-page "http://github.com/tailhook/quick-error")
    (synopsis "Macro which makes error types pleasant to write")
    (description "This crate provides a macro which makes error types pleasant
to write.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-quote
  (package
    (name "rust-quote")
    (version "0.6.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nw0klza45hf127kfyrpxsxd5jw2l6h21qxalil3hkr7bnf7kx7s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-rustc-std-workspace-core
  (package
    (name "rust-rustc-std-workspace-core")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-std-workspace-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1309xhwyai9xpz128xrfjqkmnkvgjwddznmj7brbd8i8f58zamhr"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rustc-std-workspace-core")
    (synopsis "Explicitly empty crate for rust-lang/rust integration")
    (description "This crate provides an explicitly empty crate for
rust-lang/rust integration.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-scoped-tls
  (package
    (name "rust-scoped-tls")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scoped-tls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1hj8lifzvivdb1z02lfnzkshpvk85nkgzxsy2hc0zky9wf894spa"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/scoped-tls")
    (synopsis "Rust library providing the old standard library's scoped_thread_local")
    (description "This crate provides a library implementation of the standard
library's old @code{scoped_thread_local!} macro for providing scoped access to
@dfn{thread local storage} (TLS) so any type can be stored into TLS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-scopeguard
  (package
    (name "rust-scopeguard")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "03aay84r1f6w87ckbpj6cc4rnsxkxcfs13n5ynxjia0qkgjiabml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/scopeguard")
    (synopsis "Scope guard which will run a closure even out of scope")
    (description "This package provides a RAII scope guard that will run a
given closure when it goes out of scope, even if the code between panics
(assuming unwinding panic).  Defines the macros @code{defer!},
@code{defer_on_unwind!}, @code{defer_on_success!} as shorthands for guards
with one of the implemented strategies.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-semver-parser
  (package
    (name "rust-semver-parser")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver-parser" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ahqhvgpzhcsd28id7xnrjv4419i9yyalhm7d7zi430qx0hi2vml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/steveklabnik/semver-parser")
    (synopsis "Parsing of the semver spec")
    (description "This package provides for parsing of the semver spec.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-shlex
  (package
    (name "rust-shlex")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shlex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1lmv6san7g8dv6jdfp14m7bdczq9ss7j7bgsfqyqjc3jnjfippvz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/comex/rust-shlex")
    (synopsis "Split a string into shell words, like Python's shlex")
    (description "This crate provides a method to split a string into shell
words, like Python's shlex.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-slab
  (package
    (name "rust-slab")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "slab" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1y59xsa27jk84sxzswjk60xcjf8b4fm5960jwpznrrcmasyva4f1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/carllerche/slab")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description "This create provides a pre-allocated storage for a uniform
data type.")
    (license license:expat)))

(define-public rust-spin
  (package
    (name "rust-spin")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spin" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0m9clchsj0rf13bggsgvbv9haiy0f6rhvnvkpvkk8720a5pkydj4"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mvdnes/spin-rs.git")
    (synopsis "Synchronization primitives based on spinning")
    (description "This crate provides synchronization primitives based on
spinning.  They may contain data, are usable without @code{std},and static
initializers are available.")
    (license license:expat)))

(define-public rust-stdweb-internal-runtime
  (package
    (name "rust-stdweb-internal-runtime")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-runtime" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nhpyra7glbwcpakhpj5a3d7h7kx1ynif473nzshmk226m91f8ym"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal runtime for the @code{stdweb} crate")
    (description "This crate provides internal runtime for the @code{stdweb}
crate.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-strsim
  (package
    (name "rust-strsim")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xphwhf86yxxmcpvm4mikj8ls41f6nf7gqyjm98b74mfk81h6b03"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis "Rust implementations of string similarity metrics")
    (description "This crate includes implementations of string similarity
metrics.  It includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro,
and Jaro-Winkler.")
    (license license:expat)))

(define-public rust-synstructure-test-traits
  (package
    (name "rust-synstructure-test-traits")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "synstructure_test_traits" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1b3fs2b9kc1gy9dilaxqjbdl4z0mlrbbxjzkprdx953rif1c3q66"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/synstructure_test_traits")
    (synopsis "Helper test traits for synstructure doctests")
    (description
     "This package provides helper test traits for synstructure doctests.")
    (license license:expat)))

(define-public rust-typenum
  (package
    (name "rust-typenum")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typenum" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0sc1jirllfhdi52z1xv9yqzxzpk6v7vadd13n7wvs1wnjipn6bb1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/paholg/typenum")
    (synopsis "Rust library for type-level numbers evaluated at compile time")
    (description "Typenum is a Rust library for type-level numbers evaluated at
compile time.  It currently supports bits, unsigned integers, and signed
integers.  It also provides a type-level array of type-level numbers, but its
implementation is incomplete.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-xid
  (package
    (name "rust-unicode-xid")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z57lqh4s18rr4x0j4fw4fmp9hf9346h0kmdgqsqx0fhjr3k0wpw"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine Unicode XID related properties")
    (description "Determine whether characters have the XID_Start
or XID_Continue properties according to Unicode Standard Annex #31.")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))
