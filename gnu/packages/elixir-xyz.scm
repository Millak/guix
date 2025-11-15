;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Pierre-Henry Fröhring <phfrohring@deeplinks.com>
;;; Copyright © 2024 Igor Goryachev <igor@goryachev.org>
;;; Copyright © 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>
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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages erlang-xyz)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages)
  #:use-module (guix build mix-build-system)
  #:use-module (guix build utils)
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "0a6gs7950gpkdax18x167g8v6dy4sbbx47cchglq7cqgh5i5hmlw"))))
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
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "1b3civqrznn3dxqa3iybwbpgj8dj6f7q1zlgr8gd5jzvh5mmdqfc"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-nimble-parsec))
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "lib/makeup/token/utils.ex"
               (("@precedence Hierarchy.hierarchy_to_precedence\\(@hierarchy\\)")
                ""))
             (substitute* "lib/makeup/token/utils.ex"
               (("@token_to_class_map Hierarchy.style_to_class_map\\(@hierarchy\\)")
                ""))
             (substitute* "lib/makeup/token/utils.ex"
               (("@standard_token_types Map.keys\\(@token_to_class_map\\)")
                ""))
             (substitute* "lib/makeup/token/utils.ex"
               (("@precedence")
                "Hierarchy.hierarchy_to_precedence(@hierarchy)"))
             (substitute* "lib/makeup/token/utils.ex"
               (("@token_to_class_map")
                "Hierarchy.style_to_class_map(@hierarchy)"))
             (substitute* "lib/makeup/token/utils.ex"
               (("@standard_token_types")
                "Map.keys(token_to_class_map())")))))))
    (synopsis "Syntax highlighter for source code")
    (description
     "Makeup is a generic syntax highlighter in the style of Pygments suitable
for use in code hosting, forums, wikis or other applications that need to prettify
source code.")
    (home-page "https://hexdocs.pm/makeup/")
    (license license:bsd-2)))

(define-public elixir-jason
  (package
    (name "elixir-jason")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "18d70i31bz11nr6vgsjn5prvhkvwqbyf3xq22ck5cnsnzp6ixc7v"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (synopsis "JSON parser and generator")
    (description
     "Parser and generator are written in pure Elixir and optimized for speed.  They
are at least twice as fast as other Elixir/Erlang libraries (e.g. Poison).  The
performance is comparable to jiffy, which is implemented in C as a NIF.")
    (home-page "https://hexdocs.pm/jason/")
    (license license:asl2.0)))

(define-public elixir-file-system
  (package
    (name "elixir-file-system")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "0vakv2hmqcj0ijxlbh8bvdwislxrvpcfxvracq7a3idfcqnhjlk7"))
       (snippet #~(begin
                    (use-modules (guix build utils) (ice-9 ftw))
                    (mkdir "source")
                    (for-each (lambda (file)
                                (when (not (member file '("." ".." "source")))
                                  (rename-file file (string-append "source/" file))))
                              (scandir "."))
                    (with-directory-excursion "source"
                      (invoke (string-append #+gzip "/bin/gunzip") "-v" "contents.tar.gz")
                      (invoke (string-append #+tar "/bin/tar") "-xvf" "contents.tar")
                      (delete-file "contents.tar")
                      (delete-file "priv/inotifywait.exe"))))))
    (build-system mix-build-system)
    (propagated-inputs (list inotify-tools))
    (arguments (list #:tests? #f)) ; no tests
    (synopsis "File system change watcher")
    (description "Provides a file system change watcher wrapper based on
https://github.com/synrc/fs.")
    (home-page "https://hexdocs.pm/file_system/")
    (license license:wtfpl2)))

(define-public elixir-accessible
  (package
    (name "elixir-accessible")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "accessible" version))
       (sha256
        (base32 "15pjj1gwc8y18di224yn5g02kxvlardld24a16wzg0mb2431p88k"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls))
    (synopsis "Dead-simple Access behaviour for custom structs")
    (description "This package-provides @code{elixir-accessible}, a library
providing @code{Access} behaviour for custom structs.")
    (home-page "https://hexdocs.pm/accessible/")
    (license license:expat)))

(define-public elixir-arrows
  (package
    (name "elixir-arrows")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "arrows" version))
       (sha256
        (base32 "17gv9xlf6kysa678b421gsvbrlfymvk03mk6rqmphygxyal1ppn3"))))
    (build-system mix-build-system)
    (synopsis "Handful of (mostly) arrow macros for Elixir")
    (description "This package provides @code{elixir-arrows}, a library
implementing a handful of (mostly) arrow macros.")
    (home-page "https://hexdocs.pm/arrows/")
    (license license:asl2.0)))

(define-public elixir-assert-value
  (package
    (name "elixir-assert-value")
    (version "0.10.5")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "assert_value" version))
       (sha256
        (base32 "1rkxnbgp86yll8gr62nimfr7d2c3afhp76ggqyr5avl85v5sx2ds"))))
    (build-system mix-build-system)
    (synopsis
     "Writes and updates tests for you")
    (description
     "This packag provides @{assert_value, an @code{ExUnit} assert
that writes and updates tests for you.")
    (home-page "https://hexdocs.pm/assert_value/")
    (license license:expat)))

(define-public elixir-beam-file
  (package
    (name "elixir-beam-file")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "beam_file" version))
       (sha256
        (base32 "0mlwrh5sxvz36qqqwbyhqz9xyx93c3bv04vjmpf4wrxdba79xa89"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls elixir-recode))
    (synopsis "Interface to the BEAM file format and a decompiler")
    (description "This package implements an interface to the BEAM file format
and a decompiler.")
    (home-page "https://hexdocs.pm/beam_file/")
    (license license:expat)))

(define-public elixir-brex-result
  (package
    (name "elixir-brex-result")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "brex_result" version))
       (sha256
        (base32 "1y243rg5kc4fhq62hr45agdy2m1afx2zhv65bpspwwj83k3sl8f2"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls))
    (synopsis
     "Handle common return values")
    (description
     "This package provides tools to handle common return values in Elixir.")
    (home-page "https://hexdocs.pm/brex_result/")
    (license license:expat)))

(define-public elixir-bunt
  (package
    (name "elixir-bunt")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "1ddnkg6w3iqzb8z18b7sr7jlmmbn9rf77w4nh1mzmxm512m8cpyw"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (synopsis "256 color ANSI coloring in the terminal")
    (description "Bunt is an Elixir package that enables 256 color ANSI
coloring in the terminal and allows users to alias colors to more semantic and
application-specific names1.")
    (home-page "https://hexdocs.pm/bunt/")
    (license license:expat)))

(define-public elixir-inch-ex
  (package
    (name "elixir-inch-ex")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "1a4rjcy3hn5pc40si1d1y7qg0b0mnxx6pw825la67ky8r9gfrl4n"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (propagated-inputs (list elixir-bunt elixir-jason))
    (synopsis
     "Suggest enhancements for inline documentation")
    (description
     "This package provides a Mix task that gives you hints where to improve your
inline docs.")
    (home-page "https://hex.pm/packages/inch_ex")
    (license license:expat)))

(define-public elixir-castore
  (package
    (name "elixir-castore")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "1xaw8n8g7qfygf8z8sz7f7pwmdl4lxshi9lj2b6j386jn2j8axys"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (synopsis "Up-to-date CA certificate store")
    (description "The Elixir castore package is a micro-library that provides
an up-to-date CA certificate store file for Elixir applications.")
    (home-page "https://hexdocs.pm/castore/")
    (license license:asl2.0)))

(define-public elixir-calendar-interval
  (package
    (name "elixir-calendar-interval")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "calendar_interval" version))
       (sha256
        (base32 "1c57ig10dpg6xz9i6lll51nxk0g8qphqfab2iyihh676100mwgf1"))))
    (build-system mix-build-system)
    (synopsis "Work with calendar intervals")
    (description
     "This package provides functions for working with calendar intervals.")
    (home-page "https://hexdocs.pm/calendar_interval/")
    (license license:asl2.0)))

(define-public elixir-excoveralls
  (package
    (name "elixir-excoveralls")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "138bls6hfk87mid2zfwsidk7j06yfich2iihyach7ckb2kdpjpyn"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-castore elixir-jason))
    (arguments (list #:tests? #f)) ; no tests
    (synopsis "Coverage report tool with coveralls.io integration")
    (description
     "Library that reports test coverage statistics, with the option to
post to coveralls.io service.  It uses Erlang's cover to generate coverage
information, and posts the test coverage results to coveralls.io through the
JSON API.")
    (home-page "https://hexdocs.pm/excoveralls/")
    (license license:expat)))

(define-public elixir-combine
  (package
    (name "elixir-combine")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "combine" version))
       (sha256
        (base32 "06s5y8b0snr1s5ax9v3s7rc6c8xf5vj6878d1mc7cc07j0bvq78v"))))
    (build-system mix-build-system)
    (synopsis "Parser combinator library for Elixir projects")
    (description
     "This package provides a parser combinator library for Elixir projects.")
    (home-page "https://hexdocs.pm/combine/")
    (license license:expat)))

(define-public elixir-credo
  (package
    (name "elixir-credo")
    (version "1.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "0bigri1xh29ims5gwh94pns6jys6h82pn6zhj0zxrb5ar6b79j4b"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (propagated-inputs (list elixir-bunt elixir-file-system elixir-jason))
    (native-inputs (list elixir-excoveralls elixir-inch-ex))
    (synopsis "Static code analysis tool")
    (description
     "Credo is a static code analysis tool for the Elixir language with a focus on
teaching and code consistency.  Credo can show you refactoring opportunities in
your code, complex code fragments, warn you about common mistakes, show
inconsistencies in your naming scheme and - if needed - help you enforce a
desired coding style.")
    (home-page "https://hexdocs.pm/credo/")
    (license license:expat)))

(define-public elixir-decimal
  (package
    (name "elixir-decimal")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "decimal" version))
       (sha256
        (base32 "1b5ffcvvzmzi7gpkahn36z5wyqc3wlli7rrhrz1lgjr9rdan7mm4"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-stream-data))
    (synopsis "Arbitrary precision decimal arithmetic")
    (description "This package provides @code{elixir-decimal}, a library that
represents values internally using three integers: a sign, a coefficient, and an
exponent.  In this way, numbers of any size and with any number of decimal
places can be represented exactly.")
    (home-page "https://hexdocs.pm/decimal/")
    (license license:asl2.0)))

(define-public elixir-decorator
  (package
    (name "elixir-decorator")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "decorator" version))
       (sha256
        (base32 "0zsrasbf6z3g7xs1s8gk5g7rf49ng1dskphqfif8gnl3j3fww1qa"))))
    (build-system mix-build-system)
    (synopsis "Function decorators for Elixir")
    (description "This package provides @code{elixir-decorator}, a library
implementing function decorators for Elixir.")
    (home-page "https://hexdocs.pm/decorator/")
    (license license:expat)))

(define-public elixir-erlex
  (package
    (name "elixir-erlex")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "1c7gcm9nhf7m3pq0776sav744ak1sph63shcpzvc6i58s5wmzn9y"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (synopsis
     "Convert Erlang style structs and error messages to equivalent Elixir")
    (description
     "Converted structs and error messages are useful for pretty printing
things like Dialyzer errors and Observer.")
    (home-page "https://hexdocs.pm/erlex/")
    (license license:asl2.0)))

(define-public elixir-escape
  (package
    (name "elixir-escape")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "escape" version))
       (sha256
        (base32 "0q9s3i071ijniplgbdimgnns5xg65xwwixwsrk32kr210yx1i6dj"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-prove))
    (synopsis "ANSI escape tool")
    (description "This package provides an ANSI escape tool in Elixir.")
    (home-page "https://hexdocs.pm/escape/")
    (license license:expat)))

(define-public elixir-ex-ulid
  (package
    (name "elixir-ex-ulid")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_ulid" version))
       (sha256
        (base32 "0q08y8xw2q4dr1w6g3rcyjlgib2w2wz26znycfan7i7bg93zvgm2"))))
    (build-system mix-build-system)
    (synopsis
     "Provides an ULID implementation in Elixir")
    (description
     "This package provides @code{elixir-ex-ulid} an implementation of
Universally Unique Lexicographically Sortable Identifier (ULID).")
    (home-page "https://hexdocs.pm/ex_ulid/")
    (license license:asl2.0)))

(define-public elixir-expo
  (package
    (name "elixir-expo")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "expo" version))
       (sha256
        (base32 "0q692ycwn0f5kif55vxhx2bq1swyraypf89n671l9yq08wzzkbgv"))))
    (build-system mix-build-system)
    (synopsis
     "Low-level Gettext file handling")
    (description
     "This package implements low-level Gettext file handling (.po/.pot/.mo file
writer and parser).")
    (home-page "https://hexdocs.pm/expo/")
    (license license:asl2.0)))

(define-public elixir-faker
  (package
    (name "elixir-faker")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "faker" version))
       (sha256
        (base32 "15qpx2rvapffjq8zsk720nnm3rhn91y33jcyx642g3npb0wxigdz"))))
    (build-system mix-build-system)
    (synopsis "Generate fake data")
    (description "Faker is a pure Elixir library for generating fake data.")
    (home-page "https://hexdocs.pm/faker/")
    (license license:expat)))

(define-public elixir-dialyxir
  (package
    (name "elixir-dialyxir")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "11m9hxs5byidqyxpzv34m1hwd69jcqqv2h81qfz0cl2wrmsznb5z"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (propagated-inputs (list elixir-erlex))
    (synopsis "Mix tasks to simplify use of Dialyzer")
    (description
     "Mix Tasks are usable from the directory of the mix project you want to analyze.")
    (home-page "https://hexdocs.pm/dialyxir/")
    (license license:asl2.0)))

(define-public elixir-geo
  (package
    (name "elixir-geo")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "geo" version))
       (sha256
        (base32 "0d19k98bwnpk3fyhd2zicjyix5chph8zpcbknl0zgacc76rv5v8r"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-jason))
    (synopsis "Encodes and decodes WKB, WKT, and GeoJSON formats")
    (description "This package provides @code{elixir-geo}, a library to encode
and decode WKB, WKT, and @code{GeoJSON} formats.")
    (home-page "https://hexdocs.pm/geo/")
    (license license:expat)))

(define-public elixir-gettext
  (package
    (name "elixir-gettext")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "gettext" version))
       (sha256
        (base32 "0x5azw61grknqmn6v7dh1ly8daa1xmxhc1jnsx7lm2an0xj9d0fc"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-castore
           elixir-excoveralls
           elixir-jason))
    (inputs
     (list gnu-gettext))
    (propagated-inputs (list elixir-expo))
    (synopsis "Internationalization and localization through gettext")
    (description "This package implements internationalization and localization
through gettext.")
    (home-page "https://hexdocs.pm/gettext/")
    (license license:asl2.0)))

(define-public elixir-git-diff
  (package
    (name "elixir-git-diff")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "git_diff" version))
       (sha256
        (base32 "0prs08cf2dlswrhhkrja557dgs5jarhjkz86ldhfk4bc2cy5c1cy"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls))
    (synopsis "Simple parser for output from @command{git diff}")
    (description
     "This package provides a simple parser for output from @command{git diff}.")
    (home-page "https://hexdocs.pm/git_diff/")
    (license license:expat)))

(define-public elixir-glob-ex
  (package
    (name "elixir-glob-ex")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "glob_ex" version))
       (sha256
        (base32 "1r2xa8h10d1ynrcdbwbq8fg34k4qd5kl2svnc5g19qsn60v2j9rl"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-prove))
    (synopsis "Library for glob expressions")
    (description "This package provides a library for glob expressions.")
    (home-page "https://hexdocs.pm/glob_ex/")
    (license license:expat)))

(define-public elixir-json-serde
  (package
    (name "elixir-json-serde")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "json_serde" version))
       (sha256
        (base32 "08irad6l6v6rblxmzh5vcv087q1xim0rhzjl6x9ypz3fq7xcsyha"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-brex-result elixir-decimal elixir-jason))
    (synopsis
     "Serializes and deserializes nested datastructures")
    (description
     "@code{JsonSerde} serializes and deserializes nested Elixir datastructures,
including custom structs.")
    (home-page "https://hexdocs.pm/json_serde/")
    (license license:asl2.0)))

(define-public elixir-ham
  (package
    (name "elixir-ham")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ham" version))
       (sha256
        (base32 "16m6yvr00dri38fcli5fm69rwl2i0mvqdcclbwmx78z5q22cc75p"))))
    (build-system mix-build-system)
    (synopsis "Runtime Type checking")
    (description "This package provides @code{ham}, a library to validate
function arguments and return values against their typespecs.")
    (home-page "https://hexdocs.pm/ham/")
    (license license:asl2.0)))

(define-public elixir-igniter
  (package
    (name "elixir-igniter")
    (version "0.6.28")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "igniter" version))
       (sha256
        (base32 "1m17dg9yydr5mv65nphcc6c2bcrgcyi62xpikq3j3jmf4vb6k4xd"))))
    (build-system mix-build-system)
    (arguments
     ;; FIXME: Tests depend on elixir-eflame, which is not yet packaged.
     (list #:tests? #f))
    (propagated-inputs
     (list elixir-glob-ex
           elixir-jason
           elixir-rewrite
           elixir-sourceror
           elixir-spitfire))
    (synopsis "Code generation and project patching framework")
    (description
     "This package provides a code generation and project patching framework.")
    (home-page "https://hexdocs.pm/igniter/")
    (license license:expat)))

(define-public elixir-machete
  (package
    (name "elixir-machete")
    (version "0.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "0xxfbk888q2x3fdkh4rl3n53sya7cngxax55md2lcd6ggcn0cn1d"))))
    (build-system mix-build-system)
    (native-inputs (list elixir-credo elixir-dialyxir))
    (synopsis "Literate test matchers for ExUnit")
    (description
     "Machete provides ergonomic match operators that act as building blocks to let
you define test expectations that can match data against any combination of
literals, variables, or parametrically defined matchers.")
    (home-page "https://hexdocs.pm/machete/")
    (license license:expat)))

(define-public elixir-mime
  (package
    (name "elixir-mime")
    (version "2.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "mime" version))
       (sha256
        (base32 "1k8cx1qwgwshc3gx4hg2w9r9cvgb8pj6rdy5zwin1qcy7671hwb1"))))
    (build-system mix-build-system)
    (synopsis "MIME types module")
    (description "This package provides @code{elixir-mime}, a MIME type module
for Elixir.")
    (home-page "https://hexdocs.pm/mime/")
    (license license:asl2.0)))

(define-public elixir-mimic
  (package
    (name "elixir-mimic")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "mimic" version))
       (sha256
        (base32 "0mh9dx5v5zxghjlgxlgxkn74x0jw08iszb88c9809v9k16kw4qcx"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-ham))
    (synopsis "Mocks for Elixir functions")
    (description "This package provides @code{mimic} a mocking library for
Elixir.")
    (home-page "https://hexdocs.pm/mimic/")
    (license license:asl2.0)))

(define-public elixir-mox
  (package
    (name "elixir-mox")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "mox" version))
       (sha256
        (base32 "16h84745x12h7zrw305mzi93q092pvblr55gxrz4mqlyqqy2pff7"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-castore
           elixir-excoveralls))
    (propagated-inputs (list elixir-nimble-ownership))
    (synopsis "Mocks and explicit contracts")
    (description "This package provides @code{mox}, a library to mock and
explicit contracts for Elixir.")
    (home-page "https://hexdocs.pm/mox/")
    (license license:asl2.0)))

(define-public elixir-nebulex
  (package
    (name "elixir-nebulex")
    (version "2.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "nebulex" version))
       (sha256
        (base32 "1maq362prnjxigj3gkd6j9fy54ff4mj97r6aqz7gyqw45yvcpk89"))))
    (build-system mix-build-system)
    (arguments
     ;;Tests depend on elixir-ex2ms which is not packaged yet.
     (list #:tests? #f))
    (propagated-inputs (list elixir-decorator erlang-shards erlang-telemetry))
    (synopsis "In-memory and distributed caching toolkit for Elixir")
    (description "In-memory and distributed caching toolkit for Elixir.")
    (home-page "https://hexdocs.pm/nebulex/")
    (license license:expat)))

(define-public elixir-nimble-options
  (package
    (name "elixir-nimble-options")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "nimble_options" version))
       (sha256
        (base32 "0i0bcmpsc02ga2llakgcvnw734rqn2dzx0j8k2vc8hllr9q286w2"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-castore
           elixir-excoveralls))
    (synopsis
     "Validates and documents high-level options")
    (description
     "This package provides a tiny library for validating and documenting high-level
options.")
    (home-page "https://hexdocs.pm/nimble_options/")
    (license license:asl2.0)))

(define-public elixir-nimble-ownership
  (package
    (name "elixir-nimble-ownership")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "nimble_ownership" version))
       (sha256
        (base32 "0bdj7fvkddh8mllicqb92caxqiwcnvws3r7kycczar2l09hy899q"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-castore elixir-excoveralls))
    (synopsis "Track ownership of resources across processes")
    (description "This package provides @code{nimble_ownership}, an Elixir
library to track ownership of resources across processes.")
    (home-page "https://hexdocs.pm/nimble_ownership/")
    (license license:asl2.0)))

(define-public elixir-owl
  (package
    (name "elixir-owl")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "owl" version))
       (sha256
        (base32 "0xbl5wykss5na8l31vf4kr0vz4qmsszqzdkwyncdp91prq8rvgsr"))))
    (build-system mix-build-system)
    (native-inputs (list elixir-excoveralls))
    (propagated-inputs (list elixir-ucwidth))
    (synopsis "Toolkit for writing command-line user interfaces")
    (description
     "This package provides a toolkit for writing command-line user interfaces.")
    (home-page "https://hexdocs.pm/owl/")
    (license license:asl2.0)))

(define-public elixir-pathex
  (package
    (name "elixir-pathex")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "pathex" version))
       (sha256
        (base32 "1sabkkng5w6fq1v4vysy13vh2rh0sfjzfw3hfy3sism2bx5qx7qm"))))
    (build-system mix-build-system)
    (synopsis "Functional lenses for nested structures")
    (description "This library implements functional lenses for nested
structures in Elixir.")
    (home-page "https://hexdocs.pm/pathex/")
    (license license:bsd-2)))

(define-public elixir-poison
  (package
    (name "elixir-poison")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "poison" version))
       (sha256
        (base32 "18irww20fya32rp03q0r2fzbf1qw51w6lbb4chwmlxwl5din945v"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-decimal))
    (synopsis "Fast, pure Elixir JSON library")
    (description "An incredibly fast, pure Elixir JSON library.")
    (home-page "https://hexdocs.pm/poison/")
    (license license:bsd-0)))

(define-public elixir-process-tree
  (package
    (name "elixir-process-tree")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "process_tree" version))
       (sha256
        (base32 "04z76m5vpbmhl4apsszy93g2a3iw02kg2dvhvbp1ld8l0nzydvk8"))))
    (build-system mix-build-system)
    (synopsis "Avoid global state in Elixir applications")
    (description
     "This package provides a module for avoiding global state in Elixir
applications.")
    (home-page "https://hexdocs.pm/process_tree/")
    (license license:expat)))

(define-public elixir-prove
  (package
    (name "elixir-prove")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "prove" version))
       (sha256
        (base32 "1hkw70cbq82jjyyqdiaspi5a9v4m7z625h9z9hym0p1ziml25k2r"))))
    (build-system mix-build-system)
    (synopsis
     "Provides the macros @code{prove} and @code{batch}")
    (description
     "This package provides the macros @code{prove} and @code{batch} to write
shorter tests.")
    (home-page "https://hexdocs.pm/prove/")
    (license license:expat)))

(define-public elixir-recase
  (package
    (name "elixir-recase")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "recase" version))
       (sha256
        (base32 "072hnq686lhwiwlbcz8hxid0aj0rlvvacdrhfb8qi28jpng599zg"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls))
    (synopsis "Convert strings to any case you need")
    (description "@code{Recase} helps you to convert a string from any case to
any case.")
    (home-page "https://hexdocs.pm/recase/")
    (license license:expat)))

(define-public elixir-recode
  (package
    (name "elixir-recode")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "recode" version))
       (sha256
        (base32 "1n3gqgcl1qx5h1z9yqx28nbavgydvivm3cr3qifnbdwry07zb365"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls elixir-mox))
    (propagated-inputs
     (list elixir-escape elixir-glob-ex elixir-rewrite))
    (synopsis "Linter with autocorrection")
    (description "This package provides an experimental linter with
autocorrection.")
    (home-page "https://hexdocs.pm/recode/")
    (license license:expat)))

(define-public elixir-rewrite
  (package
    (name "elixir-rewrite")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "rewrite" version))
       (sha256
        (base32 "1m1r7mq14xvxmnlihx7scn599ljrlpnbzdz8nd3hm3ajwfqr92vz"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls))
    (propagated-inputs (list elixir-glob-ex elixir-sourceror elixir-text-diff))
    (synopsis
     "Rewrite sources in an Elixir project")
    (description
     "This package implements an API for rewriting sources in an Elixir project
powered by sourceror.")
    (home-page "https://hexdocs.pm/rewrite/")
    (license license:expat)))

(define-public elixir-sizeable
  (package
    (name "elixir-sizeable")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "sizeable" version))
       (sha256
        (base32 "0qhpc1h5iks04mcb2ypp7pkjhh9skq586l6a02s7g9zvdn759asb"))))
    (build-system mix-build-system)
    (synopsis "Make file sizes human-readable")
    (description "This package provides an Elixir library to make file sizes
human-readable.")
    (home-page "https://hexdocs.pm/sizeable/")
    (license license:expat)))

(define-public elixir-sourceror
  (package
    (name "elixir-sourceror")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "sourceror" version))
       (sha256
        (base32 "1wdkqiszn0483g2wfp85s15mz08xzhic9zbfisfrqmh45v4xznr9"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls))
    (synopsis "Utilities to work with Elixir source code")
    (description "This package provides @code{elixir-sourceror}, a library
implementing utilities to work with Elixir source code.")
    (home-page "https://hexdocs.pm/sourceror/")
    (license license:asl2.0)))

(define-public elixir-spitfire
  (package
    (name "elixir-spitfire")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "spitfire" version))
       (sha256
        (base32 "06yr860zhzxmzmljdpjq4c4n81951axl2kc1w6r430x3ai8dgvkf"))))
    (build-system mix-build-system)
    (synopsis "Error resilient parser for Elixir")
    (description "This package provides @code{spitfire}, an error resilient
parser for Elixir.")
    (home-page "https://hexdocs.pm/spitfire/")
    (license license:expat)))

(define-public elixir-stream-data
  (package
    (name "elixir-stream-data")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "stream_data" version))
       (sha256
        (base32 "1vd9i4zvg4nwk0bf17yss4r4pcd58f4zdva38qqj0sa6wdp58p7b"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls))
    (synopsis "Data generation and property-based testing for Elixir")
    (description "@code{StreamData} is an Elixir library for data generation and
property-based testing.")
    (home-page "https://hexdocs.pm/stream_data/")
    (license license:asl2.0)))

(define-public elixir-telemetry-metrics
  (package
    (name "elixir-telemetry-metrics")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "telemetry_metrics" version))
       (sha256
        (base32 "0ryfr1mwam89vpssisb6yc0n9ilffz8j6rlavfvasw6yvy6rxdz7"))))
    (build-system mix-build-system)
    (propagated-inputs (list erlang-telemetry))
    (synopsis
     "Common interface for defining metrics based on Telemetry events")
    (description
     "This package provides a common interface for defining metrics based on
Telemetry events.")
    (home-page "https://hexdocs.pm/telemetry_metrics/")
    (license license:asl2.0)))

(define-public elixir-thousand-island
  (package
    (name "elixir-thousand-island")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "thousand_island" version))
       (sha256
        (base32 "1hdnbxv4015y3hc9aq6zgs6n4y092rkax1kjp24qb0fjwm08cji0"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-jason))
    (propagated-inputs (list elixir-machete erlang-telemetry))
    (synopsis "Pure Elixir socket server")
    (description
     "This package provides a simple and modern Elixir socket server.")
    (home-page "https://hexdocs.pm/thousand_island/")
    (license license:expat)))

(define-public elixir-text-diff
  (package
    (name "elixir-text-diff")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "text_diff" version))
       (sha256
        (base32 "1myvc8rym843amsygb21j1jf0xzq6pj85akdgcslk3ikmgnamzyi"))))
    (build-system mix-build-system)
    (synopsis "Returns a formatted diff between two strings")
    (description
     "This package provides @code{TextDiff}, an Elixir library that returns a
formatted diff between two strings.")
    (home-page "https://hexdocs.pm/text_diff/")
    (license license:expat)))

(define-public elixir-timex
  (package
    (name "elixir-timex")
    (version "3.7.13")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "timex" version))
       (sha256
        (base32 "0m2cdmp172zpi6ska9yky2rj2qr4863zvd5qfglji4v6482qwn09"))))
    (build-system mix-build-system)
    (arguments
     (list
      ;; Tests appear to require network.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-locales
            (lambda _
              (mkdir-p "config")
              (call-with-output-file "config/config.exs"
                (lambda (port)
                  (display "import Config

config :gettext, :default_locale, \"en\"\n" port))))))))
    (propagated-inputs (list elixir-combine elixir-gettext elixir-tzdata))
    (synopsis
     "Comprehensive date/time library for Elixir projects")
    (description
     "Timex is a rich, comprehensive Date/Time library for Elixir projects, with
full timezone support via the @code{:tzdata} package.  If you need to manipulate
dates, times, datetimes, timestamps, etc., then Timex is for you.")
    (home-page "https://hexdocs.pm/timex/")
    (license license:expat)))

(define-public elixir-traverse
  (package
    (name "elixir-traverse")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "traverse" version))
       (sha256
        (base32 "0w8ww017nlfgqm0n73n0s72nk97xys4ayxh4sz93dbscjydfry8c"))))
    (build-system mix-build-system)
    (synopsis
     "Walk arbitrary Elixir Datastructures in a functional way")
    (description
     "Traverse is a toolset to walk arbitrary Elixir Datastructures in a
functional way.")
    (home-page "https://hexdocs.pm/traverse/")
    (license license:asl2.0)))

(define-public elixir-tzdata
  (package
    (name "elixir-tzdata")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "tzdata" version))
       (sha256
        (base32 "0a0crg70vpxc0bqdnhw7jdjv3prfj6aywls2x7a2jk86b9bqbjnl"))))
    (build-system mix-build-system)
    (arguments
     ;; FIXME: Tests fail with:
     ;;  Application hackney exited: exited in: :hackney_app.start(:normal, [])
     (list #:tests? #f))
    (propagated-inputs (list erlang-hackney))
    (synopsis "Parser and library for the tz database")
    (description "Tzdata is a parser and library for the tz database.")
    (home-page "https://hexdocs.pm/tzdata/")
    (license license:expat)))

(define-public elixir-ucwidth
  (package
    (name "elixir-ucwidth")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ucwidth" version))
       (sha256
        (base32 "1gsmfcbrdfa00czm8870pr3kc34wvnisyg7c5gxi3swfidwx3vy1"))))
    (build-system mix-build-system)
    (arguments
     ;; Tests depend on elixir-quixir which is not packaged yet.
     (list #:tests? #f))
    (synopsis
     "Port of ucwidth from C to Elixir")
    (description
     "This package provides a port of ucwidth from C to Elixir, for determining the
width (full-width or half-width) of an Unicode character.")
    (home-page "https://hexdocs.pm/ucwidth/")
    (license license:expat)))

(define-public elixir-untangle
  (package
    (name "elixir-untangle")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "untangle" version))
       (sha256
        (base32 "0p81bzvsbnzdkqjq441xslcryxhpmrl4pqda2h323ivs47ij1wa7"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-decorator))
    (synopsis "Logging and inspecting with code location information")
    (description "Untangle provides alternatives for @code{IO.inspect} and the
macros in Elixir's @code{Logger} to output code location information.  It also
provides a polyfill for @code{dbg} which was introduced in Elixir 1.14.")
    (home-page "https://hexdocs.pm/untangle/")
    (license license:asl2.0)))

(define-public elixir-verbs
  (package
    (name "elixir-verbs")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "verbs" version))
       (sha256
        (base32 "19nbkwrphns1yjk0rca1zq6v43m878r7750ppf4av3kcf58328hr"))))
    (build-system mix-build-system)
    (synopsis "Conjugates English verbs")
    (description "This package provides a library to conjugate English verbs.")
    (home-page "https://hexdocs.pm/verbs/")
    (license license:expat)))

(define-public elixir-zest
  (package
    (name "elixir-zest")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "zest" version))
       (sha256
        (base32 "0jlpldb94wm1v2kavvsy5h7w5mvjmxnkssl48mp2iphmysnddqpb"))))
    (build-system mix-build-system)
    (synopsis "Fresh approach to testing in Elixir")
    (description "This package provides a fresh approach to testing.")
    (home-page "https://hexdocs.pm/zest/")
    (license license:asl2.0)))

(define-public elixir-zstream
  (package
    (name "elixir-zstream")
    (version "0.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "zstream" version))
       (sha256
        (base32 "0kb3il3z52v4cdbsd8785qxx8qsnfm2d1hb9rcfdmz0cy3h3mi28"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls))
    (synopsis "Streaming zip file writer and reader")
    (description "This package provides a streaming zip file writer and reader.")
    (home-page "https://hexdocs.pm/zstream/")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
