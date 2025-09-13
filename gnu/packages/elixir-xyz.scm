;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Pierre-Henry Fröhring <phfrohring@deeplinks.com>
;;; Copyright © 2024 Igor Goryachev <igor@goryachev.org>
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
