;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Pierre-Henry Fröhring <phfrohring@deeplinks.com>
;;; Copyright © 2024 Igor Goryachev <igor@goryachev.org>
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
    (version "0.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "1p0myxmnjjds8bbg69dd6fvhk8q3n7lb78zd4qvmjajnzgdmw6a1"))
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

(define-public elixir-bunt
  (package
    (name "elixir-bunt")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "19bp6xh052ql3ha0v3r8999cvja5d2p6cph02mxphfaj4jsbyc53"))))
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
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "1y44amb8falsmrfzpkmf7qp6215g9kdl76g91dpna4af2jwc264l"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (synopsis "Up-to-date CA certificate store")
    (description "The Elixir castore package is a micro-library that provides
an up-to-date CA certificate store file for Elixir applications.")
    (home-page "https://hexdocs.pm/castore/")
    (license license:asl2.0)))

(define-public elixir-excoveralls
  (package
    (name "elixir-excoveralls")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "02x69ll5scvraky0k5gacvnnmldv5k04kgk02x087d9w3y8vn28i"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-castore elixir-jason))
    (arguments (list #:tests? #f)) ; no tests
    (synopsis "Coverage report tool with coveralls.io integration")
    (description
     "Library that reports test coverage statistics, with the option to
post to coveralls.io service. It uses Erlang's cover to generate coverage
information, and posts the test coverage results to coveralls.io through the
JSON API.")
    (home-page "https://hexdocs.pm/excoveralls/")
    (license license:expat)))

(define-public elixir-credo
  (package
    (name "elixir-credo")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "18jqi9s9r1587njzdxycvmmbma30cay9iamni4f3ih54jmh1r1z9"))))
    (build-system mix-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (propagated-inputs (list elixir-bunt elixir-file-system elixir-jason))
    (native-inputs (list elixir-excoveralls elixir-inch-ex))
    (synopsis "Static code analysis tool")
    (description
     "Credo is a static code analysis tool for the Elixir language with a focus on
teaching and code consistency. Credo can show you refactoring opportunities in
your code, complex code fragments, warn you about common mistakes, show
inconsistencies in your naming scheme and - if needed - help you enforce a
desired coding style.")
    (home-page "https://hexdocs.pm/credo/")
    (license license:expat)))

(define-public elixir-erlex
  (package
    (name "elixir-erlex")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "0x8c1j62y748ldvlh46sxzv5514rpzm809vxn594vd7y25by5lif"))))
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
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "00cqwhd1wabwds44jz94rvvr8z8cp12884d3lp69fqkrszb9bdw4"))))
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
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri name version))
       (sha256
        (base32 "0952603bmqsf6v3ja99zpbnbx5d52i4xksjkfj3irl45ccq5pgq9"))))
    (build-system mix-build-system)
    (native-inputs (list elixir-credo elixir-dialyxir))
    (synopsis "Literate test matchers for ExUnit")
    (description
     "Machete provides ergonomic match operators that act as building blocks to let
you define test expectations that can match data against any combination of
literals, variables, or parametrically defined matchers.")
    (home-page "https://hexdocs.pm/machete/")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
