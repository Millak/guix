;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages elixir-markup)
  #:use-module (gnu packages elixir-xyz)
  #:use-module (gnu packages erlang-xyz)
  #:use-module (guix build-system mix)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages))

(define-public elixir-earmark-ast-dsl
  (package
    (name "elixir-earmark-ast-dsl")
    (version "0.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "earmark_ast_dsl" version))
       (sha256
        (base32 "1c90204jcz1pbxxdxmvwr32y663jwvpn05izqp7zmqlp8yknzlhj"))))
    (build-system mix-build-system)
    (synopsis
     "Toolset to generate EarmarkParser AST Nodes")
    (description
     "@code{EarmarkAstDsl} is a toolset to generate @code{EarmarkParser}
conformant AST Nodes.  Its main purpose is to remove boilerplate code from
Earmark and @code{EarmarkParser} tests.")
    (home-page "https://hexdocs.pm/earmark_ast_dsl/")
    (license license:asl2.0)))

(define-public elixir-earmark-parser
  (package
    (name "elixir-earmark-parser")
    (version "1.4.44")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "earmark_parser" version))
       (sha256
        (base32 "0l1758nwqf39yg2mgx3d9zfgz2f9i44h7xqmj9csa0a75dssqy27"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-earmark-ast-dsl elixir-excoveralls elixir-floki))
    (synopsis "AST parser and generator for Markdown")
    (description "This package providesAST parser and generator for Markdown.")
    (home-page "https://hexdocs.pm/earmark_parser/")
    (license license:asl2.0)))

(define-public elixir-earmark
  (package
    (name "elixir-earmark")
    (version "1.4.47")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "earmark" version))
       (sha256
        (base32 "1y4qqq3bxim9b9jrpbdxpagsjs5whlig4zva6hxmznf2lazbx5iy"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-earmark-ast-dsl
           elixir-excoveralls
           elixir-floki
           elixir-traverse))
    (synopsis
     "Elixir Markdown converter")
    (description
     "Earmark is a pure-Elixir Markdown converter.  It is intended to be used as a
library (just call Earmark.as_html), but can also be used as a command-line tool
(run mix escript.build first).  Output generation is pluggable.")
    (home-page "https://hexdocs.pm/earmark/")
    (license license:asl2.0)))

(define-public elixir-easyhtml
  (package
    (name "elixir-easyhtml")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "easyhtml" version))
       (sha256
        (base32 "01jxhj3hpivf5c0x7d11c8xrx3d0ln6wsa78lc58g90j2vwkdadn"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-floki))
    (synopsis "Tiny wrapper around Floki")
    (description "@code{EasyHTML} makes working with HTML easy.  It is a tiny
wrapper around Floki that adds conveniences:

@itemize
@item An @code{Inspect} implementation to pretty-print HTML snippets
@item An @code{Access} implementation to search them
@item An @code{Enumerable} implementation to traverse them
item A @code{String.Chars} implementation to convert them to text
@end itemize
")
    (home-page "https://hexdocs.pm/easyhtml/")
    (license license:asl2.0)))

(define-public elixir-ex-doc
  (package
    (name "elixir-ex-doc")
    (version "0.38.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_doc" version))
       (sha256
        (base32 "1ck5y70zkppyswpagvla4zh19cryc4slw5c04lf930wa81327dpp"))))
    (build-system mix-build-system)
    (arguments
     ;; FIXME: tests depend on lazy_html which is not yet packaged.
     (list #:tests? #f))
    (native-inputs
     (list elixir-easyhtml
           elixir-jason
           elixir-floki))
    (propagated-inputs
     (list elixir-earmark-parser elixir-makeup-c elixir-makeup-elixir
           elixir-makeup-erlang elixir-makeup-html))
    (synopsis "Documentation generation tool for Elixir")
    (description "@code{ExDoc} is a documentation generation tool for Elixir.")
    (home-page "https://hexdocs.pm/ex_doc/")
    (license license:asl2.0)))

(define-public elixir-html-entities
  (package
    (name "elixir-html-entities")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "html_entities" version))
       (sha256
        (base32 "1k7xyj0q38ms3n5hbn782pa6w1vgd6biwlxr4db6319l828a6fy5"))))
    (build-system mix-build-system)
    (synopsis "Decode and encode HTML entities in a string")
    (description "This library provides an Elixir module to decode and encode
HTML entities in a string.")
    (home-page "https://hexdocs.pm/html_entities/")
    (license license:expat)))

(define-public elixir-html-sanitize-ex
  (package
    (name "elixir-html-sanitize-ex")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "html_sanitize_ex" version))
       (sha256
        (base32 "1dyc9fvkdrihmy32d85jqlzs9jrbijahq5dpdry9r57y98y8sx47"))))
    (build-system mix-build-system)
    (propagated-inputs (list erlang-mochiweb))
    (synopsis "HTML sanitizer for Elixir")
    (description "HTML sanitizer for Elixir.")
    (home-page "https://hexdocs.pm/html_sanitize_ex/")
    (license license:expat)))

(define-public elixir-floki
  (package
    (name "elixir-floki")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "floki" version))
       (sha256
        (base32 "1ndbs7q8dncxxxiq94l4ira3fvg311asyb31bdijvywk3vlkx555"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-credo elixir-jason))
    (synopsis
     "Simple HTML parser")
    (description
     "Floki is a simple HTML parser that enables search for nodes using CSS selectors.")
    (home-page "https://hexdocs.pm/floki/")
    (license license:expat)))

(define-public elixir-makeup-c
  (package
    (name "elixir-makeup-c")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "makeup_c" version))
       (sha256
        (base32 "0s98gwsvi88mxf3wjsi05l7dgkw4gzbihzlsq5ad68i86x2wzsc9"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-makeup))
    (synopsis "C lexer for the Makeup syntax highlighter")
    (description "This package provides @code{elixir-makeup-c}, a library
implementing a C lexer for the Makeup syntax highlighter.")
    (home-page "https://hexdocs.pm/makeup_c/")
    (license license:bsd-2)))

(define-public elixir-makeup-elixir
  (package
    (name "elixir-makeup-elixir")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "makeup_elixir" version))
       (sha256
        (base32 "01wmpzzf445xnc7gr7ml9hmqz6rqxpsx9bpxjzymqgia846r113j"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-makeup elixir-nimble-parsec))
    (synopsis "Elixir lexer for the Makeup syntax highlighter")
    (description "This package provides @code{elixir-makeup-elixir}, a library
implementing an Elixir lexer for the Makeup syntax highlighter.")
    (home-page "https://hexdocs.pm/makeup_elixir/")
    (license license:bsd-2)))

(define-public elixir-makeup-erlang
  (package
    (name "elixir-makeup-erlang")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "makeup_erlang" version))
       (sha256
        (base32 "09rpmb9iw49syp62s7pny4ycwijffkkk6y9698z8kmb8ydzgycxg"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-makeup))
    (synopsis "Erlang lexer for the Makeup syntax highlighter")
    (description "This package provides @code{elixir-makeup-erlang}, a library
implementing an Erlang lexer for the Makeup syntax highlighter.")
    (home-page "https://hexdocs.pm/makeup_erlang/")
    (license license:bsd-2)))

(define-public elixir-makeup-html
  (package
    (name "elixir-makeup-html")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "makeup_html" version))
       (sha256
        (base32 "1ciildxh4bmacbkbil4hhsjhp7z31ycnvq072fml59m6p6zgfmh8"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-stream-data))
    (propagated-inputs (list elixir-makeup))
    (synopsis "HTML lexer for the Makeup syntax highlighter")
    (description "This package provides @code{elixir-makeup-html}, a library
implementing an HTML lexer for the Makeup syntax highlighter.")
    (home-page "https://hexdocs.pm/makeup_html/")
    (license license:expat)))
