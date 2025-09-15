;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages elixir-markup)
  #:use-module (gnu packages elixir-xyz)
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
