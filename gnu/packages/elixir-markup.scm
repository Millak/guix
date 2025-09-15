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
