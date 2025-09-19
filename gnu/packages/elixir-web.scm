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

(define-module (gnu packages elixir-web)
  #:use-module (gnu packages erlang-xyz)
  #:use-module (gnu packages elixir-databases)
  #:use-module (gnu packages elixir-i18n)
  #:use-module (gnu packages elixir-markup)
  #:use-module (gnu packages elixir-xyz)
  #:use-module (guix build-system mix)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages))

;; This package lives here to avoid module level circular dependencies as it
;; depends on elixir-plug.
(define-public elixir-ecto-shorts
  (package
    (name "elixir-ecto-shorts")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ecto_shorts" version))
       (sha256
        (base32 "1r7cc0dhhvmqnwicrnay9lsaf2xahr919g993rbbakdca2xw0q3l"))))
    (build-system mix-build-system)
    (native-inputs (list elixir-credo elixir-excoveralls))
    (propagated-inputs (list elixir-ecto-sql elixir-error-message))
    (synopsis
     "Helper tools for making ecto interactions shorter")
    (description
     "Helper tools for making ecto interactions more pleasant and shorter.")
    (home-page "https://hexdocs.pm/ecto_shorts/")
    (license license:expat)))

;; This package lives here to avoid module level circular dependencies as it
;; depends on elixir-plug.
(define-public elixir-error-message
  (package
    (name "elixir-error-message")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "error_message" version))
       (sha256
        (base32 "0wsbcwby3grnb5s6558s2x5wrhy8f7vwbbfdk88rsacbf1w0l6k7"))))
    (build-system mix-build-system)
    (native-inputs (list elixir-excoveralls))
    (propagated-inputs (list elixir-jason elixir-plug))
    (synopsis "Make errors consistent across your system")
    (description
     "Error system to help make errors consistent across your system.")
    (home-page "https://hexdocs.pm/error_message/")
    (license license:expat)))

(define-public elixir-plug-crypto
  (package
    (name "elixir-plug-crypto")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "plug_crypto" version))
       (sha256
        (base32 "0z1424zcywdg47wsbvhmdbrngbz4lzhzsbv1jza8n774zzkbqw34"))))
    (build-system mix-build-system)
    (synopsis "Crypto-related functionality for the web")
    (description "This package provides @code{elixir-plug-crypto}, a library
implementing crypto-related functionality for the web, used by Plug.")
    (home-page "https://hexdocs.pm/plug_crypto/")
    (license license:asl2.0)))

(define-public elixir-plug-cowboy
  (package
    (name "elixir-plug-cowboy")
    (version "2.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "plug_cowboy" version))
       (sha256
        (base32 "18wa2qiy18g9s61anb5krcmmvcmivs201mx5w2x1a9h1swmn71cv"))))
    (build-system mix-build-system)
    (arguments
     (list
      #:tests? #f))
    (native-inputs
     (list elixir-x509 erlang-hackney))
    (propagated-inputs (list erlang-cowboy erlang-cowboy-telemetry elixir-plug))
    (synopsis "A Plug adapter for Cowboy")
    (description "This package provides a Plug adapter for Cowboy.")
    (home-page "https://hexdocs.pm/plug_cowboy/")
    (license license:asl2.0)))

(define-public elixir-plug
  (package
    (name "elixir-plug")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "plug" version))
       (sha256
        (base32 "1hkcahx1l6yi1hcxn7lw6cbcscwdrwrjsza3cjsj5d7j1nvpv9ap"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-mime elixir-plug-crypto erlang-telemetry))
    (synopsis "Compose web applications with functions")
    (description "Plug is:

@itemize
@item A specification for composing web applications with functions
@item Connection adapters for different web servers in the Erlang VM
@end itemize

In other words, Plug allows you to build web applications from small pieces and
run them on different web servers.  Plug is used by web frameworks such as
Phoenix to manage requests, responses, and websockets.  This documentation will
show some high-level examples and introduce the Plug's main building blocks.")
    (home-page "https://hexdocs.pm/plug/")
    (license license:asl2.0)))

(define-public elixir-websock
  (package
    (name "elixir-websock")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "websock" version))
       (sha256
        (base32 "0lxlp1h18595nqczfg15iy34kw5xbbab3yk6ml9cf8mcgwyla1b1"))))
    (build-system mix-build-system)
    (synopsis "Specification for WebSocket connections")
    (description
     "This package provides a specification for @code{WebSocket} connections.")
    (home-page "https://hexdocs.pm/websock/")
    (license license:expat)))

(define-public elixir-x509
  (package
    (name "elixir-x509")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "x509" version))
       (sha256
        (base32 "1iyg91719bkxpps3l97aj2hd67xvf4xlrq2v1x5msmkyd5sxwpjc"))))
    (build-system mix-build-system)
    (synopsis
     "Elixir package for working with X.509 certificates")
    (description
     "Elixir package for working with X.509 certificates, Certificate Signing Requests
(CSRs), Certificate Revocation Lists (CRLs) and RSA/ECC key pairs.")
    (home-page "https://hexdocs.pm/x509/")
    (license license:bsd-3)))
