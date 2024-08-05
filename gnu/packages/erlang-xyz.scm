;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages erlang-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages erlang)
  #:use-module (guix build-system rebar)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public erlang-luerl
  (package
    (name "erlang-luerl")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "luerl" version))
       (sha256
        (base32 "0paj3gr0kn8v9g6wmdnz1f16q0iy1pb83zbqsalzbw2g17vd9bww"))))
    (build-system rebar-build-system)
    (synopsis "Implementation of Lua on Erlang")
    (description "This package provides implementation of Lua on Erlang.")
    (home-page "https://hex.pm/packages/luerl")
    (license license:asl2.0)))

(define-public erlang-jose
  (package
    (name "erlang-jose")
    (version "1.11.10")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "jose" version))
       (sha256
        (base32 "0576jdjygby37qmzrs8cm5l6n622b0mi3z28j6r4s5xsz1px6v0d"))))
    (build-system rebar-build-system)
    (synopsis
     "JSON Object Signing and Encryption for Erlang and Elixir")
    (description
     "This package provides JSON Object Signing and Encryption (JOSE) for
Erlang and Elixir.")
    (home-page "https://hex.pm/packages/jose")
    (license license:expat)))

(define-public erlang-p1-utils
  (package
    (name "erlang-p1-utils")
    (version "1.0.26")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_utils" version))
       (sha256
        (base32 "0k5hfqrjrvgh7gknqj5pzpra9k1g0bgc2ac19zb8pfan2669wdyh"))))
    (build-system rebar-build-system)
    (synopsis "ProcessOne utility modules for Erlang")
    (description "This package provides ProcessOne utility modules for Erlang.")
    (home-page "https://hex.pm/packages/p1_utils")
    (license license:asl2.0)))

(define-public erlang-base64url
  (package
    (name "erlang-base64url")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "base64url" version))
       (sha256
        (base32 "0p4zf53v86zfpnk3flinjnk6cx9yndsv960386qaj0hsfgaavczr"))))
    (build-system rebar-build-system)
    (synopsis "URL safe base64-compatible codec")
    (description "This package provides URL safe base64-compatible codec
for Erlang.")
    (home-page "https://hex.pm/packages/base64url")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
