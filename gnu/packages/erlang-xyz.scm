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
