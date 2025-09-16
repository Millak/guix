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

(define-module (gnu packages elixir-databases)
  #:use-module (gnu packages erlang-xyz)
  #:use-module (gnu packages elixir-xyz)
  #:use-module (guix build-system mix)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages))

(define-public elixir-db-connection
  (package
    (name "elixir-db-connection")
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "db_connection" version))
       (sha256
        (base32 "1zd8kp85wh7bbzr5x4kc75l53r5q9xwrhfz04vrpd793kd43s6m6"))))
    (build-system mix-build-system)
    (arguments
     (list
      ;; Tests require a db process
      #:tests? #f
      #:build-per-environment #f))
    (propagated-inputs (list erlang-telemetry))
    (synopsis
     "Database connection behaviour")
    (description
     "This package provides @code{elixir-db-connection}, a library implementing
database connection behaviour and database connection pool.  It is designed for
handling transaction, prepare/execute, cursors and client process
describe/encode/decode.")
    (home-page "https://hexdocs.pm/db_connection/")
    (license license:asl2.0)))

(define-public elixir-ecto
  (package
    (name "elixir-ecto")
    (version "3.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ecto" version))
       (sha256
        (base32 "0ghb9a4mgiak6103vks6w013sa7kg8dhirxpwxbgy4q56y8r57b6"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-decimal elixir-jason erlang-telemetry))
    (synopsis
     "Toolkit for data mapping and language integrated query for Elixir")
    (description
     "This package provides a toolkit for data mapping and language integrated query
for Elixir.")
    (home-page "https://hexdocs.pm/ecto/")
    (license license:asl2.0)))
