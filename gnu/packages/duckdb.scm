;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2024 Greg Hogan <code@greghogan.com>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024, 2025 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages duckdb)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages))

;;; Commentary:
;;;
;;; This module contains DuckDB - an analytical in-process SQL database
;;; management system (https://www.duckdb.org/) and it's binding in different
;;; programming languages with other related to DuckDB packages.
;;;
;;; Code:

(define-public duckdb
  (package
    (name "duckdb")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/duckdb/duckdb")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vywdhn930z4aw2ljxp5mx3cfivpvv1d88kq4rhizq3c4hpq9yf3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; TODO: Find out how to run tests, see: <.github/workflows>.
      #:tests? #f
      #:configure-flags
      #~(list "-DBUILD_EXTENSIONS=autocomplete;icu;json;parquet;tpch;"
              ;; There is no git checkout from which to read the version tag.
              (string-append "-DOVERRIDE_GIT_DESCRIBE="
                             "v" #$version "-0-g0123456789"))))
    (home-page "https://duckdb.org")
    (synopsis "In-process SQL OLAP database management system")
    (description
     "CLI and C/C++ source libraries for DuckDB, a relational (table-oriented)
 @acronym{DBMS, Database Management System} that supports @acronym{SQL,
Structured Query Language}, contains a columnar-vectorized query execution
engine, and provides transactional @acronym{ACID, Atomicity Consistency
Isolation and Durability} guarantees via bulk-optimized @acronym{MVCC,
Multi-Version Concurrency Control}.  Data can be stored in persistent,
single-file databases with support for secondary indexes.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
