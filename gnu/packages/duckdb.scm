;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2024 Greg Hogan <code@greghogan.com>
;;; Copyright © 2023, 2025, 2026 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024, 2025 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2024, 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module (guix build-system pyproject)
  #:use-module ((guix build-system python) #:select (pypi-uri))
  #:use-module (guix build-system r)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics))

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

;; XXX: Try to inherit from duckdb and build from source with all extensions.
(define-public python-duckdb
  (package
    (name "python-duckdb")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "duckdb" version))
       (sha256
        (base32 "1x8zb47y8lzc4w0g013sim8x9vd1h96ra3dd0bvh91y73f5dyn66"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;FIXME: See <https://codeberg.org/guix/guix/issues/1436>.
      #:test-flags
      #~(list "--ignore=tests/slow/test_h2oai_arrow.py"
              ;; Do not relay on mypy.
              "--ignore=tests/stubs/test_stubs.py"
              "-k" (string-append
                    ;; Don't install anything, thank you.
                    "not test_install_non_existent_extension"
                    ;; _pybind11_conduit_v1_ not found.
                    " and not test_wrap_coverage"
                    ;; See <https://github.com/duckdb/duckdb/issues/11961>.
                    " and not test_fetchmany"
                    ;; See <https://github.com/duckdb/duckdb/issues/10702>.
                    " and not test_connection_interrupt"
                    " and not test_query_interruption"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests need this
          (add-before 'check 'set-HOME
            (lambda _ (setenv "HOME" "/tmp")))
          ;; Later versions of pybind replace "_" with "const_name".
          (add-after 'unpack 'pybind-compatibility
            (lambda _
              (with-directory-excursion "src/include/duckdb_python"
                (substitute* '("python_objects.hpp"
                               "pyfilesystem.hpp"
                               "pybind11/conversions/pyconnection_default.hpp")
                  (("const_name") "_"))))))))
    (propagated-inputs
     (list python-adbc-driver-manager))
    (native-inputs
     (list pybind11
           python-fsspec
           ;; python-google-cloud-storage ;python-grpcio fails (see: guix/guix#1436)
           python-numpy
           python-pandas
           python-psutil
           ;; python-pytest
           python-setuptools-scm
           python-setuptools
           python-wheel))
    (home-page "https://www.duckdb.org")
    (synopsis "DuckDB embedded database")
    (description "DuckDB is an in-process SQL OLAP database management
system.")
    (license license:expat)))

(define-public r-duckdb
  (package
    (name "r-duckdb")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "duckdb" version))
       (sha256
        (base32 "0br7d7gadi0gbpd45syvig6zs1b0sl3vpn8wa272hs8jv4d73ar3"))
       ;; This package bundles the duckdb sources and builds a custom variant
       ;; of duckdb.  I'd be happy to link it with our duckdb library instead,
       ;; but it does not seem possible to do that.
       #;
       (snippet
        '(delete-file "src/duckdb.tar.xz"))))
    (properties
     '((upstream-name . "duckdb")
       (updater-extra-native-inputs . ("tzdata-for-tests"))
       ;; We don't seem to need this and I don't want to package it now.
       (updater-ignored-native-inputs . ("r-dblog"))))
    (build-system r-build-system)
    (arguments (list #:tests? #false)) ;tests can time out on the build farm
    (propagated-inputs (list r-dbi))
    (native-inputs (list r-adbcdrivermanager
                         r-arrow
                         r-bit64
                         r-callr
                         r-clock
                         r-dbitest
                         r-dbplyr
                         r-dplyr
                         r-remotes
                         r-rlang
                         r-testthat
                         r-tibble
                         r-vctrs
                         r-withr
                         tzdata-for-tests))
    (home-page "https://r.duckdb.org/")
    (synopsis "DBI package for the DuckDB database management system")
    (description
     "The DuckDB project is an embedded analytical data management system with
support for the Structured Query Language (SQL).  This package includes all of
DuckDB and an R Database Interface (DBI) connector.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
