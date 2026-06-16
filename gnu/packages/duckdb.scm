;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2024 Greg Hogan <code@greghogan.com>
;;; Copyright © 2023, 2025, 2026 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024, 2025 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2024-2026 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module (guix build-system r)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)     ;python-pytest
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages time))

;;; Commentary:
;;;
;;; This module contains DuckDB - an analytical in-process SQL database
;;; management system (https://www.duckdb.org/) and it's binding in different
;;; programming languages with other related to DuckDB packages.
;;;
;;; Code:

(define %duckdb-version "1.5.3")

(define-public duckdb
  (package
    (name "duckdb")
    (version %duckdb-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/duckdb/duckdb")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0020lv9dxcrng34f0c6ibp20pxll7pyjg0p3080qgw6jf5hsvfck"))))
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

(define-public python-duckdb
  (package
    (name "python-duckdb")
    (version %duckdb-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/duckdb/duckdb-python")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d0zbg86kfpknx50zjmz9j6hcycfy8pwb2h2w5rxw741iihdyj5g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Overwrite duckdb_packaging.build_backend.
      #:build-backend "scikit_build_core.build"
      ;; tests: 3269 passed, 42 skipped, 2 deselected, 9 xfailed
      #:test-flags
      ;; Network access is required.
      #~(list "--ignore=tests/slow/test_h2oai_arrow.py"
              ;; Flaky test.
              (string-append "--deselect=tests/fast/api/"
                             "test_query_progress.py::TestQueryProgress"
                             "::test_query_progress"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'link-duckdb-source
            (lambda _
              (rmdir "external/duckdb")
              (symlink #+(package-source (this-package-native-input "duckdb"))
                       "external/duckdb")))
          (add-before 'build 'pre-build
            (lambda _
              (setenv "DUCKDB_BUILD_UNITY" "1")
              (setenv "CMAKE_DEFINE_DUCKDB_EXTENSION_AUTOLOAD_DEFAULT" "0")
              (setenv "CMAKE_DEFINE_DUCKDB_EXTENSION_AUTOINSTALL_DEFAULT" "0")))
          (add-before 'check 'set-HOME
            (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list duckdb
           ninja
           python-pytest
           python-pytz
           python-scikit-build-core
           python-setuptools-scm
           tzdata-for-tests))
    (propagated-inputs
     (list python-adbc-driver-manager
           python-fsspec
           python-ipython
           python-numpy
           python-pandas
           python-psutil
           python-pyarrow))
    (home-page "https://www.duckdb.org")
    (synopsis "DuckDB embedded database")
    (description "DuckDB is an in-process SQL OLAP database management
system.")
    (license license:expat)))

(define-public r-duckdb
  (package
    (name "r-duckdb")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "duckdb" version))
       (sha256
        (base32 "1dldlfwvc33czvlxfmwaj4p9y6sjjj5iqy98yda47a8kclrqmyam"))
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
                         r-sf
                         r-testthat
                         r-tibble
                         r-vctrs
                         r-withr
                         r-wk
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
