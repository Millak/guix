;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2022 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu packages wireservice)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public python-leather
  (package
    (name "python-leather")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wireservice/leather")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "193hchrcrh5zyj0xlfs839h25jwzxvzjpbniqjd18mgnip5mk0zn"))))
    (build-system pyproject-build-system)
    ;; XXX: Documentation requires <https://github.com/pradyunsg/furo> which
    ;; is not packaged yet and depends on some missing Node.js packages
    (native-inputs
     (list python-cssselect
           python-lxml
           python-pytest
           python-setuptools
           python-sphinx
           python-sphinx-design
           python-wheel))
    (home-page "https://leather.rtfd.org")
    (synopsis "Python charting for 80% of humans")
    (description
     "Leather is a Python charting library for those who need charts now and
don't care if they're perfect.")
    (license license:expat)))

(define python-agate-locales
  (make-glibc-utf8-locales
   glibc
   #:locales (list "ko_KR")
   #:name "python-agate-locales"))

(define-public python-agate
  (package
    (name "python-agate")
    (version "1.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wireservice/agate")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qvjlcbv42pjphz5i7vvd5p25barqmglhdzksaspg66n83gps8gv"))))
    (build-system pyproject-build-system)
    ;; XXX: Documentation requires <https://github.com/pradyunsg/furo> which
    ;; is not packaged yet and depends on some missing Node.js packages
    (native-inputs
     (list (libc-utf8-locales-for-target)
           python-agate-locales
           python-cssselect
           python-lxml
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-babel
           python-isodate
           python-leather
           python-parsedatetime
           python-pytimeparse
           python-slugify
           python-tzdata))
    (home-page "https://agate.rtfd.org")
    (synopsis "Data analysis library")
    (description
     "Agate is a Python data analysis library.  It is an alternative to numpy
and pandas that solves real-world problems with readable code.  Agate was
previously known as journalism.")
    (license license:expat)))

(define-public python-agate-sql
  (package
    (name "python-agate-sql")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wireservice/agate-sql")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03pvya65jm4s5sxwz0msj5dwjr6mk7dja3wdyh7hmf31dpczkjm8"))))
    (build-system pyproject-build-system)
    ;; XXX: Documentation requires <https://github.com/pradyunsg/furo> which
    ;; is not packaged yet and depends on some missing Node.js packages
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-agate
           python-crate
           python-sqlalchemy))
    (home-page "https://agate-sql.rtfd.org")
    (synopsis "SQL read/write support to agate")
    (description
     "@code{agatesql} uses a monkey patching pattern to add SQL support to all
@code{agate.Table} instances.")
    (license license:expat)))

(define-public python-agate-dbf
  (package
    (name "python-agate-dbf")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wireservice/agate-dbf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z9zmc05sjxw02xl9ygjsdyp32nb3m2qrig0pmvhvf5hj1faigxi"))))
    (build-system pyproject-build-system)
    ;; XXX: Documentation requires <https://github.com/pradyunsg/furo> which
    ;; is not packaged yet and depends on some missing Node.js packages
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-agate
           python-dbfread))
    (home-page "https://agate-dbf.rtfd.org")
    (synopsis "Add read support for dbf files to agate")
    (description
     "@code{agatedbf} uses a monkey patching pattern to add read for dbf files
support to all @code{agate.Table} instances.")
    (license license:expat)))

(define-public python-agate-excel
  (package
    (name "python-agate-excel")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wireservice/agate-excel")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mg5ldnyc72yllwl8x2gpb142l43wss5f4sgp610db1v2w12rzhj"))))
    (build-system pyproject-build-system)
    ;; XXX: Documentation requires <https://github.com/pradyunsg/furo> which
    ;; is not packaged yet and depends on some missing Node.js packages
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-agate
           python-olefile
           python-openpyxl
           python-xlrd))
    (home-page "https://agate-excel.rtfd.org")
    (synopsis "Add read support for Excel files (xls and xlsx) to agate")
    (description
     "@code{agateexcel} uses a monkey patching pattern to add read for xls and
xlsx files support to all @code{agate.Table} instances.")
    (license license:expat)))

(define-public csvkit
  (package
    (name "csvkit")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "csvkit" version))
       (sha256
        (base32 "1lbd2khkyr75rdc2fblvv8qkl33fv3nx6kj158qzy4spdlk6155a"))
       (patches
        (search-patches "csvkit-set-locale-for-tests.patch"))))
    (outputs (list "out" "doc"))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; AssertionError: '9748.346\n' != '9,748.346\n
      #:test-flags #~(list "-k" "not test_decimal_format")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-docs
            (lambda _
              (let* ((man1 (string-append #$output:doc "/share/man/man1")))
                (with-directory-excursion "docs"
                  (invoke "make" "man")
                  (copy-recursively "_build/man" man1))))))))
    (native-inputs
     (list (libc-utf8-locales-for-target)
           python-psycopg2 ; to test PostgreSQL support
           python-pytest
           python-setuptools
           python-sphinx
           python-sphinx-rtd-theme
           python-wheel))
    (inputs
     (list python-agate-dbf
           python-agate-excel
           python-agate-sql
           python-text-unidecode))
    (home-page "https://csvkit.rtfd.org")
    (synopsis "Command-line tools for working with CSV")
    (description "csvkit is a suite of command-line tools for converting to
and working with CSV.  It provides the following commands:
@itemize
@item Input:
  @itemize
  @item @command{in2csv}: Convert various formats to CSV.
  @item @command{sql2csv}: Execute SQL commands on a database and return the
data as CSV.
  @end itemize
@item Processing:
  @itemize
  @item @command{csvclean}: Remove common syntax errors.
  @item @command{csvcut}: Filter and truncate CSV files.
  @item @command{csvgrep}: Filter tabular data to only those rows where
certain columns contain a given value or match a regular expression.
  @item @command{csvjoin}: Merges two or more CSV tables together using a
method analogous to SQL JOIN operation.
  @item @command{csvsort}: Sort CSV files.
  @item @command{csvstack}: Stack up the rows from multiple CSV files,
optionally adding a grouping value to each row.
  @end itemize
@item Output and analysis:
  @itemize
  @item @command{csvformat}: Convert a CSV file to a custom output format.
  @item @command{csvjson}: Converts a CSV file into JSON or GeoJSON.
  @item @command{csvlook}: Renders a CSV to the command line in a
Markdown-compatible, fixed-width format.
  @item @command{csvpy}: Loads a CSV file into a @code{agate.csv.Reader}
object and then drops into a Python shell so the user can inspect the data
however they see fit.
  @item @command{csvsql}: Generate SQL statements for a CSV file or execute
those statements directly on a database.
  @item @command{csvstat}: Prints descriptive statistics for all columns in a
CSV file.
  @end itemize
@end itemize")
    (license license:expat)))
