;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2016, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021, 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 LuHui <luhux76@gmail.com>
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

(define-module (gnu packages dbm)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; This module has been separated from (gnu packages databases) to reduce the
;;; number of module references for core packages.

(define bdb-snippet
  ;; Remove some bundled and generated files.  Some of the old
  ;; Autotools files are too old for some architectures
  ;; (e.g. aarch64 and powerpc64le).
  #~(begin
      (for-each delete-file-recursively
                '("dist/configure"
                  "dist/config.sub"
                  "dist/config.guess"
                  "dist/install-sh"
                  "dist/ltmain.sh"
                  "dist/aclocal/libtool.m4"
                  "dist/aclocal/ltoptions.m4"
                  "dist/aclocal/ltsugar.m4"
                  "dist/aclocal/ltversion.m4"
                  "dist/aclocal/lt~obsolete.m4"))
      (substitute* "dist/configure.ac"
        ;; Placate 'automake'.
        (("AC_DEFINE\\(DB_WIN32\\)")
         "AC_DEFINE(DB_WIN32, [], [Description])")
        (("AC_DEFINE\\(HAVE_SYSTEM_INCLUDE_FILES\\)")
         "AC_DEFINE(HAVE_SYSTEM_INCLUDE_FILES, [], [Description])"))))

(define-public bdb-4.8
  (package
    (name "bdb")
    (version "4.8.30")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.oracle.com/berkeley-db/db-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0ampbl2f0hb1nix195kz1syrqqxpmvnvnfvphambj7xjrl3iljg0"))
             (patches (search-patches "bdb-5.3-atomics-on-gcc-9.patch"))
             (modules '((guix build utils)
                        (srfi srfi-1)))
             (snippet bdb-snippet)))
    (build-system gnu-build-system)
    (outputs '("out"                             ; programs, libraries, headers
               "doc"))                           ; 94 MiB of HTML docs
    (arguments
     (list #:tests? #f                        ; no check target available
           #:disallowed-references '("doc")
           #:out-of-source? #true
           #:configure-flags
           #~(list
              ;; Remove 7 MiB of .a files.
              "--disable-static"

              ;; The compatibility mode is needed by some packages,
              ;; notably iproute2.
              "--enable-compat185"

              ;; The following flag is needed so that the inclusion
              ;; of db_cxx.h into C++ files works; it leads to
              ;; HAVE_CXX_STDHEADERS being defined in db_cxx.h.
              "--enable-cxx")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'bootstrap
                 (lambda* (#:key inputs native-inputs outputs
                           #:allow-other-keys #:rest arguments)
                   (with-directory-excursion "dist"
                     (for-each (lambda (x)
                                 (install-file x "aclocal"))
                               (find-files "aclocal_java"))
                     (apply (assq-ref %standard-phases 'bootstrap) arguments)
                     (let ((automake-files (search-input-directory
                                            (or native-inputs inputs)
                                            "share/automake-1.16")))
                       (define (replace file)
                         (symlink (string-append automake-files "/" file) file))
                       (for-each replace '("config.sub" "config.guess"
                                           "install-sh"))))))
               (add-before 'configure 'pre-configure
                 (lambda _
                   (chdir "dist")
                   ;; '--docdir' is not honored, so we need to patch.
                   (substitute* "Makefile.in"
                     (("docdir[[:blank:]]*=.*")
                      (string-append "docdir = " #$output:doc
                                     "/share/doc/bdb")))
                   ;; Replace __EDIT_DB_VERSION__... by actual version numbers.
                   ;; s_config is responsible for this, but also runs autoconf
                   ;; again, so patch out the autoconf bits.
                   (substitute* "s_config"
                     (("^.*(aclocal|autoconf|autoheader|config\\.hin).*$") "")
                     (("^.*auto4mte.*$") "")
                     (("rm (.*) configure") "")
                     (("chmod (.*) config.guess(.*)$") ""))
                   (invoke "sh" "s_config"))))))
    (native-inputs (list autoconf automake libtool))
    (synopsis "Berkeley database")
    (description
     "Berkeley DB is an embeddable database allowing developers the choice of
SQL, Key/Value, XML/XQuery or Java Object storage for their data model.")
    ;; Starting with version 6, BDB is distributed under AGPL3. Many individual
    ;; files are covered by the 3-clause BSD license.
    (home-page
     "http://www.oracle.com/us/products/database/berkeley-db/overview/index.html")))

(define-public bdb-5.3
  (package (inherit bdb-4.8)
    (name "bdb")
    (version "5.3.28")
    (source (origin
              (inherit (package-source bdb-4.8))
              (uri (string-append "https://download.oracle.com/berkeley-db/db-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0a1n5hbl7027fbz5lm0vp0zzfp1hmxnz14wx3zl9563h83br5ag0"))
              (patch-flags '("-p0"))
              (patches (search-patches "bdb-5.3-atomics-on-gcc-9.patch"))))))

(define-public bdb-6
  (package (inherit bdb-4.8)
    (name "bdb")
    (version "6.2.32")
    (source (origin
              (inherit (package-source bdb-4.8))
              (method url-fetch)
              (uri (string-append "https://download.oracle.com/berkeley-db/db-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1yx8wzhch5wwh016nh0kfxvknjkafv6ybkqh6nh7lxx50jqf5id9"))
              (patches '())))
    ;; Starting with version 6, BDB is distributed under AGPL3. Many individual
    ;; files are covered by the 3-clause BSD license.
    (license (list license:agpl3+ license:bsd-3))))

(define-public bdb bdb-6)

(define-public gdbm
  (package
    (name "gdbm")
    (version "1.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gdbm/gdbm-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1kfapds42j1sjq6wl7fygipw5904wpbfa5kwppj3mwgz44fhicbl"))))
    (arguments `(#:configure-flags '("--enable-libgdbm-compat"
                                     "--disable-static")))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org.ua/software/gdbm")
    (synopsis
     "Hash library of database functions compatible with traditional dbm")
    (description
     "GDBM is a library for manipulating hashed databases.  It is used to
store key/value pairs in a file in a manner similar to the Unix dbm library
and provides interfaces to the traditional file format.")
    (license license:gpl3+)))
