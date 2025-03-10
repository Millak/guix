;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2016, 2018, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016, 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2015-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017, 2020 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2015, 2017, 2018, 2019, 2021-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Kristofer Buffington <kristoferbuffington@gmail.com>
;;; Copyright © 2018 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019, 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021, 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021, 2024 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 David Larsson <david.larsson@selfhosted.xyz>
;;; Copyright © 2021 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Simon Streit <simon@netpanic.org>
;;; Copyright © 2021 Alexandre Hannud Abdo <abdo@member.fsf.org>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022 Thomas Albers Raviola <thomas@thomaslabs.org>
;;; Copyright © 2021, 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023, 2024 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2023 Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 gemmaro <gemmaro.dev@gmail.com>
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

(define-module (gnu packages databases)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages pantheon)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix bzr-download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public duckdb
  (package
    (name "duckdb")
    (version "1.1.3")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/duckdb/duckdb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1b57r4x1lnkdiv0f8r0wyhbil61l9gp1ipr37i12s0x6dv19lxi2"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; There is no git checkout from which to read the version tag.
            (substitute* "CMakeLists.txt"
              (("set\\(DUCKDB_VERSION \"[^\"]*\"")
               (string-append "set(DUCKDB_VERSION \"v" #$version "-dev0\"")))))))
    (arguments
     (list
      #:configure-flags
      '(list "-DBUILD_EXTENSIONS=autocomplete;fts;icu;json;parquet;tpch;")))
    (build-system cmake-build-system)
    (home-page "https://duckdb.org")
    (synopsis "In-process SQL OLAP database management system")
    (description "CLI and C/C++ source libraries for DuckDB, a relational
(table-oriented) @acronym{DBMS, Database Management System} that supports
@acronym{SQL, Structured Query Language}, contains a columnar-vectorized query
execution engine, and provides transactional @acronym{ACID, Atomicity
Consistency Isolation and Durability} guarantees via bulk-optimized
@acronym{MVCC, Multi-Version Concurrency Control}.  Data can be stored in
persistent, single-file databases with support for secondary indexes.")
    (license license:expat)))

(define-public ephemeralpg
  (package
    (name "ephemeralpg")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://eradman.com/ephemeralpg/code/ephemeralpg-"
             version ".tar.gz"))
       (sha256
        (base32 "1ap22ki8yz6agd0qybcjgs4b9izw1rwwcgpxn3jah2ccfyax34s6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/pg_tmp")
                 `("PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "util-linux")
                                    "/bin")
                    ,(string-append (assoc-ref inputs "postgresql")
                                    "/bin")
                    ;; For getsocket.
                    ,(string-append out "/bin"))))))))
       #:test-target "test"))
    (inputs
     (list bash-minimal postgresql util-linux))
    (native-inputs
     ;; For tests.
     (list ruby which))
    (home-page "https://eradman.com/ephemeralpg/")
    (synopsis "Run temporary PostgreSQL databases")
    (description
     "@code{pg_tmp} creates temporary PostgreSQL databases, suitable for tasks
like running software test suites.  Temporary databases created with
@code{pg_tmp} have a limited shared memory footprint and are automatically
garbage-collected after a configurable number of seconds (the default is
60).")
    (license license:isc)))

(define-public es-dump-restore
  (package
    (name "es-dump-restore")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "es_dump_restore" version))
       (sha256
        (base32
         "020yk7f1hw48clmf5501z3xv9shsdchyymcv0y2cci2c1xvr1mim"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ;; No testsuite.
    (propagated-inputs
     (list ruby-httpclient ruby-multi-json ruby-progress_bar ruby-rubyzip
           ruby-thor))
    (synopsis "Utility for dumping and restoring ElasticSearch indexes")
    (description
     "This package provides a utility for dumping the contents of an
ElasticSearch index to a compressed file and restoring the dumpfile back to an
ElasticSearch server")
    (home-page "https://github.com/patientslikeme/es_dump_restore")
    (license license:expat)))

(define-public firebird
  (package
    (name "firebird")
    (version "3.0.10")
    (source
     (let ((revision "33601-0"))
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/FirebirdSQL/"
                             "firebird/releases/download/v"
                             version "/"
                             "Firebird-" version "." revision ".tar.bz2"))
         (sha256
          (base32 "0h033xj1kxwgvdv4ncm6kk0mqybvvn203gf88xcv3avys9hbnf4i"))
         (patches (search-patches "firebird-riscv64-support-pt1.patch"
                                  "firebird-riscv64-support-pt2.patch"))
         (modules '((guix build utils)))
         (snippet
          `(begin
             (for-each
              delete-file-recursively
              (list "extern/btyacc/test" ; TODO: package and remove entirely
                    "extern/editline"
                    "extern/icu"
                    "extern/libtommath"
                    "extern/zlib"
                    "src/include/firebird/impl/boost"

                    ;; Missing licence.
                    "builds/install/arch-specific/solaris"
                    "extern/SfIO"
                    "src/msgs/templates.sql"

                    ;; Generated files missing sources.
                    "doc/Firebird-3-QuickStart.pdf"
                    (string-append "doc/Firebird-" ,version
                                   "-ReleaseNotes.pdf")
                    "doc/README.SecureRemotePassword.html")))))))
    (build-system gnu-build-system)
    (outputs (list "debug" "out"))
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "--with-fbsbin=" out "/sbin")
               (string-append "--with-fbdoc=" out "/share/doc/"
                              ,name "-" ,version)
               (string-append "--with-fbconf=" out "/lib/firebird")
               (string-append "--with-fbintl=" out "/lib/firebird/intl")
               (string-append "--with-fbmisc=" out "/lib/firebird/misc")
               (string-append "--with-fbmsg=" out "/lib/firebird")
               (string-append "--with-fbplugins=" out "/lib/firebird/plugins")
               (string-append "--with-fbudf=" out "/lib/firebird/UDF")
               "--with-fbglock=/run/firebird"
               "--with-fblog=/var/log/firebird"
               "--with-fbhelp=/var/lib/firebird/system"
               "--with-fbsecure-db=/var/lib/firebird/secure"
               "--without-fbsample"
               "--without-fbsample-db"
               "--with-system-editline"))
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             ;; The plugins/ can't find libfbclient otherwise.
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       #:tests? #f                      ; no test suite
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         ,@(if (target-riscv64?)
             `((add-before 'bootstrap 'force-bootstrap
                 (lambda _
                   (delete-file "configure")
                   ;; This file prevents automake from running.
                   (delete-file "autogen.sh"))))
             '())
         (add-after 'unpack 'use-system-boost
           (lambda _
             (substitute* "src/include/firebird/Message.h"
               (("\"\\./impl/boost/preprocessor/seq/for_each_i\\.hpp\"")
                "<boost/preprocessor/seq/for_each_i.hpp>")
               (("FB_BOOST_") "BOOST_"))))
         (add-after 'unpack 'patch-installation
           (lambda _
             (substitute*
                 "builds/install/arch-specific/linux/makeInstallImage.sh.in"
               (("/bin/sh") (which "bash"))
               ;; Remove shell script helpers from $PATH.
               (("(addLibs|cp) .*\\.sh .*@FB_SBINDIR@") ":")
               ;; Put files where Guix users expect them.
               (("(License\\.txt.*)@FB_CONFDIR" match)
                (string-append match "@FB_DOCDIR@"))
               (("@FB_CONFDIR@(.*License\\.txt.*)" match)
                (string-append "@FB_DOCDIR@" match))
               (("(cp .*/doc/.*)@FB_CONFDIR@(.*)" _ head tail)
                (string-append head "@FB_DOCDIR@" tail "\n")))
             (substitute*
                 (list "builds/install/posix-common/changeServerMode.sh.in"
                       "builds/install/posix-common/install.sh.in")
               ;; Skip phases that (could) cause problems in Guix.
               (("check(InstallUser|IfServerRunning|Libraries)|addFirebirdUser")
                ":")
               ;; Skip phases that are merely pointless on Guix.
               (("buildUninstallFile|installInitdScript|startFirebird") ":")
               ;; Omit randomly generated password with bonus timestamp.
               (("setDBAPassword") ":"))

             ;; These promote proprietary workflows not relevant on Guix.
             (for-each delete-file-recursively
                       (find-files "doc" "README\\.(build\\.msvc|NT|Win)"))))
         (add-after 'configure 'delete-init-scripts
           (lambda _
             (delete-file-recursively "gen/install/misc")))
         (add-before 'build 'set-build-environment-variables
           (lambda _
             ;; ‘isql’ needs to run & find libfbclient.so during the build.
             ;; This doubles as a rudimentary test in lieu of a test suite.
             (setenv "LD_LIBRARY_PATH"
                     (string-append (assoc-ref %build-inputs "icu4c") "/lib"))))
         (add-before 'install 'keep-embedded-debug-symbols
           (lambda _
             ;; Let the gnu-build-system separate & deal with them later.
             ;; XXX Upstream would use ‘--strip-unneeded’, shaving a whole
             ;; megabyte off Guix's 7.7M libEngine12.so, for example.
             (substitute* "gen/Makefile.install"
               (("readelf") "false"))))
         (add-after 'install 'prune-undesirable-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion out
                 ;; Remove example binaries.
                 (for-each delete-file-recursively
                           (find-files "." "example"))
                 ;; Delete (now-)empty directories.
                 (for-each rmdir
                           (list "include/firebird/impl"
                                 "lib/firebird/plugins/udr")))))))))
    (native-inputs
     (if (target-riscv64?)
       (list autoconf automake libtool)
       '()))
    (inputs
     (list boost
           editline
           icu4c
           libtommath
           ncurses
           zlib))
    (home-page "https://www.firebirdsql.org")
    (synopsis "Relational database with many ANSI SQL standard features")
    (description
     "Firebird is an SQL @acronym{RDBMS, relational database management system}
with rich support for ANSI SQL (e.g., @code{INSERT...RETURNING}) including
@acronym{UDFs, user-defined functions} and PSQL stored procedures, cursors, and
triggers.  Transactions provide full ACID-compliant referential integrity.

The database requires very little manual maintenance once set up, making it
ideal for small business or embedded use.

When installed as a traditional local or remote (network) database server,
Firebird can grow to terabyte scale with proper tuning---although PostgreSQL
may be a better choice for such very large environments.

Firebird can also be embedded into stand-alone applications that don't want or
need a full client & server.  Used in this manner, it offers richer SQL support
than SQLite as well as the option to seamlessly migrate to a client/server
database later.")
    (license
     ;; See doc/license/README.license.usage.txt for rationale & details.
     (list license:bsd-3                ; src/common/sha2/
           license:bsd-4                ; src/common/enc.cpp
           license:gpl2+                ; builds/posix/make.defaults
           (license:non-copyleft "file:///builds/install/misc/IPLicense.txt"
                                 "InterBase Public License v1.0")
           (license:non-copyleft "file:///builds/install/misc/IDPLicense.txt"
                                 "Initial Developer's Public License v1.0")
           license:lgpl2.1           ; exception for OSI-compatible licences
           license:mpl1.1            ; examples/interfaces/0{6,8}*.cpp
           license:public-domain)))) ; including files without explicit licence

(define-public leveldb
  (package
    (name "leveldb")
    (version "1.23")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/leveldb")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1chxkhb6ajdmj4p8535k4472fbmqvcismll6aapkarsr45yrvgs4"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DBUILD_SHARED_LIBS=ON"
                   "-DLEVELDB_BUILD_TESTS=ON"

                   ;; Don't install(!) the third_party test frameworks below.
                   "-DINSTALL_GTEST=OFF"
                   "-DBENCHMARK_ENABLE_INSTALL=OFF")
           #:phases
           #~(modify-phases %standard-phases
               ;; Ceph uses leveldb and depends on RTTI.
               (add-after 'unpack 'allow-RTTI
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("set\\(CMAKE_CXX_FLAGS \"\\$\\{CMAKE_CXX_FLAGS\\} -fno-rtti\"\\)")
                      ""))))
               (add-after 'unpack 'unpack-third_party-sources
                 ;; These are only for testing, so copying source is fine.
                 (lambda _
                   (copy-recursively #$(package-source googletest)
                                     "third_party/googletest")
                   (copy-recursively #$(package-source googlebenchmark)
                                     "third_party/benchmark"))))))
    (inputs
     (list snappy))
    (home-page "https://github.com/google/leveldb")
    (synopsis "Fast key-value storage library")
    (description
     "LevelDB is a fast key-value storage library that provides an ordered
mapping from string keys to string values.")
    (license license:bsd-3)))

(define-public memcached
  (package
    (name "memcached")
    (version "1.6.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://memcached.org/files/memcached-" version ".tar.gz"))
       (sha256
        (base32 "0ma8qn97hng8vp52s3906g9id75yicf96950hm40zn47k1z2vl5i"))))
    (build-system gnu-build-system)
    (inputs
     (list libevent cyrus-sasl))
    (home-page "https://memcached.org/")
    (synopsis "In-memory caching service")
    (description "Memcached is an in-memory key-value store.  It has a small
and generic API, and was originally intended for use with dynamic web
applications.")
    (license license:bsd-3)))

(define-public libmemcached
  (package
    (name "libmemcached")
    (version "1.0.18")
    ;; We build from the sources since we want to build the extra HTML
    ;; documentation which is not included with the release.
    (source (origin
              (method bzr-fetch)
              (uri (bzr-reference
                    (url "lp:libmemcached/1.0")
                    (revision (string-append "tag:" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1842s4dxdh21gdr46q4dgxigidcs6dkqnbnqjwb9l8r0bqx5nb10"))
              (patches
               (search-patches "libmemcached-build-with-gcc7.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list memcached
           libtool
           autoconf
           automake
           bison
           flex
           perl
           python-sphinx)) ;to build the HTML doc.
    (inputs
     (list libevent cyrus-sasl))
    (outputs '("out" "doc"))
    (arguments
     '(#:tests? #f                      ;many tests fail and use too much time
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'fix-configure.ac
           ;; Move the AC_CONFIG_AUX_DIR macro use under AC_INIT, otherwise we
           ;; get the error ``configure: error: cannot find install-sh,
           ;; install.sh, or shtool in "." "./.." "./../.."`` (see:
           ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19539 and
           ;; https://bugs.launchpad.net/libmemcached/+bug/1803922).
           (lambda _
             (delete-file "bootstrap.sh") ;not useful in the context of Guix
             (substitute* "configure.ac"
               (("^AC_CONFIG_AUX_DIR\\(\\[build-aux\\]\\).*") "")
               (("^AC_INIT.*" anchor)
                (string-append anchor "AC_CONFIG_AUX_DIR([build-aux])\n")))))
         (add-before 'build 'build-and-install-html-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((html (string-append (assoc-ref outputs "doc")
                                        "/share/doc/libmemcached/html/")))
               (invoke "make" "install-html")
               ;; Cleanup useless files.
               (for-each delete-file-recursively
                         (map (lambda (x) (string-append html x))
                              '("_sources" ".doctrees" ".buildinfo")))))))))
    (home-page "https://libmemcached.org/libMemcached.html")
    (synopsis "C++ library for memcached")
    (description "libMemcached is a library to use memcached in C/C++
applications.  It comes with a complete reference guide and documentation of
the API, and provides features such as:
@itemize
@item Asynchronous and synchronous transport support
@item Consistent hashing and distribution
@item Tunable hashing algorithm to match keys
@item Access to large object support
@item Local replication
@end itemize")
    (license license:bsd-3)))

(define-public python-adbc-driver-manager
  (package
    (name "python-adbc-driver-manager")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "adbc_driver_manager" version))
       (sha256
        (base32 "00i3zg6rbzdz767j9w22ajw8rxwbhkrmzwwlmx11q56cvnig0cm3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; TODO: Pack arrow-adbc/c/driver/sqlite for tests.
      #:tests? #f
      #:build-backend "setuptools.build_meta"))
    (native-inputs
     (list python-cython-3
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-typing-extensions))
    (home-page "https://arrow.apache.org/adbc/")
    (synopsis "Generic entrypoint for ADBC drivers in Python")
    (description
     "This package contains bindings for the ADBC Driver Manager, as well as a
@url{https://peps.python.org/pep-0249/,DBAPI 2.0/PEP 249-compatible} interface
on top.  This can be used to load ADBC drivers at runtime and use them from
Python.  Backend-specific packages like @code{adbc_driver_postgresql} wrap
this package in a more convenient interface, and should be preferred where
they exist.")
    (license license:asl2.0)))

(define-public python-prisma
  (package
    (name "python-prisma")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "prisma" version))
       (sha256
        (base32 "1y9m3bailnvid59dl4vx31vysaqbcg6gsppskyymaxg3m96808pc"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-cached-property
           python-click
           python-dotenv
           python-httpx
           python-jinja2
           python-nodeenv
           python-pydantic-2
           python-strenum
           python-tomlkit
           python-typing-extensions))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/RobertCraigie/prisma-client-py")
    (synopsis "Fully type-safe database client")
    (description
     "Prisma Client Python is an auto-generated and fully type-safe database
client.")
    (license license:asl2.0)))

(define-public python-pylibmc
  (package
    (name "python-pylibmc")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pylibmc" version))
       (sha256
        (base32 "1q06696lxpqn155sydg3z6dksimks6n35q72zdjsvarpal8ldypf"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-memcached-daemon
           ;; The test suite requires a memcached server.
           (lambda _
             (invoke "memcached" "-d"))))))
    (native-inputs
     (list memcached python-pytest))
    (inputs
     (list libmemcached zlib cyrus-sasl))
    (home-page "https://sendapatch.se/projects/pylibmc/")
    (synopsis "Python client for memcached")
    (description
     "@code{pylibmc} is a client in Python for memcached.  It is a wrapper
around TangentOrg’s libmemcached library, and can be used as a drop-in
replacement for the @code{python-memcached} library.")
    (license license:bsd-3)))

(define-public litecli
 (package
  (name "litecli")
  (version "1.9.0")
  (source
   (origin
     (method url-fetch)
     (uri (pypi-uri "litecli" version))
     (sha256
      (base32 "1897divrdqlhl1p5jvvm29rg3d99f48s58na7hgdzm1x13x2rbr1"))))
  (build-system python-build-system)
  (propagated-inputs
   (list python-cli-helpers
         python-click
         python-configobj
         python-prompt-toolkit
         python-pygments
         python-sqlparse))
  (native-inputs
   (list python-mock python-pytest))
  (home-page "https://litecli.com")
  (synopsis "CLI for SQLite databases")
  (description
   "@code{litecli} is a command-line client for SQLite databases that has
auto-completion and syntax highlighting.")
  (license license:bsd-3)))

(define-public python-pgspecial
  (package
    (name "python-pgspecial")
    (version "1.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pgspecial" version))
       (sha256
        (base32 "00ddkf565rjcxmfml1z4mmkns1aq8x5s5g85xmnz2scln42y4irq"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-click python-sqlparse python-psycopg2))
    (home-page "https://github.com/dbcli/pgspecial")
    (synopsis
     "Python implementation of PostgreSQL meta commands (backslash commands)")
    (description
     "This Python package provides an API to execute meta-commands (AKA
\"special\", or \"backslash commands\") on PostgreSQL.")
    (license license:bsd-3)))

(define-public python-sqlitedict
  (package
    (name "python-sqlitedict")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sqlitedict" version))
              (sha256
               (base32
                "05sxy016k3p5sjjhdg0ad9z15i6vm3rq4cr9m8nrc7jfdx0p18r3"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv"
                        "-k"
                        ;; No idea why these fail.
                        (string-append "not test_py24_error"
                                       " and not test_tablenames"))))))))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/piskvorky/sqlitedict")
    (synopsis "Persistent dict backed up by sqlite3 and pickle")
    (description
     "This package provides a lightweight wrapper around the sqlite3 database
with a simple, Pythonic @code{dict}-like interface and support for
multi-thread access.")
    (license license:asl2.0)))

(define-public pgcli
  (package
    (name "pgcli")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pgcli" version))
       (sha256
        (base32 "1dy6yzak696107pqv83296h0xhc3ahlfaydm80593gwn37krgpkc"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cli-helpers
           python-click
           python-configobj
           python-pendulum
           python-pgspecial
           python-prompt-toolkit
           python-psycopg2
           python-pygments
           python-setproctitle
           python-sqlparse))
    (native-inputs
     (list python-ipython-sql))
    (home-page "https://www.pgcli.com")
    (synopsis "PostgreSQL CLI with autocompletion and syntax highlighting")
    (description
     "@code{pgcli} is a command line interface for PostgreSQL with
autocompletion and syntax highlighting.")
    (license license:bsd-3)))

(define-public mycli
  (package
    (name "mycli")
    (version "1.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mycli" version))
       (sha256
        (base32 "0231v7f6q84mjmi1h0ni3s55m2g8p5d7x5q49bgkxlaz2bc2xwgy"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))                    ; tests expect a running MySQL
    (propagated-inputs
     (list python-cli-helpers
           python-click
           python-configobj
           python-prompt-toolkit
           python-pyaes
           python-pygments
           python-pymysql
           python-pyperclip
           python-sqlparse))
    (home-page "https://www.mycli.net")
    (synopsis
     "Terminal Client for MySQL with AutoCompletion and Syntax Highlighting")
    (description
     "MyCLI is a command line interface for MySQL, MariaDB, and Percona with
auto-completion and syntax highlighting.")
    (license license:bsd-3)))

;; XXX When updating, check whether boost-for-mysql is still needed.
;; It might suffice to patch ‘cmake/boost.cmake’ as done in the past.
(define-public mysql
  (package
    (name "mysql")
    (version "5.7.33")
    (source (origin
             (method url-fetch)
             (uri (list (string-append
                          "https://dev.mysql.com/get/Downloads/MySQL-"
                          (version-major+minor version) "/"
                          name "-" version ".tar.gz")
                        (string-append
                          "https://downloads.mysql.com/archives/get/file/"
                          name "-" version ".tar.gz")))
             (sha256
              (base32
               "1bb343mf7n0qg2qz497gxjsqprygrjz1q1pbz76hgqxnsy08sfxd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DBUILD_CONFIG=mysql_release"
         "-DWITH_SSL=system"
         "-DWITH_ZLIB=system"
         "-DDEFAULT_CHARSET=utf8"
         "-DDEFAULT_COLLATION=utf8_general_ci"
         "-DMYSQL_DATADIR=/var/lib/mysql"
         "-DMYSQL_UNIX_ADDR=/run/mysqld/mysqld.sock"
         "-DINSTALL_INFODIR=share/mysql/docs"
         "-DINSTALL_MANDIR=share/man"
         "-DINSTALL_PLUGINDIR=lib/mysql/plugin"
         "-DINSTALL_SCRIPTDIR=bin"
         "-DINSTALL_INCLUDEDIR=include/mysql"
         "-DINSTALL_DOCREADMEDIR=share/mysql/docs"
         "-DINSTALL_SUPPORTFILESDIR=share/mysql"
         "-DINSTALL_MYSQLSHAREDIR=share/mysql"
         "-DINSTALL_DOCDIR=share/mysql/docs"
         "-DINSTALL_SHAREDIR=share/mysql"
         ;; Get rid of test data.
         "-DINSTALL_MYSQLTESTDIR="
         "-DINSTALL_SQLBENCHDIR=")
       #:phases (modify-phases %standard-phases
                  (add-after
                   'install 'remove-extra-binaries
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       ;; Remove the 3 *_embedded files, which weigh in at
                       ;; 14 MiB each.
                       (for-each delete-file
                                 (find-files (string-append out "/bin")
                                             "_embedded$"))
                       #t)))
                  (add-after
                   'install 'wrap-mysql_helpers
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bin (string-append out "/bin"))
                            (awk (assoc-ref inputs "gawk"))
                            (coreutils (assoc-ref inputs "coreutils"))
                            (grep (assoc-ref inputs "grep"))
                            (ps (assoc-ref inputs "procps"))
                            (sed (assoc-ref inputs "sed")))
                       (wrap-program (string-append bin "/mysql_config")
                         `("PATH" ":" suffix
                           (,(string-append awk "/bin")
                            ,(string-append coreutils "/bin")
                            ,(string-append sed "/bin"))))
                       (wrap-program (string-append bin "/mysqld_safe")
                         `("PATH" ":" suffix
                           (,(string-append awk "/bin")
                            ,(string-append coreutils "/bin")
                            ,(string-append grep "/bin")
                            ,(string-append ps "/bin")
                            ,(string-append sed "/bin"))))))))))
    (native-inputs
     (list bison perl pkg-config))
    (inputs
     (list bash-minimal
           boost-for-mysql
           coreutils
           gawk
           grep
           libaio
           libtirpc
           ncurses
           openssl-1.1
           procps
           rpcsvc-proto ; rpcgen
           sed
           zlib))
    (home-page "https://www.mysql.com/")
    (synopsis "Fast, easy to use, and popular database")
    (description
     "MySQL is a fast, reliable, and easy to use relational database
management system that supports the standardized Structured Query
Language.")
    (license license:gpl2)))

(define-public mariadb
  (package
    (name "mariadb")
    (version "10.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.mariadb.com/MariaDB"
                                  "/mariadb-" version "/source/mariadb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1ciw7y08wms9g3hzhyria49r1b9n5wpbhkndazv95d925c8x1jsp"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled libraries, but preserve CMakeLists.txt.
                  (for-each (lambda (file)
                              (unless (string-suffix? "CMakeLists.txt" file)
                                (delete-file file)))
                            (append (find-files "extra/wolfssl")
                                    (find-files "zlib")))))))
    (build-system cmake-build-system)
    (outputs '("out" "lib" "dev"))
    (arguments
     `(#:configure-flags
       (list
         "-DBUILD_CONFIG=mysql_release"

         ;; Ensure the system libraries are used.
         "-DWITH_JEMALLOC=yes"
         "-DWITH_LIBFMT=system"
         "-DWITH_PCRE=system"
         "-DWITH_SSL=system"
         "-DWITH_ZLIB=system"

         "-DDEFAULT_CHARSET=utf8"
         "-DDEFAULT_COLLATION=utf8_general_ci"
         "-DMYSQL_DATADIR=/var/lib/mysql"
         "-DMYSQL_UNIX_ADDR=/run/mysqld/mysqld.sock"

         ;; Do not install the benchmark suite.
         "-DINSTALL_SQLBENCHDIR=false"

         (string-append "-DCMAKE_INSTALL_PREFIX=" (assoc-ref %outputs "lib"))
         (string-append "-DCMAKE_INSTALL_RPATH=" (assoc-ref %outputs "lib")
                        "/lib")
         (string-append "-DINSTALL_INFODIR=" (assoc-ref %outputs "out")
                        "/share/mysql/docs")
         (string-append "-DINSTALL_MANDIR=" (assoc-ref %outputs "out")
                        "/share/man")
         (string-append "-DINSTALL_SCRIPTDIR=" (assoc-ref %outputs "out") "/bin")
         (string-append "-DINSTALL_BINDIR=" (assoc-ref %outputs "out") "/bin")
         "-DCMAKE_INSTALL_LIBDIR=lib"
         "-DINSTALL_PLUGINDIR=lib/mysql/plugin"
         (string-append "-DINSTALL_INCLUDEDIR=" (assoc-ref %outputs "dev")
                        "/include/mysql")
         (string-append "-DINSTALL_DOCREADMEDIR=" (assoc-ref %outputs "out")
                        "/share/mysql/docs")
         (string-append "-DINSTALL_DOCDIR=" (assoc-ref %outputs "out")
                        "/share/mysql/docs")
         (string-append "-DINSTALL_SUPPORTFILESDIR=" (assoc-ref %outputs "out")
                        "/share/mysql/support-files")
         "-DINSTALL_MYSQLSHAREDIR=share/mysql"
         "-DINSTALL_SHAREDIR=share")
       ;; The test suite has spurious failures (mostly timeouts) if run in
       ;; parallel on various machines.  Only enable parallel tests on
       ;; architectures which are likely to not have this issue.
       #:parallel-tests? ,(target-x86-64?)
       #:phases
       (modify-phases %standard-phases
         ;; TODO: Move this patch to the source field.
         ,@(if (target-riscv64?)
             `((add-after 'unpack 'patch-source
                 (lambda* (#:key inputs native-inputs #:allow-other-keys)
                   (invoke "patch" "-p1" "--force" "--input"
                           (assoc-ref (or native-inputs inputs)
                                      "patch-file")))))
             '())
         (add-after 'unpack 'adjust-output-references
           (lambda _
             ;; The build system invariably prepends $CMAKE_INSTALL_PREFIX
             ;; to other variables such as $INSTALL_INCLUDEDIR, which does
             ;; not work when the latter uses an absolute file name.
             (substitute* "libmariadb/mariadb_config/mariadb_config.c.in"
               (("%s/@INSTALL_INCLUDEDIR@")
                (string-append "@INSTALL_INCLUDEDIR@"))
               ;; As of 10.5.8, the mariadb_config program tries to be
               ;; clever and computes the installation directory relative
               ;; to /proc/self/exe when running on Linux.  Make it fall
               ;; back to the old behaviour.
               (("defined\\(__linux__\\)")
                "0"))
             (substitute* "libmariadb/mariadb_config/libmariadb.pc.in"
               (("\\$\\{prefix\\}/@INSTALL_INCLUDEDIR@")
                "@INSTALL_INCLUDEDIR@"))
             (substitute* "support-files/mariadb.pc.in"
               (("^(include|bin|script|doc|man)dir=\\$\\{prefix\\}/" _ dir)
                (string-append dir "dir=")))
             (substitute* "include/CMakeLists.txt"
               (("\\\\\\$\\{CMAKE_INSTALL_PREFIX\\}/\\$\\{INSTALL_INCLUDEDIR\\}")
                "${INSTALL_INCLUDEDIR}"))
             (substitute* "cmake/mariadb_connector_c.cmake"
               (("\\\\\\$\\{CMAKE_INSTALL_PREFIX\\}/\\$\\{INSTALL_BINDIR\\}")
                "${INSTALL_BINDIR}"))))
         (add-after 'unpack 'adjust-tests
           (lambda _
             (let ((disabled-tests
                    '(;; These fail because root@hostname == root@localhost in
                      ;; the build environment, causing a user count mismatch.
                      ;; See <https://jira.mariadb.org/browse/MDEV-7761>.
                      "main.explain_non_select"
                      "main.upgrade_MDEV-19650"
                      "roles.acl_statistics"
                      "main.stat_tables_innodb"
                      "main.stat_tables"
                      "main.mysql_upgrade"

                      ;; Probably same as above, test failure reported upstream:
                      ;; <https://jira.mariadb.org/browse/MDEV-26320>.
                      "main.selectivity_no_engine"

                      ;; FIXME: This test checks various table encodings and
                      ;; fails because Guix defaults to UTF8 instead of the
                      ;; upstream default latin1_swedish_ci.  It's not easily
                      ;; substitutable because several encodings are tested.
                      "main.system_mysql_db"

                      ;; XXX: This test occasionally fails on i686-linux:
                      ;; <https://jira.mariadb.org/browse/MDEV-24458>
                      ,@(if (string-prefix? "i686" (%current-system))
                            '("main.myisampack")
                            '())))

                   ;; This file contains a list of known-flaky tests for this
                   ;; release.  Append our own items.
                   (unstable-tests (open-file "mysql-test/unstable-tests" "a")))
               (for-each (lambda (test)
                           (format unstable-tests "~a : ~a\n"
                                   test "Disabled in Guix"))
                         disabled-tests)
               (close-port unstable-tests)

               (substitute* "mysql-test/suite/binlog/t/binlog_mysqlbinlog_stop_never.test"
                 (("/bin/bash")
                  (which "bash")))

               (substitute* "mysql-test/mariadb-test-run.pl"
                 (("/bin/ls") (which "ls"))
                 (("/bin/sh") (which "sh"))))))
         (replace 'check
           (lambda* (#:key (tests? #t) parallel-tests? #:allow-other-keys)
             (if tests?
                 (with-directory-excursion "mysql-test"
                   (invoke "./mariadb-test-run"
                           "--verbose"
                           "--retry=3"
                           "--suite=main"
                           "--testcase-timeout=40"
                           "--suite-timeout=600"
                           "--parallel" (number->string (if parallel-tests?
                                                          (parallel-job-count)
                                                          1))
                           ;; Skip the replication tests: they are very I/O
                           ;; intensive and frequently causes indeterministic
                           ;; failures even on powerful hardware.
                           "--skip-rpl"
                           "--skip-test-list=unstable-tests"))
                 (format #t "test suite not run~%"))))
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out     (assoc-ref outputs "out"))
                   (dev     (assoc-ref outputs "dev"))
                   (lib     (assoc-ref outputs "lib"))
                   (openssl (dirname (search-input-file inputs "lib/libssl.so"))))
              (substitute* (list (string-append out "/bin/mariadb-install-db")
                                 (string-append out "/bin/mysql_install_db"))
                (("basedir=\"\"")
                 (string-append "basedir=\"" out "\""))
                (("\\$basedir/share/mysql")
                 (string-append lib "/share/mysql")))

              (with-directory-excursion lib
                ;; Remove tests.
                (delete-file-recursively "mysql-test")
                ;; Remove static libraries.
                (for-each delete-file (find-files "lib" "\\.a$")))

              (with-directory-excursion out
                (delete-file "share/man/man1/mysql-test-run.pl.1")
                ;; Delete huge and unnecessary executables.
                (for-each delete-file (find-files "bin" "test$")))
              (mkdir-p (string-append dev "/share"))
              (mkdir-p (string-append dev "/bin"))
              (rename-file (string-append lib "/bin/mariadbd")
                           (string-append out "/bin/mariadbd"))
              (rename-file (string-append lib "/bin/mysqld")
                           (string-append out "/bin/mysqld"))
              (mkdir-p (string-append dev "/lib"))
              (rename-file (string-append lib "/lib/pkgconfig")
                           (string-append dev "/lib/pkgconfig"))
              (rename-file (string-append out "/bin/mariadb_config")
                           (string-append dev "/bin/mariadb_config"))
              (rename-file (string-append out "/bin/mysql_config")
                           (string-append dev "/bin/mysql_config"))

              ;; Embed an absolute reference to OpenSSL in mysql_config
              ;; and the pkg-config file to avoid propagation.
              ;; XXX: how to do this for mariadb_config.c.in?
              (substitute* (list (string-append dev "/bin/mysql_config")
                                 (string-append dev "/lib/pkgconfig/mariadb.pc"))
                (("-lssl -lcrypto" all)
                 (string-append "-L" openssl " " all)))))))))
    (native-inputs
     `(,@(if (target-riscv64?)
           `(("patch" ,patch)
             ("patch-file" ,(search-patch "mariadb-rocksdb-atomic-linking.patch")))
           `())
        ("bison" ,bison)
        ("perl" ,perl)))
    (inputs
     (list fmt
           jemalloc
           libaio
           libxml2
           ncurses
           openssl
           linux-pam
           pcre2
           xz
           zlib))
    ;; The test suite is very resource intensive and can take more than three
    ;; hours on a x86_64 system.  Give slow and busy machines some leeway.
    (properties '((timeout . 64800)))        ;18 hours
    (home-page "https://mariadb.org/")
    (synopsis "SQL database server")
    (description
     "MariaDB is a multi-user and multi-threaded SQL database server, designed
as a drop-in replacement of MySQL.")
    (license license:gpl2)))

(define-public mariadb-connector-c
  (package
    (name "mariadb-connector-c")
    (version "3.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.mariadb.org/f/connector-c-" version
             "/mariadb-connector-c-" version "-src.tar.gz"
             "/from/https%3A//mirrors.ukfast.co.uk/sites/mariadb/?serve"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xb8fiissblxb319y5ifqqp86zblwis789ipb753pcb4zpnsaw82"))))
    (inputs
     (list openssl))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://mariadb.com/kb/en/mariadb-connector-c/")
    (synopsis "Client library to connect to MySQL or MariaDB")
    (description "The MariaDB Connector/C is used to connect applications
developed in C/C++ to MariaDB and MySQL databases.")
    (license license:lgpl2.1+)))

(define-public galera
  (package
    (name "galera")
    (version "26.4.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/codership/galera")
                    (commit (string-append "release_" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1agw763qx9778krcpmgvwcps73ipyjwl0niwsykcxldvzzs314r5"))))
    (build-system cmake-build-system)
    (inputs
     (list boost openssl))
    (native-inputs
     (list check))
    (home-page "https://github.com/codership/galera/")
    (synopsis "Extension to the MariaDB database server")
    (description
     "Galera is a wsrep-provider that is used with MariaDB for load-balancing
and high-availability (HA).")
    (license license:gpl2)))                  ;'COPYING' says "version 2" only

;; Don't forget to update the other postgresql packages when upgrading this one.
(define-public postgresql-16
  (package
    (name "postgresql")
    (version "16.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "0vvd73rzj0sl294v15bh8yslakqv412bxqzlkqxyjwxa8pb6c5wp"))
              (patches (search-patches
                        "postgresql-disable-normalize_exec_path.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--with-uuid=e2fs" "--with-openssl"
              (string-append "--mandir=" #$output "/share/man")
              ;; PostgreSQL installs its own Makefile (should it?).
              ;; Prevent it from retaining needless references to
              ;; the build tools in order to save size.
              "MKDIR_P=mkdir -p" "INSTALL_BIN=install -c"
              "LD=ld" "TAR=tar")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-/bin/sh
            (lambda _
              ;; Refer to the actual shell.
              (substitute* '("src/bin/pg_ctl/pg_ctl.c"
                             "src/bin/psql/command.c")
                (("/bin/sh") (which "sh")))))
          (add-before 'configure 'set-socket-dir
            (lambda _
              (substitute* '("src/include/pg_config_manual.h")
                (("DEFAULT_PGSOCKET_DIR[^\n]*")
                 "DEFAULT_PGSOCKET_DIR \"/var/run/postgresql\""))))
          (add-after 'build 'build-contrib
            (lambda _
              (invoke "make" "-C" "contrib")))
          (add-after 'install 'install-contrib
            (lambda _
              (invoke "make" "-C" "contrib" "install")))
          (add-after 'install 'install-manuals
            (lambda _
              (with-directory-excursion "doc/src/sgml"
                (invoke "make" "install-man")
                (invoke "make" "postgres.info")
                (install-file "postgres.info"
                              (string-append #$output "/share/info"))))))))
    (native-inputs
     (list docbook-xml-4.5 docbook2x libxml2 perl pkg-config texinfo))
    (inputs
     (list icu4c readline `(,util-linux "lib") openssl zlib))
    (home-page "https://www.postgresql.org/")
    (synopsis "Powerful object-relational database system")
    (description
     "PostgreSQL is a powerful object-relational database system.  It is fully
ACID compliant, has full support for foreign keys, joins, views, triggers, and
stored procedures (in multiple languages).  It includes most SQL:2008 data
types, including INTEGER, NUMERIC, BOOLEAN, CHAR, VARCHAR, DATE, INTERVAL, and
TIMESTAMP.  It also supports storage of binary large objects, including
pictures, sounds, or video.")
    (license (license:x11-style "file://COPYRIGHT"))))

(define-public postgresql-15
  (package
    (inherit postgresql-16)
    (name "postgresql")
    (version "15.8")
    (source (origin
              (inherit (package-source postgresql-16))
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "0snbxmlygf7m4cxjpscmz3yjn4lnqsw313y9xgpv7vk9k9gm20s4"))
              (patches (search-patches
                        "postgresql-disable-resolve_symlinks.patch"))))
    (native-inputs (modify-inputs (package-native-inputs postgresql-16)
                     (delete "pkg-config")))
    (inputs (modify-inputs (package-inputs postgresql-16)
              (delete "icu4c")))))

(define-public postgresql-14
  (package
    (inherit postgresql-15)
    (name "postgresql")
    (version "14.13")
    (source (origin
              (inherit (package-source postgresql-15))
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "0misc5yiklflz96n7wxcdzzg0lcc4ahd0flzqsg6mcjs955krajr"))))))

(define-public postgresql-13
  (package
    (inherit postgresql-14)
    (version "13.16")
    (source (origin
              (inherit (package-source postgresql-14))
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "0rc8rpsw2lwa5af35zd8iifah02wg2rnn1i890h2h8zh55hvpjy9"))))))

(define-deprecated/public postgresql-11 #f
  (package
    (inherit postgresql-13)
    (name "postgresql")
    (version "11.22")
    (source (origin
              (inherit (package-source postgresql-13))
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "1w71xf97i3hha6vl05xqf960k75nczs6375w3f2phwhdg9ywkdrc"))))
    (native-inputs
     (modify-inputs (package-native-inputs postgresql-13)
       (replace "docbook-xml" docbook-xml-4.2)))))

(define-deprecated/public postgresql-10 #f
  (package
    (inherit postgresql-13)
    (version "10.23")
    (source (origin
              (inherit (package-source postgresql-13))
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "1sgfssjc9lnzijhn108r6z26fri655k413f1c9b8wibjhd9b594l"))))
    (native-inputs
     (modify-inputs (package-native-inputs postgresql-13)
       (append opensp docbook-sgml-4.2)
       (delete "docbook-xml")))))

(define-public postgresql postgresql-14)

(define-public libpg-query-17
  (package
    (name "libpg-query")
    (version "17-6.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/pganalyze/libpg_query.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y4594pf2wl88m66z9xhxzmm675408dbw7bkg4snfcifij97f0c7"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; tests run as part of build
           #:make-flags
           #~(list (string-append "prefix=" #$output))
           #:phases
           #~(modify-phases %standard-phases
              (replace 'configure
                (lambda _
                  (setenv "CC" #$(cc-for-target)))))))
    (native-inputs
     (list which))
    (synopsis "Postgres SQL parser")
    (description "This package provides a Postgres SQL parser as a library.")
    (home-page "https://github.com/pganalyze/libpg_query")
    (license license:bsd-3)))

(define-public libpg-query libpg-query-17)

(define-public timescaledb
  (package
    (name "timescaledb")
    (version "2.16.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/timescale/timescaledb")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v17x00a15il4r3rbr0waqjv1nwzy6rcqxgfi2hdk1x235s5dg5h"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove files carrying the proprietary TIMESCALE license.
               '(begin
                  (delete-file-recursively "tsl")
                  (for-each delete-file
                            '("scripts/c_license_header-timescale.h"
                              "scripts/license_tsl.spec"
                              "scripts/sql_license_tsl.sql"
                              "test/perl/AccessNode.pm"
                              "test/perl/DataNode.pm"
                              "test/perl/TimescaleNode.pm"))))))
    (build-system cmake-build-system)
    (arguments
     (list #:imported-modules `((guix build union)
                                ,@%cmake-build-system-modules)
           #:modules `(,@%cmake-build-system-modules
                       (guix build union)
                       (ice-9 match))
           #:configure-flags #~(list "-DAPACHE_ONLY=ON"
                                     "-DSEND_TELEMETRY_DEFAULT=OFF")
           #:test-target "regresschecklocal"
           #:phases
           #~(modify-phases (@ (guix build cmake-build-system) %standard-phases)
               (add-after 'unpack 'patch-install-location
                 (lambda _
                   ;; Install extension to the output instead of the
                   ;; PostgreSQL store directory.
                   (substitute* '("CMakeLists.txt"
                                  "cmake/GenerateScripts.cmake"
                                  "sql/CMakeLists.txt")
                     (("\\$\\{PG_SHAREDIR\\}/extension")
                      (string-append #$output "/share/extension")))
                   ;; Likewise for the library.
                   (substitute* '("src/CMakeLists.txt"
                                  "src/loader/CMakeLists.txt")
                     (("\\$\\{PG_PKGLIBDIR\\}")
                      (string-append #$output "/lib")))))
               (add-after 'unpack 'remove-kernel-version
                 ;; Do not embed the running kernel version for reproducible
                 ;; builds
                 (lambda _
                   (substitute* "src/config.h.in"
                     (("BUILD_OS_VERSION ..CMAKE_SYSTEM_VERSION.")
                      "BUILD_OS_VERSION \""))))
               ;; Run the tests after install to make it easier to create the
               ;; required PostgreSQL+TimescaleDB filesystem union.
               (delete 'check)
               (add-after 'install 'prepare-tests
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((pg-data (string-append (getcwd) "/../pg-data"))
                         (pg-union (string-append (getcwd) "/../pg-union")))
                     (match inputs
                       (((names . directories) ...)
                        (union-build pg-union (cons #$output directories))))
                     (setenv "PATH" (string-append pg-union "/bin:"
                                                   (getenv "PATH")))
                     (invoke "initdb" "-D" pg-data)
                     (copy-file "test/postgresql.conf"
                                (string-append pg-data "/postgresql.conf"))

                     (invoke "pg_ctl" "-D" pg-data
                             "-o" (string-append "-k " pg-data)
                             "-l" (string-append pg-data "/db.log")
                             "start"))))
               #$@(cond
                   ((string=? "x86-64-linux" (%current-system))
                    #~())
                   ((member (%current-system)
                            (list "aarch64-linux" "i686-linux"))
                    #~((add-after 'unpack 'skip-failing-tests
                         (lambda _
                           (substitute* "test/sql/CMakeLists.txt"
                             (((string-append
                                "("
                                (string-join
                                 '(" append\\.sql\\.in"
                                   " chunk_adaptative\\.sql"
                                   " histogram_test\\.sql\\.in")
                                 "|")
                                ")")
                               all)
                              (string-append "#" all)))))))
                   (else
                    #~((add-after 'unpack 'skip-failing-tests
                         (lambda _
                           (substitute* "test/sql/CMakeLists.txt"
                             (("histogram_test\\.sql\\.in")
                              "#histogram_test.sql.in")))))))
               (add-after 'prepare-tests 'check
                 (assoc-ref %standard-phases 'check)))))
    (inputs (list openssl postgresql))
    (home-page "https://www.timescale.com/")
    (synopsis "Time-series extension for PostgreSQL")
    (description
     "TimescaleDB is a database designed to make SQL scalable for
time-series data.  It is engineered up from PostgreSQL and packaged as a
PostgreSQL extension, providing automatic partitioning across time and space
(partitioning key), as well as full SQL support.")
    (license license:asl2.0)))

(define-public pgvector
  (package
    (name "pgvector")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pgvector/pgvector")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19zcjrlmyj7gfbn8prh014yq50iy4dg97pirsm7idxsr829vwyc5"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Do not use -march=native
      #:make-flags
      '(list "OPTFLAGS=")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'install
            (lambda _
              (let ((extension (string-append #$output "/share/extension"))
                    (lib (string-append #$output "/lib"))
                    (headers (string-append #$output "/include/server/extension/vector")))
                (for-each mkdir-p (list extension lib headers))
                (install-file "vector.so" lib)
                (chmod (string-append lib "/vector.so") #o755)
                (install-file "vector.control" extension)
                (for-each (lambda (file)
                            (install-file file extension))
                          (find-files "sql" "\\.sql$"))
                (install-file "src/vector.h" headers)))))))
    (inputs (list postgresql))
    (home-page "https://github.com/pgvector/pgvector")
    (synopsis "Vector similarity search for Postgres")
    (description
     "This package provides a vector similarity search extension for Postgres.
Store your vectors with the rest of your data.  It supports:

@itemize
@item exact and approximate nearest neighbor search;
@item L2 distance, inner product, and cosine distance;
@item any language with a Postgres client.
@end itemize
")
    (license (license:x11-style "file://COPYRIGHT"))))

(define-public pgloader
  (package
    (name "pgloader")
    (version "3.6.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dimitri/pgloader")
             (commit (string-append "v" version))))
       (sha256
        (base32 "03kp3ms2sjz4gwb94xs404mi63fnv1bq00hyqxyvc9csmicxzawn"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     ;; NOTE: (Sharlatan-20210119T211511+0000) Tests are disabled due to being
     ;; dependent on Quicklisp, main build target is `pgloader-standalone' which
     ;; does not require Quicklisp workarounds. There is no `install' target
     ;; configured in Makefile.
     (list #:tests? #f
           #:strip-binaries? #f
           #:make-flags
           #~(list "pgloader-standalone" "BUILDAPP_SBCL=buildapp")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'set-home
                 (lambda _
                   (setenv "HOME" "/tmp")))
               (add-after 'unpack 'patch-Makefile
                 (lambda _
                   (substitute* "Makefile"
                     (("--sbcl.*") "--sbcl $(CL) --asdf-path . \\\n"))))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((bin (string-append #$output "/bin")))
                     (mkdir-p bin)
                     (install-file "build/bin/pgloader"  bin)))))))
    (native-inputs
     (list buildapp sbcl))
    (inputs
     (list sbcl-alexandria
           sbcl-cl-abnf
           sbcl-cl-base64
           sbcl-cl-csv
           sbcl-cl-fad
           sbcl-cl-log
           sbcl-cl-markdown
           sbcl-cl-mustache
           sbcl-cl-ppcre
           sbcl-cl-sqlite
           sbcl-closer-mop
           sbcl-command-line-arguments
           sbcl-db3
           sbcl-drakma
           sbcl-esrap
           sbcl-flexi-streams
           sbcl-ixf
           sbcl-local-time
           sbcl-lparallel
           sbcl-metabang-bind
           sbcl-mssql
           sbcl-postmodern
           sbcl-py-configparser
           sbcl-qmynd
           sbcl-quri
           sbcl-split-sequence
           sbcl-trivial-backtrace
           sbcl-usocket
           sbcl-uuid
           sbcl-yason
           sbcl-zs3))
    (home-page "https://pgloader.io/")
    (synopsis "Tool to migrate data to PostgreSQL")
    (description
     "@code{pgloader} is a program that can load data or migrate databases from
CSV, DB3, iXF, SQLite, MS-SQL or MySQL to PostgreSQL.")
    (license (license:x11-style "file://LICENSE"))))

(define-public python-pymysql
  (package
    (name "python-pymysql")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pymysql" version))
       (sha256
        (base32 "1l2cj0ps96g3bblvhdszgyjv9bi405bxrx0bqq1p8h9bmwd629z1"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-cryptography python-pynacl))
    (arguments
     `(#:tests? #f))                    ; tests expect a running MySQL
    (home-page "https://github.com/PyMySQL/PyMySQL/")
    (synopsis "Pure-Python MySQL driver")
    (description
     "PyMySQL is a pure-Python MySQL client library, based on PEP 249.
Most public APIs are compatible with @command{mysqlclient} and MySQLdb.")
    (license license:expat)))

(define-public qdbm
  (package
    (name "qdbm")
    (version "1.8.78")
    (source
      (origin
       (method url-fetch)
       (uri (string-append "http://fallabs.com/" name "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "0gmpvhn02pkq280ffmn4da1g4mdr1xxz7l80b7y4n7km1mrzwrml"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))
       #:make-flags (list "CFLAGS=-fPIC")))
    (home-page "https://fallabs.com/qdbm/")
    (synopsis "Key-value database")
    (description "QDBM is a library of routines for managing a
database.  The database is a simple data file containing key-value
pairs.  Every key and value is serial bytes with variable length.
Binary data as well as character strings can be used as a key or a
value.  There is no concept of data tables or data types.  Records are
organized in a hash table or B+ tree.")
    (license license:lgpl2.1+)))

(define-public recutils
  (package
    (name "recutils")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/recutils/recutils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03kf91f20brn2ffljfjzirxh5xj99m1mvvspcx2lph9000mmj0b3"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--disable-static"
                   (string-append "--with-bash-headers="
                                  (search-input-directory %build-inputs
                                                          "include/bash")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'symlink-bash-loadables
                 (lambda* (#:key outputs #:allow-other-keys)
                   (with-directory-excursion (string-append
                                              (assoc-ref outputs "out")
                                              "/lib")
                     (mkdir "bash")
                     (for-each
                      (compose symlink
                               (lambda (loadable)
                                 (values
                                  (string-append (getcwd) "/" loadable ".so")
                                  (string-append "bash/" loadable))))
                      '("readrec" "testrec"))))))))
    (native-inputs
     (list bc check-0.14 pkg-config))
    (inputs
     ;; TODO: Add more optional inputs.
     (list bash                         ; /bin/bash for native compilation
           `(,bash "include")
           curl
           libgcrypt
           `(,util-linux "lib")))
    (synopsis "Manipulate plain text files as databases")
    (description
     "GNU Recutils is a set of tools and libraries for creating and
manipulating text-based, human-editable databases.  Despite being text-based,
databases created with Recutils carry all of the expected features such as
unique fields, primary keys, time stamps and more.  Many different field
types are supported, as is encryption.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/recutils/")))

(define-public emacs-rec-mode
  (package
    (name "emacs-rec-mode")
    (version "1.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/"
                                  "rec-mode-" version ".tar"))
              (sha256
               (base32
                "0pi483g5qgz6gvyi13a4ldfbkaad3xkad08aqfcnmsdylvc9zzma"))
              (snippet #~(begin (delete-file "rec-mode.info")))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-program-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (emacs-substitute-variables "rec-mode.el"
                ("rec-recfix" (search-input-file inputs "bin/recfix"))
                ("rec-recinf" (search-input-file inputs "bin/recinf"))
                ("rec-recsel" (search-input-file inputs "bin/recsel")))))
          (add-before 'install 'make-info
            (lambda _
              (invoke "makeinfo" "--no-split"
                      "-o" "rec-mode.info" "rec-mode.texi"))))))
    (inputs (list recutils))
    (native-inputs (list texinfo))
    (home-page "https://www.gnu.org/software/recutils/")
    (synopsis "Emacs mode for working with recutils database files")
    (description "This package provides an Emacs major mode @code{rec-mode}
for working with GNU Recutils text-based, human-editable databases.  It
supports editing, navigation, and querying of recutils database files
including field and record folding.")
    (license license:gpl3+)))

(define-public emacs-recutils
  (deprecated-package "emacs-recutils" emacs-rec-mode))

(define-public rocksdb
  (package
    (name "rocksdb")
    (version "6.26.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/facebook/rocksdb")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mylma106w93kxhj89g9y1ccdq7m9m94wrmv5nyr17yc1zsk87sg"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; TODO: unbundle gtest.
                  (delete-file "build_tools/gnu_parallel")
                  (substitute* "Makefile"
                    (("build_tools/gnu_parallel") "parallel"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DROCKSDB_BUILD_SHARED=1"
             ;; Ceph requires that RTTI is enabled.
             "-DUSE_RTTI=1"
             ;; Prevent the build from passing '-march=native' to the compiler.
             "-DPORTABLE=1")

       ;; Many tests fail on 32-bit platforms. There are multiple
       ;; reports about this upstream, but it's not going to be
       ;; supported any time soon.  What's worse: Release builds don't
       ;; include tests, and overriding the build system to build
       ;; tests anyway fails with missing TEST_ symbols.
       #:tests? #false
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-CMakeLists.txt
           (lambda _
             (substitute* "CMakeLists.txt"
               ;; build reproducibly
               (("set\\(BUILD_DATE \"\\$\\{TS\\}\"")
                "set(BUILD_DATE \"1970-01-01\""))))
         (add-after 'unpack 'build-generically
           (lambda _
             (substitute* "CMakeLists.txt"
               (("if\\(HAVE_SSE42\\)") "if(FALSE)")))))))
    (native-inputs
     (list parallel perl procps python which))
    (inputs
     (list bzip2
           gflags
           jemalloc
           lz4
           snappy
           zlib))
    (home-page "https://rocksdb.org/")
    (synopsis "Persistent key-value store for fast storage")
    (description
     "RocksDB is a library that forms the core building block for a fast
key-value server, especially suited for storing data on flash drives.  It
has a @dfn{Log-Structured-Merge-Database} (LSM) design with flexible tradeoffs
between @dfn{Write-Amplification-Factor} (WAF), @dfn{Read-Amplification-Factor}
(RAF) and @dfn{Space-Amplification-Factor} (SAF).  It has multi-threaded
compactions, making it specially suitable for storing multiple terabytes of
data in a single database.  RocksDB is partially based on @code{LevelDB}.")
    ;; RocksDB is dual licensed under GPL2 and ASL 2.0.  Some header
    ;; files carry the 3-clause BSD license.
    (license (list license:gpl2 license:asl2.0 license:bsd-3))))

(define-public sparql-query
  (package
    (name "sparql-query")
    (version "1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/tialaramex/sparql-query")
                     (commit version)))
              (sha256
               (base32 "0a84a89idpjhj9w2y3fmvzv7ldps1cva1kxvfmh897k02kaniwxk"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     (list curl glib libxml2 ncurses readline))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; The Makefile uses git to obtain versioning information. This phase
         ;; substitutes the git invocation with the package version.
         (add-after 'unpack 'remove-git-dependency
           (lambda _
             (substitute* "Makefile"
               (("^gitrev :=.*$")
                (string-append "gitrev = \"v" ,version "\"")))
             #t))
         ;; The install phase of the Makefile assumes $PREFIX/usr/local/bin.
         ;; This replacement does the same thing, except for using $PREFIX/bin
         ;; instead.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "sparql-query" bin)
               (symlink (string-append bin "/sparql-query")
                        (string-append bin "/sparql-update")))
             #t))
         (replace 'check
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" `(,@make-flags "scan-test"))
             (invoke "./scan-test"))))))
    (home-page "https://github.com/tialaramex/sparql-query/")
    (synopsis "Command-line tool for accessing SPARQL endpoints over HTTP")
    (description "Sparql-query is a command-line tool for accessing SPARQL
endpoints over HTTP.  It has been intentionally designed to @code{feel} similar to
tools for interrogating SQL databases.  For example, you can enter a query over
several lines, using a semi-colon at the end of a line to indicate the end of
your query.  It also supports readline so that you can more easily recall and
edit previous queries, even across sessions.  It can be used non-interactively,
for example from a shell script.")
    ;; Some files (like scan-sparql.c) contain a GPLv3+ license header, while
    ;; others (like sparql-query.c) contain a GPLv2+ license header.
    (license (list license:gpl3+))))

(define-public sqitch
  (package
    (name "sqitch")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DW/DWHEELER/App-Sqitch-v"
             version ".tar.gz"))
       (sha256
        (base32 "1ayiwg9kh3w0nbacbcln7h944z94vq5vnnd5diz86033bpbnq57f"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-check-environment
           (lambda _
             (setenv "TZ" "UTC")
             (setenv "HOME" "/tmp")))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "PERL5LIB")))
               (wrap-program (string-append out "/bin/sqitch")
                 `("PERL5LIB" ":" prefix
                   (,(string-append out "/lib/perl5/site_perl"
                                    ":"
                                    path))))))))))
    (native-inputs
     (list perl-capture-tiny
           perl-io-pager
           perl-module-build
           perl-module-runtime
           perl-path-class
           perl-test-deep
           perl-test-dir
           perl-test-exception
           perl-test-file
           perl-test-file-contents
           perl-test-mockmodule
           perl-test-mockobject
           perl-test-nowarnings
           perl-test-warn))
    (inputs
     (list bash-minimal
           perl-class-xsaccessor
           perl-clone
           perl-config-gitlike
           perl-datetime
           perl-datetime-timezone
           perl-dbd-mysql
           perl-dbd-pg
           perl-dbd-sqlite
           perl-dbi
           perl-devel-stacktrace
           perl-encode-locale
           perl-hash-merge
           perl-ipc-run3
           perl-ipc-system-simple
           perl-libintl-perl
           perl-list-moreutils
           perl-moo
           perl-mysql-config
           perl-namespace-autoclean
           perl-path-class
           perl-perlio-utf8_strict
           perl-string-formatter
           perl-string-shellquote
           perl-sub-exporter
           perl-template-tiny
           perl-template-toolkit
           perl-throwable
           perl-try-tiny
           perl-type-tiny
           perl-type-tiny-xs
           perl-uri
           perl-uri-db))
    (home-page "https://sqitch.org/")
    (synopsis "Database change management tool")
    (description
     "Sqitch is a standalone change management system for database schemas,
which uses SQL to describe changes.")
    (license license:x11)))

(define-public sqlcrush
  ;; Unfortunately, there is no proper upstream release and may never be.
  (let ((commit "502a583e97a84efdeb48e59f1bfe403daa9681ee")
        (revision "2"))
    (package
      (name "sqlcrush")
      (version (git-version "0.1.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/coffeeandscripts/sqlcrush")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0x2q4m9ryw68kifalnm3x4bv9v2xrc2ffsiap8m9wnw6lf1h05la"))))
      (build-system python-build-system)
      (inputs
       (list python-psycopg2 python-pymysql python-sqlalchemy))
      (home-page "https://github.com/coffeeandscripts/sqlcrush")
      (synopsis "Text console-based database viewer and editor")
      (description
       "SQLcrush lets you view and edit a database directly from the text
console through an ncurses interface.  You can explore each table's structure,
browse and edit the contents, add and delete entries, all while tracking your
changes.")
      (license license:gpl3+)))) ; no headers, see README.md

(define-public tdb
  (package
    (name "tdb")
    (version "1.4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/tdb/tdb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03n2hz4sv003gpkyp57hk5kiw4xk9f2dkxq75kzk2gskxy6idyx4"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; The 'configure' script is a wrapper for Waf and
               ;; doesn't recognize things like '--enable-fast-install'.
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
    (native-inputs
     (list docbook-xsl
           libxcrypt
           libxslt
           python ;for the Waf build system
           which))
    (home-page "https://tdb.samba.org/")
    (synopsis "Trivial database")
    (description
     "TDB is a Trivial Database.  In concept, it is very much like GDBM,
and BSD's DB except that it allows multiple simultaneous writers and uses
locking internally to keep writers from trampling on each other.  TDB is also
extremely small.")
    (license license:lgpl3+)))

(define-public perl-dbi
  (package
    (name "perl-dbi")
    (version "1.643")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/T/TI/TIMB/DBI-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1yinx39960y241vf2sknxj0dfz82a5m9gvklq5rw78k0nlyrjawa"))))
    (build-system perl-build-system)
    (synopsis "Database independent interface for Perl")
    (description "This package provides a database interface for Perl.")
    (home-page "https://metacpan.org/release/DBI")
    (license license:perl-license)))

(define-public perl-dbix-class
  (package
    (name "perl-dbix-class")
    (version "0.082843")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RI/RIBASUSHI/"
                           "DBIx-Class-" version ".tar.gz"))
       (sha256
        (base32 "0lgjw7z4y2inf3yyfph1ljdp4f5zdmy9vh56fj8w9n19rdp0n7il"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-dbd-sqlite
           perl-module-install
           perl-package-stash
           perl-test-deep
           perl-test-exception
           perl-test-warn))
    (propagated-inputs
     (list perl-class-accessor-grouped
           perl-class-c3-componentised
           perl-class-inspector
           perl-config-any
           perl-context-preserve
           perl-data-dumper-concise
           perl-data-page
           perl-dbi
           perl-devel-globaldestruction
           perl-hash-merge
           perl-module-find
           perl-moo-2
           perl-mro-compat
           perl-namespace-clean
           perl-path-class
           perl-scope-guard
           perl-sql-abstract-classic
           perl-sub-name
           perl-try-tiny))
    (home-page "https://metacpan.org/release/DBIx-Class")
    (synopsis "Extensible and flexible object <-> relational mapper")
    (description "An SQL to OO mapper with an object API inspired by
Class::DBI (with a compatibility layer as a springboard for porting) and a
resultset API that allows abstract encapsulation of database operations.  It
aims to make representing queries in your code as perl-ish as possible while
still providing access to as many of the capabilities of the database as
possible, including retrieving related records from multiple tables in a
single query, \"JOIN\", \"LEFT JOIN\", \"COUNT\", \"DISTINCT\", \"GROUP BY\",
\"ORDER BY\" and \"HAVING\" support.")
    (properties
     ;; This is needed for perl-catalyst-authentication-store-dbix-class
     `((updater-extra-propagated-inputs . ("perl-data-page"))))
    (license license:perl-license)))

(define-public perl-dbix-class-cursor-cached
  (package
    (name "perl-dbix-class-cursor-cached")
    (version "1.001004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AR/ARCANEZ/"
                           "DBIx-Class-Cursor-Cached-" version ".tar.gz"))
       (sha256
        (base32
         "09b2jahn2x12qm4f7qm1jzsxbz7qn1czp6a3fnl5l2i3l4r5421p"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-cache-cache perl-dbd-sqlite perl-module-install))
    (propagated-inputs
     (list perl-carp-clan perl-dbix-class))
    (home-page "https://metacpan.org/release/DBIx-Class-Cursor-Cached")
    (synopsis "Cursor with built-in caching support")
    (description "DBIx::Class::Cursor::Cached provides a cursor class with
built-in caching support.")
    (license license:perl-license)))

(define-public perl-dbix-class-introspectablem2m
  (package
    (name "perl-dbix-class-introspectablem2m")
    (version "0.001002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "DBIx-Class-IntrospectableM2M-" version ".tar.gz"))
       (sha256
        (base32
         "1w47rh2241iy5x3a9bqsyd5kdp9sk43dksr99frzv4qn4jsazfn6"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install))
    (propagated-inputs
     (list perl-dbix-class))
    (home-page "https://metacpan.org/release/DBIx-Class-IntrospectableM2M")
    (synopsis "Introspect many-to-many relationships")
    (description "Because the many-to-many relationships are not real
relationships, they can not be introspected with DBIx::Class.  Many-to-many
relationships are actually just a collection of convenience methods installed
to bridge two relationships.  This DBIx::Class component can be used to store
all relevant information about these non-relationships so they can later be
introspected and examined.")
    (license license:perl-license)))

(define-public perl-dbix-class-schema-loader
  (package
    (name "perl-dbix-class-schema-loader")
    (version "0.07049")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "DBIx-Class-Schema-Loader-" version ".tar.gz"))
       (sha256
        (base32
         "0r57fv71ypxafb85cpxph1hdqii7ipjwvc19yb6fpkvq2ggcssg8"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-config-any
           perl-config-general
           perl-dbd-sqlite
           perl-dbix-class-introspectablem2m
           perl-module-install
           perl-moose
           perl-moosex-markasmethods
           perl-moosex-nonmoose
           perl-namespace-autoclean
           perl-test-deep
           perl-test-differences
           perl-test-exception
           perl-test-pod
           perl-test-warn))
    (propagated-inputs
     (list perl-class-unload
           perl-class-inspector
           perl-class-accessor-grouped
           perl-class-c3-componentised
           perl-carp-clan
           perl-data-dump
           perl-dbix-class
           perl-hash-merge
           perl-list-moreutils
           perl-lingua-en-inflect-phrase
           perl-lingua-en-inflect-number
           perl-lingua-en-tagger
           perl-namespace-clean
           perl-mro-compat
           perl-scope-guard
           perl-string-camelcase
           perl-string-toidentifier-en
           perl-sub-name
           perl-try-tiny))
    (arguments `(#:tests? #f))          ;TODO: t/20invocations.t fails
    (home-page "https://metacpan.org/release/DBIx-Class-Schema-Loader")
    (synopsis "Create a DBIx::Class::Schema based on a database")
    (description "DBIx::Class::Schema::Loader automates the definition of a
DBIx::Class::Schema by scanning database table definitions and setting up the
columns, primary keys, unique constraints and relationships.")
    (license license:perl-license)))

(define-public perl-dbix-class-deploymenthandler
  (package
    (name "perl-dbix-class-deploymenthandler")
    (version "0.002233")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MM/MMCCLIMON/DBIx-Class-DeploymentHandler-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1bikp1q3pm1xphfdr8kmsqvzzpk06an3112mhb6gl8vlpg81ch4m"))))
    (build-system perl-build-system)
    (native-inputs (list perl-aliased
                         perl-dbd-sqlite
                         perl-dbix-class-schema-loader
                         perl-test-fatal
                         perl-test-most
                         perl-test-requires))
    (propagated-inputs (list perl-carp-clan
                             perl-context-preserve
                             perl-dbix-class
                             perl-log-contextual
                             perl-module-runtime
                             perl-moose
                             perl-moosex-role-parameterized
                             perl-namespace-autoclean
                             perl-path-class
                             perl-sql-splitstatement
                             perl-sql-translator
                             perl-sub-exporter-progressive
                             perl-sub-quote
                             perl-text-brew
                             perl-try-tiny
                             perl-yaml))
    (home-page "https://metacpan.org/release/DBIx-Class-DeploymentHandler")
    (synopsis "Extensible DBIx::Class deployment")
    (description
     "@code{DBIx::Class::DeploymentHandler} is a tool for deploying and
upgrading databases with @code{DBIx::Class}.  It is designed to be much more
flexible than @code{DBIx::Class::Schema::Versioned}, hence the use of Moose
and lots of roles.")
    (license license:perl-license)))

(define-public perl-dbix-simple
  (package
    (name "perl-dbix-simple")
    (version "1.37")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JU/JUERD/DBIx-Simple-" version
                    ".tar.gz"))
              (sha256
               (base32
                "1qyaj01zb6xplzcp7pj0b9609fvd89c9c4an3i00g2g05jm13ls6"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-dbi))
    (home-page "https://metacpan.org/release/DBIx-Simple")
    (synopsis "Object-oriented interface to DBI")
    (description
     "DBIx::Simple provides a simplified interface to DBI, Perl's powerful
database module.  This module is aimed at rapid development and easy
maintenance.  Query preparation and execution are combined in a single method,
the result object (which is a wrapper around the statement handle) provides
easy row-by-row and slurping methods.")
    ;; The POD info says: "Pick your favorite OSI-approved license"
    ;; Use unlicense because it has the least restrictions.
    (license license:unlicense)))

(define-public perl-dbicx-testdatabase
  (package
    (name "perl-dbicx-testdatabase")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JR/JROCKWAY/DBICx-TestDatabase-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1kqaiygxiarrqkgqbq1s3xilx77msbdsqrdaqf4628811d9w4fwf"))))
    (build-system perl-build-system)
    (native-inputs (list perl-dbix-class perl-module-install))
    (propagated-inputs (list perl-dbd-sqlite
                             perl-sql-translator))
    (home-page "https://metacpan.org/release/DBICx-TestDatabase")
    (synopsis "Create a temporary database from a DBIx::Class::Schema")
    (description
     "This module creates a temporary SQLite database, deploys a DBIC schema,
and then connects to it. This lets you easily test DBIC schema. Since you have
a fresh database for every test, you don't have to worry about cleaning up
after your tests, ordering of tests affecting failure, etc.")
    (license license:perl-license)))

(define-public perl-dbix-class-dynamicdefault
  (package
    (name "perl-dbix-class-dynamicdefault")
    (version "0.04")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MS/MSTROUT/DBIx-Class-DynamicDefault-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1yssda00988hn9v96jl95apbzib74vgbcgf69m5mhhj64amm33r2"))))
    (build-system perl-build-system)
    (native-inputs (list perl-dbicx-testdatabase perl-module-install))
    (propagated-inputs (list perl-dbix-class))
    (home-page "https://metacpan.org/release/DBIx-Class-DynamicDefault")
    (synopsis "Automatically set and update fields")
    (description
     "Automatically set and update fields with values calculated at runtime.
Ipdate or create actions will set the specified columns to the value returned
by the callback you specified as a method name or code reference.")
    (license license:perl-license)))

(define-public perl-dbix-class-timestamp
  (package
    (name "perl-dbix-class-timestamp")
    (version "0.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RI/RIBASUSHI/DBIx-Class-TimeStamp-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1zhr4r5ffihqsh2imap7zmps2y0h71piqqhg2nqhcwkjhvlxb958"))))
    (build-system perl-build-system)
    (native-inputs (list perl-class-accessor-grouped
                         perl-datetime-format-mysql
                         perl-datetime-format-sqlite
                         perl-dbd-sqlite
                         perl-module-install
                         perl-test-pod
                         perl-time-warp))
    (propagated-inputs (list perl-datetime
                             perl-dbix-class
                             perl-dbix-class-dynamicdefault))
    (home-page "https://metacpan.org/release/DBIx-Class-TimeStamp")
    (synopsis
     "DBIx::Class extension to update and create date and time based fields")
    (description
     "This package works in conjunction with @code{InflateColumn::DateTime} to
automatically set update and create date and time based fields in a table.")
    (license license:perl-license)))

(define-public perl-dbd-pg
  (package
    (name "perl-dbd-pg")
    (version "3.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TU/TURNSTEP/"
                           "DBD-Pg-" version ".tar.gz"))
       (sha256
        (base32
         "0zn17xb6bmixkmv53p576igzw1jd43cwql35r19m56jwahxm9iqk"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-dbi))
    (propagated-inputs
     (list perl-dbi postgresql))
    (home-page "https://metacpan.org/release/DBD-Pg")
    (synopsis "DBI PostgreSQL interface")
    (description "This package provides a PostgreSQL driver for the Perl5
@dfn{Database Interface} (DBI).")
    (license license:perl-license)))

(define-public perl-dbd-mysql
  (package
    (name "perl-dbd-mysql")
    (version "4.050")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DV/DVEEDEN/"
                           "DBD-mysql-" version ".tar.gz"))
       (sha256
        (base32 "0y4djb048i09dk19av7mzfb3khr72vw11p3ayw2p82jsy4gm8j2g"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'skip-library-detection
           ;; Avoid dependencies on perl-devel-checklib, openssl, and zlib.  They
           ;; are really only needed for the test suite; their absence does not
           ;; affect the build or the end result.
           (lambda _
             (substitute* "Makefile.PL"
               (("use Devel::CheckLib;" match)
                (string-append "# " match))
               (("assert_lib")
                "print"))
             #t)))
       ;; Tests require running MySQL server.
       #:tests? #f))
    (propagated-inputs
     `(("perl-dbi" ,perl-dbi)
       ("mysql" ,mariadb "lib")
       ("mysql-dev" ,mariadb "dev")))
    (home-page "https://metacpan.org/release/DBD-mysql")
    (synopsis "DBI MySQL interface")
    (description "This package provides a MySQL driver for the Perl5
@dfn{Database Interface} (DBI).")
    (license license:perl-license)))

(define-public perl-dbd-sqlite
  (package
    (name "perl-dbd-sqlite")
    (version "1.66")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/I/IS/ISHIGAKI/DBD-SQLite-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1zljln5nh61gj3k22a1fv2vhx5l83waizmarwkh77hk6kzzmvrw9"))))
    (build-system perl-build-system)
    (inputs (list sqlite))
    (propagated-inputs (list perl-dbi))
    (synopsis "SQlite interface for Perl")
    (description "DBD::SQLite is a Perl DBI driver for SQLite, that includes
the entire thing in the distribution.  So in order to get a fast transaction
capable RDBMS working for your Perl project you simply have to install this
module, and nothing else.")
    (license license:perl-license)
    (home-page "https://metacpan.org/release/DBD-SQLite")))

(define-public perl-mysql-config
  (package
    (name "perl-mysql-config")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DA/DARREN/MySQL-Config-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1svn7ccw2gc4cazvc58j84rxhnc9vs01zpird0l8460598j475qr"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/MySQL-Config")
    (synopsis "Parse and utilize MySQL's /etc/my.cnf and ~/.my.cnf files")
    (description
     "@code{MySQL::Config} emulates the @code{load_defaults} function from
libmysqlclient.  It will fill an array with long options, ready to be parsed by
@code{Getopt::Long}.")
    (license license:perl-license)))

(define-public perl-sql-abstract
  (package
    (name "perl-sql-abstract")
    (version "1.87")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "SQL-Abstract-" version ".tar.gz"))
       (sha256
        (base32 "0jhw91b23wc9bkfwcgvka4x5ddxk58m9bcp5ay7a3vx77nla09p9"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install perl-test-deep perl-test-exception
           perl-test-warn))
    (propagated-inputs
     (list perl-hash-merge perl-moo perl-mro-compat perl-text-balanced))
    (home-page "https://metacpan.org/release/SQL-Abstract")
    (synopsis "Generate SQL from Perl data structures")
    (description "This module was inspired by the excellent DBIx::Abstract.
While based on the concepts used by DBIx::Abstract, the concepts used have
been modified to make the SQL easier to generate from Perl data structures.
The underlying idea is for this module to do what you mean, based on the data
structures you provide it, so that you don't have to modify your code every
time your data changes.")
    (license license:perl-license)))

(define-public perl-sql-abstract-classic
  (package
    (name "perl-sql-abstract-classic")
    (version "1.91")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RI/RIBASUSHI/"
                           "SQL-Abstract-Classic-" version ".tar.gz"))
       (sha256
        (base32 "0a7g13hs3kdxrjn43sfli09mgsi9d6w0dfw6hlk268av17yisgaf"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-deep perl-test-exception perl-test-warn))
    (propagated-inputs
     (list perl-mro-compat perl-sql-abstract))
    (home-page "https://metacpan.org/release/SQL-Abstract-Classic")
    (synopsis "Generate SQL from Perl data structures")
    (description
     "This module is nearly identical to @code{SQL::Abstract} 1.81, and exists
to preserve the ability of users to opt into the new way of doing things in
later versions according to their own schedules.

It is an abstract SQL generation module based on the concepts used by
@code{DBIx::Abstract}, with several important differences, especially when it
comes to @code{WHERE} clauses.  These concepts were modified to make the SQL
easier to generate from Perl data structures.

The underlying idea is for this module to do what you mean, based on the data
structures you provide it.  You shouldn't have to modify your code every time
your data changes, as this module figures it out.")
    (license license:perl-license)))

(define-public perl-sql-splitstatement
  (package
    (name "perl-sql-splitstatement")
    (version "1.00023")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/V/VE/VEESH/"
                           "SQL-SplitStatement-" version ".tar.gz"))
       (sha256
        (base32 "0ppkx46nydzlnsxf9a8pkyb74wggfrdiiwafab143lrarlh88x0s"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-differences perl-test-exception perl-test-script))
    (propagated-inputs
     (list perl-class-accessor perl-list-moreutils perl-regexp-common
           perl-sql-tokenizer))
    (home-page "https://metacpan.org/release/SQL-SplitStatement")
    (synopsis "Split SQL code into atomic statements")
    (description "This module tries to split any SQL code, even including
non-standard extensions, into the atomic statements it is composed of.")
    (license license:perl-license)))

(define-public perl-sql-tokenizer
  (package
    (name "perl-sql-tokenizer")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IZ/IZUT/"
                           "SQL-Tokenizer-" version ".tar.gz"))
       (sha256
        (base32
         "1qa2dfbzdlr5qqdam9yn78z5w3al5r8577x06qan8wv58ay6ka7s"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/SQL-Tokenizer")
    (synopsis "SQL tokenizer")
    (description "SQL::Tokenizer is a tokenizer for SQL queries.  It does not
claim to be a parser or query verifier.  It just creates sane tokens from a
valid SQL query.")
    (license license:perl-license)))

(define-public perl-sql-translator
  (package
    (name "perl-sql-translator")
    (version "1.63")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/V/VE/VEESH/SQL-Translator-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0cric118ms3dcnb3m2a1jdhdxmai8wwp5jbx34mf72s9jd6b11aq"))))
    (build-system perl-build-system)
    (native-inputs (list perl-file-sharedir-install
                         perl-json-maybexs
                         perl-test-differences
                         perl-test-exception
                         perl-test-most
                         perl-xml-writer
                         perl-yaml))
    (propagated-inputs (list perl-carp-clan
                             perl-dbi
                             perl-file-sharedir
                             perl-gd
                             perl-graph
                             perl-graphviz
                             perl-moo
                             perl-package-variant
                             perl-parse-recdescent
                             perl-spreadsheet-parseexcel
                             perl-strictures-2
                             perl-sub-quote
                             perl-template-toolkit
                             perl-text-recordparser
                             perl-try-tiny
                             perl-xml-libxml))
    (home-page "https://metacpan.org/release/SQL-Translator")
    (synopsis "Manipulate structured data definitions (SQL and more)")
    (description
     "@code{SQL::Translator} is a group of Perl modules that converts
vendor-specific SQL table definitions into other formats, such as other
vendor-specific SQL, ER diagrams, documentation (POD and HTML), XML, and
@code{Class::DBI} classes. The main focus is SQL, but parsers exist for other
structured data formats, including Excel spreadsheets and arbitrarily
delimited text files. Through the separation of the code into parsers and
producers with an object model in between, it's possible to combine any parser
with any producer, to plug in custom parsers or producers, or to manipulate
the parsed data via the built-in object model. Presently only the definition
parts of SQL are handled (CREATE, ALTER), not the manipulation of
data (INSERT, UPDATE, DELETE).")
    (license license:perl-license)))

(define-public perl-test-postgresql
  (package
    (name "perl-test-postgresql")
    (version "1.29")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/T/TJ/TJC/Test-PostgreSQL-" version
                    ".tar.gz"))
              (sha256
               (base32
                "17jgiqdyprb8q4855anica2kr0lxdi9rnq27vsfclk6ai7jzgb0q"))))
    (build-system perl-build-system)
    (arguments
     (list #:tests? #f)) ;2/41 fail, require running Postgres server
    (native-inputs (list perl-module-build-tiny
                         perl-test-sharedfork))
    (propagated-inputs (list perl-dbd-pg
                             perl-dbi
                             perl-file-which
                             perl-function-parameters
                             perl-moo
                             perl-tie-hash-method
                             perl-try-tiny
                             perl-type-tiny))
    (home-page "https://metacpan.org/release/Test-PostgreSQL")
    (synopsis "PostgreSQL runner for tests")
    (description
     "@code{Test::PostgreSQL} automatically setups a PostgreSQL instance in a
temporary directory, and destroys it when the perl script exits.")
    (license license:artistic2.0)))

(define-public unixodbc
  (package
   (name "unixodbc")
   (version "2.3.9")
   (source (origin
            (method url-fetch)
            (uri
             (string-append
              "ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-"
              version ".tar.gz"))
            (sha256
             (base32 "01xj65d02i3yjy7p9z08y9jakcs5szmz4rask868n7387nn3x0sj"))))
   (build-system gnu-build-system)
   (synopsis "Data source abstraction library")
   (description "Unixodbc is a library providing an API with which to access
data sources.  Data sources include SQL Servers and any software with an ODBC
Driver.")
   (license license:lgpl2.1+)
   ;; COPYING contains copy of lgpl2.1 - but copyright notices just say "LGPL"
   (home-page "https://www.unixodbc.org")))

(define-public nanodbc
  (package
    (name "nanodbc")
    (version "2.14.0")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/nanodbc/nanodbc")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1253bnrmchga3ra99jqkd2p29bc5h2ip79xd8afblz6b1v00wmbm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ;; The tests require ODBC backends to be installed.
       (list "-DNANODBC_DISABLE_TESTS=ON"
             "-DBUILD_SHARED_LIBS=ON")
       #:tests? #false))
    (inputs
     (list unixodbc))
    (home-page "https://nanodbc.io/")
    (synopsis "C++ wrapper for the native C ODBC API")
    (description "The goal for nanodbc is to make developers happy by providing
a simpler and less verbose API for working with ODBC.  Common tasks should be
easy, requiring concise and simple code.")
    (license license:expat)))

(define-public nanodbc-for-irods
  (package
    (inherit nanodbc)
    (arguments
     `(#:tests? #false
       #:configure-flags
       '("-DBUILD_SHARED_LIBS=ON"
         ;; The tests require ODBC backends to be installed.
         "-DNANODBC_DISABLE_TESTS=ON"
         "-DCMAKE_CXX_COMPILER=clang++"
         "-DCMAKE_CXX_FLAGS=-stdlib=libc++"
         "-DCMAKE_EXE_LINKER_FLAGS=-lc++abi")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (cons (string-append (assoc-ref inputs "libcxx")
                                             "/include/c++/v1")
                              ;; Hide GCC's C++ headers so that they do not interfere with
                              ;; the Clang headers.
                              (delete (string-append gcc "/include/c++")
                                      (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                    #\:)))
                        ":"))
               (format #true
                       "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                       (getenv "CPLUS_INCLUDE_PATH"))))))))
    (properties `((hidden? . #true)))
    (inputs
     `(("unixodbc" ,unixodbc)
       ("libcxx" ,libcxx+libcxxabi-6)
       ("libcxxabi" ,libcxxabi-6)
       ("clang" ,clang-6)))))

(define-public unqlite
  (package
    (name "unqlite")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              ;; Contains bug fixes against the official release, and has an
              ;; autotooled build system.
              (uri (string-append "https://github.com/aidin36/tocc/releases/"
                                  "download/v1.0.0/"
                                  "unqlite-unofficial-" version ".tar.gz"))
              (sha256
               (base32
                "1sbpvhg15gadq0mpcy16q7k3rkg4b4dicpnn5xifpkpn02sqik3s"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f))          ;No check target
    (home-page "https://www.unqlite.org")
    (synopsis "In-memory key/value and document store")
    (description
     "UnQLite is an in-process software library which implements a
self-contained, serverless, zero-configuration, transactional NoSQL
database engine.  UnQLite is a document store database similar to
Redis, CouchDB, etc., as well as a standard key/value store
similar to BerkeleyDB, LevelDB, etc.")
    (license license:bsd-2)))

;; NOTE: When upgrading redis, make sure that redis is still released
;; under a Free Software license.
(define-public redis
  (package
    (name "redis")
    (version "7.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.redis.io/releases/redis-"
                                  version".tar.gz"))
              (sha256
               (base32
                "03ixkzdc43lr4q3my0yax1f1gqz6sr6hch5qdipmbd725xxdc47v"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled jemalloc, as the package will use the libc one
               '(begin (delete-file-recursively "deps/jemalloc")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           "MALLOC=libc"
                           "LDFLAGS=-ldl"
                           (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* "runtest"
                (("^TCLSH=.*")
                 (string-append "TCLSH=" (which "tclsh"))))
              (substitute* "tests/support/server.tcl"
                (("/usr/bin/env")
                 (which "env")))))
          (add-after 'unpack 'adjust-tests
            (lambda _
              ;; Disable failing tests.
              (substitute* "tests/test_helper.tcl"
                ;; The AOF tests cause the test suite to hang waiting for a
                ;; "background AOF rewrite to finish", perhaps because dead
                ;; processes persist as zombies in the build environment.
                (("unit/aofrw") "")
                (("integration/aof([^-]|-multi-part)") "")
                ;; The OOM score tests try to raise the current OOM score, but
                ;; our build environment already sets it for all children to
                ;; the highest possible one (1000).  We can't lower it because
                ;; we don't have CAP_SYS_RESOURCE.
                (("unit/oom-score-adj") "")
                (("integration/failover") "")
                (("integration/replication[^-]") "")))))))
    (native-inputs (list pkg-config procps tcl which))
    (synopsis "Key-value cache and store")
    (description "Redis is an advanced key-value cache and store.  Redis
supports many data structures including strings, hashes, lists, sets, sorted
sets, bitmaps and hyperloglogs.")
    (home-page "https://redis.io/")
    ;; These two CVEs have long been fixed.
    (properties `((lint-hidden-cve . ("CVE-2022-3647" "CVE-2022-33105"))))
    (license license:bsd-3)))

(define-public hiredis
  (package
    (name "hiredis")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redis/hiredis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zld30j3kpzqr9w3vkpd6mm3f1b1yk3dlgp9lp6gpsybjjfr2i6h"))))
    (build-system cmake-build-system)
    (native-inputs
     ;; needed for testing
     (list redis))
    (synopsis "Minimalistic C client library for the Redis database")
    (description "This package provides a library for sending commands and
receiving replies to and from a Redis server.  It comes with a synchronous
API, asynchronous API and reply parsing API.  Only the binary-safe Redis
protocol is supported.")
    (home-page "https://github.com/redis/hiredis")
    (license license:bsd-3)))

(define-public ruby-hiredis
  (package
    (name "ruby-hiredis")
    (version "0.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/redis/hiredis-rb")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05y4g7frhym59m9x208zpvg2qvqvfjlgqmygxj8sqgl07n0ww1ks"))
              (patches (search-patches
                        "ruby-hiredis-use-system-hiredis.patch"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f                       ;require native extension
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-hiredis-include-directory
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "ext/hiredis_ext/extconf.rb"
                ;; Adjust the hiredis include directory.
                (("\\$CFLAGS << \" -I/usr/include/hiredis\"")
                 (format #f "$CFLAGS << \" -I~a\""
                         (search-input-directory inputs "include/hiredis"))))))
          (add-after 'unpack 'disable-building-c-extension
            (lambda _
              ;; FIXME: The produced native extension appears to segfault when
              ;; run; disable building it until a solution is found (see:
              ;; https://github.com/redis/hiredis-rb/issues/93).
              (substitute* "ext/hiredis_ext/extconf.rb"
                (("build_hiredis = true")
                 "build_hiredis = false"))))
          ;; FIXME: Un-comment phase after the extension can be made to run
          ;; without crashing (see above).
          ;; (add-after 'build 'build-ext
          ;;   (lambda _
          ;;     (setenv "CC" #$(cc-for-target))
          ;;     (invoke "rake" "compile")))
          (add-before 'check 'start-redis
            (lambda _
              (invoke "redis-server" "--daemonize" "yes")))
          (add-after 'install 'delete-mkmf.log
            (lambda _
              ;; This build log captures non-deterministic file names (see:
              ;; https://github.com/rubygems/rubygems/issues/6259).
              (for-each delete-file (find-files #$output "^mkmf\\.log$")))))))
    (native-inputs (list redis ruby-rake-compiler))
    (inputs (list hiredis))
    (synopsis "Ruby wrapper for hiredis")
    (description "@code{hiredis-rb} is a Ruby extension that wraps
@code{hiredis}, a minimalist C client for Redis.  Both the synchronous
connection API and a separate protocol reader are supported.  It is primarily
intended to speed up parsing multi bulk replies.")
    (home-page "https://github.com/redis/hiredis-rb")
    (license license:bsd-3)))

(define-public ruby-redis
  (package
    (name "ruby-redis")
    (version "4.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "redis" version))
        (sha256
         (base32
          "15x2sr6h094rjbvg8pkq6m3lcd5abpyx93aifvfdz3wv6x55xa48"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Tests require a running redis server.
    (synopsis "Ruby client for Redis' API")
    (description
     "This package provides a Ruby client that tries to match Redis' API
one-to-one, while still providing an idiomatic interface.")
    (home-page "https://github.com/redis/redis-rb")
    (license license:expat)))

(define-public go-github-com-cupcake-rdb
  (package
    (name "go-github-com-cupcake-rdb")
    (version "0.0.0-20161107195141-43ba34106c76")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tent/rdb")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1l4bsn5yj8r875crz1rsk6dlvhv0bd8mgazsch5vl4c19v0fs2ib"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/cupcake/rdb"))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://github.com/tent/rdb")
    (synopsis "Redis RDB parser for Go")
    (description
     "Package rdb implements parsing and encoding of the Redis RDB file format.")
    (license license:expat)))

(define-public kyotocabinet
  (package
    (name "kyotocabinet")
    (version "1.2.79")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://fallabs.com/kyotocabinet/pkg/"
                                  "kyotocabinet-" version ".tar.gz"))
              (sha256
               (base32
                "079ymsahlrijswgwfr2la9yw5h57l752cprhp5dz31iamsj1vyv7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-opt" ;"-march=native". XXX this also turns off -O0.
        (string-append "LDFLAGS=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib"))))
    (inputs (list zlib))
    (home-page "https://fallabs.com/kyotocabinet/")
    (synopsis
     "Kyoto Cabinet is a modern implementation of the DBM database")
    (description
     "Kyoto Cabinet is a standalone file-based database that supports Hash
and B+ Tree data storage models.  It is a fast key-value lightweight
database and supports many programming languages.  It is a NoSQL database.")
    (license license:gpl3+)))

(define-public tokyocabinet
  (package
    (name "tokyocabinet")
    (version "1.4.48")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://fallabs.com/tokyocabinet/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "140zvr0n8kvsl0fbn2qn3f2kh3yynfwnizn4dgbj47m975yg80x0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-pthread" "--enable-off64"
             ,@(if (target-x86?)
                 ;; Enables x86 specific cflags.
                 '("--enable-fastest")
                 '())
        (string-append "LDFLAGS=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib"))))
    (inputs
     (list zlib))
    (home-page "http://fallabs.com/tokyocabinet/")
    (synopsis "Tokyo Cabinet is a modern implementation of the DBM database")
    (description
     "Tokyo Cabinet is a library of routines for managing a database.
The database is a simple data file containing records, each is a pair of a
key and a value.  Every key and value is serial bytes with variable length.
Both binary data and character string can be used as a key and a value.
There is neither concept of data tables nor data types.  Records are
organized in hash table, B+ tree, or fixed-length array.")
    (license license:lgpl2.1+)))

(define-public wiredtiger
  (package
    (name "wiredtiger")
    (version "2.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://source.wiredtiger.com/releases/wiredtiger-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0krwnb2zfbhvjaskwl875qzd3y626s84zcciq2mxr5c5riw3yh6s"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-lz4" "--with-builtins=snappy,zlib")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-test/fops
           (lambda _
             ;; XXX: timed out after 3600 seconds of silence
             (substitute* "Makefile"
               (("test/fops") ""))
             #t)))))
    (inputs
     (list lz4 zlib snappy))
    (home-page "https://source.wiredtiger.com/")
    (synopsis "NoSQL data engine")
    (description
     "WiredTiger is an extensible platform for data management.  It supports
row-oriented storage (where all columns of a row are stored together),
column-oriented storage (where columns are stored in groups, allowing for
more efficient access and storage of column subsets) and log-structured merge
trees (LSM), for sustained throughput under random insert workloads.")
    (license license:gpl3) ; or GPL-2
    ;; configure.ac: WiredTiger requires a 64-bit build.
    (supported-systems (delete "riscv64-linux" %64bit-supported-systems))))

(define-public wiredtiger-3
  (package
    (inherit wiredtiger)
    (name "wiredtiger")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://source.wiredtiger.com/releases/wiredtiger-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "014awypv579ascg4jbx4pndj2wld337m79yyzrzyr7hxrff139jx"))))))

(define-public guile-wiredtiger
  (package
    (name "guile-wiredtiger")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://framagit.org/a-guile-mind/guile-wiredtiger.git")
                    (commit "340ad4bc2ff4dcc6216a2f5c6f9172ca320ac66b")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "15j36bvxxzil7qpwlmh1rffqpva3ynvrcpqhhqbj2c9208ayz595"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f  ;; tests can't be run in parallel, yet.
       #:configure-flags
       (list (string-append "--with-libwiredtiger-prefix="
                            (assoc-ref %build-inputs "wiredtiger")))
       #:make-flags '("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list wiredtiger-3 guile-2.2))
    (propagated-inputs
     (list guile2.2-bytestructures))
    (synopsis "WiredTiger bindings for GNU Guile")
    (description
     "This package provides Guile bindings to the WiredTiger ``NoSQL''
database.")
    (home-page "https://framagit.org/a-guile-mind/guile-wiredtiger")
    (license license:gpl3+)))

(define-public perl-db-file
 (package
  (name "perl-db-file")
  (version "1.858")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://cpan/authors/id/P/PM/PMQS/DB_File-"
                          version ".tar.gz"))
      (sha256
        (base32 "1xm7s2ag15498kp7g8r20gxk22ncz3b3hz4b3srqf7ypif3a5dyf"))))
  (build-system perl-build-system)
  (arguments
   (list
    #:phases
    #~(modify-phases %standard-phases
        (add-before 'configure 'modify-config.in
          (lambda _
            (substitute* "config.in"
              (("/usr/local/BerkeleyDB")
               #$(this-package-input "bdb"))))))))
  (inputs (list bdb))
  (native-inputs (list perl-test-pod))
  (home-page "https://metacpan.org/release/DB_File")
  (synopsis "Perl5 access to Berkeley DB version 1.x")
  (description
    "The DB::File module provides Perl bindings to the Berkeley DB version 1.x.")
  (license license:perl-license)))

(define-public lmdb
  (package
    (name "lmdb")
    (version "0.9.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.openldap.org/openldap/openldap.git")
             (commit (string-append "LMDB_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0airps4cd0d91nbgy7hgvifa801snxwxzwxyr6pdv61plsi7h8l3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       ;; TODO: Make this unconditional on core-updates.
       ,@(if (%current-target-system)
             `(#:make-flags `(,(string-append "CC=" ,(cc-for-target))
                              ,(string-append "AR=" ,(ar-for-target))))
             '())
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "libraries/liblmdb")
             (substitute* "Makefile"
               (("/usr/local") (assoc-ref outputs "out")))
            #t))
         (add-after 'install 'create-pkg-config-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib/pkgconfig"))
               (with-output-to-file (string-append out "/lib/pkgconfig/liblmdb.pc")
                 (lambda _
                   (format #t "prefix=~a~@
                           exec_prefix=~a~@
                           libdir=~a/lib~@
                           includedir=~a/include~@
                           ~@
                           Name: liblmdb~@
                           Version: ~a~@
                           Description: Lightning Memory-Mapped Database library~@
                           Libs: -L${libdir} -llmdb~@
                           Cflags: -I${includedir}~%"
                           out out out out ,version)))
                 #t))))))
    (home-page "https://symas.com/lmdb/")
    (synopsis "Lightning Memory-Mapped Database library")
    (description
     "The @dfn{Lightning Memory-Mapped Database} (LMDB) is a high-performance
transactional database.  Unlike more complex relational databases, LMDB handles
only key-value pairs (stored as arbitrary byte arrays) and relies on the
underlying operating system for caching and locking, keeping the code small and
simple.
The use of ‘zero-copy’ memory-mapped files combines the persistence of classic
disk-based databases with high read performance that scales linearly over
multiple cores.  The size of each database is limited only by the size of the
virtual address space — not physical RAM.")
    (license license:openldap2.8)))

(define-public lmdbxx
  (package
    (name "lmdbxx")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hoytech/lmdbxx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12k5rz74d1l0skcks9apry1svkl96g9lf5dcgylgjmh7v1jm0b7c"))))
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (build-system gnu-build-system)
    (inputs (list lmdb))
    (home-page "https://github.com/hoytech/lmdbxx")
    (synopsis "C++11 wrapper for the LMDB embedded B+ tree database library")
    (description "@code{lmdbxx} is a comprehensive @code{C++} wrapper for the
@code{LMDB} embedded database library, offering both an error-checked
procedural interface and an object-oriented resource interface with RAII
semantics.")
    (license license:unlicense)))

(define-public libpqxx
  (package
    (name "libpqxx")
    (version "7.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jtv/libpqxx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qwpfba8g55jjv0xnsk4hhf2cmhk7mdirxx115cvnjjw97ppy0z0"))))
    (build-system gnu-build-system)
    (native-inputs (list gcc-11 python-wrapper))
    (inputs (list postgresql))
    (arguments '(#:tests? #f))      ;tests require a running PostgreSQL server
    (synopsis "C++ connector for PostgreSQL")
    (description
     "Libpqxx is a C++ library to enable user programs to communicate with the
PostgreSQL database back-end.  The database back-end can be local or it may be
on another machine, accessed via TCP/IP.")
    (home-page "https://pqxx.org/")
    (license license:bsd-3)))

(define-public python-peewee
  (package
    (name "python-peewee")
    (version "3.17.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "peewee" version))
       (sha256
        (base32 "1a67kdmyd0y3xzhqj5r90wlpqfg703cqx36awg429qsq2w0cs5gy"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                    ; fails to import test data
    (inputs
     (list sqlite))
    (native-inputs
     (list python-cython
           python-setuptools
           python-wheel))
    (home-page "https://github.com/coleifer/peewee/")
    (synopsis "Small object-relational mapping utility")
    (description
     "Peewee is a simple and small ORM (object-relation mapping) tool.  Peewee
handles converting between pythonic values and those used by databases, so you
can use Python types in your code without having to worry.  It has built-in
support for sqlite, mysql and postgresql.  If you already have a database, you
can autogenerate peewee models using @code{pwiz}, a model generator.")
    (license license:expat)))

(define-public python-pypika-tortoise
  (package
    (name "python-pypika-tortoise")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pypika-tortoise" version))
       (sha256
        (base32 "0dmzpsnlqjjz0vm0r9xjk69xfsm235bpnk3jccr8ww4s8y7qc0nq"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list poetry python-setuptools))
    (home-page "https://github.com/tortoise/pypika-tortoise")
    (synopsis "Pypika fork for tortoise-orm")
    (description "Pypika-tortoise is a fork of pypika which has been
streamlined for its use in the context of tortoise-orm.  It removes support
for many database kinds that tortoise-orm doesn't need, for example.")
    (license license:asl2.0)))

(define-public python-sphinxcontrib-asyncio
  (package
    (name "python-sphinxcontrib-asyncio")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sphinxcontrib-asyncio" version))
        (sha256
          (base32 "0bkj010ygsr7m769llf2aq4bbjfhdwqrrabi98j8gpvyzvh2dzcr"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;no test suite
    (propagated-inputs (list python-sphinx))
    (home-page "https://github.com/aio-libs/sphinxcontrib-asyncio")
    (synopsis "Sphinx extension to support coroutines in markup")
    (description "This package is a Sphinx extension providing additional
coroutine-specific markup.")
    (license license:asl2.0)))

(define-public python-asyncpg
  (package
    (name "python-asyncpg")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asyncpg" version))
       (sha256
        (base32 "0h1573lp4607nppflnnjrhn7yrfy6i54cm98gi4qbcikjykfdy33"))))
    (build-system python-build-system)
    (propagated-inputs (list python-typing-extensions))
    (native-inputs
     (list postgresql
           python-cython
           python-pytest
           python-uvloop))
    (home-page "https://github.com/MagicStack/asyncpg")
    (synopsis "Fast PostgreSQL database client library for Python")
    (description "@code{asyncpg} is a database interface library designed
specifically for PostgreSQL and Python/asyncio.  @code{asyncpg} is an
efficient, clean implementation of PostgreSQL server binary protocol for use
with Python's asyncio framework.")
    (license license:asl2.0)))

(define-public python-asyncmy
  (package
    (name "python-asyncmy")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "asyncmy" version))
        (sha256
          (base32 "0i18zxy6xvzv6dk791xifn2sw2q4zvqwpzrzy8qx51d3mp8z6gng"))))
    (build-system python-build-system)
    (native-inputs (list python-cython))
    (home-page "https://github.com/long2ice/asyncmy")
    (synopsis "Fast MySQL driver for Python")
    (description "@code{asyncmy} is a fast @code{asyncio} MySQL driver, which
reuses most of @code{pymysql} and @code{aiomysql} but rewrites the core
protocol with Cython for performance.")
    (license license:asl2.0)))

(define-public python-aiomysql
  (package
    (name "python-aiomysql")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "aiomysql" version))
        (sha256
          (base32 "0zhhisf2bz2hv0vcklw45k54yr0f7gx61gnq4lc7vdp6v97nqs0d"))))
    (build-system pyproject-build-system)
    (arguments '(#:tests? #f))           ;test suite requires docker
    (propagated-inputs (list python-pymysql))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/aio-libs/aiomysql")
    (synopsis "MySQL driver for Python")
    (description "@code{aiomysql} is a driver for accessing a MySQL database
from the @code{asyncio} Python framework.  It depends on and reuses most parts
of PyMySQL.  @code{aiomysql} tries to preserve the same API as the
@code{aiopg} library.")
    (license license:expat)))

(define-public python-tortoise-orm
  (package
    (name "python-tortoise-orm")
    (version "0.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tortoise/tortoise-orm")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19rgyvs2y9gn27x71y7djdz6rb6bszgvprv55q1hr4266wy6g999"))))
    (build-system pyproject-build-system)
    ;; The test suite relies on asynctest, which is abandoned and doesn't
    ;; support Python >= 3.8.
    (arguments '(#:tests? #f))
    (native-inputs
     (list poetry))
    (propagated-inputs
     (list python-aiomysql
           python-aiosqlite-0.17
           python-asyncmy
           python-asyncpg
           python-ciso8601
           python-iso8601
           python-pypika-tortoise
           python-pytz
           python-rapidjson
           python-uvloop))
    (home-page "https://github.com/tortoise/tortoise-orm")
    (synopsis "Asynchronous Object Relational Mapper (ORM) for Python")
    (description "Tortoise ORM is an easy-to-use asyncio ORM (Object
Relational Mapper) inspired by Django.  Tortoise ORM was built with relations
in mind and admiration for the excellent and popular Django ORM.  It's
engraved in its design that you are working not with just tables, you work
with relational data.")
    (license license:asl2.0)))

(define-public aerich
  (package
    (name "aerich")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tortoise/aerich")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pcy945bg890p12s7cyw0mg7hxwsxyy570j600sbf7kwj2d3lilg"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list poetry
           python-bandit
           python-cryptography
           python-isort
           python-pydantic
           python-pytest
           python-pytest-asyncio
           python-pytest-mock
           python-pytest-xdist))
    (propagated-inputs
     (list python-asyncmy
           python-asyncpg
           python-click
           python-ddlparse
           python-dictdiffer
           python-tomlkit
           python-tortoise-orm))
    (home-page "https://github.com/tortoise/aerich")
    (synopsis "Database migrations tool for Tortoise @acronym{ORM, Object Relational
Mapper}")
    (description
     "This package provides @code{aerich}, a Python database migrations tool
for Tortoise @acronym{ORM, Object Relational Mapper}.  It can be used both
programmatically or as a standalone CLI application.")
    (license license:asl2.0)))

(define-public sqlcipher
  (package
    (name "sqlcipher")
    (version "3.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sqlcipher/sqlcipher")
             (commit (string-append "v" version))))
       (sha256
        (base32 "168wb6fvyap7y8j86fb3xl5rd4wmhiq0dxvx9wxwi5kwm1j4vn1a"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     `(("libcrypto" ,openssl)
       ("libtcl8.6" ,tcl))) ; required for running the tests
    (native-inputs
     (list tcl))
    (arguments
     '(#:configure-flags
       '("--enable-tempstore=yes"
         "CFLAGS=-DSQLITE_HAS_CODEC -DSQLITE_ENABLE_FTS3"
         "LDFLAGS=-lcrypto -ltcl8.6"
         "--disable-tcl")
       ;; tests cannot be run from the Makefile
       ;; see: <https://github.com/sqlcipher/sqlcipher/issues/172>
       #:test-target "testfixture"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'build-test-runner
           (assoc-ref %standard-phases 'check))
         (replace 'check
           (lambda _
             (invoke "./testfixture" "test/crypto.test"))))))
    (home-page "https://www.zetetic.net/sqlcipher/")
    (synopsis
     "Library providing transparent encryption of SQLite database files")
    (description "SQLCipher is an implementation of SQLite, extended to
provide transparent 256-bit AES encryption of database files.  Pages are
encrypted before being written to disk and are decrypted when read back.  It’s
well suited for protecting embedded application databases and for mobile
development.")
    ;; The source files
    ;; src/{crypto.c,crypto_impl.c,crypto.h,crypto_cc.c,crypto_libtomcrypt.c},
    ;; src/{crypto_openssl.c,sqlcipher.h}, tool/crypto-speedtest.tcl,
    ;; test/crypto.test are licensed under a 3-clause BSD license. All other
    ;; source files are in the public domain.
    (license (list license:public-domain license:bsd-3))))

(define-public python-pyodbc
  (package
    (name "python-pyodbc")
    (version "4.0.35")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyodbc" version))
       (sha256
        (base32 "1j7577acd2f16zifw49ajg0aw7vm0pdg6jxrr1dlaa5rx14azfcj"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete precompiled binaries.  The corresponding source is included.
        #~(for-each delete-file (find-files "." "\\.pyc$")))))
    (build-system python-build-system)
    (inputs
     (list unixodbc))
    (arguments
     ;; XXX Tests fail with ‘Can't open lib 'SQL Server Native Client 10.0' :
     ;; file not found (0) (SQLDriverConnect)")’.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "python3" "tests3/test.py")))))))
    (home-page "https://github.com/mkleehammer/pyodbc")
    (synopsis "Python ODBC Library")
    (description "@code{python-pyodbc} provides a Python DB-API driver
for ODBC.")
    (license (license:x11-style "file:///LICENSE.TXT"))))

(define-public mdbtools
  (package
    (name "mdbtools")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brianb/mdbtools")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0gwcpp9y09xhs21g7my2fs8ncb8i6ahlyixcx8jd3q97jbzj441l"))))
    (build-system gnu-build-system)
    (inputs
     (list glib))
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           txt2man
           which))
    (home-page "https://mdbtools.sourceforge.net/")
    (synopsis "Read Microsoft Access databases")
    (description "MDB Tools is a set of tools and applications to read the
proprietary MDB file format used in Microsoft's Access database package.  This
includes programs to export schema and data from Microsoft's Access database
file format to other databases such as MySQL, Oracle, Sybase, PostgreSQL,
etc., and an SQL engine for performing simple SQL queries.")
    (license (list license:lgpl2.0
                   license:gpl2+))))

(define-public python-lmdb
  (package
    (name "python-lmdb")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lmdb" version))
              (sha256
               (base32
                "1di1gj2agbxwqqwrpk4w58dpfah0kl10ha20s63dlqdd1bgzydj1"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled lmdb source files.
               '(begin
                  (for-each delete-file (list "lib/lmdb.h"
                                              "lib/mdb.c"
                                              "lib/midl.c"
                                              "lib/midl.h"))
                  #t))))
    (build-system python-build-system)
    (inputs
     (list lmdb))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-system-lmdb
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lmdb (assoc-ref inputs "lmdb")))
               (setenv "LMDB_PURE" "set") ; don't apply env-copy-txn.patch
               (setenv "LMDB_FORCE_SYSTEM" "set")
               (setenv "LMDB_INCLUDEDIR" (string-append lmdb "/include"))
               (setenv "LMDB_LIBDIR" (string-append lmdb "/lib"))
               #t))))
       ;; Tests fail with: ‘lmdb.tool: Please specify environment (--env)’.
       #:tests? #f))
    (home-page "https://github.com/dw/py-lmdb")
    (synopsis "Python binding for the ‘Lightning’ database (LMDB)")
    (description
     "python-lmdb or py-lmdb is a Python binding for the @dfn{Lightning
Memory-Mapped Database} (LMDB), a high-performance key-value store.")
    (license
     (list license:openldap2.8
           ;; ‘lib/win32/inttypes.h’ and ‘lib/win32-stdint/stdint.h’ are BSD-3,
           ;; but not actually needed on platforms currently supported by Guix.
           license:bsd-3))))

(define-public virtuoso-ose
  (package
    (name "virtuoso-ose")
    (version "7.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/virtuoso/virtuoso/" version "/"
                           "virtuoso-opensource-" version ".tar.gz"))
       (sha256
        (base32 "0mk25gr1pafmps4nsydjprwswbzwch8b583nlwh7x2031sz7ald1"))
       (patches (search-patches "virtuoso-ose-remove-pre-built-jar-files.patch"))
       (modules '((guix build utils)))
       ;; This snippet removes pre-built Java archives.
       (snippet
        #~(for-each delete-file-recursively
                    (list "binsrc/hibernate"
                          "binsrc/jena"
                          "binsrc/jena2"
                          "binsrc/jena3"
                          "binsrc/jena4"
                          "binsrc/rdf4j"
                          "binsrc/sesame"
                          "binsrc/sesame2"
                          "binsrc/sesame3"
                          "binsrc/sesame4"
                          "libsrc/JDBCDriverType4")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; tests require a network connection
      ;; TODO: Removing the libsrc/zlib source directory breaks the build.
      ;; This indicates that the internal zlib code may still be used.
      #:configure-flags
      #~(list "--without-internal-zlib"
              "--with-readline"
              "--enable-static=no")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'bootstrap
            (lambda _
              (invoke "sh" "autogen.sh")))
          (add-after 'unpack 'avoid-embedding-kernel-and-timestamps
            ;; For a reproducible build, avoid embedding the kernel version and
            ;; timestamps.
            (lambda _
              (substitute*
                  (list "bin/makever"
                        "appsrc/ODS-Polls/make_vad.sh"
                        "appsrc/ODS-Blog/make_vad.sh"
                        "appsrc/ODS-Community/make_vad.sh"
                        "appsrc/ODS-Framework/make_vad.sh"
                        "appsrc/ODS-Framework/oauth/make_vad.sh"
                        "appsrc/ODS-WebMail/make_vad.sh"
                        "appsrc/ODS-Calendar/make_vad.sh"
                        "appsrc/ODS-Gallery/make_vad.sh"
                        "appsrc/ODS-Briefcase/make_vad.sh"
                        "appsrc/ODS-FeedManager/make_vad.sh"
                        "appsrc/ODS-Bookmark/make_vad.sh"
                        "appsrc/ODS-Addressbook/make_vad.sh"
                        "binsrc/dbpedia/make_vad.sh"
                        "binsrc/samples/demo/make_vad.sh"
                        "binsrc/samples/demo/mkdoc.sh"
                        "binsrc/samples/sparql_demo/make_vad.sh"
                        "binsrc/bpel/make_vad.sh"
                        "binsrc/fct/make_vad.sh"
                        "binsrc/rdf_mappers/make_vad.sh"
                        "binsrc/isparql/make_vad.sh"
                        "binsrc/conductor/mkvad.sh")
                (("^UNAME_SYSTEM=.*") "UNAME_SYSTEM=unknown\n")
                (("^UNAME_RELEASE=.*") "UNAME_RELEASE=unknown\n")
                (("^PACKDATE=.*") "PACKDATE=2012-04-18\n")
                (("^DATE=.*") "DATE=2012-04-18\n"))))
          ;; Even with "--enable-static=no", "libvirtuoso-t.a" is left in
          ;; the build output.  The following phase removes it.
          (add-after 'install 'remove-static-libs
            (lambda _
              (for-each
               (lambda (file)
                 (delete-file (string-append #$output "/lib/" file)))
               '("libvirtuoso-t.a"
                 "libvirtuoso-t.la")))))))
    (native-inputs
     (list autoconf automake bison flex gperf libtool))
    (inputs
     (list openssl net-tools readline which zlib))
    (home-page "https://vos.openlinksw.com/owiki/wiki/VOS/")
    (synopsis "Multi-model database system")
    (description "Virtuoso is a scalable cross-platform server that combines
relational, graph, and document data management with web application server
and web services platform functionality.")
    ;; configure: error: ... can only be build on 64bit platforms
    (supported-systems %64bit-supported-systems)
    (license license:gpl2)))

(define-public python-ccm
  (package
    (name "python-ccm")
    (version "2.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ccm" version))
       (sha256
        (base32
         "177dfxsmk3k4cih6fh6v8d91bh4nqx7ns6pc07w7m7i3cvdx3c8n"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pyyaml
           ;; Not listed in setup.py, but used in ccmlib/node.py for full
           ;; functionality
           python-psutil python-six))
    (home-page "https://github.com/pcmanus/ccm")
    (synopsis "Cassandra Cluster Manager for Apache Cassandra clusters on
localhost")
    (description "Cassandra Cluster Manager is a development tool for testing
local Cassandra clusters. It creates, launches and removes Cassandra clusters
on localhost.")
    (license license:asl2.0)))

(define-public python-sqlalchemy
  (package
    (name "python-sqlalchemy")
    (version "1.4.42")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "SQLAlchemy" version))
      (sha256
       (base32 "0qzkxy47y06fqh1m7a0p7q2r9h48x9k5kl3znzhx2vj79j8l2zhp"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-cython ; for C extensions
           python-pytest python-mock python-pytest-xdist ; for tests
           python-setuptools python-wheel))
    (propagated-inputs
     (list python-greenlet))
    (arguments
     (list
      #:test-flags
      '(list ;; The memory usage tests are very expensive and run in sequence;
             ;; skip them.
             "-k" "not test_memusage.py")))
    (home-page "https://www.sqlalchemy.org")
    (synopsis "Database abstraction library")
    (description
     "SQLAlchemy is the Python SQL toolkit and Object Relational Mapper that
gives application developers the full power and flexibility of SQL.  It
provides a full suite of well known enterprise-level persistence patterns,
designed for efficient and high-performing database access, adapted into a
simple and Pythonic domain language.")
    (license license:x11)))

(define-public python-sqlalchemy-2
  (package
    (name "python-sqlalchemy")
    (version "2.0.36")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sqlalchemy" version))
       (sha256
        (base32 "1i99kgm8hc38iffgp3wdbixpdc5jfjklabh8lyp9h8vd1dl6f9vz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Remove expensive tests.
      #~(list "--ignore=test/aaa_profiling/test_memusage.py"
              "--ignore=test/ext/mypy/test_mypy_plugin_py3k.py"
              "--ignore=test/typing/test_mypy.py"
              ;; Disable a test that fails randomly.
              "-k" "test_recycle_pool_no_race")))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-greenlet
           python-typing-extensions))
    (home-page "https://www.sqlalchemy.org")
    (synopsis "SQL toolkit and object relational mapper")
    (description
     "SQLAlchemy is the Python SQL toolkit and @acronym{ORM, Object Relational
Mapper} that gives application developers the full power and flexibility of
SQL.  It provides a full suite of well known enterprise-level persistence
patterns, designed for efficient and high-performing database access, adapted
into a simple and Pythonic domain language.")
    (license license:expat)))

(define-public python-sqlalchemy-stubs
  (package
    (name "python-sqlalchemy-stubs")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sqlalchemy-stubs" version))
       (sha256
        (base32
         "1bppjmv7v7m0q8gwg791pgxbx4ay7mna0zq204pn9vw28kfxcrf6"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-mypy python-typing-extensions))
    (home-page "https://github.com/dropbox/sqlalchemy-stubs")
    (synopsis "SQLAlchemy stubs and mypy plugin")
    (description "This package contains type stubs and a mypy plugin to
provide more precise static types and type inference for SQLAlchemy
framework.")
    (license license:asl2.0)))

(define-public python-sqlalchemy-utils
  (package
    (name "python-sqlalchemy-utils")
    (version "0.38.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "SQLAlchemy-Utils" version))
        (sha256
         (base32 "0k8z0mjhvdv302kn0nhci8b2dgw4cn2akprsf37ma1540ykgp6lz"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: Many tests require a running database server.
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       (zero? (system* "py.test" "sqlalchemy_utils" "tests")))))
    (propagated-inputs
     (list python-six python-sqlalchemy))
    (native-inputs
     (list python-dateutil python-flexmock python-psycopg2 python-pytest
           python-pytz))
    (home-page "https://github.com/kvesteri/sqlalchemy-utils")
    (synopsis "Various utility functions for SQLAlchemy")
    (description
     "SQLAlchemy-utils provides various utility functions and custom data types
for SQLAlchemy.  SQLAlchemy is an SQL database abstraction library for Python.

You might also want to install the following optional dependencies:
@enumerate
@item @code{python-passlib}
@item @code{python-babel}
@item @code{python-cryptography}
@item @code{python-pytz}
@item @code{python-psycopg2}
@item @code{python-furl}
@item @code{python-flask-babel}
@end enumerate
")
    (license license:bsd-3)))

(define-public python-alchemy-mock
  (package
    (name "python-alchemy-mock")
    (version "0.4.3")
    (home-page "https://github.com/miki725/alchemy-mock")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "alchemy-mock" version))
              (sha256
               (base32
                "0ylxygl3bcdapzz529n8wgk7vx9gjwb3ism564ypkpd7dbsw653r"))
             (snippet
              #~(begin (use-modules (guix build utils))
                       (substitute* "alchemy_mock/comparison.py"
                         (("collections\\.Mapping") "collections.abc.Mapping"))))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      ;; Create pytest.ini that adds doctest options to
                      ;; prevent test failure.  Taken from tox.ini.
                      (call-with-output-file "pytest.ini"
                        (lambda (port)
                          (format port "[pytest]
doctest_optionflags=IGNORE_EXCEPTION_DETAIL
")))
                      (invoke "pytest" "-vv" "--doctest-modules"
                              "alchemy_mock/"))))))
    (native-inputs
     (list python-mock python-pytest))
    (propagated-inputs
     (list python-six python-sqlalchemy))
    (synopsis "Mock helpers for SQLAlchemy")
    (description
     "This package provides mock helpers for SQLAlchemy that makes it easy
to mock an SQLAlchemy session while preserving the ability to do asserts.

Normally Normally SQLAlchemy's expressions cannot be easily compared as
comparison on binary expression produces yet another binary expression, but
this library provides functions to facilitate such comparisons.")
    (license license:expat)))

(define-public python-alembic
  (package
    (name "python-alembic")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "alembic" version))
       (sha256
        (base32 "0jrh9q4h2jv2bafpd6isx2dvc90rpx6j7fpdvfwd0hin7fsr425h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--ignore=tests/integration"
              "-k" (string-join
                    ;; XXX: Tests require fresh python-pytz timezones, remove
                    ;; when updated.
                    (list "not test_custom_tz"
                          "test_custom_tz_lowercase"
                          "test_custom_tz_utc"
                          "test_custom_tzdata_tz")
                    " and not "))))
    (native-inputs
     (list python-mock
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-dateutil
           python-editor
           python-mako
           python-sqlalchemy-2
           python-typing-extensions))
    (home-page "https://bitbucket.org/zzzeek/alembic")
    (synopsis "Database migration tool for SQLAlchemy")
    (description
     "Alembic is a lightweight database migration tool for usage with the
SQLAlchemy Database Toolkit for Python.")
    (license license:expat)))

(define-public python-sqlite-fts4
  (package
    (name "python-sqlite-fts4")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sqlite-fts4" version))
              (sha256
               (base32
                "034kx0ac556sywy1p4qcrc36l24w3q0xwswqv2z9s3k8yvm5xc3q"))))
    (build-system python-build-system)
    (native-inputs (list python-pytest))
    (home-page "https://github.com/simonw/sqlite-fts4")
    (synopsis "Python functions for working with SQLite FTS4 search")
    (description "This package provides custom SQLite functions written
in Python for ranking documents indexed using the SQLite's FTS4 full
text search extension.")
    (license license:asl2.0)))

(define-public python-sqlite-utils
  (package
    (name "python-sqlite-utils")
    (version "3.38")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/simonw/sqlite-utils")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rai8rbw5drxppypyl3r8j9gr12278h3snkj7l280zkskbxxh7hf"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'relax-requirements
                          (lambda _
                            (substitute* "setup.py"
                              ;; This is a variant designed to have a binary
                              ;; wheel made available on PyPI, which is not a
                              ;; concern to Guix.
                              (("click-default-group-wheel")
                               "click-default-group")))))))
    (propagated-inputs (list python-click python-click-default-group
                             python-dateutil python-sqlite-fts4
                             python-tabulate))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/simonw/sqlite-utils")
    (synopsis
     "CLI tool and Python utility functions for manipulating SQLite databases")
    (description
     "This package provides a CLI tool and Python utility functions for
manipulating SQLite databases.  It's main features are:
@itemize
@item
Pipe JSON (or CSV or TSV) directly into a new SQLite database file,
automatically creating a table with the appropriate schema.
@item
Run in-memory SQL queries, including joins, directly against data in
CSV, TSV or JSON files and view the results.
@item
Configure SQLite full-text search against your database tables and run
search queries against them, ordered by relevance.
@item
Run transformations against your tables to make schema changes that
SQLite ALTER TABLE does not directly support, such as changing the type
of a column.
@item
Extract columns into separate tables to better normalize your existing
data.
@end itemize")
    (license license:asl2.0)))

(define-public python-pickleshare
  (package
    (name "python-pickleshare")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pickleshare" version))
       (sha256
        (base32 "1jmghg3c53yp1i8cm6pcrm280ayi8621rwyav9fac7awjr3kss47"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/vivainio/pickleshare")
    (synopsis "Tiny key value database with concurrency support")
    (description
     "PickleShare is a small ‘shelve’-like datastore with concurrency support.
Like shelve, a PickleShareDB object acts like a normal dictionary.  Unlike
shelve, many processes can access the database simultaneously.  Changing a
value in database is immediately visible to other processes accessing the same
database.  Concurrency is possible because the values are stored in separate
files.  Hence the “database” is a directory where all files are governed by
PickleShare.")
    (license license:expat)))

(define-public python-apsw
  (package
    (name "python-apsw")
    (version "3.46.0.0")
    ;; The compressed release has fetching functionality disabled.
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/rogerbinns/apsw/releases/download/"
             version "/apsw-" version ".zip"))
       (sha256
        (base32
         "10yfbasi4mq63g0svyl1h49ylwn9znjylq78id16dzxzk9q9ipdx"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list unzip python-setuptools python-wheel))
    (inputs (list sqlite-next))         ;SQLite 3.45.1 required.
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build" "--enable-all-extensions"
                      "--enable=load_extension")))
          (add-after 'build 'build-test-helper
            (lambda _
              (invoke "gcc" "-fPIC" "-shared" "-o" "./testextension.sqlext"
                      "-I." "-Isqlite3" "src/testextension.c"))))))
    (home-page "https://github.com/rogerbinns/apsw/")
    (synopsis "Another Python SQLite Wrapper")
    (description
     "APSW is a Python wrapper for the SQLite embedded relational database
engine.  In contrast to other wrappers such as pysqlite it focuses on being a
minimal layer over SQLite attempting just to translate the complete SQLite API
into Python.")
    (license license:zlib)))

(define-public python-aiosqlite
  (package
    (name "python-aiosqlite")
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/omnilib/aiosqlite")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a8sggh1wwbpl46k5qcfmp97s9hjysna0x7mwwc53kyfm0m95wf8"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "python" "-m" "coverage" "run" "-m"
                                      "aiosqlite.tests")
                              (invoke "python" "-m" "coverage" "report")))))))
    (native-inputs (list python-flit-core
                         python-coverage
                         python-mypy))
    (home-page "https://github.com/jreese/aiosqlite")
    (synopsis
     "Asyncio bridge for sqlite3")
    (description
     "The package aiosqlite replicates the standard sqlite3 module, but with
async versions of all the standard connection and cursor methods, and context
managers for automatically closing connections.")
    (license license:expat)))

(define-public python-aiosqlite-0.17
  (package
    (inherit python-aiosqlite)
    (version "0.17.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/omnilib/aiosqlite")
                    (commit (string-append "v" version))))
              (file-name (git-file-name (package-name python-aiosqlite)
                                        version))
              (sha256
               (base32
                "1agh7b9g7rgryvb8flph85i8m80ai1rinpljxzlsrs0s0y616qgg"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "python" "-m" "unittest" "aiosqlite.tests")
                          (format #t "test suite not run~%")))))))
    (propagated-inputs
     (list python-typing-extensions))
    (native-inputs
     (list python-flit-core python-aiounittest))))

(define-public python-databases
  (package
    (name "python-databases")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "databases" version))
       (sha256
        (base32 "0x5nqhzgjqimv2ybjbzy5vv0l23g0n1g5f6fnyahbf1f7nfl2bga"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-aiosqlite
           python-aiopg
           python-aiomysql
           python-asyncpg
           python-asyncmy
           python-sqlalchemy))
    (home-page "https://github.com/encode/databases")
    (synopsis "Asynchronous database abstraction library")
    (description "Databases provides a wrapper around asynchronous database
libraries with SQLALchemy.")
    (license license:bsd-3)))

(define-public python-psycopg2
  (package
    (name "python-psycopg2")
    (version "2.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "psycopg2" version))
       (sha256
        (base32 "04chl9f7v7k1zssa40pmk06jvpyqiss2lpjq50dq69nqix0mhlgi"))))
    (build-system python-build-system)
    (arguments
     ;; Tests would require a postgresql database "psycopg2_test"
     ;; and a running postgresql database management service.
     '(#:tests? #f)) ; TODO re-enable after providing a test-db.
    (inputs
     (list postgresql)) ; libpq
    (home-page "https://www.psycopg.org/")
    (synopsis "Python PostgreSQL adapter")
    (description
     "psycopg2 is a thread-safe PostgreSQL adapter that implements DB-API
2.0.")
    (license license:lgpl3+)))

(define-public python-psycopg-pool
  (package
    (name "python-psycopg-pool")
    ;; The connection pooling code is on a different release cadence
    ;; from the driver code, so fetch the latest PyPI release.
    (version "3.1.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "psycopg-pool" version))
              (sha256
               (base32
                "1pkx7nj1mhacwpna7ldzqfqxd1xg8826600r0bs9ad1h93f429yh"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f                  ;run for psycopg below
           #:phases
           #~(modify-phases %standard-phases
               ;; This module requires 'psycopg', however psycopg needs this
               ;; for its tests.  Disable sanity check to break the cycle.
               (delete 'sanity-check))))
    (home-page "https://www.psycopg.org/")
    (synopsis "Connection pooler for psycopg")
    (description
     "This module provides connection pool implementations that can be used
with the @code{psycopg} PostgreSQL driver.")
    (license license:lgpl3+)))

(define-public python-psycopg
  (package
    (name "python-psycopg")
    (version "3.2.4")
    (source (origin
              ;; Fetch from git because PyPI contains only cythonized sources.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/psycopg/psycopg")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fb83fhaa3saapqv37901hlv017bj40q0dmkxd79aaq442sjf9w2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-o" "asyncio_mode=auto"
             ;; FIXME: Many of the typing tests are failing,
             ;; conveniently tagged as slow...
             "-k" "not slow" "..")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'change-directory
            (lambda _ (chdir "psycopg")))
          (add-after 'build 'build-c-extensions
            (lambda _
              (with-directory-excursion "../psycopg_c"
                ((assoc-ref %standard-phases 'build)))))
          (add-after 'install 'install-c-extensions
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (with-directory-excursion "../psycopg_c"
                ((assoc-ref %standard-phases 'install)
                 #:inputs inputs #:outputs outputs))))
          (add-before 'check 'start-postgresql
            (lambda* (#:key inputs tests? #:allow-other-keys)
              (when tests?
                (setenv "TZDIR" (search-input-directory inputs
                                                        "share/zoneinfo"))
                (let ((dbdir (string-append (getcwd) "/../pgdir")))
                  (invoke "initdb" "-D" dbdir)
                  (invoke "pg_ctl" "-D" dbdir
                          "-o" (string-append "-k " dbdir)
                          "-l" (string-append dbdir "/db.log")
                          "start")
	          (invoke "psql" "-h" dbdir "-d" "postgres"
                          "-c" "CREATE DATABASE nixbld;"))))))))
    (native-inputs
     (list python-cython-3
           python-mypy
           python-psycopg-pool
           python-pytest
           python-pytest-asyncio
           python-anyio
           python-setuptools
           python-tenacity
           python-wheel
           pproxy
           tzdata-for-tests))
    (inputs
     (list postgresql))
    (home-page "https://www.psycopg.org/")
    (synopsis "PostgreSQL driver for Python")
    (description
     "Psycopg 3 is a new implementation of the popular @code{psycopg2}
database adapter for Python.")
    (license license:lgpl3+)))

(define-public python-sadisplay
  (package
    (name "python-sadisplay")
    (version "0.4.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sadisplay" version))
      (sha256
        (base32
          "15jxwgla3q4xsp6rw8inqaiy1kdzc8l2cixj8amqcf0ji47icrxg"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (begin
                            (setenv "PYTHONPATH"
                                    (string-append ".:" (or (getenv "PYTHONPATH")
                                                           "")))
                            (invoke "pytest" "-vv"))
                          (format #t "test suite not run~%")))))))
    (propagated-inputs
      (list python-sqlalchemy))
    (native-inputs
     ;; For tests.
      (list python-pytest))
    (home-page "https://bitbucket.org/estin/sadisplay")
    (synopsis "SQLAlchemy schema displayer")
    (description "This package provides a program to build Entity
Relationship diagrams from a SQLAlchemy model (or directly from the
database).")
    (license license:bsd-3)))

(define-public yoyo-migrations
  (package
    (name "yoyo-migrations")
    (version "8.2.0")
    (source
     (origin
       ;; We use the upstream repository, as the tests are not included in the
       ;; PyPI releases.
       (method hg-fetch)
       (uri (hg-reference
             (url "https://hg.sr.ht/~olly/yoyo")
             (changeset (string-append "v" version "-release"))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "1al030ix0w63hr4s3mqry6s0mlqdj8p242pdqks06br7c25nx3yj"))))
    (build-system python-build-system)
    (arguments
     ;; XXX: Tests require a connection to some pgsql database and psycopg
     ;; fails to connect to it.
     '(#:tests? #f))
    (propagated-inputs
     (list python-sqlparse python-tabulate python-importlib-metadata))
    (home-page "https://ollycope.com/software/yoyo/latest/")
    (synopsis "Database migrations with SQL")
    (description
     "Yoyo is a database schema migration tool.  Migrations are written as SQL
files or Python scripts that define a list of migration steps.")
    (license license:asl2.0)))

(define-public python-mysqlclient
  (package
    (name "python-mysqlclient")
    (version "2.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mysqlclient" version))
       (sha256
        (base32 "0hdznfz9095d2qhl7awbp39s7wpqbxn37xzan487qzaf8srrzg1k"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           #~'("tests/test__mysql.py"   ;tests not needing a live db
               "tests/test_MySQLdb_times.py")))
    (native-inputs
     (list pkg-config python-pytest python-setuptools python-wheel))
    (inputs (list mariadb-connector-c))
    (home-page "https://github.com/PyMySQL/mysqlclient")
    (synopsis "MySQLdb is an interface to the popular MySQL database server for Python")
    (description "MySQLdb is an interface to the popular MySQL database server
for Python.  The design goals are:
@enumerate
@item Compliance with Python database API version 2.0 [PEP-0249],
@item Thread-safety,
@item Thread-friendliness (threads will not block each other).
@end enumerate")
    (license license:gpl2)))

(define-public python-hiredis
  (package
    (name "python-hiredis")
    (version "3.1.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/redis/hiredis-py")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g9qj37phq9989av0bwkqy40f5wx9q08y19dsmzwd1ssqsxwhfav"))
              (patches
               (search-patches "python-hiredis-use-system-hiredis.patch"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (inputs (list hiredis))
    (home-page "https://github.com/redis/hiredis-py")
    (synopsis "Python extension that wraps protocol parsing code in hiredis")
    (description "Python-hiredis is a python extension that wraps protocol
parsing code in hiredis.  It primarily speeds up parsing of multi bulk replies.")
    (license license:bsd-3)))

(define-public python-fakeredis
  (package
    (name "python-fakeredis")
    (version "2.26.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/cunla/fakeredis-py")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10f9qwpc9vlcd2411c398n9kwjsk399vk1pjd9dbczlhvsn9s5bq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "-m" "not slow")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'poetry-compatibility
            (lambda _
              ;; Our version of poetry does not understand "to".
              (substitute* "pyproject.toml"
                ((", to = \"fakeredis\" ") ""))))
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                (("sortedcontainers = \"\\^2\\.4\"")
                 "sortedcontainers = \"^2.1\""))))
          ;; Tests require a running Redis server.
          (add-before 'check 'start-redis
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "redis-server" "--daemonize" "yes"
                        "--port" "6390")))))))
    (native-inputs (list python-poetry-core python-pytest
                         python-pytest-asyncio python-pytest-mock
                         redis))
    (propagated-inputs
     (list python-redis
           python-sortedcontainers
           python-typing-extensions))
    (home-page "https://github.com/cunla/fakeredis-py")
    (synopsis "Fake implementation of redis API for testing purposes")
    (description
     "Fakeredis is a pure-Python implementation of the redis-py Python client
that simulates talking to a redis server.  It was created for a single purpose:
to write unit tests.

Setting up redis is not hard, but one often wants to write unit tests that don't
talk to an external server such as redis.  This module can be used as a
reasonable substitute.")
    (license license:bsd-3)))

(define-public python-redis
  (package
    (name "python-redis")
    (version "5.2.0")
    (source (origin
              ;; The PyPI archive lacks some test resources such as the TLS
              ;; certificates under docker/stunnel/keys.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/redis/redis-py")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f38s704gpm8ra6vdrqhicfq7m77in60kbgcmhvmviq9qj6v3505"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-m"
              ;; These tests are disabled in the official CI run (see:
              ;; https://raw.githubusercontent.com/redis/redis-py/master/
              ;; .github/workflows/install_and_test.sh).
              (string-append "not onlycluster "
                             "and not redismod "
                             "and not ssl "
                             "and not graph")
              "-k" (string-join
                    (list
                     ;; The autoclaim test fails with "AssertionError: assert
                     ;; [b'0-0', [], []] == [b'0-0', []]".
                     "not test_xautoclaim "
                     ;; These tests cause the following error: "Error 111
                     ;; connecting to localhost:6380. Connection refused."
                     ;; (see: https://github.com/redis/redis-py/issues/2109).
                     "test_sync"
                     "test_psync"
                     ;; Same with: "Error 111 connecting to
                     ;; localhost:6479. Connection refused."
                     "test_tfcall"
                     "test_tfunction_load_delete"
                     "test_tfunction_list"
                     ;; AssertionError: assert 3 == 2
                     "test_acl_list")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests require a running Redis server.
          (add-before 'check 'start-redis
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "redis-server" "--daemonize" "yes"
                        "--enable-debug-command" "yes"
                        "--enable-module-command" "local")))))))
    (native-inputs
     (list python-numpy
           python-pytest
           python-pytest-asyncio-0.23
           python-pytest-cov
           python-pytest-timeout
           python-setuptools
           python-wheel
           redis))
    (propagated-inputs
     (list python-async-timeout))
    (home-page "https://github.com/redis/redis-py")
    (synopsis "Redis Python client")
    (description
     "This package provides a Python interface to the Redis key-value store.")
    (license license:expat)))

(define-public python-aioredis
  (deprecated-package "python-aioredis" python-redis))

(define-public python-rq
  (package
    (name "python-rq")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rq/rq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dnjm2s036l4j4ypq0h903vh132dp2wiwjrn8jicz1nw829dqpzf"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'start-redis
                    (lambda _
                      (invoke "redis-server" "--daemonize" "yes")))
                  (replace 'check
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        ;; Drop test that needs the SDK for Sentry.io.
                        (delete-file "tests/test_sentry.py")
                        ;; Ensure 'rq' and 'rqworker' ends up on PATH.
                        (setenv "PATH" (string-append out "/bin:"
                                                      (getenv "PATH")))
                        (invoke "pytest" "-vv")))))))
    (native-inputs
     (list python-mock python-psutil python-pytest redis))
    (propagated-inputs
     (list python-click python-redis))
    (home-page "https://python-rq.org/")
    (synopsis "Simple job queues for Python")
    (description
     "RQ (Redis Queue) is a simple Python library for queueing jobs and
processing them in the background with workers.  It is backed by Redis and it
is designed to have a low barrier to entry.")
    (license license:bsd-2)))

(define-public python-rq-scheduler
  (package
    (name "python-rq-scheduler")
    (version "0.10.0")
    (home-page "https://github.com/rq/rq-scheduler")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xg6yazqs5kbr2ayvhvljs1h5vgx5k5dds613fmhswln7gglf9hk"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'start-redis
                    (lambda _
                      (invoke "redis-server" "--daemonize" "yes")))
                  (replace 'check
                    (lambda _
                      (substitute* "run_tests.py"
                        (("/usr/bin/env")
                         (which "env")))
                      (invoke "./run_tests.py"))))))
    (native-inputs
     (list redis which))
    (propagated-inputs
     (list python-croniter python-rq))
    (synopsis "Job scheduling capabilities for RQ (Redis Queue)")
    (description
     "This package provides job scheduling capabilities to @code{python-rq}
(Redis Queue).")
    (license license:expat)))

(define-public python-sqlparse
  (package
    (name "python-sqlparse")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sqlparse" version))
              (sha256
               (base32
                "0s3jyllg0ka0n7pgqfng1hzvh39li853dr40qcp4s4dv8r481jk9"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "py.test")))
         ;; XXX: The regular wrap phase ends up storing pytest as a runtime
         ;; dependency.  See <https://bugs.gnu.org/25235>.
         (replace 'wrap
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (python (assoc-ref (or native-inputs inputs) "python"))
                    (sitedir (string-append "/lib/python"
                                            (python-version python)
                                            "/site-packages")))
               (wrap-program (string-append out "/bin/sqlformat")
                 `("PYTHONPATH" ":" prefix
                   ,(map (lambda (output)
                           (string-append output sitedir))
                         (list python out))))))))))
    (native-inputs (list python-pytest))
    (inputs (list bash-minimal))
    (home-page "https://github.com/andialbrecht/sqlparse")
    (synopsis "Non-validating SQL parser")
    (description "Sqlparse is a non-validating SQL parser for Python.  It
provides support for parsing, splitting and formatting SQL statements.")
    (license license:bsd-3)
    (properties '((cpe-name . "sqlparse")))))

(define-public python-sql
  (package
    (name "python-sql")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-sql" version))
       (sha256
        (base32 "0xnimfzlxj1ddrb5xj3s4gaii278a0gpxrvwmdmrdxgjfdi3lq4x"))))
    (build-system python-build-system)
    (home-page "https://python-sql.tryton.org/")
    (synopsis "Library to write SQL queries in a pythonic way")
    (description "@code{python-sql} is a library to write SQL queries, that
transforms idiomatic python function calls to well-formed SQL queries.")
    (license license:bsd-3)))

(define-public python-pypika
  (package
    (name "python-pypika")
    (version "0.47.6")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/kayak/pypika")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "001pg36sw9a36zdd1kccbymcxndphjcjbbrsy6ri7ng8h4dgz549"))))
    (build-system python-build-system)
    (native-inputs
     (list python-parameterized))
    (home-page "https://github.com/kayak/pypika")
    (synopsis "SQL query builder API for Python")
    (description
     "PyPika is a python SQL query builder that exposes the full richness of
the SQL language using a syntax that reflects the resulting query.")
    (license license:asl2.0)))

(define-public apache-orc
  (package
    (name "apache-orc")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/orc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fi6d045wakks0x8clplyxgal342kljqjql7vq5gbd6a2qnaz6m2"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:configure-flags
      #~(list "-DBUILD_JAVA=OFF"
              "-DINSTALL_VENDORED_LIBS=OFF"
              "-DCMAKE_CXX_FLAGS=-fPIC"
              (string-append "-DGTEST_HOME=" #$(this-package-native-input "googletest"))
              (string-append "-DZSTD_HOME=" (assoc-ref %build-inputs "zstd:lib"))
              (string-append "-DZLIB_HOME=" #$(this-package-input "zlib"))
              (string-append "-DPROTOBUF_HOME=" #$(this-package-input "protobuf"))
              (string-append "-DLZ4_HOME=" #$(this-package-input "lz4"))
              (string-append "-DSNAPPY_HOME=" #$(this-package-input "snappy")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-bad-test
            (lambda _
              ;; This one test fails with an obscure error:
              ;;
              ;; Expected: (std::string::npos) != (error.find(error_msg)),
              ;; actual: 18446744073709551615 vs 18446744073709551615
              (substitute* "tools/test/TestFileScan.cc"
                (("findProgram\\(\"tools/src/orc-scan\"\\);" m)
                 (string-append m "return;")))))
          (add-after 'unpack 'do-not-download-orc-format
            (lambda _
              (substitute* "cmake_modules/ThirdpartyToolchain.cmake"
                (("URL \"https://archive.apache.org/dist/orc/orc-format.*")
                 (string-append "URL \"file://"
                                #$(this-package-native-input "orc-format")
                                "\"\n")))))
          (add-after 'unpack 'timezone-fallback
            (lambda _
              ;; In the build container we don't have /etc/localtime
              (substitute* "c++/src/Timezone.cc"
                (("return getTimezoneByFilename\\(LOCAL_TIMEZONE\\);")
                 "if (!std::filesystem::exists(std::filesystem::path(LOCAL_TIMEZONE))) {
  return getTimezoneByName(\"UTC\");
}
return getTimezoneByFilename(LOCAL_TIMEZONE);"))))
          (add-before 'check 'pre-check
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "TZDIR" (search-input-directory inputs
                                                      "share/zoneinfo")))))))
    (inputs
     `(("lz4" ,lz4)
       ("protobuf" ,protobuf)
       ("snappy" ,snappy)
       ("zlib" ,zlib "static")
       ("zstd" ,zstd)
       ("zstd:lib" ,zstd "lib")))
    (native-inputs
     `(("googletest" ,googletest)
       ("orc-format" ,(origin
                        (method url-fetch)
                        (uri "https://archive.apache.org/dist/orc/orc-format-1.0.0/\
orc-format-1.0.0.tar.gz")
                        (sha256
                         (base32
                          "1mccbna3mqhhlqs4pw0fa4pgjnq4c41jhxrh84mq27sbz5gsx7vk"))))
       ("pkg-config" ,pkg-config)
       ("tzdata" ,tzdata-for-tests)))
    (home-page "https://orc.apache.org/")
    (synopsis "Columnar storage for Hadoop workloads")
    (description "ORC is a self-describing type-aware columnar file format
designed for Hadoop workloads.  It is optimized for large streaming reads, but
with integrated support for finding required rows quickly.")
    (license license:asl2.0)))

;; There are many wrappers for this in other languages. When touching, please
;; be sure to ensure all dependencies continue to build.
(define-public apache-arrow
  (package
    (name "apache-arrow")
    (version "19.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/arrow")
             (commit (string-append "apache-arrow-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l7nc12ykh6b9pcf20mvs4xsk9cyz7a45sdfs08rhilrhd8g10dn"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'enter-source-directory
            (lambda _ (chdir "cpp")))
          (add-after 'unpack 'set-env
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "cpp/cmake_modules/ThirdpartyToolchain.cmake"
                (("set\\(xsimd_SOURCE.*") ""))
              (setenv "BOOST_ROOT" #$(this-package-input "boost"))
              (setenv "BROTLI_HOME" #$(this-package-input "brotli"))
              (setenv "FLATBUFFERS_HOME" #$(this-package-input "flatbuffers"))
              (setenv "RAPIDJSON_HOME" #$(this-package-input "rapidjson")))))
      #:build-type "Release"
      #:configure-flags
      #~(list "-DARROW_PYTHON=ON"
              ;; Parquet options
              "-DARROW_PARQUET=ON"
              "-DPARQUET_BUILD_EXECUTABLES=ON"
              ;; The maintainers disallow using system versions of
              ;; jemalloc:
              ;; https://issues.apache.org/jira/browse/ARROW-3507. This
              ;; is unfortunate because jemalloc increases performance:
              ;; https://arrow.apache.org/blog/2018/07/20/jemalloc/.
              "-DARROW_JEMALLOC=OFF"
              ;; System mimalloc is not suported and must be bundled:
              ;; https://github.com/apache/arrow/issues/42211.
              "-DARROW_MIMALLOC=OFF"

              ;; The CMake option ARROW_DEPENDENCY_SOURCE is a global
              ;; option that instructs the build system how to resolve
              ;; each dependency. SYSTEM = Finding the dependency in
              ;; system paths using CMake's built-in find_package
              ;; function, or using pkg-config for packages that do not
              ;; have this feature
              "-DARROW_DEPENDENCY_SOURCE=SYSTEM"
              "-Dxsimd_SOURCE=SYSTEM"

              "-DARROW_RUNTIME_SIMD_LEVEL=NONE"
              "-DARROW_SIMD_LEVEL=NONE"
              "-DARROW_PACKAGE_KIND=Guix"

              ;; Split output into its component packages.
              (string-append "-DCMAKE_INSTALL_PREFIX=" #$output:lib)
              (string-append "-DCMAKE_INSTALL_RPATH=" #$output:lib "/lib")
              (string-append "-DCMAKE_INSTALL_BINDIR=" #$output "/bin")
              (string-append "-DCMAKE_INSTALL_INCLUDEDIR=" #$output:include
                             "/share/include")

              "-DARROW_WITH_SNAPPY=ON"
              "-DARROW_WITH_ZLIB=ON"
              "-DARROW_WITH_ZSTD=ON"
              "-DARROW_WITH_LZ4=ON"
              "-DARROW_COMPUTE=ON"
              "-DARROW_CSV=ON"
              "-DARROW_DATASET=ON"
              "-DARROW_FILESYSTEM=ON"
              "-DARROW_HDFS=ON"
              "-DARROW_JSON=ON"
              ;; Arrow Python C++ integration library (required for
              ;; building pyarrow). This library must be built against
              ;; the same Python version for which you are building
              ;; pyarrow. NumPy must also be installed. Enabling this
              ;; option also enables ARROW_COMPUTE, ARROW_CSV,
              ;; ARROW_DATASET, ARROW_FILESYSTEM, ARROW_HDFS, and
              ;; ARROW_JSON.
              "-DARROW_PYTHON=ON"
              "-DARROW_ORC=ON"
              "-DORC_SOURCE=SYSTEM"

              ;; Building the tests forces on all the
              ;; optional features and the use of static
              ;; libraries.
              "-DARROW_BUILD_TESTS=OFF"
              ;;"-DBENCHMARK_ENABLE_TESTING=OFF"
              "-DARROW_BUILD_STATIC=OFF")))
    (inputs
     (list apache-orc
           boost
           brotli
           bzip2
           double-conversion
           gflags
           glog
           grpc
           protobuf
           python
           python-numpy
           rapidjson
           re2
           snappy
           xsimd))
    ;; These are all listed under Requires.private in arrow.pc
    (propagated-inputs
     (list `(,apache-thrift "lib") lz4 utf8proc zlib
           `(,zstd "lib")))
    (native-inputs
     (list pkg-config))
    (outputs '("out" "lib" "include"))
    (home-page "https://arrow.apache.org/")
    (synopsis "Columnar in-memory analytics")
    (description "Apache Arrow is a columnar in-memory analytics layer
designed to accelerate big data.  It houses a set of canonical in-memory
representations of flat and hierarchical data along with multiple
language-bindings for structure manipulation.  It also provides IPC and common
algorithm implementations.")
    (license license:asl2.0)))

(define-public apache-arrow-for-ceph
  (package
    (name "apache-arrow")
    (version "6.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/arrow")
             (commit (string-append "apache-arrow-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mcw361akqw4sxnnpnr9c9v1zk4hphk6gcq763pcb19yzljh88ig"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enter-source-directory
           (lambda _ (chdir "cpp")))
         (add-after 'unpack 'set-env
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "cpp/src/parquet/parquet.pc.in"
               (("includedir=\\$\\{prefix\\}/")
                "includedir="))
             (substitute* "cpp/cmake_modules/ThirdpartyToolchain.cmake"
               (("set\\(xsimd_SOURCE.*") ""))
             (setenv "BOOST_ROOT" (assoc-ref inputs "boost"))
             (setenv "BROTLI_HOME" (assoc-ref inputs "brotli"))
             (setenv "FLATBUFFERS_HOME" (assoc-ref inputs "flatbuffers"))
             (setenv "RAPIDJSON_HOME" (assoc-ref inputs "rapidjson")))))
       #:build-type "Release"
       #:configure-flags
       (list "-DARROW_PYTHON=ON"
             "-DARROW_GLOG=ON"
             ;; Parquet options
             "-DARROW_PARQUET=ON"
             "-DPARQUET_BUILD_EXECUTABLES=ON"
             ;; The maintainers disallow using system versions of
             ;; jemalloc:
             ;; https://issues.apache.org/jira/browse/ARROW-3507. This
             ;; is unfortunate because jemalloc increases performance:
             ;; https://arrow.apache.org/blog/2018/07/20/jemalloc/.
             "-DARROW_JEMALLOC=OFF"

             ;; The CMake option ARROW_DEPENDENCY_SOURCE is a global
             ;; option that instructs the build system how to resolve
             ;; each dependency. SYSTEM = Finding the dependency in
             ;; system paths using CMake's built-in find_package
             ;; function, or using pkg-config for packages that do not
             ;; have this feature
             "-DARROW_DEPENDENCY_SOURCE=SYSTEM"
             "-Dxsimd_SOURCE=SYSTEM"

             "-DARROW_RUNTIME_SIMD_LEVEL=NONE"
             "-DARROW_SIMD_LEVEL=NONE"
             "-DARROW_PACKAGE_KIND=Guix"

             ;; Split output into its component packages.
             (string-append "-DCMAKE_INSTALL_PREFIX="
                            (assoc-ref %outputs "lib"))
             (string-append "-DCMAKE_INSTALL_RPATH="
                            (assoc-ref %outputs "lib")
                            "/lib")
             (string-append "-DCMAKE_INSTALL_BINDIR="
                            (assoc-ref %outputs "out")
                            "/bin")
             (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                            (assoc-ref %outputs "include")
                            "/share/include")

             "-DARROW_WITH_SNAPPY=ON"
             "-DARROW_WITH_ZLIB=ON"
             "-DARROW_WITH_ZSTD=ON"
             "-DARROW_WITH_LZ4=ON"
             "-DARROW_COMPUTE=ON"
             "-DARROW_CSV=ON"
             "-DARROW_DATASET=ON"
             "-DARROW_FILESYSTEM=ON"
             "-DARROW_HDFS=ON"
             "-DARROW_JSON=ON"
             ;; Arrow Python C++ integration library (required for
             ;; building pyarrow). This library must be built against
             ;; the same Python version for which you are building
             ;; pyarrow. NumPy must also be installed. Enabling this
             ;; option also enables ARROW_COMPUTE, ARROW_CSV,
             ;; ARROW_DATASET, ARROW_FILESYSTEM, ARROW_HDFS, and
             ;; ARROW_JSON.
             "-DARROW_PYTHON=ON"

             ;; Building the tests forces on all the
             ;; optional features and the use of static
             ;; libraries.
             "-DARROW_BUILD_TESTS=OFF"
             "-DBENCHMARK_ENABLE_GTEST_TESTS=OFF"
             ;;"-DBENCHMARK_ENABLE_TESTING=OFF"
             "-DARROW_BUILD_STATIC=OFF")))
    (inputs
     (list boost
           brotli
           bzip2
           double-conversion
           gflags
           glog
           grpc
           protobuf
           python
           python-numpy
           rapidjson
           re2
           snappy
           xsimd))
    ;; These are all listed under Requires.private in arrow.pc
    (propagated-inputs
     (list (list apache-thrift "lib")
           lz4
           utf8proc
           zlib
           (list zstd "lib")))
    (native-inputs
     (list pkg-config))
    (outputs '("out" "lib" "include"))
    (home-page "https://arrow.apache.org/")
    (synopsis "Columnar in-memory analytics")
    (description "Apache Arrow is a columnar in-memory analytics layer
designed to accelerate big data.  It houses a set of canonical in-memory
representations of flat and hierarchical data along with multiple
language-bindings for structure manipulation.  It also provides IPC and common
algorithm implementations.")
    (license license:asl2.0)))

(define-public apache-arrow-0.16
  (package
    (name "apache-arrow")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/arrow")
             (commit (string-append "apache-arrow-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15bplqy5708bxy1mynzjkd3d2g8v2wd36z8l0ap8yyyq54l3gdvy"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enter-source-directory
           (lambda _
             (chdir "cpp")
             (substitute* "src/parquet/CMakeLists.txt"
               (("    parquet_constants.cpp") "")
               (("set\\(THRIFT_OUTPUT_FILES \\$\\{THRIFT_OUTPUT_FILES\\}.*") "")
               ((".*\"\\$\\{THRIFT_OUTPUT_DIR\\}/parquet_constants.cpp\"\\).*") ""))))
         (add-after 'unpack 'set-env
           (lambda _
             (setenv "BOOST_ROOT" (assoc-ref %build-inputs "boost"))
             (setenv "BROTLI_HOME" (assoc-ref %build-inputs "brotli"))
             (setenv "FLATBUFFERS_HOME" (assoc-ref %build-inputs "flatbuffers"))
             (setenv "RAPIDJSON_HOME" (assoc-ref %build-inputs "rapidjson")))))
       #:build-type "Release"
       #:configure-flags
       (list "-DARROW_PYTHON=ON"
             "-DARROW_GLOG=ON"
             "-DARROW_SSE42=OFF"
             "-DARROW_BOOST_USE_SHARED=ON"
             ;; Parquet options
             "-DARROW_PARQUET=ON"

             ;; The maintainers disallow using system versions of
             ;; jemalloc:
             ;; https://issues.apache.org/jira/browse/ARROW-3507. This
             ;; is unfortunate because jemalloc increases performance:
             ;; https://arrow.apache.org/blog/2018/07/20/jemalloc/.
             "-DARROW_JEMALLOC=OFF"

             ;; The CMake option ARROW_DEPENDENCY_SOURCE is a global
             ;; option that instructs the build system how to resolve
             ;; each dependency. SYSTEM = Finding the dependency in
             ;; system paths using CMake's built-in find_package
             ;; function, or using pkg-config for packages that do not
             ;; have this feature
             "-DARROW_DEPENDENCY_SOURCE=SYSTEM"

             ;; Split output into its component packages.
             (string-append "-DCMAKE_INSTALL_PREFIX="
                            (assoc-ref %outputs "out"))
             (string-append "-DCMAKE_INSTALL_RPATH="
                            (assoc-ref %outputs "out")
                            "/lib")
             (string-append "-DCMAKE_INSTALL_BINDIR="
                            (assoc-ref %outputs "out")
                            "/bin")
             (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                            (assoc-ref %outputs "include")
                            "/share/include")


             "-DARROW_WITH_SNAPPY=ON"
             "-DARROW_WITH_ZLIB=ON"
             "-DARROW_WITH_ZSTD=ON"
             "-DARROW_WITH_LZ4=ON"
             "-DARROW_COMPUTE=ON"
             "-DARROW_CSV=ON"
             "-DARROW_DATASET=ON"
             "-DARROW_FILESYSTEM=ON"
             "-DARROW_HDFS=ON"
             "-DARROW_JSON=ON"
             ;; Arrow Python C++ integration library (required for
             ;; building pyarrow). This library must be built against
             ;; the same Python version for which you are building
             ;; pyarrow. NumPy must also be installed. Enabling this
             ;; option also enables ARROW_COMPUTE, ARROW_CSV,
             ;; ARROW_DATASET, ARROW_FILESYSTEM, ARROW_HDFS, and
             ;; ARROW_JSON.
             "-DARROW_PYTHON=ON"

             ;; Building the tests forces on all the
             ;; optional features and the use of static
             ;; libraries.
             "-DARROW_BUILD_TESTS=OFF"
             "-DBENCHMARK_ENABLE_GTEST_TESTS=OFF"
             ;;"-DBENCHMARK_ENABLE_TESTING=OFF"
             "-DARROW_BUILD_STATIC=OFF")))
    (inputs
     `(("boost" ,boost)
       ("brotli" ,brotli)
       ("double-conversion" ,double-conversion)
       ("snappy" ,snappy)
       ("gflags" ,gflags)
       ("glog" ,glog)
       ("apache-thrift" ,apache-thrift "lib")
       ("protobuf" ,protobuf)
       ("rapidjson" ,rapidjson)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("lz4" ,lz4)
       ("zstd" ,zstd "lib")
       ("re2" ,re2)
       ("grpc" ,grpc)
       ("python-3" ,python)
       ("python-numpy" ,python-numpy)))
    (native-inputs
     (list pkg-config apache-thrift))
    (outputs '("out" "include"))
    (home-page "https://arrow.apache.org/")
    (synopsis "Columnar in-memory analytics")
    (description "Apache Arrow is a columnar in-memory analytics layer
designed to accelerate big data.  It houses a set of canonical in-memory
representations of flat and hierarchical data along with multiple
language-bindings for structure manipulation.  It also provides IPC and common
algorithm implementations.")
    (license license:asl2.0)))

(define-public python-pyarrow
  (package
    (inherit apache-arrow)
    (name "python-pyarrow")
    (build-system python-build-system)
    (arguments
     (list
      ;; XXX: Test data is distributed separately in
      ;; <https://github.com/apache/arrow-testing> 39MiB and requires
      ;; additional steps to be implemented, see
      ;; <https://github.com/apache/arrow/blob/main/ci/scripts/python_build.sh>.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build) ; XXX the build is performed again during the install phase
          (add-after 'unpack 'enter-source-directory
            (lambda _ (chdir "python")))
          (add-after 'enter-source-directory 'set-version
            (lambda _
              ;; XXX: This python-setuptools-scm option is available in v8+:
              ;; TypeError: Configuration.__init__() got an unexpected
              ;; keyword argument 'version_file'
              (substitute* "pyproject.toml"
                (("version_file = .*") ""))

              ;; Version file generation ad-hoc, remove when a newer version
              ;; of python-setuptools-scm is packed.
              (with-output-to-file "pyarrow/_generated_version.py"
                (let* ((version #$(package-version this-package) )
                       (version-tuple (string-join (string-split version #\.) ", ")))
                  (lambda ()
                    (format #t
                            "__version__ = version = '~a'
__version_tuple__ = version_tuple = (~a)~%" version version-tuple))))))
          (add-before 'install 'set-pyarrow-build-options
            (lambda _
              (setenv "PYARROW_BUNDLE_ARROW_CPP_HEADERS" "0")
              (setenv "PYARROW_CMAKE_OPTIONS"
                      (string-append "-DCMAKE_INSTALL_RPATH=" #$output))
              (setenv "PYARROW_PARALLEL"
                      (number->string (parallel-job-count)))
              (setenv "PYARROW_WITH_DATASET" "1")
              (setenv "PYARROW_WITH_HDFS" "1")
              (setenv "PYARROW_WITH_ORC" "1")
              (setenv "PYARROW_WITH_PARQUET" "1"))))))
    (propagated-inputs
     (list (list apache-arrow "lib")
           (list apache-arrow "include")
           apache-orc
           python-numpy
           python-pandas
           python-six))
    (native-inputs
     (list cmake-minimal
           pkg-config
           python-cython
           python-pytest
           python-pytest-runner
           python-setuptools-scm))
    (outputs '("out"))
    (home-page "https://arrow.apache.org/docs/python/")
    (synopsis "Python bindings for Apache Arrow")
    (description
     "This library provides a Pythonic API wrapper for the reference Arrow C++
implementation, along with tools for interoperability with pandas, NumPy, and
other traditional Python scientific computing packages.")
    (license license:asl2.0)))

(define-public python-pyarrow-0.16
  (package
    (inherit apache-arrow-0.16)
    (name "python-pyarrow")
    (build-system python-build-system)
    (arguments
     '(#:tests? #f          ; XXX There are no tests in the "python" directory
       #:phases
       (modify-phases %standard-phases
         (delete 'build) ; XXX the build is performed again during the install phase
         (add-after 'unpack 'enter-source-directory
           (lambda _ (chdir "python")))
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))))
         (add-before 'install 'patch-cmake-variables
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Replace cmake locations with hardcoded guix links for the
             ;; underlying C++ library and headers.  This is a pretty awful
             ;; hack.
             (substitute* "cmake_modules/FindParquet.cmake"
               (("# Licensed to the Apache Software Foundation" m)
                (string-append "set(PARQUET_INCLUDE_DIR \""
                               (assoc-ref inputs "apache-arrow:include")
                               "/share/include\")\n" m))
               (("find_package_handle_standard_args" m)
                (string-append "set(PARQUET_LIB_DIR \""
                               (assoc-ref inputs "apache-arrow:lib")
                               "/lib\")\n" m)))))
         (add-before 'install 'patch-parquet-library
           (lambda _
             (substitute* "CMakeLists.txt"
               (("parquet_shared") "parquet"))))
         (add-before 'install 'set-PYARROW_WITH_PARQUET
           (lambda _
             (setenv "PYARROW_WITH_PARQUET" "1"))))))
    (propagated-inputs
     `(("apache-arrow:lib" ,apache-arrow-0.16)
       ("apache-arrow:include" ,apache-arrow-0.16 "include")
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-six" ,python-six)))
    (native-inputs
     (list cmake-minimal
           pkg-config
           python-cython
           python-pytest
           python-pytest-runner
           python-setuptools-scm))
    (outputs '("out"))
    (home-page "https://arrow.apache.org/docs/python/")
    (synopsis "Python bindings for Apache Arrow")
    (description
     "This library provides a Pythonic API wrapper for the reference Arrow C++
implementation, along with tools for interoperability with pandas, NumPy, and
other traditional Python scientific computing packages.")
    (license license:asl2.0)))

(define-public python-fastparquet
  (package
    (name "python-fastparquet")
    (version "2024.2.0")
    (source
     (origin
       ;; Fastparquet uses setuptools-scm to find the current version. This
       ;; only works when we use the PyPI tarball, which does not contain
       ;; tests. Instead, we use the git-fetch method and set the version via
       ;; envar.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dask/fastparquet")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f32dj1xvd11l0siznqd33dpjlhg9siylcjcfkcdlqfcy45jfj3v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.py"
                ;; Remove dependencies on git.
                (("^.*\"git\", \"status\".*$") "")
                ;; Guix is only compatible with a single version of numpy
                ;; at a time. We can safely remove this dependency.
                (("'oldest-supported-numpy'") ""))))
          (add-before 'build 'pretend-version
            ;; The version string is usually derived via setuptools-scm, but
            ;; without the git metadata available, the version string is set
            ;; to '0.0.0'.
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'check 'build-cython-extensions
            ;; Cython extensions need to be built for the check phase.
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-cramjam
           python-fsspec
           python-lzo
           python-numpy
           python-packaging
           python-pandas))
    (native-inputs
     (list python-cython
           python-pytest-runner
           python-pytest-xdist
           python-setuptools-scm))
    (home-page "https://github.com/dask/fastparquet")
    (synopsis "Python implementation of the Parquet file format")
    (description
     "@code{fastparquet} is a Python implementation of the Parquet file
format.  @code{fastparquet} is used implicitly by @code{dask}, @code{pandas}
and @code{intake-parquet}.  It supports the following compression algorithms:

@itemize
@item Gzip
@item Snappy
@item Brotli
@item LZ4
@item Zstd
@item LZO (optionally)
@end itemize")
    (license license:asl2.0)))

(define-public python-crate
  (package
    (name "python-crate")
    (version "0.23.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "crate" version))
              (sha256
               (base32
                "0ngmlvi320c5gsxab0s7qgq0ck4jdlcwvb6lbjhnfprafdp56vvx"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-urllib3))
    (home-page "https://github.com/crate/crate-python")
    (synopsis "CrateDB Python client")
    (description
     "This package provides a Python client library for CrateDB.
It implements the Python DB API 2.0 specification and includes support for
SQLAlchemy.")
    (license license:asl2.0)))

(define-public libdbi
  (package
    (name "libdbi")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libdbi/libdbi/libdbi-"
                                  version "/libdbi-" version ".tar.gz"))
              (sha256
               (base32
                "00s5ra7hdlq25iv23nwf4h1v3kmbiyzx0v9bhggjiii4lpf6ryys"))))
    (build-system gnu-build-system)
    (synopsis "Database independent abstraction layer in C")
    (description
     "This library implements a database independent abstraction layer in C,
similar to the DBI/DBD layer in Perl.  Writing one generic set of code,
programmers can leverage the power of multiple databases and multiple
simultaneous database connections by using this framework.")
    (home-page "https://libdbi.sourceforge.net/")
    (license license:lgpl2.1+)))

(define-public libdbi-drivers
  (package
    (name "libdbi-drivers")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libdbi-drivers/"
                                  "libdbi-drivers/libdbi-drivers-" version
                                  "/libdbi-drivers-" version ".tar.gz"))
              (sha256
               (base32
                "0m680h8cc4428xin4p733azysamzgzcmv4psjvraykrsaz6ymlj3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list ;; For tests.
           inetutils glibc-locales mariadb))
    (inputs
     `(("libdbi" ,libdbi)
       ("mariadb:dev" ,mariadb "dev")
       ("mariadb:lib" ,mariadb "lib")
       ("postgresql" ,postgresql)
       ("sqlite" ,sqlite)))
    (arguments
     `(#:configure-flags
       (let ((libdbi (assoc-ref %build-inputs "libdbi"))
             (mysql:inc (assoc-ref %build-inputs "mariadb:dev"))
             (mysql:lib (assoc-ref %build-inputs "mariadb:lib"))
             (postgresql (assoc-ref %build-inputs "postgresql"))
             (sqlite (assoc-ref %build-inputs "sqlite")))
         (list "--disable-docs"
               (string-append "--with-dbi-incdir=" libdbi "/include")
               (string-append "--with-dbi-libdir=" libdbi "/lib")
               "--with-mysql"
               (string-append "--with-mysql-incdir=" mysql:inc "/include/mysql")
               (string-append "--with-mysql-libdir=" mysql:lib "/lib")
               "--with-pgsql"
               (string-append "--with-pgsql-incdir=" postgresql "/include")
               (string-append "--with-pgsql-libdir=" postgresql "/lib")
               "--with-sqlite3"
               (string-append "--with-sqlite-incdir=" sqlite "/include")
               (string-append "--with-sqlite-libdir=" sqlite "/lib")))
       #:tests? #f  ; FIXME: Find why the tests get stuck forever.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "tests/test_mysql.sh"
               (("^MYMYSQLD=.*")
                (string-append "MYMYSQLD="
                               (assoc-ref inputs "mariadb")
                               "/bin/mysqld")))
             #t))
         (add-after 'install 'remove-empty-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((var (string-append (assoc-ref outputs "out") "/var")))
               (delete-file-recursively var))
             #t)))))
    (synopsis "Database drivers for the libdbi framework")
    (description
     "The @code{libdbi-drivers} library provides the database specific drivers
for the @code{libdbi} framework.

The drivers officially supported by @code{libdbi} are:
@itemize
@item MySQL,
@item PostgreSQL,
@item SQLite.
@end itemize")
    (home-page "https://libdbi-drivers.sourceforge.net/")
    (license license:lgpl2.1+)))

(define-public soci
  (package
    (name "soci")
    (version "4.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SOCI/soci/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12aq7pama96l2c1kmfkclb4bvrsxs9a8ppgk5gmzw45w2lg35i0y"))))
    (build-system cmake-build-system)
    (propagated-inputs
     ;; Headers of soci has include-references to headers of these inputs.
     `(("firebird" ,firebird)
       ("postgresql" ,postgresql)
       ("sqlite" ,sqlite)
       ("odbc" ,unixodbc)
       ("boost" ,boost)
       ("mariadb:dev" ,mariadb "dev")))
    (arguments
     `(#:configure-flags
       ;; C++11 (-DSOCI_CXX11) is OFF by default.  hyperledger-iroha needs it.
       (list "-DCMAKE_CXX_STANDARD=17"
             "-DSOCI_LIBDIR=lib"
             ;; This is for relocation when linking statically
             "-DCMAKE_CXX_FLAGS=-fPIE")
       #:tests? #f))         ; may require running database management systems
    (synopsis "C++ Database Access Library")
    (description
     "SOCI is an abstraction layer for several database backends, including
PostreSQL, SQLite, ODBC and MySQL.")
    (home-page "https://soci.sourceforge.net/")
    (license license:boost1.0)))

(define-public freetds
  (package
    (name "freetds")
    (version "1.2.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freetds.org/files/stable/"
                           "freetds-" version ".tar.gz"))
       (sha256
        (base32 "11fzwcahc1bc8npxbif0448v9cwyf7k04167i7fcspmfw7a0hj0d"))))
    (build-system gnu-build-system)
    (arguments
     ;; NOTE: (Sharlatan-20210110213908+0000) some tests require DB connection,
     ;; disabled for now.
     `(#:tests? #f))
    (home-page "https://www.freetds.org/")
    (synopsis "Client libraries for MS SQL and Sybase servers")
    (description
     "FreeTDS is an implementation of the Tabular DataStream protocol, used for
connecting to MS SQL and Sybase servers over TCP/IP.")
    (license license:lgpl2.0+)))

(define-public python-tinydb
  (package
    (name "python-tinydb")
    (version "4.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tinydb" version))
              (sha256
               (base32 "0r6mavw0wm03lbmqbgglsyff3vws86ddg3q8h29glpd7qnlqdmkd"))))
    (build-system pyproject-build-system)
    (native-inputs
      (list python-poetry-core python-pytest))
    (home-page "https://github.com/msiemens/tinydb")
    (synopsis "TinyDB is a lightweight document oriented database")
    (description
     "TinyDB is a small document oriented database written in pure Python
with no external dependencies.  The targets are small apps that would
be blown away by a SQL-DB or an external database server.")
    (license license:expat)))

(define-public sequeler
  (package
    (name "sequeler")
    (version "0.8.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Alecaddd/sequeler")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0biggmsn8k7j6pdrwk29whl56qlfgvf5d9vjpgz4nyqih56wgh9j"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") "true")
               (("update-desktop-database") "true")))))))
    (native-inputs
     `(;("appstream-glib" ,appstream-glib)  ; validation fails for lack of network
       ("gettext-minimal" ,gettext-minimal)
       ("glib:bin" ,glib "bin")             ; for glib-compile-resources
       ("gtk+" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("glib" ,glib)
       ("granite" ,granite)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("gtksourceview-3" ,gtksourceview-3)
       ("libgda" ,libgda)
       ("libgee" ,libgee)
       ("libsecret" ,libsecret)
       ("libssh2" ,libssh2)
       ("libxml2" ,libxml2)))
    (synopsis "Friendly SQL Client")
    (description "Sequeler is a native Linux SQL client built in Vala and
Gtk.  It allows you to connect to your local and remote databases, write SQL in
a handy text editor with language recognition, and visualize SELECT results in
a Gtk.Grid Widget.")
    (home-page "https://github.com/Alecaddd/sequeler")
    (license license:gpl2+)))

(define-public sqlitebrowser
  (package
    (name "sqlitebrowser")
    (version "3.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sqlitebrowser/sqlitebrowser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v4a59lgqng9hqvbc2k504aclinpwpk8hi6ckirbr6m1pnf26kfr"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "libs/qcustomplot-source/")
           (delete-file-recursively "libs/qhexedit/")
           (delete-file-recursively "libs/qscintilla")))))
    (build-system qt-build-system)
    (arguments
     (list #:configure-flags
           ;; TODO: Unbundle json (nlohmann-json).
           #~(list (string-append "-DQSCINTILLA_INCLUDE_DIR="
                                  #$(this-package-input "qscintilla")
                                  "/include/Qsci")
                   "-DFORCE_INTERNAL_QCUSTOMPLOT=OFF"
                   "-DFORCE_INTERNAL_QHEXEDIT=OFF"
                   "-DENABLE_TESTING=ON")))
    (inputs
     (list qcustomplot
           qhexedit
           qscintilla
           qtbase-5
           sqlite))
    (native-inputs (list qttools-5))
    (home-page "https://sqlitebrowser.org/")
    (synopsis "Visual database browser and editor for SQLite")
    (description "Sqlitebrowser lets you create, design, and edit database files
compatible with SQLite using a graphical user interface.")
    (license
     ;; dual license
     (list license:gpl3+
           license:mpl2.0))))

(define-public sqls
  (package
    (name "sqls")
    ;; TODO: The latest version requires a way more packages to be available
    ;; in Guix.
    (version "0.2.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sqls-server/sqls")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13837v27avdp2nls3vyy7ml12nj7rxragchwf92adn10ffp4aj6c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/lighttiger2505/sqls"
      ;; XXX: Try to enable more or all tests.
      #:test-subdirs #~(list "ast/..."
                             "dialect/..."
                             "parser/..."
                             "internal/completer"
                             "internal/config/..."
                             "internal/database"
                             "internal/debug"
                             "internal/formatter"
                             "internal/lsp")))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-go-sql-driver-mysql
           go-github-com-k0kubun-pp
           go-github-com-lib-pq
           go-github-com-mattn-go-runewidth
           go-github-com-mattn-go-sqlite3
           go-github-com-olekukonko-tablewriter
           go-github-com-pkg-errors
           go-github-com-sourcegraph-jsonrpc2
           go-golang-org-x-crypto
           go-golang-org-x-xerrors
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/sqls-server/sqls")
    (synopsis "SQL language server written in Go")
    (description
     "This package implements the @acronym{LSP, Language Server Protocol} for SQL.")
    (license license:expat)))

(define-public python-dogpile-cache
  (package
    (name "python-dogpile-cache")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dogpile_cache" version))
       (sha256
        (base32 "1h58g3iyv6gc6x3in252d4c1v5sncvishjy82dz3zpazbxbra0jg"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-mako
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-decorator
           python-stevedore
           python-typing-extensions))
    (home-page "https://github.com/sqlalchemy/dogpile.cache")
    (synopsis "Caching front-end based on the Dogpile lock")
    (description "@code{dogpile.cache} is a caching API which provides a
generic interface to caching backends of any variety, and additionally
provides API hooks which integrate these cache backends with the locking
mechanism of @code{dogpile}.")
    (license license:expat)))

(define-public datasette
  (package
    (name "datasette")
    (version "1.0a16")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/simonw/datasette")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10c754idn9ka5hhai1qwjwlxw4dajdlrh162k71i5gwn4cgq6wr5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; See https://github.com/simonw/datasette/issues/2048
      #~(list "-k" (string-append
                    ;; These contain two unexpected extra items.
                    "not test_searchable"
                    " and not test_searchmode")
              "-n" (number->string (parallel-job-count)))))
    (propagated-inputs
     (list python-aiofiles
           python-asgi-csrf
           python-asgiref
           python-asyncinject
           python-click
           python-click-default-group
           python-httpx
           python-hupper
           python-itsdangerous
           python-janus
           python-jinja2
           python-mergedeep
           python-pint
           python-pluggy
           python-pyyaml
           python-sqlite-utils
           python-uvicorn))
    (native-inputs
     (list nss-certs-for-test
           python-beautifulsoup4
           python-black
           python-cogapp
           python-pip
           python-pytest
           python-pytest-asyncio
           python-pytest-runner
           python-pytest-timeout
           python-pytest-xdist
           python-setuptools
           python-trustme
           python-wheel))
    (home-page "https://datasette.io/")
    (synopsis "Multi-tool for exploring and publishing data")
    (description "Datasette is a tool for exploring and publishing data.
It helps people take data of any shape or size and publish that as an
interactive, explorable website and accompanying API.")
    (license license:asl2.0)))
