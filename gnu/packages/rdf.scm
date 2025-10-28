;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2018, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2020, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 pukkamustard <pukkamustard@posteo.net>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2023 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages rdf)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public raptor2
  (package
    (name "raptor2")
    (version "2.0.16")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.librdf.org/source/" name
                                 "-" version ".tar.gz"))
             (sha256
              (base32
               "1026whyxpajwijlr4k5c0iliwn09mwxrg7gkvd5kb0n9ga6vg788"))
             (patches (search-patches "raptor2-libxml2.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list curl libxml2 libxslt zlib))
    (arguments
     `(#:parallel-tests? #f))
    (home-page "https://librdf.org/raptor/")
    (synopsis "RDF syntax library")
    (description "Raptor is a C library providing a set of parsers and
serialisers that generate Resource Description Framework (RDF) triples
by parsing syntaxes or serialise the triples into a syntax.  The supported
parsing syntaxes are RDF/XML, N-Quads, N-Triples 1.0 and 1.1, TRiG,
Turtle 2008 and 2013, RDFa 1.0 and 1.1, RSS tag soup including all versions
of RSS, Atom 1.0 and 0.3, GRDDL and microformats for HTML, XHTML and
XML.  The serialising syntaxes are RDF/XML (regular, abbreviated, XMP),
Turtle 2013, N-Quads, N-Triples 1.1, Atom 1.0, RSS 1.0, GraphViz DOT,
HTML and JSON.")
    (license license:lgpl2.1+))) ; or any choice of gpl2+ or asl2.0

(define-public clucene
  (package
    (name "clucene")
    (version "2.3.3.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/clucene/"
                                 "clucene-core-unstable/2.3/clucene-core-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1arffdwivig88kkx685pldr784njm0249k0rb1f1plwavlrw9zfx"))
             (patches (search-patches "clucene-pkgconfig.patch"
                                      "clucene-contribs-lib.patch"
                                      "clucene-gcc-14.patch"))))
    (build-system cmake-build-system)
    (inputs
     (list boost ; could also use bundled copy
           zlib))
    (arguments
     `(#:configure-flags '("-DBUILD_CONTRIBS_LIB=ON")
       #:tests? #f)) ; Tests do not compile, as TestIndexSearcher.cpp uses
                     ; undeclared usleep. After fixing this, one needs to run
                     ; "make test" in addition to "make cl_test", then
                     ; SimpleTest fails.
                     ; Notice that the library appears to be unmaintained
                     ; with no reaction to bug reports.
    (home-page "https://clucene.sourceforge.net/")
    (synopsis "C text indexing and searching library")
    (description "CLucene is a high-performance, scalable, cross platform,
full-featured indexing and searching API.  It is a port of the very popular
Java Lucene text search engine API to C++.")
    (license license:lgpl2.1)))

(define-public lucene++
  (package
    (name "lucene++")
    (version "3.0.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/luceneplusplus/LucenePlusPlus")
                     (commit (string-append "rel_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12v7r62f7pqh5h210pb74sfx6h70lj4pgfpva8ya2d55fn0qxrr2"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (substitute* (list "src/config/core/CMakeLists.txt"
                                      "src/config/contrib/CMakeLists.txt")
                     (("include/pkgconfig")
                      "lib/pkgconfig")
                     (("include/cmake")
                      "share/cmake/lucene++"))))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "-DLIB_DESTINATION:PATH="
                                  #$output "/lib")
                   "-DINSTALL_GTEST:BOOL=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 ;; XXX Tests are built unconditionally during the 'build phase.
                 ;; There's no ‘test’ target.  README.md suggests running this.
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "src/test/lucene++-tester"
                             "--test_dir=../source/src/test/testfiles")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list boost zlib))
    (home-page "https://github.com/luceneplusplus/LucenePlusPlus")
    (synopsis "Text search engine")
    (description "Lucene++ is an up to date C++ port of the popular Java
Lucene library, a high-performance, full-featured text search engine.")
    (license (list license:asl2.0 license:lgpl3+)))); either asl or lgpl.

(define-public lrdf
  (package
    (name "lrdf")
    (version "0.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/swh/LRDF")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00wzkfb8y0aqd519ypz067cq099dpc89w69zw8ln39vl6f9x2pd4"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-out-of-tree-references
           (lambda _
             ;; remove_test depends on an out-of-tree RDF file
             (substitute* "examples/Makefile.am"
               (("instances_test remove_test") "instances_test")
               (("\\$\\(TESTS\\) remove_test") "$(TESTS)"))
             #t))
         ;; The default bootstrap phase executes autogen.sh, which fails.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vif") #t)))))
    (inputs
     (list cyrus-sasl mit-krb5 nettle raptor2 zlib))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://github.com/swh/LRDF")
    (synopsis "Lightweight RDF library for accessing LADSPA plugin metadata")
    (description
     "LRDF is a library to make it easy to manipulate RDF files describing
LADSPA plugins.  It can also be used for general RDF manipulation.  It can
read RDF/XLM and N3 files and export N3 files, and it also has a light
taxonomic inference capability.")
    (license license:gpl2)))

(define-public rasqal
  (package
    (name "rasqal")
    (version "0.9.33")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.librdf.org/source/" name
                                 "-" version ".tar.gz"))
             (sha256
              (base32
               "0z6rrwn4jsagvarg8d5zf0j352kjgi33py39jqd29gbhcnncj939"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl perl-xml-dom ; for the tests
           pkg-config))
    (inputs
     (list libgcrypt libxml2 mpfr pcre
           `(,util-linux "lib")))
    (propagated-inputs
     (list raptor2)) ; stipulated by rasqal.pc
    (arguments
     `(#:parallel-tests? #f
       ; test failure reported upstream, see
       ; http://bugs.librdf.org/mantis/view.php?id=571
       #:tests? #f))
    (home-page "https://librdf.org/rasqal/")
    (synopsis "RDF query library")
    (description "Rasqal is a C library that handles Resource Description
Framework (RDF) query language syntaxes, query construction and execution
of queries returning results as bindings, boolean, RDF graphs/triples or
syntaxes.  The supported query languages are SPARQL Query 1.0,
SPARQL Query 1.1, SPARQL Update 1.1 (no executing) and the Experimental
SPARQL extensions (LAQRS).  Rasqal can write binding query results in the
SPARQL XML, SPARQL JSON, CSV, TSV, HTML, ASCII tables, RDF/XML and
Turtle/N3 and read them in SPARQL XML, RDF/XML and Turtle/N3.")
    (license license:lgpl2.1+))) ; or any choice of gpl2+ or asl2.0

(define-public redland
  ;; XXX: No tags for the last release, altough it's clear from
  ;; Github and logs a new release has been made.
  (let ((commit "3ec9bda623107f9b1c86c0a3f261ffd3f8a40965")
        (revision "0"))
    (package
      (name "redland")
      (version (git-version "1.0.17" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dajobe/librdf")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qwxmwi96lqjqi1mdx0mfbw23a5xg20xbv8bv18pmr4h4fxr51q7"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            ;; XXX: Copied files from libtool have been generated with
            ;; another version of autoconf.
            (add-after 'bootstrap 'bootstrap-libtool
              (lambda _
                (with-directory-excursion "libltdl"
                  (delete-file "aclocal.m4")
                  (invoke "autoreconf" "-vfi"))))
            ;; XXX: 1/17 fails with recent compilers. No patch seems to have
            ;; been done for Nix nor Debian.
            (add-after 'unpack 'disable-problematic-test
              (lambda _
                (substitute* "src/Makefile.am"
                  (("test rdf_parser_test")
                   "test")))))))
      (native-inputs
       (list autoconf
             automake
             libtool
             gtk-doc
             perl ; needed for installation
             pkg-config))
      (propagated-inputs
       (list rasqal)) ; in Requires.private field of .pc
      (inputs
       (list bdb))
      (home-page "https://librdf.org/")
      (synopsis "RDF library")
      (description
       "The Redland RDF Library (librdf) provides the RDF API and triple
stores.")
      (license license:lgpl2.1+)))) ; or any choice of gpl2+ or asl2.0

(define-public serd
  (package
    (name "serd")
    (version "0.32.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.drobilla.net/serd-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "18cwj8xxsaq6iw45kcljbhrral0cqvav80p4mdv2l7g0d2a6ks6i"))))
    (build-system meson-build-system)
    (native-inputs (list python-minimal))
    (home-page "https://drobilla.net/software/serd.html")
    (synopsis "Library for RDF syntax supporting Turtle and NTriples")
    (description
     "Serd is a lightweight C library for RDF syntax which supports reading
and writing Turtle and NTriples.  Serd is not intended to be a swiss-army
knife of RDF syntax, but rather is suited to resource limited or performance
critical applications (e.g. converting many gigabytes of NTriples to Turtle),
or situations where a simple reader/writer with minimal dependencies is
ideal (e.g. in LV2 implementations or embedded applications).")
    (license license:isc)))

(define-public sord
  (package
    (name "sord")
    (version "0.16.16")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.drobilla.net/sord-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1l2zjz6gypxbf1z32zyqkljdcn9mz452djc4xq1dlhv1fmnqfzr5"))))
    (build-system meson-build-system)
    (inputs
     (list pcre))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list serd zix))                 ;required by sord-0.pc
    (home-page "https://drobilla.net/software/sord.html")
    (synopsis "C library for storing RDF data in memory")
    (description
     "Sord is a lightweight C library for storing RDF data in memory.")
    (license license:isc)))

(define-public python-rdflib
  (package
    (name "python-rdflib")
    (version "7.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rdflib" version))
        (sha256
         (base32
          "0glin9v600gmaa0pm8f742ja390ncr7xi0x95j05hiansdmyhk8n"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-m" "not webtest"
             ;; These two tests attempt to use "pip install"
             "-k" "not test_sparqleval and not test_parser")))
    (native-inputs
     (list python-pytest python-poetry-core))
    (propagated-inputs
      (list python-isodate
            python-lxml
            python-networkx
            python-orjson
            python-pyparsing))
    (home-page "https://github.com/RDFLib/rdflib")
    (synopsis "Python RDF library")
    (description
      "RDFLib is a Python library for working with RDF, a simple yet
powerful language for representing information.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))

(define-public python-rdflib-6
  (package
    (name "python-rdflib")
    (version "6.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rdflib" version))
        (sha256
         (base32
          "1q122padnlmwm4slzpc90hz5bf2nj1d0rk3yxancmx04ywgmkbvj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This test needs a font that is not shipped.
      '(list "--ignore=test/test_so_69984830.py"
             ;; These tests need internet access.
             "--ignore=rdflib/extras/infixowl.py"
             "--ignore=test/test_examples.py"
             "--ignore=test/test_sparql/test_service.py"
             "--ignore-glob=test/test_extras/test_infixowl/*.py"
             "--ignore=test/jsonld/test_onedotone.py"
             ;; These tests use pip install
             "--ignore=test/test_misc/test_plugins.py"
             ;; Unknown causes
             "--ignore=rdflib/__init__.py"
             "--ignore=test/test_misc/test_parse_file_guess_format.py"
             ;; Exceeds maximum recursion depth
             "-k" "not test_literal_addsub")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'compatibility
           (lambda _
             (substitute* "pyproject.toml"
               (("^isodate = .*") "isodate = \">0.6.0\"\n"))
             (substitute* "PKG-INFO"
               (("^Requires-Dist: isodate .*")
                "Requires-Dist: isodate (>=0.6.0)\n")))))))
    (native-inputs
     (list python-poetry-core python-pytest python-pytest-cov))
    (propagated-inputs
      (list python-html5lib python-isodate python-pyparsing))
    (home-page "https://github.com/RDFLib/rdflib")
    (synopsis "Python RDF library")
    (description
      "RDFLib is a Python library for working with RDF, a simple yet
powerful language for representing information.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))

(define-public python-cfgraph
  (package
    (name "python-cfgraph")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "CFGraph" version))
        (sha256
         (base32
          "0x7yz0lvqb6mkhl5fbml27sppmscgpf8v2ism9jzzf0h982ffzxm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-rdflib))
    (home-page "https://github.com/hsolbrig/CFGraph")
    (synopsis "RDF Collections flattener for rdflib")
    (description
     "This package contains RDF Collections flattener for @code{rdflib}.")
    (license license:asl2.0)))

(define-public hdt-cpp
  (package
    (name "hdt-cpp")
    (version "1.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/rdfhdt/hdt-cpp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vsq80jnix6cy78ayag7v8ajyw7h8dqyad1q6xkf2hzz3skvr34z"))))
    (build-system gnu-build-system)
    (inputs
     (list serd zlib))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://github.com/rdfhdt/hdt-cpp")
    (synopsis "C++ implementation of the HDT compression format")
    (description "Header Dictionary Triples (HDT) is a compression format for
RDF data that can also be queried for Triple Patterns.  This package provides a
C++ library as well as various command-line tools to to work with HDT.")
(license license:lgpl2.1+)))

(define-public python-pyrdfa3
  (package
    (name "python-pyrdfa3")
    (version "3.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyRdfa3" version))
       (sha256
        (base32 "1hhlhgqkc3igzdpxllf41drrqxm5aswqhwvnjqb90q3zjnmiss3k"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f)) ;no test suite
    (propagated-inputs (list python-html5lib python-rdflib python-requests))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://www.w3.org/2012/pyRdfa/")
    (synopsis "RDFa Python distiller/parser library")
    (description "This library can extract RDFa 1.1 from (X)HTML, SVG, or XML.
It can produce serialized versions of the extracted graph, or an RDFLib
Graph.")
    (license license:bsd-3)))

(define-public python-sparqlwrapper
  (package
    (name "python-sparqlwrapper")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/RDFLib/sparqlwrapper")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b4vg754kcxvinwdv7pjfmwbnmgm50w8mb2naf2lwp27bpyllvkb"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; The test suite simply queries external HTTP endpoints.
    (propagated-inputs (list python-rdflib))
    (home-page "https://rdflib.dev/sparqlwrapper/")
    (synopsis "SPARQL Endpoint interface to Python")
    (description "Python wrapper around a SPARQL service.  It helps in creating
the query URI and, possibly, convert the result into a more manageable
format.")
    (license license:w3c)))
