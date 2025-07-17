;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2016, 2018-2019, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015-2018, 2020-2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021, 2023 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 David Larsson <david.larsson@selfhosted.xyz>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2025 Antoine Côté <antoine.cote@posteo.net>
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

(define-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix deprecation)
  #:use-module (guix utils)
  #:use-module (guix search-paths)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public libxmlb
  (package
    (name "libxmlb")
    (version "0.3.14")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/hughsie/libxmlb")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qcp881s559wc73db91vjccrv0d3zva87l5jdp5w0ygzz6bmg5cn"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     (list gobject-introspection gtk-doc/stable pkg-config))
    (inputs
     (list appstream-glib glib))
    (propagated-inputs
     (list xz `(,zstd "lib")))             ; in Requires.private of xmlb.pc
    (synopsis "Library to help create and query binary XML blobs")
    (description "Libxmlb library takes XML source, and converts it to a
structured binary representation with a deduplicated string table; where the
strings have the NULs included.  This allows an application to mmap the binary
XML file, do an XPath query and return some strings without actually parsing
the entire document.")
    (home-page "https://github.com/hughsie/libxmlb")
    (license license:lgpl2.1+)))

(define-public expat
  (package
    (name "expat")
    (version "2.5.0")
    (replacement expat/fixed)
    (source (let ((dot->underscore (lambda (c) (if (char=? #\. c) #\_ c))))
              (origin
                (method url-fetch)
                (uri (list (string-append "mirror://sourceforge/expat/expat/"
                                          version "/expat-" version ".tar.xz")
                           (string-append
                            "https://github.com/libexpat/libexpat/releases/download/R_"
                            (string-map dot->underscore version)
                            "/expat-" version ".tar.xz")))
                (sha256
                 (base32
                  "1gnwihpfz4x18rwd6cbrdggmfqjzwsdfh1gpmc0ph21c4gq2097g")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'install 'move-static-library
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out    (assoc-ref outputs "out"))
                            (static (assoc-ref outputs "static")))
                        (mkdir-p (string-append static "/lib"))
                        (link (string-append out "/lib/libexpat.a")
                              (string-append static "/lib/libexpat.a"))
                        (delete-file (string-append out "/lib/libexpat.a"))
                        (substitute* (string-append out "/lib/libexpat.la")
                          (("old_library=.*")
                           "old_library=''"))))))))
    (outputs '("out" "static"))
    (home-page "https://libexpat.github.io/")
    (synopsis "Stream-oriented XML parser library written in C")
    (description
     "Expat is an XML parser library written in C.  It is a
stream-oriented parser in which an application registers handlers for
things the parser might find in the XML document (like start tags).")
    (license license:expat)))

(define-public expat/fixed
 (hidden-package
  (package
    (inherit expat)
    (replacement expat/fixed)
    (source (origin
              (inherit (package-source expat))
              (patches (search-patches "expat-CVE-2024-45490.patch"
                                       "expat-CVE-2024-45491.patch"
                                       "expat-CVE-2024-45492.patch")))))))

(define-public libebml
  (package
    (name "libebml")
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.matroska.org/downloads/libebml/"
                           "libebml-" version ".tar.xz"))
       (sha256
        (base32 "06r2md4jysp5q5lx108vgv8b7c596zhh7wr6sk12knlj0l5n8wa9"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=YES")
       #:tests? #f))                    ; no test suite
    (home-page "https://matroska-org.github.io/libebml/")
    (synopsis "C++ library to parse EBML files")
    (description "libebml is a C++ library to read and write @dfn{EBML}
(Extensible Binary Meta Language) files.  EBML was designed to be a simplified
binary extension of XML for the purpose of storing and manipulating data in a
hierarchical form with variable field lengths.")
    (license license:lgpl2.1)))

;; Note: Remember to check python-libxml2 when updating this package.
(define-public libxml2
  (package
    (name "libxml2")
    (version "2.9.14")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/libxml2/"
                                 (version-major+minor version)"/libxml2-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1vnzk33wfms348lgz9pvkq9li7jm44pvm73lbr3w1khwgljlmmv0"))))
    (build-system gnu-build-system)
    (outputs '("out" "static" "doc"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (target-loongarch64?)
                 #~((add-after 'unpack 'update-config-scripts
                      (lambda* (#:key inputs native-inputs #:allow-other-keys)
                        ;; Replace outdated config.guess and config.sub.
                        (for-each (lambda (file)
                                    (install-file
                                     (search-input-file
                                      (or native-inputs inputs)
                                      (string-append "/bin/" file)) "."))
                                  '("config.guess" "config.sub")))))
                 #~())
          (add-after 'install 'use-other-outputs
            (lambda _
              (let ((doc (string-append #$output:doc "/share/"))
                    (static (string-append #$output:static "/lib/")))
                (for-each mkdir-p (list doc static))

                (rename-file (string-append #$output "/share/gtk-doc")
                             (string-append doc "/gtk-doc"))

                (for-each
                 (lambda (ar)
                   (rename-file ar
                                (string-append static (basename ar))))
                 (find-files (string-append #$output "/lib") "\\.a$"))

                ;; Remove reference to the static library from the .la
                ;; file such that Libtool does the right thing when both
                ;; the shared and static variants are available.
                (substitute* (string-append #$output "/lib/libxml2.la")
                  (("^old_library='libxml2.a'") "old_library=''"))))))))
    (home-page "http://www.xmlsoft.org/")
    (synopsis "C parser for XML")
    (inputs (list xz))
    (propagated-inputs (list zlib)) ; libxml2.la says '-lz'.
    (native-inputs (append (if (target-loongarch64?)
                               (list config)
                               '())
                           (list perl)))
    (native-search-paths
     (list $SGML_CATALOG_FILES $XML_CATALOG_FILES))
    (search-paths native-search-paths)
    (description
     "Libxml2 is the XML C parser and toolkit developed for the Gnome
project (but it is usable outside of the Gnome platform).")
    (license license:x11)))

(define-public libxml2-xpath0
  (package/inherit libxml2
    (name "libxml2-xpath0")
    (source (origin
              (inherit (package-source libxml2))
              (patches (append (search-patches
                                "libxml2-xpath0-Add-option-xpath0.patch")
                               (origin-patches (package-source libxml2))))))
    (description
     "Libxml2-xpath0 is like libxml2 but with a patch applied that
provides an @code{--xpath0} option to @command{xmllint} that enables it
to output XPath results with a null delimiter.")))

(define-public python-libxml2
  (package/inherit libxml2
    (name "python-libxml2")
    (source (origin
              (inherit (package-source libxml2))
              (patches
                (append (search-patches "python-libxml2-utf8.patch")
                        (origin-patches (package-source libxml2))))))
    (build-system python-build-system)
    (outputs '("out"))
    (arguments
     (list
      ;; XXX: Tests are specified in 'Makefile.am', but not in 'setup.py'.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (chdir "python")
              (let ((libxml2-headers (search-input-directory
                                      inputs "include/libxml2")))
                (substitute* "setup.py"
                  ;; The build system ignores C_INCLUDE_PATH & co, so
                  ;; provide the absolute directory name.
                  (("/opt/include")
                   (dirname libxml2-headers)))))))))
    (inputs (list libxml2))
    (synopsis "Python bindings for the libxml2 library")))

(define-public libxlsxwriter
  (package
    (name "libxlsxwriter")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
         (url "https://github.com/jmcnamara/libxlsxwriter")
         (commit (string-append "RELEASE_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14c5rgx87nhzasr0j7mcfr1w7ifz0gmdiqy2xq59di5xvcdrpxpv"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled minizip source
        '(begin
           (delete-file-recursively "third_party/minizip")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "USE_STANDARD_TMPFILE=1"
             "USE_SYSTEM_MINIZIP=1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (native-inputs
     (list python-pytest))
    (inputs
     (list minizip))
    (home-page "https://github.com/jmcnamara/libxlsxwriter")
    (synopsis "C library for creating Excel XLSX files")
    (description
     "Libxlsxwriter is a C library that can be used to write text, numbers,
formulas and hyperlinks to multiple worksheets in an Excel 2007+ XLSX file.")
    (license (list license:bsd-2
                   license:public-domain)))) ; third_party/md5

(define-public libxslt
  (package
    (name "libxslt")
    (version "1.1.37")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources"
                                 "/libxslt/" (version-major+minor version)
                                 "/libxslt-" version ".tar.xz"))
             (sha256
              (base32
               "1d1s2bk0m6d7bzml9w90ycl0jlpcy4v07595cwaddk17h3f2fjrs"))
             (patches (search-patches "libxslt-generated-ids.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'disable-fuzz-tests
                 (lambda _
                   ;; Disable libFuzzer tests, because they require
                   ;; instrumentation builds of libxml2 and libxslt.
                   (substitute* "tests/Makefile"
                     (("exslt plugins fuzz")
                      "exslt plugins"))
                   ;; Also disable Python tests since they require
                   ;; python-libxml2 which would introduce a
                   ;; circular dependency.
                   (substitute* "python/Makefile"
                     (("cd tests && \\$\\(MAKE\\) tests")
                      "$(info Python tests are disabled by Guix.)")))))
           #:configure-flags
           (if (%current-target-system)
               ;; 'configure.ac' uses 'AM_PATH_PYTHON', which looks for
               ;; 'python' in $PATH and tries to run it.  Skip all that when
               ;; cross-compiling.
               #~'("--without-python")
               #~'())))
    (home-page "http://xmlsoft.org/XSLT/index.html")
    (synopsis "C library for applying XSLT stylesheets to XML documents")
    (inputs
     (list libgcrypt
           libxml2
           python-minimal-wrapper
           zlib
           xz))
    (native-inputs
     (list pkg-config))
    (native-search-paths %libxslt-search-paths)
    (description
     "Libxslt is an XSLT C library developed for the GNOME project.  It is
based on libxml for XML parsing, tree manipulation and XPath support.")
    (license license:x11)))

(define-public openjade
  (package
    (name "openjade")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/openjade/openjade/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l92sfvx1f0wmkbvzv1385y1gb3hh010xksi1iyviyclrjb7jb8x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--enable-spincludedir="
                            (assoc-ref %build-inputs "opensp")
                            "/include/OpenSP")
             (string-append "--enable-splibdir="
                            (assoc-ref %build-inputs "opensp") "/lib")
             ;; Workaround segfaults in OpenJade (see:
             ;; https://bugs.launchpad.net/ubuntu/+source/openjade/+bug/1869734).
             "CXXFLAGS=-O0")
       #:parallel-build? #f             ;build fails otherwise
       ;; The test suite fails with diff errors between the actual and
       ;; expected results, like: (char<? #\a #\A) returning #t rather than
       ;; #f (see: https://sourceforge.net/p/openjade/bugs/150/).
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-deprecated-getopt
           ;; See: https://sourceforge.net/p/openjade/bugs/140/.
           (lambda _
             (substitute* "msggen.pl"
               (("use POSIX;") "use POSIX;\nuse Getopt::Std;")
               (("do 'getopts.pl';") "")
               (("&Getopts") "getopts"))
             #t))
         (add-after 'replace-deprecated-getopt 'fix-locale-lookup
           ;; See: https://sourceforge.net/p/openjade/bugs/149/.
           (lambda _
             (substitute* "testsuite/expr-lang.dsl"
               (("\\(language \"EN\" \"US\"\\)")
                "(language \"EN\" \"US.UTF-8\")"))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             ;; TODO: Generate the manpage from source, with
             ;; openjade-bootstrap and jadetex.  See the file docsrc/Makefile.
             (let* ((out (assoc-ref outputs "out"))
                    (man1 (string-append out "/share/man/man1")))
               (install-file "docsrc/openjade.1" man1)
               #t)))
         (add-after 'install-doc 'install-dtds
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (dtd (string-append out "/sgml/dtd")))
              (mkdir-p dtd)
              (copy-recursively "dsssl" dtd)
              #t)))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key tests? out #:allow-other-keys)
             (if tests?
                 (with-directory-excursion "testsuite"
                   (invoke "make"))
                 (format #t "test suite not run~%"))
             #t)))))
    (inputs
     (list opensp))
    (native-inputs
     (list perl))
    (home-page "https://openjade.sourceforge.net/")
    (synopsis "ISO/IEC 10179:1996 standard DSSSL language implementation")
    (description "OpenJade is an implementation of Document Style Semantics
and Specification Language (DSSSL), a style language to format SGML or XML
documents.  It contains backends for various formats such as RTF, HTML, TeX,
MIF, SGML2SGML, and FOT.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

(define-public perl-graph-readwrite
  (package
    (name "perl-graph-readwrite")
    (version "2.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/N/NE/NEILB/Graph-ReadWrite-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0jlsg64pmy6ka5q5gy851nnyfgjzvhyxc576bhns3vi2x5ng07mh"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-graph perl-parse-yapp perl-xml-parser perl-xml-writer))
    (home-page "https://metacpan.org/release/Graph-ReadWrite")
    (synopsis "Modules for reading and writing directed graphs")
    (description "This is a collection of perl classes for reading and writing
directed graphs in a variety of file formats.  The graphs are represented in
Perl using Jarkko Hietaniemi's @code{Graph} classes.

There are two base classes. @code{Graph::Reader} is the base class for classes
which read a graph file and create an instance of the Graph class.
@code{Graph::Writer} is the base class for classes which take an instance of
the @code{Graph} class and write it out in a specific file format.")
    (license license:perl-license)))

(define-public perl-xml-atom
  (package
    (name "perl-xml-atom")
    (version "0.43")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                                  "XML-Atom-" version ".tar.gz"))
              (sha256
               (base32
                "0b8bpdnvz9sqwjhjkydbzy4karb7nn6i15b8g4mczrznlsb3hnaf"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-perl-search-path
           (lambda _
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":"
                                    (getenv "PERL5LIB")))
             #t)))))
    (native-inputs
     ;; TODO package: perl-datetime-format-atom
     (list perl-html-tagset perl-module-build-tiny perl-module-install))
    (propagated-inputs
     (list perl-class-data-inheritable
           perl-datetime
           perl-datetime-timezone
           perl-digest-sha1
           perl-libwww
           perl-uri
           perl-xml-libxml
           perl-xml-xpath))
    (home-page "https://metacpan.org/release/XML-Atom")
    (synopsis "Atom feed and API implementation")
    (description
     "Atom is a syndication, API, and archiving format for weblogs and other data.
@code{XML::Atom} implements the feed format as well as a client for the API.")
    (license license:perl-license)))

(define-public perl-xml-descent
  (package
    (name "perl-xml-descent")
    (version "1.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AN/ANDYA/"
                                  "XML-Descent-" version ".tar.gz"))
              (sha256
               (base32
                "0l5xmw2hd95ypppz3lyvp4sn02ccsikzjwacli3ydxfdz1bbh4d7"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-test-differences perl-xml-tokeparser))
    (home-page "https://metacpan.org/release/XML-Descent")
    (synopsis "Recursive descent XML parsing")
    (description
     "The conventional models for parsing XML are either @dfn{DOM}
(a data structure representing the entire document tree is created) or
@dfn{SAX} (callbacks are issued for each element in the XML).

XML grammar is recursive - so it's nice to be able to write recursive
parsers for it.  @code{XML::Descent} allows such parsers to be created.")
    (license license:perl-license)))

(define-public perl-xml-parser
  (package
    (name "perl-xml-parser")
    (version "2.46")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TO/TODDR/XML-Parser-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0pai3ik47q7rgnix9644c673fwydz52gqkxr9kxwq765j4j36cfk"))))
    (build-system perl-build-system)
    (arguments `(#:make-maker-flags
                 (let ((expat (assoc-ref %build-inputs "expat")))
                   (list (string-append "EXPATLIBPATH=" expat "/lib")
                         (string-append "EXPATINCPATH=" expat "/include")))))
    (inputs (list expat))
    (license license:perl-license)
    (synopsis "Perl bindings to the Expat XML parsing library")
    (description
     "This module provides ways to parse XML documents.  It is built on top of
XML::Parser::Expat, which is a lower level interface to James Clark's expat
library.  Each call to one of the parsing methods creates a new instance of
XML::Parser::Expat which is then used to parse the document.  Expat options
may be provided when the XML::Parser object is created.  These options are
then passed on to the Expat object on each parse call.  They can also be given
as extra arguments to the parse methods, in which case they override options
given at XML::Parser creation time.")
    (home-page "https://metacpan.org/release/XML-Parser")))

(define-public perl-xml-tokeparser
  (package
    (name "perl-xml-tokeparser")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/P/PO/PODMASTER/"
                                  "XML-TokeParser-" version ".tar.gz"))
              (sha256
               (base32
                "1hnpwb3lh6cbgwvjjgqzcp6jm4mp612qn6ili38adc9nhkwv8fc5"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-xml-parser))
    (home-page "https://metacpan.org/release/XML-TokeParser")
    (synopsis "Simplified interface to XML::Parser")
    (description
     "@code{XML::TokeParser} provides a procedural (\"pull mode\") interface
to @code{XML::Parser} in much the same way that Gisle Aas'
@code{HTML::TokeParser} provides a procedural interface to @code{HTML::Parser}.
@code{XML::TokeParser} splits its XML input up into \"tokens\", each
corresponding to an @code{XML::Parser} event.")
    (license license:perl-license)))

(define-public perl-libxml
  (package
    (name "perl-libxml")
    (version "0.08")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/K/KM/KMACLEOD/libxml-perl-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1jy9af0ljyzj7wakqli0437zb2vrbplqj4xhab7bfj2xgfdhawa5"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-xml-parser))
    (license license:perl-license)
    (synopsis "Perl modules for working with XML")
    (description
     "A collection of smaller Perl modules, scripts, and documents for working
with XML in Perl.  libxml-perl software works in combination with
@code{XML::Parser}, PerlSAX, @code{XML::DOM}, @code{XML::Grove}, and others.")
    (home-page "https://metacpan.org/release/libxml-perl")))

(define-public perl-xml-libxml
  (package
    (name "perl-xml-libxml")
    (version "2.0134")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "XML-LibXML-" version ".tar.gz"))
       (sha256
        (base32
         "1ks69xymv6zkj7hvaymjvb78ch81abri7kg4zrwxhdfsqb8a9g7h"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-xml-namespacesupport perl-xml-sax))
    (inputs
     (list libxml2))
    (home-page "https://metacpan.org/release/XML-LibXML")
    (synopsis "Perl interface to libxml2")
    (description "This module implements a Perl interface to the libxml2
library which provides interfaces for parsing and manipulating XML files.  This
module allows Perl programmers to make use of the highly capable validating
XML parser and the high performance DOM implementation.")
    (license license:perl-license)))

(define-public perl-xml-libxml-simple
  (package
    (name "perl-xml-libxml-simple")
    (version "1.01")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-LibXML-Simple-" version ".tar.gz"))
              (sha256
               (base32
                "19k50d80i9dipsl6ln0f4awv9wmdg0xm3d16z8mngmvh9c8ci66d"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-file-slurp-tiny perl-xml-libxml))
    (home-page "https://metacpan.org/release/XML-LibXML-Simple")
    (synopsis "XML::LibXML based XML::Simple clone")
    (description
     "This package provides the same API as @code{XML::Simple} but is based on
@code{XML::LibXML}.")
    (license license:perl-license)))

(define-public perl-xml-libxslt
  (package
    (name "perl-xml-libxslt")
    (version "1.96")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "XML-LibXSLT-" version ".tar.gz"))
       (sha256
        (base32
         "0wyl8klgr65j8y8fzgwz9jlvfjwvxazna8j3dg9gksd2v973fpia"))))
    (build-system perl-build-system)
    (inputs
     (list libxslt))
    (propagated-inputs
     (list perl-xml-libxml))
    (home-page "https://metacpan.org/release/XML-LibXSLT")
    (synopsis "Perl bindings to GNOME libxslt library")
    (description "This Perl module is an interface to the GNOME project's
libxslt library.")
    (license license:perl-license)))

(define-public perl-xml-namespacesupport
  (package
    (name "perl-xml-namespacesupport")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PERIGRIN/"
                           "XML-NamespaceSupport-" version ".tar.gz"))
       (sha256
        (base32
         "1vz5pbi4lm5fhq2slrs2hlp6bnk29863abgjlcx43l4dky2rbsa7"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/XML-NamespaceSupport")
    (synopsis "XML namespace support class")
    (description "This module offers a simple to process namespaced XML
names (unames) from within any application that may need them.  It also helps
maintain a prefix to namespace URI map, and provides a number of basic
checks.")
    (license license:perl-license)))

(define-public perl-xml-rss
  (package
    (name "perl-xml-rss")
    (version "1.62")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                                  "XML-RSS-" version ".tar.gz"))
              (sha256
               (base32
                "0klb8ghd405pdkmn25lp3i4j2lfydz8w581sk51p3zy788s0c9yk"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-manifest perl-test-differences
           perl-test-pod perl-test-pod-coverage))
    ;; XXX: The test which uses this modules does not run, even when it is included
    ;; it is ignored. ("perl-test-trailingspace" ,perl-test-trailingspace)
    (inputs
     (list perl-datetime perl-datetime-format-mail
           perl-datetime-format-w3cdtf perl-html-parser perl-xml-parser))
    (home-page "https://metacpan.org/release/XML-RSS")
    (synopsis "Creates and updates RSS files")
    (description
     "This module provides a basic framework for creating and maintaining
RDF Site Summary (RSS) files.  This distribution also contains many examples
that allow you to generate HTML from an RSS, convert between 0.9, 0.91, and
1.0 version, and more.")
    (license license:perl-license)))

(define-public perl-xml-sax
  (package
    (name "perl-xml-sax")
    (version "1.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRANTM/"
                           "XML-SAX-" version ".tar.gz"))
       (sha256
        (base32 "0am13vnv8qsjafr5ljakwnkhlwpk15sga02z8mxsg9is0j3w61j5"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-xml-namespacesupport perl-xml-sax-base))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before
                   'install 'augment-path
                   ;; The install target tries to load the newly-installed
                   ;; XML::SAX module, but can't find it, so we need to tell
                   ;; perl where to look.
                   (lambda* (#:key outputs #:allow-other-keys)
                     (setenv "PERL5LIB"
                             (string-append (getenv "PERL5LIB") ":"
                                            (assoc-ref outputs "out")
                                            "/lib/perl5/site_perl"))
                     #t)))))
    (home-page "https://metacpan.org/release/XML-SAX")
    (synopsis "Perl API for XML")
    (description "XML::SAX consists of several framework classes for using and
building Perl SAX2 XML parsers, filters, and drivers.")
    (license license:perl-license)))

(define-public perl-xml-sax-base
  (package
    (name "perl-xml-sax-base")
    (version "1.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRANTM/"
                           "XML-SAX-Base-" version ".tar.gz"))
       (sha256
        (base32
         "1l1ai9g1z11ja7mvnfl5mj346r13jyckbg9qlw6c2izglidkbjv6"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/XML-SAX-Base")
    (synopsis "Base class for SAX Drivers and Filters")
    (description "This module has a very simple task - to be a base class for
PerlSAX drivers and filters.  Its default behaviour is to pass the input
directly to the output unchanged.  It can be useful to use this module as a
base class so you don't have to, for example, implement the characters()
callback.")
    (license license:perl-license)))

(define-public perl-xml-simple
  (package
    (name "perl-xml-simple")
    (version "2.25")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GR/GRANTM/XML-Simple-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1y6vh328zrh085d40852v4ij2l4g0amxykswxd1nfhd2pspds7sk"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-xml-parser perl-xml-sax))
    (license license:perl-license)
    (synopsis "Perl module for easy reading/writing of XML files")
    (description
     "The XML::Simple module provides a simple API layer on top of an
underlying XML parsing module (either XML::Parser or one of the SAX2
parser modules).")
    (home-page "https://metacpan.org/release/XML-Simple")))

(define-public perl-xml-regexp
  (package
    (name "perl-xml-regexp")
    (version "0.04")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TJ/TJMATHER/XML-RegExp-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0m7wj00a2kik7wj0azhs1zagwazqh3hlz4255n75q21nc04r06fz"))))
    (build-system perl-build-system)
    (inputs
     (list perl-xml-parser))
    (license license:perl-license)
    (synopsis "Perl regular expressions for XML tokens")
    (description
     "XML::RegExp contains regular expressions for the following XML tokens:
BaseChar, Ideographic, Letter, Digit, Extender, CombiningChar, NameChar,
EntityRef, CharRef, Reference, Name, NmToken, and AttValue.")
    (home-page "https://metacpan.org/release/XML-RegExp")))

(define-public perl-xml-dom
  (package
    (name "perl-xml-dom")
    (version "1.46")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TJ/TJMATHER/XML-DOM-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0phpkc4li43m2g44hdcvyxzy9pymqwlqhh5hwp2xc0cv8l5lp8lb"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-libwww perl-libxml perl-xml-parser perl-xml-regexp))
    (license license:perl-license)
    (synopsis
     "Perl module for building DOM Level 1 compliant document structures")
    (description
     "This module extends the XML::Parser module by Clark Cooper.  The
XML::Parser module is built on top of XML::Parser::Expat, which is a lower
level interface to James Clark's expat library.  XML::DOM::Parser is derived
from XML::Parser.  It parses XML strings or files and builds a data structure
that conforms to the API of the Document Object Model.")
    (home-page "https://metacpan.org/release/XML-DOM")))

(define-public perl-xml-compile-tester
  (package
    (name "perl-xml-compile-tester")
    (version "0.91")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-Tester-" version ".tar.gz"))
              (sha256
               (base32
                "1drzwziwi96rfkh48qpw4l225mcbk8ppl2157nj92cslcpwwdk75"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-log-report perl-test-deep))
    (home-page "https://metacpan.org/release/XML-Compile-Tester")
    (synopsis "XML::Compile related regression testing")
    (description
     "The @code{XML::Compile} module suite has extensive regression testing.
This module provide functions which simplify writing tests for
@code{XML::Compile} related distributions.")
    (license license:perl-license)))

(define-public perl-xml-compile
  (package
    (name "perl-xml-compile")
    (version "1.63")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-" version ".tar.gz"))
              (sha256
               (base32
                "0psr5pwsk2biz2bfkigmx04v2rfhs6ybwcfmcrrg7gvh9bpp222b"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-log-report
           perl-xml-compile-tester
           perl-xml-libxml
           perl-scalar-list-utils
           perl-test-deep
           perl-types-serialiser))
    (home-page "https://metacpan.org/release/XML-Compile")
    (synopsis "Compilation-based XML processing")
    (description
     "@code{XML::Compile} can be used to translate a Perl data-structure into
XML or XML into a Perl data-structure, both directions under rigid control by
a schema.")
    (license license:perl-license)))

(define-public perl-xml-compile-cache
  (package
    (name "perl-xml-compile-cache")
    (version "1.06")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-Cache-" version ".tar.gz"))
              (sha256
               (base32
                "181qf1s7ymgi7saph3cf9p6dbxkxyh1ja23na4dchhi8v5mi66sr"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-log-report perl-xml-compile perl-xml-compile-tester
           perl-xml-libxml-simple))
    (home-page "https://metacpan.org/release/XML-Compile-Cache")
    (synopsis "Cache compiled XML translators")
    (description
     "This package provides methods to cache compiled XML translators.")
    (license license:perl-license)))

(define-public perl-xml-compile-soap
  (package
    (name "perl-xml-compile-soap")
    (version "3.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-SOAP-" version ".tar.gz"))
              (sha256
               (base32
                "1a3650al287x781i3flylwbik1ss3xfw7sgdcaz5qrjqvhpn6mnn"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-file-slurper
           perl-libwww
           perl-log-report
           perl-xml-compile
           perl-xml-compile-cache
           perl-xml-compile-tester))
    (home-page "https://metacpan.org/release/XML-Compile-SOAP")
    (synopsis "Base-class for SOAP implementations")
    (description
     "This module provides a class to handle the SOAP protocol.  The first
implementation is @url{SOAP1.1,
http://www.w3.org/TR/2000/NOTE-SOAP-20000508/}, which is still most often
used.")
    (license license:perl-license)))

(define-public perl-xml-compile-wsdl11
  (package
    (name "perl-xml-compile-wsdl11")
    (version "3.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-WSDL11-" version ".tar.gz"))
              (sha256
               (base32
                "09ayl442hzvn97q4ghn5rz4r82dm9w3l69hixhb29h9xq9ysi7ba"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-log-report perl-xml-compile perl-xml-compile-cache
           perl-xml-compile-soap))
    (home-page "https://metacpan.org/release/XML-Compile-WSDL11")
    (synopsis "Create SOAP messages defined by WSDL 1.1")
    (description
     "This module understands WSDL version 1.1.  A WSDL file defines a set of
messages to be send and received over SOAP connections.  This involves
encoding of the message to be send into XML, sending the message to the
server, collect the answer, and finally decoding the XML to Perl.")
    (license license:perl-license)))

(define-public perl-xml-feed
  (package
    (name "perl-xml-feed")
    (version "0.63")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DA/DAVECROSS/"
                                  "XML-Feed-" version ".tar.gz"))
              (sha256
               (base32
                "04frqhikmyq0i9ldraisbvppyjhqg6gz83l2rqpmp4f2h9n9k2lw"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-uri perl-class-data-inheritable))
    (propagated-inputs
     (list perl-class-errorhandler
           perl-datetime
           perl-datetime-format-flexible
           perl-datetime-format-iso8601
           perl-datetime-format-mail
           perl-datetime-format-natural
           perl-datetime-format-w3cdtf
           perl-feed-find
           perl-html-parser
           perl-libwww
           perl-module-pluggable
           perl-uri-fetch
           perl-xml-atom
           perl-xml-libxml
           perl-xml-rss))
    (home-page "https://metacpan.org/release/XML-Feed")
    (synopsis "XML Syndication Feed Support")
    (description "@code{XML::Feed} is a syndication feed parser for both RSS and
Atom feeds.  It also implements feed auto-discovery for finding feeds, given a URI.
@code{XML::Feed} supports the following syndication feed formats:
RSS 0.91, RSS 1.0, RSS 2.0, Atom")
    (license license:perl-license)))

(define-public perl-xml-xpath
  (package
    (name "perl-xml-xpath")
    (version "1.48")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MANWAR/"
                                  "XML-XPath-" version ".tar.gz"))
              (sha256
               (base32
                "1kch6w4zk7rzfimbwakz8qyhjhrvnp97158af0p5p7i3dgimpivv"))))
    (build-system perl-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-xpath
            (lambda _
              (let ((xpath (string-append #$output "/bin/xpath"))
                    (perl5lib
                     (search-path-as-list
                      '("/lib/perl5/site_perl")
                      (list #$(this-package-input "perl-xml-parser")
                            #$output))))
                (wrap-program xpath
                  `("PERL5LIB" ":" prefix ,perl5lib)))))
          (add-after 'wrap-xpath 'check-wrap
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (unsetenv "PERL5LIB")
                (invoke/quiet (string-append #$output "/bin/xpath"))))))))
    (native-inputs
     (list perl-path-tiny perl-test-leaktrace))
    (propagated-inputs
     (list perl-xml-parser))
    (home-page "https://metacpan.org/release/XML-XPath")
    (synopsis "Parse and evaluate XPath statements")
    (description
     "This module aims to comply exactly to the @url{XPath specification,
https://www.w3.org/TR/xpath} and yet allow extensions to be added in
the form of functions.  It also provides the command @command{xpath}.")
    (license license:perl-license)))

(define-public pugixml
  (package
    (name "pugixml")
    (version "1.12.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/zeux/pugixml/releases/download/v"
                          version "/pugixml-" version ".tar.gz"))
      (sha256
       (base32 "1ixg6fpr7vhkg9bn2g2qmmwpy974z7nx7zq81whm2h6c36lp3xnw"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:tests? #f))                    ; no tests
    (native-inputs
     (list pkg-config))
    (home-page "https://pugixml.org")
    (synopsis "Light-weight, simple and fast XML parser for C++ with XPath support")
    (description
     "pugixml is a C++ XML processing library, which consists of a DOM-like
interface with rich traversal/modification capabilities, a fast XML parser
which constructs the DOM tree from an XML file/buffer, and an XPath 1.0
implementation for complex data-driven tree queries.  Full Unicode support is
also available, with Unicode interface variants and conversions between
different Unicode encodings which happen automatically during
parsing/saving.")
    (license license:expat)))

(define-public python-pyxb-x
  (package
    (name "python-pyxb-x")
    (version "1.2.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyxb_x" version))
       (sha256
        (base32 "1d9p42aklk0w5yy39p319h5ldvy7glng0jcgcjk6xgg6sfl1yh5z"))))
    (build-system pyproject-build-system)
    (arguments
     ;; XXX: tests FAILED (failures=3, errors=122)
     (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "http://pyxb.sourceforge.net")
    (synopsis "Python XML Schema Bindings")
    (description
     "@code{PyXB-X} (\"pixbix\") is a pure Python package that generates Python source
code for classes that correspond to data structures defined by XMLSchema.")
    (license (list license:asl2.0    ; Most files.
                   license:expat     ; pyxb/utils/six.py
                   license:gpl2      ; bundled jquery in doc is dual MIT/GPL2
                   license:psfl))))  ; pyxb/utils/activestate.py

(define-public xmlto
  (package
    (name "xmlto")
    (version "0.0.28")
    (source
     (origin
      (method url-fetch)
      ;; The old source on fedorahosted.org is offline permanently:
      ;; <https://bugs.gnu.org/25989>
      (uri (string-append "mirror://debian/pool/main/x/xmlto/"
                          "xmlto_" version ".orig.tar.bz2"))
      (file-name (string-append name "-" version ".tar.bz2"))
      (sha256
       (base32
        "0xhj8b2pwp4vhl9y16v3dpxpsakkflfamr191mprzsspg4xdyc0i"))))
    (build-system gnu-build-system)
    (arguments
     ;; Make sure the reference to util-linux's 'getopt' is kept in 'xmlto'.
     (list
      #:configure-flags
      #~(list "CFLAGS=-g -O2 -Wno-error=implicit-int"
              (string-append "GETOPT="
                             #$(this-package-input "util-linux")
                             "/bin/getopt"))))
    (native-inputs
     (list util-linux))
    (inputs
     (list util-linux ; for 'getopt'
           libxml2 ; for 'xmllint'
           libxslt))                     ; for 'xsltproc'
    (native-search-paths %libxslt-search-paths)
    (home-page "http://cyberelk.net/tim/software/xmlto/")
    (synopsis "Front-end to an XSL toolchain")
    (description
     "Xmlto is a front-end to an XSL toolchain.  It chooses an appropriate
stylesheet for the conversion you want and applies it using an external
XSL-T processor.  It also performs any necessary post-processing.")
    (license license:gpl2+)))

(define-public xmlsec
  (package
    (name "xmlsec")
    (version "1.2.37")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.aleksey.com/xmlsec/download/"
                                  "xmlsec1-" version ".tar.gz"))
              (sha256
               (base32
                "0747w8mnnyawvvzlvhjpkwm3998c7l5f1hjy1gfvsmhydp5zp3az"))))
    (build-system gnu-build-system)
    (propagated-inputs                  ; according to xmlsec1.pc
     (list libxml2 libxslt))
    (inputs
     (list gnutls libgcrypt libltdl))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.aleksey.com/xmlsec/")
    (synopsis "XML Security Library")
    (description
     "The XML Security Library is a C library based on Libxml2.  It
supports XML security standards such as XML Signature, XML Encryption,
Canonical XML (part of Libxml2) and Exclusive Canonical XML (part of
Libxml2).")
    (properties
     '((upstream-name . "xmlsec1")))
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))))

(define-public xmlsec-nss
  (package/inherit xmlsec
    (name "xmlsec-nss")
    (native-inputs
     (modify-inputs (package-native-inputs xmlsec)
       (prepend `(,nss "bin"))))        ;certutil, for tests
    (inputs
     (list nss libltdl))
    (arguments
     ;; NSS no longer supports MD5 since 3.59, don't attempt to use it.
     '(#:configure-flags '("--disable-md5")))
    (synopsis "XML Security Library (using NSS instead of GnuTLS)")))

(define-public xmlsec-openssl
  (package/inherit xmlsec
    (name "xmlsec-openssl")
    (inputs
     (list openssl libltdl))
    (synopsis "XML Security Library (using OpenSSL instead of GnuTLS)")))

(define-public minixml
  (package
    (name "minixml")
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/michaelrsweet/mxml/"
                                  "releases/download/v" version
                                  "/mxml-" version ".tar.gz"))
              (sha256
               (base32
                "0cncvb0xhbq2i7rszj6pmcs3b97f0a17j081z0cmcfrrzv8kwrhc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
      #:tests? #f))                    ; tests are run during build
    (home-page "https://www.msweet.org/mxml/")
    (synopsis "Small XML parsing library")
    (description
     "Mini-XML is a small C library to read and write XML files and strings in
UTF-8 and UTF-16 encoding.")
    ;; LGPL 2.0+ with additional exceptions for static linking
    (license license:lgpl2.0+)))

;; TinyXML is an unmaintained piece of software, so the patches and build
;; system massaging have no upstream potential.
(define-public tinyxml
  (package
    (name "tinyxml")
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/tinyxml/tinyxml/"
                                  version "/tinyxml_"
                                  (string-join (string-split version #\.) "_")
                                  ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14smciid19lvkxqznfig77jxn5s4iq3jpb47vh5a6zcaqp7gvg8m"))
              (patches (search-patches "tinyxml-use-stl.patch"
                                       "tinyxml-CVE-2023-34194.patch"))))
    (build-system gnu-build-system)
    ;; This library is missing *a lot* of the steps to make it usable, so we
    ;; have to add them here, like every other distro must do.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'build-shared-library
           (lambda _
             (invoke ,(cxx-for-target) "-Wall" "-O2" "-shared" "-fpic"
                     "tinyxml.cpp" "tinyxmlerror.cpp"
                     "tinyxmlparser.cpp" "tinystr.cpp"
                     "-o" "libtinyxml.so")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./xmltest"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include (string-append out "/include"))
                    (lib (string-append out "/lib"))
                    (pkgconfig (string-append out "/lib/pkgconfig"))
                    (doc (string-append out "/share/doc")))
               ;; Install libs and headers.
               (install-file "libtinyxml.so" lib)
               (install-file "tinystr.h" include)
               (install-file "tinyxml.h" include)
               ;; Generate and install pkg-config file.
               (mkdir-p pkgconfig)
               ;; Software such as Kodi expect this file to be present, but
               ;; it's not provided in the source code.
               (call-with-output-file (string-append pkgconfig "/tinyxml.pc")
                 (lambda (port)
                   (format port "prefix=~a
exec_prefix=${prefix}
libdir=${exec_prefix}/lib
includedir=${prefix}/include

Name: TinyXML
Description: A simple, small, C++ XML parser
Version: ~a
Libs: -L${libdir} -ltinyxml
Cflags: -I${includedir}
"
                           out ,version)))
               ;; Install docs.
               (mkdir-p doc)
               (copy-recursively "docs" (string-append doc "tinyxml"))
               #t))))))
    (synopsis "Small XML parser for C++")
    (description "TinyXML is a small and simple XML parsing library for the
C++ programming language.")
    (home-page "http://www.grinninglizard.com/tinyxml/index.html")
    (license license:zlib)))

(define-public tinyxml2
  (package
    (name "tinyxml2")
    (version "8.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leethomason/tinyxml2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0raa8r2hsagk7gjlqjwax95ib8d47ba79n91r4aws2zg8y6ssv1d"))))
    (build-system cmake-build-system)
    (synopsis "Small XML parser for C++")
    (description "TinyXML2 is a small and simple XML parsing library for the
C++ programming language.")
    (home-page "http://www.grinninglizard.com/tinyxml2/")
    (license license:zlib)))

(define-public xmlstarlet
 (package
   (name "xmlstarlet")
   (version "1.6.1")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/xmlstar/xmlstarlet/"
                          version "/xmlstarlet-" version ".tar.gz"))
      (sha256
       (base32
        "1jp737nvfcf6wyb54fla868yrr39kcbijijmjpyk4lrpyg23in0m"))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases
     (modify-phases %standard-phases
       (add-before 'check 'drop-failing-tests
         (lambda _
           ;; FIXME: Why are these tests failing.
           (substitute* "Makefile"
             (("^examples/schema1\\\\") "\\")
             (("^examples/valid1\\\\") "\\"))
           #t))
       (add-after 'install 'symlink-xmlstarlet
         (lambda* (#:key outputs #:allow-other-keys)
           ;; Other distros usually either rename or symlink the `xml' binary
           ;; as `xmlstarlet', let's do it as well for compatibility.
           (let* ((out (assoc-ref outputs "out"))
                  (bin (string-append out "/bin")))
             (symlink "xml" (string-append bin "/xmlstarlet"))
             #t))))))
   (inputs
    (list libxslt libxml2))
   (home-page "https://xmlstar.sourceforge.net/")
   (synopsis "Command line XML toolkit")
   (description "XMLStarlet is a set of command line utilities which can be
used to transform, query, validate, and edit XML documents.  XPath is used to
match and extract data, and elements can be added, deleted or modified using
XSLT and EXSLT.")
   (license license:x11)))

(define-public html-xml-utils
 (package
   (name "html-xml-utils")
   (version "8.6")
   (source
    (origin
      (method url-fetch)
      (uri (string-append
            "https://www.w3.org/Tools/HTML-XML-utils/html-xml-utils-"
            version ".tar.gz"))
      (sha256
       (base32 "1cjgvgrg3bjfxfl0nh55vhr37ijld7pd8bl7s8j3kkbcyfg7512y"))))
   (build-system gnu-build-system)
   (home-page "https://www.w3.org/Tools/HTML-XML-utils/")
   (synopsis "Command line utilities to manipulate HTML and XML files")
   (description "HTML-XML-utils provides a number of simple utilities for
manipulating and converting HTML and XML files in various ways.  The suite
consists of the following tools:

@itemize
 @item @command{asc2xml} convert from @code{UTF-8} to @code{&#nnn;} entities
 @item @command{xml2asc} convert from @code{&#nnn;} entities to @code{UTF-8}
 @item @command{hxaddid} add IDs to selected elements
 @item @command{hxcite} replace bibliographic references by hyperlinks
 @item @command{hxcite} mkbib - expand references and create bibliography
 @item @command{hxclean} apply heuristics to correct an HTML file
 @item @command{hxcopy} copy an HTML file while preserving relative links
 @item @command{hxcount} count elements and attributes in HTML or XML files
 @item @command{hxextract} extract selected elements
 @item @command{hxincl} expand included HTML or XML files
 @item @command{hxindex} create an alphabetically sorted index
 @item @command{hxmkbib} create bibliography from a template
 @item @command{hxmultitoc} create a table of contents for a set of HTML files
 @item @command{hxname2id} move some @code{ID=} or @code{NAME=} from A
elements to their parents
 @item @command{hxnormalize} pretty-print an HTML file
 @item @command{hxnsxml} convert output of hxxmlns back to normal XML
 @item @command{hxnum} number section headings in an HTML file
 @item @command{hxpipe} convert XML to a format easier to parse with Perl or AWK
 @item @command{hxprintlinks} number links and add table of URLs at end of an HTML file
 @item @command{hxprune} remove marked elements from an HTML file
 @item @command{hxref} generate cross-references
 @item @command{hxselect} extract elements that match a (CSS) selector
 @item @command{hxtoc} insert a table of contents in an HTML file
 @item @command{hxuncdata} replace CDATA sections by character entities
 @item @command{hxunent} replace HTML predefined character entities to @code{UTF-8}
 @item @command{hxunpipe} convert output of pipe back to XML format
 @item @command{hxunxmlns} replace \"global names\" by XML Namespace prefixes
 @item @command{hxwls} list links in an HTML file
 @item @command{hxxmlns} replace XML Namespace prefixes by \"global names\"
@end itemize
")
   (license license:expat)))

(define-public xlsx2csv
  (package
    (name "xlsx2csv")
    (version "0.7.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dilshod/xlsx2csv")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p10571295f8zw1lsma8k5z07hrk9aspar0lsz8zpgjl7v35zcq7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (substitute* "test/run"
               ;; Run tests with `python' only.
               (("^(PYTHON_VERSIONS = ).*" all m)
                (string-append m "['']")))
             (when tests?
               (invoke "test/run")))))))
    (home-page "https://github.com/dilshod/xlsx2csv")
    (synopsis "XLSX to CSV converter")
    (description
     "Xlsx2csv is a program to convert Microsoft Excel 2007 XML (XLSX and
XLSM) format spreadsheets into plaintext @dfn{comma separated values} (CSV)
files.  It is designed to be fast and to handle large input files.")
    (license license:gpl2+)))

(define-public python-defusedxml
  (package
    (name "python-defusedxml")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "defusedxml" version))
       (sha256
        (base32 "0s9ym98jrd819v4arv9gmcr6mgljhxd9q866sxi5p4c5n4nh7cqv"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/tiran/defusedxml")
    (native-inputs
     (list python-setuptools
           python-wheel))
    (synopsis "XML bomb protection for Python stdlib modules")
    (description
     "Defusedxml provides XML bomb protection for Python stdlib modules.")
    (license license:psfl)))

(define-public freexl
  (package
    (name "freexl")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.gaia-gis.it/gaia-sins/"
                                  "freexl-sources/"
                                  "freexl-" version ".tar.gz"))
              (sha256
               (base32
                "1w57w73gfj2niz9dn235hn5wsvxpdbj6sp5zxcg7rasqvvqharqp"))))
    (build-system gnu-build-system)
    (inputs
     (list expat minizip))
    (home-page "https://www.gaia-gis.it/fossil/freexl/index")
    (synopsis "Read Excel files")
    (description
     "FreeXL is a C library to extract valid data from within an Excel
(.xls, .xlsx) or LibreOffice (.ods) spreadsheet.")
    ;; Any of these licenses may be picked.
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:mpl1.1))))

(define-public xerces-c
  (package
    (name "xerces-c")
    (version "3.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xerces/c/3/sources/"
                                  "xerces-c-" version ".tar.xz"))
              (sha256
               (base32
                "0c42jhnhq63yzvj8whl5dpzf7p1lnd6h00kzpz4ipcj5aq1ycfb2"))))
    (build-system gnu-build-system)
    (arguments
     (let ((system (or (%current-target-system)
                       (%current-system))))
       (if (string-prefix? "x86_64" system)
           '()
           '(#:configure-flags '("--disable-sse2")))))
    (native-inputs
     (list perl))
    (home-page "https://xerces.apache.org/xerces-c/")
    (synopsis "Validating XML parser library for C++")
    (description "Xerces-C++ is a validating XML parser written in a portable
subset of C++.  Xerces-C++ makes it easy to give your application the ability
to read and write XML data.  A shared library is provided for parsing,
generating, manipulating, and validating XML documents using the DOM, SAX, and
SAX2 APIs.")
    (license license:asl2.0)))

(define-public xlsxio
  (package
    (name "xlsxio")
    (version "0.2.35")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brechtsanders/xlsxio")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "140ap2l3qy27z1fhqpkq3a44aikhr3v5zlnm9m8vag42qiagiznx"))))
    (native-inputs
     (list expat gnu-make minizip which))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check))))
    (synopsis "C library for reading and writing .xlsx files")
    (description "XLSX I/O aims to provide a C library for reading and writing
.xlsx files.  The .xlsx file format is the native format used by Microsoft(R)
Excel(TM) since version 2007.")
    (home-page "https://github.com/brechtsanders/xlsxio")
    (license license:expat)))

(define-public perl-xml-xpathengine
  (package
    (name "perl-xml-xpathengine")
    (version "0.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIROD/"
                                  "XML-XPathEngine-" version ".tar.gz"))
              (sha256
               (base32
                "0r72na14bmsxfd16s9nlza155amqww0k8wsa9x2a3sqbpp5ppznj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/XML-XPathEngine")
    (synopsis "Re-usable XPath engine for DOM-like trees")
    (description
     "This module provides an XPath engine, that can be re-used by other
modules/classes that implement trees.

In order to use the XPath engine, nodes in the user module need to mimic DOM
nodes.  The degree of similitude between the user tree and a DOM dictates how
much of the XPath features can be used.  A module implementing all of the DOM
should be able to use this module very easily (you might need to add the
@code{cmp} method on nodes in order to get ordered result sets).")
    (license license:perl-license)))

(define-public perl-tree-xpathengine
  (package
    (name "perl-tree-xpathengine")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIROD/"
                                  "Tree-XPathEngine-" version ".tar.gz"))
              (sha256
               (base32
                "1vbbw8wxm79r3xbra8narw1dqvm34510q67wbmg2zmj6zd1k06r9"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Tree-XPathEngine")
    (synopsis "Re-usable XPath engine")
    (description
     "This module provides an XPath engine, that can be re-used by other
module/classes that implement trees.  It is designed to be compatible with
@code{Class::XPath}, ie it passes its tests if you replace @code{Class::XPath}
by @code{Tree::XPathEngine}.")
    (license license:perl-license)))

(define-public perl-xml-filter-buffertext
  (package
    (name "perl-xml-filter-buffertext")
    (version "1.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RB/RBERJON/"
                           "XML-Filter-BufferText-" version ".tar.gz"))
       (sha256
        (base32
         "0p5785c1dsk6kdp505vapb5h54k8krrz8699hpgm9igf7dni5llg"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-xml-sax-base))
    (home-page "https://metacpan.org/release/XML-Filter-BufferText")
    (synopsis "Filter to put all characters() in one event")
    (description "This is a very simple filter.  One common cause of
grief (and programmer error) is that XML parsers aren't required to provide
character events in one chunk.  They can, but are not forced to, and most
don't.  This filter does the trivial but oft-repeated task of putting all
characters into a single event.")
    (license license:perl-license)))

(define-public perl-xml-sax-writer
  (package
    (name "perl-xml-sax-writer")
    (version "0.57")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/P/PE/PERIGRIN/"
                    "XML-SAX-Writer-" version ".tar.gz"))
              (sha256
               (base32
                "1w1cd1ybxdvhmnxdlkywi3x5ka3g4md42kyynksjc09vyizd0q9x"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-libxml perl-xml-filter-buffertext
           perl-xml-namespacesupport perl-xml-sax-base))
    (home-page "https://metacpan.org/release/XML-SAX-Writer")
    (synopsis "SAX2 XML Writer")
    (description
     "This is an XML writer that understands SAX2.  It is based on
@code{XML::Handler::YAWriter}.")
    (license license:perl-license)))

(define-public perl-xml-handler-yawriter
  (package
    (name "perl-xml-handler-yawriter")
    (version "0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KR/KRAEHE/"
                           "XML-Handler-YAWriter-" version ".tar.gz"))
       (sha256
        (base32
         "11d45a1sz862va9rry3p2m77pwvq3kpsvgwhc5ramh9mbszbnk77"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-libxml))
    (home-page "https://metacpan.org/release/XML-Handler-YAWriter")
    (synopsis "Yet another Perl SAX XML Writer")
    (description "YAWriter implements Yet Another @code{XML::Handler::Writer}.
It provides a flexible escaping technique and pretty printing.")
    (license license:perl-license)))

(define-public perl-xml-twig
  (package
    (name "perl-xml-twig")
    (version "3.52")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIROD/"
                                  "XML-Twig-" version ".tar.gz"))
              (sha256
               (base32
                "1bc0hrz4jp6199hi29sdxmb9gyy45whla9hd19yqfasgq8k5ixzy"))))
    (build-system perl-build-system)
    (inputs
     (list expat))
    (propagated-inputs
     (list perl-html-tidy
           perl-html-tree
           perl-io-captureoutput
           perl-io-string
           perl-io-stringy
           perl-libxml
           perl-xml-filter-buffertext
           perl-xml-handler-yawriter
           perl-xml-parser
           perl-xml-sax-writer
           perl-xml-simple
           perl-xml-xpathengine
           perl-test-pod
           perl-tree-xpathengine))
    (home-page "https://metacpan.org/release/XML-Twig")
    (synopsis "Perl module for processing huge XML documents in tree mode")
    (description "@code{XML::Twig} is an XML transformation module.  Its
strong points: can be used to process huge documents while still being in tree
mode; not bound by DOM or SAX, so it is very perlish and offers a very
comprehensive set of methods; simple to use; DWIMs as much as possible.

What it doesn't offer: full SAX support (it can export SAX, but only reads
XML), full XPath support (unless you use @code{XML::Twig::XPath}), nor DOM
support.")
    (license license:perl-license)))

(define-public xmlrpc-c
  (package
    (name "xmlrpc-c")
    (version "1.43.08")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/xmlrpc-c/Xmlrpc-c%20Super%20Stable/"
                                 version "/xmlrpc-c-" version ".tgz"))
             (sha256
              (base32
               "18zwbj6i2hpcn5riiyp8i6rml0sfv60dd7phw1x8g4r4lj2bbxf9"))))
    (build-system gnu-build-system)
    (inputs
     (list curl))
    (native-inputs
     (list ;; For tools, if ever needed.
           perl))
    (arguments
     `(#:make-flags ; Add $libdir to the RUNPATH of all the executables.
       (list (string-append "LDFLAGS_PERSONAL=-Wl,-rpath=" %output "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-/bin/sh-in-tests
           (lambda _
             (substitute* "GNUmakefile"
               (("#! /bin/sh") (which "sh")))
             #t)))))
    (home-page "https://xmlrpc-c.sourceforge.net/")
    (synopsis "Lightweight RPC library based on XML and HTTP")
    (description
     "XML-RPC is a quick-and-easy way to make procedure calls over the Internet.
It converts the procedure call into an XML document, sends it to a remote
server using HTTP, and gets back the response as XML.  This library provides a
modular implementation of XML-RPC for C and C++.")
    (license (list license:psfl license:expat))))

(define-public opensp
  (package
    (name "opensp")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/openjade/opensp/"
                                  version "/OpenSP-" version ".tar.gz"))
              (sha256
               (base32
                "1khpasr6l0a8nfz6kcf3s81vgdab8fm2dj291n5r2s53k228kx2p"))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (native-inputs
     (list docbook-xml-4.1.2
           docbook-xsl
           xmlto
           ;; Dependencies to regenerate the 'configure' script.
           autoconf
           automake
           gettext-minimal
           libtool))
    (arguments
     (list
      ;; Note: we cannot use '--enable-full-doc-build' as this would require
      ;; Openjade, which in turn requires this package.

      ;; Skip the tests that are known to fail (see:
      ;; https://sourceforge.net/p/openjade/mailman/message/6182316/)
      #:make-flags #~(list "TESTS_THAT_FAIL=")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-configure
            ;; The configure script in the release was made with an older
            ;; Autoconf and lacks support for the `--docdir' option.
            (lambda _
              (delete-file "configure")))
          (add-after 'delete-configure 'honor-docdir
            ;; docdir is not honored due to being hardcoded in the various
            ;; Makefile.am (see: https://sourceforge.net/p/openjade/bugs/147/).
            (lambda _
              (substitute* '("Makefile.am" "doc/Makefile.am" "docsrc/Makefile.am")
                (("^docdir = .*") "docdir = @docdir@\n"))))
          (add-after 'delete-configure 'fix-tests-makefile.am
            ;; Remove the trailing $(SHELL) from the TESTS_ENVIRONMENT variable
            ;; definition. Otherwise, when targets are built using
            ;; "$(am__check_pre) $(LOG_DRIVER) [...]", there would be two
            ;; $(SHELL) expansion which fails the build.
            (lambda _
              (substitute* "tests/Makefile.am"
                (("^\tOSGMLNORM=`echo osgmlnorm\\|sed '\\$\\(transform\\)'`\\\\")
                 "\tOSGMLNORM=`echo osgmlnorm|sed '$(transform)'`")
                (("^\t\\$\\(SHELL\\)\n") "")))))))
    (native-search-paths (list $SGML_CATALOG_FILES))
    (home-page "https://openjade.sourceforge.net/")
    (synopsis "Suite of SGML/XML processing tools")
    (description "OpenSP is an object-oriented toolkit for SGML parsing and
entity management.  It is a fork of James Clark's SP suite.  The tools it
contains can be used to parse, validate, and normalize SGML and XML files.
The central program included in this package is @code{onsgmls}, which replaces
@code{sgmls}, @code{ospam}, @code{ospent}, @code{osgmlnorm}, and @code{osx}.")
    (license
     ;; expat license with added clause regarding advertising
     (license:non-copyleft
      "file://COPYING"
      "See COPYING in the distribution."))))

(define-public python-elementpath
  (package
    (name "python-elementpath")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "elementpath" version))
       (sha256
        (base32
         "1kxx573ywqfh6j6aih2i6hhsya6kz79qq4bgz6yskwk6b18jyr8z"))))
    (build-system python-build-system)
    ;; The test suite is not run, to avoid a dependency cycle with
    ;; python-xmlschema.
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/sissaschool/elementpath")
    (synopsis
     "XPath 1.0/2.0 parsers and selectors for ElementTree and lxml")
    (description
     "The proposal of this package is to provide XPath 1.0 and 2.0 selectors
for Python's ElementTree XML data structures, both for the standard
ElementTree library and for the @uref{http://lxml.de, lxml.etree} library.

For lxml.etree this package can be useful for providing XPath 2.0 selectors,
because lxml.etree already has its own implementation of XPath 1.0.")
    (license license:expat)))

(define-public python-lxml
  (package
    (name "python-lxml")
    (version "5.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lxml" version))
       (sha256
        (base32 "11yvrzlswlh81z6lpmds2is2jd3wkigpwj6mcfcaggl0h64w8bdv"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "make" "test")))))))
    (inputs
     (list libxml2 libxslt))
    (home-page "https://lxml.de/")
    (synopsis "Python XML processing library")
    (description
     "The lxml XML toolkit is a Pythonic binding for the C libraries
libxml2 and libxslt.")
    (license license:bsd-3))) ; and a few more, see LICENSES.txt

(define-deprecated python-lxml-4.7 python-lxml)
(export python-lxml-4.7)

(define-public python-untangle
  ;; The latest tagged release is from 2014; use the latest commit.
  (let ((revision "1")
        (commit "fb916a9621175d000a3b0ca9322d3b3ebf8570c0"))
    (package
      (name "python-untangle")
      ;; PyPI currently offers some untagged 1.1.1 version.
      (version (git-version "1.1.1" revision commit))
      (source
       (origin
         (method git-fetch)             ;no tests in pypi archive
         (uri (git-reference
               (url "https://github.com/stchris/untangle")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0dn2jz9ajncbqx3pdlgqaxmngl6pdiaz03nj8mkddasckdq9lbrh"))))
      (build-system python-build-system)
      (arguments (list #:phases #~(modify-phases %standard-phases
                                    (replace 'check
                                      (lambda* (#:key tests? #:allow-other-keys)
                                        (when tests?
                                          (invoke "python" "tests/tests.py")))))))
      (home-page "http://0chris.com/untangle")
      (synopsis "XML to Python objects conversion library")
      (description "@code{untangle} is a tiny Python library which converts an
XML document to a Python object.")
      (license license:expat))))

(define-public python-xmlschema
  (package
    (name "python-xmlschema")
    (version "1.2.5")
    (source (origin
              ;; Unit tests are not distributed with the PyPI archive.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sissaschool/xmlschema")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rsa75x86gdjalvy4riq7613szb616hff80crx006chyppzdkxmq"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 ;; Disable test_export_remote__issue_187, which is known to
                 ;; fail (see:
                 ;; https://github.com/sissaschool/xmlschema/issues/206).
                 (invoke "python" "-m" "unittest" "-v"
                         "-k" "not test_export_remote__issue_187")
                 (format #t "test suite not run~%")))))))
    (native-inputs
     (list python-lxml))   ;for tests
    (propagated-inputs
     (list python-elementpath))
    (home-page "https://github.com/sissaschool/xmlschema")
    (synopsis "XML Schema validator and data conversion library")
    (description
     "The @code{xmlschema} library is an implementation of
@url{https://www.w3.org/2001/XMLSchema, XML Schema} for Python.  It has
full support for the XSD 1.0 and 1.1 standards, an XPath-based API for
finding schema's elements and attributes; and can encode and decode
XML data to JSON and other formats.")
    (license license:expat)))

(define-public python-xmltodict
  (package
    (name "python-xmltodict")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xmltodict" version))
       (sha256
        (base32
         "08cadlb9vsb4pmzc99lz3a2lx6qcfazyvgk10pcqijvyxlwcdn2h"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-coverage python-nose python-setuptools python-wheel))
    (home-page "https://github.com/martinblech/xmltodict")
    (synopsis "Work with XML like you are working with JSON")
    (description "This package provides a Python library to convert XML to
@code{OrderedDict}.")
    (license license:expat)))

(define-public xml-namespace-xsd
  (package
    (name "xml-namespace-xsd")
    (version "2009-01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.w3.org/"
                           (string-replace-substring version "-" "/")
                           "/xml.xsd"))
       (sha256
        (base32 "0agqmxbhk2q9xa38m02z7ggbb124z6avnqyhz8k43iicqhv1fw6c"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("xml.xsd" "/xml/dtd/namespace/xml.xsd")
                         ("catalog.xml" "/xml/dtd/namespace/catalog.xml"))
      #:phases #~(modify-phases %standard-phases
                   (add-before 'install 'create-catalog
                     (lambda _
                       (invoke "xmlcatalog"
                               "--noout"
                               "--create"
                               "--add"
                               "uri"
                               "http://www.w3.org/2001/xml.xsd"
                               "xml.xsd"
                               "catalog.xml"))))))
    (native-inputs (list libxml2))
    (home-page "https://www.w3.org/XML/1998/namespace")
    (synopsis "XML Schema for XML namespace")
    (description
     "This package provides an XML Schema and its catalog.  The schema constrains the
syntax of @code{xml:lang}, @code{xml:spec}, @code{xml:base}, and @code{xml:id} in the
schema language defined by the XML Schema Recommendation Second Edition of 28 October
2004.")
    (license license:w3c)))

(define-public xmlpatch
  (package
    (name "xmlpatch")
    (version "0.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ufz/xmlpatch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10kjg7lz9p4xnv96053mj18dmc7lj7iqzx98z3aagnw6hfwdri7f"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON")))
    (native-inputs (list pkg-config))
    (inputs (list libxml2 glib))
    (home-page "https://xmlpatch.sourceforge.net")
    (synopsis "XML patch library")
    (description
     "XML Patch is a C++ library and command-line interface
(the @command{xml-diff} and @command{xml-patch} commands) for patching XML
files with XPath expressions.")
    (license license:lgpl2.1+)))
