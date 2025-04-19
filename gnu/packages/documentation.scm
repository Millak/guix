;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020, 2021 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim.counoyer@gmail.com>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024 Felix Gruber <felgru@posteo.net>
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

(define-module (gnu packages documentation)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix deprecation)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public latex2html
  (package
    (name "latex2html")
    (version "2024")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/latex2html/latex2html")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h5ya6pvh87y7xf5c05plbvm35qnn5qd0fllnhp5kym43zmr4prh"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-configure
                     (lambda* (#:key outputs #:allow-other-keys)
                       (substitute* "configure"
                         (("/usr/local")
                          #$output)
                         (("\\$\\{CONFIG_SHELL-/bin/sh\\}")
                          (which "bash")))))
                   (replace 'configure
                     (lambda _
                       (invoke "./configure")))
                   (add-after 'configure 'patch-cfgcache
                     (lambda* (#:key outputs #:allow-other-keys)
                       (substitute* "cfgcache.pm"
                         (("/usr/local")
                          #$output)))))))
    (inputs (list perl))
    (synopsis "LaTeX documents to HTML")
    (description "LaTeX2HTML is a utility that converts LaTeX documents to web
pages in HTML.")
    (home-page "https://www.latex2html.org/")
    (license license:gpl2+)))

(define-public asciidoc
  (package
    (name "asciidoc")
    (version "9.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/asciidoc/asciidoc-py")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1clf1axkns23wfmh48xfspzsnw04pjh4mq1pshpzvj0cwxhz0yaq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                     ; no 'check' target
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (invoke "autoconf")))
         ;; Some XML-related binaries are required for asciidoc's proper usage.
         ;; Without these, asciidoc fails when parsing XML documents, either
         ;; reporting a missing "xmllint" binary or, when passed the
         ;; "--no-xmllint" option, a missing "xsltproc" binary.
         ;; The following phase enables asciidoc to find some of them.
         (add-before 'configure 'set-xml-binary-paths
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let* ((libxml2 (assoc-ref inputs "libxml2"))
                              (xmllint (string-append libxml2 "/bin/xmllint"))
                              (libxslt (assoc-ref inputs "libxslt"))
                              (xsltproc (string-append libxslt "/bin/xsltproc")))
                         (substitute* "a2x.py"
                           (("XMLLINT = 'xmllint'")
                            (string-append "XMLLINT = '" xmllint "'"))
                           (("XSLTPROC = 'xsltproc'")
                            (string-append "XSLTPROC = '" xsltproc "'"))))))
         ;; Make asciidoc use the local docbook-xsl package instead of fetching
         ;; it from the internet at run-time.
         (add-before 'install 'make-local-docbook-xsl
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* (find-files "docbook-xsl" ".*\\.xsl$")
                         (("xsl:import href=\"http://docbook.sourceforge.net/\
release/xsl/current")
                          (string-append
                           "xsl:import href=\""
                           (string-append (assoc-ref inputs "docbook-xsl")
                                          "/xml/xsl/docbook-xsl-"
                                          ,(package-version docbook-xsl)))))))
         ;; Do the same for docbook-xml.
         (add-before 'install 'make-local-docbook-xml
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "docbook45.conf"
                         (("http://www.oasis-open.org/docbook/xml/4.5/docbookx.dtd")
                          (string-append (assoc-ref inputs "docbook-xml")
                                         "/xml/dtd/docbook/docbookx.dtd"))))))))
    (native-inputs (list autoconf))
    (inputs (list python docbook-xml-4.5 docbook-xsl libxml2 libxslt))
    (home-page "https://asciidoc.org/")
    (synopsis "Text-based document generation system")
    (description
     "AsciiDoc is a text document format for writing notes, documentation,
articles, books, ebooks, slideshows, web pages, man pages and blogs.
AsciiDoc files can be translated to many formats including HTML, PDF,
EPUB, man page.

AsciiDoc is highly configurable: both the AsciiDoc source file syntax and
the backend output markups (which can be almost any type of SGML/XML
markup) can be customized and extended by the user.")
    (license license:gpl2+)))

(define-deprecated asciidoc-py3 asciidoc)

(define-public doxygen
  (package
    (name "doxygen")
    (version "1.9.8")
    (home-page "https://www.doxygen.nl/")
    (source (origin
              (method url-fetch)
              (uri (list (string-append home-page "files/doxygen-"
                                        version ".src.tar.gz")
                         (string-append "mirror://sourceforge/doxygen/rel-"
                                        version "/doxygen-" version
                                        ".src.tar.gz")))
              (sha256
               (base32
                "0qjgw7bnx668hpi4r8m366vsq118s9365zf8z4x5yjrqx0ld5qq5"))))
    (build-system cmake-build-system)
    (native-inputs
     (list bison
           flex
           libxml2                      ;provides xmllint for the tests
           python))                     ;for creating the documentation
    (inputs
     (list bash-minimal))
    (arguments
     ;; Force cmake to use iconv header from cross-libc instead of the one
     ;; from native libc.
     (list
      #:configure-flags
      (if (%current-target-system)
          #~(list (string-append "-DICONV_INCLUDE_DIR="
                                 (assoc-ref %build-inputs "cross-libc")
                                 "/include"))
          #~'())
      #:test-target "tests"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-bibtex-test
            (lambda _
              ;; Disable test that requires bibtex to avoid a
              ;; circular dependency.
              (for-each delete-file-recursively
                        '("testing/012" "testing/012_cite.dox"))))
          (add-before 'configure 'patch-sh
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((/bin/sh (search-input-file inputs "/bin/sh")))
                (substitute* "src/portable.cpp"
                  (("/bin/sh")
                   /bin/sh))))))))
    (synopsis "Generate documentation from annotated sources")
    (description "Doxygen is the de facto standard tool for generating
documentation from annotated C++ sources, but it also supports other popular
programming languages such as C, Objective-C, C#, PHP, Java, Python,
IDL (Corba, Microsoft, and UNO/OpenOffice flavors), Fortran, VHDL, Tcl,
and to some extent D.")
    (license license:gpl3+)))

(define-public halibut
  (package
    (name "halibut")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.chiark.greenend.org.uk/~sgtatham/halibut/halibut-"
                    version "/halibut-" version ".tar.gz"))
              (sha256
               (base32
                "0ciikn878vivs4ayvwvr63nnhpcg12m8023xv514zxqpdxlzg85a"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ;No tests.
    (native-inputs (list pkg-config perl))
    (home-page "https://www.chiark.greenend.org.uk/~sgtatham/halibut/")
    (synopsis "Documentation production system for software manuals")
    (description
     "Halibut is a text formatting system designed primarily for writing software
documentation.  It accepts a single source format and outputs any combination of
plain text, HTML, Unix man or info pages, PostScript or PDF.  It has extensive
support for indexing and cross-referencing, and generates hyperlinks within output
documents wherever possible.  It supports Unicode, with the ability to fall back to
an alternative representation if Unicode output is not available.")
    (license license:expat)))

(define-public doc++
  (package
    (name "doc++")
    (version "3.4.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceforge.net/projects/docpp/"
                                  "files/doc++-" version ".tar.gz"))
              (sha256
               (base32
                "0i37zlxl8g352s4hzpdx0657k5x3czh3xcsfr27irc708gb277pn"))
              (patches (search-patches "doc++-include-directives.patch"
                                       "doc++-segfault-fix.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list flex gettext-minimal))
    (home-page "https://docpp.sourceforge.net")
    (synopsis "Documentation system for C, C++, IDL, and Java")
    (description
     "DOC++ is a documentation system for C, C++, IDL, and Java.  It can
generate both TeX output for high-quality hardcopies or HTML output for online
browsing.  The documentation is extracted directly from the C/C++/IDL source
or Java class files.")
    (license license:gpl2+)))

(define-public pod2pdf
  (package
    (name "pod2pdf")
    (version "0.42")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "mirror://cpan/authors/id/J/JO/JONALLEN/pod2pdf-"
                     version
                     ".tar.gz"))
              (sha256
                (base32
                  "0w5p7yy01vph74nfr9qzjb18p1avmhhcpza0qz9r88fmb0blbiyv"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-getopt-argvfile
           perl-pdf-api2
           perl-pod-parser))
    (home-page "https://metacpan.org/release/pod2pdf")
    (synopsis "Convert Pod to PDF format")
    (description "pod2pdf converts documents written in Perl's @acronym{POD, Plain Old
Documentation} format to PDF files.  It also supports some extensions to the POD
format, and supports the file types JPG, GIF, TIFF, PNG, and PNM for embedded
objects.")
    (license license:artistic2.0)))

;; XXX: This project looks not maintained, and some tests fail to pass, see
;; <https://github.com/spyder-ide/docrepr/issues/49>.
(define-public python-docrepr
  (package
    (name "python-docrepr")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/spyder-ide/docrepr")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ma5gwy93m1djd3zdlnqfrwhgr8ic1qbsz5kkrb9f987ax40lfkd"))
              (patches (search-patches "python-docrepr-fix-tests.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; all tests fail
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-sources
            (lambda _
              ;; XXX: This fixes an issue where shutil.copytree would fail
              ;; merging directories with same files copied by Sphinx from the
              ;; store (hence read-only, throwing a Permission denied error).
              ;; In the case this happens, it falls back to a manual copy
              ;; routine that omits overwriting same-named files (see:
              ;; https://github.com/spyder-ide/docrepr/issues/54).
              (substitute* "docrepr/utils.py"
                (("except TypeError")
                 "except (TypeError, shutil.Error)")))))))
    (native-inputs
     (list python-ipython
           python-matplotlib
           python-numpy
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-docutils
           python-jinja2
           python-matplotlib
           python-sphinx))
    (home-page "https://github.com/spyder-ide/docrepr/")
    (synopsis "Python docstrings to HTML renderer")
    (description "Docrepr renders Python docstrings to HTML with Sphinx.  It
can generate rich and plain representations of docstrings, alongside
additional metadata about the object to which the docstring belongs.")
    (license license:bsd-3)))

(define-public scrollkeeper
  (package
    (name "scrollkeeper")
    (version "0.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/scrollkeeper/scrollkeeper/"
                           version "/scrollkeeper-" version ".tar.gz"))
       (sha256
        (base32 "1bfxwxc1ngh11v36z899sz9qam366r050fhkyb5adv65lb1x62sa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-xml-catalog="
                            (assoc-ref %build-inputs "docbook-xml")
                            "/xml/dtd/docbook/catalog.xml"))))
    (inputs
     (list perl libxml2 libxslt
           ;; The configure script checks for either version 4.2 or 4.1.2.
           docbook-xml-4.2))
    (native-inputs
     (list intltool))
    (home-page "https://scrollkeeper.sourceforge.net/")
    (synopsis "Open Documentation Cataloging Project")
    (description
     "ScrollKeeper is a cataloging system for documentation.  It manages
documentation metadata as specified by the Open Source Metadata Framework and
provides a simple API to allow help browsers to find, sort, and search the
document catalog.  It will also be able to communicate with catalog servers on
the Net to search for documents which are not on the local system.")
    (license license:lgpl2.1+)))

(define-public zeal
  (package
    (name "zeal")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zealdocs/zeal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yz9zz18rh1d67w40ib4pna70vqkwa9i9nyj423rjysv5rdj2pzp"))))
    (build-system qt-build-system)
    (arguments
      (list
       #:tests? #f))                    ;no tests
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list bash-minimal
           libarchive
           sqlite
           qtbase-5
           qtdeclarative-5
           qtwayland-5
           qtwebchannel-5
           qtwebengine-5
           qtquickcontrols-5
           qtx11extras
           xcb-util-keysyms))
    (home-page "https://zealdocs.org/")
    (synopsis "Offline documentation browser inspired by Dash")
    (description "Zeal is a simple offline documentation browser
inspired by Dash.")
    (license license:gpl3+)))

(define-public markdeep
  (package
    (name "markdeep")
    (version "1.16")
    (home-page "https://casual-effects.com/markdeep/")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/morgan3d/markdeep")
                   (commit (string-append "v0" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32 "05bvw3993xh1260ckclwk4jw38hvgiff0b2940ryhbhz0p1k41l8"))))
    (build-system copy-build-system)
    (arguments
     (list #:modules '((guix build utils)
                       (guix build copy-build-system)
                       (ice-9 popen))
           #:install-plan ''(("." "/share/markdeep/"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-urls
                 (lambda _
                   (for-each (lambda (filename)

                               (substitute* filename
                                 ;; Don't include a reference to the remote version.
                                 (("<script src=\"https://casual-effects\\.com/\
markdeep/latest/markdeep\\.min\\.js\\?\"></script>")
                                  "")
                                 (("MATHJAX_URL = .*$")
                                  ;; Use our local copy of mathjax
                                  (string-append "MATHJAX_URL = 'file://" #$js-mathjax
                                                 "/share/javascript/es5/tex-mml-chtml.js'"))))
                             (find-files "." (lambda (file stat)
                                               (string-suffix? ".js" file))))))
               (add-before 'install 'minify
                 (lambda _
                   (for-each (lambda (filename)
                               (let ((minified-filename
                                      (string-append
                                       (string-drop-right filename 3)
                                       ".min.js")))
                                 (format #t "~a -> ~a~%" filename minified-filename)
                                 (let ((minified (open-pipe* OPEN_READ
                                                             "uglifyjs" filename)))
                                   (call-with-output-file minified-filename
                                     (lambda (port)
                                       (dump-port minified port)))

                                   (let ((exit (close-pipe minified)))
                                     (unless (zero? exit)
                                       (error "uglifyjs failed" exit))))))
                             (find-files "latest"
                                         (lambda (path stat)
                                           (and (string-suffix? ".js" path)
                                                (not (string-suffix? ".min.js"
                                                                     path)))))))))))
    (inputs (list js-mathjax))
    (native-inputs (list uglifyjs))
    (synopsis "Tool for displaying markdown documents in a web-browser")
    (description "Markdeep is a technology for writing plain text documents that can
be displayed in any web browser, whether local or remote.  It supports diagrams,
calendars, equations, and other features as extensions of Markdown syntax.")
    (license license:bsd-2)))

(define-public stddoc
  (let ((commit "6eef9deaf2e36bae812f50e448a8012b3e5efb14")
        (revision "1"))
    (package
      (name "stddoc")
      (version (git-version "1.0.2" revision commit))
      (home-page "https://github.com/r-lyeh/stddoc.c")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "06phjp7wbf4x1sagxwfapgv6iyiixmijxxbg2clb48kyvjg5mlwn"))
         (snippet #~(delete-file "stddoc.c.html"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f ; no tests
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'configure
                   (lambda _
                     (substitute* "stddoc.c"
                       ;; Note: For some reason quote characters are being
                       ;; inserted into urls and tags, eg.
                       ;; 'https://morgan3d.github.io/m""arkdeep/latest/markdeep.min.js?'
                       (("https://casual-effects.com/m\"*arkdeep/latest/")
                        (string-append #$markdeep "/share/markdeep/latest/"))
                       (("https://morgan3d.github.io/m\"*arkdeep/latest/")
                        (string-append #$markdeep "/share/markdeep/latest/")))))
                 (replace 'build
                   (lambda _
                     (invoke #$(cc-for-target)
                             "-O2" "-g" "-o" "stddoc" "stddoc.c")
                     (with-input-from-file "stddoc.c"
                       (lambda _
                         (with-output-to-file "stddoc.c.html"
                           (lambda _
                             (invoke #$(if (%current-target-system)
                                           "stddoc"
                                           "./stddoc"))))))))
                 (replace 'install
                   (lambda _
                     (install-file "stddoc"
                                   (string-append #$output "/bin"))
                     (install-file "stddoc.c.html"
                                   (string-append #$output
                                                  "/share/doc")))))))
      (native-inputs (if (%current-target-system) (list this-package) '()))
      (synopsis "Documentation generator for multiple programming languages")
      (description "@code{stddoc.c} is a tiny documentation generator with many
supported programming languages.  Markdeep code comments are extracted from stdin and
printed into stdout as a HTML file.")
      (license license:unlicense))))
