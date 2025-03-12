;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 EuAndreh <eu@euandre.org>
;;; Copyright © 2021 Noisytoot <noisytoot@disroot.org>
;;; Copyright © 2021 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2022, 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2025 Vinicius Monego <monego@posteo.net>
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

(define-module (gnu packages markup)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public hoedown
  (package
    (name "hoedown")
    (version "3.0.7")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/hoedown/hoedown")
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1kr3hxjg2dgmwy9738qgj3sh3f5cygx0zxskkfhrg7x19bq9yd26"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)) ; no configure script
       #:test-target "test"))
    (native-inputs
     `(("python" ,python-2)
       ("tidy" ,tidy-html)))
    (synopsis "Markdown processing library")
    (description "Hoedown is a standards compliant, fast, secure markdown
processing library written in C.")
    (home-page "https://github.com/hoedown/hoedown")
    (license license:expat)))

(define-public markdown
  (package
    (name "markdown")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://daringfireball.net/projects/downloads/"
             (string-capitalize name) "_" version ".zip"))
       (sha256
        (base32 "0dq1pj91pvlwkv0jwcgdfpv6gvnxzrk3s8mnh7imamcclnvfj835"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source"))
               (out    (assoc-ref %outputs "out"))
               (perlbd (string-append (assoc-ref %build-inputs "perl") "/bin"))
               (unzip  (search-input-file %build-inputs "/bin/unzip")))
           (mkdir-p out)
           (with-directory-excursion out
             (invoke unzip source)
             (mkdir "bin")
             (mkdir-p "share/doc")
             (rename-file "Markdown_1.0.1/Markdown.pl" "bin/markdown")
             (rename-file "Markdown_1.0.1/Markdown Readme.text"
                          "share/doc/README")
             (patch-shebang "bin/markdown" (list perlbd))
             (delete-file-recursively "Markdown_1.0.1"))
           #t))))
    (native-inputs (list unzip))
    (inputs (list perl))
    (home-page "http://daringfireball.net/projects/markdown")
    (synopsis "Text-to-HTML conversion tool")
    (description
     "Markdown is a text-to-HTML conversion tool for web writers.  It allows
you to write using an easy-to-read, easy-to-write plain text format, then
convert it to structurally valid XHTML (or HTML).")
    (license (license:non-copyleft "file://License.text"
                                   "See License.text in the distribution."))))

(define-public latexml
  (package
    (name "latexml")
    (version "0.8.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/brucemiller/LaTeXML")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
         (base32
           "0lppm66fxadsbn73xh0nfqdi4y6d9j6xph1slvlp6ynrfbnn5p6s"))))
    (build-system perl-build-system)
    (arguments
     (let ((wraplibs
             (list "perl-archive-zip"
                   "perl-common-sense"
                   "perl-db-file"
                   "perl-encode-locale"
                   "perl-file-which"
                   "perl-getopt-long"
                   "perl-http-date"
                   "perl-http-message"
                   "perl-image-magick"
                   "perl-image-size"
                   "perl-io-string"
                   "perl-json-xs"
                   "perl-libwww"
                   "perl-mime-base64"
                   "perl-parse-recdescent"
                   "perl-pod-parser"
                   "perl-text-unidecode"
                   "perl-time-hires"
                   "perl-try-tiny"
                   "perl-types-serialiser"
                   "perl-uri"
                   "perl-xml-libxml"
                   "perl-xml-libxslt"
                   "perl-xml-sax-base")))
       (list
        ;; some tests skip due to missing dependencies
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'find-itself
              ;; Fix run-time 'Can't locate [].pm in @INC' failure and remove
              ;; need for extensive set of propagated inputs
              (lambda* (#:key inputs outputs #:allow-other-keys)
                  (with-directory-excursion (string-append #$output "/bin")
                    (for-each
                     (lambda* (program)
                       (wrap-program program
                         `("PERL5LIB" ":" prefix
                           (,(string-append #$output "/lib/perl5/site_perl")
                            ,#$@(map (lambda (in)
                                       (file-append
                                        (this-package-input in) "/lib/perl5/site_perl"))
                                     wraplibs)))))
                     (find-files "." ".*")))))))))
    (inputs
     (list bash-minimal
           libxml2
           libxslt
           perl
           perl-archive-zip
           perl-common-sense
           perl-db-file
           perl-encode-locale
           perl-file-which
           perl-getopt-long
           perl-http-date
           perl-http-message
           perl-image-magick
           perl-image-size
           perl-io-string
           perl-json-xs
           perl-libwww
           perl-mime-base64
           perl-parse-recdescent
           perl-pod-parser
           perl-text-unidecode
           perl-time-hires
           perl-try-tiny
           perl-types-serialiser
           perl-uri
           perl-xml-libxml
           perl-xml-libxslt
           perl-xml-sax-base))
    (native-inputs
     (list texlive-bin
           perl-test-more-utf8
           perl-extutils-manifest
           perl-data-dumper
           perl-ipc-run3
           perl-file-temp))
    (home-page "https://math.nist.gov/~BMiller/LaTeXML/")
    (synopsis "LaTeX to XML, HTML, MathML, epub and Jats converter")
    (description "This package provides a LaTeX converter, with the following goals:
@itemize
@item Faithful emulation of TEX’s behaviour;
@item Easily extensible;
@item Lossless, preserving both semantic and presentation cues;
@item Use an abstract LATEX-like, extensible, document type;
@item Infer the semantics of mathematical content
@end itemize")
    (license license:cc0)))

(define-public lowdown
  (package
    (name "lowdown")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://kristaps.bsd.lv/lowdown/snapshots/lowdown-"
             version ".tar.gz"))
       (sha256
        (base32 "0y88gffrg1zrin0y53j4gbkmpia0r8p0kyklj501wavkqi83j7pk"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "regress"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (invoke "./configure"
                      (string-append "PREFIX=" #$output)
                      (string-append "MANDIR=" #$output "/share/man"))))
          (replace 'install
            (lambda _
              (invoke "make" "install" "install_libs"))))
      #:make-flags #~(list "CFLAGS=-fPIC")))
    (native-inputs
     (list which))
    (home-page "https://kristaps.bsd.lv/lowdown/")
    (synopsis "Simple Markdown translator")
    (description "Lowdown is a Markdown translator producing HTML5, roff
documents in the ms and man formats, LaTeX, gemini, and terminal output.")
    (license license:isc)))

(define-public discount
  (package
    (name "discount")
    (version "2.2.7")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.pell.portland.or.us/~orc/Code/"
                   "discount/discount-" version ".tar.bz2"))
             (sha256
              (base32
               "024mxv0gpvilyfczarcgy5m7h4lv6qvhjfpf5i73qkxhszjjn9mi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:parallel-build? #f             ; libmarkdown won't be built in time
       #:make-flags (list
                     (string-append "LFLAGS=-L. -Wl,-rpath="
                                    (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-AC_PATH
           (lambda _
             ;; The default value is not suitable, so override using an
             ;; environment variable. This just affects the build, and not the
             ;; resulting store item.
             (setenv "AC_PATH" (getenv "PATH"))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "CC" ,(cc-for-target))
               ;; The ‘validate-runpath’ phase fails otherwise.
               (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib"))
               (invoke "./configure.sh"
                       (string-append "--prefix=" out)
                       "--shared")))))))
    (native-inputs
     (list pkg-config))
    (synopsis "Markdown processing library, written in C")
    (description
     "Discount is a markdown implementation, written in C.  It provides a
@command{markdown} command, and a library.")
    (home-page "https://www.pell.portland.or.us/~orc/Code/discount/")
    (license license:bsd-3)))

(define-public perl-text-markdown
  (package
    (name "perl-text-markdown")
    (version "1.000031")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/B/BO/BOBTFISH/Text-Markdown-" version
             ".tar.gz"))
       (sha256
        (base32 "06y79lla8adkqhrs41xdddqjs81dcrh266b50mfbg37bxkawd4f1"))))
    (build-system perl-build-system)
    (native-inputs (list perl-list-moreutils
                         perl-module-install
                         perl-test-differences
                         perl-test-exception))
    (home-page "https://metacpan.org/release/Text-Markdown")
    (synopsis "Convert Markdown syntax to (X)HTML")
    (description "@code{Text::Markdown} is a Perl module that provides an
alternate implementation of the Markdown implementation by John Gruber (see
the markdown package).  It is a slower implementation, but better
maintained.")
    (license license:bsd-3)))

(define-public perl-text-markdown-discount
  (package
    (name "perl-text-markdown-discount")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/S/SE/SEKIMURA/Text-Markdown-Discount-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xx7v3wnla7m6wa3h33whxw3vvincaicg4yra1b9wbzf2aix9rnw"))
       (patches
        (search-patches "perl-text-markdown-discount-unbundle.patch"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-ldflags
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("OTHERLDFLAGS = ")
                (string-append
                      "OTHERLDFLAGS = -lmarkdown -Wl,-rpath="
                      (assoc-ref inputs "discount")
                      "/lib")))
             #t)))))
    (inputs
     (list discount))
    (home-page
     "https://metacpan.org/release/Text-Markdown-Discount")
    (synopsis
     "Fast function for converting Markdown to HTML using Discount")
    (description
     "Text::Markdown::Discount is a Perl extension to the Discount markdown
implementation.

@example
  use Text::Markdown::Discount;
  my $html = markdown($text)
@end example")
    (license license:perl-license)))

(define-public python-cmarkgfm
  (package
    (name "python-cmarkgfm")
    (version "2022.10.27")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cmarkgfm" version))
              (sha256
               (base32
                "16875bazqd7p7qiky343w0fzasqziyvf72nipyh1r47a2rvsrnck"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled cmark and generated headers.
                  (for-each delete-file-recursively
                            '("third_party/cmark" "generated"))))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'copy-cmark-gfm
                 (lambda _
                   ;; This package needs the cmark-gfm source files
                   ;; to generate FFI bindings.
                   (copy-recursively #+(package-source (this-package-input
                                                        "cmark-gfm"))
                                     "third_party/cmark")))
               (add-after 'unpack 'install-cmark-headers
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; XXX: Loosely based on 'regenerate' from noxfile.py.
                   (let ((version.h (search-input-file
                                     inputs "/include/cmark-gfm_version.h")))
                     (for-each (lambda (file)
                                 (install-file file "generated/unix/"))
                               (cons version.h
                                     (find-files (dirname version.h)
                                                 "_export\\.h$"))))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests? (invoke "pytest" "-vv" "tests")))))))
    (native-inputs (list python-pytest))
    (inputs (list cmark-gfm))
    (propagated-inputs (list python-cffi))
    (home-page "https://github.com/theacodes/cmarkgfm")
    (synopsis "Python bindings for GitHub's fork of cmark")
    (description
     "This package provides a minimal set of Python bindings for the
GitHub cmark fork (@code{cmark-gfm}).")
    (license license:expat)))

(define-public python-markdownify
  (package
    (name "python-markdownify")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch) ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/matthewwithanm/python-markdownify")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09gh3grgiaiyhgpd1fias9xac45d4zw37ikkzyaavsixfzg0akbr"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-beautifulsoup4 python-six))
    (home-page "https://github.com/matthewwithanm/python-markdownify")
    (synopsis "Converts HTML to Markdown")
    (description "This package provides @code{markdownify} a Python library to
convert HTML to Markdown.")
    (license license:expat)))

(define-public cmark
  (package
    (name "cmark")
    (version "0.31.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/commonmark/cmark")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0llj68l9rxdhral0zyv0bz6yzqsxgq8d3730082sl3kx78lsq5qq"))))
    (build-system cmake-build-system)
    (arguments
     '(#:test-target "test"))
    (native-inputs (list python))
    (synopsis "CommonMark Markdown reference implementation")
    (description
     "CommonMark is a strongly defined, highly compatible specification of
Markdown.  @code{cmark} is the C reference implementation of CommonMark.  It
provides the @code{libcmark} shared library for parsing CommonMark to an
abstract syntax tree (@dfn{AST}) and rendering the document as HTML, groff man,
LaTeX, CommonMark, or an XML representation of the AST.  It also provides the
command-line program @command{cmark} for parsing and rendering CommonMark.")
    (home-page "https://commonmark.org")
    ;; cmark is distributed with a BSD-2 license, but some components are Expat
    ;; licensed. The CommonMark specification is Creative Commons CC-BY-SA 4.0
    ;; licensed. See 'COPYING' in the source distribution for more information.
    (license (list license:bsd-2 license:expat license:cc-by-sa4.0))))

(define-public perl-commonmark
  (package
    (name "perl-commonmark")
    (version "0.290000")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/N/NW/NWELLNHOF/CommonMark-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1pgaqa4f00i9r5z7l9xiya0q51ysq0nhpvgr0f3rza3cxz1v80d5"))))
    (build-system perl-build-system)
    (arguments
     `(#:make-maker-flags
       ;; MakeMaker ignores LIBRARY_PATH.
       (list (format #f "LIBS=-L~a/lib -lcmark"
                     (assoc-ref %build-inputs "cmark")))))
    (inputs (list cmark perl-test-leaktrace perl-devel-checklib
                  perl-module-build))
    (home-page "https://metacpan.org/release/CommonMark")
    (synopsis "Interface to the CommonMark C library")
    (description
     "This module is an XS wrapper around the official
CommonMark C library libcmark.  It closely follows the original API.")
    (license license:perl-license)))

(define-public cmark-gfm
  (package
    (inherit cmark)
    (name "cmark-gfm")
    (version "0.29.0.gfm.13")
    (home-page "https://github.com/github/cmark-gfm")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1apy9i76rgs0bmgdlpjszv0fpqhlap2s12m68wvnsv8j3fsqc90y"))))
    (arguments
     (list #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'install-config
                          (lambda _
                            ;; XXX: cmark-gfm-core-extensions.h includes this file.
                            (install-file "src/config.h"
                                          (string-append #$output "/include")))))))
    (synopsis "GitHub flavored CommonMark")
    (description
     "This package is a fork of @code{cmark}, with GitHub-specific Markdown
additions.")))

(define-public smu
  (package
    (name "smu")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Gottox/smu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jm7lhnzjx4q7gcwlkvsbffcy0zppywyh50d71ami6dnq182vvcc"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:tests? #f                      ; no tests included
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))
    (home-page "https://github.com/Gottox/smu")
    (synopsis "Simple markup")
    (description
     "Smu is a very simple and minimal markup language.  It is
designed for using in wiki-like environments.  Smu makes it very
easy to write your documents on the fly and convert them into HTML.
Smu is capable to parse very large documents.  As long as you avoid an huge
amount of indents it scales just great.

Smu was started as a rewrite of Markdown but became something more
lightweight and consistent.  The biggest difference between Markdown
and smu is that smu doesn't support reference style links.")
    (license license:x11)))

(define-public md4c
  (package
    (name "md4c")
    (version "0.4.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mity/md4c/")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12pdh4rfjc3b0cblj5nz3jksr2376lx8ay0vw5dwa1s97q09pczq"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/mity/md4c/")
    (synopsis "C Markdown parser compliant to CommonMark")
    (description "MD4C is a C Markdown parser with a
SAX-like interface.  It is compliant to the CommonMark specification,
with a few extensions.")
    (license license:expat)))

(define-public python-mistletoe
  (package
    (name "python-mistletoe")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mistletoe" version))
       (sha256
        (base32 "1sfv79fway4iya9i3rmz1bkj12lhzgazd4n7kv8phi4vvn57h3mx"))))
    (build-system pyproject-build-system)
    (arguments
     ;; FileNotFoundError (not distributed in PyPI).
     (list #:test-flags #~(list "-k" "not test_main")))
    (native-inputs
     (list python-parameterized
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/miyuchina/mistletoe")
    (synopsis "Extensible Markdown parser in pure Python")
    (description
     "The @code{mistletoe} Markdown parser is a CommonMark-compliant Markdown
parser that supports definitions of custom tokens.

Parsing Markdown into an abstract syntax tree also allows @code{mistletoe} to
swap out renderers for different output formats, without touching any of the
core components.")
    (license license:expat)))
