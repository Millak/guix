;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2014, 2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2022, 2024-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2020, 2021 Sergei Trofimovich <slyfox@inbox.ru>
;;; Copyright © 2018, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020-2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2024-2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2025 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2025 Anderson Torres <anderson.torres.8519@gmail.com>
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

(define-module (gnu packages compiler-tools)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages python)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public byacc
  (package
    (name "byacc")
    (version "20240109")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://invisible-mirror.net/archives/byacc/byacc-"
             version ".tgz"))
       (sha256
        (base32
         "0il4w1vwbglayakywyghiqhcjpg1yvv5ww2p8ylz32bi05wpg2gj"))
       (snippet
        #~(begin
            ;; Remove machine-generated files
            (for-each delete-file
                      (list "configure"
                            "btyaccpar.c"
                            "yaccpar.c"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           gawk))
    (home-page "https://invisible-island.net/byacc/byacc.html")
    (synopsis "Berkeley Yacc LALR parser generator")
    (description
     "Berkeley Yacc is an LALR(1) parser generator.  Yacc reads the grammar
specification from a file and generates an LALR(1) parser for it.  The parsers
consist of a set of LALR(1) parsing tables and a driver routine written in the
C programming language.")
    (license license:public-domain)))

(define-public dparser
  (package
    (name "dparser")
    (version "1.33a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jplevyak/dparser/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vzfi7d573qsmfxkgnzqkalhv06i2zc8hm0pwcgrgj8382g01zg1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags
      #~(list
         (string-append "CC=" #$(cc-for-target))
         (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (home-page "https://dparser.sourceforge.net/")
    (synopsis "Scannerless GLR parser generator")
    (description
     "DParser is scannerless GLR parser generator.  The form of the text to be
parsed can be specified using a combination of regular expressions and grammar
productions.  Because of the parsing technique, a scannerless GLR parser based
on the Tomita algorithm the grammar can be ambiguous, right or left recursive,
have any number of null productions, and because there is no separate
tokenizer, can include whitespace in terminals and have terminals which are
prefixes of other terminals.")
    (license (list license:bsd-3))))

(define-public flex
  (package
    (name "flex")
    (version "2.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/westes/flex"
             "/releases/download/v" version "/"
             "flex-" version ".tar.gz"))
       (sha256
        (base32
         "15g9bv236nzi665p9ggqjlfn4dwck5835vf0bbw2cz7h5c1swyp8"))))
    (build-system gnu-build-system)
    (arguments
     (if (%current-target-system)
         (list #:configure-flags
               #~'("ac_cv_func_malloc_0_nonnull=yes"
                   "ac_cv_func_realloc_0_nonnull=yes"))
         '()))
    (native-inputs
     (list ;; m4 is not present in PATH when cross-building
           m4))
    (inputs
     (list
      (package
        (inherit bison)
        (arguments
         ;; Disable tests, since they require flex.
         (substitute-keyword-arguments arguments
           ((#:tests? _ #f) #f)))
        (inputs (modify-inputs inputs
                  (delete "flex"))))))
    (propagated-inputs (list m4))
    (home-page "https://github.com/westes/flex")
    (synopsis "Fast lexical analyser generator")
    (description
     "Flex is a tool for generating scanners.  A scanner, sometimes
called a tokenizer, is a program which recognizes lexical patterns in
text.  The flex program reads user-specified input files, or its standard
input if no file names are given, for a description of a scanner to
generate.  The description is in the form of pairs of regular expressions
and C code, called rules.  Flex generates a C source file named,
\"lex.yy.c\", which defines the function yylex().  The file \"lex.yy.c\"
can be compiled and linked to produce an executable.  When the executable
is run, it analyzes its input for occurrences of text matching the
regular expressions for each rule.  Whenever it finds a match, it
executes the corresponding C code.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

;;; The package is named orangeduck-mpc to differentiate it from GNU mpc.
(define-public orangeduck-mpc
  ;; The last release lacks an 'install' target.
  (let ((commit "1049534fc56b1971345c7aaa792dea55d6f9b7bc")
        (revision "1"))
    (package
      (name "orangeduck-mpc")
      (version (git-version "0.9.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/orangeduck/mpc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1wpxchbjxsyksr8il9pvg195nvqzsjdshzyzwikxda0fss0p7aap"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                  (string-append "PREFIX=" #$output))
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'patch-Makefile
                            (lambda _
                              (substitute* "Makefile"
                                ;; Do not attempt to alter the permissions,
                                ;; otherwise 'install' would error with
                                ;; "cannot stat [...] Permission denied"
                                ;; errors.
                                (("\\s\\-m[0-9]{3}\\s")
                                 " "))))
                          (delete 'configure))))
      (home-page "https://github.com/orangeduck/mpc")
      (synopsis "Parser Combinator library for C")
      (description "@code{mpc} is a lightweight Parser Combinator library for C.
@code{mpc} can help with tasks such as:
@itemize
@item Building a new programming language
@item Building a new data format
@item Parsing an existing programming language
@item Parsing an existing data format
@item Embedding a Domain Specific Language
@item Implementing Greenspun's Tenth Rule.
@end itemize")
      (license license:bsd-2))))

(define-public lexy
  (package
    (name "lexy")
    (version "2025.05.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/foonathan/lexy")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14j2z7x2l65q95j5br5nw7awgd87p9m2xw7mma4qspiricd0rniq"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DLEXY_BUILD_DOCS=OFF")  ; needs Hugo
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-dependencies
            (lambda _
              (substitute* "tests/CMakeLists.txt"
                (("^message\\(STATUS \"Fetching doctest\"\\).*") "")
                (("^include\\(FetchContent\\).*") "")
                (("^FetchContent_Declare\\(doctest .*") "")
                (("^FetchContent_MakeAvailable\\(doctest\\)")
                 "find_package(doctest REQUIRED)")
                (("^(target_link_libraries\\(lexy_test_base .*) doctest\\)"
                  _ prefix)
                 (string-append prefix ")"))))))))
    (native-inputs (list doctest))
    (home-page "https://lexy.foonathan.net/")
    (synopsis "C++ parser combinator library")
    (description
     "lexy is a parser combinator library for C++17 and later.")
    (license license:boost1.0)))

(define-public oyacc
  (package
    (name "oyacc")
    (version "6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ibara/yacc")
              (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a2nyg2nnh1las40klnp037sapaaph86kjx258cpqcwnk4cv3rnx"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags
      #~(list (string-append "BINDIR=" #$output "/bin")
              (string-append "MANDIR=" #$output "/share/man/man1"))))
    (home-page "https://github.com/ibara/yacc")
    (synopsis "Portable YACC from OpenBSD")
    (description
     "@command{oyacc} is a portable version of the OpenBSD's
@acronym{yacc, Yet Another Compiler Compiler} program, with no dependencies
besides libc.")
    (license license:bsd-2)))

(define-public oyacc-as-yacc-wrapper
  (package/inherit oyacc
    (name "oyacc-as-yacc-wrapper")
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (let ((bindir (string-append #$output "/bin"))
                  (oyacc (string-append #$(this-package-input "oyacc")
                                        "/bin/oyacc")))
              (mkdir-p bindir)
              (symlink oyacc (string-append bindir "/yacc")))))))
    (inputs (list oyacc))
    (description
     "This package provides the @command{yacc} command, implemented as a
symbolic link to the @command{oyacc} command from the same-named package.")))

(define-public packcc
  (package
    (name "packcc")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/arithy/packcc")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00aad9hx9kks2wyba8m2gb1pgsc1sxfpgndnfqhs7jswzks6455w"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules
      '((guix build cmake-build-system)
        ((guix build gnu-build-system) #:prefix gnu:)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          ;; INFO: This project does not use ctest.
          (replace 'check
            (assoc-ref gnu:%standard-phases 'check)))))
    (native-inputs
     (list bats python))
    (home-page "https://github.com/arithy/packcc")
    (synopsis "Packrat parser generator for C")
    (description
     "PackCC is a @url{https://arxiv.org/abs/cs/0603077, packrat}
parser generator for the C programming language.  Its main features are:
@itemize
@item Generates a parser in C from a grammar described in a PEG.
@item Gives your parser great efficiency by packrat parsing.
@item Supports direct and indirect left-recursive grammar rules.
@end itemize
The grammar of your parser can be described in a @acronym{PEG, Parsing
Expression Grammar}.  The PEG is a top-down parsing language, and is similar
to the regular-expression grammar.  The PEG does not require tokenization to
be a separate step, and tokenization rules can be written in the same way as
any other grammar rules.")
    (license license:expat)))

(define-public pegtl
  (package
    (name "pegtl")
    (version "3.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/taocpp/PEGTL")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01adjqqdr9yf4h379jl1x0526kvixfpqk4hdk9mna49qc4s5hpi1"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/taocpp/PEGTL")
    (synopsis "Parsing Expression Grammar template library")
    (description
     "The @acronym{PEGTL, Parsing Expression Grammar Template Library} is a
zero-dependency C++ header-only parser combinator library for creating parsers
according to a @acronym{PEG, Parsing Expression Grammar}.")
    (license license:expat)))

(define-public re2c
  (package
    (name "re2c")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/skvadrik/" name
                           "/releases/download/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "07ysqgdm0h566a8lwnpdgycp93vz7zskzihsgah3bla0ycj2pp69"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests?
      (not (or (%current-target-system)
               ;; TODO: run_tests.py hangs
               (system-hurd?)))
      #:phases
      (if (target-arm32?)
          #~(modify-phases %standard-phases
              (add-after 'unpack 'patch-sources
                (lambda _
                  (invoke "patch" "-p1" "--force" "--input"
                          #$(local-file (search-patch
                                         "re2c-Use-maximum-alignment.patch"))))))
          #~%standard-phases)))
    (native-inputs
     (list python))             ; for the test driver
    (home-page "https://re2c.org/")
    (synopsis "Lexer generator")
    (description
     "@code{re2c, Regular Expressions to Code} is a flexible lexical analyser.
Instead of using traditional table-driven approaches, it encodes a finite
state machine directly in the code in the form of jumps and comparisons.")
    (license license:public-domain)))

(define-public reflex
  (package
    (name "reflex")
    (version "20260131")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://invisible-island.net/archives/reflex/reflex-"
             version ".tgz"))
       (sha256
        (base32 "1ga82ycrfmfq6g3r8b8ixlhj82wnlwf9kibzhyivjfv26dgscldb"))
       (snippet
        #~(begin
            (delete-file "configure")))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           ;; Works with any POSIX yacc, but let's use a non-self-hosted one.
           oyacc-as-yacc-wrapper))
    (home-page "https://invisible-island.net/reflex/reflex.html")
    (synopsis "Variant of flex lexical scanner")
    (description
     "Reflex is a POSIX-compatible variant of the @command{flex} lexical scanner
generator.")
    (license license:bsd-2)))
