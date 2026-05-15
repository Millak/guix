;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2014, 2019, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2024-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2020, 2021 Sergei Trofimovich <slyfox@inbox.ru>
;;; Copyright © 2018, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (gnu packages bison)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages python)
  #:use-module (gnu packages)
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
     (list help2man
           ;; m4 is not present in PATH when cross-building
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
