;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2024-2025 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages gawk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

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
        #~(begin (delete-file "btyaccpar.c")
                 (delete-file "yaccpar.c")))))
    (native-inputs
     (list gawk))
    (build-system gnu-build-system)
    (home-page "https://invisible-island.net/byacc/byacc.html")
    (synopsis "Berkeley Yacc LALR parser generator")
    (description
     "Berkeley Yacc is an LALR(1) parser generator.  Yacc reads the grammar
specification from a file and generates an LALR(1) parser for it.  The parsers
consist of a set of LALR(1) parsing tables and a driver routine written in the
C programming language.")
    (license license:public-domain)))

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
