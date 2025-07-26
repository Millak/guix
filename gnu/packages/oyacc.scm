;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages oyacc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

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
    (synopsis "Portable YACC from OpenBSD")
    (description
     "@command{oyacc} is a portable version of the OpenBSD's
@acronym{yacc, Yet Another Compiler Compiler} program, with no dependencies
besides libc.")
    (home-page "https://github.com/ibara/yacc")
    (license license:bsd-2)))
