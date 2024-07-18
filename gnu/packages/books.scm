;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2023 Adam Faiz <adam.faiz@disroot.org>
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

(define-module (gnu packages books)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages music)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages version-control))

(define-public book-sparc
  (package
    (name "book-sparc")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artyom-poptsov/SPARC")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1ns2vs5yb9z1hj9gq5y55qz7c9azzhr866b665s8fq50q5m4yhbc"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (substitute* "version.tex.in"
                     (("@COMMIT@") ""))
                   (substitute* "Makefile"
                     (("all: sparc.pdf") "all: install")
                     (("^sparc.pdf:") "install:")
                     (("(cp out/sparc.pdf) sparc.pdf" all cp)
                      (string-append
                       "mkdir -p $(DESTDIR)$(PREFIX)/share/doc/book-sparc"
                       " && " cp
                       " $(DESTDIR)$(PREFIX)/share/doc/book-sparc/sparc.pdf")))))))
    (build-system gnu-build-system)
    (native-inputs
     (list bash-minimal
           fontconfig
           inkscape
           lilypond
           perl
           python-pygments
           which))
    (inputs
     (list font-liberation
           texlive-acronym
           texlive-adjustbox
           texlive-biblatex
           texlive-bibtex
           texlive-bibtexperllibs
           texlive-bigfoot
           texlive-chngcntr
           texlive-circuitikz
           texlive-collection-langcyrillic
           texlive-fontspec
           texlive-glossaries
           texlive-glossaries-english
           texlive-glossaries-extra
           texlive-koma-script
           texlive-lilyglyphs
           texlive-minted
           texlive-multirow
           texlive-pgf
           texlive-pgfplots
           texlive-subfiles
           texlive-svg
           texlive-t1utils
           texlive-textpos
           texlive-transparent
           texlive-trimspaces
           texlive-xetex))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'check)
               (delete 'configure)
               (add-before 'build 'set-envs
                 (lambda _
                   (setenv "REPRODUCIBILITY" "yes")
                   (setenv "PREFIX" #$output))))))
    (home-page "https://github.com/artyom-poptsov/SPARC")
    (synopsis "Book on combining art and technology")
    (description
     "Science, Programming, Art and Radioelectronics Club (SPARC) is a book that
explains how to combine the topics mentined in the title to build projects.  The
book can be used to teach programming classes in colleges and to organize
workshops in hackerspaces or other community-driven spaces.  Currently the book
is available only in Russian.")
    (license license:cc-by-sa4.0)))
