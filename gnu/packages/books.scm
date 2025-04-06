;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2024, 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
    (version "4.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artyom-poptsov/SPARC")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1n1ci224g09d105dgy8zpr8k4xg76d639p2ic90726b75sr0sm4w"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           bash-minimal
           fontconfig
           inkscape
           lilypond
           perl
           python-pygments
           which))
    (inputs
     (list font-liberation
           git
           texlive-acronym
           texlive-adjustbox
           texlive-biblatex
           texlive-bibtex
           texlive-bibtexperllibs
           texlive-bigfoot
           texlive-chngcntr
           texlive-circuitikz
           texlive-collection-langcyrillic
           texlive-csquotes
           texlive-fancyvrb
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
           texlive-upquote
           texlive-xetex))
   (arguments
    (list #:tests? #f                   ; no tests
          #:modules (append %default-gnu-imported-modules
                            '((ice-9 regex)
                              (srfi srfi-1)))
          #:phases #~(modify-phases %standard-phases
                       (add-before 'build 'configure-environment
                         (lambda* (#:key inputs make-flags parallel-build?
                                   #:allow-other-keys)
                           (let* ((src (assoc-ref inputs "source"))
                                  (rx  (make-regexp "/gnu/store/(.*)-book-sparc-.*"))
                                  (src-hash (match:substring (regexp-exec rx src) 1))
                                  (random-seed
                                   (fold (lambda (ch prev)
                                           (+ (char->integer ch)
                                              prev))
                                         0
                                         (string->list src-hash))))
                             (setenv "RANDOMSEED" (number->string random-seed))
                             (setenv "REPRODUCIBILITY" "yes"))))
                       (replace 'install
                         (lambda _
                           (let ((doc-dir (string-append #$output
                                                         "/share/doc/sparc/")))
                             (mkdir-p doc-dir)
                             (copy-file "sparc.ru.pdf"
                                        (string-append doc-dir
                                                       "sparc.ru.pdf"))
                             (copy-file "sparc.en.pdf"
                                        (string-append doc-dir
                                                       "sparc.en.pdf"))))))))
    (home-page "https://github.com/artyom-poptsov/SPARC")
    (synopsis "Book on combining art and technology")
    (description
     "Science, Programming, Art and Radioelectronics Club (SPARC) is a book that
explains how to combine the topics mentioned in the title to build projects.  The
book can be used to teach programming classes in colleges and to organize
workshops in hackerspaces or other community-driven spaces.  Currently the book
is available in Russian and English.")
    (license license:cc-by-sa4.0)))
