;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Adam Faiz <adam.faiz@disroot.org>
;;; Copyright © 2023 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2023 宋文武 <iyzsong@envs.net>
;;; Copyright © 2023-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Gabriel Santos <gabrielsantosdesouza@disroot.org>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages music)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml))

(define-public book-faif
  (package
    (name "book-faif")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.fsf.org/faif/faif-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "0qf14d0n6k1dn9z0fdnx9qkhn4iq685xd443w7l7w54bm931p7dw"))))
    (build-system copy-build-system)
    (native-inputs
     (list (texlive-local-tree
            (list texlive-caption
                  texlive-endnotes
                  texlive-etoolbox
                  texlive-fncychap
                  texlive-helvetic
                  texlive-times
                  texlive-ucs))))
    (arguments
     (list
      #:install-plan #~'(("faif-2.0.pdf" "share/doc/faif/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'writable-texmfvar
            ;; Generating font shapes require a writable TEXMFVAR directory.
            (lambda _ (setenv "TEXMFVAR" "/tmp")))
          (add-before 'install 'build
            (lambda _
              (invoke "pdflatex" "faif-2.0.tex"))))))
    (home-page "https://www.fsf.org/faif")
    (synopsis "Free as in Freedom (2.0)")
    (description
     "In 2002, Sam Williams wrote Free as in Freedom, a biography of Richard
M. Stallman.  In its epilogue, Williams expressed hope that choosing to
distribute his book under the GNU Free Documentation License would enable and
encourage others to share corrections and their own perspectives through
modifications to his work.  Free as in Freedom (2.0) is Stallman's revision of
the original biography.  While preserving Williams's viewpoint, it includes
factual corrections and extensive new commentary by Stallman, as well as new
prefaces by both authors written for the occasion.  It is a rare kind of
biography, where the reader has the benefit of both the biographer's original
words and the subject's response.")
    (license license:fdl1.3+)))

(define-public book-sicp
  (let ((commit "bda03f79d6e2e8899ac2b5ca6a3732210e290a79")
        (revision "3"))
    (package
      (name "book-sicp")
      (version (git-version "20180718" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sarabander/sicp")
                      (commit commit)))
                (sha256
                 (base32
                  "0mng7qrj2dvssyffr9ycnf4a5k0kadp4dslq7mc5bhzq1qxyjs2w"))
                (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (native-inputs (list gzip texinfo))
      (arguments
       (list #:install-plan ''(("html" "share/doc/sicp/")
                               ("sicp.info" "share/info/"))
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'remove-obsolete-commands
                            (lambda _
                              ;; Reported upstream:
                              ;; https://github.com/sarabander/sicp/issues/46.
                              (substitute* "sicp-pocket.texi"
                                (("@setshortcontentsaftertitlepage")
                                 ""))))
                          (add-before 'install 'build
                            (lambda _
                              (invoke "makeinfo" "--no-split"
                                      "--output=sicp.info"
                                      "sicp-pocket.texi"))))))
      (home-page "https://sarabander.github.io/sicp")
      (synopsis "Structure and Interpretation of Computer Programs")
      (description "Structure and Interpretation of Computer Programs (SICP) is
a textbook aiming to teach the principles of computer programming.

Using Scheme, a dialect of the Lisp programming language, the book explains
core computer science concepts such as abstraction in programming,
metalinguistic abstraction, recursion, interpreters, and modular programming.")
      (license license:cc-by-sa4.0))))

(define-public book-sparc
  (package
    (name "book-sparc")
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artyom-poptsov/SPARC")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1vln9fvd2ajb55alm0bq3hgkaakfmrvws0f9sshywffclpnj7lnj"))
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
           (texlive-local-tree
            (list texlive-acronym
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
                  texlive-framed
                  texlive-glossaries
                  texlive-glossaries-english
                  texlive-glossaries-extra
                  texlive-koma-script
                  texlive-libkpathsea
                  texlive-lilyglyphs
                  texlive-lineno
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
                  texlive-xetex
                  texlive-xstring))
           which))
    (inputs (list font-liberation git))
    (arguments
     (list #:tests? #f                  ; no tests
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

(define-public sword
  (package
    (name "sword")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.crosswire.org/ftpmirror/pub/sword/source/v"
             (version-major+minor version) "/sword-" version ".tar.gz"))
       (sha256
        (base32 "12kb881w7p79is3ysy2w5bnzj8axfk05lb1ya8413brgvvrrqh22"))))
    (build-system cmake-build-system)
    (native-inputs (list curl icu4c-73 zlib))
    (arguments
     (list
      #:configure-flags
      #~(list "-DSWORD_BUILD_TESTS=Yes"
              "-DSWORD_USE_INTERNAL_ZLIB=No")))
    (home-page "https://www.crosswire.org/sword/")
    (synopsis "Cross-platform open source tools for writing Bible software")
    (description
     "The SWORD Project is a free Bible software project used to create Bible
software, with support for multiple texts and languages.")
    (license license:gpl2+)))

(define-public xiphos
  (package
    (name "xiphos")
    (version "4.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/crosswire/xiphos")
             (commit version)))
       (sha256
        (base32 "15p8ahbcd8vjm1ch0wahjfj20agd06va8rvgw1awnyzkcw2xsf8x"))
       (patches (search-patches "xiphos-glib.patch"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs (list appstream
                         appstream-glib
                         atk
                         biblesync
                         desktop-file-utils ;for 'desktop-file-validate'
                         (list glib "bin")
                         gettext-minimal
                         gsettings-desktop-schemas
                         gtk+
                         libgsf
                         minizip
                         pkg-config
                         sword
                         util-linux ;for 'uuidgen'
                         (list util-linux "lib") ;for 'libuuid'
                         webkitgtk-with-libsoup2
                         yelp-tools
                         zip))
    (inputs (list dbus dbus-glib libxml2 python python-lxml))
    (arguments
     (list
      #:tests? #f)) ;No tests
    (home-page "https://xiphos.org/")
    (synopsis "Open Source Bible Study Software")
    (description
     "Xiphos is a Bible study tool using GTK.  It uses Sword to
display bibles, commentaries, dictionaries, and other texts and images.
Xiphos includes features such as searching, biblesync, bookmarks,
parallel study, and original language study.")
    (license license:gpl2+)))

(define-public book-emacs-lisp-elements
  (package
    (name "book-emacs-lisp-elements")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/protesilaos/emacs-lisp-elements")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0774zycgx3zspn6f8pszap1751xgrlv2h3x66kksnj589nfnwzil"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f ;No code —no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'makeinfo
            (lambda _
              (emacs-makeinfo "elispelem.org"))))))
    (native-inputs (list texinfo))
    (home-page "https://protesilaos.com/emacs/emacs-lisp-elements")
    (synopsis "Overview of the Emacs Lisp programming language")
    (description
     "Emacs Lisp Elements is a book written by Protesilaos Stavrou, providing
a big picture view of the @acronym{Elisp, Emacs Lisp} programming language by
combining prose with code.  This book aims to provide an idea of how Elisp
works by showing some of the main concepts and patterns encountered in
everyday Elisp code.

This book is not intended as a replacement for the built-in Emacs Lisp
Reference Manual, but instead to give readers enough information to reason
about Elisp code.")
    (license license:fdl1.3+)))
