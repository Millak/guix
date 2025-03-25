;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023, 2024, 2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (test-texlive)
  #:use-module (gnu packages tex)
  #:use-module (guix import texlive)
  #:use-module (guix tests)
  #:use-module (guix tests http)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-26)
  #:use-module (web client)
  #:use-module (ice-9 match))

(test-begin "texlive")

(define %fake-tlpdb
  '((database-revision . 12345)
    ("12many"
     . ((name
         . "12many")
        (catalogue
         . "one2many")
        (shortdesc
         . "Generalising mathematical index sets")
        (longdesc
         . "In the discrete branches of mathematics...")
        (docfiles
         . ("texmf-dist/doc/latex/12many/12many.pdf"
            "texmf-dist/doc/latex/12many/README"))
        (srcfiles
         . ("texmf-dist/source/latex/12many/12many.dtx"
            "texmf-dist/source/latex/12many/12many.ins"))
        (runfiles
         . ("texmf-dist/tex/latex/12many/12many.sty"))
        (catalogue-license . "lppl")))
    ("adforn"
     (name . "adforn")
     (shortdesc . "OrnementsADF font with TeX/LaTeX support")
     (longdesc . "The bundle provides the Ornements ADF font...")
     (execute "addMap OrnementsADF.map")
     (docfiles
      "texmf-dist/doc/fonts/adforn/COPYING"
      "texmf-dist/doc/fonts/adforn/NOTICE"
      "texmf-dist/doc/fonts/adforn/README"
      "texmf-dist/doc/fonts/adforn/adforn.pdf")
     (runfiles
      "texmf-dist/fonts/afm/arkandis/adforn/OrnementsADF.afm"
      "texmf-dist/fonts/enc/dvips/adforn/OrnementsADF.enc"
      "texmf-dist/fonts/map/dvips/adforn/OrnementsADF.map"
      "texmf-dist/fonts/tfm/arkandis/adforn/OrnementsADF.tfm"
      "texmf-dist/fonts/type1/arkandis/adforn/OrnementsADF.pfb"
      "texmf-dist/tex/latex/adforn/adforn.sty"
      "texmf-dist/tex/latex/adforn/uornementsadf.fd")
     (catalogue-license . "lppl gpl2"))
    ("authorindex"
     (name . "authorindex")
     (shortdesc . "Index citations by author names")
     (longdesc . "This package allows the user to...")
     (depend "authorindex.ARCH")
     (docfiles "texmf-dist/doc/latex/authorindex/COPYING")
     (runfiles
      "texmf-dist/scripts/authorindex/authorindex"
      "texmf-dist/tex/latex/authorindex/authorindex.sty")
     (catalogue-license . "lppl"))
    ("authorindex.x86_64-linux"
     (name . "authorindex.x86_64-linux")
     (binfiles "bin/amd64-netbsd/authorindex"))
    ("chs-physics-report"
     . ((name . "ch-physics-report")
        (shortdesc . "Physics lab reports...")
        (longdesc . "This package may...")
        (docfiles
         .
         ("texmf-dist/doc/latex/chs-physics-report/README.txt"
          "texmf-dist/doc/latex/chs-physics-report/chs-physics-report.pdf"))
        (runfiles
         .
         ("texmf-dist/tex/latex/chs-physics-report/chs-physics-report.sty"))
        (catalogue-license . "pd cc-by-sa-3")))
    ("collection-basic"
     (name . "collection-basic")
     (shortdesc . "Essential programs and files")
     (longdesc . "These files are regarded as basic...")
     (depend "amsfonts" "hyph-utf8" "hyphen-base" "texlive-common"
             "texlive.infra" "tlshell"))
    ("collection-texworks"
     (name . "collection-texworks")
     (shortdesc . "TeXworks editor...")
     (longdesc . "See http...")
     (depend "texworks" "collection-basic"))
    ("cyrillic-bin"
     (name . "cyrillic-bin")
     (shortdesc . "Cyrillic bibtex and makeindex")
     (depend "cyrillic-bin.ARCH")
     (docfiles
      "texmf-dist/doc/man/man1/rubibtex.1"
      "texmf-dist/doc/man/man1/rubibtex.man1.pdf")
     (runfiles
      "texmf-dist/scripts/texlive-extra/rumakeindex.sh"
      "texmf-dist/scripts/texlive-extra/rubibtex.sh"))
    ("cyrillic-bin.x86_64-linux"
     (name . "cyrillic-bin.x86_64-linux")
     (shortdesc . "x86_64-linux files of cyrillic-bin")
     (binfiles
      "bin/x86_64-linux/rubibtex"
      "bin/x86_64-linux/rumakeindex"))
    ("example"
     . ((name . "example")
        (shortdesc . "Typeset examples...")
        (longdesc . "The package makes it easier...")
        (runfiles
         .
         ("texmf-dist/tex/latex/example/example.sty"))
        (catalogue-license . "gpl")))
    ("lollipop"
     (name . "lollipop")
     (shortdesc . "TeX made easy")
     (longdesc . "Lollipop is TeX made easy...")
     (execute "AddFormat name=lollipop engine=tex options=\"lollipop.ini\"...")
     (docfiles
      "texmf-dist/doc/otherformats/lollipop/README"
      "texmf-dist/doc/otherformats/lollipop/manual/address.tex"
      "texmf-dist/doc/otherformats/lollipop/manual/appendix.tex"
      "texmf-dist/doc/otherformats/lollipop/manual/btxmac.tex"
      "texmf-dist/doc/otherformats/lollipop/manual/comm.tex"
      "texmf-dist/doc/otherformats/lollipop/manual/comment.tex"
      "texmf-dist/doc/otherformats/lollipop/manual/example.tex"
      "texmf-dist/doc/otherformats/lollipop/manual/extern.tex"
      "texmf-dist/doc/otherformats/lollipop/manual/head.tex"
      "texmf-dist/doc/otherformats/lollipop/manual/list.tex"
      "texmf-dist/doc/otherformats/lollipop/manual/lollipop-manual.bib"
      "texmf-dist/doc/otherformats/lollipop/manual/lollipop-manual.pdf")
     (runfiles
      "texmf-dist/tex/lollipop/lollipop-define.tex"
      "texmf-dist/tex/lollipop/lollipop-document.tex"
      "texmf-dist/tex/lollipop/lollipop-float.tex"
      "texmf-dist/tex/lollipop/lollipop-fontdefs.tex"
      "texmf-dist/tex/lollipop/lollipop-fonts.tex"
      "texmf-dist/tex/lollipop/lollipop-heading.tex"
      "texmf-dist/tex/lollipop/lollipop-lists.tex"
      "texmf-dist/tex/lollipop/lollipop-output.tex"
      "texmf-dist/tex/lollipop/lollipop-plain.tex"
      "texmf-dist/tex/lollipop/lollipop-text.tex"
      "texmf-dist/tex/lollipop/lollipop-tools.tex"
      "texmf-dist/tex/lollipop/lollipop.ini"
      "texmf-dist/tex/lollipop/lollipop.tex")
     (catalogue-license . "gpl3"))
    ("m-tx"
     (name . "m-tx")
     (shortdesc . "A preprocessor for pmx")
     (longdesc . "M-Tx is a preprocessor to pmx")
     (depend "m-tx.ARCH")
     (runfiles "texmf-dist/scripts/m-tx/m-tx.lua"))
    ("m-tx.x86_64-linux"
     (name . "m-tx.x86_64-linux")
     (binfiles "bin/x86_64-linux/m-tx"
               "bin/x86_64-linux/prepmx"))
    ("pax"
     (name . "pax")
     (shortdesc . "Extract and reinsert PDF...")
     (longdesc . "If PDF files are...")
     (depend "pax.ARCH")
     (docfiles
      "texmf-dist/doc/latex/pax/README")
     (srcfiles
      "texmf-dist/source/latex/pax/Makefile"
      "texmf-dist/source/latex/pax/build.xml")
     (runfiles
      "texmf-dist/scripts/pax/pdfannotextractor.pl")
     (catalogue-license . "lppl gpl"))
    ("pax.x86_64-linux"
     (name . "pax.x86_64-linux")
     (shortdesc . "x86_64-linux files of pax")
     (binfiles
      "bin/x86_64-linux/pdfannotextractor"))
    ("r_und_s"
     (name . "r_und_s")
     (runfiles "texmf-dist/tex/latex/r_und_s/r_und_s.sty"))
    ("stricttex"
     . ((name
         . "stricttex")
        (shortdesc
         . "Strictly balanced brackets and numbers in command names")
        (longdesc
         . "This is a small, LuaLaTeX-only package providing you with three,
sometimes useful features: It allows you to make brackets [...]  \"strict\",
meaning that each [ must be balanced by a ]. It allows you to use numbers in
command names, so that you can do stuff like \\newcommand\\pi12{\\pi_{12}}. It
allows you to use numbers and primes in command names, so that you can do
stuff like \\newcommand\\pi'12{\\pi '_{12}}.")
        (docfiles
         . ("texmf-dist/doc/lualatex/stricttex/README.md"
            "texmf-dist/doc/lualatex/stricttex/stricttex.pdf"))
        (runfiles
         . ("texmf-dist/tex/lualatex/stricttex/stricttex.lua"
            "texmf-dist/tex/lualatex/stricttex/stricttex.sty"))
        (catalogue-license . "lppl1.3c")))
    ("tex"
     (name . "tex")
     (shortdesc . "A sophisticated typesetting engine")
     (longdesc . "TeX is a typesetting system that incorporates...")
     (depend "cm" "hyphen-base" "tex.ARCH")
     (docfiles "texmf-dist/doc/man/man1/tex.1")
     (catalogue-license . "knuth"))
    ("texsis"
     . ((name
         . "texsis")
        (shortdesc
         . "Plain TeX macros for Physicists")
        (longdesc
         . "TeXsis is a TeX macro package which provides useful features for
typesetting research papers and related documents. For example, it includes
support specifically for: Automatic numbering of equations, figures, tables
and references; Simplified control of type sizes, line spacing, footnotes,
running headlines and footlines, and tables of contents, figures and tables;
Specialized document formats for research papers, preprints and \"e-prints\",
conference proceedings, theses, books, referee reports, letters, and
memoranda; Simplified means of constructing an index for a book or thesis;
Easy to use double column formatting; Specialized environments for lists,
theorems and proofs, centered or non-justified text, and listing computer
code; Specialized macros for easily constructing ruled tables. TeXsis was
originally developed for physicists, but others may also find it useful. It is
completely compatible with Plain TeX.")
        (depend . ("tex" "plain" "knuth-lib" "hyphen-base" "cm"))
        (docfiles
         . ("texmf-dist/doc/man/man1/texsis.1"
            "texmf-dist/doc/man/man1/texsis.man1.pdf"
            "texmf-dist/doc/otherformats/texsis/base/COPYING"
            "texmf-dist/doc/otherformats/texsis/base/Example.tex"
            "texmf-dist/doc/otherformats/texsis/base/Fonts.tex"
            "texmf-dist/doc/otherformats/texsis/base/INSTALL"
            "texmf-dist/doc/otherformats/texsis/base/Install.tex"
            "texmf-dist/doc/otherformats/texsis/base/MANIFEST"
            "texmf-dist/doc/otherformats/texsis/base/Manual.fgl"
            "texmf-dist/doc/otherformats/texsis/base/Manual.ref"
            "texmf-dist/doc/otherformats/texsis/base/Manual.tbl"
            "texmf-dist/doc/otherformats/texsis/base/Manual.tex"
            "texmf-dist/doc/otherformats/texsis/base/NEWS"
            "texmf-dist/doc/otherformats/texsis/base/README"
            "texmf-dist/doc/otherformats/texsis/base/TXSapxF.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXScover.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSdcol.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSdoc.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSdoc0.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSdocM.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSend.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSenvmt.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSeqns.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSfigs.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSfmts.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSfonts.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSinstl.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSintro.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSletr.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSmisc.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSprns.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSrefs.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSrevs.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSruled.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSsects.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSsite.000"
            "texmf-dist/doc/otherformats/texsis/base/TXSsymb.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXStags.doc"
            "texmf-dist/doc/otherformats/texsis/base/index.tex"
            "texmf-dist/doc/otherformats/texsis/base/letr"
            "texmf-dist/doc/otherformats/texsis/base/penguin.eps"
            "texmf-dist/doc/otherformats/texsis/base/penguin2.eps"
            "texmf-dist/doc/otherformats/texsis/base/texsis.el"
            "texmf-dist/doc/otherformats/texsis/base/texsis.lsm"))
        (runfiles
         . ("texmf-dist/bibtex/bst/texsis/texsis.bst"
            "texmf-dist/tex/texsis/base/AIP.txs"
            "texmf-dist/tex/texsis/base/CVformat.txs"
            "texmf-dist/tex/texsis/base/Elsevier.txs"
            "texmf-dist/tex/texsis/base/Exam.txs"
            "texmf-dist/tex/texsis/base/Formletr.txs"
            "texmf-dist/tex/texsis/base/IEEE.txs"
            "texmf-dist/tex/texsis/base/PhysRev.txs"
            "texmf-dist/tex/texsis/base/Spanish.txs"
            "texmf-dist/tex/texsis/base/Swedish.txs"
            "texmf-dist/tex/texsis/base/TXSconts.tex"
            "texmf-dist/tex/texsis/base/TXSdcol.tex"
            "texmf-dist/tex/texsis/base/TXSenvmt.tex"
            "texmf-dist/tex/texsis/base/TXSeqns.tex"
            "texmf-dist/tex/texsis/base/TXSfigs.tex"
            "texmf-dist/tex/texsis/base/TXSfmts.tex"
            "texmf-dist/tex/texsis/base/TXSfonts.tex"
            "texmf-dist/tex/texsis/base/TXShead.tex"
            "texmf-dist/tex/texsis/base/TXSinit.tex"
            "texmf-dist/tex/texsis/base/TXSletr.tex"
            "texmf-dist/tex/texsis/base/TXSmacs.tex"
            "texmf-dist/tex/texsis/base/TXSmemo.tex"
            "texmf-dist/tex/texsis/base/TXSprns.tex"
            "texmf-dist/tex/texsis/base/TXSrefs.tex"
            "texmf-dist/tex/texsis/base/TXSruled.tex"
            "texmf-dist/tex/texsis/base/TXSsects.tex"
            "texmf-dist/tex/texsis/base/TXSsite.tex"
            "texmf-dist/tex/texsis/base/TXSsymb.tex"
            "texmf-dist/tex/texsis/base/TXStags.tex"
            "texmf-dist/tex/texsis/base/TXStitle.tex"
            "texmf-dist/tex/texsis/base/Tablebod.txs"
            "texmf-dist/tex/texsis/base/WorldSci.txs"
            "texmf-dist/tex/texsis/base/color.txs"
            "texmf-dist/tex/texsis/base/nuclproc.txs"
            "texmf-dist/tex/texsis/base/printfont.txs"
            "texmf-dist/tex/texsis/base/spine.txs"
            "texmf-dist/tex/texsis/base/texsis.tex"
            "texmf-dist/tex/texsis/base/thesis.txs"
            "texmf-dist/tex/texsis/base/twin.txs"
            "texmf-dist/tex/texsis/config/texsis.ini"))
        (catalogue-license . "lppl")))
    ("trsym"
     (name . "trsym")
     (shortdesc . "Symbols for transformations")
     (longdesc . "The bundle provides Metafont...")
     (docfiles "texmf-dist/doc/latex/trsym/README"
               "texmf-dist/doc/latex/trsym/manifest.txt"
               "texmf-dist/doc/latex/trsym/trsym.pdf")
     (srcfiles "texmf-dist/source/latex/trsym/trsym.dtx"
               "texmf-dist/source/latex/trsym/trsym.ins")
     (runfiles "texmf-dist/fonts/source/public/trsym/trsy.mf"
               "texmf-dist/fonts/source/public/trsym/trsy10.mf"
               "texmf-dist/fonts/source/public/trsym/trsy12.mf"
               "texmf-dist/fonts/tfm/public/trsym/trsy10.tfm"
               "texmf-dist/fonts/tfm/public/trsym/trsy12.tfm"
               "texmf-dist/tex/latex/trsym/trsym.sty"
               "texmf-dist/tex/latex/trsym/utrsy.fd")
     (catalogue-license . "lppl"))
    ("vlna"
     (name . "vlna")
     (shortdesc . "Add ~ after non-syllabic preposition")
     (longdesc . "Preprocessor for TeX source")
     (depend "vlna.ARCH")
     (docfiles "texmf-dist/doc/man/man1/vlna.1"))
    ("vlna.x86_64-linux"
     (shortdesc "x86_64-linux files of vlna")
     (binfiles "bin/x86_64-linux/vlna"))
    ("web"
     (depend "web.ARCH")
     (docfiles "texmf-dist/doc/man/man1/tangle.1"))
    ("web.x86_64-linux"
     (name . "web.x86_64-linux")
     (binfiles "bin/x86_64-linux/tangle"))))

(test-assert "texlive->guix-package, no docfiles"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "example"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-example")
               ('version _)
               ('source ('origin
                          ('method 'svn-multi-fetch)
                          ('uri ('svn-multi-reference
                                 ('url ('texlive-packages-repository 'version))
                                 ('revision 12345)
                                 ('locations ('list "tex/latex/example/"))))
                          ('file-name ('git-file-name 'name 'version))
                          ('sha256
                           ('base32 (? string? hash)))))
               ('build-system 'texlive-build-system)
               ('home-page (? string?))
               ('synopsis (? string?))
               ('description (? string?))
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "texsis"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-texsis")
               ('version _)
               ('source ('origin
                          ('method 'svn-multi-fetch)
                          ('uri ('svn-multi-reference
                                 ('url ('texlive-packages-repository 'version))
                                 ('revision 12345)
                                 ('locations
                                  ('list "bibtex/bst/texsis/"
                                         "doc/man/man1/texsis.1"
                                         "doc/man/man1/texsis.man1.pdf"
                                         "doc/otherformats/texsis/base/"
                                         "tex/texsis/base/"
                                         "tex/texsis/config/"))))
                          ('file-name ('git-file-name 'name 'version))
                          ('sha256
                           ('base32 (? string? hash)))))
               ('outputs ''("out" "doc"))
               ('build-system 'texlive-build-system)
               ('propagated-inputs
                ('list 'texlive-cm
                       'texlive-hyphen-base
                       'texlive-knuth-lib
                       'texlive-plain
                       'texlive-tex))
               ('home-page (? string?))
               ('synopsis (? string?))
               ('description (? string?))
               ('license 'lppl))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, with METAFONT files"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "trsym"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name _)
               ('version _)
               ('source _)
               ('outputs _)
               ('build-system _)
               ('native-inputs
                ('list 'texlive-metafont))
               ('home-page (? string?))
               ('synopsis (? string?))
               ('description (? string?))
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, with catalogue entry, no inputs"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "12many"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-12many")
               ('version _)
               ('source ('origin
                          ('method 'svn-multi-fetch)
                          ('uri ('svn-multi-reference
                                 ('url ('texlive-packages-repository 'version))
                                 ('revision 12345)
                                 ('locations ('list "doc/latex/12many/"
                                                    "source/latex/12many/"
                                                    "tex/latex/12many/"))))
                          ('file-name ('git-file-name 'name 'version))
                          ('sha256
                           ('base32 (? string? hash)))))
               ('outputs ''("out" "doc"))
               ('build-system 'texlive-build-system)
               ('home-page "https://ctan.org/pkg/one2many")
               ('synopsis (? string?))
               ('description (? string?))
               ('license 'lppl))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, multiple licenses"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "chs-physics-report"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-chs-physics-report")
               ('version _)
               ('source ('origin
                          ('method 'svn-multi-fetch)
                          ('uri ('svn-multi-reference
                                 ('url ('texlive-packages-repository 'version))
                                 ('revision 12345)
                                 ('locations
                                  ('list "doc/latex/chs-physics-report/"
                                         "tex/latex/chs-physics-report/"))))
                          ('file-name ('git-file-name 'name 'version))
                          ('sha256
                           ('base32 (? string? hash)))))
               ('outputs ''("out" "doc"))
               ('build-system 'texlive-build-system)
               ('home-page (? string?))
               ('synopsis (? string?))
               ('description (? string?))
               ('license ('list 'public-domain 'cc-by-sa3.0)))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, meta-package"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "collection-texworks"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-collection-texworks")
               ('version '(package-version texlive-source))
               ('source #f)
               ('build-system 'trivial-build-system)
               ('arguments
                ('list '#:builder ('gexp ('mkdir ('ungexp 'output)))))
               ('propagated-inputs
                ('list 'texlive-collection-basic))
               ('home-page "https://www.tug.org/texlive/")
               ('synopsis (? string?))
               ('description (? string?))
               ('license
                ('fsf-free "https://www.tug.org/texlive/copying.html")))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, with TeX format"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "lollipop"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-lollipop")
               ('version _)
               ('source ('origin
                          ('method 'svn-multi-fetch)
                          ('uri ('svn-multi-reference
                                 ('url ('texlive-packages-repository 'version))
                                 ('revision 12345)
                                 ('locations ('list "doc/otherformats/lollipop/"
                                                    "tex/lollipop/"))))
                          ('file-name ('git-file-name 'name 'version))
                          ('sha256
                           ('base32 (? string? hash)))))
               ('outputs ''("out" "doc"))
               ('build-system 'texlive-build-system)
               ('arguments ('list '#:create-formats ('gexp ('list "lollipop"))))
               ('home-page (? string?))
               ('synopsis (? string?))
               ('description (? string?))
               ('license 'gpl3))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, execute but no TeX format"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "adforn"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-adforn")
               ('version _)
               ('source _)
               ('outputs ''("out" "doc"))
               ('build-system 'texlive-build-system)
               ('home-page (? string?))
               ('synopsis (? string?))
               ('description (? string?))
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, translate dependencies"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "collection-basic"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-collection-basic")
               ('version _)
               ('source _)
               ('build-system 'trivial-build-system)
               ('arguments
                ('list '#:builder ('gexp ('mkdir ('ungexp 'output)))))
               ('propagated-inputs
                ('list 'texlive-amsfonts 'texlive-hyphen-complete))
               ('home-page (? string?))
               ('synopsis (? string?))
               ('description (? string?))
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, lonely `hyphen-base' dependency and ARCH"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "tex"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-tex")
               ('version _)
               ('source _)
               ('outputs _)
               ('build-system 'texlive-build-system)
               ('arguments ('list '#:texlive-latex-bin? #f))
               ('propagated-inputs
                ('list 'texlive-cm 'texlive-hyphen-base))
               ('home-page (? string?))
               ('synopsis (? string?))
               ('description (? string?))
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, single script, no extension"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "authorindex"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-authorindex")
               ('version _)
               ('source _)
               ('outputs _)
               ('build-system 'texlive-build-system)
               ('arguments
                ('list '#:link-scripts ('gexp ('list "authorindex"))))
               ('home-page (? string?))
               ('synopsis (? string?))
               ('description (? string?))
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, multiple scripts, with extensions"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "cyrillic-bin"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-cyrillic-bin")
               ('version _)
               ('source _)
               ('outputs _)
               ('build-system 'texlive-build-system)
               ('arguments
                ('list '#:link-scripts
                       ('gexp ('list "rubibtex.sh" "rumakeindex.sh"))))
               ('home-page _)
               ('synopsis _)
               ('description _)
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, script with associated input"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "pax"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-pax")
               ('version _)
               ('source _)
               ('outputs _)
               ('build-system 'texlive-build-system)
               ('arguments
                ('list '#:link-scripts ('gexp ('list "pdfannotextractor.pl"))))
               ('inputs
                ('list 'perl))
               ('home-page _)
               ('synopsis _)
               ('description _)
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, propagated binaries, no script"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "vlna"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-vlna")
               ('version _)
               ('source _)
               ('outputs _)
               ('build-system 'texlive-build-system)
               ('propagated-inputs
                ('list 'texlive-vlna-bin))
               ('home-page _)
               ('synopsis _)
               ('description _)
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, propagated binaries and scripts"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "m-tx"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-m-tx")
               ('version _)
               ('source _)
               ('build-system 'texlive-build-system)
               ('arguments
                ('list '#:link-scripts ('gexp ('list "m-tx.lua"))))
               ('propagated-inputs
                ('list 'texlive-m-tx-bin))
               ('home-page _)
               ('synopsis _)
               ('description _)
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, with skipped propagated binaries"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "web"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-web")
               ('version _)
               ('source _)
               ('outputs _)
               ('build-system 'texlive-build-system)
               ('home-page _)
               ('synopsis _)
               ('description _)
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-assert "texlive->guix-package, with upstream-name property"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "r_und_s"
                                             #:version "0"
                                             #:database %fake-tlpdb)))
          (match result
            (('package
               ('name "texlive-r-und-s")
               ('version _)
               ('source _)
               ('properties _)
               ('build-system 'texlive-build-system)
               ('home-page _)
               ('synopsis _)
               ('description _)
               ('license _))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-end "texlive")
